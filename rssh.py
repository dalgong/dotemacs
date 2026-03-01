#!/usr/bin/env python3
"""rssh — SSH Multiplexer over a Single Connection.

Session multiplexing modeled after OpenSSH's ControlMaster/ControlPath
(see PROTOCOL.mux).  A single persistent SSH connection carries multiple
concurrent sessions, file transfers, and port forwards.

Architecture:
  [rssh client] --mux protocol--> [rssh daemon] --transport--> [rssh server]
       Unix socket                                SSH stdin/stdout
       (SSH mux-style)                            (custom TLV)

The mux protocol between client and daemon follows SSH PROTOCOL.mux
conventions: hello exchange with version negotiation, request IDs for
all client messages, standard response types (OK, FAILURE, etc.).

The transport protocol between daemon and server uses a compact TLV
binary format for channel multiplexing over a single SSH connection.

Modes:
  --server            Run as server on remote host (binary protocol on stdio)
  connect <host>      Start daemon, establish connection
  <host> [cmd...]     Interactive shell or command execution (auto-connects)
  cp src dst          File transfer (host:path notation)
  disconnect <host>   Tear down connection
  status [<host>]     Show connection status
  list                List active connections
"""

import asyncio
import base64
import fcntl
import grp
import json
import os
import pty
import pwd
import select
import shutil
import signal
import socket
import stat as stat_mod
import struct
import sys
import termios
import threading
import time
import tty

# ---------------------------------------------------------------------------
# Transport protocol layer — Binary TLV (Type-Length-Value)
#
# Used between daemon and remote server over SSH stdin/stdout.
# Frame format: [1-byte type][4-byte big-endian payload length][payload]
# ---------------------------------------------------------------------------

MSG_READY          = 0x01
MSG_OPEN           = 0x02
MSG_OPENED         = 0x03
MSG_DATA           = 0x04
MSG_RESIZE         = 0x05
MSG_EOF            = 0x06
MSG_CLOSE          = 0x07
MSG_ERROR          = 0x08
MSG_PUT            = 0x09
MSG_GET            = 0x0A
MSG_FILE_INFO      = 0x0B
MSG_FILE_DATA      = 0x0C
MSG_FILE_DONE      = 0x0D
MSG_FILE_ERROR     = 0x0E
MSG_PING           = 0x0F
MSG_PONG           = 0x10
MSG_RPC            = 0x11
MSG_RPC_RESULT     = 0x12
MSG_NOTIFY         = 0x13

_TYPE_TO_ID = {
    "ready": MSG_READY, "open": MSG_OPEN, "opened": MSG_OPENED,
    "data": MSG_DATA, "resize": MSG_RESIZE, "eof": MSG_EOF,
    "close": MSG_CLOSE, "error": MSG_ERROR,
    "put": MSG_PUT, "get": MSG_GET, "file_info": MSG_FILE_INFO,
    "file_data": MSG_FILE_DATA, "file_done": MSG_FILE_DONE,
    "file_error": MSG_FILE_ERROR,
    "ping": MSG_PING, "pong": MSG_PONG,
    "rpc": MSG_RPC, "rpc_result": MSG_RPC_RESULT, "notify": MSG_NOTIFY,
}
_ID_TO_TYPE = {v: k for k, v in _TYPE_TO_ID.items()}


# -- Binary helpers for variable-length fields (transport protocol) --

def _pack_str(s: str) -> bytes:
    b = s.encode("utf-8")
    return struct.pack("!H", len(b)) + b

def _unpack_str(buf, off):
    (n,) = struct.unpack_from("!H", buf, off)
    return buf[off + 2:off + 2 + n].decode("utf-8"), off + 2 + n

def _pack_str_list(lst) -> bytes:
    parts = [struct.pack("!H", len(lst))]
    for s in lst:
        parts.append(_pack_str(s))
    return b"".join(parts)

def _unpack_str_list(buf, off):
    (count,) = struct.unpack_from("!H", buf, off); off += 2
    result = []
    for _ in range(count):
        s, off = _unpack_str(buf, off)
        result.append(s)
    return result, off

def _pack_str_dict(d) -> bytes:
    parts = [struct.pack("!H", len(d))]
    for k, v in d.items():
        parts.append(_pack_str(str(k)))
        parts.append(_pack_str(str(v)))
    return b"".join(parts)

def _unpack_str_dict(buf, off):
    (count,) = struct.unpack_from("!H", buf, off); off += 2
    result = {}
    for _ in range(count):
        k, off = _unpack_str(buf, off)
        v, off = _unpack_str(buf, off)
        result[k] = v
    return result, off

def _mode_to_int(mode) -> int:
    if isinstance(mode, str):
        return int(mode, 0) if mode.startswith(("0o", "0O")) else int(mode, 8)
    return mode


# -- Transport payload encode / decode --

def _encode_payload(header: dict, data: bytes) -> bytes:
    t = header["type"]

    if t in ("ready", "ping", "pong"):
        return b""

    if t == "open":
        return (struct.pack("!IBHH", header.get("ch", 0),
                            1 if header.get("pty") else 0,
                            header.get("rows", 24), header.get("cols", 80))
                + _pack_str_list(header.get("cmd", ["bash"]))
                + _pack_str_dict(header.get("env", {})))

    if t == "opened":
        return struct.pack("!II", header["ch"], header.get("pid", 0))

    if t == "data":
        return struct.pack("!I", header["ch"]) + data

    if t == "resize":
        return struct.pack("!IHH", header["ch"],
                           header.get("rows", 24), header.get("cols", 80))

    if t == "eof":
        return struct.pack("!I", header["ch"])

    if t == "close":
        return struct.pack("!Ii", header["ch"], header.get("exit_code", 0))

    if t == "error":
        return struct.pack("!I", header.get("ch", 0)) + header.get("msg", "").encode("utf-8")

    if t == "put":
        return (struct.pack("!IQH", header["req"], header.get("size", 0),
                            _mode_to_int(header.get("mode", 0o644)))
                + _pack_str(header["path"]))

    if t == "get":
        return struct.pack("!I", header["req"]) + _pack_str(header["path"])

    if t == "file_info":
        return struct.pack("!IQH", header["req"], header.get("size", 0),
                           _mode_to_int(header.get("mode", 0o644)))

    if t == "file_data":
        return struct.pack("!I", header["req"]) + data

    if t == "file_done":
        return struct.pack("!I", header["req"])

    if t == "file_error":
        return struct.pack("!I", header.get("req", 0)) + header.get("msg", "").encode("utf-8")

    if t == "rpc":
        req_json = json.dumps({"method": header["method"],
                                "params": header.get("params", {})}).encode("utf-8")
        return struct.pack("!I", header["req"]) + req_json

    if t == "rpc_result":
        result_json = json.dumps(header.get("result")).encode("utf-8")
        return struct.pack("!I", header["req"]) + result_json

    if t == "notify":
        return json.dumps({"method": header["method"],
                           "params": header.get("params", {})}).encode("utf-8")

    raise ValueError(f"Unknown message type: {t}")


def _decode_payload(msg_type: str, payload: bytes):
    header = {"type": msg_type}
    data = b""

    if msg_type in ("ready", "ping", "pong"):
        pass

    elif msg_type == "open":
        off = 0
        ch, pty_flag, rows, cols = struct.unpack_from("!IBHH", payload, off); off += 9
        cmd, off = _unpack_str_list(payload, off)
        env, off = _unpack_str_dict(payload, off)
        header.update(ch=ch, pty=bool(pty_flag), rows=rows, cols=cols, cmd=cmd)
        if env:
            header["env"] = env

    elif msg_type == "opened":
        ch, pid = struct.unpack_from("!II", payload, 0)
        header.update(ch=ch, pid=pid)

    elif msg_type == "data":
        (ch,) = struct.unpack_from("!I", payload, 0)
        header["ch"] = ch
        data = payload[4:]

    elif msg_type == "resize":
        ch, rows, cols = struct.unpack_from("!IHH", payload, 0)
        header.update(ch=ch, rows=rows, cols=cols)

    elif msg_type == "eof":
        (ch,) = struct.unpack_from("!I", payload, 0)
        header["ch"] = ch

    elif msg_type == "close":
        ch, exit_code = struct.unpack_from("!Ii", payload, 0)
        header.update(ch=ch, exit_code=exit_code)

    elif msg_type == "error":
        (ch,) = struct.unpack_from("!I", payload, 0)
        header["ch"] = ch
        header["msg"] = payload[4:].decode("utf-8", errors="replace")

    elif msg_type == "put":
        off = 0
        req, size, mode_int = struct.unpack_from("!IQH", payload, off); off += 14
        path, off = _unpack_str(payload, off)
        header.update(req=req, size=size, mode=mode_int, path=path)

    elif msg_type == "get":
        off = 0
        (req,) = struct.unpack_from("!I", payload, off); off += 4
        path, off = _unpack_str(payload, off)
        header.update(req=req, path=path)

    elif msg_type == "file_info":
        req, size, mode_int = struct.unpack_from("!IQH", payload, 0)
        header.update(req=req, size=size, mode=mode_int)

    elif msg_type == "file_data":
        (req,) = struct.unpack_from("!I", payload, 0)
        header["req"] = req
        data = payload[4:]

    elif msg_type == "file_done":
        (req,) = struct.unpack_from("!I", payload, 0)
        header["req"] = req

    elif msg_type == "file_error":
        (req,) = struct.unpack_from("!I", payload, 0)
        header["req"] = req
        header["msg"] = payload[4:].decode("utf-8", errors="replace")

    elif msg_type == "rpc":
        (req,) = struct.unpack_from("!I", payload, 0)
        header["req"] = req
        rpc_obj = json.loads(payload[4:].decode("utf-8"))
        header["method"] = rpc_obj["method"]
        header["params"] = rpc_obj.get("params", {})

    elif msg_type == "rpc_result":
        (req,) = struct.unpack_from("!I", payload, 0)
        header["req"] = req
        header["result"] = json.loads(payload[4:].decode("utf-8"))

    elif msg_type == "notify":
        notify_obj = json.loads(payload.decode("utf-8"))
        header["method"] = notify_obj["method"]
        header["params"] = notify_obj.get("params", {})

    return header, data


# -- Transport frame encode / decode / read / write --

def encode_frame(header: dict, data: bytes = b"") -> bytes:
    type_id = _TYPE_TO_ID[header["type"]]
    payload = _encode_payload(header, data)
    return struct.pack("!BI", type_id, len(payload)) + payload


def decode_frame(type_id: int, payload: bytes):
    msg_type = _ID_TO_TYPE.get(type_id)
    if msg_type is None:
        raise ValueError(f"Unknown message type ID: {type_id}")
    return _decode_payload(msg_type, payload)


async def read_frame(reader: asyncio.StreamReader):
    hdr = await reader.readexactly(5)
    type_id = hdr[0]
    (length,) = struct.unpack("!I", hdr[1:5])
    payload = await reader.readexactly(length) if length else b""
    return decode_frame(type_id, payload)


def read_frame_sync(fd) -> tuple:
    hdr = _read_exactly(fd, 5)
    type_id = hdr[0]
    (length,) = struct.unpack("!I", hdr[1:5])
    payload = _read_exactly(fd, length) if length else b""
    return decode_frame(type_id, payload)


def _read_exactly(fd, n: int) -> bytes:
    buf = bytearray()
    while len(buf) < n:
        if isinstance(fd, int):
            chunk = os.read(fd, n - len(buf))
        elif isinstance(fd, socket.socket):
            chunk = fd.recv(n - len(buf))
        else:
            chunk = fd.read(n - len(buf))
        if not chunk:
            raise EOFError("Connection closed")
        buf.extend(chunk)
    return bytes(buf)


def write_frame_sync(fd, header: dict, data: bytes = b""):
    frame = encode_frame(header, data)
    _write_all(fd, frame)


def _write_all(fd, data: bytes):
    if isinstance(fd, int):
        mv = memoryview(data)
        written = 0
        while written < len(mv):
            written += os.write(fd, mv[written:])
    elif isinstance(fd, socket.socket):
        fd.sendall(data)
    else:
        fd.write(data)
        fd.flush()


# ---------------------------------------------------------------------------
# Mux protocol layer — SSH mux (Client <-> Daemon, over Unix socket)
#
# Modeled after OpenSSH PROTOCOL.mux.
# Wire format: [uint32 packet_length][uint32 type][body]
# String wire format: [uint32 length][data]
# Bool wire format: uint32 (0 = false, nonzero = true)
# ---------------------------------------------------------------------------

# Protocol version (matches OpenSSH SSHMUX_VER)
MUX_VERSION = 4

# Standard SSH mux message types (from OpenSSH PROTOCOL.mux / mux.c)
MUX_MSG_HELLO           = 0x00000001
MUX_C_NEW_SESSION       = 0x10000002
MUX_C_ALIVE_CHECK       = 0x10000004
MUX_C_TERMINATE         = 0x10000005
MUX_C_OPEN_FWD          = 0x10000006
MUX_C_CLOSE_FWD         = 0x10000007
MUX_C_NEW_STDIO_FWD     = 0x10000008
MUX_C_STOP_LISTENING    = 0x10000009
MUX_S_OK                = 0x80000001
MUX_S_PERMISSION_DENIED = 0x80000002
MUX_S_FAILURE           = 0x80000003
MUX_S_EXIT_MESSAGE      = 0x80000004
MUX_S_ALIVE             = 0x80000005
MUX_S_SESSION_OPENED    = 0x80000006
MUX_S_REMOTE_PORT       = 0x80000007
MUX_S_TTY_ALLOC_FAIL    = 0x80000008

# Forwarding types
MUX_FWD_LOCAL   = 1
MUX_FWD_REMOTE  = 2
MUX_FWD_DYNAMIC = 3

# rssh extension message types (session data relay — no fd passing)
MUX_C_SESSION_DATA      = 0x20000001
MUX_C_SESSION_RESIZE    = 0x20000002
MUX_C_SESSION_EOF       = 0x20000003
MUX_C_FILE_PUT          = 0x20000010
MUX_C_FILE_GET          = 0x20000011
MUX_C_FILE_DATA         = 0x20000012
MUX_C_FILE_DONE         = 0x20000013
MUX_S_SESSION_DATA      = 0xA0000001
MUX_S_FILE_INFO         = 0xA0000010
MUX_S_FILE_DATA         = 0xA0000011
MUX_S_FILE_DONE         = 0xA0000012
MUX_S_FILE_ERROR        = 0xA0000013

# rssh RPC extension message types
MUX_C_RPC               = 0x20000020
MUX_S_RPC_RESULT        = 0xA0000020
MUX_S_NOTIFY            = 0xA0000022


# -- Mux wire format helpers --

def mux_pack_u32(val: int) -> bytes:
    return struct.pack("!I", val)

def mux_pack_u64(val: int) -> bytes:
    return struct.pack("!Q", val)

def mux_pack_bool(val: bool) -> bytes:
    return struct.pack("!I", 1 if val else 0)

def mux_pack_string(s) -> bytes:
    if isinstance(s, str):
        b = s.encode("utf-8")
    elif s is None:
        b = b""
    else:
        b = s
    return struct.pack("!I", len(b)) + b


class MuxParser:
    """Parses fields from an SSH mux packet body."""

    def __init__(self, data: bytes):
        self._data = data
        self._off = 0

    @property
    def remaining(self) -> int:
        return len(self._data) - self._off

    def get_u32(self) -> int:
        val, = struct.unpack_from("!I", self._data, self._off)
        self._off += 4
        return val

    def get_u64(self) -> int:
        val, = struct.unpack_from("!Q", self._data, self._off)
        self._off += 8
        return val

    def get_bool(self) -> bool:
        return self.get_u32() != 0

    def get_string(self) -> bytes:
        length = self.get_u32()
        val = self._data[self._off:self._off + length]
        self._off += length
        return val

    def get_cstring(self) -> str:
        return self.get_string().decode("utf-8", errors="replace")


# -- Mux packet read / write --

def mux_encode_packet(msg_type: int, body: bytes = b"") -> bytes:
    """Encode: [uint32 packet_length][uint32 type][body]."""
    inner = struct.pack("!I", msg_type) + body
    return struct.pack("!I", len(inner)) + inner


async def mux_read_packet(reader: asyncio.StreamReader):
    """Read one mux packet. Returns (msg_type, body_bytes)."""
    hdr = await reader.readexactly(4)
    length, = struct.unpack("!I", hdr)
    if length < 4:
        raise ValueError(f"Mux packet too short: {length}")
    payload = await reader.readexactly(length)
    msg_type, = struct.unpack_from("!I", payload, 0)
    return msg_type, payload[4:]


def mux_read_packet_sync(fd) -> tuple:
    """Read one mux packet synchronously. Returns (msg_type, body_bytes)."""
    hdr = _read_exactly(fd, 4)
    length, = struct.unpack("!I", hdr)
    if length < 4:
        raise ValueError(f"Mux packet too short: {length}")
    payload = _read_exactly(fd, length)
    msg_type, = struct.unpack_from("!I", payload, 0)
    return msg_type, payload[4:]


def mux_write_packet_sync(fd, msg_type: int, body: bytes = b""):
    """Write one mux packet synchronously."""
    _write_all(fd, mux_encode_packet(msg_type, body))


# -- Mux body builders --

def _mux_hello_body(version: int = MUX_VERSION) -> bytes:
    return mux_pack_u32(version)

def _mux_ok_body(rid: int) -> bytes:
    return mux_pack_u32(rid)

def _mux_failure_body(rid: int, reason: str) -> bytes:
    return mux_pack_u32(rid) + mux_pack_string(reason)

def _mux_session_opened_body(rid: int, sid: int) -> bytes:
    return mux_pack_u32(rid) + mux_pack_u32(sid)

def _mux_alive_body(rid: int, pid: int) -> bytes:
    return mux_pack_u32(rid) + mux_pack_u32(pid)

def _mux_exit_message_body(sid: int, exitval: int) -> bytes:
    return mux_pack_u32(sid) + mux_pack_u32(exitval & 0xFFFFFFFF)

def _mux_session_data_body(sid: int, data: bytes) -> bytes:
    return mux_pack_u32(sid) + mux_pack_string(data)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

RSSH_DIR = os.path.expanduser("~/.rssh")
CHUNK_SIZE = 65536


# ---------------------------------------------------------------------------
# Server mode  (runs on remote host — UNCHANGED)
# ---------------------------------------------------------------------------

class Server:
    """Runs on remote host. Reads/writes binary TLV frames on stdin/stdout."""

    def __init__(self):
        self.channels = {}
        self.write_queue = None
        self.loop = None
        self.managed_procs = {}  # pid -> {proc, stdout_buf, stderr_buf, exited, exit_code}
        self.watchers = {}       # path -> task

    async def run(self):
        self.loop = asyncio.get_event_loop()
        self.write_queue = asyncio.Queue()
        writer_task = asyncio.ensure_future(self._writer())
        await self.send({"type": "ready"})
        try:
            await self._reader()
        except (EOFError, ConnectionError, OSError):
            pass
        finally:
            writer_task.cancel()
            for ch_id in list(self.channels):
                await self._close_channel(ch_id, -1)

    async def send(self, header: dict, data: bytes = b""):
        await self.write_queue.put(encode_frame(header, data))

    async def _writer(self):
        stdout_fd = sys.stdout.buffer.fileno()
        while True:
            frame = await self.write_queue.get()
            try:
                os.write(stdout_fd, frame)
            except OSError:
                return

    async def _reader(self):
        stdin_fd = sys.stdin.buffer.fileno()
        while True:
            try:
                header, data = await self.loop.run_in_executor(
                    None, read_frame_sync, stdin_fd
                )
            except EOFError:
                return
            await self._dispatch(header, data)

    async def _dispatch(self, header: dict, data: bytes):
        msg_type = header.get("type")
        if msg_type == "open":
            await self._handle_open(header)
        elif msg_type == "data":
            await self._handle_data(header, data)
        elif msg_type == "resize":
            await self._handle_resize(header)
        elif msg_type == "eof":
            await self._handle_eof(header)
        elif msg_type == "put":
            await self._handle_put(header)
        elif msg_type == "get":
            await self._handle_get(header)
        elif msg_type == "file_data":
            await self._handle_file_data(header, data)
        elif msg_type == "file_done":
            await self._handle_file_done(header)
        elif msg_type == "ping":
            await self.send({"type": "pong"})
        elif msg_type == "rpc":
            await self._handle_rpc(header)

    async def _handle_open(self, header: dict):
        ch_id = header["ch"]
        cmd = header.get("cmd", ["bash"])
        use_pty = header.get("pty", False)
        env_vars = header.get("env", {})
        rows = header.get("rows", 24)
        cols = header.get("cols", 80)
        env = os.environ.copy()
        env.update(env_vars)
        env["TERM"] = env.get("TERM", "xterm-256color")
        if use_pty:
            await self._open_pty_channel(ch_id, cmd, env, rows, cols)
        else:
            await self._open_pipe_channel(ch_id, cmd, env)

    async def _open_pty_channel(self, ch_id, cmd, env, rows, cols):
        master_fd, slave_fd = pty.openpty()
        winsize = struct.pack("HHHH", rows, cols, 0, 0)
        fcntl.ioctl(slave_fd, termios.TIOCSWINSZ, winsize)
        pid = os.fork()
        if pid == 0:
            os.close(master_fd)
            os.setsid()
            fcntl.ioctl(slave_fd, termios.TIOCSCTTY, 0)
            os.dup2(slave_fd, 0)
            os.dup2(slave_fd, 1)
            os.dup2(slave_fd, 2)
            if slave_fd > 2:
                os.close(slave_fd)
            os.execvpe(cmd[0], cmd, env)
        else:
            os.close(slave_fd)
            self.channels[ch_id] = {
                "type": "pty", "master_fd": master_fd,
                "pid": pid, "alive": True,
            }
            await self.send({"type": "opened", "ch": ch_id, "pid": pid})
            thread = threading.Thread(
                target=self._pty_reader_thread,
                args=(ch_id, master_fd), daemon=True,
            )
            thread.start()
            asyncio.ensure_future(self._wait_child(ch_id, pid))

    def _pty_reader_thread(self, ch_id, master_fd):
        try:
            while True:
                try:
                    data = os.read(master_fd, 4096)
                except OSError:
                    break
                if not data:
                    break
                frame = encode_frame({"type": "data", "ch": ch_id}, data)
                asyncio.run_coroutine_threadsafe(
                    self.write_queue.put(frame), self.loop
                )
        except Exception:
            pass

    async def _wait_child(self, ch_id, pid):
        while True:
            try:
                rpid, status = os.waitpid(pid, os.WNOHANG)
            except ChildProcessError:
                await self._close_channel(ch_id, -1)
                return
            if rpid != 0:
                exit_code = os.WEXITSTATUS(status) if os.WIFEXITED(status) else -1
                await self._close_channel(ch_id, exit_code)
                return
            await asyncio.sleep(0.1)

    async def _open_pipe_channel(self, ch_id, cmd, env):
        try:
            proc = await asyncio.create_subprocess_exec(
                *cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.STDOUT,
                env=env,
            )
        except Exception as e:
            await self.send({"type": "error", "ch": ch_id, "msg": str(e)})
            return
        self.channels[ch_id] = {"type": "pipe", "proc": proc, "alive": True}
        await self.send({"type": "opened", "ch": ch_id, "pid": proc.pid})
        asyncio.ensure_future(self._pipe_reader(ch_id, proc))

    async def _pipe_reader(self, ch_id, proc):
        try:
            while True:
                data = await proc.stdout.read(4096)
                if not data:
                    break
                await self.send({"type": "data", "ch": ch_id}, data)
        except Exception:
            pass
        exit_code = await proc.wait()
        await self._close_channel(ch_id, exit_code)

    async def _close_channel(self, ch_id, exit_code):
        ch = self.channels.pop(ch_id, None)
        if ch is None:
            return
        ch["alive"] = False
        if ch["type"] == "pty":
            try:
                os.close(ch["master_fd"])
            except OSError:
                pass
            try:
                os.kill(ch["pid"], signal.SIGTERM)
            except (OSError, ProcessLookupError):
                pass
        elif ch["type"] == "pipe":
            proc = ch["proc"]
            if proc.stdin and not proc.stdin.is_closing():
                proc.stdin.close()
            try:
                proc.kill()
            except ProcessLookupError:
                pass
        await self.send({"type": "close", "ch": ch_id, "exit_code": exit_code})

    async def _handle_data(self, header: dict, data: bytes):
        ch = self.channels.get(header["ch"])
        if ch is None:
            return
        if ch["type"] == "pty":
            try:
                os.write(ch["master_fd"], data)
            except OSError:
                pass
        elif ch["type"] == "pipe":
            if ch["proc"].stdin:
                ch["proc"].stdin.write(data)
                await ch["proc"].stdin.drain()

    async def _handle_resize(self, header: dict):
        ch = self.channels.get(header["ch"])
        if ch and ch["type"] == "pty":
            winsize = struct.pack("HHHH", header.get("rows", 24),
                                  header.get("cols", 80), 0, 0)
            try:
                fcntl.ioctl(ch["master_fd"], termios.TIOCSWINSZ, winsize)
            except OSError:
                pass

    async def _handle_eof(self, header: dict):
        ch = self.channels.get(header["ch"])
        if ch is None:
            return
        if ch["type"] == "pty":
            try:
                os.write(ch["master_fd"], b"\x04")
            except OSError:
                pass
        elif ch["type"] == "pipe" and ch["proc"].stdin:
            ch["proc"].stdin.close()

    async def _handle_put(self, header: dict):
        req = header["req"]
        path = header["path"]
        mode = header.get("mode", 0o644)
        if isinstance(mode, str):
            mode = int(mode, 8)
        self.channels[f"file_{req}"] = {
            "type": "file_put", "path": path, "mode": mode,
            "size": header.get("size", 0), "received": 0, "fd": None,
        }
        try:
            os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
            self.channels[f"file_{req}"]["fd"] = open(path, "wb")
        except Exception as e:
            await self.send({"type": "file_error", "req": req, "msg": str(e)})
            self.channels.pop(f"file_{req}", None)

    async def _handle_get(self, header: dict):
        req = header["req"]
        path = header["path"]
        try:
            stat = os.stat(path)
            size = stat.st_size
            mode = oct(stat.st_mode & 0o7777)
            await self.send({"type": "file_info", "req": req, "size": size, "mode": mode})
            with open(path, "rb") as f:
                while True:
                    chunk = f.read(CHUNK_SIZE)
                    if not chunk:
                        break
                    await self.send({"type": "file_data", "req": req}, chunk)
            await self.send({"type": "file_done", "req": req})
        except Exception as e:
            await self.send({"type": "file_error", "req": req, "msg": str(e)})

    async def _handle_file_data(self, header: dict, data: bytes):
        ch = self.channels.get(f"file_{header['req']}")
        if ch and ch["fd"]:
            ch["fd"].write(data)
            ch["received"] += len(data)

    async def _handle_file_done(self, header: dict):
        req = header["req"]
        ch = self.channels.pop(f"file_{req}", None)
        if ch and ch["fd"]:
            ch["fd"].close()
            try:
                os.chmod(ch["path"], ch["mode"])
            except OSError:
                pass
            await self.send({"type": "file_done", "req": req})

    # -- RPC dispatch --

    async def _handle_rpc(self, header: dict):
        req = header["req"]
        method = header["method"]
        params = header.get("params", {})
        try:
            result = await self._dispatch_rpc(method, params)
            await self.send({"type": "rpc_result", "req": req,
                             "result": {"ok": result}})
        except Exception as e:
            await self.send({"type": "rpc_result", "req": req,
                             "result": {"error": {"message": str(e)}}})

    async def _dispatch_rpc(self, method, params):
        dispatch = {
            "run": self._rpc_run,
            "stat": self._rpc_stat,
            "exists": self._rpc_exists,
            "truename": self._rpc_truename,
            "dir.list": self._rpc_dir_list,
            "read": self._rpc_read,
            "write": self._rpc_write,
            "copy": self._rpc_copy,
            "rename": self._rpc_rename,
            "delete": self._rpc_delete,
            "mkdir": self._rpc_mkdir,
            "rmdir": self._rpc_rmdir,
            "symlink": self._rpc_symlink,
            "chmod": self._rpc_chmod,
            "chown": self._rpc_chown,
            "system.info": self._rpc_system_info,
            "system.getenv": self._rpc_system_getenv,
            "batch": self._rpc_batch,
            "run_parallel": self._rpc_run_parallel,
            "watch.add": self._rpc_watch_add,
            "watch.remove": self._rpc_watch_remove,
            "watch.list": self._rpc_watch_list,
            "process.start": self._rpc_process_start,
            "process.read": self._rpc_process_read,
            "process.write": self._rpc_process_write,
            "process.kill": self._rpc_process_kill,
            "process.close_stdin": self._rpc_process_close_stdin,
            "process.list": self._rpc_process_list,
        }
        handler = dispatch.get(method)
        if handler is None:
            raise ValueError(f"Unknown RPC method: {method}")
        return await handler(params)

    # -- RPC: run (sync command execution) --

    async def _rpc_run(self, params):
        cmd = params.get("cmd", "/bin/sh")
        args = params.get("args", [])
        if isinstance(cmd, str):
            full_cmd = [cmd] + args
        else:
            full_cmd = list(cmd) + args
        cwd = params.get("cwd", os.path.expanduser("~"))
        if not os.path.isdir(cwd):
            cwd = os.path.expanduser("~")
        env = os.environ.copy()
        env.update(params.get("env", {}))
        stdin_data = params.get("stdin")
        try:
            proc = await asyncio.create_subprocess_exec(
                *full_cmd,
                stdin=asyncio.subprocess.PIPE if stdin_data else asyncio.subprocess.DEVNULL,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd=cwd, env=env)
            stdin_bytes = stdin_data.encode("utf-8") if isinstance(stdin_data, str) else stdin_data
            stdout, stderr = await proc.communicate(input=stdin_bytes)
        except FileNotFoundError:
            return {"stdout": "", "stderr": f"{full_cmd[0]}: command not found\n",
                    "exit_code": 127}
        return {
            "stdout": base64.b64encode(stdout).decode("ascii"),
            "stderr": base64.b64encode(stderr).decode("ascii"),
            "exit_code": proc.returncode,
        }

    # -- RPC: file operations --

    async def _rpc_stat(self, params):
        path = params["path"]
        lstat = params.get("lstat", True)
        try:
            st = os.lstat(path) if lstat else os.stat(path)
        except FileNotFoundError:
            raise FileNotFoundError(f"No such file: {path}")
        except PermissionError:
            raise PermissionError(f"Permission denied: {path}")
        if stat_mod.S_ISDIR(st.st_mode):
            ftype = "directory"
        elif stat_mod.S_ISLNK(st.st_mode):
            ftype = "symlink"
        elif stat_mod.S_ISREG(st.st_mode):
            ftype = "file"
        elif stat_mod.S_ISCHR(st.st_mode):
            ftype = "chardevice"
        elif stat_mod.S_ISBLK(st.st_mode):
            ftype = "blockdevice"
        elif stat_mod.S_ISFIFO(st.st_mode):
            ftype = "fifo"
        elif stat_mod.S_ISSOCK(st.st_mode):
            ftype = "socket"
        else:
            ftype = "unknown"
        result = {
            "type": ftype,
            "mode": st.st_mode & 0o7777,
            "size": st.st_size,
            "mtime": int(st.st_mtime),
            "atime": int(st.st_atime),
            "ctime": int(st.st_ctime),
            "uid": st.st_uid,
            "gid": st.st_gid,
            "nlinks": st.st_nlink,
            "inode": st.st_ino,
            "dev": st.st_dev,
        }
        if ftype == "symlink":
            try:
                result["link_target"] = os.readlink(path)
            except OSError:
                result["link_target"] = ""
        try:
            result["uname"] = pwd.getpwuid(st.st_uid).pw_name
        except (KeyError, OSError):
            result["uname"] = str(st.st_uid)
        try:
            result["gname"] = grp.getgrgid(st.st_gid).gr_name
        except (KeyError, OSError):
            result["gname"] = str(st.st_gid)
        return result

    async def _rpc_exists(self, params):
        return {"exists": os.path.exists(params["path"])}

    async def _rpc_truename(self, params):
        return {"path": os.path.realpath(params["path"])}

    async def _rpc_dir_list(self, params):
        path = params["path"]
        include_attrs = params.get("attrs", False)
        include_hidden = params.get("hidden", True)
        entries = []
        try:
            with os.scandir(path) as it:
                for entry in it:
                    if not include_hidden and entry.name.startswith("."):
                        continue
                    item = {"name": entry.name}
                    try:
                        if entry.is_dir(follow_symlinks=False):
                            item["type"] = "directory"
                        elif entry.is_symlink():
                            item["type"] = "symlink"
                        elif entry.is_file(follow_symlinks=False):
                            item["type"] = "file"
                        else:
                            item["type"] = "other"
                    except OSError:
                        item["type"] = "unknown"
                    if include_attrs:
                        try:
                            st = entry.stat(follow_symlinks=False)
                            item["attrs"] = {
                                "mode": st.st_mode & 0o7777,
                                "size": st.st_size,
                                "mtime": int(st.st_mtime),
                                "atime": int(st.st_atime),
                                "ctime": int(st.st_ctime),
                                "uid": st.st_uid,
                                "gid": st.st_gid,
                                "nlinks": st.st_nlink,
                                "inode": st.st_ino,
                                "dev": st.st_dev,
                            }
                            if item["type"] == "symlink":
                                try:
                                    item["attrs"]["link_target"] = os.readlink(entry.path)
                                except OSError:
                                    item["attrs"]["link_target"] = ""
                            try:
                                item["attrs"]["uname"] = pwd.getpwuid(st.st_uid).pw_name
                            except (KeyError, OSError):
                                item["attrs"]["uname"] = str(st.st_uid)
                            try:
                                item["attrs"]["gname"] = grp.getgrgid(st.st_gid).gr_name
                            except (KeyError, OSError):
                                item["attrs"]["gname"] = str(st.st_gid)
                        except OSError:
                            pass
                    entries.append(item)
        except FileNotFoundError:
            raise FileNotFoundError(f"No such directory: {path}")
        except PermissionError:
            raise PermissionError(f"Permission denied: {path}")
        return entries

    async def _rpc_read(self, params):
        path = params["path"]
        offset = params.get("offset", 0)
        length = params.get("length")
        st = os.stat(path)
        with open(path, "rb") as f:
            if offset:
                f.seek(offset)
            data = f.read(length) if length else f.read()
        return {
            "content": base64.b64encode(data).decode("ascii"),
            "size": st.st_size,
            "mode": st.st_mode & 0o7777,
        }

    async def _rpc_write(self, params):
        path = params["path"]
        content = base64.b64decode(params["content"])
        append = params.get("append", False)
        if params.get("create_parents", False):
            d = os.path.dirname(path)
            if d:
                os.makedirs(d, exist_ok=True)
        with open(path, "ab" if append else "wb") as f:
            f.write(content)
        if "mode" in params:
            os.chmod(path, params["mode"])
        return {"size": len(content)}

    async def _rpc_copy(self, params):
        src, dest = params["src"], params["dest"]
        if os.path.isdir(src):
            shutil.copytree(src, dest, symlinks=True)
        elif params.get("preserve", True):
            shutil.copy2(src, dest)
        else:
            shutil.copy(src, dest)
        return {}

    async def _rpc_rename(self, params):
        os.rename(params["src"], params["dest"])
        return {}

    async def _rpc_delete(self, params):
        os.unlink(params["path"])
        return {}

    async def _rpc_mkdir(self, params):
        path = params["path"]
        mode = params.get("mode", 0o755)
        if params.get("parents", True):
            os.makedirs(path, mode=mode, exist_ok=True)
        else:
            os.mkdir(path, mode)
        return {}

    async def _rpc_rmdir(self, params):
        path = params["path"]
        if params.get("recursive", False):
            shutil.rmtree(path)
        else:
            os.rmdir(path)
        return {}

    async def _rpc_symlink(self, params):
        os.symlink(params["target"], params["link_path"])
        return {}

    async def _rpc_chmod(self, params):
        os.chmod(params["path"], params["mode"])
        return {}

    async def _rpc_chown(self, params):
        os.chown(params["path"], params.get("uid", -1), params.get("gid", -1))
        return {}

    async def _rpc_system_info(self, _params):
        u = os.uname()
        return {"hostname": u.nodename, "sysname": u.sysname,
                "release": u.release, "machine": u.machine,
                "uid": os.getuid(), "gid": os.getgid(),
                "home": os.path.expanduser("~"), "pid": os.getpid()}

    async def _rpc_system_getenv(self, params):
        return {"value": os.environ.get(params["name"])}

    # -- RPC: batch --

    async def _rpc_batch(self, params):
        results = []
        for req in params["requests"]:
            try:
                r = await self._dispatch_rpc(req["method"], req.get("params", {}))
                results.append({"result": r})
            except Exception as e:
                results.append({"error": {"message": str(e)}})
        return {"results": results}

    # -- RPC: run_parallel --

    async def _rpc_run_parallel(self, params):
        tasks = [self._rpc_run(cmd) for cmd in params["commands"]]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        return {"results": [r if not isinstance(r, Exception)
                            else {"error": str(r)} for r in results]}

    # -- RPC: filesystem watching --

    async def _rpc_watch_add(self, params):
        path = params["path"]
        if path in self.watchers:
            return {"status": "already_watching"}
        task = asyncio.ensure_future(
            self._watch_loop(path, params.get("recursive", False)))
        self.watchers[path] = task
        return {"status": "ok"}

    async def _rpc_watch_remove(self, params):
        task = self.watchers.pop(params["path"], None)
        if task:
            task.cancel()
            return {"status": "ok"}
        return {"status": "not_watching"}

    async def _rpc_watch_list(self, _params):
        return {"paths": list(self.watchers.keys())}

    async def _watch_loop(self, path, recursive):
        snapshot = self._scan_dir(path, recursive)
        try:
            while True:
                await asyncio.sleep(2)
                new_snapshot = self._scan_dir(path, recursive)
                changed = [p for p in set(snapshot) | set(new_snapshot)
                           if snapshot.get(p) != new_snapshot.get(p)]
                if changed:
                    await self.send({"type": "notify", "method": "fs.changed",
                                     "params": {"paths": changed}})
                snapshot = new_snapshot
        except asyncio.CancelledError:
            pass

    def _scan_dir(self, path, recursive):
        result = {}
        try:
            with os.scandir(path) as it:
                for entry in it:
                    try:
                        st = entry.stat(follow_symlinks=False)
                        result[entry.path] = st.st_mtime
                        if recursive and entry.is_dir(follow_symlinks=False):
                            result.update(self._scan_dir(entry.path, True))
                    except OSError:
                        pass
        except OSError:
            pass
        return result

    # -- RPC: process management --

    async def _rpc_process_start(self, params):
        cmd = params.get("cmd", "/bin/sh")
        args = params.get("args", [])
        full_cmd = ([cmd] + args) if isinstance(cmd, str) else (list(cmd) + args)
        cwd = params.get("cwd", os.path.expanduser("~"))
        if not os.path.isdir(cwd):
            cwd = os.path.expanduser("~")
        env = os.environ.copy()
        env.update(params.get("env", {}))
        proc = await asyncio.create_subprocess_exec(
            *full_cmd, stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE, cwd=cwd, env=env)
        self.managed_procs[proc.pid] = {
            "proc": proc, "stdout_buf": bytearray(),
            "stderr_buf": bytearray(), "exited": False, "exit_code": None,
        }
        asyncio.ensure_future(self._managed_proc_reader(proc.pid))
        return {"pid": proc.pid}

    async def _managed_proc_reader(self, pid):
        info = self.managed_procs.get(pid)
        if not info:
            return
        proc = info["proc"]
        async def _read(stream, buf):
            try:
                while True:
                    data = await stream.read(4096)
                    if not data:
                        break
                    buf.extend(data)
            except Exception:
                pass
        await asyncio.gather(_read(proc.stdout, info["stdout_buf"]),
                             _read(proc.stderr, info["stderr_buf"]))
        await proc.wait()
        info["exited"] = True
        info["exit_code"] = proc.returncode

    async def _rpc_process_read(self, params):
        pid = params["pid"]
        info = self.managed_procs.get(pid)
        if info is None:
            raise ValueError(f"Process not found: {pid}")
        timeout_ms = params.get("timeout_ms", 0)
        if timeout_ms > 0 and not info["stdout_buf"] and not info["stderr_buf"] and not info["exited"]:
            deadline = time.time() + timeout_ms / 1000.0
            while time.time() < deadline and not info["stdout_buf"] and not info["stderr_buf"] and not info["exited"]:
                await asyncio.sleep(0.01)
        stdout, stderr = bytes(info["stdout_buf"]), bytes(info["stderr_buf"])
        info["stdout_buf"].clear()
        info["stderr_buf"].clear()
        result = {
            "stdout": base64.b64encode(stdout).decode("ascii") if stdout else "",
            "stderr": base64.b64encode(stderr).decode("ascii") if stderr else "",
            "exited": info["exited"],
        }
        if info["exited"]:
            result["exit_code"] = info["exit_code"]
            self.managed_procs.pop(pid, None)
        return result

    async def _rpc_process_write(self, params):
        pid = params["pid"]
        info = self.managed_procs.get(pid)
        if info is None:
            raise ValueError(f"Process not found: {pid}")
        data = params.get("data", "")
        if isinstance(data, str):
            data = base64.b64decode(data)
        info["proc"].stdin.write(data)
        await info["proc"].stdin.drain()
        return {}

    async def _rpc_process_kill(self, params):
        pid = params["pid"]
        info = self.managed_procs.get(pid)
        if info is None:
            raise ValueError(f"Process not found: {pid}")
        info["proc"].send_signal(params.get("signal", 15))
        return {}

    async def _rpc_process_close_stdin(self, params):
        info = self.managed_procs.get(params["pid"])
        if info is None:
            raise ValueError(f"Process not found: {params['pid']}")
        if info["proc"].stdin and not info["proc"].stdin.is_closing():
            info["proc"].stdin.close()
        return {}

    async def _rpc_process_list(self, _params):
        return {"processes": [{"pid": pid, "exited": i["exited"],
                               "exit_code": i["exit_code"]}
                              for pid, i in self.managed_procs.items()]}


def run_server():
    os.makedirs(RSSH_DIR, exist_ok=True)
    log_path = os.path.join(RSSH_DIR, "server.log")
    log_fd = open(log_path, "a")
    os.dup2(log_fd.fileno(), 2)
    server = Server()
    asyncio.run(server.run())


# ---------------------------------------------------------------------------
# Client daemon  (ControlMaster — speaks mux protocol to clients)
# ---------------------------------------------------------------------------

class Daemon:
    """Runs locally. Owns the ssh/et subprocess. Listens on Unix socket.
    Speaks mux protocol (PROTOCOL.mux style) to CLI clients, and
    transport protocol (TLV) to the remote server."""

    def __init__(self, host: str, use_et: bool = False, notify_fd: int = -1):
        self.host = host
        self.use_et = use_et
        self.ssh_proc = None
        self.write_queue = None
        self.next_ch_id = 1
        self.loop = None
        safe_host = host.replace("/", "_").replace("..", "_")
        self.sock_path = os.path.join(RSSH_DIR, f"{safe_host}.sock")
        self.pid_path = os.path.join(RSSH_DIR, f"{safe_host}.pid")
        self.ready = False
        self.notify_fd = notify_fd
        self.start_error = ""
        # Session tracking (mux sessions)
        self.sessions = {}          # session_id -> {ch_id, writer, request_id}
        self.ch_to_session = {}     # ch_id -> session_id
        self.next_session_id = 1
        # File transfer tracking
        self.file_reqs = {}         # req_id -> writer
        # Port forwarding
        self.forwards = {}          # (ftype, lhost, lport) -> {server, chost, cport}
        self.fwd_channels = {}      # ch_id -> {local_writer, ...}
        # Unix socket listener ref for stop_listening
        self._unix_server = None
        # RPC tracking
        self.rpc_reqs = {}          # req_id -> writer
        self.rpc_clients = set()    # all connected client writers (for notifications)

    async def run(self):
        self.loop = asyncio.get_event_loop()
        self.write_queue = asyncio.Queue()
        os.makedirs(RSSH_DIR, exist_ok=True)

        if os.path.exists(self.sock_path):
            try:
                os.unlink(self.sock_path)
            except OSError:
                pass

        if not await self._start_transport():
            self._notify_parent(f"error:{self.start_error}")
            return

        self._notify_parent("ok")
        self._daemonize()

        with open(self.pid_path, "w") as f:
            f.write(str(os.getpid()))

        writer_task = asyncio.ensure_future(self._transport_writer())
        reader_task = asyncio.ensure_future(self._transport_reader())

        self._unix_server = await asyncio.start_unix_server(
            self._handle_client, path=self.sock_path
        )
        os.chmod(self.sock_path, 0o600)

        try:
            await reader_task
        except Exception:
            pass
        finally:
            self._unix_server.close()
            await self._unix_server.wait_closed()
            writer_task.cancel()
            # Close forwarding listeners
            for fwd in self.forwards.values():
                fwd["server"].close()
            self._cleanup()

    async def _start_transport(self) -> bool:
        script_path = os.path.abspath(__file__)
        with open(script_path, "rb") as f:
            script_data = f.read()
        encoded = base64.b64encode(script_data).decode("ascii")

        bootstrap_cmd = (
            'python3 -c "'
            "import sys,base64;"
            "exec(base64.b64decode(sys.stdin.readline().strip()))"
            '"'
        )

        if self.use_et:
            transport_cmd = ["et", self.host, "-c", bootstrap_cmd]
        else:
            transport_cmd = [
                "ssh", "-o", "ControlMaster=no",
                self.host, bootstrap_cmd,
            ]

        try:
            self.ssh_proc = await asyncio.create_subprocess_exec(
                *transport_cmd,
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=None,
            )
        except Exception as e:
            self.start_error = f"failed to start transport: {e}"
            return False

        launcher = (
            f"import sys,base64,os\n"
            f"script = base64.b64decode({repr(encoded)})\n"
            f"sys.argv = ['rssh.py', '--server']\n"
            f"exec(compile(script, 'rssh.py', 'exec'))\n"
        )
        launcher_encoded = base64.b64encode(launcher.encode("utf-8")).decode("ascii")
        self.ssh_proc.stdin.write(launcher_encoded.encode("ascii") + b"\n")
        await self.ssh_proc.stdin.drain()

        try:
            header, _ = await asyncio.wait_for(
                read_frame(self.ssh_proc.stdout), timeout=30
            )
            if header.get("type") != "ready":
                self.start_error = f"unexpected response from server: {header}"
                return False
        except asyncio.TimeoutError:
            self.start_error = "timeout waiting for server (is python3 installed on remote?)"
            return False
        except asyncio.IncompleteReadError:
            self.start_error = "connection closed before server was ready"
            return False
        except Exception as e:
            self.start_error = f"failed to read server ready: {e}"
            return False

        self.ready = True
        return True

    async def _transport_writer(self):
        while True:
            frame = await self.write_queue.get()
            try:
                self.ssh_proc.stdin.write(frame)
                await self.ssh_proc.stdin.drain()
            except (OSError, ConnectionError):
                return

    async def _transport_reader(self):
        try:
            while True:
                header, data = await read_frame(self.ssh_proc.stdout)
                await self._dispatch_from_server(header, data)
        except (EOFError, asyncio.IncompleteReadError, ConnectionError):
            pass

    async def send_to_server(self, header: dict, data: bytes = b""):
        await self.write_queue.put(encode_frame(header, data))

    # -- Dispatch transport responses → mux clients --

    async def _dispatch_from_server(self, header: dict, data: bytes):
        msg_type = header.get("type")
        ch_id = header.get("ch")
        req_id = header.get("req")

        if msg_type == "pong":
            return

        # RPC responses → translate to mux extension
        if msg_type == "rpc_result" and req_id is not None and req_id in self.rpc_reqs:
            writer = self.rpc_reqs.pop(req_id)
            try:
                result_json = json.dumps(header.get("result", {}))
                body = mux_pack_u32(req_id) + mux_pack_string(result_json)
                writer.write(mux_encode_packet(MUX_S_RPC_RESULT, body))
                await writer.drain()
            except (OSError, ConnectionError):
                pass
            return

        # Server push notifications → broadcast to all connected clients
        if msg_type == "notify":
            method = header.get("method", "")
            params_json = json.dumps(header.get("params", {}))
            body = mux_pack_string(method) + mux_pack_string(params_json)
            pkt = mux_encode_packet(MUX_S_NOTIFY, body)
            dead = set()
            for w in self.rpc_clients:
                try:
                    w.write(pkt)
                    await w.drain()
                except (OSError, ConnectionError):
                    dead.add(w)
            self.rpc_clients -= dead
            return

        # File transfer responses → translate to mux extension
        if req_id is not None and req_id in self.file_reqs:
            writer = self.file_reqs[req_id]
            try:
                if msg_type == "file_info":
                    body = (mux_pack_u32(req_id)
                            + mux_pack_u64(header.get("size", 0))
                            + mux_pack_u32(_mode_to_int(header.get("mode", 0o644))))
                    writer.write(mux_encode_packet(MUX_S_FILE_INFO, body))
                elif msg_type == "file_data":
                    body = mux_pack_u32(req_id) + mux_pack_string(data)
                    writer.write(mux_encode_packet(MUX_S_FILE_DATA, body))
                elif msg_type == "file_done":
                    body = mux_pack_u32(req_id)
                    writer.write(mux_encode_packet(MUX_S_FILE_DONE, body))
                elif msg_type == "file_error":
                    body = mux_pack_u32(req_id) + mux_pack_string(header.get("msg", ""))
                    writer.write(mux_encode_packet(MUX_S_FILE_ERROR, body))
                await writer.drain()
            except (OSError, ConnectionError):
                pass
            if msg_type in ("file_done", "file_error"):
                self.file_reqs.pop(req_id, None)
            return

        # Forwarding channel data
        if ch_id is not None and ch_id in self.fwd_channels:
            fwd = self.fwd_channels[ch_id]
            try:
                if msg_type == "opened":
                    fwd["opened"].set()
                elif msg_type == "data":
                    fwd["local_writer"].write(data)
                    await fwd["local_writer"].drain()
                elif msg_type in ("close", "error"):
                    fwd["local_writer"].close()
                    self.fwd_channels.pop(ch_id, None)
            except (OSError, ConnectionError):
                self.fwd_channels.pop(ch_id, None)
            return

        # Session responses → translate to mux messages
        if ch_id is not None and ch_id in self.ch_to_session:
            session_id = self.ch_to_session[ch_id]
            session = self.sessions.get(session_id)
            if session is None:
                return
            writer = session["writer"]
            try:
                if msg_type == "opened":
                    pkt = mux_encode_packet(
                        MUX_S_SESSION_OPENED,
                        _mux_session_opened_body(session["request_id"], session_id))
                    writer.write(pkt)
                    await writer.drain()

                elif msg_type == "data":
                    pkt = mux_encode_packet(
                        MUX_S_SESSION_DATA,
                        _mux_session_data_body(session_id, data))
                    writer.write(pkt)
                    await writer.drain()

                elif msg_type == "close":
                    exit_code = header.get("exit_code", 0)
                    if exit_code < 0:
                        exit_code = 255
                    pkt = mux_encode_packet(
                        MUX_S_EXIT_MESSAGE,
                        _mux_exit_message_body(session_id, exit_code))
                    writer.write(pkt)
                    await writer.drain()
                    self.sessions.pop(session_id, None)
                    self.ch_to_session.pop(ch_id, None)

                elif msg_type == "error":
                    pkt = mux_encode_packet(
                        MUX_S_FAILURE,
                        _mux_failure_body(session["request_id"],
                                          header.get("msg", "error")))
                    writer.write(pkt)
                    await writer.drain()
                    self.sessions.pop(session_id, None)
                    self.ch_to_session.pop(ch_id, None)

            except (OSError, ConnectionError):
                self.sessions.pop(session_id, None)
                self.ch_to_session.pop(ch_id, None)

    # -- Unix socket client handler (mux protocol) --

    async def _handle_client(self, reader: asyncio.StreamReader,
                             writer: asyncio.StreamWriter):
        """Handle a mux client connection. Performs hello exchange,
        then dispatches mux commands."""
        session_ids = set()
        file_req_ids = set()
        rpc_req_ids = set()
        self.rpc_clients.add(writer)
        try:
            # Hello exchange — daemon sends first
            writer.write(mux_encode_packet(MUX_MSG_HELLO, _mux_hello_body()))
            await writer.drain()

            msg_type, body = await mux_read_packet(reader)
            if msg_type != MUX_MSG_HELLO:
                return
            p = MuxParser(body)
            version = p.get_u32()
            if version != MUX_VERSION:
                return
            # Skip extensions
            while p.remaining >= 8:
                try:
                    p.get_cstring()
                    p.get_cstring()
                except (struct.error, IndexError):
                    break

            # Dispatch loop
            while True:
                msg_type, body = await mux_read_packet(reader)
                await self._dispatch_mux(msg_type, body, reader, writer,
                                         session_ids, file_req_ids, rpc_req_ids)
        except (EOFError, asyncio.IncompleteReadError, ConnectionError,
                struct.error, ValueError):
            pass
        finally:
            self.rpc_clients.discard(writer)
            for sid in list(session_ids):
                session = self.sessions.pop(sid, None)
                if session:
                    self.ch_to_session.pop(session.get("ch_id"), None)
            for rid in list(file_req_ids):
                self.file_reqs.pop(rid, None)
            for rid in list(rpc_req_ids):
                self.rpc_reqs.pop(rid, None)
            try:
                writer.close()
            except Exception:
                pass

    async def _dispatch_mux(self, msg_type, body, reader, writer,
                            session_ids, file_req_ids, rpc_req_ids):
        if msg_type == MUX_C_NEW_SESSION:
            await self._mux_new_session(body, writer, session_ids)
        elif msg_type == MUX_C_ALIVE_CHECK:
            await self._mux_alive_check(body, writer)
        elif msg_type == MUX_C_TERMINATE:
            await self._mux_terminate(body, writer)
            raise ConnectionError("terminated")
        elif msg_type == MUX_C_OPEN_FWD:
            await self._mux_open_fwd(body, writer)
        elif msg_type == MUX_C_CLOSE_FWD:
            await self._mux_close_fwd(body, writer)
        elif msg_type == MUX_C_STOP_LISTENING:
            await self._mux_stop_listening(body, writer)
        elif msg_type == MUX_C_SESSION_DATA:
            await self._mux_session_data(body)
        elif msg_type == MUX_C_SESSION_RESIZE:
            await self._mux_session_resize(body)
        elif msg_type == MUX_C_SESSION_EOF:
            await self._mux_session_eof(body)
        elif msg_type == MUX_C_FILE_PUT:
            await self._mux_file_put(body, writer, file_req_ids)
        elif msg_type == MUX_C_FILE_GET:
            await self._mux_file_get(body, writer, file_req_ids)
        elif msg_type == MUX_C_FILE_DATA:
            await self._mux_file_data(body)
        elif msg_type == MUX_C_FILE_DONE:
            await self._mux_file_done(body)
        elif msg_type == MUX_C_RPC:
            await self._mux_rpc(body, writer, rpc_req_ids)
        else:
            # Unknown — try to extract request_id and send failure
            try:
                p = MuxParser(body)
                rid = p.get_u32()
                writer.write(mux_encode_packet(
                    MUX_S_FAILURE,
                    _mux_failure_body(rid, f"unsupported request 0x{msg_type:08x}")))
                await writer.drain()
            except Exception:
                pass

    # -- Session management --

    async def _mux_new_session(self, body, writer, session_ids):
        p = MuxParser(body)
        request_id = p.get_u32()
        _reserved = p.get_string()
        want_tty = p.get_bool()
        _want_x11 = p.get_bool()
        _want_agent = p.get_bool()
        _want_subsystem = p.get_bool()
        _escape_char = p.get_u32()
        terminal_type = p.get_cstring()
        command = p.get_cstring()

        env_vars = {}
        while p.remaining > 0:
            try:
                env_str = p.get_cstring()
                if "=" in env_str:
                    k, v = env_str.split("=", 1)
                    env_vars[k] = v
            except Exception:
                break

        ch_id = self.next_ch_id
        self.next_ch_id += 1
        session_id = self.next_session_id
        self.next_session_id += 1

        cmd = ["bash"] if not command else ["bash", "-c", command]
        if want_tty and terminal_type:
            env_vars.setdefault("TERM", terminal_type)

        rows = int(env_vars.pop("RSSH_ROWS", "24"))
        cols = int(env_vars.pop("RSSH_COLS", "80"))

        transport_header = {
            "type": "open", "ch": ch_id, "pty": want_tty,
            "rows": rows, "cols": cols, "cmd": cmd,
        }
        if env_vars:
            transport_header["env"] = env_vars

        self.sessions[session_id] = {
            "ch_id": ch_id, "writer": writer, "request_id": request_id,
        }
        self.ch_to_session[ch_id] = session_id
        session_ids.add(session_id)

        await self.send_to_server(transport_header)

    async def _mux_session_data(self, body):
        p = MuxParser(body)
        session_id = p.get_u32()
        data = p.get_string()
        session = self.sessions.get(session_id)
        if session:
            await self.send_to_server({"type": "data", "ch": session["ch_id"]}, data)

    async def _mux_session_resize(self, body):
        p = MuxParser(body)
        session_id = p.get_u32()
        rows = p.get_u32()
        cols = p.get_u32()
        session = self.sessions.get(session_id)
        if session:
            await self.send_to_server({
                "type": "resize", "ch": session["ch_id"],
                "rows": rows, "cols": cols,
            })

    async def _mux_session_eof(self, body):
        p = MuxParser(body)
        session_id = p.get_u32()
        session = self.sessions.get(session_id)
        if session:
            await self.send_to_server({"type": "eof", "ch": session["ch_id"]})

    # -- Management --

    async def _mux_alive_check(self, body, writer):
        p = MuxParser(body)
        rid = p.get_u32()
        writer.write(mux_encode_packet(
            MUX_S_ALIVE, _mux_alive_body(rid, os.getpid())))
        await writer.drain()

    async def _mux_terminate(self, body, writer):
        p = MuxParser(body)
        rid = p.get_u32()
        writer.write(mux_encode_packet(MUX_S_OK, _mux_ok_body(rid)))
        await writer.drain()
        if self.ssh_proc:
            self.ssh_proc.terminate()
        self._cleanup()
        asyncio.get_event_loop().stop()

    async def _mux_stop_listening(self, body, writer):
        p = MuxParser(body)
        rid = p.get_u32()
        if self._unix_server:
            self._unix_server.close()
            self._unix_server = None
        writer.write(mux_encode_packet(MUX_S_OK, _mux_ok_body(rid)))
        await writer.drain()

    # -- File transfer --

    async def _mux_file_put(self, body, writer, file_req_ids):
        p = MuxParser(body)
        rid = p.get_u32()
        path = p.get_cstring()
        size = p.get_u64()
        mode = p.get_u32()
        self.file_reqs[rid] = writer
        file_req_ids.add(rid)
        await self.send_to_server({
            "type": "put", "req": rid, "path": path,
            "size": size, "mode": mode,
        })

    async def _mux_file_get(self, body, writer, file_req_ids):
        p = MuxParser(body)
        rid = p.get_u32()
        path = p.get_cstring()
        self.file_reqs[rid] = writer
        file_req_ids.add(rid)
        await self.send_to_server({"type": "get", "req": rid, "path": path})

    async def _mux_file_data(self, body):
        p = MuxParser(body)
        rid = p.get_u32()
        data = p.get_string()
        await self.send_to_server({"type": "file_data", "req": rid}, data)

    async def _mux_file_done(self, body):
        p = MuxParser(body)
        rid = p.get_u32()
        await self.send_to_server({"type": "file_done", "req": rid})

    # -- RPC relay --

    async def _mux_rpc(self, body, writer, rpc_req_ids):
        p = MuxParser(body)
        rid = p.get_u32()
        req_json = p.get_cstring()
        req_obj = json.loads(req_json)
        self.rpc_reqs[rid] = writer
        rpc_req_ids.add(rid)
        await self.send_to_server({
            "type": "rpc", "req": rid,
            "method": req_obj["method"],
            "params": req_obj.get("params", {}),
        })

    # -- Port forwarding --

    async def _mux_open_fwd(self, body, writer):
        p = MuxParser(body)
        rid = p.get_u32()
        fwd_type = p.get_u32()
        listen_host = p.get_cstring()
        listen_port = p.get_u32()
        connect_host = p.get_cstring()
        connect_port = p.get_u32()

        if fwd_type != MUX_FWD_LOCAL:
            writer.write(mux_encode_packet(
                MUX_S_FAILURE,
                _mux_failure_body(rid, "only local forwarding is supported")))
            await writer.drain()
            return

        fwd_key = (fwd_type, listen_host, listen_port)
        if fwd_key in self.forwards:
            writer.write(mux_encode_packet(
                MUX_S_FAILURE,
                _mux_failure_body(rid, "forwarding already active")))
            await writer.drain()
            return

        try:
            bind_host = listen_host or "127.0.0.1"
            server = await asyncio.start_server(
                lambda r, w: self._handle_fwd_connection(
                    r, w, connect_host, connect_port),
                host=bind_host, port=listen_port,
            )
        except Exception as e:
            writer.write(mux_encode_packet(
                MUX_S_FAILURE, _mux_failure_body(rid, str(e))))
            await writer.drain()
            return

        self.forwards[fwd_key] = {
            "server": server,
            "connect_host": connect_host,
            "connect_port": connect_port,
        }

        actual_port = listen_port
        if listen_port == 0 and server.sockets:
            actual_port = server.sockets[0].getsockname()[1]
            resp_body = mux_pack_u32(rid) + mux_pack_u32(actual_port)
            writer.write(mux_encode_packet(MUX_S_REMOTE_PORT, resp_body))
        else:
            writer.write(mux_encode_packet(MUX_S_OK, _mux_ok_body(rid)))
        await writer.drain()

    async def _mux_close_fwd(self, body, writer):
        p = MuxParser(body)
        rid = p.get_u32()
        fwd_type = p.get_u32()
        listen_host = p.get_cstring()
        listen_port = p.get_u32()
        _connect_host = p.get_cstring()
        _connect_port = p.get_u32()

        fwd_key = (fwd_type, listen_host, listen_port)
        fwd = self.forwards.pop(fwd_key, None)
        if fwd:
            fwd["server"].close()
            writer.write(mux_encode_packet(MUX_S_OK, _mux_ok_body(rid)))
        else:
            writer.write(mux_encode_packet(
                MUX_S_FAILURE, _mux_failure_body(rid, "no such forwarding")))
        await writer.drain()

    async def _handle_fwd_connection(self, local_reader, local_writer,
                                     connect_host, connect_port):
        ch_id = self.next_ch_id
        self.next_ch_id += 1

        opened_event = asyncio.Event()
        self.fwd_channels[ch_id] = {
            "local_writer": local_writer,
            "opened": opened_event,
        }

        await self.send_to_server({
            "type": "open", "ch": ch_id, "pty": False,
            "rows": 24, "cols": 80,
            "cmd": ["nc", connect_host, str(connect_port)],
        })

        try:
            await asyncio.wait_for(opened_event.wait(), timeout=15)
        except asyncio.TimeoutError:
            self.fwd_channels.pop(ch_id, None)
            local_writer.close()
            return

        # Relay local → remote
        try:
            while True:
                data = await local_reader.read(4096)
                if not data:
                    await self.send_to_server({"type": "eof", "ch": ch_id})
                    break
                await self.send_to_server({"type": "data", "ch": ch_id}, data)
        except (OSError, ConnectionError):
            pass
        finally:
            self.fwd_channels.pop(ch_id, None)

    # -- Daemon lifecycle --

    def _notify_parent(self, msg: str):
        if self.notify_fd >= 0:
            try:
                os.write(self.notify_fd, msg.encode())
            except OSError:
                pass
            try:
                os.close(self.notify_fd)
            except OSError:
                pass
            self.notify_fd = -1

    def _daemonize(self):
        os.setsid()
        devnull = os.open(os.devnull, os.O_RDWR)
        os.dup2(devnull, 0)
        log_path = os.path.join(RSSH_DIR, f"{self.host}.daemon.log")
        log_fd = os.open(log_path, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o600)
        os.dup2(log_fd, 1)
        os.dup2(log_fd, 2)
        if devnull > 2:
            os.close(devnull)
        if log_fd > 2:
            os.close(log_fd)

    def _cleanup(self):
        try:
            os.unlink(self.sock_path)
        except OSError:
            pass
        try:
            os.unlink(self.pid_path)
        except OSError:
            pass


def run_daemon(host: str, use_et: bool = False, foreground: bool = False):
    os.makedirs(RSSH_DIR, exist_ok=True)

    if foreground:
        daemon = Daemon(host, use_et)
        try:
            asyncio.run(daemon.run())
        except KeyboardInterrupt:
            daemon._cleanup()
        return

    r_fd, w_fd = os.pipe()
    pid = os.fork()

    if pid > 0:
        os.close(w_fd)
        data = b""
        while True:
            chunk = os.read(r_fd, 4096)
            if not chunk:
                break
            data += chunk
        os.close(r_fd)

        msg = data.decode(errors="replace").strip()
        if msg == "ok":
            print(f"rssh: connected to {host} (daemon pid {pid})", file=sys.stderr)
        elif msg.startswith("error:"):
            print(f"rssh: {msg[6:]}", file=sys.stderr)
            try:
                os.waitpid(pid, 0)
            except ChildProcessError:
                pass
            sys.exit(1)
        else:
            print(f"rssh: {msg or 'daemon exited unexpectedly'}", file=sys.stderr)
            sys.exit(1)
        return

    os.close(r_fd)
    signal.signal(signal.SIGHUP, signal.SIG_IGN)
    daemon = Daemon(host, use_et, notify_fd=w_fd)
    try:
        asyncio.run(daemon.run())
    except KeyboardInterrupt:
        pass
    daemon._cleanup()
    os._exit(0)


# ---------------------------------------------------------------------------
# Client — mux protocol helpers
# ---------------------------------------------------------------------------

def ensure_daemon(host: str, use_et: bool = False) -> str:
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if os.path.exists(sock_path):
        try:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.connect(sock_path)
            sock.close()
            return sock_path
        except (ConnectionRefusedError, OSError):
            try:
                os.unlink(sock_path)
            except OSError:
                pass

    print(f"rssh: connecting to {host}...", file=sys.stderr)
    run_daemon(host, use_et)

    for _ in range(60):
        time.sleep(0.5)
        if os.path.exists(sock_path):
            try:
                sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                sock.connect(sock_path)
                sock.close()
                return sock_path
            except (ConnectionRefusedError, OSError):
                continue

    print("rssh: failed to connect to daemon", file=sys.stderr)
    sys.exit(1)


def mux_connect(sock_path: str) -> socket.socket:
    """Connect to daemon and perform mux hello exchange."""
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect(sock_path)

    # Read daemon's hello
    msg_type, body = mux_read_packet_sync(sock)
    if msg_type != MUX_MSG_HELLO:
        sock.close()
        raise ConnectionError("expected MUX_MSG_HELLO from daemon")
    p = MuxParser(body)
    version = p.get_u32()
    if version != MUX_VERSION:
        sock.close()
        raise ConnectionError(f"unsupported mux version: {version}")

    # Send our hello
    mux_write_packet_sync(sock, MUX_MSG_HELLO, _mux_hello_body())
    return sock


def _mux_request_alive(sock: socket.socket, rid: int = 1) -> int:
    """Send alive check, return daemon PID."""
    mux_write_packet_sync(sock, MUX_C_ALIVE_CHECK, mux_pack_u32(rid))
    msg_type, body = mux_read_packet_sync(sock)
    if msg_type == MUX_S_ALIVE:
        p = MuxParser(body)
        p.get_u32()  # rid
        return p.get_u32()  # pid
    return 0


# ---------------------------------------------------------------------------
# Client CLI — interactive shell / command execution
# ---------------------------------------------------------------------------

def run_cli(host: str, command=None, force_pty=None, env_vars=None, use_et=False):
    sock_path = ensure_daemon(host, use_et)
    sock = mux_connect(sock_path)

    is_tty = os.isatty(sys.stdin.fileno())
    if force_pty is True:
        use_pty = True
    elif force_pty is False:
        use_pty = False
    elif command is None:
        use_pty = is_tty
    else:
        use_pty = is_tty

    # Build MUX_C_NEW_SESSION body (matching SSH PROTOCOL.mux)
    request_id = 1
    cmd_str = ""
    if command:
        cmd_str = " ".join(command) if isinstance(command, list) else command
    term = os.environ.get("TERM", "xterm-256color") if use_pty else ""

    if not cmd_str and not command:
        shell = os.environ.get("SHELL", "bash")
        cmd_str = os.path.basename(shell)

    body = mux_pack_u32(request_id)
    body += mux_pack_string(b"")          # reserved
    body += mux_pack_bool(use_pty)        # want tty
    body += mux_pack_bool(False)          # want X11
    body += mux_pack_bool(False)          # want agent
    body += mux_pack_bool(False)          # subsystem
    body += mux_pack_u32(0xFFFFFFFF)      # no escape char
    body += mux_pack_string(term)         # terminal type
    body += mux_pack_string(cmd_str)      # command

    # Environment strings (KEY=VAL)
    env_list = []
    if env_vars:
        for k, v in env_vars:
            env_list.append(f"{k}={v}")
    if use_pty and is_tty:
        rows, cols = _get_terminal_size()
        env_list.append(f"RSSH_ROWS={rows}")
        env_list.append(f"RSSH_COLS={cols}")
    for env_str in env_list:
        body += mux_pack_string(env_str)

    mux_write_packet_sync(sock, MUX_C_NEW_SESSION, body)

    # Read response
    msg_type, resp_body = mux_read_packet_sync(sock)
    if msg_type == MUX_S_SESSION_OPENED:
        p = MuxParser(resp_body)
        p.get_u32()  # rid
        session_id = p.get_u32()
    elif msg_type == MUX_S_FAILURE:
        p = MuxParser(resp_body)
        p.get_u32()  # rid
        reason = p.get_cstring()
        print(f"rssh: {reason}", file=sys.stderr)
        sock.close()
        sys.exit(1)
    else:
        print(f"rssh: unexpected response: 0x{msg_type:08x}", file=sys.stderr)
        sock.close()
        sys.exit(1)

    # Send initial resize
    if use_pty and is_tty:
        rows, cols = _get_terminal_size()
        resize_body = mux_pack_u32(session_id) + mux_pack_u32(rows) + mux_pack_u32(cols)
        mux_write_packet_sync(sock, MUX_C_SESSION_RESIZE, resize_body)

    # Terminal setup
    old_settings = None
    if use_pty and is_tty:
        old_settings = termios.tcgetattr(sys.stdin.fileno())
        tty.setraw(sys.stdin.fileno())
        def on_winch(signum, frame):
            rows, cols = _get_terminal_size()
            try:
                rb = mux_pack_u32(session_id) + mux_pack_u32(rows) + mux_pack_u32(cols)
                mux_write_packet_sync(sock, MUX_C_SESSION_RESIZE, rb)
            except Exception:
                pass
        signal.signal(signal.SIGWINCH, on_winch)

    exit_code = 0
    try:
        exit_code = _relay_io_mux(sock, session_id, use_pty and is_tty)
    finally:
        if old_settings is not None:
            termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, old_settings)
        sock.close()

    sys.exit(exit_code)


def _strip_osc(data: bytes, state: list) -> bytes:
    """Strip OSC escape sequences from terminal output."""
    if not state[0] and b'\x1b]' not in data:
        return data
    out = bytearray()
    i = 0
    in_osc = state[0]
    prev_esc = state[1]
    while i < len(data):
        b = data[i]
        if in_osc:
            if b == 0x07:
                in_osc = False
            elif prev_esc and b == 0x5c:
                in_osc = False
                prev_esc = False
            else:
                prev_esc = (b == 0x1b)
        else:
            if b == 0x1b and i + 1 < len(data) and data[i + 1] == 0x5d:
                in_osc = True
                prev_esc = False
                i += 2
                continue
            elif b == 0x1b and i + 1 == len(data):
                state[0] = False
                state[1] = True
                i += 1
                continue
            elif state[1] and b == 0x5d:
                in_osc = True
                state[1] = False
                i += 1
                continue
            else:
                if state[1]:
                    out.append(0x1b)
                    state[1] = False
                out.append(b)
        i += 1
    state[0] = in_osc
    if not in_osc:
        state[1] = prev_esc if not in_osc else False
    return bytes(out)


def _relay_io_mux(sock: socket.socket, session_id: int, raw_mode: bool) -> int:
    """Relay I/O between local terminal and daemon using mux protocol."""
    exit_code = 0
    sock.setblocking(False)

    stdin_fd = sys.stdin.fileno()
    stdout_fd = sys.stdout.fileno()
    sock_fd = sock.fileno()

    sock_buf = bytearray()
    osc_state = [False, False]

    # In non-raw mode (pipe/TRAMP), intercept job-control signals and
    # forward the corresponding control characters to the remote session
    # instead of letting them kill the local rssh process.
    pending_ctrl = []
    old_handlers = {}
    if not raw_mode:
        _SIG_CTRL = {
            signal.SIGINT: b'\x03',   # C-c
            signal.SIGTSTP: b'\x1a',  # C-z
            signal.SIGQUIT: b'\x1c',  # C-\
            signal.SIGHUP: None,      # ignore — stdin close handles cleanup
        }
        for sig, ctrl in _SIG_CTRL.items():
            try:
                if ctrl is None:
                    old_handlers[sig] = signal.signal(sig, signal.SIG_IGN)
                else:
                    def _handler(signum, frame, _c=ctrl):
                        pending_ctrl.append(_c)
                    old_handlers[sig] = signal.signal(sig, _handler)
            except OSError:
                pass

    running = True
    while running:
        # Forward any pending control characters from intercepted signals
        while pending_ctrl:
            ctrl = pending_ctrl.pop(0)
            try:
                sock.setblocking(True)
                mux_write_packet_sync(
                    sock, MUX_C_SESSION_DATA,
                    _mux_session_data_body(session_id, ctrl))
                sock.setblocking(False)
            except Exception:
                running = False
                break
        if not running:
            break

        try:
            readable, _, _ = select.select([stdin_fd, sock_fd], [], [], 1.0)
        except (InterruptedError, OSError):
            continue

        for fd in readable:
            if fd == stdin_fd:
                try:
                    data = os.read(stdin_fd, 4096)
                except OSError:
                    data = b""
                if not data:
                    try:
                        sock.setblocking(True)
                        mux_write_packet_sync(sock, MUX_C_SESSION_EOF,
                                              mux_pack_u32(session_id))
                        sock.setblocking(False)
                    except Exception:
                        pass
                    continue
                try:
                    sock.setblocking(True)
                    mux_write_packet_sync(
                        sock, MUX_C_SESSION_DATA,
                        _mux_session_data_body(session_id, data))
                    sock.setblocking(False)
                except Exception:
                    running = False
                    break

            elif fd == sock_fd:
                try:
                    chunk = sock.recv(65536)
                except BlockingIOError:
                    continue
                except OSError:
                    running = False
                    break
                if not chunk:
                    running = False
                    break
                sock_buf.extend(chunk)

                # Process complete mux packets
                while len(sock_buf) >= 4:
                    pkt_len, = struct.unpack("!I", sock_buf[:4])
                    if len(sock_buf) < 4 + pkt_len:
                        break
                    payload = bytes(sock_buf[4:4 + pkt_len])
                    del sock_buf[:4 + pkt_len]

                    if len(payload) < 4:
                        continue
                    mt, = struct.unpack_from("!I", payload, 0)
                    pkt_body = payload[4:]

                    if mt == MUX_S_SESSION_DATA:
                        p = MuxParser(pkt_body)
                        p.get_u32()  # sid
                        data = p.get_string()
                        try:
                            out = data if raw_mode else _strip_osc(data, osc_state)
                            if out:
                                os.write(stdout_fd, out)
                        except OSError:
                            pass

                    elif mt == MUX_S_EXIT_MESSAGE:
                        p = MuxParser(pkt_body)
                        p.get_u32()  # sid
                        exit_code = p.get_u32()
                        if exit_code > 255:
                            exit_code = 255
                        running = False
                        break

                    elif mt == MUX_S_FAILURE:
                        p = MuxParser(pkt_body)
                        p.get_u32()  # rid
                        reason = p.get_cstring()
                        sys.stderr.write(f"\rrssh: {reason}\n")
                        running = False
                        break

    for sig, handler in old_handlers.items():
        signal.signal(sig, handler)
    return exit_code


def _get_terminal_size():
    try:
        size = os.get_terminal_size(sys.stdin.fileno())
        return size.lines, size.columns
    except OSError:
        return 24, 80


# ---------------------------------------------------------------------------
# Pipe mode — for TRAMP and scripted use
# ---------------------------------------------------------------------------

def run_pipe(host: str, command=None, env_vars=None, use_et: bool = False):
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")

    if os.path.exists(sock_path):
        try:
            sock = mux_connect(sock_path)
            exit_code = _pipe_via_daemon(sock, command, env_vars)
            sys.exit(exit_code)
        except (ConnectionRefusedError, OSError):
            pass

    exit_code = asyncio.run(_pipe_inline(host, command, env_vars, use_et))
    sys.exit(exit_code)


def _pipe_via_daemon(sock: socket.socket, command=None, env_vars=None) -> int:
    """Open a session through existing daemon using mux protocol."""
    env = {}
    if env_vars:
        env.update(dict(env_vars))
    use_pty = (not command) or os.isatty(sys.stdin.fileno())

    request_id = 1
    cmd_str = ""
    if command:
        cmd_str = " ".join(command) if isinstance(command, list) else command
    else:
        env["TERM"] = "dumb"
        cmd_str = "bash --norc --noprofile"

    body = mux_pack_u32(request_id)
    body += mux_pack_string(b"")
    body += mux_pack_bool(use_pty)
    body += mux_pack_bool(False)
    body += mux_pack_bool(False)
    body += mux_pack_bool(False)
    body += mux_pack_u32(0xFFFFFFFF)
    body += mux_pack_string(env.get("TERM", ""))
    body += mux_pack_string(cmd_str)
    for k, v in env.items():
        body += mux_pack_string(f"{k}={v}")

    mux_write_packet_sync(sock, MUX_C_NEW_SESSION, body)

    msg_type, resp_body = mux_read_packet_sync(sock)
    if msg_type != MUX_S_SESSION_OPENED:
        sock.close()
        return 1

    p = MuxParser(resp_body)
    p.get_u32()  # rid
    session_id = p.get_u32()

    # When running a command with a tty stdin (e.g. TRAMP direct-async for
    # eat), put the local terminal into raw mode so characters are delivered
    # immediately instead of being line-buffered by the PTY's canonical mode.
    # Skip this for the control connection (no command) so TRAMP's shell
    # interaction continues to work in cooked mode.
    is_tty = os.isatty(sys.stdin.fileno())
    raw_mode = bool(command) and use_pty and is_tty
    old_settings = None
    if raw_mode:
        old_settings = termios.tcgetattr(sys.stdin.fileno())
        tty.setraw(sys.stdin.fileno())
    try:
        exit_code = _relay_io_mux(sock, session_id, raw_mode)
    finally:
        if old_settings is not None:
            termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, old_settings)
    sock.close()
    return exit_code


async def _pipe_inline(host: str, command=None, env_vars=None,
                       use_et: bool = False) -> int:
    """Direct SSH connection without daemon. Uses transport protocol directly."""
    loop = asyncio.get_event_loop()

    script_path = os.path.abspath(__file__)
    with open(script_path, "rb") as f:
        script_data = f.read()
    encoded = base64.b64encode(script_data).decode("ascii")

    bootstrap_cmd = (
        'python3 -c "'
        "import sys,base64;"
        "exec(base64.b64decode(sys.stdin.readline().strip()))"
        '"'
    )

    if use_et:
        transport_cmd = ["et", host, "-c", bootstrap_cmd]
    else:
        transport_cmd = ["ssh", "-o", "ControlMaster=no", host, bootstrap_cmd]

    try:
        proc = await asyncio.create_subprocess_exec(
            *transport_cmd,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=None,
        )
    except Exception:
        return 1

    launcher = (
        f"import sys,base64,os\n"
        f"script = base64.b64decode({repr(encoded)})\n"
        f"sys.argv = ['rssh.py', '--server']\n"
        f"exec(compile(script, 'rssh.py', 'exec'))\n"
    )
    launcher_encoded = base64.b64encode(launcher.encode("utf-8")).decode("ascii")
    proc.stdin.write(launcher_encoded.encode("ascii") + b"\n")
    await proc.stdin.drain()

    try:
        header, _ = await asyncio.wait_for(read_frame(proc.stdout), timeout=30)
        if header.get("type") != "ready":
            return 1
    except (asyncio.TimeoutError, asyncio.IncompleteReadError, EOFError):
        return 1

    env = {}
    if env_vars:
        env.update(dict(env_vars))
    use_pty = (not command) or os.isatty(sys.stdin.fileno())
    open_header = {
        "type": "open", "ch": 1, "pty": use_pty,
        "env": env, "rows": 24, "cols": 80,
    }
    if command:
        open_header["cmd"] = command if isinstance(command, list) else ["bash", "-c", command]
    else:
        env["TERM"] = "dumb"
        open_header["cmd"] = ["bash", "--norc", "--noprofile"]

    proc.stdin.write(encode_frame(open_header))
    await proc.stdin.drain()

    try:
        header, _ = await read_frame(proc.stdout)
        if header.get("type") != "opened":
            return 1
    except (asyncio.IncompleteReadError, EOFError):
        return 1

    stdin_fd = sys.stdin.fileno()
    stdout_fd = sys.stdout.fileno()
    exit_code = 0
    done = asyncio.Event()

    # Same raw mode logic as _pipe_via_daemon: enter raw mode when running
    # a command with a tty stdin (TRAMP direct-async for eat).
    is_tty = os.isatty(stdin_fd)
    raw_mode = bool(command) and use_pty and is_tty
    old_settings = None
    if raw_mode:
        old_settings = termios.tcgetattr(stdin_fd)
        tty.setraw(stdin_fd)

    async def relay_stdin():
        try:
            while not done.is_set():
                try:
                    data = await loop.run_in_executor(None, os.read, stdin_fd, 4096)
                except OSError:
                    break
                if not data:
                    proc.stdin.write(encode_frame({"type": "eof", "ch": 1}))
                    await proc.stdin.drain()
                    break
                proc.stdin.write(encode_frame({"type": "data", "ch": 1}, data))
                await proc.stdin.drain()
        except (OSError, ConnectionError):
            pass

    async def relay_stdout():
        nonlocal exit_code
        try:
            while True:
                header, data = await read_frame(proc.stdout)
                msg_type = header.get("type")
                if msg_type == "data":
                    os.write(stdout_fd, data)
                elif msg_type == "close":
                    exit_code = header.get("exit_code", 0)
                    break
        except (asyncio.IncompleteReadError, EOFError, OSError):
            pass
        done.set()

    # Intercept job-control signals: forward control chars to remote
    _SIG_CTRL_INLINE = {
        signal.SIGINT: b'\x03',
        signal.SIGTSTP: b'\x1a',
        signal.SIGQUIT: b'\x1c',
    }
    for sig, ctrl in _SIG_CTRL_INLINE.items():
        try:
            loop.add_signal_handler(
                sig, lambda c=ctrl: proc.stdin.write(
                    encode_frame({"type": "data", "ch": 1}, c)))
        except (OSError, NotImplementedError):
            pass
    try:
        loop.add_signal_handler(signal.SIGHUP, lambda: None)
    except (OSError, NotImplementedError):
        pass

    stdout_task = asyncio.ensure_future(relay_stdout())
    stdin_task = asyncio.ensure_future(relay_stdin())
    await stdout_task
    stdin_task.cancel()

    if old_settings is not None:
        termios.tcsetattr(stdin_fd, termios.TCSADRAIN, old_settings)

    try:
        proc.terminate()
    except ProcessLookupError:
        pass
    return exit_code


# ---------------------------------------------------------------------------
# File transfer (mux protocol)
# ---------------------------------------------------------------------------

def run_cp(args: list, use_et: bool = False):
    if len(args) != 2:
        print("Usage: rssh cp <src> <dst>", file=sys.stderr)
        print("  rssh cp host:/remote/path /local/path   (download)", file=sys.stderr)
        print("  rssh cp /local/path host:/remote/path   (upload)", file=sys.stderr)
        sys.exit(1)

    src, dst = args
    src_host, src_path = _parse_host_path(src)
    dst_host, dst_path = _parse_host_path(dst)

    if src_host and dst_host:
        print("rssh: cannot copy between two remote hosts", file=sys.stderr)
        sys.exit(1)
    if not src_host and not dst_host:
        print("rssh: at least one path must be remote (host:path)", file=sys.stderr)
        sys.exit(1)

    if src_host:
        _download(src_host, src_path, dst_path or dst, use_et)
    else:
        _upload(dst_host, src_path or src, dst_path, use_et)


def _parse_host_path(spec: str):
    if ":" in spec and not spec.startswith("/"):
        parts = spec.split(":", 1)
        return parts[0], parts[1]
    return None, None


def _download(host: str, remote_path: str, local_path: str, use_et: bool):
    sock_path = ensure_daemon(host, use_et)
    sock = mux_connect(sock_path)

    req_id = int(time.time() * 1000) % 1000000
    body = mux_pack_u32(req_id) + mux_pack_string(remote_path)
    mux_write_packet_sync(sock, MUX_C_FILE_GET, body)

    received = 0
    total_size = 0
    fd = None

    try:
        while True:
            msg_type, resp_body = mux_read_packet_sync(sock)
            p = MuxParser(resp_body)

            if msg_type == MUX_S_FILE_INFO:
                p.get_u32()  # req
                total_size = p.get_u64()
                _mode = p.get_u32()
                fd = open(local_path, "wb")
            elif msg_type == MUX_S_FILE_DATA:
                p.get_u32()  # req
                data = p.get_string()
                if fd is None:
                    fd = open(local_path, "wb")
                fd.write(data)
                received += len(data)
                if total_size > 0:
                    pct = received * 100 // total_size
                    sys.stderr.write(f"\rrssh: downloading {received}/{total_size} ({pct}%)")
                    sys.stderr.flush()
            elif msg_type == MUX_S_FILE_DONE:
                if fd:
                    fd.close()
                sys.stderr.write(f"\rrssh: downloaded {received} bytes\n")
                break
            elif msg_type == MUX_S_FILE_ERROR:
                p.get_u32()  # req
                reason = p.get_cstring()
                if fd:
                    fd.close()
                    os.unlink(local_path)
                print(f"\nrssh: {reason}", file=sys.stderr)
                sys.exit(1)
            elif msg_type == MUX_S_FAILURE:
                p.get_u32()  # rid
                reason = p.get_cstring()
                print(f"\nrssh: {reason}", file=sys.stderr)
                sys.exit(1)
    finally:
        sock.close()


def _upload(host: str, local_path: str, remote_path: str, use_et: bool):
    if not os.path.isfile(local_path):
        print(f"rssh: {local_path}: no such file", file=sys.stderr)
        sys.exit(1)

    stat = os.stat(local_path)
    size = stat.st_size
    mode = stat.st_mode & 0o7777

    sock_path = ensure_daemon(host, use_et)
    sock = mux_connect(sock_path)

    req_id = int(time.time() * 1000) % 1000000

    body = (mux_pack_u32(req_id) + mux_pack_string(remote_path)
            + mux_pack_u64(size) + mux_pack_u32(mode))
    mux_write_packet_sync(sock, MUX_C_FILE_PUT, body)

    sent = 0
    with open(local_path, "rb") as f:
        while True:
            chunk = f.read(CHUNK_SIZE)
            if not chunk:
                break
            data_body = mux_pack_u32(req_id) + mux_pack_string(chunk)
            mux_write_packet_sync(sock, MUX_C_FILE_DATA, data_body)
            sent += len(chunk)
            pct = sent * 100 // size if size > 0 else 100
            sys.stderr.write(f"\rrssh: uploading {sent}/{size} ({pct}%)")
            sys.stderr.flush()

    mux_write_packet_sync(sock, MUX_C_FILE_DONE, mux_pack_u32(req_id))

    try:
        msg_type, resp_body = mux_read_packet_sync(sock)
        if msg_type == MUX_S_FILE_ERROR:
            p = MuxParser(resp_body)
            p.get_u32()
            reason = p.get_cstring()
            print(f"\nrssh: {reason}", file=sys.stderr)
            sys.exit(1)
        sys.stderr.write(f"\rrssh: uploaded {sent} bytes\n")
    except EOFError:
        sys.stderr.write(f"\rrssh: uploaded {sent} bytes (no ack)\n")
    finally:
        sock.close()


# ---------------------------------------------------------------------------
# Management commands (mux protocol)
# ---------------------------------------------------------------------------

def run_status(host: str = None):
    if host:
        _status_one(host)
    else:
        if not os.path.exists(RSSH_DIR):
            print("rssh: no connections")
            return
        found = False
        for f in os.listdir(RSSH_DIR):
            if f.endswith(".sock"):
                found = True
                _status_one(f[:-5])
        if not found:
            print("rssh: no connections")


def _status_one(host: str):
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if not os.path.exists(sock_path):
        print(f"{host}: not connected")
        return
    try:
        sock = mux_connect(sock_path)
        pid = _mux_request_alive(sock)
        sock.close()
        if pid:
            print(f"{host}: connected (daemon pid {pid})")
        else:
            print(f"{host}: unknown status")
    except (ConnectionRefusedError, OSError, EOFError):
        print(f"{host}: not connected (stale socket)")
        try:
            os.unlink(sock_path)
        except OSError:
            pass


def run_list():
    if not os.path.exists(RSSH_DIR):
        print("rssh: no connections")
        return
    found = False
    for f in sorted(os.listdir(RSSH_DIR)):
        if f.endswith(".sock"):
            host = f[:-5]
            found = True
            sock_path = os.path.join(RSSH_DIR, f)
            try:
                sock = mux_connect(sock_path)
                pid = _mux_request_alive(sock)
                sock.close()
                print(f"{host}  (pid {pid})" if pid else f"{host}  (unknown)")
            except (ConnectionRefusedError, OSError, EOFError):
                print(f"{host}  (stale)")
                try:
                    os.unlink(sock_path)
                except OSError:
                    pass
    if not found:
        print("rssh: no connections")


def run_disconnect(host: str):
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if not os.path.exists(sock_path):
        print(f"rssh: {host}: not connected")
        return

    try:
        sock = mux_connect(sock_path)
        mux_write_packet_sync(sock, MUX_C_TERMINATE, mux_pack_u32(1))
        try:
            mux_read_packet_sync(sock)
        except EOFError:
            pass
        sock.close()
        print(f"rssh: disconnected from {host}")
    except (ConnectionRefusedError, OSError):
        print(f"rssh: {host}: not connected (cleaning up stale socket)")
        try:
            os.unlink(sock_path)
        except OSError:
            pass

    pid_path = os.path.join(RSSH_DIR, f"{host}.pid")
    try:
        os.unlink(pid_path)
    except OSError:
        pass


# ---------------------------------------------------------------------------
# Port forwarding
# ---------------------------------------------------------------------------

def run_forward(host: str, forward_specs: list, use_et: bool = False):
    """Set up port forwarding through the daemon."""
    sock_path = ensure_daemon(host, use_et)
    sock = mux_connect(sock_path)
    request_id = 1

    for fwd_type_str, spec in forward_specs:
        parts = spec.split(":")
        if len(parts) == 3:
            listen_host = "127.0.0.1"
            listen_port = int(parts[0])
            connect_host = parts[1]
            connect_port = int(parts[2])
        elif len(parts) == 4:
            listen_host = parts[0]
            listen_port = int(parts[1])
            connect_host = parts[2]
            connect_port = int(parts[3])
        else:
            print(f"rssh: invalid forward spec: {spec}", file=sys.stderr)
            sys.exit(1)

        body = (mux_pack_u32(request_id) + mux_pack_u32(MUX_FWD_LOCAL)
                + mux_pack_string(listen_host) + mux_pack_u32(listen_port)
                + mux_pack_string(connect_host) + mux_pack_u32(connect_port))
        mux_write_packet_sync(sock, MUX_C_OPEN_FWD, body)

        msg_type, resp = mux_read_packet_sync(sock)
        p = MuxParser(resp)
        p.get_u32()  # rid

        if msg_type == MUX_S_OK:
            print(f"rssh: forwarding {listen_host}:{listen_port} -> "
                  f"{connect_host}:{connect_port}", file=sys.stderr)
        elif msg_type == MUX_S_REMOTE_PORT:
            actual_port = p.get_u32()
            print(f"rssh: forwarding {listen_host}:{actual_port} -> "
                  f"{connect_host}:{connect_port}", file=sys.stderr)
        elif msg_type == MUX_S_FAILURE:
            reason = p.get_cstring()
            print(f"rssh: forwarding failed: {reason}", file=sys.stderr)
            sys.exit(1)

        request_id += 1
    sock.close()


# ---------------------------------------------------------------------------
# Mux control (-O) commands
# ---------------------------------------------------------------------------

def run_mux_control(host: str, ctl_cmd: str, use_et: bool = False):
    """Handle -O check|stop|exit."""
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if not os.path.exists(sock_path):
        print(f"rssh: {host}: not connected", file=sys.stderr)
        sys.exit(255)

    if ctl_cmd == "check":
        try:
            sock = mux_connect(sock_path)
            pid = _mux_request_alive(sock)
            sock.close()
            print(f"Master running (pid={pid})")
            sys.exit(0)
        except (ConnectionRefusedError, OSError, EOFError):
            print(f"rssh: {host}: not connected", file=sys.stderr)
            sys.exit(255)

    elif ctl_cmd in ("stop", "exit"):
        try:
            sock = mux_connect(sock_path)
            if ctl_cmd == "stop":
                mux_write_packet_sync(sock, MUX_C_STOP_LISTENING, mux_pack_u32(1))
            else:
                mux_write_packet_sync(sock, MUX_C_TERMINATE, mux_pack_u32(1))
            try:
                mux_read_packet_sync(sock)
            except EOFError:
                pass
            sock.close()
            print(f"{'Stop listening' if ctl_cmd == 'stop' else 'Exit'} request sent.")
        except (ConnectionRefusedError, OSError, EOFError):
            print(f"rssh: {host}: not connected", file=sys.stderr)
            sys.exit(255)

    else:
        print(f"rssh: unknown -O command: {ctl_cmd}", file=sys.stderr)
        sys.exit(1)


# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# RPC mode — JSON-RPC over stdin/stdout via daemon
# ---------------------------------------------------------------------------

def run_rpc(host: str, use_et: bool = False):
    """Read JSON-RPC requests from stdin (one per line), relay through
    daemon, write JSON-RPC responses to stdout (one per line).
    Includes request id in responses for correlation."""
    sock_path = ensure_daemon(host, use_et)
    sock = mux_connect(sock_path)
    request_id = 0

    try:
        while True:
            line = sys.stdin.readline()
            if not line:
                break
            line = line.strip()
            if not line:
                continue
            try:
                req = json.loads(line)
            except json.JSONDecodeError as e:
                print(json.dumps({"id": None,
                                  "error": {"message": f"Invalid JSON: {e}"}}),
                      flush=True)
                continue

            request_id += 1
            req_json = json.dumps(req)
            body = mux_pack_u32(request_id) + mux_pack_string(req_json)
            mux_write_packet_sync(sock, MUX_C_RPC, body)

            # Read response (may get notifications too)
            while True:
                msg_type, resp_body = mux_read_packet_sync(sock)
                if msg_type == MUX_S_RPC_RESULT:
                    p = MuxParser(resp_body)
                    rid = p.get_u32()
                    result_json = p.get_cstring()
                    # Wrap with id for correlation
                    result_obj = json.loads(result_json)
                    result_obj["id"] = rid
                    print(json.dumps(result_obj), flush=True)
                    break
                elif msg_type == MUX_S_NOTIFY:
                    p = MuxParser(resp_body)
                    method = p.get_cstring()
                    params_json = p.get_cstring()
                    notify = {"notification": method,
                              "params": json.loads(params_json)}
                    print(json.dumps(notify), flush=True)
                elif msg_type == MUX_S_FAILURE:
                    p = MuxParser(resp_body)
                    p.get_u32()  # rid
                    reason = p.get_cstring()
                    print(json.dumps({"id": request_id,
                                      "error": {"message": reason}}),
                          flush=True)
                    break
                else:
                    break
    except (EOFError, KeyboardInterrupt):
        pass
    finally:
        sock.close()


def print_usage():
    print("""rssh — SSH Multiplexer over a Single Connection

Usage:
  rssh connect <host> [--et]       Establish connection, start daemon
  rssh disconnect <host>           Tear down connection
  rssh status [<host>]             Show connection status
  rssh list                        List all active connections

  rssh <host>                      Interactive shell (auto-connects)
  rssh <host> <command> [args...]  Run command (auto-connects)
  rssh <host> -t <command>         Force PTY
  rssh -T <host> <command>         Disable PTY
  rssh -l <user> <host>            Login as user
  rssh --pipe <host>               Pipe mode (for TRAMP/scripts)
  rssh --rpc <host>                RPC mode (JSON-RPC over stdin/stdout)

  rssh cp <host>:<path> <local>    Download
  rssh cp <local> <host>:<path>    Upload

  rssh -L [bind:]port:host:port <host>   Local port forwarding
  rssh -O check|stop|exit <host>         Mux control commands
  rssh <host> -e KEY=VAL                 Set env var""")


def main():
    args = sys.argv[1:]

    if not args or args[0] in ("-h", "--help"):
        print_usage()
        sys.exit(0)

    if args[0] == "--server":
        run_server()
        return

    if args[0] == "connect":
        if len(args) < 2:
            print("Usage: rssh connect <host> [--et]", file=sys.stderr)
            sys.exit(1)
        run_daemon(args[1], "--et" in args)
        return

    if args[0] == "disconnect":
        if len(args) < 2:
            print("Usage: rssh disconnect <host>", file=sys.stderr)
            sys.exit(1)
        run_disconnect(args[1])
        return

    if args[0] == "status":
        run_status(args[1] if len(args) > 1 else None)
        return

    if args[0] == "list":
        run_list()
        return

    if args[0] == "cp":
        use_et = "--et" in args
        cp_args = [a for a in args[1:] if a != "--et"]
        run_cp(cp_args, use_et)
        return

    # CLI mode
    force_pty = None
    use_et = False
    pipe_mode = False
    rpc_mode = False
    env_vars = []
    login_user = None
    forward_specs = []
    ctl_cmd = None
    remaining = []
    i = 0
    while i < len(args):
        if args[i] == "-t":
            force_pty = True
        elif args[i] == "-T":
            force_pty = False
        elif args[i] == "--et":
            use_et = True
        elif args[i] == "--pipe":
            pipe_mode = True
        elif args[i] == "--rpc":
            rpc_mode = True
        elif args[i] == "-l" and i + 1 < len(args):
            i += 1
            login_user = args[i]
        elif args[i] == "-e" and i + 1 < len(args):
            i += 1
            kv = args[i]
            if "=" in kv:
                env_vars.append(kv.split("=", 1))
            else:
                print(f"rssh: invalid env var: {kv} (expected KEY=VAL)", file=sys.stderr)
                sys.exit(1)
        elif args[i] == "-L" and i + 1 < len(args):
            i += 1
            forward_specs.append(("local", args[i]))
        elif args[i] == "-O" and i + 1 < len(args):
            i += 1
            ctl_cmd = args[i]
        else:
            remaining.append(args[i])
        i += 1

    if not remaining:
        print_usage()
        sys.exit(1)

    host = remaining[0]
    if login_user:
        host = f"{login_user}@{host}"
    command = remaining[1:] if len(remaining) > 1 else None

    if command:
        if len(command) == 1:
            command = command[0]

    # -O control command
    if ctl_cmd:
        run_mux_control(host, ctl_cmd, use_et)
        return

    # -L forwarding only (no session)
    if forward_specs and not command and pipe_mode is False:
        run_forward(host, forward_specs, use_et)
        return

    if rpc_mode:
        run_rpc(host, use_et=use_et)
    elif pipe_mode:
        run_pipe(host, command=command,
                 env_vars=env_vars if env_vars else None, use_et=use_et)
    else:
        # Set up forwarding before session if both specified
        if forward_specs:
            run_forward(host, forward_specs, use_et)
        run_cli(host, command=command, force_pty=force_pty,
                env_vars=env_vars if env_vars else None, use_et=use_et)


if __name__ == "__main__":
    main()
