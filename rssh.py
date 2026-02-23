#!/usr/bin/env python3
"""rssh — SSH Multiplexer over a Single Connection.

A single Python file that multiplexes multiple command/shell sessions and file
transfers over one SSH or ET connection. Zero external dependencies (Python 3.8+
stdlib only). Self-deploying.

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
import os
import pty
import select
import signal
import socket
import struct
import sys
import termios
import threading
import time
import tty

# ---------------------------------------------------------------------------
# Protocol layer — Binary TLV (Type-Length-Value)
#
# Frame format: [1-byte type][4-byte big-endian payload length][payload]
# Each message type defines its own binary payload layout.
# ---------------------------------------------------------------------------

# Message type IDs
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
MSG_STATUS         = 0x11
MSG_STATUS_RESP    = 0x12
MSG_DISCONNECT     = 0x13
MSG_DISCONNECT_ACK = 0x14

_TYPE_TO_ID = {
    "ready": MSG_READY, "open": MSG_OPEN, "opened": MSG_OPENED,
    "data": MSG_DATA, "resize": MSG_RESIZE, "eof": MSG_EOF,
    "close": MSG_CLOSE, "error": MSG_ERROR,
    "put": MSG_PUT, "get": MSG_GET, "file_info": MSG_FILE_INFO,
    "file_data": MSG_FILE_DATA, "file_done": MSG_FILE_DONE,
    "file_error": MSG_FILE_ERROR,
    "ping": MSG_PING, "pong": MSG_PONG,
    "status": MSG_STATUS, "status_response": MSG_STATUS_RESP,
    "disconnect": MSG_DISCONNECT, "disconnect_ack": MSG_DISCONNECT_ACK,
}
_ID_TO_TYPE = {v: k for k, v in _TYPE_TO_ID.items()}


# -- Binary helpers for variable-length fields --

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


# -- Payload encode / decode per message type --

def _encode_payload(header: dict, data: bytes) -> bytes:
    t = header["type"]

    if t in ("ready", "ping", "pong", "status", "disconnect", "disconnect_ack"):
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
        return struct.pack("!I", header["req"]) + header.get("msg", "").encode("utf-8")

    if t == "status_response":
        return (struct.pack("!IIIIB",
                            header.get("pid", 0),
                            header.get("transport_pid") or 0,
                            header.get("channels", 0),
                            header.get("file_transfers", 0),
                            1 if header.get("ready") else 0)
                + _pack_str(header.get("host", "")))

    raise ValueError(f"Unknown message type: {t}")


def _decode_payload(msg_type: str, payload: bytes):
    header = {"type": msg_type}
    data = b""

    if msg_type in ("ready", "ping", "pong", "status", "disconnect", "disconnect_ack"):
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

    elif msg_type == "status_response":
        off = 0
        pid, transport_pid, channels, file_transfers, ready_flag = \
            struct.unpack_from("!IIIIB", payload, off); off += 17
        host, off = _unpack_str(payload, off)
        header.update(pid=pid, transport_pid=transport_pid or None,
                      channels=channels, file_transfers=file_transfers,
                      ready=bool(ready_flag), host=host)

    return header, data


# -- Frame encode / decode / read / write --

def encode_frame(header: dict, data: bytes = b"") -> bytes:
    """Serialize a frame: [1-byte type][4-byte big-endian payload length][payload]."""
    type_id = _TYPE_TO_ID[header["type"]]
    payload = _encode_payload(header, data)
    return struct.pack("!BI", type_id, len(payload)) + payload


def decode_frame(type_id: int, payload: bytes):
    """Decode a frame given its type ID and payload. Returns (header_dict, data_bytes)."""
    msg_type = _ID_TO_TYPE.get(type_id)
    if msg_type is None:
        raise ValueError(f"Unknown message type ID: {type_id}")
    return _decode_payload(msg_type, payload)


async def read_frame(reader: asyncio.StreamReader):
    """Read one frame from an asyncio StreamReader. Returns (header, data)."""
    hdr = await reader.readexactly(5)
    type_id = hdr[0]
    (length,) = struct.unpack("!I", hdr[1:5])
    payload = await reader.readexactly(length) if length else b""
    return decode_frame(type_id, payload)


def read_frame_sync(fd) -> tuple:
    """Read one frame synchronously. Returns (header, data) or raises EOFError."""
    hdr = _read_exactly(fd, 5)
    type_id = hdr[0]
    (length,) = struct.unpack("!I", hdr[1:5])
    payload = _read_exactly(fd, length) if length else b""
    return decode_frame(type_id, payload)


def _read_exactly(fd, n: int) -> bytes:
    """Read exactly n bytes from fd (file-like with .read, raw fd int, or socket)."""
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
    """Write one frame synchronously to a file-like binary object or socket."""
    frame = encode_frame(header, data)
    if isinstance(fd, int):
        mv = memoryview(frame)
        written = 0
        while written < len(mv):
            written += os.write(fd, mv[written:])
    elif isinstance(fd, socket.socket):
        fd.sendall(frame)
    else:
        fd.write(frame)
        fd.flush()


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

RSSH_DIR = os.path.expanduser("~/.rssh")
CHUNK_SIZE = 65536  # 64KB for file transfers


# ---------------------------------------------------------------------------
# Server mode
# ---------------------------------------------------------------------------

class Server:
    """Runs on remote host. Reads/writes binary frames on stdin/stdout."""

    def __init__(self):
        self.channels = {}       # ch_id -> channel info dict
        self.write_queue = None  # asyncio.Queue for serialized output
        self.loop = None

    async def run(self):
        self.loop = asyncio.get_event_loop()
        self.write_queue = asyncio.Queue()

        # Writer task: serialize all output through one queue
        writer_task = asyncio.ensure_future(self._writer())

        # Send ready signal
        await self.send({"type": "ready"})

        # Read frames from stdin
        try:
            await self._reader()
        except (EOFError, ConnectionError, OSError):
            pass
        finally:
            writer_task.cancel()
            # Clean up all channels
            for ch_id in list(self.channels):
                await self._close_channel(ch_id, -1)

    async def send(self, header: dict, data: bytes = b""):
        await self.write_queue.put(encode_frame(header, data))

    async def _writer(self):
        """Single writer consuming from queue to stdout."""
        stdout_fd = sys.stdout.buffer.fileno()
        while True:
            frame = await self.write_queue.get()
            try:
                os.write(stdout_fd, frame)
            except OSError:
                return

    async def _reader(self):
        """Read frames from stdin."""
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
        elif msg_type == "error":
            pass  # Log and ignore

    # -- Channel open --

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

        # Set initial size
        winsize = struct.pack("HHHH", rows, cols, 0, 0)
        fcntl.ioctl(slave_fd, termios.TIOCSWINSZ, winsize)

        pid = os.fork()
        if pid == 0:
            # Child process
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
            # Parent
            os.close(slave_fd)
            self.channels[ch_id] = {
                "type": "pty",
                "master_fd": master_fd,
                "pid": pid,
                "alive": True,
            }
            await self.send({"type": "opened", "ch": ch_id, "pid": pid})

            # Start reader thread for PTY
            thread = threading.Thread(
                target=self._pty_reader_thread,
                args=(ch_id, master_fd),
                daemon=True,
            )
            thread.start()

            # Start waiter for child process
            asyncio.ensure_future(self._wait_child(ch_id, pid))

    def _pty_reader_thread(self, ch_id, master_fd):
        """Thread that reads from PTY master and enqueues data frames."""
        try:
            while True:
                try:
                    data = os.read(master_fd, 4096)
                except OSError:
                    break
                if not data:
                    break
                frame = encode_frame({"type": "data", "ch": ch_id}, data)
                # Thread-safe: put into asyncio queue from thread
                asyncio.run_coroutine_threadsafe(
                    self.write_queue.put(frame), self.loop
                )
        except Exception:
            pass

    async def _wait_child(self, ch_id, pid):
        """Wait for child process to exit."""
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

        self.channels[ch_id] = {
            "type": "pipe",
            "proc": proc,
            "alive": True,
        }
        await self.send({"type": "opened", "ch": ch_id, "pid": proc.pid})

        # Read stdout
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

    # -- Data/resize/eof --

    async def _handle_data(self, header: dict, data: bytes):
        ch_id = header["ch"]
        ch = self.channels.get(ch_id)
        if ch is None:
            return
        if ch["type"] == "pty":
            try:
                os.write(ch["master_fd"], data)
            except OSError:
                pass
        elif ch["type"] == "pipe":
            proc = ch["proc"]
            if proc.stdin:
                proc.stdin.write(data)
                await proc.stdin.drain()

    async def _handle_resize(self, header: dict):
        ch_id = header["ch"]
        ch = self.channels.get(ch_id)
        if ch and ch["type"] == "pty":
            rows = header.get("rows", 24)
            cols = header.get("cols", 80)
            winsize = struct.pack("HHHH", rows, cols, 0, 0)
            try:
                fcntl.ioctl(ch["master_fd"], termios.TIOCSWINSZ, winsize)
            except OSError:
                pass

    async def _handle_eof(self, header: dict):
        ch_id = header["ch"]
        ch = self.channels.get(ch_id)
        if ch is None:
            return
        if ch["type"] == "pty":
            try:
                os.write(ch["master_fd"], b"\x04")  # Ctrl-D
            except OSError:
                pass
        elif ch["type"] == "pipe":
            proc = ch["proc"]
            if proc.stdin:
                proc.stdin.close()

    # -- File transfer --

    async def _handle_put(self, header: dict):
        req = header["req"]
        path = header["path"]
        mode = header.get("mode", "0644")
        size = header.get("size", 0)
        self.channels[f"file_{req}"] = {
            "type": "file_put",
            "path": path,
            "mode": int(mode, 8) if isinstance(mode, str) else mode,
            "size": size,
            "received": 0,
            "fd": None,
        }
        try:
            os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
            fd = open(path, "wb")
            self.channels[f"file_{req}"]["fd"] = fd
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
        req = header["req"]
        key = f"file_{req}"
        ch = self.channels.get(key)
        if ch and ch["fd"]:
            ch["fd"].write(data)
            ch["received"] += len(data)

    async def _handle_file_done(self, header: dict):
        req = header["req"]
        key = f"file_{req}"
        ch = self.channels.pop(key, None)
        if ch and ch["fd"]:
            ch["fd"].close()
            try:
                os.chmod(ch["path"], ch["mode"])
            except OSError:
                pass
            await self.send({"type": "file_done", "req": req})



def run_server():
    """Entry point for server mode."""
    # Redirect stderr to log file
    os.makedirs(RSSH_DIR, exist_ok=True)
    log_path = os.path.join(RSSH_DIR, "server.log")
    log_fd = open(log_path, "a")
    os.dup2(log_fd.fileno(), 2)

    server = Server()
    asyncio.run(server.run())


# ---------------------------------------------------------------------------
# Client daemon
# ---------------------------------------------------------------------------

class Daemon:
    """Runs locally. Owns the ssh/et subprocess. Listens on Unix socket."""

    def __init__(self, host: str, use_et: bool = False, notify_fd: int = -1):
        self.host = host
        self.use_et = use_et
        self.ssh_proc = None
        self.write_queue = None
        self.next_ch_id = 1
        self.clients = {}       # ch_id -> (reader, writer) for unix socket clients
        self.file_reqs = {}     # req_id -> (reader, writer) for file transfer clients
        self.loop = None
        self.sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
        self.pid_path = os.path.join(RSSH_DIR, f"{host}.pid")
        self.ready = False
        self.notify_fd = notify_fd  # pipe fd to signal parent process
        self.start_error = ""

    async def run(self):
        self.loop = asyncio.get_event_loop()
        self.write_queue = asyncio.Queue()

        os.makedirs(RSSH_DIR, exist_ok=True)

        # Clean up stale socket
        if os.path.exists(self.sock_path):
            try:
                os.unlink(self.sock_path)
            except OSError:
                pass

        # Start SSH/ET subprocess
        if not await self._start_transport():
            self._notify_parent(f"error:{self.start_error}")
            return

        # Transport is up — notify parent and detach from terminal
        self._notify_parent("ok")
        self._daemonize()

        # Write PID file
        with open(self.pid_path, "w") as f:
            f.write(str(os.getpid()))

        # Start writer to ssh transport
        writer_task = asyncio.ensure_future(self._transport_writer())

        # Start reader from ssh transport
        reader_task = asyncio.ensure_future(self._transport_reader())

        # Start Unix socket server
        server = await asyncio.start_unix_server(
            self._handle_client, path=self.sock_path
        )
        os.chmod(self.sock_path, 0o600)

        # Wait for completion (ssh death or explicit shutdown)
        try:
            await reader_task
        except Exception:
            pass
        finally:
            server.close()
            await server.wait_closed()
            writer_task.cancel()
            self._cleanup()

    async def _start_transport(self) -> bool:
        """Start ssh/et subprocess and bootstrap server."""
        # Read our own source for bootstrapping
        script_path = os.path.abspath(__file__)
        with open(script_path, "rb") as f:
            script_data = f.read()

        encoded = base64.b64encode(script_data).decode("ascii")

        # Build bootstrap command
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
                stderr=None,  # inherit — allows SSH auth prompts via /dev/tty
            )
        except Exception as e:
            self.start_error = f"failed to start transport: {e}"
            return False

        # Send bootstrap payload (the script itself, to be exec'd on remote)
        # The remote python3 reads one line from stdin, base64 decodes it, exec's it
        # We need to send a small launcher that will exec the full script in server mode
        launcher = (
            f"import sys,base64,os\n"
            f"script = base64.b64decode({repr(encoded)})\n"
            f"sys.argv = ['rssh.py', '--server']\n"
            f"exec(compile(script, 'rssh.py', 'exec'))\n"
        )
        launcher_encoded = base64.b64encode(launcher.encode("utf-8")).decode("ascii")
        self.ssh_proc.stdin.write(launcher_encoded.encode("ascii") + b"\n")
        await self.ssh_proc.stdin.drain()

        # Wait for ready signal
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
        """Write frames from queue to ssh stdin."""
        while True:
            frame = await self.write_queue.get()
            try:
                self.ssh_proc.stdin.write(frame)
                await self.ssh_proc.stdin.drain()
            except (OSError, ConnectionError):
                return

    async def _transport_reader(self):
        """Read frames from ssh stdout and dispatch to clients."""
        try:
            while True:
                header, data = await read_frame(self.ssh_proc.stdout)
                await self._dispatch_from_server(header, data)
        except (EOFError, asyncio.IncompleteReadError, ConnectionError):
            pass

    async def _dispatch_from_server(self, header: dict, data: bytes):
        """Route server frames to the appropriate local client."""
        msg_type = header.get("type")
        ch_id = header.get("ch")
        req_id = header.get("req")

        if msg_type == "pong":
            return

        if req_id is not None and req_id in self.file_reqs:
            # File transfer response — forward to the requesting client
            writer = self.file_reqs[req_id]
            try:
                frame = encode_frame(header, data)
                writer.write(frame)
                await writer.drain()
            except (OSError, ConnectionError):
                self.file_reqs.pop(req_id, None)
            if msg_type in ("file_done", "file_error"):
                self.file_reqs.pop(req_id, None)
            return

        if ch_id is not None and ch_id in self.clients:
            writer = self.clients[ch_id]
            try:
                frame = encode_frame(header, data)
                writer.write(frame)
                await writer.drain()
            except (OSError, ConnectionError):
                self.clients.pop(ch_id, None)
            if msg_type == "close":
                self.clients.pop(ch_id, None)
            return

    async def send_to_server(self, header: dict, data: bytes = b""):
        await self.write_queue.put(encode_frame(header, data))

    async def _handle_client(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        """Handle a connection from a CLI client on the Unix socket."""
        try:
            header, data = await read_frame(reader)
        except (EOFError, asyncio.IncompleteReadError):
            writer.close()
            return

        msg_type = header.get("type")

        if msg_type == "status":
            await self._handle_status(writer)
            return

        if msg_type == "disconnect":
            await self._handle_disconnect(writer)
            return

        if msg_type in ("put", "get"):
            req_id = header.get("req")
            self.file_reqs[req_id] = writer
            await self.send_to_server(header, data)
            # Continue reading file data from client
            try:
                while True:
                    h, d = await read_frame(reader)
                    await self.send_to_server(h, d)
                    if h.get("type") in ("file_done", "file_error"):
                        break
            except (EOFError, asyncio.IncompleteReadError):
                pass
            return

        if msg_type == "open":
            ch_id = self.next_ch_id
            self.next_ch_id += 1
            header["ch"] = ch_id
            self.clients[ch_id] = writer

            # Forward open to server
            await self.send_to_server(header, data)

            # Relay client->server data
            try:
                while True:
                    h, d = await read_frame(reader)
                    h["ch"] = ch_id
                    await self.send_to_server(h, d)
            except (EOFError, asyncio.IncompleteReadError):
                pass
            finally:
                self.clients.pop(ch_id, None)
            return

        writer.close()

    async def _handle_status(self, writer: asyncio.StreamWriter):
        info = {
            "type": "status_response",
            "host": self.host,
            "pid": os.getpid(),
            "transport_pid": self.ssh_proc.pid if self.ssh_proc else None,
            "channels": len(self.clients),
            "file_transfers": len(self.file_reqs),
            "ready": self.ready,
        }
        frame = encode_frame(info)
        writer.write(frame)
        await writer.drain()
        writer.close()

    async def _handle_disconnect(self, writer: asyncio.StreamWriter):
        frame = encode_frame({"type": "disconnect_ack"})
        writer.write(frame)
        await writer.drain()
        writer.close()
        # Shutdown
        if self.ssh_proc:
            self.ssh_proc.terminate()
        self._cleanup()
        asyncio.get_event_loop().stop()

    def _notify_parent(self, msg: str):
        """Send status message to parent process via pipe, then close it."""
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
        """Detach from terminal after SSH auth is complete."""
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
    """Start the client daemon."""
    os.makedirs(RSSH_DIR, exist_ok=True)

    if foreground:
        daemon = Daemon(host, use_et)
        try:
            asyncio.run(daemon.run())
        except KeyboardInterrupt:
            daemon._cleanup()
        return

    # Fork to background, but keep the terminal for SSH authentication.
    # The child delays setsid() until after the SSH connection is established.
    r_fd, w_fd = os.pipe()
    pid = os.fork()

    if pid > 0:
        # Parent: wait for child to report connection status via pipe
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

    # Child: keep controlling terminal so SSH can authenticate via /dev/tty
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
# Client CLI — helper to connect to daemon
# ---------------------------------------------------------------------------

def ensure_daemon(host: str, use_et: bool = False) -> str:
    """Ensure daemon is running for the given host. Returns socket path."""
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if os.path.exists(sock_path):
        # Check if daemon is alive
        try:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.connect(sock_path)
            sock.close()
            return sock_path
        except (ConnectionRefusedError, OSError):
            # Stale socket
            try:
                os.unlink(sock_path)
            except OSError:
                pass

    # Start daemon
    print(f"rssh: connecting to {host}...", file=sys.stderr)
    run_daemon(host, use_et)

    # Wait for socket
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


def connect_to_daemon(sock_path: str) -> socket.socket:
    """Connect to daemon Unix socket and return the socket."""
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect(sock_path)
    return sock


# ---------------------------------------------------------------------------
# Client CLI mode — interactive shell / command execution
# ---------------------------------------------------------------------------

def run_cli(host: str, command=None, force_pty=None, env_vars=None, use_et=False):
    """Run an interactive shell or command via the daemon."""
    sock_path = ensure_daemon(host, use_et)
    sock = connect_to_daemon(sock_path)

    # Determine PTY usage
    is_tty = os.isatty(sys.stdin.fileno())
    if force_pty is True:
        use_pty = True
    elif force_pty is False:
        use_pty = False
    elif command is None:
        use_pty = is_tty
    else:
        use_pty = is_tty  # use pty if terminal is available

    # Build open frame
    open_header = {
        "type": "open",
        "ch": 0,  # daemon will reassign
        "pty": use_pty,
    }

    if command:
        open_header["cmd"] = command if isinstance(command, list) else ["bash", "-c", command]
    else:
        shell = os.environ.get("SHELL", "bash")
        open_header["cmd"] = [os.path.basename(shell)]

    if env_vars:
        open_header["env"] = dict(env_vars)

    if use_pty and is_tty:
        rows, cols = _get_terminal_size()
        open_header["rows"] = rows
        open_header["cols"] = cols

    # Send open request
    write_frame_sync(sock, open_header)

    # Wait for opened response
    try:
        header, _ = read_frame_sync(sock)
    except EOFError:
        print("rssh: connection lost", file=sys.stderr)
        sys.exit(1)

    if header.get("type") == "error":
        print(f"rssh: {header.get('msg', 'unknown error')}", file=sys.stderr)
        sys.exit(1)

    if header.get("type") != "opened":
        print(f"rssh: unexpected response: {header}", file=sys.stderr)
        sys.exit(1)

    # Terminal setup for PTY mode
    old_settings = None
    if use_pty and is_tty:
        old_settings = termios.tcgetattr(sys.stdin.fileno())
        tty.setraw(sys.stdin.fileno())
        # Handle SIGWINCH
        def on_winch(signum, frame):
            rows, cols = _get_terminal_size()
            try:
                write_frame_sync(sock, {"type": "resize", "ch": 0, "rows": rows, "cols": cols})
            except Exception:
                pass
        signal.signal(signal.SIGWINCH, on_winch)

    exit_code = 0
    try:
        exit_code = _relay_io(sock, use_pty and is_tty)
    finally:
        if old_settings is not None:
            termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, old_settings)
        sock.close()

    sys.exit(exit_code)


def _relay_io(sock: socket.socket, raw_mode: bool) -> int:
    """Relay I/O between local terminal and daemon socket."""
    exit_code = 0
    sock.setblocking(False)

    stdin_fd = sys.stdin.fileno()
    stdout_fd = sys.stdout.fileno()
    sock_fd = sock.fileno()

    # Buffer for partial frame reads from socket
    sock_buf = bytearray()

    running = True
    while running:
        read_fds = [stdin_fd, sock_fd]
        try:
            readable, _, _ = select.select(read_fds, [], [], 1.0)
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
                        write_frame_sync(sock, {"type": "eof", "ch": 0})
                        sock.setblocking(False)
                    except Exception:
                        pass
                    continue
                try:
                    sock.setblocking(True)
                    write_frame_sync(sock, {"type": "data", "ch": 0}, data)
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

                # Process complete frames
                while len(sock_buf) >= 5:
                    type_id = sock_buf[0]
                    (length,) = struct.unpack("!I", sock_buf[1:5])
                    if len(sock_buf) < 5 + length:
                        break
                    payload = bytes(sock_buf[5:5 + length])
                    del sock_buf[:5 + length]

                    header, data = decode_frame(type_id, payload)
                    msg_type = header.get("type")

                    if msg_type == "data":
                        try:
                            os.write(stdout_fd, data)
                        except OSError:
                            pass
                    elif msg_type == "close":
                        exit_code = header.get("exit_code", 0)
                        running = False
                        break
                    elif msg_type == "error":
                        sys.stderr.write(f"\rrssh: {header.get('msg', 'error')}\n")
                        running = False
                        break

    return exit_code


def _get_terminal_size():
    try:
        size = os.get_terminal_size(sys.stdin.fileno())
        return size.lines, size.columns  # rows, cols
    except OSError:
        return 24, 80


# ---------------------------------------------------------------------------
# Pipe mode — for TRAMP and scripted use
# ---------------------------------------------------------------------------

def run_pipe(host: str, command=None, env_vars=None, use_et: bool = False):
    """Pipe mode: transparent relay without terminal manipulation.

    Used by TRAMP and other programmatic callers. If a daemon is running,
    connects through it (multiplexed). Otherwise, runs SSH inline.
    No raw mode, no status messages, no SIGWINCH handling.
    """
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")

    # Try daemon first
    if os.path.exists(sock_path):
        try:
            sock = connect_to_daemon(sock_path)
            exit_code = _pipe_via_daemon(sock, command, env_vars)
            sys.exit(exit_code)
        except (ConnectionRefusedError, OSError):
            pass

    # No daemon — run SSH inline (no fork, no daemon creation)
    exit_code = asyncio.run(_pipe_inline(host, command, env_vars, use_et))
    sys.exit(exit_code)


def _pipe_via_daemon(sock: socket.socket, command=None, env_vars=None) -> int:
    """Open a channel through existing daemon and relay I/O."""
    env = {}
    if env_vars:
        env.update(dict(env_vars))
    # Propagate the local connection type to the remote: if our stdin
    # is a PTY (caller wants terminal semantics, e.g. eat), allocate a
    # PTY on the remote.  If stdin is a pipe (caller wants clean
    # output, e.g. hg), use a pipe on the remote.  The interactive
    # shell always needs a PTY for prompt detection.
    use_pty = (not command) or os.isatty(sys.stdin.fileno())
    open_header = {
        "type": "open",
        "ch": 0,  # daemon will reassign
        "pty": use_pty,
        "rows": 24,
        "cols": 80,
        "env": env,
    }
    if command:
        open_header["cmd"] = command if isinstance(command, list) else ["bash", "-c", command]
    else:
        # TERM=dumb suppresses shell integration escape sequences (OSC 3008,
        # bracketed paste, etc.) that break TRAMP's prompt detection.
        # Only set for the interactive shell — commands (eat, hg, etc.)
        # need a real TERM value.
        env["TERM"] = "dumb"
        # --norc --noprofile prevents .bashrc from enabling shell integration
        # escape sequences (OSC 3008, bracketed paste) that confuse TRAMP.
        open_header["cmd"] = ["bash", "--norc", "--noprofile"]

    write_frame_sync(sock, open_header)

    try:
        header, _ = read_frame_sync(sock)
    except EOFError:
        sock.close()
        return 1

    if header.get("type") != "opened":
        sock.close()
        return 1

    exit_code = _relay_io(sock, False)
    sock.close()
    return exit_code


async def _pipe_inline(host: str, command=None, env_vars=None, use_et: bool = False) -> int:
    """Direct SSH connection without daemon. Single session, no multiplexing."""
    loop = asyncio.get_event_loop()

    # Read our own source for bootstrapping
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
            stderr=None,  # inherit for SSH auth prompts
        )
    except Exception:
        return 1

    # Bootstrap
    launcher = (
        f"import sys,base64,os\n"
        f"script = base64.b64decode({repr(encoded)})\n"
        f"sys.argv = ['rssh.py', '--server']\n"
        f"exec(compile(script, 'rssh.py', 'exec'))\n"
    )
    launcher_encoded = base64.b64encode(launcher.encode("utf-8")).decode("ascii")
    proc.stdin.write(launcher_encoded.encode("ascii") + b"\n")
    await proc.stdin.drain()

    # Wait for ready
    try:
        header, _ = await asyncio.wait_for(read_frame(proc.stdout), timeout=30)
        if header.get("type") != "ready":
            return 1
    except (asyncio.TimeoutError, asyncio.IncompleteReadError, EOFError):
        return 1

    # Open channel
    env = {}
    if env_vars:
        env.update(dict(env_vars))
    use_pty = (not command) or os.isatty(sys.stdin.fileno())
    open_header = {
        "type": "open",
        "ch": 1,
        "pty": use_pty,
        "env": env,
        "rows": 24,
        "cols": 80,
    }
    if command:
        open_header["cmd"] = command if isinstance(command, list) else ["bash", "-c", command]
    else:
        # TERM=dumb only for the interactive TRAMP shell.
        env["TERM"] = "dumb"
        open_header["cmd"] = ["bash", "--norc", "--noprofile"]

    proc.stdin.write(encode_frame(open_header))
    await proc.stdin.drain()

    # Wait for opened
    try:
        header, _ = await read_frame(proc.stdout)
        if header.get("type") != "opened":
            return 1
    except (asyncio.IncompleteReadError, EOFError):
        return 1

    # Relay I/O: stdin → server, server → stdout
    stdin_fd = sys.stdin.fileno()
    stdout_fd = sys.stdout.fileno()
    exit_code = 0
    done = asyncio.Event()

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

    stdout_task = asyncio.ensure_future(relay_stdout())
    stdin_task = asyncio.ensure_future(relay_stdin())

    await stdout_task
    stdin_task.cancel()

    try:
        proc.terminate()
    except ProcessLookupError:
        pass

    return exit_code


# ---------------------------------------------------------------------------
# File transfer
# ---------------------------------------------------------------------------

def run_cp(args: list, use_et: bool = False):
    """Handle cp subcommand: rssh cp host:path local OR rssh cp local host:path."""
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
        # Download
        _download(src_host, src_path, dst_path or dst, use_et)
    else:
        # Upload
        _upload(dst_host, src_path or src, dst_path, use_et)


def _parse_host_path(spec: str):
    """Parse host:path notation. Returns (host, path) or (None, None)."""
    if ":" in spec and not spec.startswith("/"):
        parts = spec.split(":", 1)
        return parts[0], parts[1]
    return None, None


def _download(host: str, remote_path: str, local_path: str, use_et: bool):
    """Download a file from remote host."""
    sock_path = ensure_daemon(host, use_et)
    sock = connect_to_daemon(sock_path)

    req_id = int(time.time() * 1000) % 1000000

    write_frame_sync(sock, {"type": "get", "req": req_id, "path": remote_path})

    received = 0
    total_size = 0
    fd = None

    try:
        while True:
            header, data = read_frame_sync(sock)
            msg_type = header.get("type")

            if msg_type == "file_info":
                total_size = header.get("size", 0)
                fd = open(local_path, "wb")
                continue
            elif msg_type == "file_data":
                if fd is None:
                    fd = open(local_path, "wb")
                fd.write(data)
                received += len(data)
                if total_size > 0:
                    pct = received * 100 // total_size
                    sys.stderr.write(f"\rrssh: downloading {received}/{total_size} ({pct}%)")
                    sys.stderr.flush()
                continue
            elif msg_type == "file_done":
                if fd:
                    fd.close()
                sys.stderr.write(f"\rrssh: downloaded {received} bytes\n")
                break
            elif msg_type == "file_error":
                if fd:
                    fd.close()
                    os.unlink(local_path)
                print(f"\nrssh: {header.get('msg', 'download error')}", file=sys.stderr)
                sys.exit(1)
                break
            elif msg_type == "error":
                print(f"\nrssh: {header.get('msg', 'error')}", file=sys.stderr)
                sys.exit(1)
    finally:
        sock.close()


def _upload(host: str, local_path: str, remote_path: str, use_et: bool):
    """Upload a file to remote host."""
    if not os.path.isfile(local_path):
        print(f"rssh: {local_path}: no such file", file=sys.stderr)
        sys.exit(1)

    stat = os.stat(local_path)
    size = stat.st_size
    mode = oct(stat.st_mode & 0o7777)

    sock_path = ensure_daemon(host, use_et)
    sock = connect_to_daemon(sock_path)

    req_id = int(time.time() * 1000) % 1000000

    write_frame_sync(sock, {
        "type": "put",
        "req": req_id,
        "path": remote_path,
        "mode": mode,
        "size": size,
    })

    sent = 0
    with open(local_path, "rb") as f:
        while True:
            chunk = f.read(CHUNK_SIZE)
            if not chunk:
                break
            write_frame_sync(sock, {"type": "file_data", "req": req_id}, chunk)
            sent += len(chunk)
            pct = sent * 100 // size if size > 0 else 100
            sys.stderr.write(f"\rrssh: uploading {sent}/{size} ({pct}%)")
            sys.stderr.flush()

    write_frame_sync(sock, {"type": "file_done", "req": req_id})

    # Wait for ack
    try:
        header, _ = read_frame_sync(sock)
        if header.get("type") == "file_error":
            print(f"\nrssh: {header.get('msg', 'upload error')}", file=sys.stderr)
            sys.exit(1)
        sys.stderr.write(f"\rrssh: uploaded {sent} bytes\n")
    except EOFError:
        sys.stderr.write(f"\rrssh: uploaded {sent} bytes (no ack)\n")
    finally:
        sock.close()


# ---------------------------------------------------------------------------
# Management commands
# ---------------------------------------------------------------------------

def run_status(host: str = None):
    """Show connection status."""
    if host:
        _status_one(host)
    else:
        # Show all connections
        if not os.path.exists(RSSH_DIR):
            print("rssh: no connections")
            return
        found = False
        for f in os.listdir(RSSH_DIR):
            if f.endswith(".sock"):
                h = f[:-5]
                found = True
                _status_one(h)
        if not found:
            print("rssh: no connections")


def _status_one(host: str):
    """Show status for one host."""
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if not os.path.exists(sock_path):
        print(f"{host}: not connected")
        return

    try:
        sock = connect_to_daemon(sock_path)
        write_frame_sync(sock, {"type": "status"})
        header, _ = read_frame_sync(sock)
        sock.close()

        if header.get("type") == "status_response":
            print(f"{host}: connected")
            print(f"  daemon pid: {header.get('pid')}")
            print(f"  transport pid: {header.get('transport_pid')}")
            print(f"  active channels: {header.get('channels')}")
            print(f"  file transfers: {header.get('file_transfers')}")
        else:
            print(f"{host}: unknown status")
    except (ConnectionRefusedError, OSError, EOFError):
        print(f"{host}: not connected (stale socket)")
        try:
            os.unlink(sock_path)
        except OSError:
            pass


def run_list():
    """List all active connections."""
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
                sock = connect_to_daemon(sock_path)
                write_frame_sync(sock, {"type": "status"})
                header, _ = read_frame_sync(sock)
                sock.close()
                channels = header.get("channels", 0)
                print(f"{host}  ({channels} channel{'s' if channels != 1 else ''})")
            except (ConnectionRefusedError, OSError, EOFError):
                print(f"{host}  (stale)")
                try:
                    os.unlink(sock_path)
                except OSError:
                    pass
    if not found:
        print("rssh: no connections")


def run_disconnect(host: str):
    """Disconnect from a host."""
    sock_path = os.path.join(RSSH_DIR, f"{host}.sock")
    if not os.path.exists(sock_path):
        print(f"rssh: {host}: not connected")
        return

    try:
        sock = connect_to_daemon(sock_path)
        write_frame_sync(sock, {"type": "disconnect"})
        try:
            header, _ = read_frame_sync(sock)
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

    # Also clean up PID file
    pid_path = os.path.join(RSSH_DIR, f"{host}.pid")
    try:
        os.unlink(pid_path)
    except OSError:
        pass


# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

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

  rssh cp <host>:<path> <local>    Download
  rssh cp <local> <host>:<path>    Upload

  rssh <host> -e KEY=VAL           Set env var""")


def main():
    args = sys.argv[1:]

    if not args or args[0] in ("-h", "--help"):
        print_usage()
        sys.exit(0)

    # Server mode (used internally)
    if args[0] == "--server":
        run_server()
        return

    # Management commands
    if args[0] == "connect":
        if len(args) < 2:
            print("Usage: rssh connect <host> [--et]", file=sys.stderr)
            sys.exit(1)
        host = args[1]
        use_et = "--et" in args
        run_daemon(host, use_et)
        return

    if args[0] == "disconnect":
        if len(args) < 2:
            print("Usage: rssh disconnect <host>", file=sys.stderr)
            sys.exit(1)
        run_disconnect(args[1])
        return

    if args[0] == "status":
        host = args[1] if len(args) > 1 else None
        run_status(host)
        return

    if args[0] == "list":
        run_list()
        return

    if args[0] == "cp":
        use_et = "--et" in args
        cp_args = [a for a in args[1:] if a != "--et"]
        run_cp(cp_args, use_et)
        return

    # CLI mode: rssh [-t|-T] [-l user] [-e KEY=VAL ...] [--et] [--pipe] <host> [command...]
    force_pty = None
    use_et = False
    pipe_mode = False
    env_vars = []
    login_user = None
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

    # If command is a list, join for bash -c execution
    if command:
        if len(command) == 1:
            command = command[0]
        else:
            # Pass as a list to be executed directly
            command = command

    if pipe_mode:
        run_pipe(
            host,
            command=command,
            env_vars=env_vars if env_vars else None,
            use_et=use_et,
        )
    else:
        run_cli(
            host,
            command=command,
            force_pty=force_pty,
            env_vars=env_vars if env_vars else None,
            use_et=use_et,
        )


if __name__ == "__main__":
    main()
