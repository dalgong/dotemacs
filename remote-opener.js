#! /usr/bin/env node
//
//  usage:
//  0. (once) put a xdg-open replacement program as follows
//     $ cat > ~/bin/xdg-open <<EOF; chmod +x ~/bin/xdg-open
//     #! /bin/bash
//
//     [[ -n "${DISPLAY}" ]] && exec /usr/bin/xdg-open "$@"
//
//     curl -G --data-urlencode "url=$1" http://localhost:8123/open
//     EOF
//  1. (on desktop) run this command on a terminal
//     $ ./remote-opener.js
//  2. (on laptop) port forward 8123
//     $ '-L 8123:localhost:8123'
//  3. (on laptop) open a page to http://localhost:8123/start
//     make sure allow popup always

const PORT = 8123;

let http = require('http');
let url = require('url');
let fs = require('fs');
let events = require('events');
const { spawn } = require('child_process');

const page_contents = `
<html>
  <head><title>main</title>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
  </head>
  <body>
    <h1>polling...</h1>
    <script  type="text/javascript">
      var timer
      function pollServer(){
          $.ajax({
              url: '/poll',
              success: (data) => {
                 let url = data.url;
                 if (!url.startsWith("http") && !url.startsWith("data:")) {
                   url = "http://" + url;
                 }
                 window.open(url, '_blank').focus()
              },
              complete: () => pollServer()
          });
      }
      pollServer();
    </script>
  </body>
</html>
`;

let show_main_page = (req, res) => {
  res.writeHead(200, {'Content-Type': 'text/html'});
  res.end(page_contents);
}

let eventEmitter = new events.EventEmitter();
let pendingURL = "";

http.createServer((req, res) => {
    const q = url.parse(req.url, true);
    console.log(q.pathname);
    if (q.pathname == "/start") {
        return show_main_page(req, res);
    }
    if (q.pathname == "/open") {
        console.log("GOT: " + q.query["url"]);
        pendingURL = q.query["url"];
        eventEmitter.emit("new");
        res.writeHead(200, {'Content-Type': 'text/plain'});
        return res.end("OK\n");
    }
    if (q.pathname == "/poll") {
        eventEmitter.on("new", () => {
            res.writeHead(200, {'Content-Type': 'application/json'});
            return res.end(JSON.stringify({'url': pendingURL}));
        });
    }
    if (q.pathname == "/pprof") {
        console.log('pprof -flame ' + q.query['file']);
        const pprof = spawn('pprof', ['-flame', q.query['file']]);
        pprof.stdout.on('data', (url) => {
            console.log(` result: ${url}`);
            res.writeHead(302, {'Location': encodeURI(url)});
            res.end();
        });
        pprof.stderr.on('data', (data) => { console.error(`stderr: {data}`)});
        pprof.on('close', (code) => {});
    }
}).listen(PORT, 'localhost');
