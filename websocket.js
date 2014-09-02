var args = process.argv.slice(2);
var sockjs = require('sockjs');
var http = require('http');

var server = http.createServer();

var sockjs_opts = {sockjs_url: "http://cdn.sockjs.org/sockjs-0.3.min.js"};

var sockjs_echo = sockjs.createServer(sockjs_opts);
// just echoes message back to sender
sockjs_echo.on('connection', function(conn) {
    conn.on('data', function(message) {
        conn.write(message);
    });
});

sockjs_echo.installHandlers(server, {prefix:'/echo'});

// app is a callback function or an express application
module.exports = server;

if (!module.parent) {
  var port = args[0] ? parseInt(args[0]) : 8888
  server.listen(port, '0.0.0.0');
  console.log(' [*] Listening on 0.0.0.0:' + port );
}
