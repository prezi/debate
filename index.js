var args = process.argv.slice(2);

var http = require('http');
var node_static = require('node-static');
var path = require('path');

// 2. Static files server
var static_directory = new node_static.Server(path.resolve(__dirname, "public"));

// 3. Usual http stuff
var server = http.createServer();
server.addListener('request', function(req, res) {
    static_directory.serve(req, res);
});
server.addListener('upgrade', function(req,res){
    res.end();
});

// app is a callback function or an express application
module.exports = server;

if (!module.parent) {
  var port = args[0] ? parseInt(args[0]) : 9999
  server.listen(port, '0.0.0.0');
  console.log(' [*] Listening on 0.0.0.0:' + port );
}
