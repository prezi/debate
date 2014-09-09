var path = require("path");
var config = {};

/*
config.websocketServer = 'node'
config.websocketArgs = [path.resolve(__dirname, '../websocket.js')]
*/
config.websocketServer = path.resolve(__dirname, '..', 'dist/build/debate-example/debate-example');
config.websocketArgs = ['-t', 'xhr'];
config.websocketCommand = "debate-example";

module.exports = config
