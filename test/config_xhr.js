var path = require("path");
var config = {};

/*
config.websocketServer = 'node'
config.websocketArgs = [path.resolve(__dirname, '../websocket.js')]
*/
config.websocketServer = path.resolve(__dirname, '..', 'dist/build/debate-example/debate-example');
config.websocketArgs = ['-t', 'xhr'];
console.log(config);

module.exports = config
