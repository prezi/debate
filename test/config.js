var path = require("path");
var config = {};

/*
config.websocketServer = 'node'
config.websocketArgs = [path.resolve(__dirname, '../websocket.js')]
*/
config.websocketServer = path.resolve(__dirname, '..', 'dist/build/debate/debate');
config.websocketArgs = [];
console.log(config);

module.exports = config
