var path = require("path");
var config = {};

config.websocketServer = 'node'
config.websocketArgs = [path.resolve(__dirname, '../websocket.js')]

module.exports = config
