var path = require("path");
var config = {};

/*
config.websocketServer = 'node'
config.websocketArgs = [path.resolve(__dirname, '../websocket.js')]
*/
config.websocketServer = path.resolve(__dirname, '..', 'dist/build/debate-example/debate-example');
config.websocketArgs = [];
config.websocketCommand = "debate-example";

config.chatServer = path.resolve(__dirname, '..', 'dist/build/chat-server-example/chat-server-example');
config.chatArgs = [];
config.chatCommand = "chat-server-example";

module.exports = config
