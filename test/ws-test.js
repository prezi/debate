var WebSocket = require('ws');

var numJob = process.argv[3];

// simple login
var simpleLoginTest = function(ws, numJob) {

    message(ws, null, "LOGIN test" + numJob + " pass" + numJob, null);

    message(ws, "test" + numJob, "MSG hello there", "Lobby");

    message(ws, "test" + numJob, "LOGOUT", "Lobby");
}

// login and join
var joinRoomTest = function(ws, numJob) {

    message(ws, null, "LOGIN test" + numJob + " pass" + numJob, null);

    message(ws, "test" + numJob, "JOIN room" + numJob, "Lobby");

    message(ws, "test" + numJob, "MSG hello there", "room" + numJob);

    message(ws, "test" + numJob, "LOGOUT", "Lobby");
}

var wsTest = function(testFunction, numJob) {
    // random server id
    var server = random_number_string(1000);
    // session id
    var connid = random_string(8);

    // making the sockjs url
    var ws = new WebSocket('ws://localhost:8989/chat' + '/' + server + '/' + connid + '/' + 'websocket');

    ws.on('message', function(data, flags) {
        var type = data.slice(0, 1);
        switch(type) {
        case 'o':
            console.log('open');

            testFunction(ws, numJob);

            setTimeout(function() { ws.close();}, 1000);

            break;
        case 'a':
            var payload = JSON.parse(data.slice(1) || '[]');
            console.log(payload);
            break;
        case 'h':
            //console.log('heartbeat');
            break;
        }
    });
};

var message = function(ws, user, payload, channel) {
   var msg = {user: user, message: payload, channel: channel};

   // double stringification: one from client, one from sockjs lib
   ws.send(JSON.stringify([JSON.stringify(msg)]));
};

// random string function from sockjs
var random_string_chars = 'abcdefghijklmnopqrstuvwxyz0123456789_';
var random_string = function(length, max) {
    max = max || random_string_chars.length;
    var i, ret = [];
    for(i=0; i < length; i++) {
        ret.push( random_string_chars.substr(Math.floor(Math.random() * max),1) );
    }
    return ret.join('');
};

var random_number = function(max) {
    return Math.floor(Math.random() * max);
};

var random_number_string = function(max) {
    var t = (''+(max - 1)).length;
    var p = Array(t+1).join('0');
    return (p + random_number(max)).slice(-t);
};

wsTest(simpleLoginTest, numJob);
