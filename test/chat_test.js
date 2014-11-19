var WebSocket = require('ws'),
    assert = require("assert"),
    psTree = require("ps-tree"),
    sys = require('sys'),
    cp = require('child_process'),
    exec = cp.exec,
    config = require('./config.js');

describe("blocking behavior", function() {
    this.timeout(10000);
    var chatProcess;

    // making sure there are no surviving chat processes
    // note: could be dangerous if run on production !!
    before(function(done) {
        exec('killall ' + config.chatCommand, function(error, stdout, stderr) {
            setTimeout(done, 500);
        });
    });

    // if child process created, it should always be killed at the end
    afterEach(function(done) {
        if (chatProcess) {
            chatProcess.kill();
            chatProcess.on('close', function() {
                done();
            });
        } else {
            done();
        }
    });

    beforeEach(function(done) {
        //making sure no extra processes running from failed test runs
        if (typeof chatProcess !== "undefined" && chatProcess !== null) {
            chatProcess.kill();
        }
        chatProcess = cp.spawn(config.chatServer, config.chatArgs);
        chatProcess.stdout.on('data', function(data) {
            console.log('stdout: ', data);
        });
        chatProcess.stderr.on('data', function(data) {
            var buff = new Buffer(data);
            console.log('stderr: ', buff.toString('utf8'));
        });
        setTimeout(done, 500);
    });

    it("continues functioning if a client disconnects", function(done) {
        // random server id
        var server = random_number_string(1000);
        // making the sockjs url for 2 websockets - session id for each
        var ws = new WebSocket('ws://localhost:8989/chat' + '/' + server + '/' + random_string(8) + '/' + 'websocket');
        var ws2 = new WebSocket('ws://localhost:8989/chat' + '/' + server + '/' + random_string(8) + '/' + 'websocket');
        var loggedOut = new RegExp("logout")

        ws.on('message', function(data, flags) {
            console.log('ws', data);
            var type = data.slice(0, 1);
            switch(type) {
            case 'o':
                console.log('ws open');
                break;
            case 'a':
                var payload = JSON.parse(data.slice(1) || '[]');
                console.log(payload);
                break;
            case 'h':
                break;
            }
        });

        ws2.on('message', function(data, flags) {
            var type = data.slice(0, 1);
            console.log('ws2', data);
            switch(type) {
            case 'o':
                console.log('ws2 open');

                // assuming that by that time both are open
                setTimeout(function() {
                    // reproduce blocking situation
                    message(ws, null, "LOGIN test1 pass1", null);

                    message(ws2, null, "LOGIN test2 pass2", null);

                    message(ws, "test1", "MSG hello there", "Lobby");

                    message(ws2, "test2", "MSG hello too", "Lobby")

                    ws.close();

                    setTimeout(function() {
                        message(ws2, "test2", "MSG hello1", "Lobby") 
                        message(ws2, "test2", "MSG hello2", "Lobby") 
                       // message(ws2, "test2", "MSG hello3", "Lobby") 
                        message(ws2, "test2", "LOGOUT", "Lobby") 
                    }, 500);
                }, 500);
            case 'a':
                var payload = JSON.parse(data.slice(1) || '[]');
                console.log(payload);
                console.log(loggedOut.test(payload));
                // we reached the end of the messages so it didn't block :)
                if (loggedOut.test(payload)) {
                    console.log('logged out');
                    ws2.close();
                    done();
                }
                break;
            case 'h':
                break;
            }
        });
    });
});

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
