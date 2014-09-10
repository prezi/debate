$( document ).ready(function() {
    var sockjs_url = 'http://0.0.0.0:8989/chat';

    var chatbox  = $('#chatbox');
    var input  = $('#input input');
    var form = $('#input form');

    var print = function(m, p) {
        p = (p === undefined) ? '' : JSON.stringify(p);
        chatbox.append($("<code>").text(m + ' ' + p));
        chatbox.append($("<br>"));
        chatbox.scrollTop(chatbox.scrollTop()+10000);
    };

    var reconnect = function() {
        var sockjs = new SockJS(sockjs_url, undefined, {protocols_whitelist: ['websocket', 'xhr-polling'], debug: true});

        // when connecting, should indicated to user that connected, and that should login
        sockjs.onopen    = function()  {};
        // handle refuse access to room/join room, leave room
        sockjs.onmessage = function(e) {
           console.log(e.data);
           print(e.data);
        };
        sockjs.onclose = function(e) {
                // broadcast leaving in common room
                setTimeout(reconnect, 1000); // try every second
        };
        form.unbind(); // before rebinding with current socket
        // send messages: identify, join room, leave room, say something
        form.submit(function() {
            sockjs.send(input.val());
            input.val('');
            return false;
        });
        return sockjs;
    };

    input.focus();

    var s = reconnect();

});
