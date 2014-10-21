$( document ).ready(function() {
    // constants
    var sockjs_url = 'http://0.0.0.0:8989/chat';

    var reconnect = function() {
        var sockjs = new SockJS(sockjs_url, undefined, {protocols_whitelist: ['websocket', 'xhr-polling'], debug: true});

        // when connecting, should indicated to user that connected, and that should login
        sockjs.onopen    = function()  {
            sockjs.send(JSON.stringify({message: "STATUS user1 pass1"}));
        };
        // handle refuse access to room/join room, leave room
        sockjs.onmessage = function(e) {
          var message = $.parseJSON(e.data);
          console.log(message);
          $("#status").text(e.data);
        };
        sockjs.onclose = function(e) {
                // broadcast leaving in common room: let server know?
                setTimeout(reconnect, 1000); // try every second
        };
        return sockjs;
    };

    var s = reconnect();

});
