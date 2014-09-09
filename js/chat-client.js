$( document ).ready(function() {
    var sockjs_url = 'http://0.0.0.0:8888/echo';

    var chatbox  = $('#chatbox');
    var input  = $('#input input');
    var form = $('#input form');

    var print = function(m, p) {
        p = (p === undefined) ? '' : JSON.stringify(p);
        div.append($("<code>").text(m + ' ' + p));
        div.append($("<br>"));
        div.scrollTop(div.scrollTop()+10000);
    };

    var reconnect = function() {
        var sockjs = new SockJS(sockjs_url, undefined, {protocols_whitelist: ['websocket', 'xhr-polling'], debug: true});

        sockjs.onopen    = function()  { // broadcast hello in common room };
        sockjs.onmessage = function(e) {  print(e.data); // handle refuse access to room/join room, leave room };
        sockjs.onclose = function(e) {
                // broadcast leaving in common room
                setTimeout(reconnect, 1000); // try every second
        };
        form.unbind(); // before rebinding with current socket
        // send messages: identify, join room, leave room, say something
        form.submit(function() {
            sockjs.send(inp.val());
            inp.val('');
            return false;
        });
        return sockjs;
    };

    $('#first input').focus();

    var s = reconnect();

});
