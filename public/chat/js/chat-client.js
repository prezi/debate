$( document ).ready(function() {
    var sockjs_url = 'http://0.0.0.0:8989/chat';

    var chatbox  = $('#chatbox');
    var input  = $('#input input');
    var form = $('#input form');
    var channels = $('#channels .list');
    var user = null;
    var channelList = [];
    var currentChannel = null;
    var loggedIn = false;
    var mainChatRoom = "Lobby";

    var print = function(m, p) {
        p = (p === undefined) ? '' : JSON.stringify(p);
        chatbox.append($("<code>").text(m + ' ' + p));
        chatbox.append($("<br>"));
        chatbox.scrollTop(chatbox.scrollTop()+10000);
    };

    var addChannel = function(channel) {
        channels.append($("<li id=\"" + channel + "\">").text(channel));
        channelList.push(channel);
    }

    var setCurrentChannel = function(channel) {
        currentChannel = channel;
        channels.filter("li").removeClass("current");
        channels.filter("." + channel).addClass("current");
    }

    var channelMessage = function(channel, message) {
        if ($.inArray(channel, channelList) != -1 && currentChannel == channel) {
            print(message);
        }
    }

    var messageJSON = function(user, channel, message) {
        var json = {};
        if (user) { json.user = user };
        if (channel) { json.channel = channel };
        json.message = message;
        return json
    }

    var reconnect = function() {
        var sockjs = new SockJS(sockjs_url, undefined, {protocols_whitelist: ['websocket', 'xhr-polling'], debug: true});

        // when connecting, should indicated to user that connected, and that should login
        sockjs.onopen    = function()  {};
        // handle refuse access to room/join room, leave room
        sockjs.onmessage = function(e) {
          var message = $.parseJSON(e.data);
          // possible messages:
          // - user logged in: {user: user, command: login}
          // - user logged out {user: user, command: logout}
          // - normal chat message {user: user, command: chat, message: message, channel: x}
          // - joined channel successfully {user: user, command: join, channel: x}
          switch (message.command) {
              case "login":
                  if (!loggedIn) {
                      addChannel(mainChatRoom);
                      setCurrentChannel(mainChatRoom);
                      user = message.user;
                      loggedIn = true;
                  }
                  channelMessage(mainChatRoom, message.user + " just logged in");
                  break;
              case "logout":
                  channelMessage(mainChatRoom, message.user + " just logged out");
                  if (loggedIn && user == message.user) {
                    channelList = [];
                    currentChannel = null;
                    loggedIn = false;
                  }
                  break;
              default:
                  console.log("default", message);
                  break;
          }
          
        };
        sockjs.onclose = function(e) {
                // broadcast leaving in common room: let server know?
                setTimeout(reconnect, 1000); // try every second
        };
        form.unbind(); // before rebinding with current socket
        // send messages: identify, join room, leave room, say something
        form.submit(function() {
            // data to be sent includes: the user, channel and the message
            sockjs.send(JSON.stringify(messageJSON(user, currentChannel, input.val())));
            input.val('');
            return false;
        });
        return sockjs;
    };

    input.focus();

    var s = reconnect();

});
