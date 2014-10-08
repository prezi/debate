$( document ).ready(function() {
    // constants
    var sockjs_url = 'http://0.0.0.0:8989/chat';

    var chatbox  = $('#chatbox');
    var input  = $('#input input');
    var form = $('#input form');
    var channels = $('#channels .list');
    var mainChatRoom = "Lobby";

    // global state -- bad?  potentially move to data structure
    var user = null;
    var channelList = [];
    var currentChannel = null;
    var currentChat = $('#chatbox');
    var loggedIn = false;

    var print = function(m, p) {
        p = (p === undefined) ? '' : JSON.stringify(p);
        currentChat.append($("<code>").text(m + ' ' + p));
        currentChat.append($("<br>"));
        currentChat.scrollTop(chatbox.scrollTop()+10000);
    };

    var addChannel = function(channel) {
        channels.append($("<li id=\"" + channel + "\">").text(channel));
        channels.find("#" + channel).click(function() { setCurrentChannel(channel); });
        channelList.push(channel);
        chatbox.append($("<div id=\"" + channel + "-chat\">"))
    }

    var removeChannel = function(channel) {
        channels.find("#" + channel).remove();
        var i = channelList.indexOf(channel);
        if (i > -1) { channelList.splice(i, 1); }
        chatbox.find("#" + channel + "-chat").remove();
    }

    // TODO invisible panels for not current channels which become visible
    var setCurrentChannel = function(channel) {
        currentChannel = channel;
        // channel list on the right
        channels.find("li").removeClass("current");
        channels.find("#" + channel).addClass("current");
        // chatbox for this channel
        chatbox.children().hide();
        currentChat = chatbox.find("#" + channel + "-chat");
        currentChat.show();
    }

    var removeChannels = function() {
        console.log('removing channels');
        channels.empty();
        channelList = [];
        chatbox.empty();
    }

    // TODO use invisible pannels if in channel list so post anyway
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

    var reinitialize = function() {
        removeChannels();
        user = null;
        loggedIn = false;
        currentChannel = null;
        currentChat = chatbox;
    }

    var reconnect = function() {
        var sockjs = new SockJS(sockjs_url, undefined, {protocols_whitelist: ['websocket', 'xhr-polling'], debug: true});

        // when connecting, should indicated to user that connected, and that should login
        sockjs.onopen    = function()  { reinitialize(); };
        // handle refuse access to room/join room, leave room
        sockjs.onmessage = function(e) {
          var message = $.parseJSON(e.data);
          console.log(message);
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
              case "loginFailed":
                  print("Login failed");
                  break;
              case "loginRequired":
                  print("Please login first with LOGIN user password");
                  break;
              case "alreadyLoggedIn":
                  channelMessage(mainChatRoom, "you're already logged in");
                  break;
              case "logout":
                  channelMessage(mainChatRoom, message.user + " just quit");
                  if (loggedIn && user == message.user) { reinitialize(); }
                  break;
              case "join":
                  if (message.user == user) {
                    addChannel(message.channel)
                    setCurrentChannel(message.channel)
                  }
                  channelMessage(message.channel, message.user + " just joined the channel");
                  break;
              case "leave":
                  if (message.user == user) {
                    setCurrentChannel(mainChatRoom)
                    removeChannel(message.channel)
                  }
                  channelMessage(message.channel, message.user + " just left the channel");
                  break;
              case "msg":
                  channelMessage(message.channel, message.message);
                  break;
              default:
                  console.log("command not known");
                  break;
          }
          
        };
        sockjs.onclose = function(e) {
                // broadcast leaving in common room: let server know?
                setTimeout(reconnect, 1000); // try every second
        };
        form.unbind('submit'); // before rebinding with current socket
        // send messages: identify, join room, leave room, say something
        form.bind('submit', function() {
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
