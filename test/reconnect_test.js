var browser = require("testium").getBrowser(),
    assert = require("assert"),
    url = require("url"),
    sys = require('sys'),
    exec = require('child_process').exec;


describe('reconnect behavior', function() {
    it('connects when accessing the page', function() {
        exec('node index.js', function(error, stdout, stderr) {
            browser.navigateTo("/")
            browser.assert.elementHasText("#status", "connected")
        });
    })

    it('shows disconnect when server goes down', function() {
        exec('node index.js', function(error, stdout, stderr) {
            browser.navigateTo("/");
            browser.assert.elementHasText("#status", "connected")
            /// cowboy time with killall
            exec('killall node', function(error, stdout, stderr) {
              browser.assert.elementHasText("#status", "disconnected")
            });
        });
    })
    it('reconnects when server goes back up', function() {
        exec('node index.js', function(error, stdout, stderr) {
            browser.navigateTo("/");
            browser.assert.elementHasText("#status", "connected")
            /// cowboy time with killall
            exec('killall node', function(error, stdout, stderr) {
              browser.assert.elementHasText("#status", "disconnected")
              exec('node index.js', function(error, stdout, stderr) {
                 browser.assert.elementHasText("#status", "connected")
              });
            });
        });
    })
});

