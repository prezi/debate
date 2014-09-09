var browser = require("testium").getBrowser(),
    assert = require("assert"),
    psTree = require("ps-tree"),
    url = require("url"),
    sys = require('sys'),
    cp = require('child_process'),
    exec = cp.exec;

var reconnect_test = function(config) {
    describe('reconnect behavior', function() {
        var websocketChild;
        // if child process created, it should always be killed at the end
        afterEach(function(done) {
            if (websocketChild) {
                psTree(websocketChild.pid, function(err, children) {
                    cp.spawn('kill', ['-9'].concat(children.map(function (p) {return p.PID})))
                    websocketChild.kill();
                    websocketChild.on('close', function() {
                    done();
                    });
                });
            } else {
                done();
            }
        });

        beforeEach(function(done) {
            websocketChild = cp.spawn(config.websocketServer, config.websocketArgs);
            setTimeout(done, 1000);
        });

        it('connects when accessing the page', function() {
            browser.navigateTo("/")
            browser.assert.elementHasText("#status", "reconnected")
        });

        it('shows disconnect when server is down', function(done) {
            browser.navigateTo("/")
            psTree(websocketChild.pid, function(err, children) {
                cp.spawn('kill', ['-9'].concat(children.map(function (p) {return p.PID})))
                websocketChild.kill();
                websocketChild.on('close', function(code, signal) {
                websocketChild = null;
                browser.assert.elementHasText("#status", "disconnected")
                done();
                });
            });
        });

        it('reconnects when server goes back up', function(done) {
            browser.navigateTo("/");
            psTree(websocketChild.pid, function(err, children) {
                cp.spawn('kill', ['-9'].concat(children.map(function (p) {return p.PID})))
                websocketChild.kill();
                websocketChild.on('close', function(code, signal) {
                    // re-spawn
                    websocketChild = cp.spawn(config.websocketServer, config.websocketArgs);
                    setTimeout(function() {
                        browser.assert.elementHasText("#status", "reconnected")
                        done();
                    }, 1000);
                });
            });
        });
    });
};

module.exports = reconnect_test
