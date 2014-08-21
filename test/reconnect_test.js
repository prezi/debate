//require('./helper');

console.log('loading test');
var browser = require("testium").getBrowser(),
    assert = require("assert"),
    url = require("url");

describe('reconnect behavior', function() {
    it('connects when accessing the page', function() {
      console.log(browser.getUrl());
      browser.navigateTo("/");
      browser.assert.elementHasText("#status", "connected")
    })
});

