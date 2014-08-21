var testium = require("testium"),
    path = require("path");

var options = {
  tests: path.resolve(__dirname, "*_test.js"),
  browser: process.env.BROWSER || "firefox",
  seleniumServer: "http://127.0.0.1:4444/wd/hub"
}

var server = require("../index").listen(3002, function() {
  options.applicationPort = server.address().port;
  console.log('about to run testium');
  testium.run(options, function(error, status) { process.exit(status) });
})
