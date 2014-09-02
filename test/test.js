var testium = require("testium"),
    path = require("path");

var options = {
  tests: path.resolve(__dirname, "*_test.js"),
  browser: process.env.BROWSER || "firefox",
  seleniumServer: "http://127.0.0.1:4444/wd/hub",
  applicationPort: 3002,
  screenshotDirectory: path.resolve(__dirname, "screenshots")
}

var server = require("../index.js").listen(3002, function() {
  testium.run(options, function(error, status) { process.exit(status) });
});
