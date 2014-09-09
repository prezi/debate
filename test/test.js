var testium = require("testium"),
    path = require("path");

var options = {
  tests: path.resolve(__dirname, "*_test_ws.js"),
  browser: process.env.BROWSER || "firefox",
  seleniumServer: "http://127.0.0.1:4444/wd/hub",
  applicationPort: 3002,
  screenshotDirectory: path.resolve(__dirname, "screenshots")
}

var options_xhr = {
  tests: path.resolve(__dirname, "*_test_xhr.js"),
  browser: process.env.BROWSER || "firefox",
  seleniumServer: "http://127.0.0.1:4444/wd/hub",
  applicationPort: 3002,
  screenshotDirectory: path.resolve(__dirname, "screenshots")
}

var server = require("../index.js").listen(3002, function() {
  console.log("Testing websockets ...");
  testium.run(options, function(error, status) { 
    if (status == 0) {
        console.log("Testing xhr ...");
        testium.run(options_xhr, function(error, status_xhr) { process.exit(status_xhr) });
    } else {
      process.exit(status)
    } 
  });
});
