#!/bin/sh

start-selenium &
selenium_pid=$!

selenium_running=0
while [ $selenium_running -eq 0 ]; do
	echo "Waiting for Selenium to start..."
	sleep 1
	curl 'http://localhost:4444/wd/hub'
	if [ $? -eq 0 ]; then
		selenium_running=1
	fi
done

echo "\n\nTesting Websocket...\n"
node test/test.js
status=$?

if [ $status -eq 0 ]; then
  echo "\n\nTesting XHR-polling...\n"
  node test/test_xhr.js
  status=$?
fi

kill $selenium_pid

exit $status
