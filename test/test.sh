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

node test/test.js
status=$?

kill $selenium_pid

exit $status
