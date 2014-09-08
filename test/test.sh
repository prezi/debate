#!/bin/sh

start-selenium &
selenium_pid=$!

# make sure that selenium is up and running
selenium_running=0
while [ $selenium_running -eq 0 ]; do
	echo "Sleeping..."
	sleep 1
	curl 'http://localhost:4444/wd/hub'
	if [ $? -eq 0 ]; then
		selenium_running=1
	fi
done

node test/test.js
status=$?

if [ $status -eq 0 ]; then
  node test/test_xhr.js
  status=$?
fi

kill $selenium_pid

exit $status
