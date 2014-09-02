#!/bin/sh

start-selenium &
selenium_pid=$!

# TODO make sure that selenium is up and running
sleep 1

node test/test.js
status=$?

kill $selenium_pid

exit $status