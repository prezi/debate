# Debate

Chat server in Haskell using sockjs.

## Prerequisites

node.js stable version and npm
on mac os x:

    brew install node

for tests: selenium: download [selenium server](http://docs.seleniumhq.org/download/)

## Run integration tests

check out project
run

    npm install

(xcode may be required for native dependencies :( )

start selenium server

    java -jar <downloaded selenium server>

run tests

    node test/test.js

## Note Haskell tests

at the time of writing stable happy doesn't play well with GHC 7.8.  Install the latest version of happy on your system for hlint to install.

## Run Chat server example

Run the websocket app:

    cabal run chat-server-example

access the page in a browser

    http://0.0.0.0:8989/

To see the status page

    http://0.0.0.0:8989/status.html

## Load tests

Install tsung

    brew install tsung

A not too unrealistic scenario was added in load/ws.xml - to run it

    tsung -f load/ws.xml

For full performance, do the following:

Increase the file descriptor limit for both the process and tsung:

sudo launchctl limit maxfiles 1000000 1000000
ulimit -n 100000

Use all cores (N2 in following examples for 2 cores)

cabal run chat-server-example -- +RTS -N2
