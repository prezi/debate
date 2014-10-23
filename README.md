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

Install js dependencies

    npm install

Install GNU Parallel (brew install parallel on mac).

    parallel -j 10 Starting {}\;node test/ws-test.js --connect {} ::: $(seq 40)

For instance: this will run 10 processes in parallel until a total number of 40 have been carried out.
