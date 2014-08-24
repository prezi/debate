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
