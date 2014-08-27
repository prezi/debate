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
