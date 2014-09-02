#!/bin/sh -ex

npm install
cabal install
npm test
