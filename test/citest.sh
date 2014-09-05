#!/bin/sh -ex

npm install
cabal install
cabal test
npm test
