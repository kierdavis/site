#!/bin/sh

cabal build

s=dist/build/site/site
$s rebuild
$s server
