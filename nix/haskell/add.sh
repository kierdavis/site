#!/bin/sh
set -eu
nix-shell -p cabal2nix --run "cabal2nix cabal://$1 > $1.nix"
