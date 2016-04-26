#!/bin/bash
set -eux -o pipefail

stack build
stack exec --package=cabal-install -- cabal check
stack test
