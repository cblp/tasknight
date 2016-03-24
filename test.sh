#!/bin/bash
set -eux -o pipefail

stack build --ghc-options=-Werror
stack exec --package=cabal-install -- cabal check
stack test
