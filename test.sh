#!/bin/bash
set -eux -o pipefail

stack build
(   cd tasknight-dashboard
    stack exec --package=cabal-install -- cabal check
)
stack test
