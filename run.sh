#!/bin/bash
set -eux -o pipefail

stack exec --package=. tasknight-dashboard
