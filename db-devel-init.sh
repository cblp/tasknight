#!/bin/bash
set -eux -o pipefail

if [[ `uname -s` == Darwin ]]; then
    # assume brew
    psql_cmd="psql postgres"
elif [[ ${TRAVIS:-false} == true ]]; then
    # Travis CI
    psql_cmd="psql --username=postgres"
else
    # assume ubuntu
    psql_cmd="sudo -u postgres psql"
fi

echo "
    CREATE DATABASE tasknight_test;
    CREATE USER tasknight WITH password 'tasknight';
    GRANT ALL privileges ON DATABASE tasknight_test TO tasknight;
" | $psql_cmd
