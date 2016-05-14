#!/bin/bash
set -eux -o pipefail

docker_image="cblp/tasknight-build"

eval $(docker-machine env default)

docker build --tag="$docker_image" docker

stack_docker_options=(
    --docker
    --docker-image="$docker_image"
)

stack_docker="stack ${stack_docker_options[@]}"

$stack_docker setup

$stack_docker build

(   cd tasknight-dashboard
    $stack_docker exec --package=cabal-install -- cabal check
)
$stack_docker test
