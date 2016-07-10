# version: 0.0.0
FROM ubuntu:16.04
MAINTAINER Yuriy Syrovetskiy <cblp@cblp.su>
RUN   apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442  \
  &&  echo 'deb http://download.fpcomplete.com/ubuntu xenial main'                        \
      > /etc/apt/sources.list.d/fpco.list                                                 \
  &&  apt-get update                                                                      \
  &&  apt-get install -y libicu-dev libpq-dev moreutils postgresql stack sudo             \
  &&  adduser --uid=1000 user
ENTRYPOINT ["docker/entry_point"]
# EXPOSE 80
