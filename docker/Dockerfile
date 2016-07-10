# version: 0.0.0
FROM ubuntu:16.04
MAINTAINER Yuriy Syrovetskiy <cblp@cblp.su>
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
RUN echo 'deb http://download.fpcomplete.com/ubuntu xenial main' \
    > /etc/apt/sources.list.d/fpco.list
RUN apt-get update
RUN apt-get install -y libicu-dev libpq-dev postgresql
ENTRYPOINT ["docker/entry_point"]
# EXPOSE 80
