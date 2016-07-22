FROM ubuntu:16.04
ENV version 2016-07-22
MAINTAINER Yuriy Syrovetskiy <cblp@cblp.su>
RUN apt-get update
RUN apt-get install -y build-essential libgmp-dev libicu-dev libpq-dev postgresql
ENTRYPOINT ["docker/entry_point"]
# EXPOSE 80
