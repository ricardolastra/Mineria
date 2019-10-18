# Ubuntu con sqlite3
#pull imagen base
FROM ubuntu:latest
#Instalar sqlite3 dentro de la imagen
RUN apt-get update && apt-get install -y build-essential
RUN apt-get install -y sqlite3 libsqlite3-dev
RUN mkdir /db
RUN /usr/bin/sqlite3 /db/test.db
RUN apt-get install nano
RUN apt-get install ssh
RUN apt-get install wget
RUN apt-get install curl
RUN apt-get install zcat
RUN apt-get install tar
RUN apt-get install gunzip
RUN apt-get install gnu-parallel
RUN apt-get install tabix
RUN apt-get install bgzip
CMD /bin/bash