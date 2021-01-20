#!/bin/bash

workplace=`pwd`
container_name=`hostname -s`${workplace//\//_}
n=`sudo docker image ls | grep scnjil/perftool | wc -l`


if [ $n -lt 1 ]; then
   sudo docker pull scnjil/perftool:v2
fi

if [ 'start' = $1 ]; then
   sudo docker run -itd -v $workplace:/tmp1 -w /tmp1 --rm --name=$container_name scnjil/perftool:v2 bash
fi

if [ 'stop' = $1 ]; then
   sudo docker container stop $container_name
fi

if [ 'status' = $1 ]; then
   sudo docker ps | grep $container_name
fi
