#!/bin/bash
n=`sudo docker image ls | grep scnjil/perftool | wc -l`

if [ $n -lt 1 ]; then
   sudo docker pull scnjil/perftool:v2
fi

if [ 'start' = $1 ]; then
   sudo docker run -itd -v `pwd`:/tmp1 -w /tmp1 --rm --name=datacollect scnjil/perftool:v2 bash
fi

if [ 'stop' = $1 ]; then
   sudo docker container stop datacollect
fi

if [ 'status' = $1 ]; then
   sudo docker ps | grep datacollect 
fi
