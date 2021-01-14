#!/bin/bash
sudo docker pull scnjil/perftool:v2
sudo docker run -itd -v `pwd`:/tmp1 -w /tmp1 --rm --name=datacollect scnjil/perftool:v2 bash
