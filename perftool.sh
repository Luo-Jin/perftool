p1=$1
workspace=`pwd`
container_name=`hostname -s`${workspace//\//_}

if [ ! $p1 ] ; then
   p1="login"
fi
 sudo docker exec -it $container_name Rscript code/main.R $p1 $2 $3
