p1=$1
workplace=`pwd`
container_name=`hostname -s`${workplace//\//_}
echo $container_name
if [ ! $p1 ] ; then
   p1="login"
fi
 sudo docker exec -it $container_name Rscript code/main.R $p1 $2 $3
