p1=$1
workpalce=`pwd`

if [ ! $p1 ] ; then
   p1="login"
fi
 sudo docker exec -it $workpalce Rscript code/main.R $p1 $2 $3
