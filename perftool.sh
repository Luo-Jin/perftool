p1=$1
echo $p1
if [ ! $p1 ] ; then
   p1="login"
fi
 sudo docker exec -it datacollect Rscript code/main.R $p1 $2 $3
