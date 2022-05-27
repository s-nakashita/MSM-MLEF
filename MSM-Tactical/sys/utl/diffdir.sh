#!/bin/ksh

#
#diff two directories
#

#set -x 
if [ $# -ne 3 ]; then
  echo "Wrong argument,$0 $dir1 $dir2 $filetype(F,f,h...)"
fi

fllist=`cd $1;ls *$3`

for fl in $fllist
do

  echo 'do diff between '$1/$fl' '$2/$fl
  diff $1/$fl $2/$fl 
  echo '================================================='

done

exit 0
