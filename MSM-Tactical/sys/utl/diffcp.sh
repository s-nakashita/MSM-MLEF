#!/bin/ksh

#copy indir file into compile dir
#

set -x 
if [ $# -ne 3 ]; then
  echo "Wrong argument,$0 $dir1 $dir2 $filetype(F,f,h...)"
fi

fllist=`cd $1;ls *.$3`

for fl in $fllist
do

  if [ -s $2/$fl ]; then
     dffln=`diff $2/$fl $1/$fl |wc -l`
     if [ $dffln -gt 0 ]; then
       cp $1/$fl $2
      fi
  else
      cp $1/$fl $2
  fi

done

exit 0
