#!/bin/sh
#############################################################
#
# script:  average.sh
# purpose: grib file average calculation
#
# $1 is the grib file specification (example: pgb, flx, dia)
# $2 is starting forecast hour (at least 2-digit)
# $3 is ending forecast hour (at least 2-digit)
# $4 is forecast hour interval
# $5 is output file name
#
#  This script assumes that the input grib files are named as:
#    $1.ft?? where '??' is forecast hour of at least 2 digit
#
if [ $# -ne 5 ] ; then
	echo "$0: argument error"
fi
set -x
file=$1
fhs=$2
fhe=$3
intvl=$4
outgrib=$5
#
prog=grmean
#
echo "$GRIBKPDS5"  >$prog.$file.$fhs-$fhe.parm
echo "$GRIBKPDS6" >>$prog.$file.$fhs-$fhe.parm
echo "0"          >>$prog.$file.$fhs-$fhe.parm
echo "$outgrib"   >>$prog.$file.$fhs-$fhe.parm
rm -f temp.parm.$$ 2>/dev/null
fh=$fhs
count=0
while [ $fh -le $fhe ] ; do
	fhr=`expr $fh + 0`
	if [ $fh -lt 10 ] ; then
		fhr=0$fh
	fi
	echo $file.f$fhr >>temp.parm.$$
	fh=`expr $fh + $intvl`
	count=`expr $count + 1`
done
#
echo "$count"    >>$prog.$file.$fhs-$fhe.parm
cat temp.parm.$$ >>$prog.$file.$fhs-$fhe.parm
echo "-2"        >>$prog.$file.$fhs-$fhe.parm
echo "$fhe"      >>$prog.$file.$fhs-$fhe.parm
rm -f temp.parm.$$
#
ulimit
rm $prog.x
ln -fs $UTLDIR/$prog.x $prog.x
./$prog.x <$prog.$file.$fhs-$fhe.parm \
	>$prog.$file.$fhs-$fhe.out2 2>$prog.$file.$fhs-$fhe.err
err=$?
if [ $err -eq 0 ]; then
  rm -f $prog.$file.$fhs-$fhe.parm
  rm -f $prog.$file.$fhs-$fhe.out2
  rm -f $prog.$file.$fhs-$fhe.err                                    
else
  echo "wrong in $prog"
fi
#
