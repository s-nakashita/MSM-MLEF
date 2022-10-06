#!/bin/sh
export SDATE=2022061800
export GLOBAL=GFS
export BV_H=12
export SIGN=
export IRES=3

RUNDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $RUNDIR
echo $POSTDIR

MEM=2
while [ $MEM -le 10 ]; do
if [ $MEM -lt 10 ]; then
MEM=00$MEM
else
MEM=0$MEM
fi
export MEM
if [ $GLOBAL != GFS ];then
### downscaling
cd $RUNDIR
export BV=no
./run || exit 2 #1>run.log 2>run.err
fi
#### breeding
export BV=yes
#for j in $(seq 0 1 1);do
j=0
if [ $j -eq 0 ];then
export BP=
else
export BP=wbp
fi
### start cycle
cd $RUNDIR
./run || exit 2 #1>run.log 2>run.err
cd $POSTDIR
export IDATE=$SDATE
./run_calcte.sh || exit 4 #1>out.log 2>out.err
#done
MEM=`expr $MEM + 1`
done
