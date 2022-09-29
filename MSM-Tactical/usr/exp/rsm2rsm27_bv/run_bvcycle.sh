#!/bin/sh
export SDATE=2022083000
export SDATE=2022061400
export GLOBAL=GFS
export SIGN=

RUNDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $RUNDIR
echo $POSTDIR

export CYCLE=1
#### prepare control
cd $RUNDIR
export MEM=000
./run || exit 2 #1>run.log 2>run.err

MEM=1
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
#i=1
#while [ $i -le 4 ];do
#export CYCLE=$i
cd $POSTDIR
./run_addprtb.sh || exit 3 #1>out.log 2>out.err
cd $RUNDIR
./run || exit 2 #1>run.log 2>run.err
cd $POSTDIR
./run_calcte.sh || exit 4 #1>out.log 2>out.err
#i=`expr $i + 1`
#done
#done
MEM=`expr $MEM + 1`
done
