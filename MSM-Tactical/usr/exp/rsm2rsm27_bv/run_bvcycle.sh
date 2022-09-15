#!/bin/sh
export SDATE=2022083000
export MEM=004
export BP=wbp
export SIGN=

RUNDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $RUNDIR
echo $POSTDIR

MEM=1
while [ $MEM -le 10 ]; do
if [ $MEM -lt 10 ]; then
MEM=00$MEM
else
MEM=0$MEM
fi
export MEM
#for j in $(seq 0 1 1);do
#if [ $j -eq 0 ];then
#export BP=
#else
export BP=wbp
#fi
#### prepare member
#cd $RUNDIR
#export CYCLE=0
#./run || exit 2 #1>run.log 2>run.err
### start cycle
i=5
#while [ $i -le 4 ];do
export CYCLE=$i
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
