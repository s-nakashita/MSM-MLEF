#!/bin/sh
export GLOBAL=GEFS
export IDATE=2022083000
export BV_H=6
#export GLOBAL=GFS
#export IDATE=2022061400
#export BV_H=12
export SIGN=

EXPDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $EXPDIR
echo $POSTDIR

export CYCLE=9
### control
export MEM=000
export SDATE=$IDATE
./run || exit 2 #1>run.log 2>run.err

MEM=1
while [ $MEM -le 10 ]; do
if [ $GLOBAL = GFS ] && [ $CYCLE -eq 1 ];then
cd $EXPDIR
PDATE=`cat pdate.txt | awk '{if(NR == '$MEM') {print $1}}'`
echo $PDATE
export PDATE
fi
if [ $MEM -lt 10 ]; then
MEM=00$MEM
else
MEM=0$MEM
fi
export MEM
if [ $GLOBAL != GFS ];then
### downscaling
cd $EXPDIR
export BV=no
export SDATE=$IDATE
./run || exit 2 #1>run.log 2>run.err
cd $POSTDIR
./run_calcte.sh || exit 4 #1>out.log 2>out.err
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
cd $EXPDIR
. ./configure
if [ ! -d $RUNDIR ]; then
cd $POSTDIR
./run_addprtb.sh || exit 3 #1>out.log 2>out.err
cd $EXPDIR
fi
export SDATE=$IDATE
./run || exit 2 #1>run.log 2>run.err
cd $POSTDIR
./run_calcte.sh || exit 4 #1>out.log 2>out.err
#done
MEM=`expr $MEM + 1`
done
