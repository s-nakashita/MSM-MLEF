#!/bin/sh
export GLOBAL=GFS
export IDATE=2022082900
export BV_H=6
export TETYPE=dry
export QADJ=yes
#export GLOBAL=GFS
#export IDATE=2022061400
#export BV_H=12
export SIGN=

EXPDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $EXPDIR
echo $POSTDIR

for CYCLE in $(seq 1 5);do
export CYCLE
### control
cd $EXPDIR
export MEM=000
export SDATE=$IDATE
./run || exit 2 #1>run.log 2>run.err

MEM=1
while [ $MEM -le 10 ]; do
if [ $GLOBAL = GFS ] && [ $CYCLE -eq 1 ];then
cd $EXPDIR
PDATE=`cat pdate2.txt | awk '{if(NR == '$MEM') {print $1}}'`
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
if [ $CYCLE -gt 1 ]; then
   PCYCLE=`expr $CYCLE - 1`
   fh=`expr $BV_H \* $PCYCLE`
   SDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${IDATE}"`
   export SDATE
fi
. ./configure
#if [ ! -d $RUNDIR ]; then
#export SDATE=$IDATE
#./run || exit 2 #1>run.log 2>run.err
#cd $POSTDIR
#./run_calcte.sh || exit 4 #1>out.log 2>out.err
#fi
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
if [ $CYCLE -gt 1 ]; then
   PCYCLE=`expr $CYCLE - 1`
   fh=`expr $BV_H \* $PCYCLE`
   SDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${IDATE}"`
   export SDATE
fi
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
done
