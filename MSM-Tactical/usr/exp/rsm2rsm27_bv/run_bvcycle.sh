#!/bin/sh
export GLOBAL=GFS
export MEMBER=10
export IDATE=2022082900
export BV_H=6
export TETYPE=dry
export SCL=
export QADJ=yes
export BP=wbp
#export GLOBAL=GFS
#export IDATE=2022061400
#export BV_H=12
export SIGN=
export PYENV='/home/nakashita/.local/bin/python3'

EXPDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $EXPDIR
echo $POSTDIR

for CYCLE in $(seq 5 5);do
export CYCLE
### control
cd $EXPDIR
export MEM=000
export SDATE=$IDATE
./run || exit 2 #1>run.log 2>run.err

if [ do$BP = dowbp ];then
cd $POSTDIR
./run_addprtbbase.sh
cd -
fi

MEM=8
while [ $MEM -le $MEMBER ]; do
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
MEM=`expr $MEM + 1`
done
done
