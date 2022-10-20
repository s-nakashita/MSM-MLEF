#!/bin/sh
#
# for control run
#
export IDATE=2022083118
export EDATE=2022083118
export GLOBAL=GEFS
#export IDATE=2022061800
#export EDATE=2022062000
#export GLOBAL=GFS
export SIGN=
export BV_H=12

RUNDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $RUNDIR
echo $POSTDIR

cd $RUNDIR
export CYCLE=0
export MEM=000
#### start cycle
while [ $IDATE -le $EDATE ];do
export SDATE=$IDATE
echo $SDATE
./run || exit 2 #1>run.log 2>run.err
IDATE=`date -j -f "%Y%m%d%H" -v+${BV_H}H +"%Y%m%d%H" "${IDATE}"`
echo $IDATE
done
