#!/bin/sh
#
# for control run
#
#export IDATE=2022083000
export IDATE=2022061800
export EDATE=2022061812
export GLOBAL=GFS
export SIGN=
export BV_H=12

RUNDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $RUNDIR
echo $POSTDIR

cd $RUNDIR
export MEM=000
export BV=no
#### start cycle
while [ $IDATE -le $EDATE ];do
export SDATE=$IDATE
echo $SDATE
./run || exit 2 #1>run.log 2>run.err
IDATE=`date -j -f "%Y%m%d%H" -v+${BV_H}H +"%Y%m%d%H" "${IDATE}"`
echo $IDATE
done
