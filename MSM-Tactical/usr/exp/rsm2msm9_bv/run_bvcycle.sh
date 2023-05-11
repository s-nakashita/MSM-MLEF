#!/bin/sh
export IDATE=2022082900
export GLOBAL=GFS
export BV_H=6
export TETYPE=dry
export SCL=
export QADJ=yes
export BP=wbp
export SCLBASE=
#export IDATE=2022061400
#export GLOBAL=GFS
#export BV_H=12
export SIGN=
export IRES=9

RUNDIR=`pwd`
POSTDIR=`cd ../../post && pwd`
echo $RUNDIR
echo $POSTDIR

export CYCLE=5
if [ $CYCLE -gt 1 ]; then
   PCYCLE=`expr $CYCLE - 1`
   fh=`expr $BV_H \* $PCYCLE`
   SDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${IDATE}"`
   export SDATE
fi

### control
cd $RUNDIR
export MEM=000
export BV=no
./run || exit 2 #1>run.log 2>run.err

MEM=1
while [ $MEM -le 10 ]; do
if [ $MEM -lt 10 ]; then
MEM=00$MEM
else
MEM=0$MEM
fi
export MEM
#if [ $GLOBAL != GFS ];then
#### downscaling
#cd $RUNDIR
#export BV=no
#./run || exit 2 #1>run.log 2>run.err
#cd $POSTDIR
#./run_calcte.sh || exit 4 #1>out.log 2>out.err
#fi
#### breeding
export BV=yes
### start cycle
cd $RUNDIR
./run || exit 2 #1>run.log 2>run.err
cd $POSTDIR
./run_calcte.sh || exit 4 #1>out.log 2>out.err
./run_spectra.sh || exit 5
MEM=`expr $MEM + 1`
done
