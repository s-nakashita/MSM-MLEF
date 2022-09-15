#!/bin/sh
set -e
export SDATE=2022083000
export CYCLE=5
export BP=
SMEM=1
EMEM=10
MEM=$SMEM
while [ $MEM -le $EMEM ];do
if [ $MEM -lt 10 ];then
MEM=00$MEM
else
MEM=0$MEM
fi
echo $MEM
export MEM
#for SIGN in p m;do
#export SIGN
/home/nakashita/Development/grmsm/MSM-Tactical/usr/exp/rsm2rsm27_bv/run || exit 10
#done
MEM=`expr $MEM + 1`
done

