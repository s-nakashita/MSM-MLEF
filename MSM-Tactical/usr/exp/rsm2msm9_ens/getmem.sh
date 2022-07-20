#!/bin/sh
set -e
SMEM=0
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
/home/nakashita/Development/grmsm/MSM-Tactical/usr/exp/rsm2msm9_ens/run || exit 10
MEM=`expr $MEM + 1`
done

