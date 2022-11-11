#!/usr/local/bin/bash
set -e
SDATE=2022111100
EDATE=2022111100
NPARALLEL=11
while [ $SDATE -le $EDATE ];do
export SDATE
SMEM=0
EMEM=10
MEM=$SMEM
while [ $MEM -le $EMEM ];do
### parallel processing
N=1
while [ $N -le $NPARALLEL ];do

if [ $MEM -le $EMEM ];then
if [ $MEM -lt 10 ];then
MEM=00$MEM
else
MEM=0$MEM
fi
echo "MEMBER $MEM in $N"
#export MEM
sh /home/nakashita/Development/grmsm/MSM-Tactical/usr/exp/gefs2rsm27/run $SDATE $MEM &
fi
MEM=`expr $MEM + 1`

N=`expr $N + 1`
done
### wait for the end of parallel processing
time wait
done
#### Date change
SDATE=`date -j -f "%Y%m%d%H" -v+6H +"%Y%m%d%H" "${SDATE}"`
done
