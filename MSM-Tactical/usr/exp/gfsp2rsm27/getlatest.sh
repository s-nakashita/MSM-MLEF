#!/bin/sh
set -e
SDATE=`date -ju -v-4H "+%Y%m%d%H"`
echo $SDATE > tmp.log
export SDATE
/home/nakashita/Development/grmsm/MSM-Tactical/usr/exp/gfsp2rsm27/run >> tmp.log|| exit 10

