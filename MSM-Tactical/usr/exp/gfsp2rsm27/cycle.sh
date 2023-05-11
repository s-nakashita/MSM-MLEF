#!/bin/sh
set -e
SDATE=2022062006
EDATE=2022062306
INCH=12
while [ $SDATE -le $EDATE ];do
  echo $SDATE
  export SDATE
  ./run || exit 10
  SDATE=`date -j -f "%Y%m%d%H" -v+${INCH}H +"%Y%m%d%H" "${SDATE}"`
done

