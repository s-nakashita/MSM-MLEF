#!/bin/sh
set -e
SDATE=2021083100
EDATE=2021093000
INCH=24
while [ $SDATE -le $EDATE ];do
  echo $SDATE
  export SDATE
  ./run || exit 10
  SDATE=`date -j -f "%Y%m%d%H" -v+${INCH}H +"%Y%m%d%H" "${SDATE}"`
done

