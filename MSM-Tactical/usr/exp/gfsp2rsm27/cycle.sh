#!/bin/sh
set -e
SDATE=2020082900
EDATE=2020083012
INCH=12
while [ $SDATE -le $EDATE ];do
  echo $SDATE
  export SDATE
  ./run || exit 10
  SDATE=`date -j -f "%Y%m%d%H" -v+${INCH}H +"%Y%m%d%H" "${SDATE}"`
done

