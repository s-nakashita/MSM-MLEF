#!/bin/sh
set -e
year=2022
month=05
iday=10
eday=14
for day in $(seq ${iday} ${eday});do
  if [ $day -lt 10 ]; then
    day=0$day
  fi
  for hour in 00 12; do
    SDATE=${year}${month}${day}${hour}
    echo $SDATE
    export SDATE
    ./run || exit 10
  done
done

