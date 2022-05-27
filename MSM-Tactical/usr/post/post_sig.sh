#!/bin/sh
SDATE=2022051112
DATADIR=${PWD}/../work/rsm2msm9_3nest/$SDATE
SRCDIR=${PWD}
EXEC=read_sig
make ${EXEC}
cd $DATADIR
ln -s ${SRCDIR}/${EXEC} ${EXEC}
ft=0
end_hour=24
inc_h=1
rm -f fort.*
while [ $ft -le $end_hour ]; do
if [ $ft -lt 10 ]; then
  ft=0$ft
fi
in=r_sig.f$ft
out=sig.f${ft}.bin
ctl=sig.f${ft}.ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s $ctl fort.61
./${EXEC} 2>&1
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
rm fort.*
ft=`echo $ft + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
