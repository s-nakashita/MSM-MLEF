#!/bin/sh
#
# convert r_sig.fNN to GrADS file
#
SDATE=2022060800
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
SRCDIR=${PWD}
EXEC=read_sig
make ${EXEC}
cd $DATADIR
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=48
inc_h=1
rm -f fort.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in=r_sig.f$fh
out=sig.f${fh}.bin
ctl=sig.f${fh}.ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s $ctl fort.61
./${EXEC} 2>&1
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
rm fort.*
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
