#!/bin/sh
#
# convert r_flx.fNN to GrADS file
#
#SDATE=2022071800
#IRES=9
if [ $# != 2 ]; then
  echo "Usage : ./post_flx.sh init(YYYYMMDDHH) res(9 or 3)"
  exit 1
fi
SDATE=${1}
IRES=${2}
if [ $IRES -eq 9 ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm9_ens/$SDATE/000
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_jpn/$SDATE
else
echo "Invalid resolution. Specify 9 or 3."
exit 2
fi
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
EXEC=read_flx
cd $SRCDIR
make ${EXEC}
cd $DATADIR
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=0
inc_h=3
rm -f fort.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in1=r_sig.f$fh
in2=r_flx.f$fh
out=flx.f${fh}.bin
ctl=flx.f${fh}.ctl
ln -s $in1 fort.11
ln -s $in2 fort.12
ln -s $out fort.51
ln -s footer.ctl fort.61
./${EXEC} #1>>${EXEC}.log 2>&1
echo "dset ^${out}" > header.ctl
head -n 70 sig.f${fh}.ctl | tail -n 69 > middle.ctl
cat header.ctl middle.ctl footer.ctl > $ctl
rm header.ctl middle.ctl footer.ctl
rm fort.*
## binary -> netcdf
#cdo -f nc import_binary $ctl ${out%.*}.nc
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
