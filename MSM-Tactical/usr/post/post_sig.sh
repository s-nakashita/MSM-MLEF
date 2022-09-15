#!/bin/sh
#
# convert r_sig.fNN to GrADS file
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 3 ]; then
  echo "Usage : ./post_sig.sh init(YYYYMMDDHH) res(9 or 3) end_hour [mem(3-digit)]"
  exit 1
fi
SDATE=${1}
IRES=${2}
ENDHOUR=${3}
MEM=${4:-000}
if [ $IRES -eq 27 ]; then
#DATADIR=/zdata/grmsm/work/gefs2rsm27_nomad/$SDATE/$MEM
if [ "$MEM" = "000" ]; then
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv/$SDATE
else
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv/$SDATE/$MEM
fi
#DATADIR=/zdata/grmsm/work/rsm2rsm27_bv/$SDATE/bvm${MEM}_a5
elif [ $IRES -eq 9 ]; then
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE/bvc1
#DATADIR=/zdata/grmsm/work/rsm2msm9_ens/$SDATE/$MEM
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_jpn/$SDATE
else
echo "Invalid resolution. Specify 9 or 3."
exit 2
fi
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
EXEC=read_sig
cd $SRCDIR
gmake ${EXEC}
cd $DATADIR
pwd
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
inc_h=3
rm -f fort.*
rm ${EXEC}.log
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in=r_sig.f$fh
out=sig.f${fh}.bin
ctl=sig.f${fh}.ctl
rm $out
rm $ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s $ctl fort.61
./${EXEC} 1>>${EXEC}.log 2>&1
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
rm fort.*
## binary -> netcdf
#cdo -f nc import_binary $ctl ${out%.*}.nc
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
