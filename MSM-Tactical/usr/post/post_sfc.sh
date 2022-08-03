#!/bin/sh
#
# convert r_sfc.fNN to GrADS file
# caution : This scripts has to be carried out after post_sig.sh
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 2 ]; then
  echo "Usage : ./post_sfc.sh init(YYYYMMDDHH) res(9 or 3) [mem(3-digit)]"
  exit 1
fi
SDATE=${1}
IRES=${2}
MEM=${3:-000}
if [ $IRES -eq 9 ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm9_ens/$SDATE/$MEM
FIGDIR=/zdata/grmsm/fig/rsm2msm9_jpn/$SDATE
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_jpn/$SDATE
FIGDIR=/zdata/grmsm/fig/msm2msm3_jpn/$SDATE
else
echo "Invalid resolution. Specify 9 or 3."
exit 2
fi
if [ ! -d $FIGDIR ];then
  mkdir -p $FIGDIR
fi
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
EXEC=read_sfc
cd $SRCDIR
make ${EXEC}
cd $DATADIR
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=48
inc_h=3
rm -f fort.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in=r_sfc.f$fh
out=sfc.f${fh}.bin
ctl=sfc.f${fh}.ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s footer.ctl fort.61
./${EXEC} 1>>${EXEC}.log 2>&1 
echo "dset ^${out}" > header.ctl
head -n 70 sig.f${fh}.ctl | tail -n 69 > middle.ctl
cat header.ctl middle.ctl footer.ctl > $ctl
rm header.ctl middle.ctl footer.ctl
## binary -> netcdf
#cdo -f nc import_binary $ctl ${out%.*}.nc
rm fort.*
## plot SST
#if [ `expr $fh % 24` -eq 0 ]; then
#${HOME}/.local/bin/grads -lbx -c "run ${MSMDIR}/usr/grscripts/sst.gs ${fh}"
#mv sst_f${fh}.png $FIGDIR
#fi
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
