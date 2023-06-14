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
ENDHOUR=${3:-24}
MEM=${4}
if [ $IRES -eq 27 ]; then
ICLD=0 #hydrostatic
inc_h=3
if [ do$MEM = do ]; then
DATADIR=/zdata/grmsm/work/gfsp2rsm27_nomad/$SDATE
DATADIR=/zdata/grmsm/work/gfsp2rsm27_himsst/$SDATE
DATADIR=/zdata/grmsm/work/rsm2rsm27/$SDATE
DATADIR=/zdata/grmsm/work/rsm2rsm27_t02tv1/$SDATE
else
#DATADIR=/zdata/grmsm/work/gefs2rsm27_nomad/$SDATE/$MEM
if [ "$MEM" = "000" ]; then
DATADIR=/zdata/grmsm/work/rsm2rsm27_bvgfs/$SDATE
else
DATADIR=/zdata/grmsm/work/rsm2rsm27_bvgfs/$SDATE/$MEM
fi
#DATADIR=/zdata/grmsm/work/rsm2rsm27_bv/$SDATE/bvm${MEM}_a5
fi
elif [ $IRES -eq 18 ]; then
ICLD=0
inc_h=3
#DATADIR=/zdata/grmsm/work/rsm2rsm18_truth/$SDATE
DATADIR=/zdata/grmsm/work/rsm2rsm18_osse/$SDATE
#DATADIR=/zdata/grmsm/work/rsm2rsm18_osse/$SDATE/da_prep.siml30.grid.scl.iter1.l200.v4.rs90.000
elif [ $IRES -eq 9 ]; then
ICLD=1 #nonhydrostatic
inc_h=1
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
#DATADIR=/zdata/grmsm/work/rsm2msm9_truth/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm9_osse/$SDATE
if [ ! -z $MEM ]; then
DATADIR=/zdata/grmsm/work/rsm2msm9_osse/$SDATE/bvdry$MEM
#DATADIR=/zdata/grmsm/work/rsm2msm9_osse/$SDATE/da_preprh.siml30.uniform.scl.iter1.l100.v4.rs90.$MEM
fi
#if [ "$MEM" = "000" ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_bvgfs/$SDATE
#else
#DATADIR=/zdata/grmsm/work/rsm2msm9_bvgfs/$SDATE/$MEM
#fi
elif [ $IRES -eq 3 ]; then
ICLD=1 #nonhydrostatic
inc_h=1
#DATADIR=/zdata/grmsm/work/msm2msm3_jpn/$SDATE
ICLD=0
#DATADIR=/zdata/grmsm/work/rsm2msm3_truthb/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm3_osse/$SDATE/da_prep.siml30.grid.scl.iter1.l100.v4.rs90.000
#DATADIR=/zdata/grmsm/work/rsm2msm3_osseb/$SDATE/noda000
elif [ $IRES -gt 27 ]; then ##debug for base perturbation
ICLD=0 #hydrostatic
inc_h=3
if [ do$MEM = do ] || [ "$MEM" = "000" ]; then
DATADIR=/zdata/grmsm/work/gfsp2rsm27_nomad_gfsz/$SDATE
#DATADIR=/zdata/grmsm/work/gfsp2rsm27_rda/$SDATE
else
DATADIR=/zdata/grmsm/work/gfsp2rsm27_rda/$SDATE/$MEM
fi
else
echo "Invalid resolution. Specify 27, 9 or 3."
exit 2
fi
if [ ! -d $DATADIR ]; then
echo "no such directory, "$DATADIR
exit 3
fi
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
EXEC=read_sig
cd $SRCDIR
gmake ${EXEC}
cd $DATADIR
pwd
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
cat <<EOF >read_sig.nml
&namlst_cld
 icld=${ICLD},
&end
EOF
fh=0
#fh=$ENDHOUR
end_hour=$ENDHOUR
rm -f fort.*
rm ${EXEC}.log
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in=r_sig.f$fh
out=sig.f${fh}.nconv.bin
ctl=sig.f${fh}.nconv.ctl
#in=rbssigf$fh
#out=sigb.f${fh}.bin
#ctl=sigb.f${fh}.ctl
rm $out
rm $ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s $ctl fort.61
./${EXEC} < read_sig.nml 1>>${EXEC}.log 2>&1
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
rm fort.*
## binary -> netcdf
#cdo -f nc import_binary $ctl ${out%.*}.nc
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
