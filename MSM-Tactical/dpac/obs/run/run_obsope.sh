#!/bin/sh
set -e
dcddir=/zdata/grmsm/work/DATA/dcd
datadir=/zdata/grmsm/work/msm2msm3_bv
member=10
adate=2022061812
fhour=0
lmin=-60
rmin=60
prep=_prep
single=T
yyyy=`echo ${adate} | cut -c1-4`
yy=`echo ${adate} | cut -c3-4`
mm=`echo ${adate} | cut -c5-6`
dd=`echo ${adate} | cut -c7-8`
hh=`echo ${adate} | cut -c9-10`
echo $yyyy $mm $dd $hh
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`
if [ $lmin -lt 0 ];then
sdate=`date -j -f "%Y%m%d%H%M" -v${lmin}M +"%y%m%d%H%M" "${adate}00"`
else
sdate=`date -j -f "%Y%m%d%H%M" -v+${lmin}M +"%y%m%d%H%M" "${adate}00"`
fi
if [ $rmin -lt 0 ];then
edate=`date -j -f "%Y%m%d%H%M" -v${rmin}M +"%H%M" "${adate}00"`
else
edate=`date -j -f "%Y%m%d%H%M" -v+${rmin}M +"%H%M" "${adate}00"`
fi
obsf=upper${prep}.${sdate}-${edate}
outf=obsda${prep}
logf=obsope${prep}
if [ "$single" = "T" ];then
  outf=${outf}.single
  logf=${logf}.single
fi
echo $obsf $outf
cat <<EOF >obsope.nml
&param_ens
 member=${member},
&end
&param_obsope
 obsin_num=1,
 obsin_name='${obsf}',
 obs_out=,
 obsout_basename='${outf}.@@@@',
 fguess_basename=,
 single_obs=${single},
 slot_start=,
 slot_end=,
 slot_base=,
 slot_tint=,
&end
EOF
cat obsope.nml

rm -f gues.* ${outf}*
fh=`printf '%0.2d' $fhour`
ln -s ${datadir}/${adate}/r_sig.f$fh gues.0000.grd
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
ln -s ${datadir}/${adate}/bv${mem}/r_sig.f$fh gues.0${mem}.grd
m=`expr $m + 1`
done

./obsope_serial < obsope.nml | tee ${logf}.log

echo "END"
