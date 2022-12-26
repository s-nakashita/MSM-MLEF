#!/bin/sh
set -e
datadir=/zdata/grmsm/work/msm2msm3_bv
obsdir=/zdata/grmsm/work/dpac/obs
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/obs
member=10
adate=2022061812
fhour=0
lmin=-60
rmin=60
prep=_preprh
single=T
useobs='all'
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
if [ "$useobs" = "all" ];then
  luseobs=
else
  outf=${outf}.${useobs}
  logf=${logf}.${useobs}
  case $useobs in
    'u'  ) luseobs="T,F,F,F,F,F,F,F,F" ;;
    'v'  ) luseobs="F,T,F,F,F,F,F,F,F" ;;
    't'  ) luseobs="F,F,T,F,F,F,F,F,F" ;;
    'q'  ) luseobs="F,F,F,T,F,F,F,F,F" ;;
    'rh' ) luseobs="F,F,F,F,T,F,F,F,F" ;;
    'ps' ) luseobs="F,F,F,F,F,T,F,F,F" ;;
    'td' ) luseobs="F,F,F,F,F,F,T,F,F" ;;
    'wd' ) luseobs="F,F,F,F,F,F,F,T,F" ;;
    'ws' ) luseobs="F,F,F,F,F,F,F,F,T" ;;
  esac
fi
echo $obsf $outf
echo "luseobs="$luseobs

wdir=${obsdir}/${adate}
mkdir -p $wdir
cd $wdir
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
 luseobs=${luseobs},
 slot_start=,
 slot_end=,
 slot_base=,
 slot_tint=,
&end
EOF
cat obsope.nml

rm -f gues.*
fh=`printf '%0.2d' $fhour`
ln -s ${datadir}/${adate}/r_sig.f$fh gues.0000.grd
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
ln -s ${datadir}/${adate}/bv${mem}/r_sig.f$fh gues.0${mem}.grd
m=`expr $m + 1`
done

ln -fs ${bindir}/obsope_serial .
./obsope_serial < obsope.nml | tee ${logf}.log
rm gues.*

echo "END"
