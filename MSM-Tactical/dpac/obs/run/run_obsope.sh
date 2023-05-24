#!/bin/sh
set -e
datadir=/zdata/grmsm/work/msm2msm3_jpn
#datadir=/zdata/grmsm/work/msm2msm3_bv
#datadir=/zdata/grmsm/work/rsm2msm9_bv
#datadir=/zdata/grmsm/work/rsm2rsm18_da
#obsdir=/zdata/grmsm/work/dpac/obs
#obsdir=/zdata/grmsm/work/rsm2rsm18_da/obs
obsdir=/zdata/grmsm/work/rsm2msm3_da/obs
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/obs
member=0
adate=${1:-2022061812}
fhour=${2:-0}
platform=prepbufr
lmin=-30
rmin=30
prep=
single=F
useobs='all'
parallel=F
NODE=5
if [ $parallel = T ];then
RUNENV="mpiexec -n ${NODE} "
else
RUNENV=''
fi
echo $RUNENV

odate=$adate
yyyy=`echo ${odate} | cut -c1-4`
yy=`echo ${odate} | cut -c3-4`
mm=`echo ${odate} | cut -c5-6`
dd=`echo ${odate} | cut -c7-8`
hh=`echo ${odate} | cut -c9-10`
echo $yyyy $mm $dd $hh
set +e
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`
set -e
if [ $lmin -lt 0 ];then
sdate=`date -j -f "%Y%m%d%H%M" -v${lmin}M +"%y%m%d%H%M" "${odate}00"`
else
sdate=`date -j -f "%Y%m%d%H%M" -v+${lmin}M +"%y%m%d%H%M" "${odate}00"`
fi
if [ $rmin -lt 0 ];then
edate=`date -j -f "%Y%m%d%H%M" -v${rmin}M +"%H%M" "${odate}00"`
else
edate=`date -j -f "%Y%m%d%H%M" -v+${rmin}M +"%H%M" "${odate}00"`
fi
if [ $platform = prepbufr ]; then
obsin_num=3
obsf=ADPUPA${prep}.${sdate}-${edate}
obsf2=ADPSFC.${sdate}-${edate}
obsf3=SFCSHP.${sdate}-${edate}
else
obsin_num=2
obsf=upper${prep}.${sdate}-${edate}
obsf2=surf.${sdate}-${edate}
fi
outf=obsda${prep}_3_fh${fhour}
logf=obsope${prep}_3_fh${fhour}
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
if [ $parallel = T ];then
  outf=${outf}_n${NODE}
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
 obsin_num=${obsin_num},
 obsin_name='${obsf}','${obsf2}','${obsf3}',
 obs_out=T,
 obsout_basename='${outf}.@@@@',
 fguess_basename=,
 nobsmax=,
 single_obs=${single},
! lonw=124.8,
! lone=132.1,
! lats=29.1,
! latn=33.8,
 luseobs=${luseobs},
 slot_start=,
 slot_end=,
 slot_base=,
 slot_tint=,
&end
&param_lmlef
 mean=,
 debug_obs=,
&end
EOF
if [ $parallel = T ];then
mv obsope.nml tmp1.nml
cat <<EOF >tmp2.nml
&param_corsm
 njsep=${NODE},
 jghost=1,
 nisep=1,
 ighost=0,
&end
EOF
cat tmp1.nml tmp2.nml > obsope.nml
rm tmp*.nml
fi
cat obsope.nml

rm -f gues.*
idate=`date -j -f "%Y%m%d%H" -v-${fhour}H +"%Y%m%d%H" "$adate"`
fh=`printf '%0.2d' $fhour`
ln -s ${datadir}/${idate}/r_sig.f$fh gues.0000.sig.grd
ln -s ${datadir}/${idate}/r_sfc.f$fh gues.0000.sfc.grd
ln -s ${datadir}/${idate}/r_flx.f$fh gues.0000.flx.grd
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
ln -s ${datadir}/${idate}/bv${mem}/r_sig.f$fh gues.0${mem}.sig.grd
ln -s ${datadir}/${idate}/bv${mem}/r_sfc.f$fh gues.0${mem}.sfc.grd
ln -s ${datadir}/${idate}/bv${mem}/r_flx.f$fh gues.0${mem}.flx.grd
m=`expr $m + 1`
done

ln -fs obsope.nml STDIN
if [ $parallel = T ];then
ln -fs ${bindir}/obsope_parallel obsope
${RUNENV} ./obsope 2>${logf}_n${NODE}.err
for n in $(seq 1 $NODE);do
  nnn=`printf '%0.3d' $n`
  mv NOUT-$nnn ${logf}-${nnn}.log
done
else
ln -fs ${bindir}/obsope_serial obsope
${RUNENV} ./obsope 2>${logf}.err | tee ${logf}.log
fi
rm gues.* STDIN

echo "END"
