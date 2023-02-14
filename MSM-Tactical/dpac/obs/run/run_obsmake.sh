#!/bin/sh
set -ex
#datadir=/zdata/grmsm/work/msm2msm3_bv
wdir=rsm2msm9_da
#wdir=rsm2rsm27_da
datadir=/zdata/grmsm/work/$wdir
obsdir=/zdata/grmsm/work/$wdir/obs
stadir=/zdata/grmsm/work/DATA/station
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/obs
member=40
truth=25
tetype=dry
idate=2022061812
lmin=0
rmin=0
prep=_preprh
nisep=5
njsep=2
ighost=1
jghost=1
NODE=`expr $nisep \* $njsep`
RUNENV="mpiexec -n ${NODE} "
echo $RUNENV

#fhour=6
for fhour in $(seq 0 6 24);do
adate=`date -j -f "%Y%m%d%H" -v+${fhour}H +"%Y%m%d%H" "${idate}"`
yyyy=`echo ${adate} | cut -c1-4`
yy=`echo ${adate} | cut -c3-4`
mm=`echo ${adate} | cut -c5-6`
dd=`echo ${adate} | cut -c7-8`
hh=`echo ${adate} | cut -c9-10`
echo $yyyy $mm $dd $hh
set +e
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`
set -e
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
logf=obsmake${prep}
if [ "$single" = "T" ];then
  logf=${logf}.single
fi
if [ -z $prep ];then
# u,v,t,q,rh,ps,t2,td,wd,ws
  luseobs="F,F,T,F,F,T,F,T,T,T"
else
  case $prep in
    '_prep'     ) luseobs="T,T,T,T,F,T,F,F,F,F" ;;
    '_preprh'  ) luseobs="T,T,T,F,T,T,F,F,F,F" ;;
  esac
fi
echo $logf
echo "luseobs="$luseobs

wdir=${obsdir}/${adate}
mkdir -p $wdir/tmp
cd $wdir/tmp
## station
rm -f *_station.txt
ln -s $stadir/$yyyy/$mm/$adate.ADPUPA.jpn.txt upper_station.txt
cp $stadir/$yyyy/$mm/$adate.ADPSFC.jpn.txt surf_station_1.txt
cp $stadir/$yyyy/$mm/$adate.SFCSHP.jpn.txt surf_station_2.txt
cat surf_station_1.txt surf_station_2.txt > surf_station.txt
rm surf_station_1.txt surf_station_2.txt
cat <<EOF >obsmake.nml
&param_ens
 member=,
&end
&param_corsm
 njsep=${njsep},
 nisep=${nisep},
 jghost=${jghost},
 ighost=${ighost},
&end
&param_obsope
 obsin_num=2,
 obsin_name=,
 obs_out=,
 obsout_basename=,
 fguess_basename=,
 nobsmax=,
 single_obs=${single},
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
&param_obsmake
 atime=${yyyy},${imm},${idd},${ihh},0
 lmin=${lmin},
 rmin=${rmin},
 ibuf=10,
 jbuf=10, 
 kint=3,
 dist_obs_upper=300.0d3, 
 dist_obs_synop=100.0d3,
 stationin=T,
 station_fname='upper_station.txt','surf_station.txt',
&end
EOF
cat obsmake.nml

tmem=`printf '%0.3d' $truth`
rm -f gues.*.grd
fh=`printf '%0.2d' $fhour`
ln -s ${datadir}/${idate}/bv${tetype}${tmem}/r_sig.f$fh gues.0000.sig.grd
ln -s ${datadir}/${idate}/bv${tetype}${tmem}/r_sfc.f$fh gues.0000.sfc.grd
ln -s ${datadir}/${idate}/bv${tetype}${tmem}/r_flx.f$fh gues.0000.flx.grd

rm -f STDIN obsmake *.siml*.${sdate}-${edate}.dat
ln -s obsmake.nml STDIN
ln -s ${bindir}/obsmake obsmake
${RUNENV} ./obsmake #2>${logf}.err | tee ${logf}.log
mv surf.siml.${sdate}-${edate}.dat ../surf.siml.uniform.${sdate}-${edate}.dat
mv upper.siml.${sdate}-${edate}.dat ../upper${prep}.siml.uniform.${sdate}-${edate}.dat
mv NOUT-001 ../${logf}.log
rm -f NOUT-*
#mv *station.txt ../

done
echo "END"
