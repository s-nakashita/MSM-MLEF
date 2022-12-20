#!/bin/sh
set -e
obsdir=/zdata/grmsm/work/dpac/obs
datadir=/zdata/grmsm/work/msm2msm3_bv
outdir=/zdata/grmsm/work/dpac/singletest
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/lmlef
NODE=1
member=10
adate=2022061812
fhour=0
single=T
prep=
maxiter=10
hloc=100
saveens=1 #0:save all ensemble, 1:save only ctrl, mean and spread
yyyy=`echo ${adate} | cut -c1-4`
yy=`echo ${adate} | cut -c3-4`
mm=`echo ${adate} | cut -c5-6`
dd=`echo ${adate} | cut -c7-8`
hh=`echo ${adate} | cut -c9-10`
echo $yyyy $mm $dd $hh
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`
obsf=obsda${prep}
obsinf=obsg${prep}
obsoutf=obsa${prep}
if [ "${single}" = "T" ];then
obsf=${obsf}.single
obsinf=${obsinf}.single
obsoutf=${obsoutf}.single
fi

wdir=${outdir}/${adate}
mkdir -p $wdir
cd $wdir
cat <<EOF >lmlef.nml
&namlst_commlef
 debug=,
 jout=,
 ls=-1,
 opt=,
 ngauss=,
&end
&param_ens
 member=${member},
&end
&param_obsope
 obsin_num=,
 obsin_name=,
 obs_out=,
 obsout_basename=,
 fguess_basename=,
 slot_start=,
 slot_end=,
 slot_base=,
 slot_tint=,
&end
&param_lmlef
 obsda_in_basename='${obsinf}.@@@@',
 obsda_out_basename='${obsoutf}.@@@@.iter${maxiter}.l${hloc}',
 gues_in_basename=,
 anal_out_basename='anal${prep}.@@@@.iter${maxiter}.l${hloc}',
 sigma_obs=${hloc}.0d3,
 sigma_obsv=,
 gross_error=,
 mean=,
 tl=,
 scl_mem=,
 maxiter=${maxiter},
 oma_monit=T,
 obsanal_output=T,
&end
EOF
cat lmlef.nml

rm -f gues.*.grd ${obsinf}.*.dat
fh=`printf '%0.2d' $fhour`
ln -s ${datadir}/${adate}/r_sig.f$fh gues.0000.grd
ln -s ${obsdir}/${adate}/${obsf}.0000.dat ${obsinf}.0000.dat
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
ln -s ${datadir}/${adate}/bv${mem}/r_sig.f$fh gues.0${mem}.grd
ln -s ${obsdir}/${adate}/${obsf}.0${mem}.dat ${obsinf}.0${mem}.dat
m=`expr $m + 1`
done
ln -fs lmlef.nml STDIN
ln -fs ${bindir}/lmlef .
mpiexec -n $NODE ./lmlef | tee lmlef.log
mv NOUT-001 stdout${prep}.iter${maxiter}.l${hloc}.txt
if [ $saveens -eq 1 ];then
m=1
while [ $m -le $member ];do
mem=`printf '%0.4d' $m`
rm ${obsoutf}.${mem}.iter${maxiter}.l${hloc} anal${prep}.${mem}.iter${maxiter}.l${hloc}
done
fi

echo "END"
