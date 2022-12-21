#!/bin/sh
set -e
obsdir=/zdata/grmsm/work/dpac/obs
datadir=/zdata/grmsm/work/msm2msm3_bv
outdir=/zdata/grmsm/work/dpac/single
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/lmlef
bindir2=/home/nakashita/Development/grmsm/MSM-Tactical/usr/post
NODE=8
member=10
adate=2022061812
fhour=0
single=T
selobs=all
prep=_prep
maxiter=1
hloc=300
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
anloutf=anal${prep}
logf=stdout${prep}
if [ "${single}" = "T" ];then
obsf=${obsf}.single
obsinf=${obsinf}.single
obsoutf=${obsoutf}.single
anloutf=${anloutf}.single
logf=${logf}.single
fi
if [ "${selobs}" != "all" ];then
obsf=${obsf}.${selobs}
obsinf=${obsinf}.${selobs}
obsoutf=${obsoutf}.${selobs}
anloutf=${anloutf}.${selobs}
logf=${logf}.${selobs}
fi
if [ ! -z $maxiter ];then
obsoutf=${obsoutf}.iter${maxiter}
anloutf=${anloutf}.iter${maxiter}
logf=${logf}.iter${maxiter}
fi
if [ ! -z $hloc ];then
sigh=${hloc}.0d3
obsoutf=${obsoutf}.l${hloc}
anloutf=${anloutf}.l${hloc}
logf=${logf}.l${hloc}
fi
echo $obsoutf
echo $anloutf
echo $logf

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
 obsda_out_basename='${obsoutf}.@@@@',
 gues_in_basename=,
 anal_out_basename='${anloutf}.@@@@',
 mean=,
 tl=,
 scl_mem=,
 debug_obs=T,
 sigma_obs=${sigh},
 sigma_obsv=,
 gross_error=,
 cov_infl_mul=0.0d0,
 maxiter=${maxiter},
 oma_monit=T,
 obsanal_output=T,
&end
EOF
#cat lmlef.nml

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
mpiexec -n $NODE ./lmlef | tee lmlef.log || exit 11
mv NOUT-001 ${logf}_n${NODE}.txt

if [ $saveens -eq 1 ];then
m=1
while [ $m -le $member ];do
mem=`printf '%0.4d' $m`
rm ${obsoutf}.${mem}.dat
rm ${anloutf}.${mem}.grd
m=`expr $m + 1`
done
fi

# postprocess
for emem in ctrl sprd;do
if [ $emem = ctrl ];then
in=${anloutf}.0000.grd
else
in=${anloutf}.${emem}.grd
fi
out=${anloutf}.${emem}.bin
ctl=${anloutf}.${emem}.ctl
rm fort.* read_sig
ln -s ${in} fort.11
ln -s ${out} fort.51
ln -s ${ctl} fort.61
ln -s ${bindir2}/read_sig .
cat << EOF > read_sig.nml
&namlst_cld
 icld=0,
&end
EOF
./read_sig < read_sig.nml
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
done
echo "END"
