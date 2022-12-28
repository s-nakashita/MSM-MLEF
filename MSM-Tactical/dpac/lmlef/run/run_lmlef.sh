#!/bin/sh
set -e
## experiment parameters
ires=27
single=F
NODE=5
member=10
adate=2022061812
fhour=0
lmin=-60
rmin=60
selobs=all
prep=_preprh
maxiter=5
hloc=300
saveens=0 #0:save all ensemble, 1:save only ctrl, mean and spread
## data directories
if [ $ires -eq 27 ];then
datadir=/zdata/grmsm/work/rsm2rsm27_bv
outdir=/zdata/grmsm/work/dpac/rsm27test
elif [ $ires -eq 9 ];then
datadir=/zdata/grmsm/work/rsm2msm9_bv
outdir=/zdata/grmsm/work/dpac/msm9test
elif [ $ires -eq 3 ];then
datadir=/zdata/grmsm/work/msm2msm3_bv
outdir=/zdata/grmsm/work/dpac/msm3test
fi
if [ $single = T ];then
outdir=/zdata/grmsm/work/dpac/single
fi
obsdir=/zdata/grmsm/work/dpac/obs
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/lmlef
bindir2=/home/nakashita/Development/grmsm/MSM-Tactical/usr/post
yyyy=`echo ${adate} | cut -c1-4`
yy=`echo ${adate} | cut -c3-4`
mm=`echo ${adate} | cut -c5-6`
dd=`echo ${adate} | cut -c7-8`
hh=`echo ${adate} | cut -c9-10`
echo $yyyy $mm $dd $hh
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`

## input observation
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
## preprocessed observation
obsda_in=F
obsextf=obsda${prep}
## output files
obsinf=obsg${prep}
obsoutf=obsa${prep}
anloutf=anal${prep}
logf=stdout${prep}
if [ "${single}" = "T" ];then
obsextf=${obsextf}.single
obsinf=${obsinf}.single
obsoutf=${obsoutf}.single
anloutf=${anloutf}.single
logf=${logf}.single
fi
luseobs=
if [ "${selobs}" != "all" ];then
obsextf=${obsextf}.${selobs}
obsinf=${obsinf}.${selobs}
obsoutf=${obsoutf}.${selobs}
anloutf=${anloutf}.${selobs}
logf=${logf}.${selobs}
case $selobs in
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

### move to working directory
wdir=${outdir}/${adate}
mkdir -p $wdir
cd $wdir
### namelist
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
 obsin_num=1,
 obsin_name='${obsf}',
 obs_out=F,
 obsout_basename=,
 fguess_basename=,
 single_obs=${single},
 luseobs=${luseobs},
 slot_start=,
 slot_end=,
 slot_base=,
 slot_tint=,
&end
&param_corsm
 njsep=${NODE},
 nisep=1,
 jghost=1,
 ighost=0,
&end
&param_lmlef
 obsda_in=${obsda_in},
 obsda_in_basename='${obsinf}.@@@@',
 obsda_out_basename='${obsoutf}.@@@@',
 gues_in_basename=,
 anal_out_basename='${anloutf}.@@@@',
 mean=,
 tl=,
 scl_mem=,
 debug_obs=,
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

## prepare input files
rm -f ${obsf}.dat
ln -s ${obsdir}/${adate}/${obsf}.dat .
rm -f gues.*.grd ${obsinf}.*.dat
fh=`printf '%0.2d' $fhour`
ln -s ${datadir}/${adate}/r_sig.f$fh gues.0000.grd
if [ $obsda_in = T ];then
ln -s ${obsdir}/${adate}/${obsextf}.0000.dat ${obsinf}.0000.dat
fi
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
if [ $ires -eq 27 ];then
ln -s ${datadir}/${adate}/bv${mem}_c10/r_sig.f$fh gues.0${mem}.grd
else
ln -s ${datadir}/${adate}/bv${mem}/r_sig.f$fh gues.0${mem}.grd
fi
if [ $obsda_in = T ];then
ln -s ${obsdir}/${adate}/${obsf}.0${mem}.dat ${obsinf}.0${mem}.dat
fi
m=`expr $m + 1`
done
ln -fs lmlef.nml STDIN
ln -fs ${bindir}/lmlef .
## start calculation
mpiexec -n $NODE ./lmlef 2>${logf}.err | tee ${logf}.err || exit 11
if [ -f NOUT-001 ];then
mv NOUT-001 ${logf}_n${NODE}.txt
fi
#rm NOUT-00*

## post process
if [ $saveens -eq 1 ];then
m=1
while [ $m -le $member ];do
mem=`printf '%0.4d' $m`
rm ${obsoutf}.${mem}.dat
rm ${anloutf}.${mem}.grd
m=`expr $m + 1`
done
fi

for emem in ctrl mean sprd;do
if [ $emem = ctrl ];then
in=gues.0000.grd
else
in=gues.${emem}.grd
fi
out=gues.${emem}.bin
ctl=gues.${emem}.ctl
rm -f fort.* read_sig
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

for emem in ctrl mean sprd;do
if [ $emem = ctrl ];then
in=${anloutf}.0000.grd
else
in=${anloutf}.${emem}.grd
fi
out=${anloutf}.${emem}.bin
ctl=${anloutf}.${emem}.ctl
rm -f fort.* read_sig
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
if [ $saveens -eq 0 ];then
m=1
while [ $m -le $member ];do
mem=`printf '%0.4d' $m`
in=${anloutf}.${mem}.grd
out=${anloutf}.${mem}.bin
ctl=${anloutf}.${mem}.ctl
rm -f fort.* read_sig
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
m=`expr $m + 1`
done
fi
echo "END"
