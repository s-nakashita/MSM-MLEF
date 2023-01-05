#!/bin/sh
set -ex
cycle=${1}
cycleda=${2}
head_bv=${HEAD:-bv}
head_da=${HEAD2:-da}
## experiment parameters
ires=${IRES:-27}
single=${SINGLEOBS:-F}
nisep=${NISEP:-5}
njsep=${NJSEP:-2}
ighost=${IGHOST:-1}
jghost=${JGHOST:-1}
NODE=`expr $nisep \* $njsep`
member=${MEMBER:-10}
adate=${SDATE:-2022061812}
fhour=${ENDHOUR:-0}
lmin=${LOBSMIN:--60}
rmin=${ROBSMIN:-60}
selobs=${SELOBS:-all}
prep=${PREP:-_preprh}
mean=${DA_MEAN}
tl=${DA_TL}
scl_mem=${DA_SCL_MEM}
maxiter=${MAXITER:-5}
hloc=${HLOC}
vloc=${VLOC}
minfl=${MINFL}
q_update_top=${Q_UPDATE_TOP}
if [ ! -z $hloc ];then
sigh=${hloc}.0d3
fi
if [ ! -z $vloc ];then
sigv=${vloc}.0d-1
fi
if [ ! -z $minfl ];then
infl_mul=${minfl}.0d-2
fi
#saveens=${SAVEENS:-0} #0:save all ensemble, 1:save only ctrl, mean and spread
## data directories
guesdir=${RUNDIR0:-/zdata/grmsm/work/rsm2rsm27_da}
analdir=${RUNDIR0:-$guesdir}
obsdir=/zdata/grmsm/work/dpac/obs
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/lmlef
bindir2=/home/nakashita/Development/grmsm/MSM-Tactical/usr/post
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
$USHDIR/decode_dcdf.sh $adate $lmin $rmin || exit 9
## preprocessed observation
obsda_in=F
obsextf=obsda${prep}
## output files
obsinf=obsg${prep}
obsoutf=obsa${prep}
logf=stdout${prep}
if [ "${single}" = "T" ];then
obsextf=${obsextf}.single
obsinf=${obsinf}.single
obsoutf=${obsoutf}.single
logf=${logf}.single
fi
luseobs=
if [ "${selobs}" != "all" ];then
obsextf=${obsextf}.${selobs}
obsinf=${obsinf}.${selobs}
obsoutf=${obsoutf}.${selobs}
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
echo $logf

### move to working directory
wdir=${analdir}/${adate}
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
 obs_out=T,
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
 njsep=${njsep},
 nisep=${nisep},
 jghost=${jghost},
 ighost=${ighost},
&end
&param_lmlef
 obsda_in=${obsda_in},
 obsda_in_basename='${obsinf}.@@@@',
 obsda_out_basename='${obsoutf}.@@@@',
 gues_in_basename=,
 anal_out_basename=,
 mean=${mean},
 tl=${tl},
 scl_mem=${scl_mem},
 debug_obs=,
 sigma_obs=${sigh},
 sigma_obsv=${sigv},
 gross_error=,
 cov_infl_mul=${infl_mul},
 maxiter=${maxiter},
 q_update_top=${q_update_top},
 q_adjust=T,
 oma_monit=T,
 obsanal_output=T,
&end
EOF
#cat lmlef.nml

## prepare input files
if [ $cycleda -eq 1 ];then
  head=$head_bv
else
  head=$head_da
fi
pdate=`date -j -f "%Y%m%d%H" -v-${fhour}H +"%Y%m%d%H" "$adate"`
rm -f ${obsf}.dat
ln -s ${obsdir}/${adate}/${obsf}.dat .
rm -f gues.*.grd ${obsinf}.*.dat
fh=`printf '%0.2d' $fhour`
ln -s ${guesdir}/${pdate}/r_sig.f$fh gues.0000.grd
if [ $obsda_in = T ];then
ln -s ${obsdir}/${pdate}/${obsextf}.0000.dat ${obsinf}.0000.dat
fi
if [ -f ${guesdir}/${pdate}/infl.grd ];then
  ln -s ${guesdir}/${pdate}/infl.grd .
fi
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
ln -s ${guesdir}/${pdate}/${head}${mem}/r_sig.f$fh gues.0${mem}.grd
if [ $obsda_in = T ];then
ln -s ${obsdir}/${adate}/${obsf}.0${mem}.dat ${obsinf}.0${mem}.dat
fi
m=`expr $m + 1`
done
ln -fs lmlef.nml STDIN
ln -fs ${bindir}/lmlef .
## start calculation
#if [ ! -f ${logf}_n${NODE}.txt ]; then
rm -f ${obsoutf}.*.dat anal.*.grd
mpiexec -n $NODE ./lmlef 2>${logf}.err #|| exit 11
mv NOUT-001 ${logf}_n${NODE}.txt
#rm NOUT-*
#fi

## post process
### write GrADS files
for vtype in gues anal;do
for emem in ctrl mean sprd;do
if [ $emem = ctrl ];then
in=${vtype}.0000.grd
else
in=${vtype}.${emem}.grd
fi
out=${vtype}.${emem}.bin
ctl=${vtype}.${emem}.ctl
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
echo $vtype $emem >> read_sig.log
./read_sig < read_sig.nml >> read_sig.log || exit 11
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
done
done
### prepare forecast initial files
### copy namelists
cp $guesdir/$pdate/rsmparm .
cp $guesdir/$pdate/rsmlocation .
cp $guesdir/$pdate/rfcstparm .
cp $guesdir/$pdate/station.parm .
### copy orography data
cp $guesdir/$pdate/rmtn.parm .
cp $guesdir/$pdate/rmtnoss .
cp $guesdir/$pdate/rmtnslm .
cp $guesdir/$pdate/rmtnvar .
mv anal.0000.grd r_sig.f00
cp ${guesdir}/${pdate}/r_sfc.f$fh r_sfc.f00
cp r_sig.f00 r_sigi
cp r_sig.f00 r_sigitdt
cp r_sfc.f00 r_sfci
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
mkdir -p $wdir/${head_da}${mem}
cd $wdir/${head_da}${mem}
### copy namelists
cp $guesdir/$pdate/${head}${mem}/rsmparm .
cp $guesdir/$pdate/${head}${mem}/rsmlocation .
cp $guesdir/$pdate/${head}${mem}/rfcstparm .
cp $guesdir/$pdate/${head}${mem}/station.parm .
### copy orography data
cp $guesdir/$pdate/${head}${mem}/rmtn.parm .
cp $guesdir/$pdate/${head}${mem}/rmtnoss .
cp $guesdir/$pdate/${head}${mem}/rmtnslm .
cp $guesdir/$pdate/${head}${mem}/rmtnvar .
mv ../anal.0$mem.grd r_sig.f00
cp ${guesdir}/${pdate}/${head}${mem}/r_sfc.f$fh r_sfc.f00
cp r_sig.f00 r_sigi
cp r_sig.f00 r_sigitdt
cp r_sfc.f00 r_sfci
cd -
m=`expr $m + 1`
done
for emem in mean sprd;do
mkdir -p ${guesdir}/${pdate}/${head}${emem}
mv gues.${emem}.grd ${guesdir}/${pdate}/${head}${emem}/r_sig.f$fh
mkdir -p ${wdir}/${head}${emem}
mv anal.${emem}.grd ${wdir}/${head}${emem}/r_sig.f00
done
#if [ $saveens -eq 1 ];then
#m=1
#while [ $m -le $member ];do
#mem=`printf '%0.4d' $m`
#rm ${obsoutf}.${mem}.dat
#rm ${anloutf}.${mem}.grd
#m=`expr $m + 1`
#done
#fi
#
#
#if [ $saveens -eq 0 ];then
#m=1
#while [ $m -le $member ];do
#mem=`printf '%0.4d' $m`
#in=${anloutf}.${mem}.grd
#out=${anloutf}.${mem}.bin
#ctl=${anloutf}.${mem}.ctl
#rm -f fort.* read_sig
#ln -s ${in} fort.11
#ln -s ${out} fort.51
#ln -s ${ctl} fort.61
#ln -s ${bindir2}/read_sig .
#cat << EOF > read_sig.nml
#&namlst_cld
# icld=0,
#&end
#EOF
#./read_sig < read_sig.nml
#sed -i -e 's/DATAFILE/'$out'/g' $ctl
#rm ${ctl}-e
#m=`expr $m + 1`
#done
#fi
rm gues.*.grd
if [ $obsda_in = T ];then
rm ${obsinf}.*.dat
fi
echo "END"
