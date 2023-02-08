#!/bin/sh
set -ex
cycle=${1}
cycleda=${2}
head_bv=${HEAD:-bv}
head_da=${HEAD2:-da}
## experiment parameters
# ensemble size
member=${MEMBER:-10}
# analysis date
adate=${SDATE:-2022061812}
# first guess lead time
fhour=${INCCYCLE:-0}
# model resolution
ires=${IRES:-27}
# observation settings
osse=${OSSE:-F}
tmem=${TMEM:-0}
single=${SINGLEOBS:-F}
lats=${DA_SINGLE_LATS}
latn=${DA_SINGLE_LATN}
lonw=${DA_SINGLE_LONW}
lone=${DA_SINGLE_LONE}
fixed_level=${DA_FIXED_LEVEL}
lmin=${LOBSMIN:--60}
rmin=${ROBSMIN:-60}
selobs=${SELOBS:-all}
prep=${PREP:-_preprh}
# parallelization
nisep=${NISEP:-5}
njsep=${NJSEP:-2}
ighost=${IGHOST:-1}
jghost=${JGHOST:-1}
NODE=`expr $nisep \* $njsep`
# DA parameters
mean=${DA_MEAN}
tl=${DA_TL}
scl_mem=${DA_SCL_MEM}
maxiter=${MAXITER:-5}
hloc=${HLOC}
vloc=${VLOC}
minfl=${MINFL}
rtpp=${RTPP}
rtps=${RTPS}
relax_spread_out=${RELAX_SPREAD_OUT}
if [ ! -z $hloc ];then
sigh=${hloc}.0d3
fi
if [ ! -z $vloc ];then
sigv=${vloc}.0d-1
fi
if [ ! -z $minfl ];then
infl_mul=${minfl}.0d-2
fi
if [ ! -z $rtpp ];then
infl_rtpp=${rtpp}.0d-2
fi
if [ ! -z $rtps ];then
infl_rtps=${rtps}.0d-2
fi
q_update_top=${Q_UPDATE_TOP}
save_info=${DA_SAVE_INFO}
## end of inputs
#debug_obs=T #debug
#print_img=$NODE #debug
if [ $single = T ];then
  debug_obs=T
  print_img=5
fi
#saveens=${SAVEENS:-0} #0:save all ensemble, 1:save only ctrl, mean and spread
## data directories
guesdir=${RUNDIR0:-/zdata/grmsm/work/rsm2rsm27_da}
analdir=${RUNDIR0:-$guesdir}
obsdir=${RUNDIR0}/obs
export obsdir
#bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/lmlef
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/builddev/lmlef
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
if [ $cycleda -eq 1 ];then
  head=$head_bv
else
  head=$head_da
fi
pdate=`date -j -f "%Y%m%d%H" -v-${fhour}H +"%Y%m%d%H" "$adate"`
fh=`printf '%0.2d' $fhour`

## preprocessed observation
obsda_in=F
obsextf=obsda${prep}
## output files
obsinf=obsg${prep}
obsoutf=obsa${prep}
logf=stdout.${head_da}
if [ "${single}" = "T" ];then
obsextf=${obsextf}.single
obsinf=${obsinf}.single
obsoutf=${obsoutf}.single
#logf=${logf}serope.
fi
luseobs=
if [ "${selobs}" != "all" ];then
obsextf=${obsextf}.${selobs}
obsinf=${obsinf}.${selobs}
obsoutf=${obsoutf}.${selobs}
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
#if [ ! -f ${logf}_n${NODE}.txt ]; then
### input observation
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
if [ $osse = T ];then
obsf=upper${prep}.siml.${sdate}-${edate}
obsf2=surf.siml.${sdate}-${edate}
else
obsf=upper${prep}.${sdate}-${edate}
obsf2=surf.${sdate}-${edate}
$USHDIR/decode_dcdf.sh $adate $lmin $rmin || exit 9
fi
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
 obsin_num=2,
 obsin_name='${obsf}','${obsf2}',
 obs_out=F,
 obsout_basename=,
 fguess_basename=,
 single_obs=${single},
 lats=${lats},
 latn=${latn},
 lonw=${lonw},
 lone=${lone},
 luseobs=${luseobs},
 fixed_level=${fixed_level},
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
 print_img=${print_img},
&end
&param_lmlef
 obsda_in=${obsda_in},
 obsda_in_basename='${obsextf}.@@@@',
 obsg_out_basename='${obsinf}.@@@@',
 obsa_out_basename='${obsoutf}.@@@@',
 gues_in_basename=,
 anal_out_basename=,
 mean=${mean},
 tl=${tl},
 scl_mem=${scl_mem},
 debug_obs=${debug_obs},
 sigma_obs=${sigh},
 sigma_obsv=${sigv},
 gross_error=,
 cov_infl_mul=${infl_mul},
 sp_infl_rtpp=${infl_rtpp},
 sp_infl_rtps=${infl_rtps},
 relax_spread_out=${relax_spread_out},
 maxiter=${maxiter},
 nonlinear=,
 zupd=,
 save_info=${save_info},
 info_out_basename=,
 ewgt_basename=,
 q_update_top=${q_update_top},
 q_adjust=T,
 oma_monit=T,
 obsgues_output=T,
 obsanal_output=T,
 debug_time=T,
&end
EOF
#cat lmlef.nml

## prepare input files
rm -f ${obsf}.dat ${obsf2}.dat
ln -s ${obsdir}/${adate}/${obsf}.dat .
ln -s ${obsdir}/${adate}/${obsf2}.dat .
rm -f gues.*.grd ${obsinf}.*.dat ${obsextf}.*.dat
if [ $cycleda -eq 1 ];then
ln -s ${guesdir}/${pdate}/r_sig.f$fh gues.0000.sig.grd
ln -s ${guesdir}/${pdate}/r_sfc.f$fh gues.0000.sfc.grd
ln -s ${guesdir}/${pdate}/r_flx.f$fh gues.0000.flx.grd
else
ln -s ${guesdir}/${pdate}/${head}000/r_sig.f$fh gues.0000.sig.grd
ln -s ${guesdir}/${pdate}/${head}000/r_sfc.f$fh gues.0000.sfc.grd
ln -s ${guesdir}/${pdate}/${head}000/r_flx.f$fh gues.0000.flx.grd
fi
if [ $obsda_in = T ];then
ln -s ${obsdir}/${pdate}/${obsextf}.0000.dat .
fi
if [ -f ${guesdir}/${pdate}/infl.grd ];then
  ln -s ${guesdir}/${pdate}/infl.grd .
fi
gmem=$member
if [ $cycleda -eq 1 ] && [ $osse = T ] && [ $tmem -le $member ]; then
  gmem=`expr $member + 1`
fi
m=1
em=1
while [ $m -le $gmem ];do
mem=`printf '%0.3d' $m`
if [ $cycleda -eq 1 ] && [ $osse = T ] && [ $m -eq $tmem ]; then
else
mem2=`printf '%0.3d' $em`
ln -s ${guesdir}/${pdate}/${head}${mem}/r_sig.f$fh gues.0${mem2}.sig.grd
ln -s ${guesdir}/${pdate}/${head}${mem}/r_sfc.f$fh gues.0${mem2}.sfc.grd
ln -s ${guesdir}/${pdate}/${head}${mem}/r_flx.f$fh gues.0${mem2}.flx.grd
if [ $obsda_in = T ];then
ln -s ${obsdir}/${adate}/${obsextf}.0${mem}.dat ${obsextf}.0${mem2}.dat
fi
em=`expr $em + 1`
fi
m=`expr $m + 1`
done
rm -f STDIN lmlef
ln -s lmlef.nml STDIN
ln -s ${bindir}/lmlef .
## start calculation
rm -f ${obsoutf}.*.dat anal.*.grd
export OMP_NUM_THREADS=1
mpiexec -n $NODE ./lmlef 2>${logf}err || exit 11
if [ ! -z ${print_img} ];then
img=`printf '%0.3d' $print_img`
mv NOUT-$img ${logf}txt
else
mv NOUT-001 ${logf}txt
fi
rm -f NOUT-*
n=1
while [ $n -le $NODE ];do
img=`printf '%0.3d' $n`
if [ -f debug_time-${img}.txt ];then
  mv debug_time-${img}.txt debug_time-${img}.${head_da}txt
fi
n=`expr $n + 1`
done
#fi # ! -f ${logf}_n${NODE}.txt

## post process
### write GrADS files
for vtype in gues anal;do
for emem in ctrl mean sprd;do
if [ $emem = ctrl ];then
in=${vtype}.0000.sig.grd
else
in=${vtype}.${emem}.sig.grd
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
if [ $relax_spread_out = T ];then
  rm -f fort.*
  ln -s rtps.sig.grd fort.11
  ln -s rtps.bin fort.51
  ln -s rtps.ctl fort.61
  echo "rtps" >> read_sig.log
  ./read_sig < read_sig.nml >> read_sig.log || exit 11
  sed -i -e 's/DATAFILE/'rtps.bin'/g' rtps.ctl
  rm rtps.ctl-e
fi
if [ $save_info = T ];then
cat << EOF > read_info.nml
&namlst_info
 member=${member},
 emem=F,
 writectl=T,
&end
EOF
  rm -f fort.* read_info
  ln -s dainfo.sig.grd fort.11
  ln -s dainfo.bin fort.51
  ln -s dainfo.ctl fort.61
  ln -s ${bindir}/read_info .
  echo "dainfo" >> read_info.log
  ./read_info < read_info.nml >> read_info.log || exit 12
  sed -i -e 's/DATAFILE/dainfo.bin/g' dainfo.ctl
  rm dainfo.ctl-e
  m=1
  while [ $m -le $member ];do
  mem=`printf '%0.3d' $m`
  rm fort.*
  if [ $m -eq 1 ];then
cat << EOF > read_info.nml
&namlst_info
 member=${member},
 emem=T,
 writectl=T,
&end
EOF
  ln -s ewgt.ctl fort.61
else
cat << EOF > read_info.nml
&namlst_info
 member=${member},
 emem=T,
 writectl=F,
&end
EOF
  fi
  ln -s ewgt.0${mem}.sig.grd fort.11
  ln -s ewgt.0${mem}.bin fort.51
  echo "ewgt $mem" >> read_info.log
  ./read_info < read_info.nml >> read_info.log || exit 12
  if [ $m -eq 1 ];then
  sed -i -e 's/DATAFILE/..\/'${head_da}'%e\/ewgt.0%e.bin/g' ewgt.ctl
  rm ewgt.ctl-e
  fi
  m=`expr $m + 1`
  done
fi
### prepare forecast initial files
if [ -d ${head_da}000 ]; then
  rm -rf ${head_da}000
fi
mkdir -p ${head_da}000
cd ${head_da}000
### copy namelists
cp ../rsmparm .
cp ../rsmlocation .
cp ../rfcstparm .
cp ../station.parm .
### copy orography data
cp ../rmtn.parm .
cp ../rmtnoss .
cp ../rmtnslm .
cp ../rmtnvar .
mv ../anal.0000.sig.grd r_sig.f00
mv ../anal.0000.sfc.grd r_sfc.f00
mv ../gues.ctrl.bin .
mv ../gues.ctrl.ctl .
mv ../anal.ctrl.bin .
mv ../anal.ctrl.ctl .
if [ -f ../${obsinf}.0000.dat ]; then
mv ../${obsinf}.0000.dat .
fi
if [ -f ../${obsoutf}.0000.dat ]; then
mv ../${obsoutf}.0000.dat . 
fi
if [ $relax_spread_out = T ];then
mv ../rtps.* .
fi
if [ $save_info = T ];then
mv ../dainfo.* .
mv ../ewgt.ctl .
fi
#cd $wdir
#cp $wdir/${head_da}000/r_sig.f00 .
#cp $wdir/${head_da}000/r_sfc.f00 .
cp r_sig.f00 r_sigi
cp r_sig.f00 r_sigitdt
cp r_sfc.f00 r_sfci
cd $wdir
m=1
while [ $m -le $member ];do
mem=`printf '%0.3d' $m`
if [ -d ${head_da}${mem} ];then
  rm -rf ${head_da}${mem}
fi
mkdir -p ${head_da}${mem}
cd ${head_da}${mem}
### copy namelists
cp ../rsmparm .
cp ../rsmlocation .
cp ../rfcstparm .
cp ../station.parm .
### copy orography data
cp ../rmtn.parm .
cp ../rmtnoss .
cp ../rmtnslm .
cp ../rmtnvar .
mv ../anal.0$mem.sig.grd r_sig.f00
mv ../anal.0$mem.sfc.grd r_sfc.f00
if [ -f ../${obsinf}.0$mem.dat ]; then
mv ../${obsinf}.0$mem.dat .
fi
if [ -f ../${obsoutf}.0$mem.dat ]; then
mv ../${obsoutf}.0$mem.dat .
fi
if [ $save_info = T ]; then
mv ../ewgt.0$mem.* .
fi
cp r_sig.f00 r_sigi
cp r_sig.f00 r_sigitdt
cp r_sfc.f00 r_sfci
cd $wdir
m=`expr $m + 1`
done
for emem in mean sprd;do
#mkdir -p ${guesdir}/${pdate}/${head}${emem}
#mv gues.${emem}.sig.grd ${guesdir}/${pdate}/${head}${emem}/r_sig.f$fh
#mv gues.${emem}.sfc.grd ${guesdir}/${pdate}/${head}${emem}/r_sfc.f$fh
#mv gues.${emem}.bin ${guesdir}/${pdate}/${head}${emem}/
#mv gues.${emem}.ctl ${guesdir}/${pdate}/${head}${emem}/
if [ -d ${head_da}${emem} ];then
  rm -rf ${head_da}${emem}
fi
mkdir -p ${head_da}${emem}
mv gues.${emem}.sig.grd ${head_da}${emem}/r_sig.f$fh
mv gues.${emem}.sfc.grd ${head_da}${emem}/r_sfc.f$fh
mv gues.${emem}.bin ${head_da}${emem}/
mv gues.${emem}.ctl ${head_da}${emem}/
mv anal.${emem}.sig.grd ${head_da}${emem}/r_sig.f00
mv anal.${emem}.sfc.grd ${head_da}${emem}/r_sfc.f00
mv anal.${emem}.bin ${head_da}${emem}/
mv anal.${emem}.ctl ${head_da}${emem}/
done
rm gues.*.grd
if [ $obsda_in = T ];then
rm ${obsinf}.*.dat
fi
echo "END"
