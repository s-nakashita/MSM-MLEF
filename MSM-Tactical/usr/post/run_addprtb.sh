#!/bin/sh
#
# add rescaled perturbations to r_sig.f00
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
IDATE=${IDATE:-2022083000} #base
PDATE=${PDATE:-2022061112} #prtb base
PMEM=${MEM:-001} #prtb member
IRES=27
CYCLE=${CYCLE:-3}
BV_H=${BV_H:-6}
TETYPE=${TETYPE}
QADJ=${QADJ:-no} #super saturation and dry adjustment
BP=${BP} #with boundary perturbation
CYCLE=${1:-$CYCLE}
#IDATE=${1}
#IRES=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
  if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
    BASE=/zdata/grmsm/work/gfsp2rsm27_nomad
    BASE=/zdata/grmsm/work/gfsp2rsm27_rda
  else
    BASE=/zdata/grmsm/work/gefs2rsm27_nomad
  fi
  DATADIR=/zdata/grmsm/work/rsm2rsm27_bvgfs
  EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
  BASE=/zdata/grmsm/work/gfsp2rsm27_nomad
  DATADIR=/zdata/grmsm/work/rsm2msm9_jpn
  #DATADIR=/zdata/grmsm/work/rsm2msm9_tparc
  EXPDIR=$MSMDIR/usr/exp/rsm2msm9
elif [ $IRES -eq 3 ]; then
  BASE=/zdata/grmsm/work/rsm2msm9_jpn
  DATADIR=/zdata/grmsm/work/msm2msm3_jpn
  EXPDIR=$MSMDIR/usr/exp/msm2msm3
else
  echo "Invalid resolution. Specify 9 or 3."
  exit 2
fi
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
if [ ! -d $EXPDIR ]; then
  echo "No such directory : $EXPDIR"
  exit 3
fi
EXEC=addprtb
cd $SRCDIR
gmake ${EXEC}
if [ $CYCLE -gt 1 ]; then
  fh=`expr $BV_H \* \( $CYCLE - 1 \)`
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  echo "forecast hour=${fh}"
  IDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${IDATE}"` #a
fi
WDIR=bv${TETYPE}${PMEM}${BP}
if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
  WDIR=bv${TETYPE}${BV_H}h${PMEM}${BP}
fi
if [ do$QADJ = doyes ];then
  WDIR=${WDIR}_qadj
fi
OUTDIR=$DATADIR/$IDATE/${WDIR}_c${CYCLE}
rm -rf $OUTDIR
mkdir -p $OUTDIR
##copy orography
cp $DATADIR/$IDATE/rmtn.parm $OUTDIR/
cp $DATADIR/$IDATE/rmtnoss $OUTDIR/
cp $DATADIR/$IDATE/rmtnslm $OUTDIR/
cp $DATADIR/$IDATE/rmtnvar $OUTDIR/
rm -rf tmp
mkdir -p tmp
cd tmp
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
# base field
echo $IDATE
ln -s $DATADIR/$IDATE/r_sig.f00 fort.11 #analysis
ln -s $DATADIR/$IDATE/r_sfc.f00 fort.14 #analysis
#ln -s $DATADIR/$IDATE/r_sig.f$fh fort.11 #guess
#ln -s $DATADIR/$IDATE/r_sfc.f$fh fort.14 #guess
# perturbation base
if [ $CYCLE -eq 1 ]; then
  if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
    cd ${EXPDIR}
    . ./configure
    export RUNFCST=no
    export RUNRINP2=yes
    export POSTTYPE=
    cd -
    echo $IGRD $JGRD
    mkdir -p $PDATE
    cd $PDATE
    export SDATE=$PDATE
    export BASEDIR=${BASE}/${SDATE}
    export RUNDIR=`pwd`
    echo $BASEDIR $RUNDIR
    export ENDHOUR=12
    export INCHOUR=12
    $JSHDIR/rsm_fcst.sh > rint.log 2>&1 || exit 10
    cd ..
    ln -s $RUNDIR/r_sig.f12 fort.12
    PDATE=`date -j -f "%Y%m%d%H" -v-12H +"%Y%m%d%H" "${PDATE}"`
    mkdir -p $PDATE
    cd $PDATE
    export SDATE=$PDATE
    export BASEDIR=${BASE}/${SDATE}
    export RUNDIR=`pwd`
    echo $BASEDIR $RUNDIR
    export ENDHOUR=24
    export INCHOUR=24
    $JSHDIR/rsm_fcst.sh > rint.log 2>&1 || exit 10
    cd ..
    ln -s $RUNDIR/r_sig.f24 fort.13
  else #ensemble
    ln -s $DATADIR/$IDATE/$PMEM/r_sig.f00 fort.12
    ln -s $DATADIR/$IDATE/r_sig.f00 fort.13
  fi
else
  PCYCLE=`expr $CYCLE - 1`
  fh2=$BV_H
  if [ $fh2 -lt 10 ]; then
    fh2=0$fh2
  fi
  PDATE=`date -j -f "%Y%m%d%H" -v-${BV_H}H +"%Y%m%d%H" "${IDATE}"` #a
  echo $PDATE #a
  if [ do$BP = dowbp ];then
    ln -s $DATADIR/$PDATE/$PMEM/r_sig.f$fh2 fort.12 #c
  else
    ln -s $DATADIR/$PDATE/r_sig.f$fh2 fort.12 #a
  fi
  #if [ $PCYCLE -eq 1 ]; then
  #ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
  #else
  ln -s $DATADIR/$PDATE/${WDIR}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
  #fi
fi
### set namelist
if [ "$TETYPE" = "dry" ];then
  epsq=0.0d0
elif [ "$TETYPE" = "weak" ]; then
  epsq=0.1d0
else
  epsq=1.0d0
fi
if [ do$QADJ = doyes ];then
  adjust_q=T
fi
SPINUP=`expr 24 / $BV_H + 1`
if [ $CYCLE -lt $SPINUP ];then
cat <<EOF >namelist
&namlst_prtb
 setnorm=T,
 teref=3.0d0,
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
 adjust_q=${adjust_q},
&end
EOF
else
#### set rescaling magnitude from ensemble spread statistics
cat <<EOF >namelist
&namlst_prtb
 setnorm=T,
 teref=3.0d0,
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
 adjust_q=${adjust_q},
&end
EOF
fi
./${EXEC} < namelist #1>>${EXEC}.log 2>&1
fh=00 #a
mv fort.51 $OUTDIR/r_sig.f$fh
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigi
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigitdt
mv fort.53 $OUTDIR/r_sfc.f$fh
cp $OUTDIR/r_sfc.f$fh $OUTDIR/r_sfci
ls -l $OUTDIR
rm fort.*
echo END
