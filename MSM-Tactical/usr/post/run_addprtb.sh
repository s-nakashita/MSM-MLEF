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
#PDATE=${PDATE:-2022061112} #prtb base
#PMEM=${MEM:-001} #prtb member
PDATELIST=${PDATELIST:-pdate.txt}
IRES=27
CYCLE=${CYCLE:-3}
BV_H=${BV_H:-6}
TETYPE=${TETYPE}
SCL=${SCL}
QADJ=${QADJ:-no} #super saturation and dry adjustment
ORTH=${ORTH:-no} #orthogonalization
BP=${BP} #with boundary perturbation
SCLBASE=${SCLBASE}
SCLPOW=${SCLPOW}
CYCLE=${1:-$CYCLE}
#IDATE=${1}
#IRES=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
#SRCDIR=${MSMDIR}/usr/post
SRCDIR=${MSMDIR}/dpac/build/pre
if [ $IRES -eq 27 ]; then
  if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
    BASE0=/zdata/grmsm/work/gfsp2rsm27_nomad
    BASE1=/zdata/grmsm/work/gfsp2rsm27_rda
  else
    BASE=/zdata/grmsm/work/gefs2rsm27_nomad
  fi
  EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
  BASE=/zdata/grmsm/work/gfsp2rsm27_nomad
  EXPDIR=$MSMDIR/usr/exp/rsm2msm9
elif [ $IRES -eq 3 ]; then
  BASE=/zdata/grmsm/work/rsm2msm9_jpn
  EXPDIR=$MSMDIR/usr/exp/msm2msm3
else
  echo "Invalid resolution. Specify 9 or 3."
  exit 2
fi
if [ ! -d $EXPDIR ]; then
  echo "No such directory : $EXPDIR"
  exit 3
fi
cd $EXPDIR
. ./configure
DATADIR=$TEMP
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
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
echo $IDATE
worktmp=$DATADIR/$IDATE/tmp
rm -rf $worktmp
mkdir -p $worktmp
cd $worktmp
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
# base field
rm -f fort.* rb.* ri.* ro.*
#ln -s $DATADIR/$IDATE/r_sig.f00 fort.11
#ln -s $DATADIR/$IDATE/r_sfc.f00 fort.12
ln -s $DATADIR/$IDATE/r_sig.f00 rb.0000.sig.grd
ln -s $DATADIR/$IDATE/r_sfc.f00 rb.0000.sfc.grd
# prtb field
nisig1=13
nisig2=14
nosig=51
nosfc=52
irow=1
MEM=1
while [ $MEM -le $MEMBER ]; do
echo 'input unit ' $nisig1 $nisig2
echo 'output unit ' $nosig $nosfc
PMEM=`printf '%0.3d' $MEM`
WDIR=bv${TETYPE}${SCL}${PMEM}${BP}${SCLBASE}
if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
  WDIR=bv${TETYPE}${SCL}${BV_H}h${PMEM}${BP}${SCLBASE}
fi
if [ ! -z $SCLPOW ]; then
  WDIR=${WDIR}p${SCLPOW}
fi
if [ do$QADJ = doyes ];then
  WDIR=${WDIR}_qadj
fi
if [ do$ORTH = doyes ];then
  WDIR=${WDIR}_orth
fi
OUTDIR=$DATADIR/$IDATE/${WDIR}_c${CYCLE}
rm -rf $OUTDIR
mkdir -p $OUTDIR
##copy orography
cp $DATADIR/$IDATE/rmtn.parm $OUTDIR/
cp $DATADIR/$IDATE/rmtnoss $OUTDIR/
cp $DATADIR/$IDATE/rmtnslm $OUTDIR/
cp $DATADIR/$IDATE/rmtnvar $OUTDIR/
MEM4=`printf '%0.4d' $MEM`
ln -s $OUTDIR/r_sig.f00 ro.$MEM4.sig.grd
ln -s $OUTDIR/r_sfc.f00 ro.$MEM4.sfc.grd
# perturbation base
if [ $CYCLE -eq 1 ]; then
  if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
    PDATE=`cat ${PDATELIST} | awk '{if(NR == '$MEM') {print $1}}'`
    echo $PDATE
    BASE=$BASE0
    if [ ! -d $BASE0/$PDATE ]; then
      BASE=$BASE1
    fi
    if [ ! -d $BASE/$PDATE ]; then
      echo "No such directory : $BASE/$PDATE"
      exit 3
    fi
    mkdir -p $PDATE
    cd $PDATE
    hhr=12
    cp $DATADIR/$IDATE/rsmlocation .
    ln -s $DATADIR/$IDATE/rmtn.parm .
    ln -s $DATADIR/$IDATE/rmtnoss .
    ln -s $DATADIR/$IDATE/rmtnslm .
    ln -s $DATADIR/$IDATE/rmtnvar .
    if [ do$G2R = doyes ] ; then
      ln -fs $BASE/$PDATE/sigf$hhr rb_sigf$hhr
      ln -fs $BASE/$PDATE/sfcf$hhr rb_sfcf$hhr
    fi
    if [ do$P2R = doyes ] ; then
      if [ do$CWBGFS = doyes ] ; then
        if [ $hhr -lt 100 ] ; then hhrr=0$hhr ; fi
        ln -fs $BASE/$PDATE/otgb2_$hhrr rb_pgbf$hhr
      else
        ln -fs $BASE/$PDATE/pgbf$hhr rb_pgbf$hhr
      fi
    else
      if [ do$C2R = doyes ] ; then
        ln -fs $BASE/$PDATE/r_sig.f$hhr rb_sigf$hhr
        ln -fs $BASE/$PDATE/r_sfc.f$hhr rb_sfcf$hhr
      fi
    fi
    $USHDIR/rinp.sh $NEST $hhr || exit 10
    MEM4=`expr 2 \* $MEM - 1`
    MEM4=`printf '%0.4d' $MEM4`
    cp r_sigi ../ri.$MEM4.sig.grd
    cd ..
    PDATE=`date -j -f "%Y%m%d%H" -v-12H +"%Y%m%d%H" "${PDATE}"`
    BASE=$BASE0
    if [ ! -d $BASE0/$PDATE ]; then
      BASE=$BASE1
    fi
    if [ ! -d $BASE/$PDATE ]; then
      echo "No such directory : $BASE/$PDATE"
      exit 3
    fi
    mkdir -p $PDATE
    cd $PDATE
    hhr=24
    cp $DATADIR/$IDATE/rsmlocation .
    ln -s $DATADIR/$IDATE/rmtn.parm .
    ln -s $DATADIR/$IDATE/rmtnoss .
    ln -s $DATADIR/$IDATE/rmtnslm .
    ln -s $DATADIR/$IDATE/rmtnvar .
    if [ do$G2R = doyes ] ; then
      ln -fs $BASE/$PDATE/sigf$hhr rb_sigf$hhr
      ln -fs $BASE/$PDATE/sfcf$hhr rb_sfcf$hhr
    fi
    if [ do$P2R = doyes ] ; then
      if [ do$CWBGFS = doyes ] ; then
        if [ $hhr -lt 100 ] ; then hhrr=0$hhr ; fi
        ln -fs $BASE/$PDATE/otgb2_$hhrr rb_pgbf$hhr
      else
        ln -fs $BASE/$PDATE/pgbf$hhr rb_pgbf$hhr
      fi
    else
      if [ do$C2R = doyes ] ; then
        ln -fs $BASE/$PDATE/r_sig.f$hhr rb_sigf$hhr
        ln -fs $BASE/$PDATE/r_sfc.f$hhr rb_sfcf$hhr
      fi
    fi
    $USHDIR/rinp.sh $NEST $hhr || exit 10
    MEM4=`expr 2 \* $MEM`
    MEM4=`printf '%0.4d' $MEM4`
    cp r_sigi ../ri.$MEM4.sig.grd
    cd ..
  else #ensemble
    #ln -s $DATADIR/$IDATE/$PMEM/r_sig.f00 fort.12
    #ln -s $DATADIR/$IDATE/r_sig.f00 fort.13
    MEM4=`expr 2 \* $MEM - 1`
    MEM4=`printf '%0.4d' $MEM4`
    ln -s $DATADIR/$IDATE/$PMEM/r_sig.f00 ri.$MEM4.sig.grd
    MEM4=`expr 2 \* $MEM`
    MEM4=`printf '%0.4d' $MEM4`
    ln -s $DATADIR/$IDATE/r_sig.f00 ri.$MEM4.sig.grd
  fi
else
  PCYCLE=`expr $CYCLE - 1`
  fh2=$BV_H
  if [ $fh2 -lt 10 ]; then
    fh2=0$fh2
  fi
  PDATE=`date -j -f "%Y%m%d%H" -v-${BV_H}H +"%Y%m%d%H" "${IDATE}"` #a
  echo $PDATE #a
#  if [ do$BP = dowbp ];then
#    ln -s $DATADIR/$PDATE/$PMEM/r_sig.f$fh2 fort.12 #c
#  else
    #ln -s $DATADIR/$PDATE/r_sig.f$fh2 fort.12 #a
    MEM4=`expr 2 \* $MEM - 1`
    MEM4=`printf '%0.4d' $MEM4`
    ln -s $DATADIR/$PDATE/r_sig.f$fh2 ri.$MEM4.sig.grd
#  fi
  #if [ $PCYCLE -eq 1 ]; then
  #ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
  #else
  #ln -s $DATADIR/$PDATE/${WDIR}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
  MEM4=`expr 2 \* $MEM`
  MEM4=`printf '%0.4d' $MEM4`
  ln -s $DATADIR/$PDATE/${WDIR}_c$PCYCLE/r_sig.f$fh2 ri.$MEM4.sig.grd
  #fi
fi
#
nisig1=`expr $nisig1 + 2`
nisig2=`expr $nisig2 + 2`
nosig=`expr $nosig + 2`
nosfc=`expr $nosfc + 2`
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
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
if [ do$ORTH = doyes ];then
  orth=T
fi
SPINUP=`expr 24 / $BV_H + 1`
if [ do$SCL != do ];then
  teref=${SCL}
  if echo "$SCL" | grep -q "^[0-9]\+$";then
    # SCL is integer
    teref=${SCL}.0d0
  fi
else
  teref=3.0d0
fi
if [ $CYCLE -lt $SPINUP ];then
cat <<EOF >namelist
&namlst_prtb
 member=${MEMBER},
 setnorm=T,
 teref=${teref},
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
 kmax=,
 adjust_q=${adjust_q},
 orth=${orth},
&end
EOF
else
#### set rescaling magnitude from ensemble spread statistics
cat <<EOF >namelist
&namlst_prtb
 member=${MEMBER},
 setnorm=T,
 teref=${teref},
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
 kmax=,
 adjust_q=${adjust_q},
 orth=${orth},
&end
EOF
fi
./${EXEC} < namelist 2>${EXEC}.err #1>>${EXEC}.log 2>&1
fh=00 #a
MEM=1
while [ $MEM -le $MEMBER ]; do
PMEM=`printf '%0.3d' $MEM`
WDIR=bv${TETYPE}${SCL}${PMEM}${BP}${SCLBASE}
if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
  WDIR=bv${TETYPE}${SCL}${BV_H}h${PMEM}${BP}${SCLBASE}
fi
if [ ! -z $SCLPOW ]; then
  WDIR=${WDIR}p${SCLPOW}
fi
if [ do$QADJ = doyes ];then
  WDIR=${WDIR}_qadj
fi
if [ do$ORTH = doyes ];then
  WDIR=${WDIR}_orth
fi
OUTDIR=$DATADIR/$IDATE/${WDIR}_c${CYCLE}
#mv fort.51 $OUTDIR/r_sig.f$fh
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigi
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigitdt
#mv fort.53 $OUTDIR/r_sfc.f$fh
cp $OUTDIR/r_sfc.f$fh $OUTDIR/r_sfci
ls -l $OUTDIR
#rm fort.*
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
echo END
