#!/bin/sh
#
# add rescaled perturbations to r_sig.f00
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
CYCLE=${1:-$CYCLE}
PMEM=${2:-001} #prtb member
IDATE=${SDATE:-2022083000} #base
PDATE1=${PDATE1:-2022061112} #prtb base
PDATE2=${PDATE2:-2022061012} #prtb base
IRES=${IRES:-27}
BV_H=${INCCYCLE:-6}
TETYPE=${TETYPE}
SCL=${SCL}
QADJ=${QADJ:-yes} #super saturation and dry adjustment
BP=${BP} #with boundary perturbation
head=${HEAD:-bv$TETYPE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/post
DATADIR=${RUNDIR0:-/zdata/grmsm/work/dpac/rsm27}
BASE0=${BASEDIR0:-/zdata/grmsm/work/gfsp2rsm27_nomad}
BASE1=${BASEDIR1:-/zdata/grmsm/work/gfsp2rsm27_rda}
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
EXEC=addprtb
cd $SRCDIR
gmake ${EXEC}
OUTDIR=$DATADIR/$IDATE/${head}${PMEM}
mkdir -p $OUTDIR
##copy namelists
cp $DATADIR/$IDATE/rsmparm $OUTDIR/
cp $DATADIR/$IDATE/rsmlocation $OUTDIR/
cp $DATADIR/$IDATE/rfcstparm $OUTDIR/
cp $STTPRM $OUTDIR/station.parm
##copy orography data
cp $DATADIR/$IDATE/rmtn.parm $OUTDIR/
cp $DATADIR/$IDATE/rmtnoss $OUTDIR/
cp $DATADIR/$IDATE/rmtnslm $OUTDIR/
cp $DATADIR/$IDATE/rmtnvar $OUTDIR/
rm -rf $OUTDIR/tmp
mkdir -p $OUTDIR/tmp
cd $OUTDIR/tmp
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
# base field
echo $SDATE
ln -s $DATADIR/$IDATE/r_sig.f00 fort.11 #analysis
ln -s $DATADIR/$IDATE/r_sfc.f00 fort.14 #analysis
#ln -s $DATADIR/$IDATE/r_sig.f$fh fort.11 #guess
#ln -s $DATADIR/$IDATE/r_sfc.f$fh fort.14 #guess
# perturbation base
if [ $CYCLE -eq 1 ]; then
  if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
    BASE=$BASE0
    PDATE=$PDATE1
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
    cp r_sigi ../fort.12
    cd ..
    #PDATE=`date -j -f "%Y%m%d%H" -v-24H +"%Y%m%d%H" "${PDATE}"`
    BASE=$BASE0
    PDATE=$PDATE2
    if [ ! -d $BASE0/$PDATE ]; then
      BASE=$BASE1
    fi
    if [ ! -d $BASE/$PDATE ]; then
      echo "No such directory : $BASE/$PDATE"
      exit 3
    fi
    mkdir -p $PDATE
    cd $PDATE
    #hhr=24
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
    cp r_sigi ../fort.13
    cd ..
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
#  if [ do$BP = dowbp ];then
#    ln -s $DATADIR/$PDATE/$PMEM/r_sig.f$fh2 fort.12 #c
#  else
    ln -s $DATADIR/$PDATE/r_sig.f$fh2 fort.12 #a
#  fi
  #if [ $PCYCLE -eq 1 ]; then
  #ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
  #else
  ln -s $DATADIR/$PDATE/${head}${PMEM}/r_sig.f$fh2 fort.13 #c
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
if [ do$SCL != do ];then
  teref=${SCL}
  if echo "$SCL" | grep -q "^[0-9]\+$";then
    # SCL is integer
    teref=${SCL}.0d0
  fi
else
  teref=3.0d0
fi
#### set rescaling magnitude from ensemble spread statistics
cat <<EOF >namelist
&namlst_prtb
 setnorm=T,
 teref=${teref},
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
 adjust_q=${adjust_q},
&end
EOF
#
./${EXEC} < namelist 1>${EXEC}.log 2>${EXEC}.err
mv ${EXEC}.log ${EXEC}.err $OUTDIR/
fh=00 #a
mv fort.51 $OUTDIR/r_sig.f$fh
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigi
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigitdt
mv fort.53 $OUTDIR/r_sfc.f$fh
cp $OUTDIR/r_sfc.f$fh $OUTDIR/r_sfci
ls -l $OUTDIR
rm fort.*
echo END
