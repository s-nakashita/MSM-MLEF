#!/bin/sh
#
# calculate spectral coefficients for forecast difference
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_spectra.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
IDATE=${IDATE:-2022083000}
IRES=${IRES:-27}
CYCLE=${CYCLE:-1}
BV_H=${BV_H:-6}
TETYPE=${TETYPE}
SCL=${SCL}
MEM=${MEM:-003}
BV=${BV:-yes}
QADJ=${QADJ:-no} #super saturation and dry adjustment
ORTH=${ORTH:-no} #orthogonalization
BP=${BP} #with boundary perturbation
SCLBASE=${SCLBASE}
SCLPOW=${SCLPOW}
MEM=${1:-$MEM}
CYCLE=${2:-$CYCLE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
#SRCDIR=${MSMDIR}/usr/post
SRCDIR=${MSMDIR}/dpac/build/post
if [ $IRES -eq 27 ]; then
  EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
  EXPDIR=$MSMDIR/usr/exp/rsm2msm9_bv
elif [ $IRES -eq 3 ]; then
  EXPDIR=$MSMDIR/usr/exp/msm2msm3_bv
else
  echo "Invalid resolution. Specify 9 or 3."
  exit 2
fi
if [ ! -d $EXPDIR ]; then
  echo "No such directory : $EXPDIR"
  exit 3
fi
EXEC=spectra
cd $SRCDIR
gmake ${EXEC}
cd ${EXPDIR}
. ./configure
cd -
DATADIR=$TEMP
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
mkdir -p tmp
cd tmp
rm -f ${EXEC} base.sig.grd prtb.sig.grd
ln -s ${SRCDIR}/${EXEC} ${EXEC}
if [ $CYCLE -gt 1 ];then
  fhs=`expr $BV_H \* \( $CYCLE - 1 \)`
else
  fhs=0
fi
dte=$ENDHOUR
inch=$PRTHOUR
SPINUP=`expr 24 / $BV_H + 1`
set +e
EXTEND=`expr $fhs % 24`
set -e
if [ $IRES -eq 27 ]; then
  if [ $EXTEND -ne 0 ] || [ $CYCLE -lt $SPINUP ]; then
    dte=$BV_H #a
  fi
fi
if [ $BV = yes ];then
  if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
    WDIR=bv${TETYPE}${SCL}${BV_H}h${MEM}${BP}${SCLBASE}
  else
    WDIR=bv${TETYPE}${SCL}${MEM}${BP}${SCLBASE}
  fi
  if [ ! -z $SCLPOW ];then
    WDIR=${WDIR}p${SCLPOW}
  fi
  if [ do$QADJ = doyes ];then
    WDIR=${WDIR}_qadj
  fi
  if [ do$ORTH = doyes ];then
    WDIR=${WDIR}_orth
  fi
fi
if [ "$TETYPE" = "dry" ];then
  epsq=0.0d0
elif [ "$TETYPE" = "weak" ]; then
  epsq=0.1d0
else
  epsq=1.0d0
fi
for dt in $(seq 0 $inch $dte);do
  CDATE=$IDATE
# base
  fh=$dt #a
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  PDATE=`date -j -f "%Y%m%d%H" -v+${fhs}H +"%Y%m%d%H" "${IDATE}"` #a
  ln -s $DATADIR/$PDATE/r_sig.f$fh base.sig.grd
# prtb
  fh=$dt
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  if [ $BV = yes ];then
    if [ $IRES -eq 27 ];then
      ln -s $DATADIR/$PDATE/${WDIR}_c${CYCLE}/r_sig.f$fh prtb.sig.grd
    else
      ln -s $DATADIR/$PDATE/${WDIR}/r_sig.f$fh prtb.sig.grd
    fi
  else
    ln -s $DATADIR/$PDATE/${MEM}/r_sig.f$fh prtb.sig.grd
  fi
cat <<EOF >NAMELIST
&NAMLST_SPECTRA
 lprtb=T,
&END
EOF
  ./${EXEC} < NAMELIST #1>>${EXEC}.log 2>&1
  if [ $BV = yes ];then
    if [ $IRES -eq 27 ];then
      mv spectrum* $DATADIR/$PDATE/${WDIR}_c${CYCLE}/
    else
      mv spectrum* $DATADIR/$PDATE/${WDIR}/
    fi
  else
    mv spectrum* $DATADIR/$PDATE/${MEM}/
  fi
  rm base.sig.grd prtb.sig.grd
done
echo END
