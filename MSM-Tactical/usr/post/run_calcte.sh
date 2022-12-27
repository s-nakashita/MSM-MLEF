#!/bin/sh
#
# calculate moist total energy for forecast difference
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
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
BP=${BP} #with boundary perturbation
MEM=${1:-$MEM}
CYCLE=${2:-$CYCLE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
  DATADIR=/zdata/grmsm/work/rsm2rsm27_bvgfs
  EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
  DATADIR=/zdata/grmsm/work/rsm2msm9_bvgfs
  EXPDIR=$MSMDIR/usr/exp/rsm2msm9_bv
elif [ $IRES -eq 3 ]; then
  DATADIR=/zdata/grmsm/work/msm2msm3_bv
  EXPDIR=$MSMDIR/usr/exp/msm2msm3_bv
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
EXEC=calcte
cd $SRCDIR
gmake ${EXEC}
cd ${EXPDIR}
. ./configure
cd -
mkdir -p tmp
cd tmp
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
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
    WDIR=bv${TETYPE}${SCL}${BV_H}h${MEM}${BP}
  else
    WDIR=bv${TETYPE}${SCL}${MEM}${BP}
  fi
  if [ do$QADJ = doyes ];then
    WDIR=${WDIR}_qadj
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
# latest forecast
  nsig=11
  fh=$dt #a
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  PDATE=`date -j -f "%Y%m%d%H" -v+${fhs}H +"%Y%m%d%H" "${IDATE}"` #a
  ln -s $DATADIR/$PDATE/r_sig.f$fh fort.$nsig #a
# previous forecast
  nsig=`expr $nsig + 1`
  fh=$dt
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  if [ $BV = yes ];then
    if [ $IRES -eq 27 ];then
      ln -s $DATADIR/$PDATE/${WDIR}_c${CYCLE}/r_sig.f$fh fort.$nsig #c
    else
      ln -s $DATADIR/$PDATE/${WDIR}/r_sig.f$fh fort.$nsig #c
    fi
  else
    ln -s $DATADIR/$PDATE/${MEM}/r_sig.f$fh fort.$nsig
  fi
  if [ $IRES -eq 27 ];then
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
&END
EOF
  else
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 epsq=${epsq},
 lonw=,
 lone=,
 lats=,
 latn=,
&END
EOF
  fi
  ./${EXEC} < NAMELIST #1>>${EXEC}.log 2>&1
  cat te.dat
  if [ $BV = yes ];then
    if [ $IRES -eq 27 ];then
      mv te.dat $DATADIR/$PDATE/${WDIR}_c${CYCLE}/te${dt}h.dat #c
    else
      mv te.dat $DATADIR/$PDATE/${WDIR}/te${dt}h.dat #c
    fi
  else
    mv te.dat $DATADIR/$PDATE/${MEM}/te${dt}h.dat
  fi
  rm fort.*
done
echo END
