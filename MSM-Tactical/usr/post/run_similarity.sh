#!/bin/sh
#
# calculate similarity index
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
BV=${BV:-yes}
QADJ=${QADJ:-yes} #super saturation and dry adjustment
ORTH=${ORTH:-no}
BP=${BP:-wbp} #with boundary perturbation
SCLBASE=${SCLBASE}
MEMBER=${MEMBER:-10}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/post
if [ $IRES -eq 27 ]; then
  DATADIR=/zdata/grmsm/work/rsm2rsm27_bvgfs
  DATADIR=/zdata/grmsm/work/rsm2rsm27_bv
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
if [ ! -d $EXPDIR ]; then
  echo "No such directory : $EXPDIR"
  exit 3
fi
EXEC=similarity
cd $SRCDIR
gmake ${EXEC}
cd ${EXPDIR}
. ./configure
#DATADIR=$TEMP
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
mkdir -p $DATADIR/${IDATE}/tmp
cd $DATADIR/${IDATE}/tmp
rm -f r_sig.* ${EXEC}
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
if [ "$TETYPE" = "dry" ];then
  epsq=0.0d0
elif [ "$TETYPE" = "weak" ]; then
  epsq=0.1d0
else
  epsq=1.0d0
fi
for dt in $(seq 0 $inch $dte);do
  fh=`printf '%0.2d' $dt`
  ## control
  ln -s $DATADIR/${IDATE}/r_sig.f$fh r_sig.0000
  ## member
  MEM=1
  while [ $MEM -le $MEMBER ]; do
  if [ $MEM -lt 10 ];then
    MEM=00$MEM
  else
    MEM=0$MEM
  fi
  if [ $BV = yes ];then
    if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
      WDIR=bv${TETYPE}${SCL}${BV_H}h${MEM}${BP}${SCLBASE}
    else
      WDIR=bv${TETYPE}${SCL}${MEM}${BP}${SCLBASE}
    fi
    if [ do$QADJ = doyes ];then
      WDIR=${WDIR}_qadj
    fi
    if [ do$ORTH = doyes ];then
      WDIR=${WDIR}_orth
    fi
    if [ $IRES -eq 27 ];then
      WDIR=${WDIR}_c${CYCLE}
    fi
  fi
  if [ ! -d $DATADIR/${IDATE}/${WDIR} ]; then
    echo 'no such directory '$DATADIR/${IDATE}/${WDIR}
    exit 99
  fi
  ln -s $DATADIR/${IDATE}/${WDIR}/r_sig.f$fh r_sig.0$MEM
  MEM=`expr $MEM + 1`
  done
  if [ $IRES -eq 27 ];then
cat <<EOF >NAMELIST
&NAMLST_SIML
 nens=${MEMBER},
 lmonit=T,
 epsq=${epsq},
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
&END
EOF
  else
cat <<EOF >NAMELIST
&NAMLST_SIML
 nens=${MEMBER},
 lmonit=T,
 epsq=${epsq},
 lonw=,
 lone=,
 lats=,
 latn=,
&END
EOF
  fi
  ./${EXEC} < NAMELIST #1>>${EXEC}.log 2>&1
  if [ $BV = yes ];then
    mv siml.dat $DATADIR/$IDATE/${WDIR}/siml${fh}h.dat #c
  else
    mv siml.dat $DATADIR/$IDATE/${MEM}/siml${fh}h.dat
  fi
  rm r_sig.*
done
echo END
