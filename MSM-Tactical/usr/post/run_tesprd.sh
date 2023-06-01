#!/bin/sh
set -x
if [ $# -lt 2 ]; then
  echo "Usage : ./run_tesprd.sh res(27 or 9) init(YYYYMMDDHH) [edate]"
  exit 1
fi
IRES=${1}
SDATE0=${2}
#export SDATE IRES
ANAL=${3:-T}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
#SRCDIR=${MSMDIR}/usr/post
SRCDIR=${MSMDIR}/dpac/build/post
if [ $IRES -eq 27 ]; then
EXPN=rsm2rsm27_da
#SDATE0=2022061000
elif [ $IRES -eq 9 ]; then
EXPN=rsm2msm9_da
#SDATE0=2022061018
#SDATE0=2022061812
elif [ $IRES -eq 3 ]; then
EXPN=msm2msm3_da
else
echo "Invalid resolution. Specify 9 or 3."
exit 2
fi
ut0=`date -j -f "%Y%m%d%H" +"%s" "${SDATE0}"`
echo $SDATE0 $ut0
EXPDIR=${MSMDIR}/usr/exp/$EXPN
if [ ! -d $EXPDIR ]; then
echo "No such directory : $EXPDIR"
exit 3
fi
cd $EXPDIR
. ./configure
echo $IGRD $JGRD
MEM=000
BV_H=${INCCYCLE:-6}
echo "MEMBER="$MEMBER
echo "BV_H="$BV_H
echo "CYCLESTART CYCLEMAX DASTART" $CYCLESTART $CYCLEMAX $DASTART
echo "TETYPE QADJ BP " $TETYPE $QADJ $BP
echo "DA_MEAN " $DA_MEAN
echo "OSSE TMEM "$OSSE $TMEM
#exit
EXEC=calctesprd
cd $SRCDIR
gmake ${EXEC}
#DATADIR=/zdata/grmsm/work/$EXPN
DATADIR=$TEMP
echo $DATADIR
if [ ! -d $DATADIR ]; then
echo "No such directory : $DATADIR"
exit 4
fi
rm -rf $DATADIR/tmp
mkdir -p $DATADIR/tmp
cd $DATADIR/tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}

fhfree=0
icyc=$CYCLESTART
#icyc=5
SDATE=$SDATE0
if [ $icyc -gt 1 ]; then
  icycp=`expr $icyc - 1`
  fhfree=`expr $icycp \* $INCCYCLE`
  SDATE=`date -j -f "%Y%m%d%H" -v+${fhfree}H +"%Y%m%d%H" "${SDATE}"`
fi
while [ $icyc -le $CYCLEMAX ];do
if [ $ANAL = T ] && [ $icyc -gt 1 ]; then
  icycp=`expr $icyc - 1`
  fhfree=`expr $icycp \* $INCCYCLE`
fi
if [ $fhfree -gt 24 ]; then
  break
fi
echo $SDATE
if [ $ANAL = F ] || [ $icyc -lt $DASTART ]; then
header=${HEAD}
#if [ $DA_MEAN = T ]; then
#header=${HEAD}m${MEMBER}
#fi
else
header=${HEAD2}
fi
echo $header $truth
fh=0
end_hour=$ENDHOUR
if [ $icyc -ge $DASTART ]; then
end_hour=$INCCYCLE
fi
inc_h=$PRTHOUR
echo $end_hour $inc_h
#exit
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
if [ $fhfree -lt 10 ]; then
  fhfree=0$fhfree
fi
  rm -f r_sig.*
# base
  if [ $ANAL = F ]; then
  #  if [ $DA_MEAN = T ]; then
  #    ln -s $DATADIR/$SDATE0/${header}m${MEMBER}mean/r_sig.f$fhfree r_sig.mean
      ln -s $DATADIR/$SDATE0/${header}mean/r_sig.f$fhfree r_sig.mean
  #  else
  #    ln -s $DATADIR/$SDATE0/r_sig.f$fhfree r_sig.mean
  #  fi
  else
  #  if [ $DA_MEAN = T ]; then
      ln -s $DATADIR/$SDATE/${header}mean/r_sig.f$fh r_sig.mean #c
  #  else
  #    if [ $header = $HEAD ];then
  #      ln -s $DATADIR/$SDATE/r_sig.f$fh r_sig.mean #c
  #    else
  #      ln -s $DATADIR/$SDATE/${header}000/r_sig.f$fh r_sig.mean #c
  #    fi
  #  fi
  fi
# member
  MEM=1
  while [ $MEM -le $MEMBER ]; do
  if [ $MEM -lt 10 ];then
  MEM=00$MEM
  else
  MEM=0$MEM
  fi
  if [ $ANAL = F ]; then
  if [ ! -d $DATADIR/$SDATE0/${header}${MEM} ]; then
  exit 99
  fi
  ln -s $DATADIR/$SDATE0/${header}${MEM}/r_sig.f$fhfree r_sig.0$MEM
  else
  if [ ! -d $DATADIR/$SDATE/${header}${MEM} ]; then
  exit 99
  fi
  ln -s $DATADIR/$SDATE/${header}${MEM}/r_sig.f$fh r_sig.0$MEM
  fi
  MEM=`expr $MEM + 1`
  done
  if [ $IRES -eq 27 ];then
cat <<EOF >NAMELIST
&NAMLST_PRTB
 nens=${MEMBER},
 epsq=,
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
 kmax=42,
&END
EOF
  else
cat <<EOF >NAMELIST
&NAMLST_PRTB
 nens=${MEMBER},
 epsq=,
 lonw=,
 lone=,
 lats=,
 latn=,
 kmax=42,
&END
EOF
  fi
#  ./${EXEC} < NAMELIST || exit 9 #1>>${EXEC}.log 2>&1
#  cat te.dat
  ./${EXEC} < NAMELIST 1>>${EXEC}.log 2>&1 || exit 9 
  head -n 5 teprof.dat
  if [ $ANAL = F ]; then
#      mv te.dat $DATADIR/${SDATE0}/te-sprd${fhfree}h.dat #c
      mv te.grd $DATADIR/${SDATE0}/te-sprd${fhfree}h.grd
      mv teprof.dat $DATADIR/${SDATE0}/teprof-sprd${fhfree}h.dat
  else
    if [ $header = $HEAD ];then
#      mv te.dat $DATADIR/${SDATE}/te-sprd${fh}h.dat
      mv te.grd $DATADIR/${SDATE}/te-sprd${fh}h.grd
      mv teprof.dat $DATADIR/${SDATE}/teprof-sprd${fh}h.dat
    else
#      mv te.dat $DATADIR/${SDATE}/${header}sprd/te-sprd${fh}h.dat
      mv te.grd $DATADIR/${SDATE}/${header}sprd/te-sprd${fh}h.grd
      mv teprof.dat $DATADIR/${SDATE}/${header}sprd/teprof-sprd${fh}h.dat
    fi
  fi
  fh=`expr $fh + $inc_h`
  fhfree=`expr $fhfree + $inc_h`
  if [ $ANAL = F ] && [ $fh -eq $end_hour ]; then
    break
  fi
if [ $fhfree -gt 24 ]; then
  break
fi
done #fh
SDATE=`date -j -f "%Y%m%d%H" -v+${INCCYCLE}H +"%Y%m%d%H" "${SDATE}"`
icyc=`expr $icyc + 1`
done #cycle
echo END
