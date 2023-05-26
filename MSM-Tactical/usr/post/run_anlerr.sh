#!/bin/sh
set -x
if [ $# -lt 2 ]; then
  echo "Usage : ./run_anlerr.sh res(27 or 9) init(YYYYMMDDHH) [edate]"
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
elif [ $IRES -eq 18 ]; then
EXPN=rsm2rsm18_da
elif [ $IRES -eq 9 ]; then
EXPN=rsm2msm9_da
#SDATE0=2022061018
#SDATE0=2022061812
elif [ $IRES -eq 3 ]; then
EXPN=rsm2msm3_da
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
#EXEC=calcte
EXEC=calctegrd
cd $SRCDIR
gmake ${EXEC}
#gmake ${EXEC2}
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
#ln -s ${SRCDIR}/${EXEC2} ${EXEC2}

if [ $IRES -eq 27 ]; then
TRUEDIR=$WORKUSR/rsm2rsm27_truth
fhtruemax=24
elif [ $IRES -eq 18 ]; then
TRUEDIR=$WORKUSR/rsm2rsm18_truth
fhtruemax=48
elif [ $IRES -eq 9 ]; then
TRUEDIR=$WORKUSR/rsm2msm9_truth2
fhtruemax=24
elif [ $IRES -eq 3 ]; then
TRUEDIR=$WORKUSR/rsm2msm3_truthb
TRUENESTDIR=$WORKUSR/rsm2msm3_truthb
DANESTDIR=$WORKUSR/rsm2msm3_osseb
DANESTHEAD=noda000
fhtruemax=6
fi
echo $TRUEDIR
#tmem=`printf '%0.3d' $TMEM`
#truth=${HEAD}${tmem}
#echo $truth
fhtrue=0
icyc=$CYCLESTART
SDATE=$SDATE0
if [ $icyc -gt 1 ]; then
  icycp=`expr $icyc - 1`
  offset=`expr $icycp \* $INCCYCLE`
  SDATE=`date -j -f "%Y%m%d%H" -v+${offset}H +"%Y%m%d%H" "${SDATE}"`
fi
while [ $icyc -le $CYCLEMAX ];do
if [ $ANAL = T ] && [ $icyc -gt 1 ]; then
  icycp=`expr $icyc - 1`
  fhtrue=`expr $icycp \* $INCCYCLE`
fi
if [ $fhtrue -gt $fhtruemax ]; then
  break
fi
echo $SDATE
if [ $icyc -lt $DASTART ]; then
header=${HEAD}
if [ $DA_MEAN = T ]; then
header=${HEAD}m${MEMBER}
fi
else
header=${HEAD2}
fi
echo $ANAL $header $truth
fh=0
if [ $ANAL = F ]; then
end_hour=$ENDHOUR
else
end_hour=$INCCYCLE
fi
inc_h=$PRTHOUR
echo $end_hour $inc_h
#exit
while [ $fh -le $end_hour ]; do
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  rm -f fort.*
  # reference state
  lbase=F
  if [ $fhtrue -lt 10 ]; then
    fhtrue=0$fhtrue
  fi
  nsig=11
  #ln -s $DATADIR/$SDATE0/$truth/r_sig.f$fhtrue fort.$nsig
  ln -s $TRUEDIR/$SDATE0/r_sig.f$fhtrue fort.$nsig
  if [ $DANEST = T ]; then
    # truth base
    lbase=T
    nsig=`expr $nsig + 1`
    ln -s $TRUENESTDIR/$SDATE0/rbssigf$fhtrue fort.$nsig
  fi
# experiment
  nsig=`expr $nsig + 1`
  if [ $ANAL = F ]; then
#    if [ $DA_MEAN = T ]; then
#      ln -s $DATADIR/$SDATE0/${header}mean/r_sig.f$fhtrue fort.$nsig
#    else
    ln -s $DATADIR/$SDATE0/r_sig.f$fhtrue fort.$nsig
#    fi
  else
    if [ $DA_MEAN = T ]; then
      ln -s $DATADIR/$SDATE/${header}mean/r_sig.f$fh fort.$nsig #c
    else
      if [ $header = $HEAD ];then
        ln -s $DATADIR/$SDATE/r_sig.f$fh fort.$nsig #c
      else
        ln -s $DATADIR/$SDATE/${header}000/r_sig.f$fh fort.$nsig #c
      fi
    fi #DA_MEAN
  fi #ANAL
  if [ $DANEST = T ]; then
    # exp base
    nsig=`expr $nsig + 1`
    ln -s $DANESTDIR/$SDATE0/$DANESTHEAD/rbssigf$fhtrue fort.$nsig
  fi
if [ $IRES -eq 18 ]; then
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 lbase=${lbase},
 epsq=,
 lonw=115,
 lone=133,
 lats=26,
 latn=38,
 kmax=42,
&END
EOF
elif [ $IRES -gt 3 ]; then
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 lbase=${lbase},
 epsq=,
 lonw=118.5,
 lone=129.5,
 lats=28.5,
 latn=37.5,
 kmax=42,
&END
EOF
else
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 lbase=${lbase},
 epsq=,
 lonw=,
 lone=,
 lats=,
 latn=,
 kmax=42,
&END
EOF
fi
  rm -f te*.grd teprof*.dat
  ./${EXEC} < NAMELIST 1>>${EXEC}.log 2>&1 || exit 9 
#  cat te.dat
#  ./${EXEC2} < NAMELIST 1>>${EXEC2}.log 2>&1 || exit 9 
#  head -n 5 teprof.dat
  if [ $ANAL = F ]; then
#    if [ $DA_MEAN = T ];then
##      mv te.dat $DATADIR/${SDATE0}/${header}mean/te-err${fhtrue}h.dat #c
#      mv te.grd $DATADIR/${SDATE0}/${header}mean/te-err${fhtrue}h.grd
#      mv teprof.dat $DATADIR/${SDATE0}/${header}mean/teprof-err${fhtrue}h.dat
#    else
##      mv te.dat $DATADIR/${SDATE0}/te-err${fhtrue}h.dat #c
      if [ $DANEST = T ]; then
        mv teb.grd $DATADIR/${SDATE0}/tebase-err${fhtrue}h.grd
        mv teprofb.dat $DATADIR/${SDATE0}/teprofbase-err${fhtrue}h.dat
        mv tec.grd $DATADIR/${SDATE0}/tecross-err${fhtrue}h.grd
        mv teprofc.dat $DATADIR/${SDATE0}/teprofcross-err${fhtrue}h.dat
        mv tep.grd $DATADIR/${SDATE0}/teprtb-err${fhtrue}h.grd
        mv teprofp.dat $DATADIR/${SDATE0}/teprofprtb-err${fhtrue}h.dat
      else
        mv te.grd $DATADIR/${SDATE0}/te-err${fhtrue}h.grd
        mv teprof.dat $DATADIR/${SDATE0}/teprof-err${fhtrue}h.dat
      fi
#    fi
  else
    if [ $DA_MEAN = T ];then
      if [ $DANEST = T ]; then
        mv teb.grd $DATADIR/${SDATE}/${header}mean/tebase-err${fh}h.grd
        mv teprofb.dat $DATADIR/${SDATE}/${header}mean/teprofbase-err${fh}h.dat
        mv tec.grd $DATADIR/${SDATE}/${header}mean/tecross-err${fh}h.grd
        mv teprofc.dat $DATADIR/${SDATE}/${header}mean/teprofcross-err${fh}h.dat
        mv tep.grd $DATADIR/${SDATE}/${header}mean/teprtb-err${fh}h.grd
        mv teprofp.dat $DATADIR/${SDATE}/${header}mean/teprofprtb-err${fh}h.dat
      else
        mv te.grd $DATADIR/${SDATE}/${header}mean/te-err${fh}h.grd #c
        mv teprof.dat $DATADIR/${SDATE}/${header}mean/teprof-err${fh}h.dat #c
      fi
    else
      if [ $header = $HEAD ];then
        if [ $DANEST = T ]; then
          mv teb.grd $DATADIR/${SDATE}/tebase-err${fh}h.grd
          mv teprofb.dat $DATADIR/${SDATE}/teprofbase-err${fh}h.dat
          mv tec.grd $DATADIR/${SDATE}/tecross-err${fh}h.grd
          mv teprofc.dat $DATADIR/${SDATE}/teprofcross-err${fh}h.dat
          mv tep.grd $DATADIR/${SDATE}/teprtb-err${fh}h.grd
          mv teprofp.dat $DATADIR/${SDATE}/teprofprtb-err${fh}h.dat
        else
#          mv te.dat $DATADIR/${SDATE}/te-err${fh}h.dat
          mv te.grd $DATADIR/${SDATE}/te-err${fh}h.grd
          mv teprof.dat $DATADIR/${SDATE}/teprof-err${fh}h.dat
	fi
      else
        if [ $DANEST = T ]; then
          mv teb.grd $DATADIR/${SDATE}/${header}000/tebase-err${fh}h.grd
          mv teprofb.dat $DATADIR/${SDATE}/${header}000/teprofbase-err${fh}h.dat
          mv tec.grd $DATADIR/${SDATE}/${header}000/tecross-err${fh}h.grd
          mv teprofc.dat $DATADIR/${SDATE}/${header}000/teprofcross-err${fh}h.dat
          mv tep.grd $DATADIR/${SDATE}/${header}000/teprtb-err${fh}h.grd
          mv teprofp.dat $DATADIR/${SDATE}/${header}000/teprofprtb-err${fh}h.dat
        else
#          mv te.dat $DATADIR/${SDATE}/${header}000/te-err${fh}h.dat
          mv te.grd $DATADIR/${SDATE}/${header}000/te-err${fh}h.grd
          mv teprof.dat $DATADIR/${SDATE}/${header}000/teprof-err${fh}h.dat
	fi
      fi
    fi
  fi
  rm fort.*
  fh=`expr $fh + $inc_h`
  fhtrue=`expr $fhtrue + $inc_h`
  if [ $ANAL = F ] && [ $fh -eq $end_hour ]; then
    break
  fi
if [ $fhtrue -gt $fhtruemax ]; then
  break
fi
done #fh
SDATE=`date -j -f "%Y%m%d%H" -v+${INCCYCLE}H +"%Y%m%d%H" "${SDATE}"`
icyc=`expr $icyc + 1`
done #cycle
echo END
