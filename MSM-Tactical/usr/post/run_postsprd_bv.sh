#!/bin/sh
#
# post processes for ensemble spread
#
set -ex
if [ $# -lt 2 ]; then
  echo "Usage : ./run_postsprd_bv.sh init(YYYYMMDDHH) res(27 or 9) [bv_h, tetype, qadj, wbp, edate]"
  exit 1
fi
SDATE=${1}
IRES=${2}
#export SDATE IRES
TETYPE=${3:-dry}
TESIZE=${4}
BP=${5}
SCLBASE=${6}
SCLPOW=${7}
QADJ=${8:-no}
ORTH=${9:-no}
BV_H=${BV_H:-6}
MEMBER=10
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
#SRCDIR=${MSMDIR}/usr/post
SRCDIR=${MSMDIR}/dpac/build/post
SDATE0=2022082900
ut0=`date -j -f "%Y%m%d%H" +"%s" "${SDATE0}"`
echo $SDATE0 $ut0
ut=`date -j -f "%Y%m%d%H" +"%s" "${SDATE}"`
echo $SDATE $ut
dt=`expr ${ut} - ${ut0}`
icyc=`expr $dt / $BV_H / 3600 + 1`
echo $dt $icyc
if [ $IRES -eq 27 ]; then
#EXPDIR=$MSMDIR/usr/exp/gefs2rsm27
EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
SUF=_c${icyc}
elif [ $IRES -eq 9 ]; then
EXPDIR=$MSMDIR/usr/exp/rsm2msm9_bv
SUF=
elif [ $IRES -eq 3 ]; then
EXPDIR=$MSMDIR/usr/exp/msm2msm3_bv
SUF=
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
echo $IGRD $JGRD
DATADIR=$TEMP/$SDATE
if [ ! -d $DATADIR ]; then
echo "No such directory : $DATADIR"
exit 3
fi
#exit
EXEC1=spectra
EXEC2=profile
cd $SRCDIR
gmake ${EXEC1} || exit 5
gmake ${EXEC2} || exit 6
if [ $BV_H -eq 6 ]; then
HEAD=bv${TETYPE}${TESIZE}
else
HEAD=bv${TETYPE}${TESIZE}${BV_H}h
fi
if [ do$ORTH = doyes ]; then
SUF=_orth${SUF}
fi
if [ do$QADJ = doyes ]; then
SUF=_qadj${SUF}
fi
if [ ! -z $BP ] && [ ! -z $SCLPOW ]; then
  SUF=${BP}${SCLBASE}p${SCLPOW}${SUF}
else
  SUF=${BP}${SCLBASE}${SUF}
fi
OUTDIR=$DATADIR/${HEAD}sprd${SUF}
rm -rf $OUTDIR/tmp
mkdir -p $OUTDIR/tmp
cd $OUTDIR/tmp
ln -s ${SRCDIR}/${EXEC1} ${EXEC1}
ln -s ${SRCDIR}/${EXEC2} ${EXEC2}
fh=0
end_hour=$ENDHOUR
inc_h=$PRTHOUR
if [ $IRES -eq 27 ] && [ $icyc -lt 5 ]; then
  end_hour=$BV_H
fi
while [ $fh -le $end_hour ]; do
rm -f *.grd
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
ln -s $OUTDIR/r_sig.f$fh base.sig.grd
ln -s $OUTDIR/r_sfc.f$fh base.sfc.grd
ln -s $OUTDIR/r_flx.f$fh base.flx.grd
## spectra
cat <<EOF > spectra.nml
&namlst_spectra
 lprtb=F,
&end
EOF
./${EXEC1} < spectra.nml || exit 10 #1>>${EXEC}.log 2>&1
mv spectrum* $OUTDIR/
## profile
cat <<EOF > profile.nml
&namlst_prof
 lprtb=F,
 lonw=,
 lone=,
 lats=,
 latn=,
&end
EOF
./${EXEC2} < profile.nml || exit 10 #1>>${EXEC}.log 2>&1
mv prof*.bin prof*.ctl $OUTDIR/
rm base.*.grd
fh=`echo $fh + $inc_h | bc`
done
ls -ltr $OUTDIR | tail -n 10
echo END
