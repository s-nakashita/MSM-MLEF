#!/bin/sh
#
# post processes for ensemble spread
#
set -ex
if [ $# -lt 2 ]; then
  echo "Usage : ./run_postsprd_da.sh init(YYYYMMDDHH) res(27 or 9)"
  exit 1
fi
SDATE=${1}
IRES=${2}
#export SDATE IRES
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/post
if [ $IRES -eq 27 ]; then
EXPN=rsm2rsm27_da
SDATE0=2022061000
elif [ $IRES -eq 9 ]; then
EXPN=rsm2msm9_da
SDATE0=2022061018
SDATE0=2022061812
SDATE0=2022082600
elif [ $IRES -eq 3 ]; then
EXPN=msm2msm3_da
SDATE0=2022061812
else
echo "Invalid resolution. Specify 9 or 3."
exit 2
fi
#
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
echo "TETYPE QADJ BP " $TETYPE $QADJ $BP
#exit
DATADIR=$TEMP/$SDATE
if [ ! -d $DATADIR ]; then
echo "No such directory : $DATADIR"
exit 4
fi
#
ut0=`date -j -f "%Y%m%d%H" +"%s" "${SDATE0}"`
echo $SDATE0 $ut0
ut=`date -j -f "%Y%m%d%H" +"%s" "${SDATE}"`
echo $SDATE $ut
set +e
dt=`expr ${ut} - ${ut0}`
set -e
icyc=`expr $dt / $BV_H / 3600 + 1`
echo $dt $icyc
#exit
EXEC1=spectra
EXEC2=profile
cd $SRCDIR
gmake ${EXEC1} || exit 5
gmake ${EXEC2} || exit 6
if [ $icyc -lt $DASTART ]; then
header=${HEAD}
else
header=${HEAD2}
fi
echo $header
OUTDIR=$DATADIR/${header}sprd
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
