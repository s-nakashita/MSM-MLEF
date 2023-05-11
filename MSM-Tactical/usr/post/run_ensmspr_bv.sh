#!/bin/sh
#
# calculate ensemble mean and spread for r_sig.fNN, r_sfc.fNN and r_flx.fNN
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 2 ]; then
  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(27 or 9) [bv_h, tetype, qadj, wbp, edate]"
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
EDATE=${EDATE:-$SDATE}
MEMBER=10
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
#SRCDIR=${MSMDIR}/usr/post
SRCDIR=${MSMDIR}/dpac/build/post
SDATE0=2022082900
ut0=`date -j -f "%Y%m%d%H" +"%s" "${SDATE0}"`
echo $SDATE0 $ut0
while [ $SDATE -le $EDATE ];do
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
EXEC=ensmspr
cd $SRCDIR
gmake ${EXEC} || exit 5
if [ $BV_H -eq 6 ]; then
HEAD=bv${TETYPE}${TESIZE}
else
HEAD=bv${TETYPE}${TESIZE}${BV_H}h
fi
if [ do$QADJ = doyes ]; then
SUF=_qadj${SUF}
fi
if [ ! -z $BP ] && [ ! -z $SCLPOW ]; then
  SUF=${BP}${SCLBASE}p${SCLPOW}${SUF}
else
  SUF=${BP}${SCLBASE}${SUF}
fi
rm -rf $DATADIR/${HEAD}mean${SUF}
mkdir -p $DATADIR/${HEAD}mean${SUF}
rm -rf $DATADIR/${HEAD}sprd${SUF}
mkdir -p $DATADIR/${HEAD}sprd${SUF}
rm -rf $DATADIR/tmp
mkdir -p $DATADIR/tmp
cd $DATADIR/tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
inc_h=$PRTHOUR
if [ $IRES -eq 27 ] && [ $icyc -lt 5 ]; then
  end_hour=$BV_H
fi
rm -f r_sig.* r_sfc.* r_flx.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
nsig=11
nsfc=21
nflx=31
MEM=1
while [ $MEM -le $MEMBER ]; do
if [ $MEM -lt 10 ];then
MEM=00$MEM
else
MEM=0$MEM
fi
if [ ! -d $DATADIR/${HEAD}${MEM}${SUF} ]; then
echo 'no such directory '$DATADIR/${HEAD}${SUF}
exit 99
fi
#for SIGN in p m;do
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_sig.f$fh fort.$nsig
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_sfc.f$fh fort.$nsfc
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_flx.f$fh fort.$nflx
ln -s $DATADIR/${HEAD}${MEM}${SUF}/r_sig.f$fh r_sig.0$MEM
ln -s $DATADIR/${HEAD}${MEM}${SUF}/r_sfc.f$fh r_sfc.0$MEM
ln -s $DATADIR/${HEAD}${MEM}${SUF}/r_flx.f$fh r_flx.0$MEM
#ln -s $DATADIR/${HEAD}${MEM}${BP}${SUF}/r_sig.f$fh fort.$nsig
#ln -s $DATADIR/${HEAD}${MEM}${BP}${SUF}/r_sfc.f$fh fort.$nsfc
#ln -s $DATADIR/${HEAD}${MEM}${BP}${SUF}/r_flx.f$fh fort.$nflx
#nsig=`expr $nsig + 1`
#nsfc=`expr $nsfc + 1`
#nflx=`expr $nflx + 1`
#done
MEM=`expr $MEM + 1`
done
ln -s r_sigm.f$fh fort.51
ln -s r_sfcm.f$fh fort.52
ln -s r_flxm.f$fh fort.53
ln -s r_sigs.f$fh fort.54
ln -s r_sfcs.f$fh fort.55
ln -s r_flxs.f$fh fort.56
cat <<EOF > ensmspr.nml
&namlst_ensmspr
 nens=$MEMBER,
&end
EOF
./${EXEC} < ensmspr.nml || exit 10 #1>>${EXEC}.log 2>&1
mv r_sigm.f$fh $DATADIR/${HEAD}mean${SUF}/r_sig.f$fh
mv r_sfcm.f$fh $DATADIR/${HEAD}mean${SUF}/r_sfc.f$fh
mv r_flxm.f$fh $DATADIR/${HEAD}mean${SUF}/r_flx.f$fh
mv r_sigs.f$fh $DATADIR/${HEAD}sprd${SUF}/r_sig.f$fh
mv r_sfcs.f$fh $DATADIR/${HEAD}sprd${SUF}/r_sfc.f$fh
mv r_flxs.f$fh $DATADIR/${HEAD}sprd${SUF}/r_flx.f$fh
rm fort.* r_sig.* r_sfc.* r_flx.*
cd $DATADIR/${HEAD}mean${SUF}
$USHDIR/rpgb_post.sh $fh
cd -
fh=`echo $fh + $inc_h | bc`
done
ls -ltr $DATADIR/${HEAD}mean${SUF} | tail -n 10
ls -ltr $DATADIR/${HEAD}sprd${SUF} | tail -n 10
#SDATE=`date -j -f "%Y%m%d%H" -v+${BV_H}H +"%Y%m%d%H" "${SDATE}"`
SDATE=`date -j -f "%Y%m%d%H" -v+24H +"%Y%m%d%H" "${SDATE}"`
done
echo END
