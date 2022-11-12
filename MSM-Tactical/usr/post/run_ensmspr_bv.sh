#!/bin/sh
#
# calculate ensemble mean and spread for r_sig.fNN, r_sfc.fNN and r_flx.fNN
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 2 ]; then
  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(27 or 9) [edate, bv_h, wbp]"
  exit 1
fi
SDATE=${1}
IRES=${2}
#export SDATE IRES
EDATE=${3:-$SDATE}
MEM=000
BV_H=${4:-6}
BP=${5}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
SDATE0=2022083000
ut0=`date -j -f "%Y%m%d%H" +"%s" "${SDATE0}"`
echo $SDATE0 $ut0
while [ $SDATE -le $EDATE ];do
ut=`date -j -f "%Y%m%d%H" +"%s" "${SDATE}"`
echo $SDATE $ut
dt=`expr ${ut} - ${ut0}`
icyc=`expr $dt / $BV_H / 3600 + 1`
echo $dt $icyc
if [ $IRES -eq 27 ]; then
#DATADIR=/zdata/grmsm/work/gefs2rsm27_nomad/$SDATE
#EXPDIR=$MSMDIR/usr/exp/gefs2rsm27
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv/$SDATE
EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
SUF=_c${icyc}
elif [ $IRES -eq 9 ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm9_bv/$SDATE
EXPDIR=$MSMDIR/usr/exp/rsm2msm9_bv
SUF=
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_bv/$SDATE
EXPDIR=$MSMDIR/usr/exp/msm2msm3_bv
SUF=
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
cd $EXPDIR
. ./configure
echo $IGRD $JGRD
#exit
EXEC=ensmspr
cd $SRCDIR
gmake ${EXEC}
if [ $BV_H -eq 6 ]; then
header=bv
else
header=bv${BV_H}h
fi
header=${header}dry
rm -rf $DATADIR/${header}mean${BP}${SUF}
mkdir -p $DATADIR/${header}mean${BP}${SUF}
rm -rf $DATADIR/${header}sprd${BP}${SUF}
mkdir -p $DATADIR/${header}sprd${BP}${SUF}
rm -rf $DATADIR/tmp
mkdir -p $DATADIR/tmp
cd $DATADIR/tmp
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
inc_h=$PRTHOUR
rm -f fort.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
nsig=11
nsfc=21
nflx=31
MEM=1
while [ $MEM -le 10 ]; do
if [ $MEM -lt 10 ];then
MEM=00$MEM
else
MEM=0$MEM
fi
if [ ! -d $DATADIR/${header}${MEM}${BP}${SUF} ]; then
exit 99
fi
#for SIGN in p m;do
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_sig.f$fh fort.$nsig
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_sfc.f$fh fort.$nsfc
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_flx.f$fh fort.$nflx
ln -s $DATADIR/${header}${MEM}${BP}${SUF}/r_sig.f$fh fort.$nsig
ln -s $DATADIR/${header}${MEM}${BP}${SUF}/r_sfc.f$fh fort.$nsfc
ln -s $DATADIR/${header}${MEM}${BP}${SUF}/r_flx.f$fh fort.$nflx
nsig=`expr $nsig + 1`
nsfc=`expr $nsfc + 1`
nflx=`expr $nflx + 1`
#done
MEM=`expr $MEM + 1`
done
ln -s r_sigm.f$fh fort.51
ln -s r_sfcm.f$fh fort.52
ln -s r_flxm.f$fh fort.53
ln -s r_sigs.f$fh fort.54
ln -s r_sfcs.f$fh fort.55
ln -s r_flxs.f$fh fort.56
./${EXEC} #1>>${EXEC}.log 2>&1
mv r_sigm.f$fh $DATADIR/${header}mean${BP}${SUF}/r_sig.f$fh
mv r_sfcm.f$fh $DATADIR/${header}mean${BP}${SUF}/r_sfc.f$fh
mv r_flxm.f$fh $DATADIR/${header}mean${BP}${SUF}/r_flx.f$fh
mv r_sigs.f$fh $DATADIR/${header}sprd${BP}${SUF}/r_sig.f$fh
mv r_sfcs.f$fh $DATADIR/${header}sprd${BP}${SUF}/r_sfc.f$fh
mv r_flxs.f$fh $DATADIR/${header}sprd${BP}${SUF}/r_flx.f$fh
rm fort.*
cd $DATADIR/${header}mean${BP}${SUF}
$USHDIR/rpgb_post.sh $fh
cd -
fh=`echo $fh + $inc_h | bc`
done
ls -ltr $DATADIR/${header}mean${BP}${SUF} | tail -n 10
ls -ltr $DATADIR/${header}sprd${BP}${SUF} | tail -n 10
#SDATE=`date -j -f "%Y%m%d%H" -v+${BV_H}H +"%Y%m%d%H" "${SDATE}"`
SDATE=`date -j -f "%Y%m%d%H" -v+24H +"%Y%m%d%H" "${SDATE}"`
done
echo END
