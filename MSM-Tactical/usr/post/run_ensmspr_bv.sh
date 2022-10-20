#!/bin/sh
#
# calculate ensemble mean and spread for r_sig.fNN, r_sfc.fNN and r_flx.fNN
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 2 ]; then
  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(27 or 9) [wbp]"
  exit 1
fi
SDATE=${1}
IRES=${2}
#export SDATE IRES
EDATE=${EDATE:-$SDATE}
MEM=000
BP=${3}
CYCLE=${4:-1}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
while [ $SDATE -le $EDATE ];do
echo $SDATE
if [ $IRES -eq 27 ]; then
#DATADIR=/zdata/grmsm/work/gefs2rsm27_nomad/$SDATE
#EXPDIR=$MSMDIR/usr/exp/gefs2rsm27
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv/$SDATE
EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm9_bv/$SDATE
EXPDIR=$MSMDIR/usr/exp/rsm2msm9_bv
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_bv/$SDATE
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
cd $EXPDIR
. ./configure
echo $IGRD $JGRD
#exit
EXEC=ensmspr
cd $SRCDIR
gmake ${EXEC}
rm -rf $DATADIR/bvmean${BP}
mkdir -p $DATADIR/bvmean${BP}
rm -rf $DATADIR/bvsprd${BP}
mkdir -p $DATADIR/bvsprd${BP}
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
#for SIGN in p m;do
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_sig.f$fh fort.$nsig
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_sfc.f$fh fort.$nsfc
#ln -s $DATADIR/bv${SIGN}${MEM}${BP}_a5/r_flx.f$fh fort.$nflx
if [ $IRES -eq 27 ];then
ln -s $DATADIR/bv${MEM}${BP}_c${CYCLE}/r_sig.f$fh fort.$nsig
ln -s $DATADIR/bv${MEM}${BP}_c${CYCLE}/r_sfc.f$fh fort.$nsfc
ln -s $DATADIR/bv${MEM}${BP}_c${CYCLE}/r_flx.f$fh fort.$nflx
else
ln -s $DATADIR/bv${MEM}${BP}/r_sig.f$fh fort.$nsig
ln -s $DATADIR/bv${MEM}${BP}/r_sfc.f$fh fort.$nsfc
ln -s $DATADIR/bv${MEM}${BP}/r_flx.f$fh fort.$nflx
fi
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
mv r_sigm.f$fh $DATADIR/bvmean${BP}/r_sig.f$fh
mv r_sfcm.f$fh $DATADIR/bvmean${BP}/r_sfc.f$fh
mv r_flxm.f$fh $DATADIR/bvmean${BP}/r_flx.f$fh
mv r_sigs.f$fh $DATADIR/bvsprd${BP}/r_sig.f$fh
mv r_sfcs.f$fh $DATADIR/bvsprd${BP}/r_sfc.f$fh
mv r_flxs.f$fh $DATADIR/bvsprd${BP}/r_flx.f$fh
rm fort.*
cd $DATADIR/bvmean${BP}
$USHDIR/rpgb_post.sh $fh
cd -
fh=`echo $fh + $inc_h | bc`
done
ls -ltr $DATADIR/bvmean${BP} | tail -n 10
ls -ltr $DATADIR/bvsprd${BP} | tail -n 10
SDATE=`date -j -f "%Y%m%d%H" -v+6H +"%Y%m%d%H" "${SDATE}"`
done
echo END
