#!/bin/sh
#
# calculate ensemble mean and spread for r_sig.fNN, r_sfc.fNN and r_flx.fNN
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 2 ]; then
  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
  exit 1
fi
SDATE=${1}
IRES=${2}
export SDATE IRES
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 9 ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
DATADIR=/zdata/grmsm/work/rsm2msm9_ens/$SDATE
EXPDIR=$MSMDIR/usr/exp/rsm2msm9_ens
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_jpn/$SDATE
EXPDIR=$MSMDIR/usr/exp/msm2msm3
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
. ${EXPDIR}/configure
echo $IGRD $JGRD
echo $USHDIR
echo $UTLDIR
EXEC=ensmspr
cd $SRCDIR
gmake ${EXEC}
mkdir -p $DATADIR/mean
mkdir -p $DATADIR/sprd
mkdir -p $DATADIR/tmp
cd $DATADIR/tmp
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
#end_hour=0
inc_h=$PRTHOUR
rm -f fort.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
#if [ ! -f $DATADIR/mean/r_sig.f$fh ] || [ ! -f $DATADIR/sprd/r_sig.f$fh ];then
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
ln -s $DATADIR/$MEM/r_sig.f$fh fort.$nsig
ln -s $DATADIR/$MEM/r_sfc.f$fh fort.$nsfc
ln -s $DATADIR/$MEM/r_flx.f$fh fort.$nflx
nsig=`expr $nsig + 1`
nsfc=`expr $nsfc + 1`
nflx=`expr $nflx + 1`
MEM=`expr $MEM + 1`
done
ln -s r_sigm.f$fh fort.51
ln -s r_sfcm.f$fh fort.52
ln -s r_flxm.f$fh fort.53
ln -s r_sigs.f$fh fort.54
ln -s r_sfcs.f$fh fort.55
ln -s r_flxs.f$fh fort.56
./${EXEC} #1>>${EXEC}.log 2>&1
mv r_sigm.f$fh $DATADIR/mean/r_sig.f$fh
mv r_sfcm.f$fh $DATADIR/mean/r_sfc.f$fh
mv r_flxm.f$fh $DATADIR/mean/r_flx.f$fh
mv r_sigs.f$fh $DATADIR/sprd/r_sig.f$fh
mv r_sfcs.f$fh $DATADIR/sprd/r_sfc.f$fh
mv r_flxs.f$fh $DATADIR/sprd/r_flx.f$fh
rm fort.*
#fi
#
# r_pgb
#
cd $DATADIR/mean
$USHDIR/rpgb_post.sh $fh || exit 14
#cd $DATADIR/sprd
#$USHDIR/rpgb_post.sh $fh || exit 15
cd $DATADIR/tmp
fh=`echo $fh + $inc_h | bc`
done
ls -ltr $DATADIR/mean | tail -n 10
ls -ltr $DATADIR/sprd | tail -n 10
echo END
