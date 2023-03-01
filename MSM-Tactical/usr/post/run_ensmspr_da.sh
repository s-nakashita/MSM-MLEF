#!/bin/sh
#
# calculate ensemble mean and spread for r_sig.fNN, r_sfc.fNN and r_flx.fNN
#
#SDATE=2022060812
#IRES=9
if [ $# -lt 2 ]; then
  echo "Usage : ./run_ensmspr_da.sh res(27 or 9) init(YYYYMMDDHH) [edate]"
  exit 1
fi
IRES=${1}
SDATE=${2}
#export SDATE IRES
EDATE=${3:-$SDATE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
#SRCDIR=${MSMDIR}/usr/post
SRCDIR=${MSMDIR}/dpac/build/post
if [ $IRES -eq 27 ]; then
EXPN=rsm2rsm27_da
SDATE0=2022061000
elif [ $IRES -eq 9 ]; then
EXPN=rsm2msm9_da
SDATE0=2022061018
SDATE0=2022061812
elif [ $IRES -eq 3 ]; then
EXPN=msm2msm3_da
SDATE0=2022061812
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
echo "TETYPE QADJ BP " $TETYPE $QADJ $BP
#exit
while [ $SDATE -le $EDATE ];do
ut=`date -j -f "%Y%m%d%H" +"%s" "${SDATE}"`
echo $SDATE $ut
dt=`expr ${ut} - ${ut0}`
icyc=`expr $dt / $BV_H / 3600 + 1`
echo $dt $icyc
DATADIR=/zdata/grmsm/work/$EXPN/$SDATE
echo $DATADIR
if [ ! -d $DATADIR ]; then
echo "No such directory : $DATADIR"
exit 4
fi
EXEC=ensmspr
cd $SRCDIR
gmake ${EXEC}
if [ $icyc -lt $DASTART ]; then
header=${HEAD}
meandir=${header}mean
sprddir=${header}sprd
#meandir=${header}m${MEMBER}mean
#sprddir=${header}m${MEMBER}sprd
else
header=${HEAD2}
meandir=${header}mean
sprddir=${header}sprd
fi
echo $header
mkdir -p $DATADIR/${meandir}
mkdir -p $DATADIR/${sprddir}
rm -rf $DATADIR/tmp
mkdir -p $DATADIR/tmp
cd $DATADIR/tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
#if [ ! -z $EXTEND ]; then
#HZ=`echo $SDATE | cut -c9-10`
#if [ $HZ -ne $EXTEND ];then
#end_hour=$INCCYCLE
#fi
#fi
inc_h=$PRTHOUR
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
rm -f r_sig.* r_sfc.* r_flx.*
MEM=1
while [ $MEM -le $MEMBER ]; do
if [ $MEM -lt 10 ];then
MEM=00$MEM
else
MEM=0$MEM
fi
if [ ! -d $DATADIR/${header}${MEM} ]; then
exit 99
fi
ln -s $DATADIR/${header}${MEM}/r_sig.f$fh r_sig.0$MEM
ln -s $DATADIR/${header}${MEM}/r_sfc.f$fh r_sfc.0$MEM
ln -s $DATADIR/${header}${MEM}/r_flx.f$fh r_flx.0$MEM
MEM=`expr $MEM + 1`
done
ln -s r_sigm.f$fh fort.51
ln -s r_sfcm.f$fh fort.52
ln -s r_flxm.f$fh fort.53
ln -s r_sigs.f$fh fort.54
ln -s r_sfcs.f$fh fort.55
ln -s r_flxs.f$fh fort.56
cat <<EOF >ensmspr.nml
&namlst_ensmspr
 nens=$MEMBER,
&end
EOF
./${EXEC} < ensmspr.nml #1>>${EXEC}.log 2>&1
mv r_sigm.f$fh $DATADIR/${meandir}/r_sig.f$fh
mv r_sfcm.f$fh $DATADIR/${meandir}/r_sfc.f$fh
mv r_flxm.f$fh $DATADIR/${meandir}/r_flx.f$fh
mv r_sigs.f$fh $DATADIR/${sprddir}/r_sig.f$fh
mv r_sfcs.f$fh $DATADIR/${sprddir}/r_sfc.f$fh
mv r_flxs.f$fh $DATADIR/${sprddir}/r_flx.f$fh
rm fort.*
cd $DATADIR/${meandir}
$USHDIR/rpgb_post.sh $fh
cd -
fh=`echo $fh + $inc_h | bc`
done
ls -ltr $DATADIR/${meandir} | tail -n 10
ls -ltr $DATADIR/${sprddir} | tail -n 10
SDATE=`date -j -f "%Y%m%d%H" -v+${BV_H}H +"%Y%m%d%H" "${SDATE}"`
done
echo END
