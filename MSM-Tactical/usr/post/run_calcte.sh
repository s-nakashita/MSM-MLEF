#!/bin/sh
#
# calculate moist total energy for forecast difference
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
SDATE=${SDATE:-2022083000}
EDATE=${EDATE:-$SDATE}
IRES=27
CYCLE=${CYCLE:-1}
BV=6
MEM=${MEM:-003}
BP=${BP} #with boundary perturbation
MEM=${1:-$MEM}
CYCLE=${2:-$CYCLE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
#DATADIR=/zdata/grmsm/work/gfsp2rsm27_nomad
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv
EXPDIR=$MSMDIR/usr/exp/gfsp2rsm27
elif [ $IRES -eq 9 ]; then
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn
#DATADIR=/zdata/grmsm/work/rsm2msm9_tparc
EXPDIR=$MSMDIR/usr/exp/rsm2msm9
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_jpn
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
EXEC=calcte
cd $SRCDIR
gmake ${EXEC}
#mkdir -p $DATADIR/stat
mkdir -p tmp
cd tmp
#. ${EXPDIR}/configure
#echo $IGRD $JGRD
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
if [ $CYCLE -gt 1 ];then
fhs=`expr $BV \* \( $CYCLE - 1 \)`
else
fhs=0
fi
#dte=`expr 48 - $fhs` #c
dte=$BV #a
if [ $CYCLE -ge 5 ]; then
dte=24
fi
#for dt in 12 24 36 48;do
for dt in $(seq 0 1 $dte);do
#dt=0 #hour
IDATE=$SDATE
while [ $IDATE -le $EDATE ];do
# latest forecast
nsig=11
if [ $fhs -gt 0 ]; then
fh=`expr $dt + $fhs`
else
fh=$dt
fi
fh=$dt #a
if [ $fh -lt 10 ]; then
fh=0$fh
fi
#ln -s $DATADIR/$IDATE/r_sig.f$fh fort.$nsig #c
PDATE=`date -j -f "%Y%m%d%H" -v+${fhs}H +"%Y%m%d%H" "${IDATE}"` #a
ln -s $DATADIR/$PDATE/r_sig.f$fh fort.$nsig #a
# previous forecast
nsig=`expr $nsig + 1`
#PDATE=`date -j -f "%Y%m%d%H" -v-${dt}H +"%Y%m%d%H" "${IDATE}"`
#echo $PDATE
#fh=`expr $fh + $dt`
#if [ $fh -lt 10 ]; then
#fh=0$fh
#fi
#ln -s $DATADIR/$PDATE/r_sig.f$fh fort.$nsig
fh=$dt
if [ $fh -lt 10 ]; then
fh=0$fh
fi
#ln -s $DATADIR/$IDATE/bv${BV}h_c${CYCLE}/r_sig.f$fh fort.$nsig #c
#if [ $CYCLE -eq 1 ]; then
#ln -s $DATADIR/$PDATE/bva${CYCLE}/r_sig.f$fh fort.$nsig #a
#else
#ln -s $DATADIR/$PDATE/bv${BV}h_a${CYCLE}/r_sig.f$fh fort.$nsig #a
#fi
ln -s $DATADIR/$PDATE/bv${MEM}${BP}_c${CYCLE}/r_sig.f$fh fort.$nsig #a
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 lonw=120.0,
 lone=164.0,
 lats=5.0,
 latn=39.0,
&END
EOF
./${EXEC} < NAMELIST #1>>${EXEC}.log 2>&1
cat te.dat
#mv te.dat $DATADIR/stat/te${dt}h${IDATE}.dat
#mv te.dat $DATADIR/$IDATE/bv${BV}h_c${CYCLE}/te${dt}h.dat #c
#if [ $CYCLE -eq 1 ]; then
#mv te.dat $DATADIR/$PDATE/bva${CYCLE}/te${dt}h.dat #a
#else
#mv te.dat $DATADIR/$PDATE/bv${BV}h_a${CYCLE}/te${dt}h.dat #a
#fi
mv te.dat $DATADIR/$PDATE/bv${MEM}${BP}_c${CYCLE}/te${dt}h.dat #a
rm fort.*
IDATE=`date -j -f  "%Y%m%d%H" -v+12H +"%Y%m%d%H" "${IDATE}"`
done
done
echo END
