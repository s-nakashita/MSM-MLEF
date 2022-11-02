#!/bin/sh
#
# calculate moist total energy for forecast difference
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
IDATE=${IDATE:-2022083000}
EDATE=${EDATE:-$IDATE}
IRES=${IRES:-27}
CYCLE=${CYCLE:-1}
BV_H=${BV_H:-6}
MEM=${MEM:-003}
BV=${BV:-yes}
BP=${BP} #with boundary perturbation
MEM=${1:-$MEM}
CYCLE=${2:-$CYCLE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
#DATADIR=/zdata/grmsm/work/gfsp2rsm27_nomad
#EXPDIR=$MSMDIR/usr/exp/gfsp2rsm27
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv
EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
#DATADIR=/zdata/grmsm/work/rsm2msm9_jpn
#DATADIR=/zdata/grmsm/work/rsm2msm9_tparc
#EXPDIR=$MSMDIR/usr/exp/rsm2msm9
DATADIR=/zdata/grmsm/work/rsm2msm9_bv
EXPDIR=$MSMDIR/usr/exp/rsm2msm9_bv
elif [ $IRES -eq 3 ]; then
DATADIR=/zdata/grmsm/work/msm2msm3_bv
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
EXEC=calcte
cd $SRCDIR
gmake ${EXEC}
#mkdir -p $DATADIR/stat
cd ${EXPDIR}
. ./configure
#echo $IGRD $JGRD
cd -
mkdir -p tmp
cd tmp
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
if [ $CYCLE -gt 1 ];then
fhs=`expr $BV_H \* \( $CYCLE - 1 \)`
else
fhs=0
fi
dte=$ENDHOUR
inch=$PRTHOUR
SPINUP=`expr 24 / $BV_H + 1`
if [ $IRES -eq 27 ] && [ $CYCLE -lt $SPINUP ]; then
#dte=`expr 48 - $fhs` #c
dte=$BV_H #a
fi
#for dt in 12 24 36 48;do
for dt in $(seq 0 $inch $dte);do
#dt=0 #hour
CDATE=$IDATE
while [ $CDATE -le $EDATE ];do
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
#ln -s $DATADIR/$CDATE/r_sig.f$fh fort.$nsig #c
PDATE=`date -j -f "%Y%m%d%H" -v+${fhs}H +"%Y%m%d%H" "${CDATE}"` #a
ln -s $DATADIR/$PDATE/r_sig.f$fh fort.$nsig #a
# previous forecast
nsig=`expr $nsig + 1`
#PDATE=`date -j -f "%Y%m%d%H" -v-${dt}H +"%Y%m%d%H" "${CDATE}"`
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
#ln -s $DATADIR/$CDATE/bv${BV_H}h_c${CYCLE}/r_sig.f$fh fort.$nsig #c
#if [ $CYCLE -eq 1 ]; then
#ln -s $DATADIR/$PDATE/bva${CYCLE}/r_sig.f$fh fort.$nsig #a
#else
#ln -s $DATADIR/$PDATE/bv${BV_H}h_a${CYCLE}/r_sig.f$fh fort.$nsig #a
#fi
if [ $BV = yes ];then
if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
WDIR=bvhalf${BV_H}h${MEM}${BP}
else
WDIR=bvhalf${MEM}${BP}
fi
if [ $IRES -eq 27 ];then
ln -s $DATADIR/$PDATE/${WDIR}_c${CYCLE}/r_sig.f$fh fort.$nsig #c
#ln -s $DATADIR/$PDATE/bv${MEM}${BP}_a${CYCLE}/r_sig.f$fh fort.$nsig #a
else
ln -s $DATADIR/$PDATE/${WDIR}/r_sig.f$fh fort.$nsig #c
fi
else
ln -s $DATADIR/$PDATE/${MEM}/r_sig.f$fh fort.$nsig
fi
if [ $IRES -eq 27 ];then
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
&END
EOF
else
cat <<EOF >NAMELIST
&NAMLST_PRTB
 lprtb=T,
 lonw=,
 lone=,
 lats=,
 latn=,
&END
EOF
fi
./${EXEC} < NAMELIST #1>>${EXEC}.log 2>&1
cat te.dat
#mv te.dat $DATADIR/stat/te${dt}h${CDATE}.dat
#mv te.dat $DATADIR/$CDATE/bv${BV_H}h_c${CYCLE}/te${dt}h.dat #c
#if [ $CYCLE -eq 1 ]; then
#mv te.dat $DATADIR/$PDATE/bva${CYCLE}/te${dt}h.dat #a
#else
#mv te.dat $DATADIR/$PDATE/bv${BV_H}h_a${CYCLE}/te${dt}h.dat #a
#fi
if [ $BV = yes ];then
if [ $IRES -eq 27 ];then
mv te.dat $DATADIR/$PDATE/${WDIR}_c${CYCLE}/te${dt}h.dat #c
#mv te.dat $DATADIR/$PDATE/bv${MEM}${BP}_a${CYCLE}/te${dt}h.dat #a
else
mv te.dat $DATADIR/$PDATE/${WDIR}/te${dt}h.dat #c
fi
else
mv te.dat $DATADIR/$PDATE/${MEM}/te${dt}h.dat
fi
rm fort.*
CDATE=`date -j -f  "%Y%m%d%H" -v+12H +"%Y%m%d%H" "${CDATE}"`
done
done
echo END
