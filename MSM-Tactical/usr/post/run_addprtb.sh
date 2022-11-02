#!/bin/sh
#
# add rescaled perturbations to r_sig.f00
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
IDATE=${IDATE:-2022083000} #base
PDATE=${PDATE:-2022061112} #prtb base
PMEM=${MEM:-001} #prtb member
IRES=27
CYCLE=${CYCLE:-3}
BV_H=${BV_H:-6}
BP=${BP} #with boundary perturbation
CYCLE=${1:-$CYCLE}
#IDATE=${1}
#IRES=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
BASE=/zdata/grmsm/work/gfsp2rsm27_nomad
else
BASE=/zdata/grmsm/work/gefs2rsm27_nomad
fi
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv
EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
BASE=/zdata/grmsm/work/gfsp2rsm27_nomad
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn
#DATADIR=/zdata/grmsm/work/rsm2msm9_tparc
EXPDIR=$MSMDIR/usr/exp/rsm2msm9
elif [ $IRES -eq 3 ]; then
BASE=/zdata/grmsm/work/rsm2msm9_jpn
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
EXEC=addprtb
cd $SRCDIR
gmake ${EXEC}
if [ $CYCLE -gt 1 ]; then
fh=`expr $BV_H \* \( $CYCLE - 1 \)`
if [ $fh -lt 10 ]; then
fh=0$fh
fi
echo "forecast hour=${fh}"
IDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${IDATE}"` #a
#else #c
#fh=00 #c
fi
WDIR=bvhalf${PMEM}${BP}
if [ $CYCLE -gt 1 ] && [ $BV_H -gt 6 ];then
WDIR=bvhalf${BV_H}h${PMEM}${BP}
fi
OUTDIR=$DATADIR/$IDATE/${WDIR}_c${CYCLE}
#OUTDIR=$DATADIR/$IDATE/${WDIR}_a${CYCLE}
rm -rf $OUTDIR
mkdir -p $OUTDIR
#OUTPDIR=$DATADIR/$IDATE/bvp${PMEM}${BP}_a${CYCLE}
#OUTMDIR=$DATADIR/$IDATE/bvm${PMEM}${BP}_a${CYCLE}
#rm -rf $OUTPDIR
#mkdir -p $OUTPDIR
#rm -rf $OUTMDIR
#mkdir -p $OUTMDIR
##copy orography
#for OUTDIR in $OUTPDIR $OUTMDIR;do
cp $DATADIR/$IDATE/rmtn.parm $OUTDIR/
cp $DATADIR/$IDATE/rmtnoss $OUTDIR/
cp $DATADIR/$IDATE/rmtnslm $OUTDIR/
cp $DATADIR/$IDATE/rmtnvar $OUTDIR/
#done
rm -rf tmp
mkdir -p tmp
cd tmp
#. ${EXPDIR}/configure
#echo $IGRD $JGRD
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
# base field
echo $IDATE
ln -s $DATADIR/$IDATE/r_sig.f00 fort.11 #analysis
ln -s $DATADIR/$IDATE/r_sfc.f00 fort.14 #analysis
#ln -s $DATADIR/$IDATE/r_sig.f$fh fort.11 #guess
#ln -s $DATADIR/$IDATE/r_sfc.f$fh fort.14 #guess
# perturbation base
if [ $CYCLE -eq 1 ]; then
if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
cd ${EXPDIR}
. ./configure
export RUNFCST=no
export RUNRINP2=yes
export POSTTYPE=
cd -
echo $IGRD $JGRD
mkdir -p $PDATE
cd $PDATE
export SDATE=$PDATE
export BASEDIR=${BASE}/${SDATE}
export RUNDIR=`pwd`
echo $BASEDIR $RUNDIR
export ENDHOUR=12
export INCHOUR=12
$JSHDIR/rsm_fcst.sh > rint.log 2>&1 || exit 10
cd ..
ln -s $RUNDIR/r_sig.f12 fort.12
PDATE=`date -j -f "%Y%m%d%H" -v-12H +"%Y%m%d%H" "${PDATE}"`
mkdir -p $PDATE
cd $PDATE
export SDATE=$PDATE
export BASEDIR=${BASE}/${SDATE}
export RUNDIR=`pwd`
echo $BASEDIR $RUNDIR
export ENDHOUR=24
export INCHOUR=24
$JSHDIR/rsm_fcst.sh > rint.log 2>&1 || exit 10
cd ..
ln -s $RUNDIR/r_sig.f24 fort.13
##ln -s $DATADIR/$IDATE/r_sigitdt fort.15 #c(dummy)
else #ensemble
#ln -s $DATADIR/$IDATE/mean/r_sig.f00 fort.13
ln -s $DATADIR/$IDATE/$PMEM/r_sig.f00 fort.12
ln -s $DATADIR/$IDATE/r_sig.f00 fort.13
fi
else
PCYCLE=`expr $CYCLE - 1`
fh2=$BV_H
if [ $fh2 -lt 10 ]; then
fh2=0$fh2
fi
#ln -s $DATADIR/$IDATE/r_sig.f$fh fort.12 #c
#ln -s $DATADIR/$IDATE/bv${BV_H}h_c${PCYCLE}/r_sig.f$fh2 fort.13 #c
##ln -s $DATADIR/$IDATE/bv${BV_H}h_c${PCYCLE}/r_sig.f$fh fort.13 #c
#ln -s $DATADIR/$IDATE/bv${BV_H}h_c${PCYCLE}/r_sigitdt fort.15 #c
PDATE=`date -j -f "%Y%m%d%H" -v-${BV_H}H +"%Y%m%d%H" "${IDATE}"` #a
echo $PDATE #a
if [ do$BP = dowbp ];then
ln -s $DATADIR/$PDATE/$PMEM/r_sig.f$fh2 fort.12 #c
else
ln -s $DATADIR/$PDATE/r_sig.f$fh2 fort.12 #a
fi
if [ $PCYCLE -eq 1 ]; then
ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
else
ln -s $DATADIR/$PDATE/${WDIR}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
#ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_a$PCYCLE/r_sig.f$fh2 fort.13 #a
#ln -s $DATADIR/$PDATE/bvm${PMEM}${BP}_a$PCYCLE/r_sig.f$fh2 fort.12 #a
#ln -s $DATADIR/$PDATE/bvp${PMEM}${BP}_a$PCYCLE/r_sig.f$fh2 fort.13 #a
fi
fi
### set namelist
#if [ $CYCLE -lt 5 ];then
cat <<EOF >namelist
&namlst_prtb
 setnorm=T,
 teref=1.0d0,
 lonw=110.0,
 lone=153.0,
 lats=15.0,
 latn=47.0,
&end
EOF
#else
##### set rescaling magnitude from ensemble spread statistics
#cat <<EOF >namelist
#&namlst_prtb
# setnorm=T,
# teref=5.8d0,
# lonw=120.0,
# lone=164.0,
# lats=5.0,
# latn=39.0,
#&end
#EOF
#fi
./${EXEC} < namelist #1>>${EXEC}.log 2>&1
fh=00 #a
mv fort.51 $OUTDIR/r_sig.f$fh
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigi
#mv fort.51 $OUTPDIR/r_sig.f$fh
#cp $OUTPDIR/r_sig.f$fh $OUTPDIR/r_sigi
#mv fort.52 $OUTMDIR/r_sig.f$fh
#cp $OUTMDIR/r_sig.f$fh $OUTMDIR/r_sigi
#if [ $CYCLE -eq 1 ]; then #c
cp $OUTDIR/r_sig.f$fh $OUTDIR/r_sigitdt
#cp $OUTPDIR/r_sig.f$fh $OUTPDIR/r_sigitdt
#cp $OUTMDIR/r_sig.f$fh $OUTMDIR/r_sigitdt
#else #c
#mv fort.54 $OUTDIR/r_sigitdt #c
#fi #c
mv fort.53 $OUTDIR/r_sfc.f$fh
cp $OUTDIR/r_sfc.f$fh $OUTDIR/r_sfci
ls -l $OUTDIR
#cp fort.53 $OUTPDIR/r_sfc.f$fh
#mv fort.53 $OUTMDIR/r_sfc.f$fh
#cp $OUTPDIR/r_sfc.f$fh $OUTPDIR/r_sfci
#cp $OUTMDIR/r_sfc.f$fh $OUTMDIR/r_sfci
#ls -l $OUTPDIR
#ls -l $OUTMDIR
rm fort.*
echo END
