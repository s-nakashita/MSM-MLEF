#!/bin/sh
#
# add rescaled perturbations to r_sig.f00
#
set -ex
#if [ $# -lt 2 ]; then
#  echo "Usage : ./run_ensmspr.sh init(YYYYMMDDHH) res(9 or 3)"
#  exit 1
#fi
SDATE=${SDATE:-2022083000} #base
PDATE=2022061112 #prtb base
PMEM=${MEM:-001} #prtb member
IRES=27
CYCLE=${CYCLE:-3}
BV_H=${BV_H:-6}
BP=${BP} #with boundary perturbation
CYCLE=${1:-$CYCLE}
#SDATE=${1}
#IRES=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
BASEDIR=/zdata/grmsm/work/gefs2rsm27_nomad
DATADIR=/zdata/grmsm/work/rsm2rsm27_bv
EXPDIR=$MSMDIR/usr/exp/rsm2rsm27_bv
elif [ $IRES -eq 9 ]; then
BASEDIR=/zdata/grmsm/work/gfsp2rsm27_nomad
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn
#DATADIR=/zdata/grmsm/work/rsm2msm9_tparc
EXPDIR=$MSMDIR/usr/exp/rsm2msm9
elif [ $IRES -eq 3 ]; then
BASEDIR=/zdata/grmsm/work/rsm2msm9_jpn
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
SDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${SDATE}"` #a
#else #c
#fh=00 #c
fi
OUTDIR=$DATADIR/$SDATE/bv${PMEM}${BP}_c${CYCLE} #c
#OUTDIR=$DATADIR/$SDATE/bv${PMEM}${BP}_a${CYCLE}
rm -rf $OUTDIR
mkdir -p $OUTDIR
#OUTPDIR=$DATADIR/$SDATE/bvp${PMEM}${BP}_a${CYCLE}
#OUTMDIR=$DATADIR/$SDATE/bvm${PMEM}${BP}_a${CYCLE}
#rm -rf $OUTPDIR
#mkdir -p $OUTPDIR
#rm -rf $OUTMDIR
#mkdir -p $OUTMDIR
##copy unchanged file
###orography
cp $DATADIR/$SDATE/rmtn.parm $OUTDIR/
cp $DATADIR/$SDATE/rmtnoss $OUTDIR/
cp $DATADIR/$SDATE/rmtnslm $OUTDIR/
cp $DATADIR/$SDATE/rmtnvar $OUTDIR/
#cp $DATADIR/$SDATE/rmtn.parm $OUTPDIR/
#cp $DATADIR/$SDATE/rmtnoss $OUTPDIR/
#cp $DATADIR/$SDATE/rmtnslm $OUTPDIR/
#cp $DATADIR/$SDATE/rmtnvar $OUTPDIR/
#cp $DATADIR/$SDATE/rmtn.parm $OUTMDIR/
#cp $DATADIR/$SDATE/rmtnoss $OUTMDIR/
#cp $DATADIR/$SDATE/rmtnslm $OUTMDIR/
#cp $DATADIR/$SDATE/rmtnvar $OUTMDIR/
mkdir -p tmp
cd tmp
#. ${EXPDIR}/configure
#echo $IGRD $JGRD
rm -f fort.*
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
# base field
echo $SDATE
ln -s $DATADIR/$SDATE/r_sig.f00 fort.11 #a
ln -s $DATADIR/$SDATE/r_sfc.f00 fort.14 #a
#ln -s $DATADIR/$SDATE/r_sig.f$fh fort.11 #c
#ln -s $DATADIR/$SDATE/r_sfc.f$fh fort.14 #c
# perturbation base
if [ $CYCLE -eq 1 ]; then
if [ $GLOBAL = GFS ]; then #deterministic=lag forecast
echo $PDATE
ln -s $DATADIR/$PDATE/r_sig.f12 fort.12
PDATE=`date -j -f "%Y%m%d%H" -v-12H +"%Y%m%d%H" "${PDATE}"`
echo $PDATE
ln -s $DATADIR/$PDATE/r_sig.f24 fort.13
##ln -s $DATADIR/$SDATE/r_sigitdt fort.15 #c(dummy)
else
### ensemble
#ln -s $DATADIR/$SDATE/mean/r_sig.f00 fort.13
ln -s $DATADIR/$SDATE/$PMEM/r_sig.f00 fort.12
ln -s $DATADIR/$SDATE/r_sig.f00 fort.13
fi
else
PCYCLE=`expr $CYCLE - 1`
fh2=$BV_H
if [ $fh2 -lt 10 ]; then
fh2=0$fh2
fi
#ln -s $DATADIR/$SDATE/r_sig.f$fh fort.12 #c
#ln -s $DATADIR/$SDATE/bv${BV_H}h_c${PCYCLE}/r_sig.f$fh2 fort.13 #c
##ln -s $DATADIR/$SDATE/bv${BV_H}h_c${PCYCLE}/r_sig.f$fh fort.13 #c
#ln -s $DATADIR/$SDATE/bv${BV_H}h_c${PCYCLE}/r_sigitdt fort.15 #c
PDATE=`date -j -f "%Y%m%d%H" -v-${BV_H}H +"%Y%m%d%H" "${SDATE}"` #a
echo $PDATE #a
if [ do$BP = dowbp ];then
ln -s $DATADIR/$PDATE/$PMEM/r_sig.f$fh2 fort.12 #c
else
ln -s $DATADIR/$PDATE/r_sig.f$fh2 fort.12 #a
fi
ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_c$PCYCLE/r_sig.f$fh2 fort.13 #c
#ln -s $DATADIR/$PDATE/bv${PMEM}${BP}_a$PCYCLE/r_sig.f$fh2 fort.13 #a
#ln -s $DATADIR/$PDATE/bvm${PMEM}${BP}_a$PCYCLE/r_sig.f$fh2 fort.12 #a
#ln -s $DATADIR/$PDATE/bvp${PMEM}${BP}_a$PCYCLE/r_sig.f$fh2 fort.13 #a
fi
### set namelist
#if [ $CYCLE -lt 5 ];then
cat <<EOF >namelist
&namlst_prtb
 setnorm=T,
 teref=2.0d0,
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
