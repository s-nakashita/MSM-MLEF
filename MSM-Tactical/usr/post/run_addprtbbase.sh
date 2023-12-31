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
IRES=27
CYCLE=${CYCLE:-3}
BV_H=${BV_H:-6}
TETYPE=${TETYPE}
SCL=${SCL}
QADJ=${QADJ:-no} #super saturation and dry adjustment
BP=${BP} #with boundary perturbation
SCLBASE=${SCLBASE}
CYCLE=${1:-$CYCLE}
#IDATE=${1}
#IRES=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
  BASE0=/zdata/grmsm/work/gfsp2rsm27_nomad
  BASE1=/zdata/grmsm/work/gfsp2rsm27_rda
  DATADIR=/zdata/grmsm/work/rsm2rsm27_bvgfs
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
cd $EXPDIR
. ./configure

EXEC=addprtbbase
cd $SRCDIR
gmake ${EXEC}
if [ $CYCLE -gt 1 ]; then
  fh=`expr $BV_H \* \( $CYCLE - 1 \)`
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  echo "forecast hour=${fh}"
  IDATE=`date -j -f "%Y%m%d%H" -v+${fh}H +"%Y%m%d%H" "${IDATE}"` #a
fi

BASEDIR=${BASE0}/${IDATE}
if [ ! -d $BASEDIR ];then
  BASEDIR=${BASE1}/${IDATE}
  if [ ! -d $BASEDIR ]; then
    echo 'base data not exist'
    exit 99
  fi
fi
# restart check
PMEM=`printf '%0.3d' $MEMBER`
if [ -d ${BASEDIR}/${SCLBASE}${PMEM} ];then
  echo "Base perturbation already done"
  exit 1
fi
echo $BASEDIR
rm -rf $BASEDIR/tmp
mkdir -p $BASEDIR/tmp
cd $BASEDIR/tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
NSAMPLE=`expr $MEMBER \* 2`
$PYENV $UTLDIR/random_sample.py ${EXPDIR}/gfs_sampledate_base.txt $NSAMPLE > pdatebase.txt
cp pdatebase.txt $DATADIR/$IDATE
h=0
while [ $h -le $ENDHOUR ]; do
fh=`printf '%0.2d' $h`
rm -f fort.*
# base field
ln -s $BASEDIR/r_sig.f$fh fort.11 #control
ln -s $BASEDIR/r_sfc.f$fh fort.12 #control
# prtb field
nisig1=13
nisig2=14
nosig=51
nosfc=52
MEM=1
irow=1
while [ $MEM -le $MEMBER ]; do
echo 'input unit ' $nisig1 $nisig2
echo 'output unit ' $nosig $nosfc
PDATE1=`cat pdatebase.txt | awk '{if(NR == '$irow') {print $1}}'`
irow=`expr $irow + 1`
PDATE2=`cat pdatebase.txt | awk '{if(NR == '$irow') {print $1}}'`
echo $PDATE1 $PDATE2
PMEM=`printf '%0.3d' $MEM` #prtb member
OUTDIR=$BASEDIR/${SCLBASE}${PMEM}
if [ $h -eq 0 ];then
rm -rf $OUTDIR
mkdir -p $OUTDIR
##copy orography
cp $BASEDIR/rmtn.parm $OUTDIR/
cp $BASEDIR/rmtnoss $OUTDIR/
cp $BASEDIR/rmtnslm $OUTDIR/
cp $BASEDIR/rmtnvar $OUTDIR/
fi
ln -s $OUTDIR/r_sig.f$fh fort.$nosig
ln -s $OUTDIR/r_sfc.f$fh fort.$nosfc
#
PRTBDIR1=${BASE0}/${PDATE1}
if [ ! -d $PRTBDIR1 ];then
  PRTBDIR1=${BASE1}/${PDATE1}
  if [ ! -d $PRTBDIR1 ];then
    echo 'not exist '$PRTBDIR1
    exit 99
  fi
fi
echo $PRTBDIR1
ln -s $PRTBDIR1/r_sig.f$fh fort.$nisig1
#
PRTBDIR2=${BASE0}/${PDATE2}
if [ ! -d $PRTBDIR2 ];then
  PRTBDIR2=${BASE1}/${PDATE2}
  if [ ! -d $PRTBDIR2 ];then
    echo 'not exist '$PRTBDIR2
    exit 99
  fi
fi
echo $PRTBDIR2
ln -s $PRTBDIR2/r_sig.f$fh fort.$nisig2
#
nisig1=`expr $nisig1 + 2`
nisig2=`expr $nisig2 + 2`
nosig=`expr $nosig + 2`
nosfc=`expr $nosfc + 2`
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
### set namelist
if [ do$QADJ = doyes ];then
  adjust_q=T
fi
if [ do$SCLBASE != do ];then
  alpha=${SCLBASE}.0d-1
else
  alpha=3.0d-1
fi
cat <<EOF >namelist
&namlst_prtb
 alpha=${alpha},
 member=${MEMBER},
 adjust_q=${adjust_q},
&end
EOF
./${EXEC} < namelist 2>/dev/null
h=`expr $h + $INCBASE`
done #while h -le ENDHOUR
echo END
