#!/bin/sh
#
# add rescaled perturbations to base fields' r_sig.f00
#
set -ex
IDATE=${SDATE:-2022083000} #base
IRES=${IRES:-27}
BV_H=${INCCYCLE:-6}
TETYPE=${TETYPE}
SCL=${SCL}
QADJ=${QADJ:-no} #super saturation and dry adjustment
BP=${BP} #with boundary perturbation
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
DATADIR=${RUNDIR:-/zdata/grmsm/work/dpac/rsm27/$SDATE}
BASE0=${BASEDIR0:-/zdata/grmsm/work/gfsp2rsm27_nomad}
BASE1=${BASEDIR1:-/zdata/grmsm/work/gfsp2rsm27_rda}
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
if [ ! -d $BASE0 ]; then
  echo "No such directory : $BASE0"
  exit 3
fi
if [ ! -d $BASE1 ]; then
  echo "No such directory : $BASE1"
  exit 3
fi

EXEC=addprtbbase
cd $SRCDIR
gmake ${EXEC}
BASEDIR=${BASE0}/${SDATE}
if [ ! -d $BASEDIR ];then
  BASEDIR=${BASE1}/${SDATE}
  if [ ! -d $BASEDIR ]; then
    echo 'base data not exist'
    exit 99
  fi
fi
echo $BASEDIR
#restart check
MEM=$MEMBER
PMEM=`printf '%0.3d' $MEM` #prtb member
OUTDIR=$BASEDIR/${PMEM}
if [ -d $OUTDIR ]; then
  echo 'base perturbation already done.'
  exit
fi

rm -rf tmp
mkdir -p tmp
cd tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
cp $DATADIR/pdatebase.txt .
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
while [ $MEM -le $MEMBER ]; do
echo 'input unit ' $nisig1 $nisig2
echo 'output unit ' $nosig $nosfc
irow=$MEM
PDATE1=`cat pdatebase.txt | awk '{if(NR == '$irow') {print $1}}'`
irow=`expr $irow + $MEMBER`
PDATE2=`cat pdatebase.txt | awk '{if(NR == '$irow') {print $1}}'`
echo $PDATE1 $PDATE2
PMEM=`printf '%0.3d' $MEM` #prtb member
OUTDIR=$BASEDIR/${PMEM}
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
