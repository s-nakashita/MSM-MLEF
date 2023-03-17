#!/bin/sh
#
# add rescaled perturbations to base fields of r_sig.f00
#
set -ex
TETYPE=${TETYPE}
QADJ=${QADJ:-no} #super saturation and dry adjustment
PSUB=${PSUB:-no} #subtract perturbation
BP=${BP} #with boundary perturbation
SCLBASE=${SCLBASE}
SCLPOW=${SCLPOW}
head=${HEAD:-bv$TETYPE}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/pre
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

rm -rf tmp
mkdir -p tmp
cd tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
cp $DATADIR/pdatebase.txt .
h=0
while [ $h -le $ENDHOUR ]; do
fh=`printf '%0.2d' $h`
rm -f *.sig.grd *.sfc.grd
# base field
if [ -s $DATADIR/rb_sigf$fh ]; then #OSSE, rprepbase already done
ln -s $DATADIR/rb_sigf$fh rb.0000.sig.grd
ln -s $DATADIR/rb_sfcf$fh rb.0000.sfc.grd
else
ln -s $BASEDIR/r_sig.f$fh $DATADIR/rb_sigf$fh #control
ln -s $BASEDIR/r_sfc.f$fh $DATADIR/rb_sfcf$fh #control
ln -s $BASEDIR/r_sig.f$fh rb.0000.sig.grd #control
ln -s $BASEDIR/r_sfc.f$fh rb.0000.sfc.grd #control
fi
# prtb field
if [ do$PSUB = doyes ]; then
  SIGN=m
else
  SIGN=
fi
MEM=1
while [ $MEM -le $MEMBER ]; do
irow=$MEM
PDATE1=`cat pdatebase.txt | awk '{if(NR == '$irow') {print $1}}'`
irow=`expr $irow + $MEMBER`
PDATE2=`cat pdatebase.txt | awk '{if(NR == '$irow') {print $1}}'`
echo $PDATE1 $PDATE2
PMEM=`printf '%0.3d' $MEM` #prtb member
OUTDIR=$DATADIR/${head}${SIGN}${PMEM}
if [ ! -d $OUTDIR ]; then
  echo 'no such directory '$OUTDIR
  exit 99
fi
MEM4=`printf '%0.4d' $MEM`
ln -s $OUTDIR/rb_sigf$fh ro.$MEM4.sig.grd
ln -s $OUTDIR/rb_sfcf$fh ro.$MEM4.sfc.grd
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
MEM4=`expr 2 \* $MEM - 1`
MEM4=`printf '%0.4d' $MEM4`
ln -s $PRTBDIR1/r_sig.f$fh ri.$MEM4.sig.grd
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
MEM4=`expr 2 \* $MEM`
MEM4=`printf '%0.4d' $MEM4`
ln -s $PRTBDIR2/r_sig.f$fh ri.$MEM4.sig.grd
#
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
 pow=${SCLPOW},
&end
EOF
./${EXEC} < namelist 2>>${EXEC}.log 1>>${EXEC}.err
h=`expr $h + $INCBASE`
done #while h -le ENDHOUR
mv ${EXEC}.log ${EXEC}.err $DATADIR/
echo END
