#!/bin/sh
#
# prepare base field for OSSE
#
set -ex
IDATE=${SDATE0:-2022083000} #cycle start
IRES=${IRES:-9}
ICYCLE=${ICYCLE:-1}
INCCYCLE=${INCCYCLE:-6}
HEADBASE=${HEAD:-bv}
HEADDA=${HEAD2:-da}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/pre
DATADIR=${RUNDIR:-/zdata/grmsm/work/dpac/rsm27/$SDATE}
BASE=${BASEDIR0:-/zdata/grmsm/work/gfsp2rsm27_nomad}
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
if [ ! -d $BASE ]; then
  echo "No such directory : $BASE"
  exit 3
fi

EXEC=replacedate
cd $SRCDIR
gmake ${EXEC}
BASEDIR=${BASE}/${IDATE}
if [ ! -d $BASEDIR ];then
  echo 'base data not exist'
  exit 99
fi
echo $BASEDIR

cd $DATADIR
rm -rf tmp
mkdir -p tmp
cd tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}

fhbase=`expr $INCCYCLE \* $ICYCLE`

h=0
while [ $h -le $ENDHOUR ]; do
fh=`printf '%0.2d' $h`
hold=`expr $h + $fhbase`
fhold=`printf '%0.2d' $hold`
rm -f fort.*
# control
ln -s $BASEDIR/r_sig.f$fhold ri.0000.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold ri.0000.sfc.grd
# member
MEM=1
while [ $MEM -le $MEMBER ]; do
PMEM=`printf '%0.3d' $MEM` #prtb member
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sig.f$fhold ri.0${PMEM}.sig.grd
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sfc.f$fhold ri.0${PMEM}.sfc.grd
#
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
### set namelist
cat <<EOF >namelist
&namlst_replace
 newfhour=${h}.0,
 member=${MEMBER},
&end
EOF
./${EXEC} < namelist #2>/dev/null
mv ro.0000.sig.grd $RUNDIR/${HEADDA}000/rb_sigf$fh
mv ro.0000.sfc.grd $RUNDIR/${HEADDA}000/rb_sfcf$fh
MEM=1
while [ $MEM -le $MEMBER ]; do
PMEM=`printf '%0.3d' $MEM` #prtb member
mv ro.0${PMEM}.sig.grd $RUNDIR/${HEADDA}${PMEM}/rb_sigf$fh
mv ro.0${PMEM}.sfc.grd $RUNDIR/${HEADDA}${PMEM}/rb_sfcf$fh
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
rm -f ri.*.sig.grd ri.*.sfc.grd
h=`expr $h + $INCBASE`
done #while h -le ENDHOUR
echo END
