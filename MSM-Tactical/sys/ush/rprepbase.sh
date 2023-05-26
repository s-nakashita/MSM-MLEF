#!/bin/sh
#
# prepare base field for OSSE
#
set -ex
CYCLEDA=${1}
MEAN=${2}
IDATE=${SDATE0:-2022083000} #cycle start
IOFFSET=${IOFFSET:-6} #first guess offset
DANEST=${DANEST:-F}
INCCYCLE=${INCCYCLE:-6}
HEADBASE=${HEAD:-bv}
HEADDA=${HEAD2:-da}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/pre
DATADIR=${RUNDIR:-/zdata/grmsm/work/dpac/rsm27/$SDATE}
BASE0=${BASEDIR0:-/zdata/grmsm/work/gfsp2rsm27_nomad}
BASE1=${BASEDIR1:-/zdata/grmsm/work/gfsp2rsm27_rda}
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi

EXEC=replacedate
cd $SRCDIR
gmake ${EXEC}

IDATE=`date -j -f "%Y%m%d%H" -v+${IOFFSET}H +"%Y%m%d%H" "$IDATE"`
BASEDIR=${BASE0}/${IDATE}
if [ ! -d $BASEDIR ];then
  BASEDIR=${BASE1}/${IDATE}
  if [ ! -d $BASEDIR ]; then
    echo 'base data not exist'
    exit 99
  fi
fi
echo $BASEDIR

cd $DATADIR
rm -rf tmp
mkdir -p tmp
cd tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
set +e
fhbase=`expr $INCCYCLE \* \( $CYCLEDA - 1 \)`
set -e
h=0
while [ $h -le $ENDHOUR ]; do
fh=`printf '%0.2d' $h`
set +e
hold=`expr $h + $fhbase`
set -e
fhold=`printf '%0.2d' $hold`
rm -f fort.*
if [ $MEAN != T ]; then
# control
ln -s $BASEDIR/r_sig.f$fhold ri.0000.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold ri.0000.sfc.grd
fi
# member
MEM=1
while [ $MEM -le $MEMBER ]; do
PMEM=`printf '%0.3d' $MEM` #prtb member
if [ $DANEST = F ]; then
ln -s $BASEDIR/r_sig.f$fhold ri.0${PMEM}.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold ri.0${PMEM}.sfc.grd
else
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sig.f$fhold ri.0${PMEM}.sig.grd
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sfc.f$fhold ri.0${PMEM}.sfc.grd
fi
#
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
### set namelist
if [ $DANEST = F ]; then
cat <<EOF >namelist
&namlst_replace
 newfhour=0.0,
 offset=${fhbase}.0,
 member=${MEMBER},
 mean=${MEAN},
&end
EOF
else
cat <<EOF >namelist
&namlst_replace
 newfhour=${h}.0,
 offset=,
 member=${MEMBER},
 mean=${MEAN},
&end
EOF
fi
./${EXEC} < namelist #2>/dev/null
if [ $MEAN != T ]; then
mv ro.0000.sig.grd $RUNDIR/${HEADDA}000/rb_sigf$fh
mv ro.0000.sfc.grd $RUNDIR/${HEADDA}000/rb_sfcf$fh
fi
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
