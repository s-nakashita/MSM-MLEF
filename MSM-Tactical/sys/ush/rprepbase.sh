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
hend=$ENDHOUR
if [ $hend -lt $INCBASE ]; then
  hend=$INCBASE
fi
while [ $h -le $hend ]; do
fh=`printf '%0.2d' $h`
set +e
hold=`expr $h + $fhbase`
ltint=`expr $hold % $INCBASE`
if [ $ltint -gt 0 ]; then
## time interpolation
h1=`expr $hold / $INCBASE \* $INCBASE`
h2=`expr $h1 + $INCBASE`
fhold=`printf '%0.2d' $h1`
fhold2=`printf '%0.2d' $h2`
else
fhold=`printf '%0.2d' $hold`
fi
set -e
rm -f fort.*
if [ $MEAN != T ]; then
# control
ln -s $BASEDIR/r_sig.f$fhold ri.0000.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold ri.0000.sfc.grd
if [ $ltint -gt 0 ]; then
ln -s $BASEDIR/r_sig.f$fhold2 r2.0000.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold2 r2.0000.sfc.grd
fi
fi
# member
MEM=1
while [ $MEM -le $MEMBER ]; do
PMEM=`printf '%0.3d' $MEM` #prtb member
if [ $DANEST = F ]; then
ln -s $BASEDIR/r_sig.f$fhold ri.0${PMEM}.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold ri.0${PMEM}.sfc.grd
if [ $ltint -gt 0 ]; then
ln -s $BASEDIR/r_sig.f$fhold2 r2.0${PMEM}.sig.grd
ln -s $BASEDIR/r_sfc.f$fhold2 r2.0${PMEM}.sfc.grd
fi
else
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sig.f$fhold ri.0${PMEM}.sig.grd
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sfc.f$fhold ri.0${PMEM}.sfc.grd
if [ $ltint -gt 0 ]; then
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sig.f$fhold2 r2.0${PMEM}.sig.grd
ln -s $BASEDIR/${HEADBASE}${PMEM}/r_sfc.f$fhold2 r2.0${PMEM}.sfc.grd
fi
fi
#
MEM=`expr $MEM + 1`
done #while MEM -le MEMBER
### set namelist
if [ $ltint -gt 0 ]; then
set +e
hoffset=`expr $h - $h1`
set -e
cat <<EOF >namelist
&namlst_replace
 newfhour=${h}.0,
 offset=${hoffset}.0,
 member=${MEMBER},
 mean=${MEAN},
 tint=T,
 fint=${hold}.0,
 fbase1=${h1}.0,
 fbase2=${h2}.0,
&end
EOF
else
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
if [ $ltint -gt 0 ]; then
rm -f r2.*.sig.grd r2.*.sfc.grd
fi
h=`expr $h + $INCHOUR`
rm -f ri.*.sig.grd ri.*.sfc.grd
done #while h -le ENDHOUR
echo END
