#!/bin/sh
#
# calculate ensemble mean and spread for r_sig.fNN, r_sfc.fNN and r_flx.fNN
#
set -ex
head=${1}
bgm=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/dpac/build/post
DATA=${RUNDIR0:-/zdata/grmsm/work/dpac/rsm27}
DATADIR=$DATA/$SDATE
if [ ! -d $DATADIR ]; then
  echo "No such directory : $DATADIR"
  exit 3
fi
echo "MEMBER="$MEMBER
echo "HEAD "$head
EXEC=ensmspr
cd $SRCDIR
gmake ${EXEC}
meandir=${head}mean
sprddir=${head}sprd
mkdir -p $DATADIR/${meandir}
mkdir -p $DATADIR/${sprddir}
rm -rf $DATADIR/tmp
mkdir -p $DATADIR/tmp
cd $DATADIR/tmp
ln -s ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
inc_h=$PRTHOUR
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
rm -f r_sig.* r_sfc.* r_flx.* fort.*
MEM=1
NENS=$MEMBER
while [ $MEM -le $MEMBER ]; do
if [ $MEM -lt 10 ];then
MEM=00$MEM
else
MEM=0$MEM
fi
if [ ! -d $DATADIR/${head}${MEM} ]; then
exit 99
fi
ln -s $DATADIR/${head}${MEM}/r_sig.f$fh r_sig.0$MEM
ln -s $DATADIR/${head}${MEM}/r_sfc.f$fh r_sfc.0$MEM
ln -s $DATADIR/${head}${MEM}/r_flx.f$fh r_flx.0$MEM
MEM=`expr $MEM + 1`
done
if [ do$bgm = doyes ] && [ do$PSUB = doyes ]; then
  NENS=`expr $NENS + $MEMBER`
  PMEM=1
  while [ $PMEM -le $MEMBER ]; do
  if [ $PMEM -lt 10 ];then
  PMEM=00$PMEM
  else
  PMEM=0$PMEM
  fi
  if [ ! -d $DATADIR/${head}m${PMEM} ]; then
  exit 99
  fi
  if [ $MEM -lt 10 ];then
  MEM=00$MEM
  else
  MEM=0$MEM
  fi
  ln -s $DATADIR/${head}m${PMEM}/r_sig.f$fh r_sig.0$MEM
  ln -s $DATADIR/${head}m${PMEM}/r_sfc.f$fh r_sfc.0$MEM
  ln -s $DATADIR/${head}m${PMEM}/r_flx.f$fh r_flx.0$MEM
  PMEM=`expr $PMEM + 1`
  MEM=`expr $MEM + 1`
  done
fi
ln -s r_sigm.f$fh fort.51
ln -s r_sfcm.f$fh fort.52
ln -s r_flxm.f$fh fort.53
ln -s r_sigs.f$fh fort.54
ln -s r_sfcs.f$fh fort.55
ln -s r_flxs.f$fh fort.56
cat <<EOF >ensmspr.nml
&namlst_ensmspr
 nens=$NENS,
&end
EOF
./${EXEC} < ensmspr.nml 2>>${EXEC}.log 1>>${EXEC}.err
mv r_sigm.f$fh $DATADIR/${meandir}/r_sig.f$fh
mv r_sfcm.f$fh $DATADIR/${meandir}/r_sfc.f$fh
mv r_flxm.f$fh $DATADIR/${meandir}/r_flx.f$fh
mv r_sigs.f$fh $DATADIR/${sprddir}/r_sig.f$fh
mv r_sfcs.f$fh $DATADIR/${sprddir}/r_sfc.f$fh
mv r_flxs.f$fh $DATADIR/${sprddir}/r_flx.f$fh
rm r_sig* r_sfc* r_flx* fort.*
cd $DATADIR/${meandir}
$USHDIR/rpgb_post.sh $fh
cd -
fh=`echo $fh + $inc_h | bc`
done
mv ${EXEC}.log ${EXEC}.err $DATADIR/
ls -ltr $DATADIR/${meandir} | tail -n 10
ls -ltr $DATADIR/${sprddir} | tail -n 10
echo END
