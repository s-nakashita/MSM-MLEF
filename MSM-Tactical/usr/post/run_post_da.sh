#!/bin/sh
if [ $# -lt 2 ]; then
  echo "Usage : ${0} init(YYYYMMDDHH) res(27, 9 or 3)"
  exit 1
fi
SDATE=${1}
IRES=${2}
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
if [ $IRES -eq 27 ]; then
ICLD=0 #hydrostatic
EXPDIR=${MSMDIR}/usr/exp/rsm2rsm27_da
DATADIR=/zdata/grmsm/work/rsm2rsm27_da/$SDATE
elif [ $IRES -eq 9 ]; then
ICLD=1 #nonhydrostatic
EXPDIR=${MSMDIR}/usr/exp/rsm2msm9_da
DATADIR=/zdata/grmsm/work/rsm2msm9_da/$SDATE
else
echo "Invalid resolution. Specify 9 or 3."
exit 2
fi
cd ${EXPDIR}
. ./configure
#ENDHOUR=0
M=25
MEMMAX=`expr $MEMBER + 2` #+mean,sprd
while [ $M -le $MEMMAX ];do
if [ $M -gt $MEMBER ];then
  if [ $M -lt $MEMMAX ]; then
    MEM=mean
    #MEM=m${MEMBER}mean
  else
    MEM=sprd
    #MEM=m${MEMBER}sprd
  fi
else
MEM=`printf '%0.3d' $M`
fi
echo $SDATE $IRES $ENDHOUR $MEM
if [ $M -eq 0 ];then
WDIR=$DATADIR
else
WDIR=$DATADIR/${HEAD}${MEM}
fi
#WDIR=$DATADIR/${HEAD2}${MEM}
if [ ! -d $WDIR ]; then
  echo 'No such directory, '$WDIR
  exit 99
fi
#
# convert r_sig.fNN to GrADS file
#
EXEC=read_sig
cd $SRCDIR
gmake ${EXEC}
cd $WDIR
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
cat <<EOF >read_sig.nml
&namlst_cld
 icld=${ICLD},
&end
EOF
fh=0
end_hour=$ENDHOUR
inc_h=$INCHOUR
rm -f fort.*
rm ${EXEC}.log
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in=r_sig.f$fh
out=sig.f${fh}.bin
ctl=sig.f${fh}.ctl
rm $out
rm $ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s $ctl fort.61
./${EXEC} < read_sig.nml 1>>${EXEC}.log 2>&1
sed -i -e 's/DATAFILE/'$out'/g' $ctl
rm ${ctl}-e
rm fort.*
fh=`expr $fh + $inc_h | bc`
done
#
# convert r_sfc.fNN to GrADS file
#
EXEC=read_sfc
cd $SRCDIR
make ${EXEC}
cd $WDIR
ln -fs ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=$ENDHOUR
inc_h=$INCHOUR
rm -f fort.*
while [ $fh -le $end_hour ]; do
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
in=r_sfc.f$fh
out=sfc.f${fh}.bin
ctl=sfc.f${fh}.ctl
ln -s $in fort.11
ln -s $out fort.51
ln -s footer.ctl fort.61
./${EXEC} 1>>${EXEC}.log 2>&1 
echo "dset ^${out}" > header.ctl
nrow=`awk '$0~/zdef/{print NR}' sig.f${fh}.ctl`
nrow=`expr $nrow - 1`
nrowm1=`expr $nrow - 1`
head -n $nrow sig.f${fh}.ctl | tail -n $nrowm1 > middle.ctl
cat header.ctl middle.ctl footer.ctl > $ctl
rm header.ctl middle.ctl footer.ctl
rm fort.*
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
M=`expr $M + 1`
done
echo END
