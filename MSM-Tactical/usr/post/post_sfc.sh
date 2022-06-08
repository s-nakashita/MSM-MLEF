#!/bin/sh
#
# convert r_sfc.fNN to GrADS file
# caution : This scripts should be carried out afher post_sig.sh
#
SDATE=2022060800
DATADIR=/zdata/grmsm/work/rsm2msm9_jpn/$SDATE
FIGDIR=/zdata/grmsm/fig/rsm2msm9_jpn/$SDATE
MSMDIR=/home/nakashita/Development/grmsm/MSM-Tactical
SRCDIR=${MSMDIR}/usr/post
EXEC=read_sfc
make ${EXEC}
cd $DATADIR
ln -s ${SRCDIR}/${EXEC} ${EXEC}
fh=0
end_hour=9
inc_h=1
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
./${EXEC} 2>&1
echo "dset ^${out}" > header.ctl
head -n 70 sig.f${fh}.ctl | tail -n 69 > middle.ctl
cat header.ctl middle.ctl footer.ctl > $ctl
rm header.ctl middle.ctl footer.ctl
#sed -i -e 's/DATAFILE/'$out'/g' $ctl
#rm ${ctl}-e
rm fort.*
# plot SST
grads -lbx -c "run ${MSMDIR}/usr/grscripts/sst.gs ${fh}"
mv sst_f${fh}.png $FIGDIR
fh=`echo $fh + $inc_h | bc`
done
ls -ltr | tail -n 10
echo END
