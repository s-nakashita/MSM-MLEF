#!/bin/sh
CDIR=${HOME}/Development/grmsm/MSM-Tactical/usr/nclscripts
ires=9
ires=${1}
fh=3
fh=${2}
WDIR=${RUNDIR}
#ODIR=/zdata/grmsm/fig/${WDIR}
ODIR=`echo $WDIR | sed -e "s/work/fig/"`
echo $ODIR
if [ ! -d $ODIR ]; then
  mkdir -p -m 775 $ODIR
fi
echo $SDATE $fh
${HOME}/.local/ncarg/bin/ncl -nQ init=\"${SDATE}\" fh=${fh} ires=${ires} wdir=\"${WDIR}\" ${CDIR}/panel6_2.ncl #1>/dev/null
if [ $fh -lt 10 ]; then
  fh=00$fh
elif [ $fh -lt 100 ]; then
  fh=0$fh
fi
convert panel6_fh${fh}.png -gravity center -crop 1024x580+0+0! ${ODIR}/panel6_fh${fh}.png
rm panel6_fh${fh}.png
