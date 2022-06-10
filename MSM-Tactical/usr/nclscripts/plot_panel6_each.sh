#!/bin/sh
CDIR=${HOME}/Development/grmsm/MSM-Tactical/usr/nclscripts
ires=9
ires=${1}
fh=3
fh=${2}
if [ $ires -eq 9 ]; then
ODIR=/zdata/grmsm/fig/rsm2msm9_jpn/${SDATE}
else
ODIR=/zdata/grmsm/fig/msm2msm3_jpn/${SDATE}
fi
echo $ODIR
if [ ! -d $ODIR ]; then
  mkdir -p $ODIR
fi
echo $SDATE $fh
${HOME}/.local/ncarg/bin/ncl -nQ init=\"${SDATE}\" fh=${fh} ires=${ires} ${CDIR}/panel6.ncl 1>/dev/null
if [ $fh -lt 10 ]; then
  fh=0$fh
fi
convert panel6_fh${fh}.png -gravity center -crop 1024x580+0+0! ${ODIR}/panel6_fh${fh}.png
rm panel6_fh${fh}.png
