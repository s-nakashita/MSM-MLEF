#!/bin/sh
CDIR=${HOME}/Development/grmsm/MSM-Tactical/usr/nclscripts
cd $CDIR
for ires in 9 3;do
#ires=9
#for dd in $(seq 1 1 6); do
#  if [ $dd -lt 10 ]; then
#    dd=0$dd
#  fi
#for hh in 00 12;do
#SDATE="2022060600"
#SDATE="202206"${dd}${hh}
if [ $ires -eq 9 ]; then
ODIR=/zdata/grmsm/fig/rsm2msm9_jpn/${SDATE}
else
ODIR=/zdata/grmsm/fig/msm2msm3_jpn/${SDATE}
fi
echo $ODIR
if [ ! -d $ODIR ]; then
  mkdir -p $ODIR
fi
for fh in $(seq 3 1 48);do
  echo $SDATE $fh
  ${HOME}/.local/ncarg/bin/ncl -nQ init=\"${SDATE}\" fh=${fh} ires=${ires} panel6.ncl 1>/dev/null
  if [ $fh -lt 10 ]; then
    fh=0$fh
  fi
  convert panel6_fh${fh}.png -gravity center -crop 1024x580+0+0! ${ODIR}/panel6_fh${fh}.png
  rm panel6_fh${fh}.png
done
cd $ODIR
convert -delay 75 -loop 0 panel6_fh*.png panel6.gif
ls -ltr
cd $CDIR
#done
#done
done
