#!/bin/sh
set -x
#
PROG=rsm
hx=$1
rm -f fort.[0-9]* 2>/dev/null
#
#jun's change add merge global surface

#ln -fs r_sfcfglb                         fort.52
ln -fs rmtnslm                           fort.23
ln -fs rmtnoss                           fort.24
ln -fs $CO2CON    fort.15
ln -fs $TUNE1     fort.43
ln -fs $O3CLIM    fort.48
ln -fs rmtnvar    fort.24
ln -fs $O3PROD    fort.28
ln -fs $O3LOSS    fort.29
ln -fs r_sigi     fort.30
ln -fs r_sigitdt  fort.31
ln -fs r_sfci     fort.32
for m in 01 02 03 04 05 06 07 08 09 10 11 12
do
  ln -fs $AERDIR/global_aeropac3a.m$m.txt aeropac3a.m$m
done
#
ln -fs r_sig.f$hx     fort.70
ln -fs r_sigftdt      fort.71
ln -fs r_sfc.f$hx     fort.72
ln -fs r_flx.f$hx     fort.73
ln -fs r_flx.f00      fort.74
ln -fs r_init         fort.78
#
# at every prthour, the following 3 outputs will be available:
# unit 75=intermediate sigma file output: r_sigf$h
# unit 76=intermediate surface file output: r_sfcf$h
# unit 79=intermediate flux file output: r_flxf$h
#
rm -f $PROG.x
#ln -fs $EXPEXE/$PROG.x $PROG.x
cp $EXPEXE/$PROG.x $PROG.x
###$RUNENV ./$PROG.x <rfcstparm >stdout.fcst$hx 2>&1
$RUNENV ./$PROG.x <rfcstparm 2>&1 | tee stdout.fcst$hx
#
if [ $? -ne 0 ] ; then
   echo " Error after "$PROG.x
   exit 301
fi
rm -f fort.[0-9]* 2>/dev/null
#
