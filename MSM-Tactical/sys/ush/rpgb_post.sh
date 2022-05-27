#!/bin/sh
set -x
##
pwd
PROG=rpgbnawips
hxs=$1
h=` expr $hxs + 0 `
if [ $h -lt 10 ] ; then h=0$h ; fi
r_pgbf=r_pgb.f$h
#

###################################
# Defining namelist
###################################

IGRD1=`expr $IGRD + 1`
JGRD1=`expr $JGRD + 1`
INTERP=${INTERP:-no}

#...........Grid pacific...........................................
if [ $INTERP = yes ] ; then
echo " &NAMPGB                                          "   > rpgb.parm
echo "         NCPUS=4, KO=$KO,                         "  >> rpgb.parm
echo "         IO=$IO, JO=$JO,                          "  >> rpgb.parm
echo "         PROJO=$PROJO,                            "  >> rpgb.parm
echo "         RLAT1O=$RLAT1O, RLON1O=$RLON1O,          "  >> rpgb.parm
echo "         RLAT2O=$RLAT2O, RLON2O=$RLON2O,          "  >> rpgb.parm
echo "         NTRAC=1, NCLD=$NCLD,NEWSLM=$NEWSLM,      "  >> rpgb.parm
echo " &END                                             "  >> rpgb.parm
else
echo " &NAMPGB                                 "   > rpgb.parm
echo "         NCPUS=4, KO=$KO,                 "  >> rpgb.parm
echo "         IO=$IGRD1, JO=$JGRD1,            "  >> rpgb.parm
echo "         NTRAC=1, NCLD=$NCLD,             "  >> rpgb.parm
echo " &END                                 "     >> rpgb.parm
fi  

##############################
# post script
##############################

rm -f fort.[0-9]* 2>/dev/null
ln -fs r_sig.f$h     fort.11
ln -fs r_flx.f$h     fort.21
ln -fs $r_pgbf       fort.51
ln -fs ctlprs        fort.61
ln -fs ctlslr        fort.62
ln -fs ctldlr        fort.63
if [ $NEWSLM = 1 ] ; then
ln -fs $SLMFILE      fort.71
fi
rm -f $PROG.x
ln -fs $UTLDIR/$PROG.x $PROG.x
${PWD}/$PROG.x <rpgb.parm >stdout.rpgb$h 2>&1
#${RUNDIR}/$PROG.x <rpgb.parm >stdout.rpgb$h 2>&1
if [ $? -ne 0 ] ; then
  echo " Error after "$PROG.x" at "$h
  exit 400
fi
#
grep ',0' ctlprs | wc -l >wc.out
read n <wc.out && rm wc.out
echo "s|DATAFILE|^$r_pgbf|g" >inp
echo "s|MAPFILE|^$r_pgbf.map|g" >>inp
echo "s|TOTALNUM|$n|g" >>inp
sed -f inp ctlprs >$r_pgbf.ctlprs
#rm -f fort.[0-9]* 2>/dev/null

exit

