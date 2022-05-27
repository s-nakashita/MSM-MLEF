#! /bin/sh
#
set -ex
#
PROG=rmtn
mtnres=$1
#
rm -f fort.* top*
if [ $mtnres -ne 30 ] ; then
   MTNDATA=top${mtnres}m
   MTNDIR=${FIXDIR}
   MTN_AVG=$MTNDIR/${MTNDATA}_avg.20i4.asc
   MTN_VAR=$MTNDIR/${MTNDATA}_var.20i4.asc
   MTN_MAX=$MTNDIR/${MTNDATA}_max.20i4.asc
   MTN_SLM=$MTNDIR/${MTNDATA}_slm.80i1.asc
   ##cp $MTNDIR/${MTNDATA}* . || exit
   ##uncompress ${MTNDATA}*
   cp rsmlocation rmtn.parm
#
else
   MTN_AVG=dummy1
   MTN_VAR=dummy2
   MTN_MAX=dummy3
   MTN_SLM=dummy4
   ln -fs ${FIXDIR}/GTOPO30/*.tar.gz .
   for i in `ls *.gz`
   do 
     gunzip -c $i | tar xvf -
   done
   rm *.tar.gz
   MTNDIR=${RUNDIR}
   echo " &namcondir" >condir.parm
   echo " condir='$MTNDIR'," >>condir.parm
   echo " &END" >>condir.parm
   cat rsmlocation condir.parm >rmtn.parm
fi
#
rm -f fort.*
#
ln -fs $MTN_AVG       fort.11
ln -fs $MTN_VAR       fort.12
ln -fs $MTN_MAX       fort.13
ln -fs $MTN_SLM       fort.14
#
ln -fs rmtnslm     fort.51
ln -fs rmtnoro     fort.52
ln -fs rmtnvar     fort.53
ln -fs rmtnors     fort.54
ln -fs rmtnoss     fort.55
#
# rm $PROG.x
ln -fs $EXPEXE/$PROG.x $PROG.x
rm -f stdout.rmtn
${PWD}/$PROG.x <rmtn.parm >stdout.rmtn 
#. ${RUNDIR}/$PROG.x <rmtn.parm >stdout.rmtn 
if [ $? -ne 0 ] ; then
   echo " Error after "$PROG.x
   exit 201
fi
echo ' Normal end of rmtn.sh '
#cat stdout.rmtn
#rm -f fort.[0-9]* 2>/dev/null
#rm -f fort.[0-9]* 

if [ $mtnres -eq 30 ] ; then
  rm -f E*.??? W*.???
fi
