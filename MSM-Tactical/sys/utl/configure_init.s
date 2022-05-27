#!/bin/sh
#
# this utility is to localize your DISK, DISKSYS, DISKUSR, and WORK definitions
# in configure file at any location by following definitions

DISK=` cd ../.. ; pwd `
WORK=` pwd `

echo "/export DISK=/a\\" 	 >sed.file
echo " export DISK=$DISK "	>>sed.file
echo "/export DISK=/d"		>>sed.file
echo "/export WORK=/a\\" 	>>sed.file
echo " export WORK=$WORK "	>>sed.file
echo "/export WORK=/d"		>>sed.file
echo "/export DISKSYS=/a\\" 	>>sed.file
echo " export DISKSYS=$DISKSYS ">>sed.file
echo "/export DISKSYS=/d"	>>sed.file
echo "/export DISKUSR=/a\\" 	>>sed.file
echo " export DISKUSR=$DISKUSR ">>sed.file
echo "/export DISKUSR=/d"	>>sed.file

sed -f sed.file configure >configout

wc -l configure >lc1out ; read lc1 out <lc1out
wc -l configout >lc2out ; read lc2 out <lc2out
if [ $lc1 = $lc2 ] ; then mv configout configure ; fi

rm lc1out lc2out sed.file
