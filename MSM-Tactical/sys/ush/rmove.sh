#!/bin/sh

# script to move old restart file to new location

set -ex

#
PROG=rmove
mtnres=$1
nest=$2
h=$3
hx=$4
#
#### save old restart files at h and h domain
  mv -f r_sigi    r_sigi_old
  mv -f r_sfci    r_sfci_old
  mv -f r_sigitdt r_sigitdt_old
### create base field at h by using h location
# $USHDIR/rmtn.sh $mtnres || exit 12
  $USHDIR/rinp.sh $nest $h || exit 22
  mv -f r_sigi    r_sigi_base
  mv -f r_sfci    r_sfci_base
  mv -f r_sigitdt r_sigitdt_base
#### new rsm location at next destination hour hx
  cp -f $WORK/route route
  while read hour cenlat cenlon
  do
    if [ $hour -eq $hx ] ; then
       echo "found next-hour center lat lon "hour" "lat" "lon
       break
    fi
  done < route
  export RCENLAT=$cenlat
  export RCENLON=$cenlon
  $USHDIR/rloc.sh || exit 7
### create new restart file at h by using hx location
  $USHDIR/rmtn.sh $mtnres || exit 13
  $USHDIR/rinp.sh $nest $h || exit 23
  mv -f r_sigi    r_sigi_new
  mv -f r_sfci    r_sfci_new
  mv -f r_sigitdt r_sigitdt_new
#### merge old restart file into new restart file
  ln -fs r_sigi_old     fort.11
  ln -fs r_sigitdt_old  fort.12
  ln -fs r_sfci_old     fort.13
  ln -fs r_sigi_base    fort.21
  ln -fs r_sigitdt_base fort.22
  ln -fs r_sfci_base    fort.23
  ln -fs r_sigi_new     fort.31
  ln -fs r_sigitdt_new  fort.32
  ln -fs r_sfci_new     fort.33
  ln -fs r_sigi         fort.51
  ln -fs r_sigitdt      fort.52
  ln -fs r_sfci         fort.53
##
  ln -fs $EXPEXE/$PROG.x $PROG.x
  ./$PROG.x >stdout.rmove$h 
  if [ $? -ne 0 ] ; then
     echo " Error after "$PROG.x" at "$h
     exit 
  fi
# cat stdout.rmove$h
  rm -f fort.[0-9]* 2>/dev/null
