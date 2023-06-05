#!/bin/sh

# User test

# Test running RTTOV for several different instruments/profiles at once.

ARG_ARCH=`perl -e 'for(@ARGV){m/^ARCH=(\S+)$/o && print "$1";}' $*`
if [ ! "x$ARG_ARCH" = "x" ]; then
  ARCH=$ARG_ARCH
fi
if [ "x$ARCH" = "x" ];
then
  echo 'Please supply ARCH'
  exit 1
fi

set -x

SESSION=test_multi_instrument
OPTS="IGNORETINY=1 TINYABS=1.E-8 $*"
WHAT="DIRECT=1 TL=1 AD=1 K=1"
CHECK="CHECK=1 TEST_REF=$SESSION.2"

./rttov_test.pl SESSION=$SESSION $WHAT $CHECK ARCH=$ARCH $OPTS -- << EOF
  TEST_LIST=avhrr/418+hirs/517+amsua/321clw+airs/421
  TEST_LIST=ahi/582+hirs/517+ssmis/301+iasi/761
  TEST_LIST=hirs/517+seviri/583opac+airs/401+mhs/301
EOF
