#!/bin/sh

# User test

# Tests RTTOV v13 direct/TL/AD/K clear-sky simulations for various MW and IR instruments.

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

SESSION=test_rttov13
OPTS="IGNORETINY=1 $*"
WHAT="DIRECT=1 TL=1 AD=1 K=1"
CHECK="CHECK=1 TEST_REF=$SESSION.2"

./rttov_test.pl SESSION=$SESSION $WHAT $CHECK ARCH=$ARCH $OPTS -- << EOF
  TEST_LIST=amsre/301
  TEST_LIST=amsua/301,amsua/321clw
  TEST_LIST=amsub/301
  TEST_LIST=msu/301
  TEST_LIST=ssmis/301,ssmis/321
  TEST_LIST=windsat/301
  TEST_LIST=hirs/517                APPLY_REG_LIMITS=1
  TEST_LIST=modis/401               SOLAR=1
  TEST_LIST=seviri/524              SOLAR=1
EOF

