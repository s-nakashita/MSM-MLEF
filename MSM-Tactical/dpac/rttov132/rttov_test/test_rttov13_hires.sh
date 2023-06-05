#!/bin/sh

# User test

# Tests RTTOV v13 direct/TL/AD/K simulations for hi-res IR instruments including cloud/aerosol profiles.

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

SESSION=test_rttov13_hires
OPTS="IGNORETINY=1 TINYABS=1.E-8 $*"
WHAT="DIRECT=1 TL=1 AD=1 K=1"
CHECK="CHECK=1 TEST_REF=$SESSION.2"

./rttov_test.pl SESSION=$SESSION $WHAT $CHECK ARCH=$ARCH $OPTS -- << EOF
  TEST_LIST=airs/401,airs/561
  TEST_LIST=airs/582,airs/583
  TEST_LIST=iasi/401,iasi/561           SOLAR=1
  TEST_LIST=iasi/582,iasi/583
EOF
