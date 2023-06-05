#!/bin/sh

# User test

# Tests the HTFRTC-RTTOV interface.

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

SESSION=test_htfrtc
OPTS="IGNORETINY=1 $* HTFRTC=1"
WHAT="DIRECT=1"
CHECK="CHECK=1 TEST_REF=$SESSION.2"

./rttov_test.pl SESSION=$SESSION $WHAT $CHECK ARCH=$ARCH $OPTS -- << EOF
  TEST_LIST=iasi/pc241htfrtc_norr
  TEST_LIST=iasi/pc241htfrtc_rr
EOF

