#!/bin/sh

# Runs all user tests
# Comment out unwanted tests

# Usage: test_core.sh ARCH=myarch [BIN=bindir]
# where myarch = architecture
#       bindir = location of binaries relative to top-level RTTOV directory (optional)


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

OPTS="$*"

./test_fwd.sh              $OPTS
./test_rttov13.sh          $OPTS
./test_rttov13_hires.sh    $OPTS
./test_solar.sh            $OPTS
./test_pc.sh               $OPTS
./test_multi_instrument.sh $OPTS
