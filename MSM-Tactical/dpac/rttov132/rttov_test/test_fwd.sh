#!/bin/sh

# User test

# Tests forward model for a wide range of instruments.

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

SESSION=test_fwd
OPTS="IGNORETINY=1 $*"
WHAT="DIRECT=1"
CHECK="CHECK=1 TEST_REF=$SESSION.2"

./rttov_test.pl SESSION=$SESSION $WHAT $CHECK ARCH=$ARCH $OPTS -- << EOF
  TEST_LIST=abi/501 SOLAR=1
  TEST_LIST=ahi/501 SOLAR=1
  TEST_LIST=airs/401,airs/561 SOLAR=1
  TEST_LIST=amsre/301
  TEST_LIST=amsua/301,amsua/321clw,amsua/399
  TEST_LIST=amsub/301
  TEST_LIST=atsr/501,atsr/502,atsr/503s SOLAR=1
  TEST_LIST=avhrr/416,avhrr/417,avhrr/418 SOLAR=1
  TEST_LIST=hirs/417,hirs/517
  TEST_LIST=iasi/401,iasi/561
  TEST_LIST=mhs/301
  TEST_LIST=modis/401,modis/521 SOLAR=1
  TEST_LIST=msu/301
  TEST_LIST=seviri/404,seviri/524,seviri/599 SOLAR=1
  TEST_LIST=ssmi/308
  TEST_LIST=ssmis/303,ssmis/321
  TEST_LIST=tmi/301
  TEST_LIST=windsat/301,windsat/321
EOF
