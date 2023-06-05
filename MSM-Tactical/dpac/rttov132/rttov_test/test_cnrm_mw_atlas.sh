#!/bin/sh
#
# CNRM MW emissivity atlas test control script
#
# This test requires the CNRM MW atlas files in the directory $DATAATLAS
# These should be downloaded from the RTTOV website
#

# Set BIN directory if supplied
BIN=`perl -e 'for(@ARGV){m/^BIN=(\S+)$/o && print "$1";}' $*`
if [ "x$BIN" = "x" ]
then
  BIN=bin
fi

######## Edit this section for your pathnames #######

# Relative to the rttov_test/test_emis_atlas.1 directory:
DATART=../../rtcoef_rttov13/rttov13pred54L
DATAATLAS=../../emis_data
EXEC=../../$BIN/rttov_cnrm_mw_atlas_test.exe
REF_TEST=../test_emis_atlas.2

# Relative to the rttov_test directory:
TEST=./test_emis_atlas.1

TEST_INPUT_FILE="rttov_cnrmmwatlas_test_input"
NMONTH=2
MONTH="6 8"

COEF_FILENAME="rtcoef_noaa_15_amsua.dat"
TEST_NUMBER=01

###################################################

ARG_ARCH=`perl -e 'for(@ARGV){m/^ARCH=(\S+)$/o && print "$1";}' $*`
if [ ! "x$ARG_ARCH" = "x" ]; then
  ARCH=$ARG_ARCH
fi
if [ "x$ARCH" = "x" ];
then
  echo 'Please supply ARCH'
  exit 1
fi

if [ ! -d $TEST ]; then
  echo "Test directory $TEST required: this script should be run from the rttov_test/ directory"
  exit
fi

cwd=`pwd`
cd $TEST

if [ ! -f $EXEC ]; then
  echo "Test executable not found: $EXEC"
  exit
fi

echo " "
echo " "
echo " Test CNRM MW emissivity atlas "
echo " "

cat  > $TEST_INPUT_FILE  <<EOF
$COEF_FILENAME
$DATAATLAS
$NMONTH         # Number of months for which to load emissivity data
$MONTH          # Months for which to load emissivity data
650             # number of profiles
85, -0.20       # latitude start, latitude step
20,  0.00       # longitude start, longitude step
 0,  0.08       # Zenith angle (degrees)
6               # number of channels
1,2,3,4,7,15    # list of channels
EOF

rm -f $COEF_FILENAME
if [ -s  $DATART/$COEF_FILENAME ] ; then
  ln -s $DATART/$COEF_FILENAME $COEF_FILENAME
else
  echo "rtcoef file not found: $DATART/$COEF_FILENAME"
  cd $cwd
  exit
fi

$EXEC < $TEST_INPUT_FILE

if [ $? -ne 0 ]; then
  echo " "
  echo "TEST FAILED"
  echo " "
  exit 1
fi

mv output_cnrm_mw_atlas.ascii  output_cnrm_mw_atlas.$TEST_NUMBER.$ARCH

if [ $? -ne 0 ]; then
  echo "Expected output file not found"
  exit 1
fi

echo
echo "Output is in the file ${TEST}/output_cnrm_mw_atlas.$TEST_NUMBER.$ARCH"

DIFF_FILE=diff_cnrm_mw_atlas.$TEST_NUMBER.$ARCH

if [ -f $REF_TEST/output_cnrm_mw_atlas.$TEST_NUMBER ] ; then
  diff -biw output_cnrm_mw_atlas.$TEST_NUMBER.$ARCH \
    $REF_TEST/output_cnrm_mw_atlas.$TEST_NUMBER > $DIFF_FILE

  if [ -s $DIFF_FILE ]; then
    echo "--- Diff file contents: ---"
    cat $DIFF_FILE
    echo "---------------------------"
  else
    echo " "
    echo "Diff file has zero size: TEST SUCCESSFUL"
    echo " "
  fi
else
  echo "Test reference output not found"
fi
echo

rm -f $TEST_INPUT_FILE
rm -f $COEF_FILENAME


cd $cwd

exit
