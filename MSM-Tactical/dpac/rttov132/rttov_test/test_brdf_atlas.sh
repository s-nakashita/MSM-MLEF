#!/bin/sh
#
# VIS/NIR BRDF atlas test control script
#
# This test requires the BRDF atlas files in the directory $DATAATLAS
# These should be downloaded from the RTTOV website
#

# Set BIN directory if supplied
BIN=`perl -e 'for(@ARGV){m/^BIN=(\S+)$/o && print "$1";}' $*`
if [ "x$BIN" = "x" ]
then
  BIN=bin
fi

######## Edit this section for your pathnames #######

# Relative to the rttov_test/test_emisatlas.1 directory:
DATART=../../rtcoef_rttov13/rttov13pred54L
DATAATLAS=../../brdf_data
EXEC=../../$BIN/rttov_brdf_atlas_test.exe
REF_TEST=../test_brdf_atlas.2

# Relative to the rttov_test directory:
TEST=./test_brdf_atlas.1

TEST_INPUT_FILE="rttov_brdf_atlas_test_input"
MONTH=08

COEF_FILENAME="rtcoef_msg_4_seviri_o3co2.dat"
TEST_NUMBER=1

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
echo " Test BRDF atlas "
echo " "

cat  > $TEST_INPUT_FILE  <<EOF
$MONTH                                    # Month for which to load emissivity data
$COEF_FILENAME
3                                         # Number of channels
1 2 3                                     # Channel list
$DATAATLAS
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

mv output_brdf_atlas.ascii  output_brdf_atlas.$TEST_NUMBER.$ARCH

if [ $? -ne 0 ]; then
  echo "Expected output file not found"
  exit 1
fi

echo
echo "Output is in the file ${TEST}/output_brdf_atlas.$TEST_NUMBER.$ARCH"

DIFF_FILE=diff_brdf_atlas.$TEST_NUMBER.$ARCH

if [ -f $REF_TEST/output_brdf_atlas.$TEST_NUMBER ] ; then
  diff -biw output_brdf_atlas.$TEST_NUMBER.$ARCH \
    $REF_TEST/output_brdf_atlas.$TEST_NUMBER > $DIFF_FILE

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
