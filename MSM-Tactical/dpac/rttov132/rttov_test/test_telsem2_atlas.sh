#!/bin/sh
#
# TELSEM2 MW emissivity atlas test control script
#
# This test requires the TELSEM2 atlas files in the directory $DATAATLAS
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
EXEC=../../$BIN/rttov_telsem2_atlas_test.exe
REF_TEST=../test_emis_atlas.2

# Relative to the rttov_test directory:
TEST=./test_emis_atlas.1

TEST_INPUT_FILE="rttov_mwatlas_test_input"
NMONTH=2
MONTH="8 10"

# Coefficient filenames are specified below

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
echo " Test TELSEM2 MW emissivity atlas "
echo " "

for TEST_NUMBER in 01 02 03
# for TEST_NUMBER in 01 
do

  case $TEST_NUMBER in
  
    '01') COEF_FILENAME=rtcoef_noaa_15_amsua.dat
   
    cat  > $TEST_INPUT_FILE  <<EOF
$NMONTH         # Number of months for which to load emissivity data
$MONTH          # List of months
$COEF_FILENAME
23              # Zenith angle (degrees)
$DATAATLAS
EOF

    ;;

    '02') COEF_FILENAME=rtcoef_dmsp_13_ssmi.dat

    cat  > $TEST_INPUT_FILE  <<EOF
$NMONTH         # Number of months for which to load emissivity data
$MONTH          # List of months
$COEF_FILENAME
57              # Zenith angle (degrees)
$DATAATLAS
EOF

    ;;

    '03') COEF_FILENAME=rtcoef_tropics_0_tropics.dat

    cat  > $TEST_INPUT_FILE  <<EOF
$NMONTH         # Number of months for which to load emissivity data
$MONTH          # List of months
$COEF_FILENAME
0               # Zenith angle (degrees)
$DATAATLAS
EOF

    ;;
  esac

  rm -f $COEF_FILENAME
  if [ -s  $DATART/$COEF_FILENAME ] ; then
    ln -s $DATART/$COEF_FILENAME $COEF_FILENAME
  else
    echo "rtcoef file not found"
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

  mv output_telsem2_atlas.ascii  output_telsem2_atlas.$TEST_NUMBER.$ARCH

  if [ $? -ne 0 ]; then
    echo "Expected output file not found"
    exit 1
  fi

  echo
  echo "Output is in the file ${TEST}/output_telsem2_atlas.$TEST_NUMBER.$ARCH"

  DIFF_FILE=diff_telsem2_atlas.$TEST_NUMBER.$ARCH

  if [ -f $REF_TEST/output_telsem2_atlas.$TEST_NUMBER ] ; then
    diff -biw output_telsem2_atlas.$TEST_NUMBER.$ARCH \
      $REF_TEST/output_telsem2_atlas.$TEST_NUMBER > $DIFF_FILE

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

done

cd $cwd

exit
