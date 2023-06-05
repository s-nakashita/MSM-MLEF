#!/bin/sh
#
# CAMEL climatology IR emissivity atlas test control script
#
# This test requires the CAMEL climatology IR emissivity atlas files in the directory $DATAATLAS
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
EXEC=../../$BIN/rttov_camel_clim_atlas_test.exe
REF_TEST=../test_emis_atlas.2

# Relative to the rttov_test directory:
TEST=./test_emis_atlas.1

TEST_INPUT_FILE="rttov_iratlas_test_input"
MONTH=08

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
echo " Test CAMEL climatology IR emissivity atlas "
echo " "

#for TEST_NUMBER in 01 02
for TEST_NUMBER in 01
do

  case $TEST_NUMBER in
  
    '01') COEF_FILENAME=rtcoef_eos_1_modis_o3co2.dat
   
    cat  > $TEST_INPUT_FILE  <<EOF
$MONTH                                             # Month for which to load emissivity data
$COEF_FILENAME
16                                                 # Number of channels
20 21 22 23 24 25 27 28 29 30 31 32 33 34 35 36    # Channel list
$DATAATLAS
EOF

    ;;

    '02') COEF_FILENAME=rtcoef_metop_2_iasi_o3.H5

    cat  > $TEST_INPUT_FILE  <<EOF
$MONTH
$COEF_FILENAME
19
93 137 185 237 285 349 413 1013 1533 2289 2877 3357 6181 6273 6381 6525 6865 7469 8105
$DATAATLAS
EOF

    ;;
  esac

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

  mv output_camel_clim_atlas.ascii  output_camel_clim_atlas.$TEST_NUMBER.$ARCH

  if [ $? -ne 0 ]; then
    echo "Expected output file not found"
    exit 1
  fi

  echo
  echo "Output is in the file ${TEST}/output_camel_clim_atlas.$TEST_NUMBER.$ARCH"

  DIFF_FILE=diff_camel_clim_atlas.$TEST_NUMBER.$ARCH

  if [ -f $REF_TEST/output_camel_clim_atlas.$TEST_NUMBER ] ; then
    diff -biw output_camel_clim_atlas.$TEST_NUMBER.$ARCH \
      $REF_TEST/output_camel_clim_atlas.$TEST_NUMBER > $DIFF_FILE

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
