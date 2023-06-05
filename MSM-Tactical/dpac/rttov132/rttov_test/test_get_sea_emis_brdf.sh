#!/bin/sh
# Script to run the rttov_get_sea_emis_brdf_test.exe test.
#
# There are no reference data for this test.
# 
# The program runs tests for one MW sensor and one VIS/IR sensor
# with rtcoef file paths provided by this script.
#
# It uses the data of the first profile in prof.dat for the
# example_fwd.exe test program.

# Set BIN directory if supplied
BIN=$(perl -e 'for(@ARGV){m/^BIN=(\S+)$/o && print "$1";}' $*)
if [ "x$BIN" = "x" ]
then
  BIN=bin
fi

######## Edit this section for test case input and pathnames ######

# Test case input data
PROF_FILENAME="test_example.1/prof.dat"   # Input profile(s)
NLEVELS=51                                # Number of profile levels

# The paths below do not need editing unless you move input files to different locations

# Paths relative to the rttov_test/${TEST_DIR} directory:
BIN_DIR=../$BIN                           # BIN directory (may be set with BIN= argument)

# MW and VIS/IR coefficient files
MW_RTCOEF_FILE=../rtcoef_rttov13/rttov13pred54L/rtcoef_jpss_0_atms.dat
VISIR_RTCOEF_FILE=../rtcoef_rttov13/rttov13pred54L/rtcoef_goes_16_abi_o3co2.dat

########################################################################

ARG_ARCH=$(perl -e 'for(@ARGV){m/^ARCH=(\S+)$/o && print "$1";}' $*)
if [ ! "x$ARG_ARCH" = "x" ]; then
  ARCH=$ARG_ARCH
fi
if [ "x$ARCH" = "x" ];
then
  echo 'Please supply ARCH'
  exit 1
fi

echo " "
echo " "
echo " Test rttov_get_sea_emis/brdf routines "
echo " "

echo  "MW rtcoef file:     ${MW_RTCOEF_FILE}"
echo  "VIS/IR rtcoef file: ${VISIR_RTCOEF_FILE}"
echo  "Input profile file: ${PROF_FILENAME}"
echo  "Number of levels:   ${NLEVELS}"

$BIN_DIR/rttov_test_get_sea_emis_brdf.exe << EOF
"${MW_RTCOEF_FILE}",    MW coefficient file
"${VISIR_RTCOEF_FILE}", VIS/IR coefficient file
"${PROF_FILENAME}",     Input profile filename
${NLEVELS}      ,       Number of levels
EOF

if [ $? -ne 0 ]; then
  echo " "
  echo "TEST FAILED"
  echo " "
  exit 1
fi

exit
