#!/bin/sh
# Script to run the example_htfrtc_fwd example program
#
# The result is compared to a reference file.
#

# Set BIN directory if supplied
BIN=$(perl -e 'for(@ARGV){m/^BIN=(\S+)$/o && print "$1";}' $*)
if [ "x$BIN" = "x" ]
then
  BIN=bin
fi

######## Edit this section for your test case input and pathnames ######

# Test case input data
HTFRTC_COEF_STATIC="htfrtc_coef_static.nc"             # HTFRTC static coef filename
HTFRTC_COEF_SENSOR="htfrtc_coef_sensor_metop_iasi.nc"  # HTFRTC sensor coef filename
PROF_FILENAME="prof_htfrtc.dat"                        # Input profile(s), usually found in $TEST_DIR set below
RADREC_FILENAME="radrec.dat"                           # Reconstructed channel list, this file need not exist (see example_htfrtc_fwd.F90)
NPROF=1                                                # Number of profiles defined in prof_htfrtc.dat
NLEVELS=51                                             # Number of profile levels
NPCSCORES=100                                          # Number of PC scores to calculate

MONTH=8                                                # Month (1-12) for emissivity atlas or 0 to not use the atlas

NTHREADS=1                                             # Number of threads to use (compile RTTOV with OpenMP to exploit this)

CHECK_REF=1                                            # Set to 0 to omit check against test reference

# The paths below do not need editing unless you move input files to different locations
# or you require a coefficient file from a different directory

# Path relative to the rttov_test directory:
TEST_DIR=./test_example.1

# Paths relative to the rttov_test/${TEST_DIR} directory:
BIN_DIR=../../$BIN                           # BIN directory (may be set with BIN= argument)
REF_TEST_DIR=../test_example.2               # Test reference data
COEF_DIR=../../rtcoef_rttov13/htfrtc         # HTFRTC Coefficients directory

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

CWD=$(pwd)
cd $TEST_DIR

echo " "
echo " "
echo " Test HTFRTC forward "
echo " "

echo  "HTFRTC coef static:   ${HTFRTC_COEF_STATIC}"
echo  "HTFRTC coef sensor:   ${HTFRTC_COEF_SENSOR}"
echo  "Input profile file:   ${PROF_FILENAME}"
echo  "Rad. rec. file:       ${RADREC_FILENAME}"
echo  "Number of profiles:   ${NPROF}"
echo  "Number of levels:     ${NLEVELS}"
echo  "Month:                ${MONTH}"
echo  "Number of PC scores:  ${NPCSCORES}"
echo  "Number of threads:    ${NTHREADS}"

# Coefficient files
HTFRTC_COEF_STATIC=$COEF_DIR/$HTFRTC_COEF_STATIC
if [ ! -f $HTFRTC_COEF_STATIC ]; then
  echo "Coef file $HTFRTC_COEF_STATIC not found, aborting..."
  exit 1
fi

HTFRTC_COEF_SENSOR=$COEF_DIR/$HTFRTC_COEF_SENSOR
if [ ! -f $HTFRTC_COEF_SENSOR ]; then
  echo "Coef file $HTFRTC_COEF_SENSOR not found, aborting..."
  exit 1
fi

$BIN_DIR/example_htfrtc_fwd.exe << EOF
"${HTFRTC_COEF_STATIC}", HTFRTC static coef filename
"${HTFRTC_COEF_SENSOR}", HTFRTC sensor coef filename
${PROF_FILENAME}       , Input profile filename
${RADREC_FILENAME}     , Rad. rec. filename
${NPROF}               , Number of profiles
${NLEVELS}             , Number of levels
${MONTH}               , Month
${NPCSCORES}           , Number of PC scores
${NTHREADS}            , Number of threads
EOF

if [ $? -ne 0 ]; then
  echo " "
  echo "TEST FAILED"
  echo " "
  exit 1
fi

if [ $CHECK_REF -ne 0 ]; then

  OUT_FILE=output_example_htfrtc_fwd.dat
  DIFF_FILE=diff_example_htfrtc_fwd.${ARCH}

  mv ${OUT_FILE} ${OUT_FILE}.${ARCH}

  if [ $? -ne 0 ]; then
    echo "Expected output file not found"
    exit 1
  fi

  echo
  echo "Output is in the file ${TEST_DIR}/${OUT_FILE}.${ARCH}"

  if [ -f ${REF_TEST_DIR}/${OUT_FILE} ]; then

    diff -biw ${OUT_FILE}.${ARCH} ${REF_TEST_DIR}/${OUT_FILE} > $DIFF_FILE

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

fi

echo

cd $CWD

exit
