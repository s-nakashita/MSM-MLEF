#!/bin/sh
# Script to run the example_pc_fwd example program
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
COEF_FILENAME="rtcoef_metop_2_iasi_pcrttov_compat.H5"        # Location of this file is set below in $COEF_DIR
PCCOEF_FILENAME="pccoef_metop_2_iasi_landsea_trace_nlte.H5"  # Location of this file is set below in $PCCOEF_DIR
PROF_FILENAME="prof_pc.dat"                                  # Input profile(s), usually found in $TEST_DIR set below
RADREC_FILENAME="radrec.dat"                                 # Reconstructed channel list, this file need not exist (see example_pc_fwd.F90)
NPROF=1                                                      # Number of profiles defined in prof_pc.dat
NLEVELS=51                                                   # Number of profile levels
IPCBND=1                                                     # PC band - AIRS: 1; IASI: 1-3 (see user guide)
IPCREG=1                                                     # PC regression set - 1-4
NPCSCORES=400                                                # Number of PC scores to calculate (1-400)

NTHREADS=1                                                   # Number of threads to use (compile RTTOV with OpenMP to exploit this)

CHECK_REF=1                                                  # Set to 0 to omit check against test reference


# The paths below do not need editing unless you move input files to different locations
# or you require a coefficient file from a different directory

# Path relative to the rttov_test directory:
TEST_DIR=./test_example.1

# Paths relative to the rttov_test/${TEST_DIR} directory:
BIN_DIR=../../$BIN                           # BIN directory (may be set with BIN= argument)
REF_TEST_DIR=../test_example.2               # Test reference data
COEF_DIR=../../rtcoef_rttov13/rttov9pred101L # Coefficients directory
PCCOEF_DIR=../../rtcoef_rttov13/pc           # PC coefficients directory

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
echo " Test PC forward "
echo " "

echo  "Coef filename:       ${COEF_FILENAME}"
echo  "PC coef filename:    ${PCCOEF_FILENAME}"
echo  "Input profile file:  ${PROF_FILENAME}"
echo  "Rad. rec. file:      ${RADREC_FILENAME}"
echo  "Number of profiles:  ${NPROF}"
echo  "Number of levels:    ${NLEVELS}"
echo  "PC band:             ${IPCBND}"
echo  "PC regression set:   ${IPCREG}"
echo  "Number of PC scores: ${NPCSCORES}"
echo  "Number of threads:   ${NTHREADS}"

# Coefficient files
COEF_FILENAME=$COEF_DIR/$COEF_FILENAME
if [ ! -f $COEF_FILENAME ]; then
  echo "Coef file $COEF_FILENAME not found, aborting..."
  exit 1
fi

PCCOEF_FILENAME=$PCCOEF_DIR/$PCCOEF_FILENAME
if [ ! -f $PCCOEF_FILENAME ]; then
  echo "Coef file $PCCOEF_FILENAME not found, aborting..."
  exit 1
fi

$BIN_DIR/example_pc_fwd.exe << EOF
"${COEF_FILENAME}"  , Coefficient filename
"${PCCOEF_FILENAME}", PC coefficient filename
${PROF_FILENAME}    , Input profile filename
${RADREC_FILENAME}  , Rad. rec. filename
${NPROF}            , Number of profiles
${NLEVELS}          , Number of levels
${IPCBND}           , PC band number
${IPCREG}           , PC regression set
${NPCSCORES}        , Number of PC scores
${NTHREADS}         , Number of threads
EOF

if [ $? -ne 0 ]; then
  echo " "
  echo "TEST FAILED"
  echo " "
  exit 1
fi

if [ $CHECK_REF -ne 0 ]; then

  OUT_FILE=output_example_pc_fwd.dat
  DIFF_FILE=diff_example_pc_fwd.${ARCH}

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
