#!/bin/sh

# Test script to run rttov_test_model_alloc.F90

# This calls the executable with valgrind/memcheck to test for memory
# leaks: lots of leaks are reported if compiling with pgf90 which are
# not reported with other compilers. Currently I assume this is some
# "interaction" between valgrind and pgf90 and not indicative of any
# problems with RTTOV.

# There is no reference for this test: the exe performs checks on
# allocations and dimensions. If it reports no errors, the test was
# successful.

# Set BIN directory if supplied
BIN=$(perl -e 'for(@ARGV){m/^BIN=(\S+)$/o && print "$1";}' $*)
if [ "x$BIN" = "x" ]
then
  BIN=bin
fi

######## Edit this section for your test case input and pathnames ######

# Test case input data
COEF_FILENAME="rtcoef_noaa_19_avhrr_o3co2.dat"  # Location of this file is set below in $COEF_DIR
NPROF=5                                         # Number of profiles defined in prof.dat
NLEVELS=51                                      # Number of profile levels
NCHAN=3                                         # Number of channels per profile
NPCSCORES=400                                   # Number of PC scores
NCHANNELSREC=183                                # Number of reconstructed radiances
NAERNMOM=64                                     # Number of aerosol Legendre coefficients
NAERPHANGLE=200                                 # Number of aerosol phase angles
NCLDNMOM=128                                    # Number of cloud Legendre coefficients
NCLDPHANGLE=300                                 # Number of cloud phase angles

# The paths below do not need editing unless you move input files to different locations
# or you require a coefficient file from a different directory

# Paths relative to the rttov_test/ directory:
BIN_DIR=../$BIN                                 # BIN directory (may be set with BIN= argument)
COEF_DIR=../rtcoef_rttov13/rttov13pred54L       # Coefficients directory

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
echo " Testing RTTOV model allocations "
echo " "

echo  "Coef filename:              ${COEF_FILENAME}"
echo  "Number of profiles:         ${NPROF}"
echo  "Number of levels:           ${NLEVELS}"
echo  "Number of channels:         ${NCHAN}"
echo  "Number of PC scores:        ${NPCSCORES}"
echo  "Number of rec rads:         ${NCHANNELSREC}"
echo  "Number of aer Leg. coefs:   ${NAERNMOM}"
echo  "Number of aer phase angles: ${NAERPHANGLE}"
echo  "Number of cld Leg. coefs:   ${NCLDNMOM}"
echo  "Number of cld phase angles: ${NCLDPHANGLE}"


# Coefficient file
rm -f $COEF_FILENAME
if [ -s $COEF_DIR/$COEF_FILENAME ]; then
  ln -s $COEF_DIR/$COEF_FILENAME
else
  echo "Coef file $COEF_DIR/$COEF_FILENAME not found, aborting..."
  exit 1
fi

# --undef-value-errors=no prevents a lot of apparently spurious output from memcheck when using ifort
valgrind --leak-check=full --show-reachable=yes --undef-value-errors=no $BIN_DIR/rttov_test_model_alloc.exe << EOF
${COEF_FILENAME}, Coefficient filename
${NPROF}        , Number of profiles
${NLEVELS}      , Number of levels
${NCHAN}        , Number of channels
${NPCSCORES}    , Number of PC scores
${NCHANNELSREC} , Number of reconstructed radiances
${NAERNMOM}     , Number of aerosol Legendre coefficients
${NAERPHANGLE}  , Number of aerosol phase angles
${NCLDNMOM}     , Number of cloud Legendre coefficients
${NCLDPHANGLE}  , Number of cloud phase angles
EOF

rm -f $COEF_FILENAME

exit
