! Description:
!> @file
!!   Executable for testing the rttov_calc_geo_sat_angles subroutine.
!
!> @brief
!!   Executable for testing the rttov_calc_geo_sat_angles subroutine.
!!
!! @details
!!   The test reference data are stored in the code: these were generated
!!   using RadSim which implements the same calculations and has already been
!!   validated. NB for the case at nadir, the azimuth reference must be set
!!   manually as this is not exactly nadir in RadSim due to precision.
!!
!!   Run the executable without arguments. It prints out success or failure.
!!
!
! Copyright:
!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 7 December 2016, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, DWD and MeteoFrance.
!
!    Copyright 2022, EUMETSAT, All Rights Reserved.
!
PROGRAM rttov_test_calc_geo_sat_angles

#include "throw.h"

  USE parkind1, ONLY : jpim, jprb
  USE rttov_types, ONLY : rttov_options, rttov_coefs, rttov_profile
  USE rttov_unix_env, ONLY : rttov_exit

  IMPLICIT NONE

#include "rttov_calc_geo_sat_angles.interface"
#include "rttov_errorreport.interface"

  INTEGER(jpim), PARAMETER :: nprofiles = 8     ! Number of test profiles
  INTEGER(jpim), PARAMETER :: ntestref = 2      ! Number of sets of reference data
  REAL(jprb),    PARAMETER :: tol = 1.E-4_jprb ! Tolerance for output comparisons

  ! Test inputs
  REAL(jprb)    :: lats(nprofiles)
  REAL(jprb)    :: lons(nprofiles)

  ! Values used when generating test reference data
  REAL(jprb)    :: geo_sat_lon = 60._jprb
  REAL(jprb)    :: geo_sat_lat = 0.5_jprb
  REAL(jprb)    :: geo_sat_height = 36000._jprb

  ! Test output reference data
  REAL(jprb)    :: ref_sat_zen(nprofiles,ntestref)
  REAL(jprb)    :: ref_sat_azi(nprofiles,ntestref)

  INTEGER(jpim)       :: err
  INTEGER(jpim)       :: itest
  TYPE(rttov_options) :: opts
  TYPE(rttov_coefs)   :: coefs
  TYPE(rttov_profile) :: profiles(nprofiles)

  ! Input data
  DATA lats /  00.00_jprb, &
               00.00_jprb, &
               00.00_jprb, &
               60.00_jprb, &
              -60.00_jprb, &
               45.00_jprb, &
              -45.00_jprb, &
              -31.42_jprb/
  DATA lons /  00.00_jprb, &
               60.00_jprb, & 
              120.00_jprb, & 
               60.00_jprb, & 
               60.00_jprb, & 
              105.00_jprb, &
              105.00_jprb, &
               87.70_jprb /

  ! Output data
  DATA ref_sat_zen / &
    ! Test 1
    68.05533_jprb, &
    00.00000_jprb, &
    68.05533_jprb, &
    68.05533_jprb, &
    68.05533_jprb, &
    68.05533_jprb, &
    68.05533_jprb, &
    47.29953_jprb, &

    ! Test 2
    68.01608_jprb,  &
    0.5884843_jprb, &
    68.01608_jprb,  &
    67.48425_jprb,  &
    68.54452_jprb,  &
    67.58210_jprb,  &
    68.44780_jprb,  &
    47.71009_jprb /

  DATA ref_sat_azi / &
    ! Test 1
     90.0000_jprb, &
      0.0000_jprb, &
    270.0000_jprb, &
    180.0000_jprb, &
    360.0000_jprb, &
    234.7356_jprb, &
    305.2644_jprb, &
    314.7972_jprb, &

    ! Test 2
     90.0000_jprb, &
      0.0000_jprb, &
    270.0000_jprb, &
    180.0000_jprb, &
    360.0000_jprb, &
    234.7356_jprb, &
    305.2644_jprb, &
    314.7972_jprb /

  TRY

  ! Don't need to load full coefs, just set the predictors version
  coefs%coef%fmv_model_ver = 13

  ! Populate profiles
  profiles(:)%latitude  = lats(:)
  profiles(:)%longitude = lons(:)

  ! -----------------------------------
  ! Test 1
  ! -----------------------------------

  ! Test with default sat height and latitude

  itest = 1

  ! Run test
  CALL rttov_calc_geo_sat_angles(err, opts, coefs, profiles, geo_sat_lon)
  THROWM(err.NE.0, "TEST FAILED: test 1, error calculating GEO sat angles")

  ! Validate results
  IF (ANY(ABS(profiles(:)%zenangle - ref_sat_zen(:,itest)) > tol)) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, "TEST FAILED: test 1, some satellite zenith angles outside tolerance")
  ENDIF
  IF (ANY(ABS(profiles(:)%azangle - ref_sat_azi(:,itest)) > tol)) THEN
    PRINT *,profiles(:)%azangle - ref_sat_azi(:,itest)
    err = errorstatus_fatal
    THROWM(err.NE.0, "TEST FAILED: test 1, some satellite azimuth angles outside tolerance")
  ENDIF


  ! -----------------------------------
  ! Test 2
  ! -----------------------------------

  ! Test with modified sat height and latitude

  itest = 2

  ! Run test
  CALL rttov_calc_geo_sat_angles(err, opts, coefs, profiles, geo_sat_lon, &
                                 geo_sat_lat, geo_sat_height)
  THROWM(err.NE.0, "TEST FAILED: test 2, error calculating GEO sat angles")

  ! Validate results
  IF (ANY(ABS(profiles(:)%zenangle - ref_sat_zen(:,itest)) > tol)) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, "TEST FAILED: test 2, some satellite zenith angles outside tolerance")
  ENDIF
  IF (ANY(ABS(profiles(:)%azangle - ref_sat_azi(:,itest)) > tol)) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, "TEST FAILED: test 2, some satellite azimuth angles outside tolerance")
  ENDIF


  ! -----------------------------------
  ! Test 3
  ! -----------------------------------

  ! Check non-zero err returned when max sat_zen exceeded
  ! NB the routine will print out an error message - this is OK

  profiles(1)%latitude = 89._jprb
  profiles(1)%longitude = 0._jprb
  geo_sat_lon = 0._jprb

  CALL rttov_calc_geo_sat_angles(err, opts, coefs, profiles(1:1), geo_sat_lon)
  THROWM(err==0, "TEST FAILED: test 3, max zenith angle exceeded without error")
  PRINT *, 'NB the above error message is expected'
  err = errorstatus_success


  ! If we reached this point the test passed
  PRINT *, ''
  PRINT *, 'TEST SUCCEEDED: all tests passed'
  PRINT *, ''

  PCATCH
END PROGRAM rttov_test_calc_geo_sat_angles