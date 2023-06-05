! Description:
!> @file
!!   Executable for testing the rttov_calc_solar_angles subroutine.
!
!> @brief
!!   Executable for testing the rttov_calc_solar_angles subroutine.
!!
!! @details
!!   The test reference data are stored in the code: these were generated
!!   using RadSim which implements the same calculations and has already been
!!   validated. NB The RadSim calculation ignores seconds, so seconds is set
!!   to zero in all test cases, but the RTTOV implementation accounts for
!!   non-zero seconds values.
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
PROGRAM rttov_test_calc_solar_angles

#include "throw.h"

  USE parkind1, ONLY : jpim, jprb
  USE rttov_types, ONLY : rttov_profile
  USE rttov_unix_env, ONLY : rttov_exit

  IMPLICIT NONE

#include "rttov_calc_solar_angles.interface"
#include "rttov_errorreport.interface"

  INTEGER(jpim), PARAMETER :: nprofiles = 9    ! Number of test profiles
  REAL(jprb),    PARAMETER :: tol = 1.E-4_jprb ! Tolerance for output comparisons

  ! Test inputs
  INTEGER(jpim) :: dates(3,nprofiles)
  INTEGER(jpim) :: times(3,nprofiles)
  REAL(jprb)    :: lats(nprofiles)
  REAL(jprb)    :: lons(nprofiles)

  ! Test output reference data
  REAL(jprb)    :: ref_sun_zen(nprofiles)
  REAL(jprb)    :: ref_sun_azi(nprofiles)

  INTEGER(jpim)       :: err
  INTEGER(jpim)       :: prof
  TYPE(rttov_profile) :: profiles(nprofiles)

  ! Input data
  DATA dates / 2020_jpim, 12_jpim, 01_jpim, &
               2021_jpim, 06_jpim, 01_jpim, &
               1979_jpim, 02_jpim, 17_jpim, &
               1979_jpim, 02_jpim, 17_jpim, &
               2004_jpim, 02_jpim, 28_jpim, &
               2004_jpim, 02_jpim, 28_jpim, &
               2004_jpim, 02_jpim, 29_jpim, &
               2022_jpim, 06_jpim, 20_jpim, &
               2022_jpim, 12_jpim, 20_jpim /
  DATA times / 12_jpim, 11_jpim, 0_jpim, & 
               12_jpim, 11_jpim, 0_jpim, & 
               12_jpim, 00_jpim, 0_jpim, &
               12_jpim, 00_jpim, 0_jpim, &
               00_jpim, 00_jpim, 0_jpim, &
               10_jpim, 45_jpim, 0_jpim, &
               15_jpim, 17_jpim, 0_jpim, &
               09_jpim, 25_jpim, 0_jpim, &
               09_jpim, 25_jpim, 0_jpim /
  DATA lats /  45.00_jprb, &
               45.00_jprb, &
               85.00_jprb, &
              -85.00_jprb, &
               00.00_jprb, &
               00.00_jprb, &
               17.00_jprb, &
               90.00_jprb, &
              -90.00_jprb /
  DATA lons /  00.00_jprb, &
               00.00_jprb, &
              160.00_jprb, &
              160.00_jprb, &
               00.00_jprb, &
               00.00_jprb, &
              -56.00_jprb, &
              111.00_jprb, &
              222.00_jprb /

  ! Output data
  DATA ref_sun_zen / 67.09678_jprb, & 
                     23.03531_jprb, & 
                     106.6657_jprb, & 
                     82.50076_jprb, & 
                     171.1168_jprb, & 
                     23.30623_jprb, & 
                     26.53364_jprb, &
                     66.56541_jprb, &
                     66.5707_jprb /
  DATA ref_sun_azi / 185.4856_jprb, &
                     187.7778_jprb, &
                     335.9534_jprb, &
                     203.1868_jprb, &
                     200.8783_jprb, &
                     110.9346_jprb, &
                     157.6685_jprb, &
                     251.8582_jprb, &
                     176.1307_jprb /

  TRY

  ! Populate profiles
  DO prof = 1, nprofiles
    profiles(prof)%date(:)   = dates(:,prof)
    profiles(prof)%time(:)   = times(:,prof)
    profiles(prof)%latitude  = lats(prof)
    profiles(prof)%longitude = lons(prof)
  ENDDO

  ! Run test
  CALL rttov_calc_solar_angles(err, profiles)
  THROWM(err.NE.0, "TEST FAILED: error calculating solar angles")

  ! Validate results
  IF (ANY(ABS(profiles(:)%sunzenangle - ref_sun_zen(:)) > tol)) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, "TEST FAILED: some solar zenith angles outside tolerance")
  ENDIF
  IF (ANY(ABS(profiles(:)%sunazangle - ref_sun_azi(:)) > tol)) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, "TEST FAILED: some solar azimuth angles outside tolerance")
  ENDIF

  ! If we reached this point the test passed
  PRINT *, ''
  PRINT *, 'TEST SUCCEEDED: all tests passed'
  PRINT *, ''

  PCATCH

END PROGRAM rttov_test_calc_solar_angles
