! Description:
!> @file
!!   Test code for rttov_get_sea_emis and rttov_get_sea_brdf routines.
!
!> @brief
!!   Test code for rttov_get_sea_emis and rttov_get_sea_brdf routines.
!!
!! @details
!!   Reads the same test profile data used by example_fwd.F90, and calls the
!!   two routines for different surface types and options, and compares the
!!   output to that from RTTOV. This is done for one MW and one VIS/IR sensor.
!!
!!   This is intended to be run by using the test_get_sea_emisbrdf.sh script.
!!
!!   As the test compares RTTOV output to that from the subroutines, there are
!!   no reference data.
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
!    Copyright 2019, EUMETSAT, All Rights Reserved.
!
PROGRAM rttov_test_get_sea_emis_brdf

#include "throw.h"

  USE rttov_types, ONLY :     &
         rttov_options,       &
         rttov_coefs,         &
         rttov_profile,       &
         rttov_transmission,  &
         rttov_radiance,      &
         rttov_chanprof,      &
         rttov_emissivity,    &
         rttov_reflectance

  USE parkind1, ONLY : jpim, jprb, jplm

  USE rttov_unix_env, ONLY : rttov_exit

  IMPLICIT NONE

#include "rttov_errorreport.interface"
#include "rttov_direct.interface"
#include "rttov_read_coefs.interface"
#include "rttov_dealloc_coefs.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_alloc_direct.interface"
#include "rttov_skipcommentline.interface"
#include "rttov_get_sea_emis.interface"
#include "rttov_get_sea_brdf.interface"

  TYPE(rttov_options)              :: opts
  TYPE(rttov_coefs)                :: coefs
  TYPE(rttov_chanprof),    POINTER :: chanprof(:)    => NULL()
  LOGICAL(jplm),           POINTER :: calcemis(:)    => NULL()
  TYPE(rttov_emissivity),  POINTER :: emissivity(:)  => NULL()
  LOGICAL(jplm),           POINTER :: calcrefl(:)    => NULL()
  TYPE(rttov_reflectance), POINTER :: reflectance(:) => NULL()
  TYPE(rttov_profile),     POINTER :: profiles(:)    => NULL()
  TYPE(rttov_transmission)         :: transmission
  TYPE(rttov_radiance)             :: radiance

  REAL(jprb),    PARAMETER :: tol = 1E-16_jprb
  INTEGER(jpim), PARAMETER :: iup = 20
  INTEGER(jpim) :: err
  CHARACTER(LEN=256) :: rtcoef_file_mw, rtcoef_file_visir, prof_filename
  INTEGER(jpim) :: nlevels, nprof, nchan, nchanprof
  INTEGER(jpim) :: iprof, isurf, icalcemis, icalcrefl, i
  INTEGER       :: ios
  LOGICAL(jplm) :: ok
  LOGICAL(jplm), ALLOCATABLE :: thermal(:), solar(:)
  !- End of header --------------------------------------------------------

  TRY

  ! --------------------------------------------------------------------------
  ! Initialisation
  ! --------------------------------------------------------------------------

  ok = .TRUE.

  WRITE(0,*) 'enter full path of MW rtcoef file'
  READ(*,*) rtcoef_file_mw
  WRITE(0,*) 'enter full path of VISIR rtcoef file'
  READ(*,*) rtcoef_file_visir
  WRITE(0,*) 'enter path of file containing profile data'
  READ(*,*) prof_filename
  WRITE(0,*) 'enter number of profile levels'
  READ(*,*) nlevels

  nprof = 1

  opts%interpolation%addinterp   = .TRUE.
  opts%rt_all%addrefrac          = .FALSE.
  opts%rt_all%ozone_data         = .FALSE.
  opts%config%verbose            = .TRUE.

  ALLOCATE(profiles(nprof))
  CALL rttov_alloc_prof(err, nprof, profiles, nlevels, opts, 1_jpim, init=.TRUE._jplm)
  THROW(err.NE.0)

  ! --------------------------------------------------------------------------
  ! Read profile data
  ! --------------------------------------------------------------------------

  OPEN(iup, file=TRIM(prof_filename), status='old', iostat=ios)
  err = ios
  THROW(err.NE.0)
  CALL rttov_skipcommentline(iup, err)

  ! Read gas units for profiles
  READ(iup,*) profiles(1)%gas_units
  profiles(:)%gas_units = profiles(1)%gas_units
  CALL rttov_skipcommentline(iup, err)

  ! Loop over all profiles and read data for each one
  DO iprof = 1, nprof

    READ(iup,*) profiles(iprof)%p(:)
    CALL rttov_skipcommentline(iup, err)
    READ(iup,*) profiles(iprof)%t(:)
    CALL rttov_skipcommentline(iup, err)
    READ(iup,*) profiles(iprof)%q(:)
    CALL rttov_skipcommentline(iup, err)
    ! Ozone profile is commented out in input profile data
!     READ(iup,*) profiles(iprof)%o3(:)
!     CALL rttov_skipcommentline(iup, errorstatus)

    READ(iup,*) profiles(iprof)%s2m%t, &
                profiles(iprof)%s2m%q, &
                profiles(iprof)%s2m%p, &
                profiles(iprof)%s2m%u, &
                profiles(iprof)%s2m%v, &
                profiles(iprof)%s2m%wfetc
    CALL rttov_skipcommentline(iup, err)

    READ(iup,*) profiles(iprof)%skin%t,        &
                profiles(iprof)%skin%salinity, & ! Salinity only applies to FASTEM over sea
                profiles(iprof)%skin%fastem      ! FASTEM only applies to MW instruments
    CALL rttov_skipcommentline(iup, err)

    READ(iup,*) profiles(iprof)%skin%surftype, &
                profiles(iprof)%skin%watertype
    CALL rttov_skipcommentline(iup, err)

    READ(iup,*) profiles(iprof)%elevation, &
                profiles(iprof)%latitude,  &
                profiles(iprof)%longitude
    CALL rttov_skipcommentline(iup, err)

    READ(iup,*) profiles(iprof)%zenangle,    &
                profiles(iprof)%azangle,     &
                profiles(iprof)%sunzenangle, &
                profiles(iprof)%sunazangle
    CALL rttov_skipcommentline(iup, err)

    READ(iup,*) profiles(iprof)%ctp, &
                profiles(iprof)%cfraction
    CALL rttov_skipcommentline(iup, err)

  ENDDO
  CLOSE(iup)

  profiles(:)%elevation = 0.   ! Required for exact match for BRDFs

  ! --------------------------------------------------------------------------
  ! Run tests
  ! --------------------------------------------------------------------------

  ! Init MW sensor

  CALL setup(err, TRIM(rtcoef_file_mw), .TRUE._jplm)
  THROW(err.NE.0)
  nchanprof = nprof * coefs%coef%fmv_chn

  ! Test for each surface type, for calcemis all F, all T, and TFTF...

  DO isurf = 0, 2
    DO icalcemis = 0, 2
      emissivity(:)%emis_in = 0.8_jprb + (/ (0.001_jprb * i, i = 1, nchanprof) /)
      CALL run_test(err, isurf, icalcemis, opts%rt_ir%addsolar)
      THROW(err.NE.0)
    ENDDO
  ENDDO

  ! Deallocate MW sensor

  CALL setup(err, TRIM(rtcoef_file_mw), .FALSE._jplm)
  THROW(err.NE.0)


  ! Init VIS/IR sensor

  CALL setup(err, TRIM(rtcoef_file_visir), .TRUE._jplm)
  THROW(err.NE.0)

  opts%rt_ir%addsolar = .TRUE.

  ! Test for each surface type, for calcemis all F, all T, and TFTF...,
  !   for calcrefl all F, all T, TFTF..., and FTFT...
  DO isurf = 0, 2
    DO icalcemis = 0, 2
      emissivity(:)%emis_in = 0.8_jprb + (/ (0.001_jprb * i, i = 1, nchanprof) /)
      DO icalcrefl= 0, 3
        reflectance(:)%refl_in = 0.03_jprb + (/ (0.001_jprb * i, i = 1, nchanprof) /)
        reflectance(:)%diffuse_refl_in = 0.04_jprb + (/ (0.001_jprb * i, i = 1, nchanprof) /)
        CALL run_test(err, isurf, icalcemis, opts%rt_ir%addsolar, icalcrefl)
        THROW(err.NE.0)
      ENDDO
    ENDDO
  ENDDO


  ! Deallocate VIS/IR sensor

  CALL setup(err, TRIM(rtcoef_file_visir), .FALSE._jplm)
  THROW(err.NE.0)


  ! --------------------------------------------------------------------------
  ! Tidy up
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_prof(err, nprof, profiles, nlevels, opts, 0_jpim)
  THROW(err.NE.0)
  DEALLOCATE(profiles)

  PRINT *, ''
  IF (ok) THEN
    PRINT *, 'TEST SUCCEEDED: all tests passed'
  ELSE
    PRINT *, 'TEST FAILED: differences occurred'
  ENDIF
  PRINT *, ''

  PCATCH
CONTAINS

  SUBROUTINE run_test(err, isurf, icalcemis, addsolar, icalcrefl)
    INTEGER(jpim),    INTENT(OUT)          :: err
    INTEGER(jpim),    INTENT(IN)           :: isurf
    INTEGER(jpim),    INTENT(IN)           :: icalcemis
    LOGICAL(jplm),    INTENT(IN)           :: addsolar
    INTEGER(jpim),    INTENT(IN), OPTIONAL :: icalcrefl

    REAL(jprb) :: emis(nchanprof), brdf(nchanprof), diffuse_refl(nchanprof)

    TRY
    emis(:) = 0._jprb
    brdf(:) = 0._jprb
    diffuse_refl(:) = 0._jprb

    profiles(:)%skin%surftype = isurf
    IF (icalcemis == 0) THEN        ! All F
      calcemis(:) = .FALSE.
    ELSE IF (icalcemis == 1) THEN   ! All T
      calcemis(:) = .TRUE.
    ELSE                            ! TFTF...
      calcemis(1::2) = .TRUE.
      calcemis(2::2) = .FALSE.
    ENDIF
    IF (addsolar) THEN
      IF (icalcrefl == 0) THEN      ! All F
        calcrefl(:) = .FALSE.
      ELSE IF (icalcrefl == 1) THEN ! All T
        calcrefl(:) = .TRUE.
      ELSE IF (icalcrefl == 2) THEN ! TFTF...
        calcrefl(1::2) = .TRUE.
        calcrefl(2::2) = .FALSE.
      ELSE                          ! FTFT...
        calcrefl(1::2) = .FALSE.
        calcrefl(2::2) = .TRUE.
      ENDIF
    ENDIF

    CALL rttov_direct(                &
            err,                      &
            chanprof,                 &
            opts,                     &
            profiles,                 &
            coefs,                    &
            transmission,             &
            radiance,                 &
            calcemis    = calcemis,   &
            emissivity  = emissivity, &
            calcrefl    = calcrefl,   &
            reflectance = reflectance) 
    THROW(err.NE.0)

    WHERE (thermal(:)) emis(:) = emissivity(:)%emis_in
    CALL rttov_get_sea_emis(err, opts, chanprof, profiles, coefs, calcemis, emis)
    THROW(err.NE.0)

    IF (ANY(ABS(emissivity(:)%emis_out - emis(:)) > tol)) THEN
      PRINT '(A,I2,A,I2)', 'Emissivity differences for isurf ', isurf, &
                            ' icalcemis ', icalcemis
      PRINT '(10F12.8)', emissivity(:)%emis_out
      PRINT '(10F12.8)', emis(:)
      PRINT *, ''
      ok = .FALSE.
    ENDIF

    IF (addsolar) THEN
      WHERE (solar(:))
        brdf(:) = reflectance(:)%refl_in
        diffuse_refl(:) = reflectance(:)%diffuse_refl_in
      END WHERE

      CALL rttov_get_sea_brdf(err, opts, chanprof, profiles, coefs, calcrefl, &
                              brdf, diffuse_refl, emis)
      IF (ANY(ABS(reflectance(:)%refl_out - brdf(:)) > tol)) THEN
        PRINT '(A,I2,3(A,L2))', 'BRDF differences for isurf ', isurf, &
                                ' icalcemis ', icalcemis, &
                                ' icalcrefl ', icalcrefl
        PRINT '(10F12.8)', reflectance(:)%refl_out
        PRINT '(10F12.8)', brdf(:)
        PRINT *, ''
        ok = .FALSE.
      ENDIF
      IF (ANY(ABS(reflectance(:)%diffuse_refl_out - diffuse_refl(:)) > tol .AND. &
          .NOT. thermal(:))) THEN
        ! Only want to compare outputs for pure-solar channels so for the purpose
        ! of output ensure thermal channel values are identical
        WHERE (thermal(:)) diffuse_refl(:) = reflectance(:)%diffuse_refl_out
        PRINT '(A,I2,3(A,L2))', 'Diffuse_refl differences for isurf ', isurf, &
                                ' icalcemis ', icalcemis, &
                                ' icalcrefl ', icalcrefl
        PRINT '(10F12.8)', reflectance(:)%diffuse_refl_out
        PRINT '(10F12.8)', diffuse_refl(:)
        PRINT *, ''
        ok = .FALSE.
      ENDIF
    ENDIF

    CATCH
  END SUBROUTINE run_test

  SUBROUTINE setup(err, coef_filename, alloc_switch)
    INTEGER(jpim),    INTENT(OUT) :: err
    CHARACTER(LEN=*), INTENT(IN)  :: coef_filename
    LOGICAL(jplm),    INTENT(IN)  :: alloc_switch

    INTEGER(jpim) :: j, ichan, iprof
    TRY

    IF (alloc_switch) THEN

      ! Read coefs
      CALL rttov_read_coefs(err, coefs, opts, file_coef=coef_filename)
      THROW(err.NE.0)
      nchan = coefs%coef%fmv_chn

      ! Allocate data
      nchanprof = nchan * nprof
      CALL rttov_alloc_direct( &
            err,                       &
            1_jpim,                    &
            nprof,                     &
            nchanprof,                 &
            nlevels,                   &
            chanprof,                  &
            opts,                      &
            coefs=coefs,               &
            transmission=transmission, &
            radiance=radiance,         &
            calcemis=calcemis,         &
            emissivity=emissivity,     &
            calcrefl=calcrefl,         &
            reflectance=reflectance,   &
            init=.TRUE._jplm)
      THROW(err.NE.0)
      reflectance(:)%refl_cloud_top = 0._jprb

      ALLOCATE(thermal(nchanprof), solar(nchanprof))

      ! Set chanprof
      DO iprof = 1, nprof
        DO ichan = 1, nchan
          j = (iprof - 1) * nchan + ichan
          chanprof(j)%prof = iprof
          chanprof(j)%chan = ichan
          thermal(j)    = (coefs%coef%ss_val_chn(ichan) < 2) .AND. &
                          (.NOT. coefs%coef%ff_val_chn(ichan) == 0)
          solar(j)      = (coefs%coef%ss_val_chn(ichan) > 0) .AND. &
                          (.NOT. coefs%coef%ff_val_chn(ichan) == 0)
        ENDDO
      ENDDO

    ELSE

      ! Deallocate data
      CALL rttov_alloc_direct( &
            err,                       &
            0_jpim,                    &
            nprof,                     &
            nchanprof,                 &
            nlevels,                   &
            chanprof,                  &
            opts,                      &
            coefs=coefs,               &
            transmission=transmission, &
            radiance=radiance,         &
            calcemis=calcemis,         &
            emissivity=emissivity,     &
            calcrefl=calcrefl,         &
            reflectance=reflectance)
      THROW(err.NE.0)

      DEALLOCATE(thermal, solar)

      ! Deallocate coefs
      CALL rttov_dealloc_coefs(err, coefs)
      THROW(err.NE.0)

    ENDIF
    CATCH
  END SUBROUTINE setup

END PROGRAM rttov_test_get_sea_emis_brdf
