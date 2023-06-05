PROGRAM rttov_test_model_alloc

! Description:
!   Tests the rttov_direct/tl/ad/k_alloc subroutines.
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
!    Copyright 2014, EUMETSAT, All Rights Reserved.

#include "throw.h"

  USE parkind1, ONLY : jprb, jpim, jplm

  USE rttov_types, ONLY : &
      rttov_options,       &
      rttov_coefs,         &
      rttov_scatt_coef,    &
      rttov_chanprof,      &
      rttov_emissivity,    &
      rttov_reflectance,   &
      rttov_profile,       &
      rttov_profile_cloud, &
      rttov_transmission,  &
      rttov_radiance,      &
      rttov_radiance2,     &
      rttov_reflectivity,  &
      rttov_traj,          &
      rttov_pccomp,        &
      rttov_opt_param,     &
      rttov_scatt_emis_retrieval_type

  USE rttov_unix_env, ONLY : rttov_exit

  IMPLICIT NONE

  INTEGER(jpim)                      :: nprofiles
  INTEGER(jpim)                      :: nchannels
  INTEGER(jpim)                      :: nlevels
  INTEGER(jpim)                      :: nchanprof
  INTEGER(jpim)                      :: npcscores
  INTEGER(jpim)                      :: nchannels_rec
  INTEGER(jpim)                      :: aer_nphangle, aer_maxnmom
  INTEGER(jpim)                      :: cld_nphangle, cld_maxnmom

  INTEGER(jpim)                      :: err

  TYPE(rttov_options)                :: opts
  TYPE(rttov_coefs)                  :: coefs

  TYPE(rttov_chanprof),      POINTER :: chanprof(:)        => NULL()
  LOGICAL(jplm),             POINTER :: calcemis(:)        => NULL()
  TYPE(rttov_emissivity),    POINTER :: emissivity(:)      => NULL()
  TYPE(rttov_emissivity),    POINTER :: emissivity_inc(:)  => NULL()
  LOGICAL(jplm),             POINTER :: calcrefl(:)        => NULL()
  TYPE(rttov_reflectance),   POINTER :: reflectance(:)     => NULL()
  TYPE(rttov_reflectance),   POINTER :: reflectance_inc(:) => NULL()

  TYPE(rttov_profile),       POINTER :: profiles(:)        => NULL()
  TYPE(rttov_profile),       POINTER :: profiles_inc(:)    => NULL()
  TYPE(rttov_transmission)           :: transmission
  TYPE(rttov_transmission)           :: transmission_inc
  TYPE(rttov_radiance)               :: radiance
  TYPE(rttov_radiance)               :: radiance_inc
  TYPE(rttov_radiance2)              :: radiance2

  TYPE(rttov_traj)                   :: traj
  TYPE(rttov_traj)                   :: traj_inc

  TYPE(rttov_pccomp)                 :: pccomp
  TYPE(rttov_pccomp)                 :: pccomp_inc
  INTEGER(jpim),             POINTER :: channels_rec(:)    => NULL()
  TYPE(rttov_profile),       POINTER :: profiles_k_pc(:)   => NULL()
  TYPE(rttov_profile),       POINTER :: profiles_k_rec(:)  => NULL()

  TYPE(rttov_opt_param)              :: aer_opt_param, aer_opt_param_inc
  TYPE(rttov_opt_param)              :: cld_opt_param, cld_opt_param_inc

  CHARACTER(LEN=256)                 :: coef_filename

  INTEGER(jpim),             POINTER    :: frequencies(:)      => NULL()
  TYPE(rttov_scatt_coef)                :: coef_scatt
  INTEGER(jpim)                         :: nhydro_frac
  TYPE(rttov_profile_cloud), POINTER    :: cld_profiles(:)     => NULL()
  TYPE(rttov_profile_cloud), POINTER    :: cld_profiles_inc(:) => NULL()
  REAL(jprb),                POINTER    :: scatt_cfrac(:)      => NULL()
  TYPE(rttov_scatt_emis_retrieval_type) :: emis_ret_terms
  TYPE(rttov_reflectivity)              :: reflectivity
  TYPE(rttov_reflectivity)              :: reflectivity_inc

#include "rttov_errorreport.interface"
#include "rttov_read_coefs.interface"
#include "rttov_dealloc_coefs.interface"
#include "rttov_alloc_direct.interface"
#include "rttov_alloc_tl.interface"
#include "rttov_alloc_ad.interface"
#include "rttov_alloc_k.interface"

  TRY

  ! -------------------------------------------
  ! Read inputs
  ! -------------------------------------------
  WRITE(0,*) 'enter name of coefficient file (in current directory)'
  READ(*,*) coef_filename
  WRITE(0,*) 'enter number of profiles'
  READ(*,*) nprofiles
  WRITE(0,*) 'enter number of profile levels'
  READ(*,*) nlevels
  WRITE(0,*) 'enter number of channels per profile'
  READ(*,*) nchannels
  WRITE(0,*) 'enter number of PC scores'
  READ(*,*) npcscores
  WRITE(0,*) 'enter number of PC reconstructed radiances'
  READ(*,*) nchannels_rec
  WRITE(0,*) 'enter number of aerosol Legendre coefficients'
  READ(*,*) aer_maxnmom
  WRITE(0,*) 'enter number of aerosol phase angles'
  READ(*,*) aer_nphangle
  WRITE(0,*) 'enter number of cloud Legendre coefficients'
  READ(*,*) cld_maxnmom
  WRITE(0,*) 'enter number of cloud phase angles'
  READ(*,*) cld_nphangle

  nchanprof = nprofiles * nchannels


  ! -------------------------------------------
  ! Read coefs - only because we need a coef file for some allocation routines
  ! -------------------------------------------
  CALL rttov_read_coefs(err, coefs, opts, file_coef=TRIM(coef_filename))
  THROWM(err.NE.0, 'Error reading coefs')

  ! For RTTOV-SCATT coefficients we just need to set the nhydro variable
  coef_scatt%nhydro = 5

  nhydro_frac = 1  ! Doesn't matter what the number of cfrac profiles is


  ! -------------------------------------------
  ! Set options
  ! -------------------------------------------

  ! These are not mutually compatible for running RTTOV,
  ! but the important thing is to test all allocations
  ! Do this *after* reading coefs to avoid having to read cld/aer/pc files

  opts%rt_ir%pc%addpc = .TRUE.
  opts%rt_ir%pc%addradrec = .TRUE.

  opts%rt_ir%addsolar = .TRUE.

  opts%rt_ir%addaerosl = .TRUE.
  opts%rt_ir%addclouds = .TRUE.
  opts%rt_ir%user_aer_opt_param = .TRUE.
  opts%rt_ir%user_cld_opt_param = .TRUE.

  INFO(' ')

  INFO('Checking direct (de)allocation')

  ! --------------------------------------------------------------------------
  ! Check direct allocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_direct( &
              err,             &
              1_jpim,          &
              nprofiles,       &
              nchanprof,       &
              nlevels,         &
              chanprof,        &
              opts,            &
              profiles,        &
              coefs,           &
              transmission,    &
              radiance,        &
              radiance2,       &
              calcemis,        &
              emissivity,      &
              calcrefl,        &
              reflectance,     &
              aer_maxnmom,     &
              aer_nphangle,    &
              aer_opt_param,   &
              cld_maxnmom,     &
              cld_nphangle,    &
              cld_opt_param,   &
              traj,            &
              npcscores,       &
              nchannels_rec,   &
              pccomp,          &
              channels_rec,    &
              frequencies,     &
              coef_scatt,      &
              nhydro_frac,     &
              cld_profiles,    &
              scatt_cfrac,     &
              emis_ret_terms,  &
              reflectivity)
  THROW(err.NE.0)

  CALL check_direct_only_alloc(err)
  THROWM(err.NE.0, 'Direct model allocation error')

  CALL check_direct_alloc(err)
  THROWM(err.NE.0, 'Direct model allocation error')

  CALL check_model_alloc( &
        err,           &
        nprofiles,     &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'Direct model allocation error')

  ! --------------------------------------------------------------------------
  ! Check direct deallocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_direct( &
              err,             &
              0_jpim,          &
              nprofiles,       &
              nchanprof,       &
              nlevels,         &
              chanprof,        &
              opts,            &
              profiles,        &
              coefs,           &
              transmission,    &
              radiance,        &
              radiance2,       &
              calcemis,        &
              emissivity,      &
              calcrefl,        &
              reflectance,     &
              aer_maxnmom,     &
              aer_nphangle,    &
              aer_opt_param,   &
              cld_maxnmom,     &
              cld_nphangle,    &
              cld_opt_param,   &
              traj,            &
              npcscores,       &
              nchannels_rec,   &
              pccomp,          &
              channels_rec,    &
              frequencies,     &
              coef_scatt,      &
              nhydro_frac,     &
              cld_profiles,    &
              scatt_cfrac,     &
              emis_ret_terms,  &
              reflectivity)
  THROW(err.NE.0)

  CALL check_direct_only_dealloc(err)
  THROWM(err.NE.0, 'Direct-model-only deallocation error')

  CALL check_direct_dealloc(err)
  THROWM(err.NE.0, 'Direct model deallocation error')

  CALL check_model_dealloc( &
        err,           &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'Direct model deallocation error')


  INFO('Checking TL (de)allocation')

  ! --------------------------------------------------------------------------
  ! Check TL allocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_tl( &
              err,                &
              1_jpim,             &
              nprofiles,          &
              nchanprof,          &
              nlevels,            &
              chanprof,           &
              opts,               &
              profiles,           &
              profiles_inc,       &
              coefs,              &
              transmission,       &
              transmission_inc,   &
              radiance,           &
              radiance_inc,       &
              radiance2,          &
              calcemis,           &
              emissivity,         &
              emissivity_inc,     &
              calcrefl,           &
              reflectance,        &
              reflectance_inc,    &
              aer_maxnmom,        &
              aer_nphangle,       &
              aer_opt_param,      &
              aer_opt_param_inc,  &
              cld_maxnmom,        &
              cld_nphangle,       &
              cld_opt_param,      &
              cld_opt_param_inc,  &
              traj,               &
              traj_inc,           &
              npcscores,          &
              nchannels_rec,      &
              pccomp,             &
              pccomp_inc,         &
              channels_rec,       &
              frequencies,        &
              coef_scatt,         &
              nhydro_frac,        &
              cld_profiles,       &
              cld_profiles_inc,   &
              reflectivity,       &
              reflectivity_inc)
  THROW(err.NE.0)

  CALL check_direct_alloc(err)
  THROWM(err.NE.0, 'TL model allocation error in direct-only variables')

  CALL check_model_alloc( &
        err,           &
        nprofiles,     &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'TL model allocation error in direct variables')

  CALL check_model_alloc( &
        err,               &
        nprofiles,         &
        emissivity_inc,    &
        reflectance_inc,   &
        profiles_inc,      &
        radiance_inc,      &
        transmission_inc,  &
        aer_opt_param_inc, &
        cld_opt_param_inc, &
        traj_inc,          &
        pccomp_inc,        &
        cld_profiles_inc,  &
        reflectivity_inc)
  THROWM(err.NE.0, 'TL model allocation error in TL variables')

  ! --------------------------------------------------------------------------
  ! Check TL deallocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_tl( &
              err,                &
              0_jpim,             &
              nprofiles,          &
              nchanprof,          &
              nlevels,            &
              chanprof,           &
              opts,               &
              profiles,           &
              profiles_inc,       &
              coefs,              &
              transmission,       &
              transmission_inc,   &
              radiance,           &
              radiance_inc,       &
              radiance2,          &
              calcemis,           &
              emissivity,         &
              emissivity_inc,     &
              calcrefl,           &
              reflectance,        &
              reflectance_inc,    &
              aer_maxnmom,        &
              aer_nphangle,       &
              aer_opt_param,      &
              aer_opt_param_inc,  &
              cld_maxnmom,        &
              cld_nphangle,       &
              cld_opt_param,      &
              cld_opt_param_inc,  &
              traj,               &
              traj_inc,           &
              npcscores,          &
              nchannels_rec,      &
              pccomp,             &
              pccomp_inc,         &
              channels_rec,       &
              frequencies,        &
              coef_scatt,         &
              nhydro_frac,        &
              cld_profiles,       &
              cld_profiles_inc,   &
              reflectivity,       &
              reflectivity_inc)
  THROW(err.NE.0)

  CALL check_direct_dealloc(err)
  THROWM(err.NE.0, 'TL model deallocation error in direct-only variables')

  CALL check_model_dealloc( &
        err,           &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'TL model deallocation error in direct variables')

  CALL check_model_dealloc( &
        err,               &
        emissivity_inc,    &
        reflectance_inc,   &
        profiles_inc,      &
        radiance_inc,      &
        transmission_inc,  &
        aer_opt_param_inc, &
        cld_opt_param_inc, &
        traj_inc,          &
        pccomp_inc,        &
        cld_profiles_inc,  &
        reflectivity_inc)
  THROWM(err.NE.0, 'TL model deallocation error in TL variables')


  INFO('Checking AD (de)allocation')

  ! --------------------------------------------------------------------------
  ! Check AD allocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_ad( &
              err,                &
              1_jpim,             &
              nprofiles,          &
              nchanprof,          &
              nlevels,            &
              chanprof,           &
              opts,               &
              profiles,           &
              profiles_inc,       &
              coefs,              &
              transmission,       &
              transmission_inc,   &
              radiance,           &
              radiance_inc,       &
              radiance2,          &
              calcemis,           &
              emissivity,         &
              emissivity_inc,     &
              calcrefl,           &
              reflectance,        &
              reflectance_inc,    &
              aer_maxnmom,        &
              aer_nphangle,       &
              aer_opt_param,      &
              aer_opt_param_inc,  &
              cld_maxnmom,        &
              cld_nphangle,       &
              cld_opt_param,      &
              cld_opt_param_inc,  &
              traj,               &
              traj_inc,           &
              npcscores,          &
              nchannels_rec,      &
              pccomp,             &
              pccomp_inc,         &
              channels_rec,       &
              frequencies,        &
              coef_scatt,         &
              nhydro_frac,        &
              cld_profiles,       &
              cld_profiles_inc,   &
              reflectivity,       &
              reflectivity_inc)
  THROW(err.NE.0)

  CALL check_direct_alloc(err)
  THROWM(err.NE.0, 'AD model allocation error in direct-only variables')

  CALL check_model_alloc( &
        err,           &
        nprofiles,     &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'AD model allocation error in direct variables')

  CALL check_model_alloc( &
        err,               &
        nprofiles,         &
        emissivity_inc,    &
        reflectance_inc,   &
        profiles_inc,      &
        radiance_inc,      &
        transmission_inc,  &
        aer_opt_param_inc, &
        cld_opt_param_inc, &
        traj_inc,          &
        pccomp_inc,        &
        cld_profiles_inc,  &
        reflectivity_inc)
  THROWM(err.NE.0, 'AD model allocation error in AD variables')

  ! --------------------------------------------------------------------------
  ! Check AD deallocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_ad( &
              err,                &
              0_jpim,             &
              nprofiles,          &
              nchanprof,          &
              nlevels,            &
              chanprof,           &
              opts,               &
              profiles,           &
              profiles_inc,       &
              coefs,              &
              transmission,       &
              transmission_inc,   &
              radiance,           &
              radiance_inc,       &
              radiance2,          &
              calcemis,           &
              emissivity,         &
              emissivity_inc,     &
              calcrefl,           &
              reflectance,        &
              reflectance_inc,    &
              aer_maxnmom,        &
              aer_nphangle,       &
              aer_opt_param,      &
              aer_opt_param_inc,  &
              cld_maxnmom,        &
              cld_nphangle,       &
              cld_opt_param,      &
              cld_opt_param_inc,  &
              traj,               &
              traj_inc,           &
              npcscores,          &
              nchannels_rec,      &
              pccomp,             &
              pccomp_inc,         &
              channels_rec,       &
              frequencies,        &
              coef_scatt,         &
              nhydro_frac,        &
              cld_profiles,       &
              cld_profiles_inc,   &
              reflectivity,       &
              reflectivity_inc)
  THROW(err.NE.0)

  CALL check_direct_dealloc(err)
  THROWM(err.NE.0, 'AD model deallocation error in direct-only variables')

  CALL check_model_dealloc( &
        err,           &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'AD model deallocation error in direct variables')

  CALL check_model_dealloc( &
        err,               &
        emissivity_inc,    &
        reflectance_inc,   &
        profiles_inc,      &
        radiance_inc,      &
        transmission_inc,  &
        aer_opt_param_inc, &
        cld_opt_param_inc, &
        traj_inc,          &
        pccomp_inc,        &
        cld_profiles_inc,  &
        reflectivity_inc)
  THROWM(err.NE.0, 'AD model deallocation error in AD variables')


  INFO('Checking K (de)allocation')

  ! --------------------------------------------------------------------------
  ! Check K allocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_k( &
              err,                &
              1_jpim,             &
              nprofiles,          &
              nchanprof,          &
              nlevels,            &
              chanprof,           &
              opts,               &
              profiles,           &
              profiles_inc,       &
              coefs,              &
              transmission,       &
              transmission_inc,   &
              radiance,           &
              radiance_inc,       &
              radiance2,          &
              calcemis,           &
              emissivity,         &
              emissivity_inc,     &
              calcrefl,           &
              reflectance,        &
              reflectance_inc,    &
              aer_maxnmom,        &
              aer_nphangle,       &
              aer_opt_param,      &
              aer_opt_param_inc,  &
              cld_maxnmom,        &
              cld_nphangle,       &
              cld_opt_param,      &
              cld_opt_param_inc,  &
              traj,               &
              traj_inc,           &
              npcscores,          &
              nchannels_rec,      &
              pccomp,             &
              pccomp_inc,         &
              profiles_k_pc,      &
              profiles_k_rec,     &
              channels_rec,       &
              frequencies,        &
              coef_scatt,         &
              nhydro_frac,        &
              cld_profiles,       &
              cld_profiles_inc,   &
              reflectivity,       &
              reflectivity_inc)
  THROW(err.NE.0)

  CALL check_direct_alloc(err)
  THROWM(err.NE.0, 'K model allocation error in direct-only variables')

  CALL check_model_alloc( &
        err,           &
        nprofiles,     &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'K model allocation error in direct variables')

  CALL check_model_alloc( &
        err,              &
        nchanprof,        &  ! nprofiles => nchanprof for K variables
        emissivity_inc,    &
        reflectance_inc,   &
        profiles_inc,      &
        radiance_inc,      &
        transmission_inc,  &
        aer_opt_param_inc, &
        cld_opt_param_inc, &
        traj_inc,          &
        pccomp_inc,        &
        cld_profiles_inc,  &
        reflectivity_inc)
  THROWM(err.NE.0, 'K model allocation error in K variables')

  CALL check_pc_k_alloc( &
        err,                   &
        npcscores * nprofiles, &
        profiles_k_pc)
  THROWM(err.NE.0, 'PC-RTTOV K model allocation error in profiles_k_pc')

  CALL check_pc_k_alloc( &
        err,                       &
        nchannels_rec * nprofiles, &
        profiles_k_rec)
  THROWM(err.NE.0, 'PC-RTTOV K model allocation error in profiles_k_rec')


  ! --------------------------------------------------------------------------
  ! Check K deallocation
  ! --------------------------------------------------------------------------

  CALL rttov_alloc_k( &
              err,                &
              0_jpim,             &
              nprofiles,          &
              nchanprof,          &
              nlevels,            &
              chanprof,           &
              opts,               &
              profiles,           &
              profiles_inc,       &
              coefs,              &
              transmission,       &
              transmission_inc,   &
              radiance,           &
              radiance_inc,       &
              radiance2,          &
              calcemis,           &
              emissivity,         &
              emissivity_inc,     &
              calcrefl,           &
              reflectance,        &
              reflectance_inc,    &
              aer_maxnmom,        &
              aer_nphangle,       &
              aer_opt_param,      &
              aer_opt_param_inc,  &
              cld_maxnmom,        &
              cld_nphangle,       &
              cld_opt_param,      &
              cld_opt_param_inc,  &
              traj,               &
              traj_inc,           &
              npcscores,          &
              nchannels_rec,      &
              pccomp,             &
              pccomp_inc,         &
              profiles_k_pc,      &
              profiles_k_rec,     &
              channels_rec,       &
              frequencies,        &
              coef_scatt,         &
              nhydro_frac,        &
              cld_profiles,       &
              cld_profiles_inc,   &
              reflectivity,       &
              reflectivity_inc)
  THROW(err.NE.0)

  CALL check_direct_dealloc(err)
  THROWM(err.NE.0, 'K model deallocation error in direct-only variables')

  CALL check_model_dealloc( &
        err,           &
        emissivity,    &
        reflectance,   &
        profiles,      &
        radiance,      &
        transmission,  &
        aer_opt_param, &
        cld_opt_param, &
        traj,          &
        pccomp,        &
        cld_profiles,  &
        reflectivity)
  THROWM(err.NE.0, 'K model deallocation error in direct variables')

  CALL check_model_dealloc( &
        err,               &
        emissivity_inc,    &
        reflectance_inc,   &
        profiles_inc,      &
        radiance_inc,      &
        transmission_inc,  &
        aer_opt_param_inc, &
        cld_opt_param_inc, &
        traj_inc,          &
        pccomp_inc,        &
        cld_profiles_inc,  &
        reflectivity_inc,  &
        profiles_k_pc,     &
        profiles_k_rec)
  THROWM(err.NE.0, 'K model deallocation error in K variables')


  ! -------------------------------------------
  ! Tidy up
  ! -------------------------------------------
  CALL rttov_dealloc_coefs(err, coefs)
  THROWM(err.NE.0, 'Error deallocating coefs')

  INFO(' ')
  INFO('TEST SUCCESSFUL (you should still use valgrind to check for memory leaks)')
  INFO(' ')

  PCATCH

CONTAINS

  SUBROUTINE check_direct_only_alloc(err)
    ! Check variables *only* allocated by rttov_alloc_direct have been allocated correctly.
    INTEGER(jpim),           INTENT(INOUT) :: err

    TRY

    ! -------------------------------------------
    ! Check array allocations
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(scatt_cfrac)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Direct-only array not associated')
    ENDIF

    IF (SIZE(scatt_cfrac) /= nprofiles) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Direct-only array has wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check emis_ret_terms structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(emis_ret_terms%cfrac)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Emissivity retrieval structure arrays not allocated')
    ENDIF

    IF (SIZE(emis_ret_terms%cfrac) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Emissivity retrieval structure arrays have wrong size')
    ENDIF

    CATCH
  END SUBROUTINE check_direct_only_alloc


  SUBROUTINE check_direct_only_dealloc(err)
    ! Check variables *only* allocated by rttov_alloc_direct have been deallocated correctly.
    INTEGER(jpim),           INTENT(INOUT) :: err

    TRY

    ! -------------------------------------------
    ! Check array deallocations
    ! -------------------------------------------
    IF (ASSOCIATED(scatt_cfrac)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Direct-only array not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check emis_ret_terms structure
    ! -------------------------------------------
    IF (ASSOCIATED(emis_ret_terms%cfrac)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Emissivity retrieval structure arrays not allocated')
    ENDIF

    CATCH
  END SUBROUTINE check_direct_only_dealloc


  SUBROUTINE check_direct_alloc(err)
    ! Check direct-only variables have been allocated correctly
    ! These are arrays that are allocated by all four routines, but which only
    ! have direct model instances, and the radiance2 structure
    INTEGER(jpim),           INTENT(INOUT) :: err

    TRY

    ! -------------------------------------------
    ! Check array allocations
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(chanprof)     .OR. &
        .NOT. ASSOCIATED(calcemis)     .OR. &
        .NOT. ASSOCIATED(calcrefl)     .OR. &
        .NOT. ASSOCIATED(channels_rec) .OR. &
        .NOT. ASSOCIATED(frequencies)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Direct-only array not associated')
    ENDIF

    IF (SIZE(chanprof)     /= nchanprof     .OR. &
        SIZE(calcemis)     /= nchanprof     .OR. &
        SIZE(calcrefl)     /= nchanprof     .OR. &
        SIZE(channels_rec) /= nchannels_rec .OR. &
        SIZE(frequencies)  /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Direct-only array has wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check radiance2 structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(radiance2%up)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Radiance2 structure arrays not allocated')
    ENDIF

    IF (SIZE(radiance2%up, DIM=1) /= nlevels-1 .OR. &
        SIZE(radiance2%up, DIM=2) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Radiance2 structure arrays have wrong size')
    ENDIF

    CATCH
  END SUBROUTINE check_direct_alloc


  SUBROUTINE check_direct_dealloc(err)
    ! Check direct-only variables have been deallocated correctly

    INTEGER(jpim),           INTENT(INOUT) :: err

    TRY

    ! -------------------------------------------
    ! Check array deallocations
    ! -------------------------------------------
    IF (ASSOCIATED(chanprof)     .OR. &
        ASSOCIATED(calcemis)     .OR. &
        ASSOCIATED(calcrefl)     .OR. &
        ASSOCIATED(channels_rec) .OR. &
        ASSOCIATED(frequencies)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Direct-only array not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check radiance2 structure
    ! -------------------------------------------
    IF (ASSOCIATED(radiance2%up)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Radiance2 structure arrays not deallocated')
    ENDIF

    CATCH
  END SUBROUTINE check_direct_dealloc


  SUBROUTINE check_model_alloc( &
                err,           &
                nprofiles,     &
                emissivity,    &
                reflectance,   &
                profiles,      &
                radiance,      &
                transmission,  &
                aer_opt_param, &
                cld_opt_param, &
                traj,          &
                pccomp,        &
                cld_profiles,  &
                reflectivity)

    ! Check variables have been allocated correctly: these can be for any model.
    ! For each structure we just need to check that every dimension is passed
    ! correctly into the individual allocation routines. This means testing one
    ! array of each size in each structure.

    INTEGER(jpim),             INTENT(INOUT) :: err
    INTEGER(jpim),             INTENT(IN)    :: nprofiles ! This is nchanprof for K

    TYPE(rttov_emissivity),    POINTER       :: emissivity(:)
    TYPE(rttov_reflectance),   POINTER       :: reflectance(:)
    TYPE(rttov_profile),       POINTER       :: profiles(:)
    TYPE(rttov_transmission),  INTENT(IN)    :: transmission
    TYPE(rttov_radiance),      INTENT(IN)    :: radiance
    TYPE(rttov_opt_param),     INTENT(IN)    :: aer_opt_param
    TYPE(rttov_opt_param),     INTENT(IN)    :: cld_opt_param
    TYPE(rttov_traj),          INTENT(IN)    :: traj
    TYPE(rttov_pccomp),        INTENT(IN)    :: pccomp
    TYPE(rttov_profile_cloud), POINTER       :: cld_profiles(:)
    TYPE(rttov_reflectivity),  INTENT(IN)    :: reflectivity

    TRY

    ! -------------------------------------------
    ! Check array allocations
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(profiles)     .OR. &
        .NOT. ASSOCIATED(cld_profiles) .OR. &
        .NOT. ASSOCIATED(emissivity)   .OR. &
        .NOT. ASSOCIATED(reflectance))THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'array not associated')
    ENDIF

    IF (SIZE(profiles)     /= nprofiles .OR. &
        SIZE(cld_profiles) /= nprofiles .OR. &
        SIZE(emissivity)   /= nchanprof .OR. &
        SIZE(reflectance)  /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'array has wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check profiles structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(profiles(1)%p)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'profiles structure arrays not allocated')
    ENDIF

    IF (SIZE(profiles(1)%p) /= nlevels) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'profiles structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check cld_profiles structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(cld_profiles(1)%ph)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_profiles structure arrays not allocated')
    ENDIF

    IF (SIZE(cld_profiles(1)%ph) /= nlevels+1) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_profiles structure arrays have wrong size')
    ENDIF

    IF (SIZE(cld_profiles(1)%hydro, DIM=1) /= nlevels .OR. &
        SIZE(cld_profiles(1)%hydro, DIM=2) /= coef_scatt%nhydro) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_profiles structure arrays have wrong size')
    ENDIF

    IF (SIZE(cld_profiles(1)%hydro_frac, DIM=1) /= nlevels .OR. &
        SIZE(cld_profiles(1)%hydro_frac, DIM=2) /= nhydro_frac) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_profiles structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check transmission structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(transmission%tau_levels)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'transmission structure arrays not allocated')
    ENDIF

    IF (SIZE(transmission%tau_levels, DIM=1) /= nlevels .OR. &
        SIZE(transmission%tau_levels, DIM=2) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'transmission structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check radiance structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(radiance%overcast)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'radiance structure arrays not allocated')
    ENDIF

    IF (SIZE(radiance%overcast, DIM=1) /= nlevels-1 .OR. &
        SIZE(radiance%overcast, DIM=2) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'radiance structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check reflectivity structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(reflectivity%zef)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'reflectivity structure arrays not allocated')
    ENDIF

    IF (SIZE(reflectivity%zef, DIM=1) /= nlevels .OR. &
        SIZE(reflectivity%zef, DIM=2) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'reflectivity structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check opt param allocations
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(aer_opt_param%pha)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'aer_opt_param structure arrays not allocated')
    ENDIF

    IF (SIZE(aer_opt_param%pha, DIM=1)     /= aer_nphangle  .OR. &
        SIZE(aer_opt_param%pha, DIM=2)     /= nlevels-1     .OR. &
        SIZE(aer_opt_param%pha, DIM=3)     /= nchanprof     .OR. &
        SIZE(aer_opt_param%legcoef, DIM=1) /= aer_maxnmom+1 .OR. &
        SIZE(aer_opt_param%legcoef, DIM=2) /= nlevels-1     .OR. &
        SIZE(aer_opt_param%legcoef, DIM=3) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'aer_opt_param structure arrays have wrong size')
    ENDIF

    IF (.NOT. ASSOCIATED(cld_opt_param%pha)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_opt_param structure arrays not allocated')
    ENDIF

    IF (SIZE(cld_opt_param%pha, DIM=1)     /= cld_nphangle  .OR. &
        SIZE(cld_opt_param%pha, DIM=2)     /= nlevels-1     .OR. &
        SIZE(cld_opt_param%pha, DIM=3)     /= nchanprof     .OR. &
        SIZE(cld_opt_param%legcoef, DIM=1) /= cld_maxnmom+1 .OR. &
        SIZE(cld_opt_param%legcoef, DIM=2) /= nlevels-1     .OR. &
        SIZE(cld_opt_param%legcoef, DIM=3) /= nchanprof) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_opt_param structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check traj structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(traj%opdp_path%atm_level) .OR. &
        .NOT. ASSOCIATED(traj%profiles_coef)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'traj structure arrays not allocated')
    ENDIF

    IF (SIZE(traj%opdp_path%atm_level, DIM=1) /= nlevels .OR. &
        SIZE(traj%opdp_path%atm_level, DIM=2) /= nchanprof .OR. &
        SIZE(traj%profiles_coef)              /= nprofiles) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'traj structure arrays have wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check pccomp structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(pccomp%total_pcscores) .OR. &
        .NOT. ASSOCIATED(pccomp%total_pccomp)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'pccomp structure arrays not allocated')
    ENDIF

    IF (SIZE(pccomp%total_pcscores) /= npcscores .OR. &
        SIZE(pccomp%total_pccomp)   /= nchannels_rec) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'pccomp structure arrays have wrong size')
    ENDIF

    CATCH
  END SUBROUTINE check_model_alloc


  SUBROUTINE check_pc_k_alloc( &
                err,        &
                nprofiles,  &
                profiles_k)

    ! Check PC-RTTOV K output Jacobian profiles structures have been allocated
    ! correctly.

    INTEGER(jpim),           INTENT(INOUT) :: err
    INTEGER(jpim),           INTENT(IN)    :: nprofiles

    TYPE(rttov_profile),     POINTER       :: profiles_k(:)

    TRY

    ! -------------------------------------------
    ! Check array allocation
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(profiles_k))THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'PC-RTTOV profiles_k_pc/rec array not associated')
    ENDIF

    IF (SIZE(profiles_k) /= nprofiles) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'PC-RTTOV profiles_k_pc/rec array has wrong size')
    ENDIF

    ! -------------------------------------------
    ! Check structure
    ! -------------------------------------------
    IF (.NOT. ASSOCIATED(profiles_k(1)%p)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'PC-RTTOV profiles_k_pc/rec structure arrays not allocated')
    ENDIF

    IF (SIZE(profiles_k(1)%p) /= nlevels) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'PC-RTTOV profiles_k_pc/rec structure arrays have wrong size')
    ENDIF

    CATCH
  END SUBROUTINE check_pc_k_alloc


  SUBROUTINE check_model_dealloc( &
                err,           &
                emissivity,    &
                reflectance,   &
                profiles,      &
                radiance,      &
                transmission,  &
                aer_opt_param, &
                cld_opt_param, &
                traj,          &
                pccomp,        &
                cld_profiles,  &
                reflectivity,  &
                profiles_k_pc, &
                profiles_k_rec)

    ! Check variables have been deallocated correctly: these can be for any model.

    INTEGER(jpim),             INTENT(INOUT)     :: err

    TYPE(rttov_emissivity),    POINTER           :: emissivity(:)
    TYPE(rttov_reflectance),   POINTER           :: reflectance(:)
    TYPE(rttov_profile),       POINTER           :: profiles(:)
    TYPE(rttov_transmission),  INTENT(IN)        :: transmission
    TYPE(rttov_radiance),      INTENT(IN)        :: radiance
    TYPE(rttov_opt_param),     INTENT(IN)        :: aer_opt_param
    TYPE(rttov_opt_param),     INTENT(IN)        :: cld_opt_param
    TYPE(rttov_traj),          INTENT(IN)        :: traj
    TYPE(rttov_pccomp),        INTENT(IN)        :: pccomp
    TYPE(rttov_profile_cloud), POINTER           :: cld_profiles(:)
    TYPE(rttov_reflectivity),  INTENT(IN)        :: reflectivity

    TYPE(rttov_profile),       POINTER, OPTIONAL :: profiles_k_pc(:)
    TYPE(rttov_profile),       POINTER, OPTIONAL :: profiles_k_rec(:)

    TRY

    ! -------------------------------------------
    ! Check array deallocations
    ! -------------------------------------------
    IF (ASSOCIATED(profiles)     .OR. &
        ASSOCIATED(cld_profiles) .OR. &
        ASSOCIATED(emissivity)   .OR. &
        ASSOCIATED(reflectance))THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'array not deallocated')
    ENDIF

    IF (PRESENT(profiles_k_pc)) THEN
      IF (ASSOCIATED(profiles_k_pc))THEN
        err = errorstatus_fatal
        THROWM(err.NE.0, 'profiles_k_pc not deallocated')
      ENDIF
    ENDIF

    IF (PRESENT(profiles_k_rec)) THEN
      IF (ASSOCIATED(profiles_k_rec))THEN
        err = errorstatus_fatal
        THROWM(err.NE.0, 'profiles_k_rec not deallocated')
      ENDIF
    ENDIF

    ! -------------------------------------------
    ! Check transmission structure
    ! -------------------------------------------
    IF (ASSOCIATED(transmission%tau_levels)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'transmission structure arrays not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check radiance structure
    ! -------------------------------------------
    IF (ASSOCIATED(radiance%overcast)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'radiance structure arrays not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check reflectivity structure
    ! -------------------------------------------
    IF (ASSOCIATED(reflectivity%zef)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'reflectivity structure arrays not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check opt param deallocations
    ! -------------------------------------------
    IF (ASSOCIATED(aer_opt_param%pha)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'aer_opt_param structure arrays not deallocated')
    ENDIF

    IF (ASSOCIATED(cld_opt_param%pha)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'cld_opt_param structure arrays not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check traj structure
    ! -------------------------------------------
    IF (ASSOCIATED(traj%opdp_path%atm_level) .OR. &
        ASSOCIATED(traj%profiles_coef)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'traj structure arrays not deallocated')
    ENDIF

    ! -------------------------------------------
    ! Check pccomp structure
    ! -------------------------------------------
    IF (ASSOCIATED(pccomp%total_pcscores) .OR. &
        ASSOCIATED(pccomp%total_pccomp)) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'pccomp structure arrays not deallocated')
    ENDIF

    CATCH
  END SUBROUTINE check_model_dealloc

END PROGRAM rttov_test_model_alloc
