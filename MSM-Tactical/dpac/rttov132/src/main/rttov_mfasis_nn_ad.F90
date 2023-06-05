!a Description:
!> @file
!!   AD of MFASIS-NN fast visible/near-IR scattering model.
!
!> @brief
!!   AD of MFASIS-NN fast visible/near-IR scattering model.
!!
!!
!! @param[out]    err                 status on exit
!! @param[in]     chanprof            specifies channels and profiles to simulate
!! @param[in]     chanflag            flags to indicate which channels with NN  available
!! @param[in]     opts                options to configure the simulations
!! @param[in]     profiles            input atmospheric profiles and surface variables
!! @param[in,out] profiles_ad         input profile perturbations
!! @param[in]     coefs               coefficients structure for instrument to simulate
!! @param[in]     ircld               information on cloud columns
!! @param[in,out] ircld_ad            cloud column perturbations
!! @param[in]     aux                 additional internal profile variables
!! @param[in,out] aux_ad              additional internal profile variable perturbations
!! @param[in]     reflectance         surface BRDFs
!! @param[in,out] reflectance_ad      surface BRDF perturbations
!! @param[in]     solar_spectrum      TOA solar irradiance for each channel
!! @param[in]     trans_scatt_ir      cloud/aerosol optical depths
!! @param[in,out] trans_scatt_ir_ad   cloud/aerosol optical depth perturbations
!! @param[in,out] radiance_ad         output radiance and BRF perturbations
!! @param[in]     adk                 switch to activate AD or K behaviour
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
SUBROUTINE rttov_mfasis_nn_ad( &
            err,              &
            chanprof,         &
            chanflag,         &
            opts,             &
            profiles,         &
            profiles_ad,      &
            coefs,            &
            ircld,            &
            ircld_ad,         &
            aux,              &
            aux_ad,           &
            reflectance,      &
            reflectance_ad,   &
            solar_spectrum,   &
            trans_scatt_ir,   &
            trans_scatt_ir_ad,&
            radiance_ad,      &
            adk)
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !   1.0     2022     first version    (O.Stiller, DWD)

#include "throw.h"

  USE parkind1, ONLY : jpim, jprb, jplm

  USE rttov_types, ONLY :        &
  rttov_chanprof,              &
  rttov_options,               &
  rttov_profile,               &
  rttov_coefs,                 &
  rttov_ircld,                 &
  rttov_profile_aux,           &
  rttov_reflectance,           &
  rttov_transmission_scatt_ir, &
  rttov_radiance
!INTF_OFF
  USE rttov_types, ONLY : &
  rttov_coef_mfasis_nn,        &
  rttov_mfasis_nn_params

  USE rttov_const, ONLY : &
  wcl_opac_deff,               &
  pi,                          &
  deg2rad,                     &
  rad2deg,                     &
  pi_r,                        &
  clw_scheme_deff,             &
  mfasis_cld,                  &
  nwcl_max,                    &
  adk_adjoint

  USE rttov_mfasis_nn_mod, ONLY : &
  aa_sc, noinput,              &
  fornado_inference_nl,        &
  comp_fbot,                   &
  nn_transform_input,          &
  nn_transform_output

  USE yomhook, ONLY : lhook, dr_hook, jphook
!INTF_ON
  IMPLICIT NONE

  INTEGER(jpim),                      INTENT(OUT)   :: err
  TYPE(rttov_chanprof),               INTENT(IN)    :: chanprof(:)
  LOGICAL(jplm),                      INTENT(IN)    :: chanflag(SIZE(chanprof))
  TYPE(rttov_options),                INTENT(IN)    :: opts
  TYPE(rttov_profile),                INTENT(IN)    :: profiles(:)
  TYPE(rttov_profile),                INTENT(INOUT) :: profiles_ad(:)
  TYPE(rttov_coefs),                  INTENT(IN)    :: coefs
  TYPE(rttov_ircld),                  INTENT(IN)    :: ircld
  TYPE(rttov_ircld),                  INTENT(INOUT) :: ircld_ad
  TYPE(rttov_profile_aux),            INTENT(IN)    :: aux
  TYPE(rttov_profile_aux),            INTENT(INOUT) :: aux_ad
  TYPE(rttov_reflectance),            INTENT(IN)    :: reflectance(SIZE(chanprof))
  TYPE(rttov_reflectance),            INTENT(INOUT) :: reflectance_ad(SIZE(chanprof))
  REAL(jprb),                         INTENT(IN)    :: solar_spectrum(SIZE(chanprof))
  TYPE(rttov_transmission_scatt_ir),  INTENT(IN)    :: trans_scatt_ir
  TYPE(rttov_transmission_scatt_ir),  INTENT(INOUT) :: trans_scatt_ir_ad
  TYPE(rttov_radiance),               INTENT(INOUT) :: radiance_ad
  INTEGER(jpim),                      INTENT(IN)    :: adk
!INTF_END

#include "rttov_errorreport.interface"

  INTEGER(jpim)              :: nchanprof, prof, profad, chan, nn, ncolms
  INTEGER(jpim)              :: nlay
  INTEGER(jpim)              :: i, j, cc
  TYPE(rttov_coef_mfasis_nn) :: mfasisnn_coefs
  TYPE(rttov_mfasis_nn_params):: params

  REAL(jprb)                 :: albedo
  REAL(jprb)                 :: albedo_ad, hvar
  REAL(jprb)                 :: mu, mu0, cad, alpha_deg
  REAL(jprb)                 :: colwei_clr, colwei_cc
  REAL(jprb)                 :: colwei_clr_ad, colwei_cc_ad
  REAL(jprb)                 :: refl_ad, refl_cc_ad, refl_clr_ad

  REAL(jphook) :: zhook_handle


  !XXX NEW MFASIS-NN introduced
  INTEGER(jpim)              :: m, nlev_surf
  INTEGER(jpim)              :: tauw_idx, taui_idx, tauwi_top_idx, tauwi_bot_idx
  ! INTEGER(jpim)              :: reffw_idx, reffi_idx
  INTEGER(jpim)              :: reffw_top_idx, reffi_top_idx, reffw_bot_idx, reffi_bot_idx, reffwi_top_idx
  INTEGER(jpim)              :: reffwi_bot_idx, psfc_idx, fpct_idx
  INTEGER(jpim)              :: vza_idx, sza_idx, alpha_idx
  REAL(jprb)                 :: tauw   , taui   , tauwi   , tauwi_top   , tauwi_bot
  REAL(jprb)                 :: tauw_ad, taui_ad, tauwi_ad, tauwi_top_ad, tauwi_bot_ad
  REAL(jprb)                 :: reffw_top   , reffi_top   , reffw_bot   , reffi_bot   , reffwi_top   , reffwi_bot
  REAL(jprb)                 :: reffw_top_ad, reffi_top_ad, reffw_bot_ad, reffi_bot_ad, reffwi_top_ad, reffwi_bot_ad
  REAL(jprb)                 :: psfc, fpct
  REAL(jprb)                 :: psfc_ad, psfc0_ad, fpct_ad
  REAL(jprb)                 :: psfc_nn, fpct_nn
  REAL(jprb)                 :: sza, vza
  REAL(jprb)                 :: sza_nn, vza_nn, alpha_nn
  REAL(jprb)                 :: tauw_nn, taui_nn, reffw_top_nn, reffw_bot_nn 
  REAL(jprb)                 :: reffi_top_nn , reffi_bot_nn , tauwi_top_nn , tauwi_bot_nn , reffwi_top_nn, reffwi_bot_nn
  REAL(jprb)                 :: fac_hi, fac_lo, tau_lo, tau_hi, tau_thr, tau_thr_v1, tauw_top, taui_top, tau
  REAL(jprb)                 ::            tau_thr_ad, tauw_top_ad, taui_top_ad, tau_ad
  REAL(jprb)                 :: tauw_thr, taui_thr, tauw_thr_ad, taui_thr_ad, logtauw, logtaui,  logtauw_ad, logtaui_ad

  REAL(jprb)                 :: tauw_bot, taui_bot
  REAL(jprb)                 :: tauw_bot_ad, taui_bot_ad
  REAL(jprb)                 :: refl_cc, refl_clr, refl
  REAL(jprb)                 :: eta, gamma
  REAL(jprb)                 :: eta_ad, gamma_ad
  REAL(jprb),    ALLOCATABLE :: dtauw(:), dtaui(:), fmp(:), dreffw(:), dreffi(:), fbotw(:), fboti(:), tauv1(:)
  REAL(jprb),    ALLOCATABLE :: dtauw_v1(:), dtaui_v1(:)
  REAL(jprb),    ALLOCATABLE :: dtauw_ad(:), dtaui_ad(:), fmp_ad(:), dreffw_ad(:), dreffi_ad(:), fbotw_ad(:), fboti_ad(:)
  REAL(jprb),    ALLOCATABLE ::             ddtaui_ad(:)
  REAL(jprb),    ALLOCATABLE :: xx(:)   , yy(:), sigo(:,:,:), yy_v1(:)
  REAL(jprb),    ALLOCATABLE :: xx_ad(:), yy_ad(:)
  ! --------------------------------------------------------------------------------------

  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN_AD', 0_jpim, ZHOOK_HANDLE)

  ! TODO: aerosol functionality not included

  ! --------------------------------------------------------------------------
  ! Initialisation
  ! --------------------------------------------------------------------------
  refl_ad    = 0
  refl_cc_ad   = 0
  colwei_cc_ad = 0
  albedo_ad = 0
  gamma_ad  = 0
  eta_ad    = 0
  tauw_ad  = 0
  taui_ad  = 0
  refl_clr_ad   = 0
  colwei_clr_ad = 0
  
  tauwi_ad     = 0
  tauwi_top_ad = 0
  tauwi_bot_ad = 0
  reffw_top_ad = 0
  reffi_top_ad = 0
  reffw_bot_ad = 0
  reffi_bot_ad = 0
  reffwi_top_ad= 0
  reffwi_bot_ad= 0
  psfc_ad      = 0
  psfc0_ad     = 0
  fpct_ad      = 0
  tau_thr_ad    = 0
  tauw_top_ad  = 0
  taui_top_ad  = 0
  tau_ad       = 0
  tauw_thr     = 0
  taui_thr     = 0
  tauw_thr_ad  = 0
  taui_thr_ad  = 0
  logtauw      = 0
  logtaui      = 0
  logtauw_ad   = 0
  logtaui_ad   = 0

  tauw_bot_ad  = 0
  taui_bot_ad  = 0

  ! --------------------------------------------------------------------------
  nchanprof = SIZE(chanprof)
  nlay = profiles(1)%nlayers

  IF (opts%rt_ir%addclouds) THEN
    mfasisnn_coefs = coefs%coef_mfasis_nn
  ELSE
    err = errorstatus_fatal
    THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
  ENDIF

  IF (mfasisnn_coefs%version >= 1) THEN
    ALLOCATE( dtauw(nlay)                 , &   ! layer optical depth of water cloud
              dtaui(nlay)                 , &   ! layer optical depth of ice cloud
              dtauw_v1(nlay)              , &   ! layer optical depth of ice cloud
              dtaui_v1(nlay)              , &   ! layer optical depth of ice cloud
              dtauw_ad(nlay)              , &   ! layer optical depth of ice cloud
              dtaui_ad(nlay)              , &   ! layer optical depth of ice cloud
             ddtaui_ad(nlay)              , &   ! layer optical depth of ice cloud
              fmp(nlay)                   , &   ! factor that defines onset of mixed-phase cloud
              fmp_ad(nlay)                , &   ! factor that defines onset of mixed-phase cloud
              dreffw(nlay)                , &   ! layer effective water cloud radii
              dreffw_ad(nlay)             , &   ! layer effective water cloud radii
              dreffi(nlay)                , &   ! layer effective ice cloud radii
              dreffi_ad(nlay)             , &   ! layer effective ice cloud radii
              fbotw(nlay)                  , &
              fboti(nlay)                  , &
              fbotw_ad(nlay)               , &
              fboti_ad(nlay)               , &
              tauv1(0:nlay)                  , &
                          STAT=err)             ! 2-layer param.: factor that defines boarder between 2 layers  
    THROWM(err.NE.0, "Allocation of memory for rttov_mfasis_nn_ad failed")

    dtauw(:)    = 0
    dtaui(:)    = 0
    dtauw_v1(:) = 0
    dtaui_v1(:) = 0
    dtauw_ad(:) = 0
    dtaui_ad(:) = 0
    ddtaui_ad(:)= 0
    fmp(:)      = 0
    fmp_ad(:)   = 0
    dreffw(:)   = 0
    dreffw_ad(:)= 0
    dreffi(:)   = 0
    dreffi_ad(:)= 0
    fbotw(:)    = 0
    fboti(:)    = 0
    fbotw_ad(:) = 0
    fboti_ad(:) = 0
    tauv1(:)    = 0
  ELSE ! IF (mfasisnn_coefs%version >= 1)
    err = errorstatus_fatal
    THROWM(err.NE.0, "MFASIS-NN version not implemented")
  ENDIF

  ! --------------------------------------------------------------------------
  ! Channel loop
  ! --------------------------------------------------------------------------
  DO i = 1, nchanprof

    IF (.NOT. chanflag(i)) CYCLE
    IF (adk == adk_adjoint) THEN
      profad = chanprof(i)%prof  ! AD
    ELSE
      profad = i                 ! K
    ENDIF
    prof = chanprof(i)%prof
    chan = chanprof(i)%chan
    nn = mfasisnn_coefs%channel_nn_index(chan)
    nlev_surf=aux%s(prof)%nearestlev_surf

    tauw = noinput
    taui = noinput
    tauwi_top = noinput
    tauwi_bot = noinput
    reffw_top = noinput
    reffi_top = noinput
    reffw_bot = noinput
    reffi_bot = noinput
    reffwi_top = noinput
    reffwi_bot = noinput
    psfc_nn = noinput
    fpct_nn = noinput
    tauw_idx = -1
    taui_idx = -1
    tauwi_top_idx = -1
    tauwi_bot_idx = -1
    reffw_top_idx = -1
    reffi_top_idx = -1
    reffw_bot_idx = -1
    reffi_bot_idx = -1
    reffwi_top_idx = -1
    reffwi_bot_idx = -1
    psfc_idx = -1
    fpct_idx = -1

    vza_nn = noinput
    sza_nn = noinput
    alpha_nn = noinput
    vza_idx = -1
    sza_idx = -1
    alpha_idx = -1

    DO m = 1, mfasisnn_coefs%nn(nn)%n_input !
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'VZA' ) vza_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'SZA' ) sza_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'ALPHA' ) alpha_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'TAUW' ) tauw_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'TAUI' ) taui_idx = m
      ! IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFW' ) reffw_idx = m
      ! IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFI' ) reffi_idx = m

      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'TAUWI_TOP' ) tauwi_top_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'TAUWI_BOT' ) tauwi_bot_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFW_TOP' ) reffw_top_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFI_TOP' ) reffi_top_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFW_BOT' ) reffw_bot_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFI_BOT' ) reffi_bot_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFWI_TOP' ) reffwi_top_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'REFFWI_BOT' ) reffwi_bot_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'PSFC' ) psfc_idx = m
      IF ( mfasisnn_coefs%nn(nn)%in(m)%name == 'FPCT' ) fpct_idx = m
    ENDDO

    ! --------------------------------------------------------------------------
    ! Albedo and angle initialisation
    ! --------------------------------------------------------------------------
    albedo = MIN(reflectance(i)%refl_out * pi, 1._jprb)

    vza = profiles(prof)%zenangle
    sza = profiles(prof)%sunzenangle

    mu  = COS(vza * deg2rad)
    mu0 = COS(sza * deg2rad)
    ! Angle convention as RTTOV/MFASIS-LUT (opposite DISORT/paper):
    ! Backscattering: alpha = 0
    ! Forward scatterng: alpha = pi
    cad = COS(profiles(prof)%azangle * deg2rad - profiles(prof)%sunazangle * deg2rad)
    alpha_deg =  rad2deg*(ACOS(mu*mu0 + SQRT(1 - mu**2)*SQRT(1 - mu0**2)*cad)) !

    ! --------------------------------------------------------------------------
    ! NN-input variables which are independent of cloud/aerosol profile
    ! --------------------------------------------------------------------------
    vza_nn  =vza
    sza_nn  =sza
    alpha_nn=alpha_deg
    params=mfasisnn_coefs%nn(nn)%in(vza_idx)
    CALL nn_transform_input(vza_nn ,  params%MIN, params%MAX, params%TRANSFORM)
    params=mfasisnn_coefs%nn(nn)%in(sza_idx)
    CALL nn_transform_input(sza_nn ,  params%MIN, params%MAX, params%TRANSFORM)
    params=mfasisnn_coefs%nn(nn)%in(alpha_idx)
    CALL nn_transform_input(alpha_nn ,params%MIN, params%MAX, params%TRANSFORM)

    psfc   = profiles(prof)%s2m%p
    psfc_nn= psfc
    params=mfasisnn_coefs%nn(nn)%in(psfc_idx)
    CALL nn_transform_input(psfc_nn , params%MIN, params%MAX, params%TRANSFORM)

    ! --------------------------------------------------------------------------
    ! Loop over (cloud) columns
    ! --------------------------------------------------------------------------
    ! No cloud columns for aerosols calculations
    IF ( mfasisnn_coefs%file_type == mfasis_cld ) THEN
      ncolms = ircld%ncolumn(prof)
    ELSE
      ncolms = 1_jpim
    ENDIF


    refl = 0._jprb
    refl_clr = 0._jprb

!=====================================================================================
!  linear computations independent of column cc
!=====================================================================================
    !================================
    ! To be consistent with other RTTOV calls we only use input AD/K increments in
    ! radiance%total and we assume all other radiance arrays are zero
    refl_ad    = refl_ad     + solar_spectrum(i) * pi_r   &
      * COS(profiles(prof)%sunzenangle * deg2rad) * radiance_ad%total(i)
    radiance_ad%total(i) = 0
  
    DO cc = 1, ncolms

      ! --------------------------------------------------------------------------
      ! Set column weight, optical depth and effective diameters
      ! --------------------------------------------------------------------------
      IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) THEN ! clouds
        colwei_cc = ircld%xcol(cc+1,prof) - ircld%xcol(cc,prof)

        tauw = 0.0_jprb
        taui = 0.0_jprb
        tauwi_top = 0.0_jprb
        tauwi_bot = 0.0_jprb
        reffw_top = 0.0_jprb
        reffi_top = 0.0_jprb
        reffw_bot = 0.0_jprb
        reffi_bot = 0.0_jprb
        reffwi_top = 0.0_jprb
        reffwi_bot = 0.0_jprb
        fpct = 1._jprb

        ! Extract optical depths and effective radii per layer
        dtauw(:) = 0._jprb
        dtaui(:) = 0._jprb
        fmp(:) = 0._jprb
        dreffw(:) = 0._jprb
        dreffi(:) = 0._jprb
        DO j = 1, nlay
!         IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
          IF (j > aux%s(prof)%nearestlev_surf - 1) CYCLE    ! Layer is entirely below surface pressure so nothing more to do
          IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN

            ! layer optical depths
            dtauw(j) = SUM(trans_scatt_ir%opdpext(1:nwcl_max,j,i))
            dtaui(j) = trans_scatt_ir%opdpext(nwcl_max+1,j,i)
            dtauw_v1(j) =dtauw(j)
            dtaui_v1(j) =dtaui(j)
            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
              dtauw(j) = dtauw(j) * (1 - aux%s(prof)%pfraction_surf) 
              dtaui(j) = dtaui(j) * (1 - aux%s(prof)%pfraction_surf)
            ENDIF

            ! layer eff. radii water
            IF ( profiles(prof)%clw_scheme == clw_scheme_deff ) THEN ! DEFF
              dreffw(j) = aux%clw_dg(j,prof)/2._jprb
            ELSE ! OPAC
              IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
                ! Modify optical depth in partial layer above surface
                dreffw(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir%opdpext(1:nwcl_max,j,i)  &
                                                                                 * (1 - aux%s(prof)%pfraction_surf))
              ELSE
                dreffw(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir%opdpext(1:nwcl_max,j,i))
              ENDIF
              IF ( dtauw(j) .GT. 0. ) THEN
                dreffw(j) = dreffw(j)/dtauw(j) 
              ELSE
                dreffw(j) = 0 ! set to zero
              ENDIF
            ENDIF
            dreffi(j) = aux%ice_dg(j,prof)/2._jprb  !

            tauw = tauw + dtauw(j)
          ENDIF
        ENDDO ! DO j = 1, nlay
        tau_thr =  1._jprb
        !---------------------------------------------------------------
        call comp_fbot(fmp, tau_thr, dtauw, nlay, aa_sc, nlev_surf)
        !---------------------------------------------------------------

        ! total optical depths
        ! tauw = SUM(dtauw(:)) ! calculated above when setting fmp(j)
        tauwi = SUM(dtaui(:) * fmp(:)) ! mixed-phase cloud ice in water cloud
        taui = SUM(dtaui(:) * (1._jprb - fmp(:)))

        ! 2-layer parameterisation of water cloud
        IF (SIZE(mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams) > 0) THEN
          IF(tauw > 0) THEN
            fac_hi =  mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(3) &
                    - mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(4) * MAX( vza, sza ) / 90._jprb
            fac_lo = 0.5_jprb 
            tau_lo = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(1)
            tau_hi = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(2)
            logtauw = MIN( MAX( (LOG(tauw) - LOG(tau_lo)) / (LOG(tau_hi) - LOG(tau_lo)), 0._jprb ), 1._jprb ) 
            tauw_thr = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtauw * pi ) ) ) * tauw

!                 tauw_top = tauw_thr
!                 tauw_bot = tauw - tauw_thr
            ! compute mean effective radii for both parts
            !---------------------------------------------------------------
            call comp_fbot(fbotw, tauw_thr, dtauw, nlay, aa_sc, nlev_surf)
            !---------------------------------------------------------------
            tauw_bot = SUM( fbotw(:) * dtauw(:))
            tauw_top = tauw - tauw_bot

            IF ( tauw_bot > 1E-6 ) THEN
              reffw_bot = SUM( fbotw(:)           * dtauw(:) * dreffw(:) ) / tauw_bot
            ELSE 
              reffw_bot = mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)%MIN !
            ENDIF
            IF ( tauw_top > 1E-6 ) THEN
              reffw_top = SUM( (1._jprb-fbotw(:)) * dtauw(:) * dreffw(:) ) / tauw_top
            ELSE
              reffw_top = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%MIN !
            ENDIF

            ! determine optical depth and mean effective radius of mixed phase ice in the lower water cloud layer
            tauwi_bot = SUM( fbotw(:) * dtaui(:) * fmp(:) )
            IF ( tauwi_bot > 1E-6 ) THEN
              reffwi_bot = SUM( fbotw(:)             * dtaui(:) * fmp(:) * dreffi(:) ) / tauwi_bot
            ELSE
              reffwi_bot = mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)%MIN !
            ENDIF
            ! determine optical depth and mean effective radius of mixed phase ice in the upper water cloud layer
            tauwi_top = SUM( (1._jprb - fbotw(:)) * dtaui(:) * fmp(:) )
            IF ( tauwi_top > 1E-6 ) THEN
              reffwi_top = SUM( (1._jprb - fbotw(:)) * dtaui(:) * fmp(:) * dreffi(:) ) / tauwi_top
            ELSE
              reffwi_top = mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)%MIN !
            ENDIF
          ELSE
            reffw_bot = mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)%MIN !
            reffw_top = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%MIN !
            reffwi_bot = mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)%MIN !
            reffwi_top = mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)%MIN !
          ENDIF
        ELSE
          err = errorstatus_fatal
          THROWM(err.NE.0, "MFASIS-NN has no information on two-layer parameterisation of water cloud")
        ENDIF

        ! 2-layer parameterisation of ice cloud
        IF (SIZE(mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams) > 0) THEN
          IF(taui > 0) THEN
            fac_hi =  mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(3) &
                    - mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(4) * MAX( vza, sza ) / 90._jprb
            fac_lo = 0.5_jprb 
            tau_lo = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(1)
            tau_hi = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(2)
            logtaui = MIN( MAX( (LOG(taui) - LOG(tau_lo)) / (LOG(tau_hi) - LOG(tau_lo)), 0._jprb ), 1._jprb ) 
            taui_thr = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtaui * pi ) ) ) * taui


            ! compute mean effective radii for both parts
            !---------------------------------------------------------------
            call comp_fbot(fboti, taui_thr, dtaui*(1._jprb - fmp(:)), nlay, aa_sc, nlev_surf)
            !---------------------------------------------------------------
            taui_bot = SUM(fboti(:) * dtaui(:) * (1._jprb - fmp(:)))
            taui_top = taui - taui_bot

            IF( taui_bot > 1E-6 ) THEN
              reffi_bot = SUM( fboti(:)          * dtaui(:) * (1._jprb-fmp) * dreffi(:) ) / taui_bot
            ELSE 
              reffi_bot = mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)%MIN !
            ENDIF

            IF ( taui_top > 1E-6 ) THEN
              reffi_top = SUM( (1._jprb-fboti(:)) * dtaui(:) * (1._jprb-fmp) * dreffi(:) ) / taui_top
            ELSE
              reffi_top = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%MIN !
            ENDIF
          ELSE
            reffi_bot = mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)%MIN !
            reffi_top = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%MIN !
          ENDIF
        ELSE
          err = errorstatus_fatal
          THROWM(err.NE.0, "MFASIS-NN has no information on two-layer parameterisation of ice cloud")
        ENDIF

        ! >>> This cloud-top-pressure is interpreted as water-cloud-top pressure. If no water-cloud is detected,
        ! >>> the pressure is set to surface pressure. If there solely is an ice-cloud present, this cloud would
        ! >>> be places at the surface and hence lead to errors. This definition causes in some situations to high
        ! >>> clouds but barely too low clouds.
        ! >>> should remain like this, as the NN was trained based on this definition

        ! threshold optical depth for detecting cloud top
        tau_thr = (tauw + taui + tauwi)/2._jprb
        tau_thr = tau_thr/(tau_thr + 1._jprb)

        ! find corresponding pressure
        tau = 0._jprb
        DO j = 1, nlay
          tau = tau + dtauw(j) + dtaui(j)
          IF ( tau > tau_thr) THEN 
!           fpct = profiles(prof)%p(j) / psfc
            fpct = profiles(prof)%p(j+1) - (tau - tau_thr)/(dtauw(j) + dtaui(j))  &
                                                  * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
            fpct = fpct / psfc
            EXIT
          ENDIF
        ENDDO

        ! --------------------------------------------------------------------------
        ! Transform input variables (by normalization) to produce NN input ->  successfully checked wrt LMU
        ! Can we write loop over dimensions in xx and do not do this for each  input parameter?
        ! --------------------------------------------------------------------------
        tauw_nn=tauw
        taui_nn=taui
        reffw_top_nn  =reffw_top
        reffw_bot_nn  =reffw_bot
        reffi_top_nn  =reffi_top
        reffi_bot_nn  =reffi_bot
        tauwi_top_nn  =tauwi_top
        tauwi_bot_nn  =tauwi_bot
        reffwi_top_nn =reffwi_top
        reffwi_bot_nn =reffwi_bot
        fpct_nn =fpct

        params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
        CALL nn_transform_input(tauw_nn      ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(taui_idx)
        CALL nn_transform_input(taui_nn      ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
        CALL nn_transform_input(reffw_top_nn ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
        CALL nn_transform_input(reffw_bot_nn ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
        CALL nn_transform_input(reffi_top_nn ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
        CALL nn_transform_input(reffi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
        CALL nn_transform_input(tauwi_top_nn ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
        CALL nn_transform_input(tauwi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
        CALL nn_transform_input(reffwi_top_nn,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
        CALL nn_transform_input(reffwi_bot_nn,params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
        CALL nn_transform_input(fpct_nn      ,params%MIN, params%MAX, params%TRANSFORM)

        ALLOCATE(xx(mfasisnn_coefs%nn(nn)%n_input))
        ALLOCATE(yy(mfasisnn_coefs%nn(nn)%n_output))
        ALLOCATE(yy_v1(mfasisnn_coefs%nn(nn)%n_output))
        xx = (/tauw_nn, taui_nn, tauwi_top_nn, tauwi_bot_nn, &
                    reffw_top_nn, reffi_top_nn, reffw_bot_nn, reffi_bot_nn, &
                   reffwi_top_nn, reffwi_bot_nn, psfc_nn, fpct_nn, sza_nn, vza_nn, alpha_nn/)
        yy(:) = 0._jprb

        CALL fornado_inference_nl( 1,                             &
                                mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
                                mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
                                mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
                                mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
                                mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
                                sigo, xx, yy)
        yy_v1=yy
        DO m = 1, mfasisnn_coefs%nn(nn)%n_output
          IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_A0' ) THEN
            params=mfasisnn_coefs%nn(nn)%out(m)
            CALL nn_transform_output(yy(1) ,params%MIN, params%MAX, params%TRANSFORM)
          ENDIF
          IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_DH' ) THEN
            params=mfasisnn_coefs%nn(nn)%out(m)
            CALL nn_transform_output(yy(2) ,params%MIN, params%MAX, params%TRANSFORM)
          ENDIF
          IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_D1' ) THEN
            params=mfasisnn_coefs%nn(nn)%out(m)
            CALL nn_transform_output(yy(3) ,params%MIN, params%MAX, params%TRANSFORM)
          ENDIF
        ENDDO

        ! --------------------------------------------------------------------------
        ! Compute refl for model profile albedo (Jonkheid et al.) -> successfully checked wrt LMU
        ! --------------------------------------------------------------------------
        IF ( yy(3) > 1E-4 ) THEN
          eta = (yy(3) - yy(2))
          gamma = ( yy(3) + yy(2) ) * yy(2)
          refl_cc = yy(1) + albedo * gamma/(yy(3) - albedo*eta)
        ELSE
          refl_cc = yy(1) + albedo * (yy(3) + yy(2))
        ENDIF
        DEALLOCATE(xx)

        ! --------------------------------------------------------------------------
        ! Add column contribution to total reflectance
        ! --------------------------------------------------------------------------
        refl = refl + refl_cc * colwei_cc
        
      ELSE
        err = errorstatus_fatal
        THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
      ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column

!==================================================================================
!==================================================================================
! Start linear computations for channel "i" and column "cc"
!==================================================================================
!==================================================================================

      ALLOCATE(xx_ad(mfasisnn_coefs%nn(nn)%n_input))
      ALLOCATE(yy_ad(mfasisnn_coefs%nn(nn)%n_output))
      yy_ad = 0
      xx_ad = 0


!     refl_ad = refl_ad + refl_cc_ad * colwei_cc + refl_cc * colwei_cc_ad
      refl_cc_ad   = refl_cc_ad   + colwei_cc *  refl_ad
      colwei_cc_ad = colwei_cc_ad + refl_cc   *  refl_ad

      ! --------------------------------------------------------------------------
      ! Compute refl for model profile albedo (Jonkheid et al.) -> successfully checked wrt LMU
      ! --------------------------------------------------------------------------

      IF ( yy(3) > 1E-4 ) THEN
        eta = (yy(3) - yy(2))
        gamma = ( yy(3) + yy(2) ) * yy(2)
        hvar=albedo *gamma   /(yy(3) - albedo*eta)**2

!         refl_cc_ad = yy_ad(1) + albedo_ad*gamma   /(yy(3) - albedo*eta) &
!                                 + albedo   *gamma_ad/(yy(3) - albedo*eta) &
!                                 + hvar * ( -yy_ad(3) + albedo_ad*eta + albedo*eta_ad )
        albedo_ad = albedo_ad + gamma   /(yy(3) - albedo*eta) * refl_cc_ad
        albedo_ad = albedo_ad + hvar * eta                    * refl_cc_ad
        gamma_ad  = gamma_ad  + albedo /(yy(3) - albedo*eta)  * refl_cc_ad
        eta_ad    = eta_ad    + hvar * albedo                 * refl_cc_ad
        yy_ad(3)  = yy_ad(3)  - hvar                          * refl_cc_ad
        yy_ad(1)  = yy_ad(1)  +                                 refl_cc_ad
        refl_cc_ad = 0._jprb

!         gamma_ad = ( yy_ad(3) + yy_ad(2) ) * yy(2) + &
!         ( yy   (3) + yy   (2) ) * yy_ad(2)
        yy_ad(3) = yy_ad(3)   + yy(2)                         * gamma_ad
        yy_ad(2) = yy_ad(2)   + ( yy(3) + 2._jprb * yy(2) )  * gamma_ad
        gamma_ad = 0._jprb

!         eta_ad = (yy_ad(3) - yy_ad(2))
        yy_ad(3) = yy_ad(3) + eta_ad
        yy_ad(2) = yy_ad(2) - eta_ad
        eta_ad = 0
      ELSE
!         refl_cc_ad = yy_ad(1) + albedo_ad * (yy(3) + yy(2))  &
!                                  + albedo    * (yy_ad(3) + yy_ad(2))

        albedo_ad = albedo_ad + (yy(3) + yy(2))               * refl_cc_ad
        yy_ad(3)  = yy_ad(3)  + albedo                        * refl_cc_ad
        yy_ad(2)  = yy_ad(2)  + albedo                        * refl_cc_ad
        yy_ad(1)  = yy_ad(1)  + refl_cc_ad
        refl_cc_ad = 0._jprb
      ENDIF

      ! --------------------------------------------------------------------------
      DO m = 1, mfasisnn_coefs%nn(nn)%n_output
        ! mfasisnn_coefs%nn(nn)%out(m)%TRANSFORM
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_A0' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_ad(yy_v1(1) , yy_ad(1) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_DH' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_ad(yy_v1(2) , yy_ad(2) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_D1' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_ad(yy_v1(3) , yy_ad(3) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
      ENDDO

      CALL fornado_inference_ad( 1_jpim,                             &
                               mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
                               mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
                               mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
                               mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
                              !  mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
                               sigo, xx_ad, yy_ad)
      yy_ad(:) = 0._jprb

!         xx_ad = (/tauw_ad, taui_ad, tauwi_top_ad, tauwi_bot_ad, &
!                      reffw_top_ad, reffi_top_ad, reffw_bot_ad, reffi_bot_ad, &
!                     reffwi_top_ad, reffwi_bot_ad, psfc_ad, fpct_ad, 0._jprb,
!                     0._jprb, 0._jprb/)


      tauw_ad      = tauw_ad      + xx_ad(1)
      taui_ad      = taui_ad      + xx_ad(2)
      tauwi_top_ad = tauwi_top_ad + xx_ad(3)
      tauwi_bot_ad = tauwi_bot_ad + xx_ad(4)
      reffw_top_ad = reffw_top_ad + xx_ad(5)
      reffi_top_ad = reffi_top_ad + xx_ad(6)
      reffw_bot_ad = reffw_bot_ad + xx_ad(7)
      reffi_bot_ad = reffi_bot_ad + xx_ad(8)
      reffwi_top_ad = reffwi_top_ad + xx_ad(9)
      reffwi_bot_ad = reffwi_bot_ad + xx_ad(10)
      psfc_ad       = psfc_ad       + xx_ad(11)
      fpct_ad       = fpct_ad       + xx_ad(12)

      xx_ad = 0

      params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
      CALL nn_transform_input_ad(tauw, tauw_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(taui_idx)
      CALL nn_transform_input_ad(taui, taui_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
      CALL nn_transform_input_ad(reffw_top, reffw_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
      CALL nn_transform_input_ad(reffw_bot, reffw_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
      CALL nn_transform_input_ad(reffi_top, reffi_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
      CALL nn_transform_input_ad(reffi_bot, reffi_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
      CALL nn_transform_input_ad(tauwi_top, tauwi_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
      CALL nn_transform_input_ad(tauwi_bot, tauwi_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
      CALL nn_transform_input_ad(reffwi_top, reffwi_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
      CALL nn_transform_input_ad(reffwi_bot, reffwi_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
      CALL nn_transform_input_ad(fpct, fpct_ad , params%MIN, params%MAX, params%TRANSFORM)
      
      DEALLOCATE(xx_ad,yy_ad, yy, yy_v1, sigo)

!==================================================================================================
      ! threshold optical depth for detecting cloud top
!            tau_thr_v1 = (tauw + taui + tauwi)/2._jprb
!            tau_thr    = tau_thr_v1/(tau_thr_v1 + 1._jprb)
!
!            tau_thr_ad = (tauw_ad + taui_ad + tauwi_ad)/2._jprb
!            tau_thr_ad = tau_thr_ad/(tau_thr_v1 + 1._jprb)  -  &
!                         tau_thr_ad * tau_thr_v1/(tau_thr_v1 + 1._jprb)**2
!
!            tau = 0._jprb
!            tau_ad = 0._jprb
!            DO j = 1, nlay
!               tau = tau + dtauw(j) + dtaui(j)
!               tau_ad = tau_ad + dtauw_ad(j) + dtaui_ad(j)
!               IF ( tau > tau_thr) THEN
!                  fpct    = profiles   (prof)%p(j+1) -   (tau    - tau_thr)/(dtauw(j) + dtaui(j))   &
!                                                                * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
!
!                  fpct_ad = profiles_ad(profad)%p(j+1) +( -(tau_ad - tau_thr_ad)/(dtauw(j) + dtaui(j)) &
!                                                                  * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
!                                                          +(tau    - tau_thr)/(dtauw(j) + dtaui(j))**2 &
!                                                                  * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
!                                                                            * (dtauw_ad(j) + dtaui_ad(j)) &
!                                                          -(tau    - tau_thr)/(dtauw(j) + dtaui(j))    &
!                                                             * (profiles_ad(profad)%p(j+1) - profiles_ad(profad)%p(j)) &
!                                                        )
!                  fpct = fpct / psfc
!                  fpct_ad = fpct_ad / psfc - fpct * psfc0_ad/psfc
!                  EXIT
!               ENDIF
!            ENDDO
      tau_thr_v1 = (tauw + taui + tauwi)/2._jprb
      tau_thr    = tau_thr_v1/(tau_thr_v1 + 1._jprb)
      tau = 0._jprb
      tauv1(:) = - 9999999._jprb
      DO j = 1, nlay
        tau = tau + dtauw(j) + dtaui(j)
        tauv1(j)=tau
        IF(tauv1(j) > tau_thr) THEN
          EXIT
        ENDIF
      ENDDO
      tau_ad = 0
      DO j = nlay, 1, -1
        IF ( tauv1(j) > tau_thr) THEN
          fpct    = profiles   (prof)%p(j+1) -   (tauv1(j)-tau_thr)/(dtauw(j) + dtaui(j))    &
                                                                     * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
          fpct = fpct / psfc

!                  fpct_ad = fpct_ad / psfc - fpct * psfc0_ad/psfc
          psfc0_ad = psfc0_ad - fpct_ad * fpct/psfc
          fpct_ad  = fpct_ad / psfc

!                  fpct_ad = profiles_ad(profad)%p(j+1) +( -(tau_ad - tau_thr_ad)/(dtauw(j) + dtaui(j)) &
!                                                                  * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
!                                                          +(tau    - tau_thr)/(dtauw(j) + dtaui(j))**2 &
!                                                                  * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
!                                                                            * (dtauw_ad(j) + dtaui_ad(j)) &
!                                                          -(tau    - tau_thr)/(dtauw(j) + dtaui(j))    &
!                                                             * (profiles_ad(profad)%p(j+1) - profiles_ad(profad)%p(j)) &

          tau_ad     =    tau_ad  - fpct_ad/(dtauw(j)+dtaui(j))    * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
          tau_thr_ad = tau_thr_ad + fpct_ad/(dtauw(j)+dtaui(j))    * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
          dtauw_ad(j)= dtauw_ad(j)+ fpct_ad * (tauv1(j)-tau_thr)/(dtauw(j)+ dtaui(j))**2 &
                                                                   * (profiles(prof)%p(j+1)-profiles(prof)%p(j))
          dtaui_ad(j)= dtaui_ad(j)+ fpct_ad * (tauv1(j)-tau_thr)/(dtauw(j)+ dtaui(j))**2 &
                                                                   * (profiles(prof)%p(j+1)-profiles(prof)%p(j))
          IF (opts%interpolation%lgradp) THEN
            profiles_ad(profad)%p(j+1) = profiles_ad(profad)%p(j+1) - fpct_ad * (tauv1(j)-tau_thr)/(dtauw(j)+dtaui(j)) + fpct_ad
            profiles_ad(profad)%p(j  ) = profiles_ad(profad)%p(j  ) + fpct_ad * (tauv1(j)-tau_thr)/(dtauw(j)+dtaui(j))
          ENDIF
          fpct_ad = 0
        ENDIF
!              tau_ad = tau_ad + dtauw_ad(j) + dtaui_ad(j)
        dtauw_ad(j) = dtauw_ad(j) + tau_ad
        dtaui_ad(j) = dtaui_ad(j) + tau_ad
      ENDDO
!            tau_thr_ad = (tauw_ad + taui_ad + tauwi_ad)/2._jprb
!            tau_thr_ad = tau_thr_ad/(tau_thr_v1 + 1._jprb)  -  &
!                         tau_thr_ad * tau_thr_v1/(tau_thr_v1 + 1._jprb)**2
      tau_thr_ad = tau_thr_ad/(tau_thr_v1 + 1._jprb)  -  &
                        tau_thr_ad * tau_thr_v1/(tau_thr_v1 + 1._jprb)**2
      tauw_ad    = tauw_ad + tau_thr_ad/2._jprb
      taui_ad    = taui_ad + tau_thr_ad/2._jprb
      tauwi_ad   = tauwi_ad+ tau_thr_ad/2._jprb
      tau_thr_ad = 0

      IF (SIZE(mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams) > 0) THEN
        IF(taui > 0) THEN
          IF ( taui_top > 1E-6 ) THEN
!                    reffi_top_ad = SUM( (    -fboti_ad(:)) * dtaui(:) * (1._jprb-fmp) * dreffi(:)    +       &
!                                        (1._jprb-fboti(:)) * dtaui_ad(:)*(1._jprb-fmp) * dreffi(:)   +       &
!                                        (1._jprb-fboti(:)) * dtaui(:)   *( -fmp_ad) * dreffi(:)   +       &
!                                        (1._jprb-fboti(:)) * dtaui(:) *(1._jprb-fmp) * dreffi_ad(:)    ) / taui_top     -       &
!                                   reffi_top/taui_top * taui_top_ad

            fboti_ad(:)   = fboti_ad(:)    - dtaui(:)           * (1._jprb-fmp) * dreffi(:) * reffi_top_ad/ taui_top
            dtaui_ad(:)   = dtaui_ad(:)    + (1._jprb-fboti(:)) * (1._jprb-fmp) * dreffi(:) * reffi_top_ad/ taui_top
            fmp_ad        = fmp_ad         - (1._jprb-fboti(:)) * dtaui(:)      * dreffi(:) * reffi_top_ad/ taui_top
            dreffi_ad(:)  = dreffi_ad(:)   + (1._jprb-fboti(:)) * (1._jprb-fmp) * dtaui(:)  * reffi_top_ad/ taui_top

            taui_top_ad   = taui_top_ad    -                                     reffi_top  * reffi_top_ad/ taui_top
            reffi_top_ad  = 0
          ELSE
            reffi_top_ad = 0._jprb
          ENDIF

          IF( taui_bot > 1E-6 ) THEN
!                    reffi_bot_ad = SUM( fboti_ad(:)     * dtaui(:) *(1._jprb-fmp) * dreffi(:)   +       &
!                                        fboti(:)        * dtaui_ad(:)*(1._jprb-fmp) * dreffi(:)   +       &
!                                        fboti(:)        * dtaui(:)   *( -fmp_ad) * dreffi(:)   +       &
!                                        fboti(:)        * dtaui(:) *(1._jprb-fmp) * dreffi_ad(:)    ) / taui_bot     -       &
!                                   reffi_bot/taui_bot * taui_bot_ad
            fboti_ad(:)    = fboti_ad(:)    + dtaui(:) *(1._jprb-fmp) * dreffi(:) * reffi_bot_ad/taui_bot
            dtaui_ad(:)    = dtaui_ad(:)    + fboti(:) *(1._jprb-fmp) * dreffi(:) * reffi_bot_ad/taui_bot
            fmp_ad         = fmp_ad         - fboti(:) * dtaui(:)     * dreffi(:) * reffi_bot_ad/taui_bot
            dreffi_ad(:)   = dreffi_ad(:)   + fboti(:) *(1._jprb-fmp) * dtaui(:)  * reffi_bot_ad/taui_bot
            taui_bot_ad    = taui_bot_ad    -                          reffi_bot  * reffi_bot_ad/taui_bot
            reffi_bot_ad = 0
          ELSE
            reffi_bot_ad = 0._jprb 
          ENDIF

!                 taui_bot_ad = SUM( fboti_ad(:) * dtaui   (:) * (1._jprb - fmp (:))    &
!                                  + fboti(:)    * dtaui_ad(:) * (1._jprb - fmp (:))    &
!                                  - fboti(:)    * dtaui   (:) * fmp_ad(:)  )
!                 taui_top_ad = taui_ad - taui_bot_ad
          taui_ad     = taui_ad     + taui_top_ad
          taui_bot_ad = taui_bot_ad - taui_top_ad
          taui_top_ad = 0
          dtaui_ad(:) = dtaui_ad(:) + fboti(:) * (1._jprb - fmp (:))   * taui_bot_ad 
          fboti_ad(:) = fboti_ad(:) + dtaui(:) * (1._jprb - fmp (:))   * taui_bot_ad
          fmp_ad(:)   = fmp_ad(:)   - fboti(:) * dtaui(:)              * taui_bot_ad
          taui_bot_ad = 0

          !---------------------------------------------------------------
          call comp_fbot_ad(fboti_ad, taui_thr_ad, ddtaui_ad, taui_thr, dtaui*(1._jprb - fmp(:)), nlay, aa_sc)
          !---------------------------------------------------------------
          fboti_ad = 0

!                 taui_top_ad = taui_thr_ad
!                 taui_bot_ad = taui_ad    - taui_thr_ad
!                 ddtaui_ad(:)= dtaui_ad(:)*(1._jprb-fmp) - dtaui(:)*fmp_ad
          fmp_ad      = fmp_ad        - dtaui(:)* ddtaui_ad(:)
          dtaui_ad(:) = dtaui_ad(:)   +  ddtaui_ad(:)*(1._jprb-fmp)                     
          ddtaui_ad(:)= 0

!A                taui_thr_ad = taui_thr_ad  - taui_bot_ad
!A                taui_ad     = taui_ad      + taui_bot_ad
!A                taui_bot_ad = 0
          
!A                taui_thr_ad = taui_thr_ad  + taui_top_ad
!A                taui_top_ad = 0
 
!--------------------------------------------------------
          fac_hi =  mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(3) &
                    - mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(4) * MAX( vza, sza ) / 90._jprb
          fac_lo = 0.5_jprb 
          tau_lo = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(1)
          tau_hi = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(2)
!--------------------------------------------------------
!                 taui_thr_ad = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtaui * pi ) ) ) * taui_ad
!                 IF(logtaui > 0.0_jprb .AND. logtaui <  1.0_jprb ) then
!                   logtaui_ad  = taui_ad/(taui + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
!                   taui_thr_ad = taui_thr_ad - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtaui * pi) * taui * logtaui_ad
!                 ENDIF
          IF(logtaui > 0.0_jprb .AND. logtaui <  1.0_jprb ) then
!                   logtaui_ad  = taui_ad/(taui + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
!                   taui_thr_ad = taui_thr_ad - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtaui * pi) * taui * logtaui_ad
            logtaui_ad  = logtaui_ad  - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtaui * pi) * taui * taui_thr_ad

            taui_ad     = taui_ad + logtaui_ad /(taui + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
            logtaui_ad  = 0
          ENDIF
!                 taui_thr_ad = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtaui * pi ) ) ) * taui_ad
          taui_ad = taui_ad + ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtaui * pi ) ) ) * taui_thr_ad
          taui_thr_ad = 0

        ELSE ! taui > 0)
            reffi_bot_ad = 0._jprb
            reffi_top_ad = 0._jprb
        ENDIF
      ELSE
        err = errorstatus_fatal
        THROWM(err.NE.0, "MFASIS-NN has no information on two-layer parameterisation of ice cloud")
      ENDIF

      IF (SIZE(mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams) > 0) THEN
        IF(tauw > 0) THEN
          IF ( tauwi_top > 1E-6 ) THEN
!                    reffwi_top_ad = SUM(-fbotw_ad(:)    * dtaui(:)   * fmp(:) * dreffi(:) +       &
!                                    (1._jprb - fbotw(:))* dtaui_ad(:)* fmp(:) * dreffi(:) +       &
!                                    (1._jprb - fbotw(:))* dtaui(:)   * fmp_ad(:) * dreffi(:) +       &
!                                    (1._jprb - fbotw(:))* dtaui(:)   * fmp(:) * dreffi_ad(:)    ) / tauwi_top     -       &
!                                    reffwi_top/tauwi_top * tauwi_top_ad
            fbotw_ad(:)    = fbotw_ad(:)    - dtaui(:)           * fmp(:)  * dreffi(:) * reffwi_top_ad/tauwi_top
            dtaui_ad(:)    = dtaui_ad(:)    +(1._jprb - fbotw(:))* fmp(:)  * dreffi(:) * reffwi_top_ad/tauwi_top 
            fmp_ad(:)      = fmp_ad(:)      +(1._jprb - fbotw(:))* dtaui(:)* dreffi(:) * reffwi_top_ad/tauwi_top
            dreffi_ad(:)   = dreffi_ad(:)   +(1._jprb - fbotw(:))* fmp(:)  * dtaui(:)  * reffwi_top_ad/tauwi_top
            tauwi_top_ad   = tauwi_top_ad   -                              reffwi_top  * reffwi_top_ad/tauwi_top
            reffwi_top_ad = 0
          ELSE
            reffwi_top_ad = 0._jprb !
          ENDIF
!                 tauwi_top_ad = SUM(-fbotw_ad(:) * dtaui(:)   * fmp(:)    + &
!                         (1._jprb - fbotw(:)) * dtaui_ad(:)* fmp(:)    + &
!                         (1._jprb - fbotw(:)) * dtaui(:)   * fmp_ad(:)     )
          fmp_ad(:)   = fmp_ad(:)   + (1._jprb - fbotw(:)) * dtaui(:) * tauwi_top_ad 
          dtaui_ad(:) = dtaui_ad(:) + (1._jprb - fbotw(:)) * fmp(:)   * tauwi_top_ad 
          fbotw_ad(:) = fbotw_ad(:) -            dtaui(:)  * fmp(:)   * tauwi_top_ad 
          tauwi_top_ad = 0

 
          IF ( tauwi_bot > 1E-6 ) THEN
!                    reffwi_bot_ad = SUM( fbotw_ad(:)    * dtaui(:)   * fmp(:) * dreffi(:) +       &
!                                        fbotw(:)        * dtaui_ad(:)* fmp(:) * dreffi(:) +       &
!                                        fbotw(:)        * dtaui(:)   * fmp_ad(:) * dreffi(:) +       &
!                                        fbotw(:)        * dtaui(:)   * fmp(:) * dreffi_ad(:)    ) / tauwi_bot - &
!                                    reffwi_bot/tauwi_bot * tauwi_bot_ad
            fbotw_ad(:)   = fbotw_ad(:)    +  dtaui(: )* fmp(:)   * dreffi(:) * reffwi_bot_ad/tauwi_bot
            dtaui_ad(:)   = dtaui_ad(:)    +  fbotw(:) * fmp(:)   * dreffi(:) * reffwi_bot_ad/tauwi_bot
            fmp_ad(:)     = fmp_ad(:)      +  fbotw(:) * dtaui(:) * dreffi(:) * reffwi_bot_ad/tauwi_bot
            dreffi_ad(:)  = dreffi_ad(:)   +  fbotw(:) * fmp(:)   * dtaui(:)  * reffwi_bot_ad/tauwi_bot
            tauwi_bot_ad  = tauwi_bot_ad   -                       reffwi_bot * reffwi_bot_ad/tauwi_bot
            reffwi_bot_ad = 0
          ELSE
            reffwi_bot_ad = 0._jprb !
          ENDIF
!                 tauwi_bot_ad = SUM( fbotw_ad(:) * dtaui(:)   * fmp(:)    + &
!                                     fbotw(:)    * dtaui_ad(:)* fmp(:)    + &
!                                     fbotw(:)    * dtaui(:)   * fmp_ad(:)     )
          fbotw_ad(:)  = fbotw_ad(:)  + dtaui(:)   * fmp(:)  * tauwi_bot_ad
          dtaui_ad(:)  = dtaui_ad(:)  + fbotw(:)   * fmp(:)  * tauwi_bot_ad
          fmp_ad(:)    = fmp_ad(:)    + fbotw(:)   * dtaui(:)* tauwi_bot_ad
          tauwi_bot_ad = 0

          IF ( tauw_top > 1E-6 ) THEN
!                    reffw_top_ad = SUM(     -fbotw_ad(:)* dtauw(:)    * dreffw(:) +       &
!                                     (1._jprb-fbotw(:)) * dtauw_ad(:) * dreffw(:) +       &
!                                     (1._jprb-fbotw(:)) * dtauw(:)    * dreffw_ad(:)    ) / tauw_top     -       &
!                                   reffw_top/tauw_top * tauw_top_ad
            fbotw_ad    = fbotw_ad    -                   dtauw(:)  * dreffw(:) * reffw_top_ad/tauw_top
            dtauw_ad(:) = dtauw_ad(:) +(1._jprb-fbotw(:))           * dreffw(:) * reffw_top_ad/tauw_top
            dreffw_ad(:)= dreffw_ad(:)+(1._jprb-fbotw(:))*dtauw(:)              * reffw_top_ad/tauw_top
            tauw_top_ad = tauw_top_ad -                               reffw_top * reffw_top_ad/tauw_top
            reffw_top_ad = 0
          ELSE
            reffw_top_ad = 0._jprb !
          ENDIF

          IF ( tauw_bot > 1E-6 ) THEN
!                    reffw_bot_ad = SUM( fbotw_ad(:)     * dtauw(:)    * dreffw(:) +       &
!                                        fbotw(:)        * dtauw_ad(:) * dreffw(:) +       &
!                                        fbotw(:)        * dtauw(:)    * dreffw_ad(:)    ) / tauw_bot     -       &
!                                   reffw_bot/tauw_bot * tauw_bot_ad
            fbotw_ad(:)  = fbotw_ad(:)  +         dtauw(:) * dreffw(:) * reffw_bot_ad/ tauw_bot
            dtauw_ad(:)  = dtauw_ad(:)  +fbotw(:)          * dreffw(:) * reffw_bot_ad/ tauw_bot
            dreffw_ad(:) = dreffw_ad(:) +fbotw(:)*dtauw(:)             * reffw_bot_ad/ tauw_bot
            tauw_bot_ad  = tauw_bot_ad  -                   reffw_bot  * reffw_bot_ad/ tauw_bot
            reffw_bot_ad = 0
          ELSE
            reffw_bot_ad = 0._jprb !
          ENDIF

!                 tauw_bot_ad = SUM( fbotw_ad(:) * dtauw(:) + fbotw(:) * dtauw_ad(:))
!                 tauw_top_ad = tauw_ad - tauw_bot_ad
          tauw_ad     = tauw_ad     + tauw_top_ad 
          tauw_bot_ad = tauw_bot_ad - tauw_top_ad 
          tauw_top_ad = 0
          fbotw_ad(:) = fbotw_ad(:) + dtauw(:) * tauw_bot_ad
          dtauw_ad(:) = dtauw_ad(:) + fbotw(:) * tauw_bot_ad
          tauw_bot_ad = 0
          !---------------------------------------------------------------
          call comp_fbot_ad(fbotw_ad, tauw_thr_ad, dtauw_ad, tauw_thr, dtauw, nlay, aa_sc)
          !---------------------------------------------------------------
          fbotw_ad = 0

!                 tauw_top_ad = tauw_thr_ad
!                 tauw_bot_ad = tauw_ad    - tauw_thr_ad
!A                tauw_thr_ad = tauw_thr_ad - tauw_bot_ad
!A                tauw_ad     = tauw_ad     + tauw_bot_ad
!A                tauw_bot_ad = 0

          tauw_thr_ad = tauw_thr_ad + tauw_top_ad
          tauw_top_ad = 0

!-----------------------------------
          fac_hi =  mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(3) &
                    - mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(4) * MAX( vza, sza ) / 90._jprb

          fac_lo = 0.5_jprb !
          tau_lo = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(1)
          tau_hi = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(2)
!-----------------------------------
!                 tauw_thr_ad = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtauw * pi ) ) ) * tauw_ad
!                 IF(logtauw > 0.0_jprb .AND. logtauw <  1.0_jprb ) then
!                   logtauw_ad  = tauw_ad/(tauw + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
!                   tauw_thr_ad = tauw_thr_ad - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtauw * pi) * tauw * logtauw_ad
!                 ENDIF
          IF(logtauw > 0.0_jprb .AND. logtauw <  1.0_jprb ) then
!                   logtauw_ad  = tauw_ad/(tauw + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
!                   tauw_thr_ad = tauw_thr_ad - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtauw * pi) * tauw * logtauw_ad
            logtauw_ad = logtauw_ad   - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtauw * pi) * tauw * tauw_thr_ad
            tauw_ad    = tauw_ad      + logtauw_ad /(tauw + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
            logtauw_ad = 0
          ENDIF

!         tauw_thr_ad =               ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtauw * pi ) ) ) * tauw_ad
          tauw_ad     = tauw_ad     + ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtauw * pi ) ) ) * tauw_thr_ad
          tauw_thr_ad = 0
!---------------------=====================
        ELSE ! tauw > 0)
          reffw_bot_ad = 0._jprb
          reffw_top_ad = 0._jprb
          reffwi_bot_ad = 0._jprb
          reffwi_top_ad = 0._jprb
        ENDIF  ! tauw > 0
      ELSE
        err = errorstatus_fatal
        THROWM(err.NE.0, "MFASIS-NN has no information on two-layer parameterisation of water cloud")
      ENDIF

!            tauwi_ad = SUM(   dtaui_ad(:) * fmp(:)     &
!                           +  dtaui   (:) * fmp_ad(:) ) ! mixed-phase cloud ice in water cloud
!            taui_ad  = SUM(   dtaui_ad(:) * (1._jprb - fmp(:))  &
!                           -  dtaui   (:) * fmp_ad(:) ) ! mixed-phase cloud ice in water cloud
      fmp_ad(:)   = fmp_ad(:)   - dtaui(:)           * taui_ad
      dtaui_ad(:) = dtaui_ad(:) + (1._jprb - fmp(:)) * taui_ad
      taui_ad  = 0
      fmp_ad(:)   = fmp_ad(:)   + dtaui(:)           * tauwi_ad
      dtaui_ad(:) = dtaui_ad(:) + fmp(:)             * tauwi_ad
      tauwi_ad = 0

      tau_thr    =  1._jprb
      !---------------------------------------------------------------
      call comp_fbot_ad(fmp_ad, tau_thr_ad, dtauw_ad, tau_thr, dtauw, nlay, aa_sc)
      !---------------------------------------------------------------
      fmp_ad = 0
      tau_thr_ad =  0

      DO j = 1, nlay
        IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
        IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN
!                  tauw_ad = tauw_ad + dtauw_ad(j)
          dtauw_ad(j) = dtauw_ad(j) + tauw_ad
!                  dreffi_ad(j) = aux_ad%ice_dg(j,prof)/2._jprb  !
          aux_ad%ice_dg(j,profad) = aux_ad%ice_dg(j,profad) + dreffi_ad(j)/2._jprb  !
          dreffi_ad(j) = 0

          ! layer eff. radii water
          IF ( profiles(prof)%clw_scheme == clw_scheme_deff ) THEN !  DEFF
!                     dreffw_ad(j) = aux_ad%clw_dg(j,prof)/2._jprb
            aux_ad%clw_dg(j,profad) = aux_ad%clw_dg(j,profad)  + dreffw_ad(j) /2._jprb
          ELSE ! OPAC
            IF ( dtauw(j) .GT. 0 ) THEN
!                        dreffw_ad(j) = dreffw_ad(j)/dtauw(j) - dreffw(j)/(dtauw(j))    * dtauw_ad(j)
              dtauw_ad(j)  = dtauw_ad(j)    -  dreffw(j)/(dtauw(j))    * dreffw_ad(j)
              dreffw_ad(j) = dreffw_ad(j)/dtauw(j)
            ELSE
              dreffw_ad(j) = 0._jprb !
            ENDIF
            

            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
!             dreffw_ad(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i)) &
!                                                                             * (1 - aux%s(prof)%pfraction_surf)  &
!                           -SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir   %opdpext(1:nwcl_max,j,i))   &
!                                                                             *  aux_ad%s(profad)%pfraction_surf
              aux_ad%s(profad)%pfraction_surf  = aux_ad%s(profad)%pfraction_surf   &
                            -SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir   %opdpext(1:nwcl_max,j,i))* dreffw_ad(j)
              trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) = trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i)  &
                            +   wcl_opac_deff(1:nwcl_max)/2._jprb * (1 - aux%s(prof)%pfraction_surf)            * dreffw_ad(j)

            ELSE
!                        dreffw_ad(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i))
              trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) = trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i)  &
                            +   wcl_opac_deff(1:nwcl_max)/2._jprb                                               * dreffw_ad(j)

            ENDIF
          ENDIF
          IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
!                     dtauw_ad(j) = dtauw_ad(j) * (1 - aux%s(prof)%pfraction_surf)  &
!                                   - dtauw_v1(j) * aux_ad%s(profad)%pfraction_surf
!                     dtaui_ad(j) = dtaui_ad(j) * (1 - aux%s(prof)%pfraction_surf)  &
!                                   - dtaui_v1(j) * aux_ad%s(profad)%pfraction_surf
            aux_ad%s(profad)%pfraction_surf = aux_ad%s(profad)%pfraction_surf - dtaui_v1(j) *  dtaui_ad(j)
            dtaui_ad(j) = dtaui_ad(j) * (1 - aux%s(prof)%pfraction_surf)
 
            aux_ad%s(profad)%pfraction_surf = aux_ad%s(profad)%pfraction_surf - dtauw_v1(j) *  dtauw_ad(j)
            dtauw_ad(j) = dtauw_ad(j) * (1 - aux%s(prof)%pfraction_surf)

          ENDIF
          ! layer optical depths
!                  dtauw_ad(j) = SUM(trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i))
!                  dtaui_ad(j) = trans_scatt_ir_ad%opdpext(nwcl_max+1,j,i)
          trans_scatt_ir_ad%opdpext(nwcl_max+1,j,i) = trans_scatt_ir_ad%opdpext(nwcl_max+1,j,i) + dtaui_ad(j)
          trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) = trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) + dtauw_ad(j)

        ENDIF
      ENDDO ! DO j = 1, nlay
      tauw_ad = 0.0_jprb
      taui_ad = 0.0_jprb
      tauwi_top_ad = 0.0_jprb
      tauwi_bot_ad = 0.0_jprb
      reffw_top_ad = 0.0_jprb
      reffi_top_ad = 0.0_jprb
      reffw_bot_ad = 0.0_jprb
      reffi_bot_ad = 0.0_jprb
      reffwi_top_ad = 0.0_jprb
      reffwi_bot_ad = 0.0_jprb
      fpct_ad = 0._jprb
 
      ! Extract optical depths and effective radii per layer
      dtauw_ad(:) = 0._jprb
      dtaui_ad(:) = 0._jprb
      fmp_ad(:) = 0._jprb
      dreffw_ad(:) = 0._jprb
      dreffi_ad(:) = 0._jprb


      ircld_ad%xcol(cc+1,profad) = ircld_ad%xcol(cc+1,profad) + colwei_cc_ad
      ircld_ad%xcol(cc,profad)   = ircld_ad%xcol(cc,profad)   - colwei_cc_ad
      colwei_cc_ad = 0

    ENDDO  ! DO cc = 1, ncolms

    ! --------------------------------------------------------------------------
    ! Compute clear reflectance and add clear column to total reflectabce ->  Is this the correct procedure also for MFASIS-NN?
    ! --------------------------------------------------------------------------
    IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) THEN
      colwei_clr = ircld%xcolclr(prof) ! clear column


      tauw = 0._jprb
      taui = 0._jprb
      tauwi_top = 0._jprb
      tauwi_bot = 0._jprb
      reffw_top = 2.5_jprb
      reffi_top = 10._jprb
      reffw_bot = 2.5_jprb
      reffi_bot = 10._jprb
      reffwi_top = 5._jprb
      reffwi_bot = 5._jprb
      fpct = 1._jprb

      tauw_nn=tauw
      taui_nn=taui
      reffw_top_nn  =reffw_top
      reffw_bot_nn  =reffw_bot
      reffi_top_nn  =reffi_top
      reffi_bot_nn  =reffi_bot
      tauwi_top_nn  =tauwi_top
      tauwi_bot_nn  =tauwi_bot
      reffwi_top_nn =reffwi_top
      reffwi_bot_nn =reffwi_bot
      fpct_nn =fpct

      params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
      CALL nn_transform_input(tauw_nn      ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(taui_idx)
      CALL nn_transform_input(taui_nn      ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
      CALL nn_transform_input(reffw_top_nn ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
      CALL nn_transform_input(reffw_bot_nn ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
      CALL nn_transform_input(reffi_top_nn ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
      CALL nn_transform_input(reffi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
      CALL nn_transform_input(tauwi_top_nn ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
      CALL nn_transform_input(tauwi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
      CALL nn_transform_input(reffwi_top_nn,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
      CALL nn_transform_input(reffwi_bot_nn,params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
      CALL nn_transform_input(fpct_nn      ,params%MIN, params%MAX, params%TRANSFORM)
 
      ALLOCATE(xx(mfasisnn_coefs%nn(nn)%n_input))
      ALLOCATE(yy(mfasisnn_coefs%nn(nn)%n_output))
      ALLOCATE(yy_v1(mfasisnn_coefs%nn(nn)%n_output))
      xx = (/tauw_nn, taui_nn, tauwi_top_nn, tauwi_bot_nn, &
               reffw_top_nn, reffi_top_nn, reffw_bot_nn, reffi_bot_nn, &
              reffwi_top_nn, reffwi_bot_nn, psfc_nn, fpct_nn, sza_nn, vza_nn, alpha_nn/)
      yy(:) = 0._jprb

      CALL fornado_inference_nl( 1,                             &
             mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
             mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
             mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
             mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
             mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
             sigo, xx, yy)
      ! --------------------------------------------------------------------------
      yy_v1=yy
      DO m = 1, mfasisnn_coefs%nn(nn)%n_output
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_A0' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_output(yy(1) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_DH' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_output(yy(2) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_D1' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_output(yy(3) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
      ENDDO

      ! --------------------------------------------------------------------------
      ! Compute refl for model profile albedo (Jonkheid et al.) -> successfully checked wrt LMU
      ! --------------------------------------------------------------------------
      IF ( yy(3) > 1E-4 ) THEN
        eta = (yy(3) - yy(2))
        gamma = ( yy(3) + yy(2) ) * yy(2)
        refl_clr = yy(1) + albedo * gamma/(yy(3) - albedo*eta)
      ELSE
        refl_clr = yy(1) + albedo * (yy(3) + yy(2))
      ENDIF

      DEALLOCATE(xx)

      ! --------------------------------------------------------------------------
      ! Add column contribution to total reflectance
      ! --------------------------------------------------------------------------
      refl = refl + refl_clr * colwei_clr
      
    ELSE
      err = errorstatus_fatal
      THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
    ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column
!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!HHH  TOP TL linear clear air HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
    IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) THEN !clouds, clear-sky column

      ALLOCATE(xx_ad(mfasisnn_coefs%nn(nn)%n_input))
      ALLOCATE(yy_ad(mfasisnn_coefs%nn(nn)%n_output))
      xx_ad(:) = 0
      yy_ad(:) = 0


!     refl_ad = refl_ad + refl_clr_ad * colwei_clr + refl_clr * colwei_clr_ad
      refl_clr_ad   = refl_clr_ad   + colwei_clr *  refl_ad
      colwei_clr_ad = colwei_clr_ad + refl_clr   *  refl_ad

      ! --------------------------------------------------------------------------
      ! Compute refl for model profile albedo (Jonkheid et al.) -> successfully checked wrt LMU
      ! --------------------------------------------------------------------------
      IF ( yy(3) > 1E-4 ) THEN
        eta = (yy(3) - yy(2))
        gamma = ( yy(3) + yy(2) ) * yy(2)
        hvar=albedo *gamma   /(yy(3) - albedo*eta)**2

!       refl_clr_ad = yy_ad(1) + albedo_ad*gamma   /(yy(3) - albedo*eta) &
!       + albedo   *gamma_ad/(yy(3) - albedo*eta) &
!       + hvar * ( -yy_ad(3) + albedo_ad*eta + albedo*eta_ad )
        albedo_ad = albedo_ad + gamma   /(yy(3) - albedo*eta) * refl_clr_ad
        albedo_ad = albedo_ad + hvar * eta                    * refl_clr_ad
        gamma_ad  = gamma_ad  + albedo /(yy(3) - albedo*eta)  * refl_clr_ad
        eta_ad    = eta_ad    + hvar * albedo                 * refl_clr_ad
        yy_ad(3)  = yy_ad(3)  - hvar                          * refl_clr_ad
        yy_ad(1)  = yy_ad(1)  +                                 refl_clr_ad
        refl_clr_ad = 0._jprb

!       gamma_ad = ( yy_ad(3) + yy_ad(2) ) * yy(2) + &
!                      ( yy   (3) + yy   (2) ) * yy_ad(2)
        yy_ad(3) = yy_ad(3)   + yy(2)                         * gamma_ad
        yy_ad(2) = yy_ad(2)   + ( yy(3) + 2._jprb * yy(2) )  * gamma_ad
        gamma_ad = 0._jprb

!       eta_ad = (yy_ad(3) - yy_ad(2))
        yy_ad(3) = yy_ad(3) + eta_ad
        yy_ad(2) = yy_ad(2) - eta_ad
        eta_ad = 0
      ELSE
!       refl_clr = yy(1) + albedo * (yy(3) + yy(2))
        albedo_ad = albedo_ad + (yy(3) + yy(2))               * refl_clr_ad
        yy_ad(3)  = yy_ad(3)  + albedo                        * refl_clr_ad
        yy_ad(2)  = yy_ad(2)  + albedo                        * refl_clr_ad
        yy_ad(1)  = yy_ad(1)  + refl_clr_ad
        refl_clr_ad = 0._jprb
      ENDIF


!======================================================

      DO m = 1, mfasisnn_coefs%nn(nn)%n_output
        ! mfasisnn_coefs%nn(nn)%out(m)%TRANSFORM
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_A0' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_ad(yy_v1(1) , yy_ad(1) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_DH' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_ad(yy_v1(2) , yy_ad(2) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_D1' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_ad(yy_v1(3) , yy_ad(3) ,params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
      ENDDO

      CALL fornado_inference_ad( 1_jpim,                             &
             mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
             mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
             mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
             mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
            !  mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
             sigo, xx_ad, yy_ad)
!         xx_ad = (/tauw_ad, taui_ad, tauwi_top_ad, tauwi_bot_ad, &
!                      reffw_top_ad, reffi_top_ad, reffw_bot_ad, reffi_bot_ad, &
!                     reffwi_top_ad, reffwi_bot_ad, psfc_ad, fpct_ad, 0._jprb, 0._jprb, 0._jprb/)

      tauw_ad      = tauw_ad      + xx_ad(1)
      taui_ad      = taui_ad      + xx_ad(2)
      tauwi_top_ad = tauwi_top_ad + xx_ad(3)
      tauwi_bot_ad = tauwi_bot_ad + xx_ad(4)
      reffw_top_ad = reffw_top_ad + xx_ad(5)
      reffi_top_ad = reffi_top_ad + xx_ad(6)
      reffw_bot_ad = reffw_bot_ad + xx_ad(7)
      reffi_bot_ad = reffi_bot_ad + xx_ad(8)
      reffwi_top_ad = reffwi_top_ad + xx_ad(9)
      reffwi_bot_ad = reffwi_bot_ad + xx_ad(10)
      psfc_ad       = psfc_ad       + xx_ad(11)
      fpct_ad       = fpct_ad       + xx_ad(12)

      xx_ad = 0


!       err = errorstatus_fatal
!       THROWM(err.NE.0, "MFASIS-NN not yet implemented for  this version" )

      params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
      CALL nn_transform_input_ad(tauw, tauw_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(taui_idx)
      CALL nn_transform_input_ad(taui, taui_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
      CALL nn_transform_input_ad(reffw_top, reffw_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
      CALL nn_transform_input_ad(reffw_bot, reffw_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
      CALL nn_transform_input_ad(reffi_top, reffi_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
      CALL nn_transform_input_ad(reffi_bot, reffi_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
      CALL nn_transform_input_ad(tauwi_top, tauwi_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
      CALL nn_transform_input_ad(tauwi_bot, tauwi_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
      CALL nn_transform_input_ad(reffwi_top, reffwi_top_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
      CALL nn_transform_input_ad(reffwi_bot, reffwi_bot_ad , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
      CALL nn_transform_input_ad(fpct, fpct_ad , params%MIN, params%MAX, params%TRANSFORM)

      DEALLOCATE(xx_ad,yy_ad, yy, yy_v1, sigo)
!==================================================================================================
!==================================================================================================
      tauw_ad = 0._jprb
      taui_ad = 0._jprb
      tauwi_top_ad = 0._jprb
      tauwi_bot_ad = 0._jprb
      reffw_top_ad = 0._jprb
      reffi_top_ad = 0._jprb
      reffw_bot_ad = 0._jprb
      reffi_bot_ad = 0._jprb
      reffwi_top_ad = 0._jprb
      reffwi_bot_ad = 0._jprb
      fpct_ad = 0._jprb

!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
      ircld_ad%xcolclr(profad) = ircld_ad%xcolclr(profad) + colwei_clr_ad
      colwei_clr_ad = 0
    ELSE
      err = errorstatus_fatal
      THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
    ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column

!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!HHH  BOTTOM TL linear clear air HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!==================================================================================
!   First linear part (before cloud column loop)
!==================================================================================
    refl_ad=0._jprb
    refl_clr_ad=0._jprb
    IF(albedo < 1._jprb) THEN
!     albedo_ad= reflectance_ad(i)%refl_out * pi
      reflectance_ad(i)%refl_out = reflectance_ad(i)%refl_out + pi * albedo_ad
      albedo_ad= 0._jprb
    ELSE
      albedo_ad= 0._jprb
    ENDIF
!      psfc_ad= profiles_ad(profad)%s2m%p
!      psfc0_ad= psfc_ad
!   CALL nn_transform_input_ad(psfc, psfc_ad, params%MIN , params%MAX, params%TRANSFORM) 
    params=mfasisnn_coefs%nn(nn)%in(psfc_idx)
    CALL nn_transform_input_ad(psfc, psfc_ad , params%MIN, params%MAX, params%TRANSFORM)
    psfc_ad = psfc_ad + psfc0_ad
    psfc0_ad= 0
    profiles_ad(profad)%s2m%p =  profiles_ad(profad)%s2m%p + psfc_ad
    psfc_ad = 0
    

!==================================================================================

  ENDDO ! nchanprof



  ! --------------------------------------------------------------------------
  ! Tidy up
  ! --------------------------------------------------------------------------

  DEALLOCATE( dtauw,     &
              dtaui,     &
              dtauw_v1,  &
              dtaui_v1,  &
              dtauw_ad,  &
              dtaui_ad,  &
              ddtaui_ad, &
              fmp,       &
              fmp_ad,    &
              dreffw,    &
              dreffw_ad, &
              dreffi,    &
              dreffi_ad, &
              fbotw,     &
              fboti,     &
              fbotw_ad,  &
              fboti_ad,  &
              stat = err )
  THROWM(err.NE.0, "Deallocation of memory for rttov_mfasis_nn_ad failed")

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN_AD', 1_jpim, ZHOOK_HANDLE)

  CATCH

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN_AD', 1_jpim, ZHOOK_HANDLE)

contains
  !----------------------------------------------------------------------------------------------
  !   Compute weighting factors fbot for bottom layers
  !----------------------------------------------------------------------------------------------

  SUBROUTINE  comp_fbot_ad(fbot_ad, tau_thr_ad, dtaul_ad, tau_thr, dtaul, nlay, aa_sc)

    IMPLICIT NONE
!===========================================================
    INTEGER(jpim),    INTENT(IN)    :: nlay
    REAL(jprb),       INTENT(INOUT) :: fbot_ad(nlay) 
    REAL(jprb),       INTENT(INOUT) :: tau_thr_ad
    REAL(jprb),       INTENT(INOUT) :: dtaul_ad(:)
    REAL(jprb),       INTENT(IN)    :: tau_thr
    REAL(jprb),       INTENT(IN)    :: dtaul(:)
    REAL(jprb),       INTENT(IN)    :: aa_sc

    REAL(jprb)                      :: fbot (nlay)
    REAL(jprb)                      :: ffunc(nlay)
    REAL(jprb)                      :: ffunc_ad(nlay)

    REAL(jprb)                      :: fbot_old, ffunc_old
    REAL(jprb)                      :: fbot_old_ad, ffunc_old_ad
    REAL(jprb)                      :: aa, aa_lim
    REAL(jprb)                      :: aa_ad
    REAL(jprb)                      :: aaa(0:nlay)
    INTEGER(jpim)                   :: j, jj

    aa_lim = aa_sc * 0.5_jprb  * pi

!------------------------------------------------------------------------------
!     1. compute nl quantities "aaa(:), fbot(:)" for linear computations below
!------------------------------------------------------------------------------
    fbot(:)   = 0._jprb
    ffunc(:)  = 0._jprb
    fbot_old  = 0._jprb
    ffunc_old = 0._jprb
    aa = - tau_thr
    aaa(:) = 0
    aaa(0) = aa
    if(aaa(0) > -aa_sc * 0.5_jprb  * pi ) THEN
      ffunc_old = 0.5_jprb*(aaa(0)+ aa_sc*0.5_jprb*pi - aa_sc*COS(aaa(0)/aa_sc) )
    endif
    DO j = 1, nlay
      IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT ! Layer below surf.
      IF(dtaul(j) /= 0) THEN
!         aa = taui_int - tau_thr
        aa = aa + dtaul(j)
        aaa(j)=aa
        IF(aa <= -aa_lim) cycle
        IF(aa >=  aa_lim) THEN
          ffunc(j) = aa
        ELSE
          ffunc(j) = 0.5_jprb*(aa+ aa_lim - aa_sc*COS(aa/aa_sc) )
        ENDIF
        fbot(j) = (ffunc(j) - ffunc_old)/dtaul(j)
        fbot_old  = fbot(j)
        ffunc_old = ffunc(j)

        IF(aa >=  aa_lim) THEN !set all remaining to one and quit
          DO jj=j+1, nlay
            fbot(jj) = 1._jprb
            aaa(jj)=aa
          ENDDO
          EXIT
        ENDIF
      ELSE
        aaa(j)=aa
        fbot(j)  = fbot_old
      ENDIF
    ENDDO
!------------------------------------------------------------------------------
!     2. start linear computations 
!------------------------------------------------------------------------------

    ffunc_ad(:)  = 0._jprb
    fbot_old_ad  = 0._jprb
    ffunc_old_ad = 0._jprb
    aa_ad = 0
    DO j = nlay, 1, -1
      IF (j > aux%s(prof)%nearestlev_surf - 1) CYCLE ! Layer below surf.
      IF( aaa(j-1) >= aa_lim) THEN
        cycle
      ENDIF
      IF(dtaul(j) /= 0) THEN
!         IF(aaa(j) <= -aa_lim) cycle
        IF(aaa(j) > -aa_lim) THEN

!1)       fbot_ad(j)   = (ffunc_ad(j) - ffunc_old_ad)/dtaul(j) - &
!                         fbot(j) * dtaul_ad(j)/dtaul(j)
!2)       ffunc_old_ad = ffunc_ad(j)
!3)       fbot_old_ad  = fbot_ad(j)

!3)------------------------
          fbot_ad(j) = fbot_ad(j) + fbot_old_ad 
          fbot_old_ad= 0
!2)------------------------

          ffunc_ad(j)= ffunc_ad(j) + ffunc_old_ad
          ffunc_old_ad = 0
!1)------------------------
          ffunc_ad(j)= ffunc_ad(j) + fbot_ad(j)/dtaul(j)
!         ffunc_old_ad= ffunc_old_ad - fbot_ad(j)/dtaul(j)
          ffunc_old_ad=              - fbot_ad(j)/dtaul(j)
          dtaul_ad(j) = dtaul_ad(j)  - fbot_ad(j)/dtaul(j) * fbot(j)
          fbot_ad(j)  = 0
!  ------------------------

          IF(aaa(j) >=  aa_lim) THEN
!           ffunc_ad(j) = aa_ad
            aa_ad = aa_ad + ffunc_ad(j)
            ffunc_ad(j) = 0
          ELSE
!           ffunc_ad(j) = 0.5_jprb*aa_ad* (1._jprb + sin(aaa(j)/aa_sc))
            aa_ad= aa_ad + ffunc_ad(j) * 0.5_jprb* (1._jprb + sin(aaa(j)/aa_sc))
            ffunc_ad(j) = 0
          ENDIF
!         IF(aaa(j) >=  aa_lim) THEN !set all remaining to one and quit
!           DO jj=j+1, nlay
!             fbot_ad(jj) = 0
!           ENDDO
!           EXIT
!         ENDIF
        ENDIF
!         aa_ad = aa_ad + dtaul_ad(j)
        dtaul_ad(j) = dtaul_ad(j) + aa_ad
      ELSE
!         fbot_ad(j)  = fbot_old_ad
        fbot_old_ad  = fbot_old_ad + fbot_ad(j)
        fbot_ad(j)   = 0
        cycle
      ENDIF
    ENDDO
!     if(aaa(0) > -aa_sc * 0.5_jprb  * pi ) THEN
!       ffunc_old_ad = 0.5_jprb*aa_ad* (1._jprb + sin(aaa(0)/aa_sc))
!     endif
    if(aaa(0) > -aa_sc * 0.5_jprb  * pi ) THEN
!       ffunc_old_ad = 0.5_jprb*aa_ad* (1._jprb + sin(aaa(0)/aa_sc))
      aa_ad= aa_ad + ffunc_old_ad * 0.5_jprb* (1._jprb + sin(aaa(0)/aa_sc))
      ffunc_old_ad = 0
    endif
!     aa = - tau_thr
    tau_thr_ad = tau_thr_ad - aa_ad

    fbot_ad(:)   = 0._jprb
!     ffunc_ad(:)  = 0._jprb
!     fbot_old_ad  = 0._jprb
!     ffunc_old_ad = 0._jprb
!     aa_ad = 0
  END SUBROUTINE comp_fbot_ad


  subroutine fornado_inference_ad( n_samples,                             &
                               n_input, n_output, n_hidden, n_nodes,  &
                                         act_h,    act_o,             &
                               weight_i, weight_h, weight_o,          &
!                                bias_i,   bias_h,   bias_o,            &
                         sigo,    xb,    yb )
!                                x, xb, y, yb )

    ! adjoint version of fornado_inference.F90

    implicit none
    integer, parameter :: sp = jprb 

    integer(jpim), intent(in) :: n_samples, n_input, n_output, n_hidden, n_nodes

    integer(jpim), intent(in) :: act_h, act_o

    real(sp), dimension( n_input, n_nodes ),             intent(in) :: weight_i
    real(sp), dimension( n_nodes, n_nodes, n_hidden-1 ), intent(in) :: weight_h
    real(sp), dimension( n_nodes, n_output ),            intent(in) :: weight_o

    ! real(sp), dimension( n_nodes ),                      intent(in) :: bias_i
    ! real(sp), dimension( n_nodes, n_hidden-1 ),          intent(in) :: bias_h
    ! real(sp), dimension( n_output ),                     intent(in) :: bias_o

!   real(sp), dimension( n_samples, n_input  ),          intent(in) :: x
    real(sp), dimension( n_samples,0:n_hidden, n_nodes ),intent(in) :: sigo

    real(sp), dimension( n_samples, n_input  ),      intent(inout) :: xb
    real(sp), dimension( n_samples, n_output ),         intent(in) :: yb

    ! real(sp), dimension( n_samples, n_output )                     ::    y
    ! real(sp), dimension( n_nodes, 0:n_hidden ) :: sigi
    real(sp), dimension( n_nodes, 0:n_hidden ) :: sigib, sigob

    integer(jpim) :: i_sample, i, j, l

    ! unvectorized version: inner loop over nodes -------------------------------


    ! loop over all input data sets
    do i_sample = 1, n_samples
      !-----------------------------------------------------------------------------
      ! start from output layer and compute adjoint backwards
      !-----------------------------------------------------------------------------

      ! from output layer to layer n_hidden-1 ......................................

      ! adjoint of activation function
      l=n_hidden
      ! adjoint of activation function ..........................................
      call actfunc_b( act_o, sigo(i_sample, l, 1:n_output), sigob(1:n_output,l), yb(i_sample,:) )
!      call actfunc_b( act_o, sigo(i_sample, 1:n_output,n_hidden), sigob(1:n_output,n_hidden), yb(i_sample,:) )
      
      sigib(:,n_hidden) = 0.0
      do i=n_output,1,-1
        do j=n_nodes,1,-1
          sigib(j,n_hidden) = sigib(j,n_hidden) + weight_o(j, i) * sigob(i,n_hidden)
        end do
        sigob(i,n_hidden) = 0.0
      end do

      ! loop over hidden layers
      do l = n_hidden-1, 1, -1

        ! adjoint of activation function ..........................................
        call actfunc_b( act_h, sigo(i_sample,l, :), sigob(:,l), sigib(:,l+1) )
!         call actfunc_b( act_h, sigo(i_sample, :,l), sigob(:,l), sigib(:,l+1) )
        
        sigib(:,l) = 0.0
        do i=n_nodes,1,-1
          do j=n_nodes,1,-1
            sigib(j,l) = sigib(j,l) + weight_h(j, i, l) * sigob(i,l)
          end do
          sigob(i,l) = 0.0
        end do
  
      end do ! layer loop

      ! from first hidden layer to input layer .....................................
  
      ! adjoint of activation function
      l=0
      call actfunc_b( act_h, sigo(i_sample,l, :), sigob(:,l), sigib(:,l+1) )
!      call actfunc_b( act_h, sigo(i_sample, :,0), sigob(:,0), sigib(:,1) )
  
      xb(i_sample,:) = 0.0
      do i=n_nodes,1,-1
        do j=n_input,1,-1
        xb(i_sample,j) = xb(i_sample,j) + weight_i(j, i) * sigob(i,0)
        end do
        sigob(i,0) = 0.0
      end do
      
    end do ! sample loop
  

  end subroutine fornado_inference_ad

  pure subroutine actfunc_b( functype, so, sid,     sod )

    ! activation functions (nonlinear + tangent linear version)

    integer, parameter :: sp = jprb !CSt: sp = kind(1.0)

    integer(jpim), intent(in) :: functype

    real(sp), dimension(:),    intent(in) :: so, sod
!   real(sp), dimension(:), intent(inout) :: si, sid
    real(sp), dimension(:), intent(inout) ::     sid

    ! integer, parameter :: k1 = 1._jprb 

    select case (functype)
    case (0)               ! linear
      sid = sod
    case (1)               ! relu
      !where( so .gt. 0 )
      !   sid = sod
      !   si  = so
      !elsewhere
      !   sid = 0._jprb
      !   si  = 0._jprb
      !endwhere
      sid = sod * (SIGN(1._jprb,so)+1._jprb)*0.5_jprb

    case (2)               ! elu
      !where( so .gt. 0 )
      !   sid = sod
      !   si  = so
      !elsewhere
      !   sid = sod * exp(so)
      !   si  =       exp(so) - 1._jprb
      !endwhere
      sid = sod * ( (SIGN(1._jprb,so)+1._jprb)*0.5_jprb + (exp(so) - 1._jprb) * (1._jprb - (SIGN(1._jprb,so)+1._jprb)*0.5_jprb) )

    case (3)               ! softplus
      sid = (sod*exp(so)) / ( exp(so) + 1._jprb )

    case (99) ! csu
      !where( so .gt. 0 )
      !   si  = so
      !   sid = sod
      !elsewhere( so .lt. -2 )
      !   si = -1._jprb
      !   sid = 0._jprb
      !elsewhere
      !   si  = -1._jprb + 0.25*(so+2)**2
      !   sid =        0.5 *(so+2)*sod
      !endwhere
      ! (SIGN(1,so  )+1)*0.5 is 1 for so> 0, else 0
      ! (SIGN(1,so+2)+1)*0.5 is 1 for so>-2, else 0
      sid = ((SIGN(1._jprb,so)+1._jprb)*0.5_jprb) * sod + &
            (1._jprb-(SIGN(1._jprb,so)+1._jprb)*0.5_jprb) * &
            (SIGN(1._jprb,so+2._jprb)+1._jprb)*0.5_jprb * 0.5_jprb*(so+2._jprb)*sod
      !     [       so .gt. 0      ]         [[       so .lt. 0       ] .and. [      so .gt. -2      ]]

    case default
      sid = -999.999_jprb
    end select

! sid = sod*(so+k1)

  end subroutine actfunc_b



  !----------------------------------------------------------------------------------------------
  !
  !   Transform input variable for NN (train_model.py normalize())
  !
  !----------------------------------------------------------------------------------------------
  SUBROUTINE nn_transform_input_ad(var_in, var_ad, minv, maxv, trafo )
    IMPLICIT NONE
!   REAL(jprb),       INTENT(INOUT) :: var    
    REAL(jprb),       INTENT(IN)    :: var_in    
    REAL(jprb),       INTENT(INOUT) :: var_ad
    REAL(jprb),       INTENT(IN)    :: minv, maxv
    CHARACTER(LEN=32),INTENT(IN)    :: trafo

    REAL(jprb)                      :: var
    IF (var_in < minv .or. var_in > maxv ) var_ad = 0
    var = MIN( MAX( var_in,minv ), maxv )

    IF (TRIM(trafo) == 'LIN') THEN ! uniform distribution between min and max
      var_ad=  var_ad    / (maxv - minv)
!        var = (var - minv) / (maxv - minv)
    ELSE IF (TRIM(trafo) == 'LOG') THEN ! uniform distribution in log(parameter)
      var_ad = (var_ad/var)        / (LOG(maxv) - LOG(minv))
!        var = (LOG(var) - LOG(minv)) / (LOG(maxv) - LOG(minv))
    ENDIF
  END SUBROUTINE nn_transform_input_ad
  !----------------------------------------------------------------------------------------------
  !
  !   Transform output variable for NN (train_model.py denormalize(), added
  !   sqrtnone trafo)
  !
  !----------------------------------------------------------------------------------------------
  SUBROUTINE nn_transform_out_ad( var_in, var_ad, minv, maxv, trafo )
    IMPLICIT NONE
!   REAL(jprb),       INTENT(INOUT) :: var
    REAL(jprb),       INTENT(IN)    :: var_in
    REAL(jprb),       INTENT(INOUT) :: var_ad
    REAL(jprb),       INTENT(IN)    :: minv, maxv
    CHARACTER(LEN=32),INTENT(IN)    :: trafo

    REAL(jprb)                      :: var
    var = var_in
    IF (TRIM(trafo) /= 'SQRTNONE') THEN
      IF (var < minv .or. var > maxv ) var_ad = 0
      var = MIN( MAX( var_in,minv ), maxv )
    ENDIF 

    IF (TRIM(trafo) == 'LIN') THEN
      var_ad=  var_ad    * (maxv - minv)
!        var = minv + var*(maxv - minv)
    ELSE IF (TRIM(trafo) == 'LOG') THEN
      ! change var first to use chain rule for differciati0n
      var = EXP( LOG(minv) + var*( LOG(maxv) - LOG(minv) ) )
      var_ad = var_ad*( LOG(maxv) - LOG(minv) )* var
    ELSE IF (TRIM(trafo) == 'SQRT') THEN
      var_ad = 2._jprb* ( SQRT(minv) + var*( SQRT(maxv) - SQRT(minv) ) ) &
                * ( SQRT(maxv) - SQRT(minv) ) * var_ad
!        var =          ( SQRT(minv) + var*( SQRT(maxv) - SQRT(minv) ) )**2
    ELSE IF (TRIM(trafo) == 'SQRTNONE') THEN
      var_ad = var*2._jprb *var_ad
!        var = var**2
    ENDIF
  END SUBROUTINE nn_transform_out_ad




END SUBROUTINE rttov_mfasis_nn_ad
