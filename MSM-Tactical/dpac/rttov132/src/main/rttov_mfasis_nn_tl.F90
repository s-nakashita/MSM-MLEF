! Description:
!> @file
!!   TL of MFASIS-NN fast visible/near-IR scattering model.
!
!> @brief
!!   TL of MFASIS-NN fast visible/near-IR scattering model.
!!
!!
!! @param[out]    err               status on exit
!! @param[in]     chanprof          specifies channels and profiles to simulate
!! @param[in]     chanflag          flags to indicate which channels with NN  available
!! @param[in]     opts              options to configure the simulations
!! @param[in]     profiles          input atmospheric profiles and surface variables
!! @param[in]     profiles_tl       input profile perturbations
!! @param[in]     coefs             coefficients structure for instrument to simulate
!! @param[in]     ircld             information on cloud columns
!! @param[in]     ircld_tl          cloud column perturbations
!! @param[in]     aux               additional internal profile variables
!! @param[in]     aux_tl            additional internal profile variable perturbations
!! @param[in]     reflectance       surface BRDFs
!! @param[in]     reflectance_tl    surface BRDF perturbations
!! @param[in]     solar_spectrum    TOA solar irradiance for each channel
!! @param[in]     trans_scatt_ir    cloud/aerosol optical depths
!! @param[in]     trans_scatt_ir_tl cloud/aerosol optical depth perturbations
!! @param[in,out] radiance_tl       output radiance and BRF perturbations
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
SUBROUTINE rttov_mfasis_nn_tl( &
            err,              &
            chanprof,         &
            chanflag,         &
            opts,             &
            profiles,         &
            profiles_tl,      &
            coefs,            &
            ircld,            &
            ircld_tl,         &
            aux,              &
            aux_tl,           &
            reflectance,      &
            reflectance_tl,   &
            solar_spectrum,   &
            trans_scatt_ir,   &
            trans_scatt_ir_tl,&
            radiance_tl)
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
  nwcl_max

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
  TYPE(rttov_profile),                INTENT(IN)    :: profiles_tl(:)
  TYPE(rttov_coefs),                  INTENT(IN)    :: coefs
  TYPE(rttov_ircld),                  INTENT(IN)    :: ircld
  TYPE(rttov_ircld),                  INTENT(IN)    :: ircld_tl
  TYPE(rttov_profile_aux),            INTENT(IN)    :: aux
  TYPE(rttov_profile_aux),            INTENT(IN)    :: aux_tl
  TYPE(rttov_reflectance),            INTENT(IN)    :: reflectance(SIZE(chanprof))
  TYPE(rttov_reflectance),            INTENT(IN)    :: reflectance_tl(SIZE(chanprof))
  REAL(jprb),                         INTENT(IN)    :: solar_spectrum(SIZE(chanprof))
  TYPE(rttov_transmission_scatt_ir),  INTENT(IN)    :: trans_scatt_ir
  TYPE(rttov_transmission_scatt_ir),  INTENT(IN)    :: trans_scatt_ir_tl
  TYPE(rttov_radiance),               INTENT(INOUT) :: radiance_tl
!INTF_END

#include "rttov_errorreport.interface"

  INTEGER(jpim)              :: nchanprof, prof, chan, nn, ncolms
  INTEGER(jpim)              :: nlay
  INTEGER(jpim)              :: i, j, cc
  TYPE(rttov_coef_mfasis_nn) :: mfasisnn_coefs
  TYPE(rttov_mfasis_nn_params):: params

  REAL(jprb)                 :: albedo
  REAL(jprb)                 :: albedo_tl, hvar
  REAL(jprb)                 :: mu, mu0, cad, alpha_deg
  REAL(jprb)                 :: colwei_clr, colwei_cc
  REAL(jprb)                 :: colwei_clr_tl, colwei_cc_tl
  REAL(jprb)                 :: refl_tl, refl_cc_tl, refl_clr_tl

  REAL(jphook) :: zhook_handle

  !XXX NEW MFASIS-NN introduced
  INTEGER(jpim)              :: m, nlev_surf
  INTEGER(jpim)              :: tauw_idx, taui_idx, tauwi_top_idx, tauwi_bot_idx
  ! INTEGER(jpim)              :: reffw_idx, reffi_idx
  INTEGER(jpim)              :: reffw_top_idx, reffi_top_idx, reffw_bot_idx, reffi_bot_idx, reffwi_top_idx
  INTEGER(jpim)              :: reffwi_bot_idx, psfc_idx, fpct_idx
  INTEGER(jpim)              :: vza_idx, sza_idx, alpha_idx
  REAL(jprb)                 :: tauw   , taui   , tauwi   , tauwi_top   , tauwi_bot
  REAL(jprb)                 :: tauw_tl, taui_tl, tauwi_tl, tauwi_top_tl, tauwi_bot_tl
  REAL(jprb)                 :: reffw_top   , reffi_top   , reffw_bot   , reffi_bot   , reffwi_top   , reffwi_bot
  REAL(jprb)                 :: reffw_top_tl, reffi_top_tl, reffw_bot_tl, reffi_bot_tl, reffwi_top_tl, reffwi_bot_tl
  REAL(jprb)                 :: psfc, fpct
  REAL(jprb)                 :: psfc_tl, psfc0_tl, fpct_tl
  REAL(jprb)                 :: psfc_nn, fpct_nn
  REAL(jprb)                 :: sza, vza
  REAL(jprb)                 :: sza_nn, vza_nn, alpha_nn
  REAL(jprb)                 :: tauw_nn, taui_nn, reffw_top_nn, reffw_bot_nn 
  REAL(jprb)                 :: reffi_top_nn , reffi_bot_nn , tauwi_top_nn , tauwi_bot_nn , reffwi_top_nn, reffwi_bot_nn
  REAL(jprb)                 :: fac_hi, fac_lo, tau_lo, tau_hi, tau_thr, tau_thr_v1, tauw_top, taui_top, tau
  REAL(jprb)                 ::            tau_thr_tl, tauw_top_tl, taui_top_tl, tau_tl
  REAL(jprb)                 :: tauw_thr, taui_thr, tauw_thr_tl, taui_thr_tl, logtauw, logtaui,  logtauw_tl, logtaui_tl

  REAL(jprb)                 :: tauw_bot, taui_bot
  REAL(jprb)                 :: tauw_bot_tl, taui_bot_tl
  REAL(jprb)                 :: refl_cc, refl_clr, refl
  REAL(jprb)                 :: eta, gamma
  REAL(jprb)                 :: eta_tl, gamma_tl
  REAL(jprb),    ALLOCATABLE :: dtauw(:), dtaui(:), fmp(:), dreffw(:), dreffi(:), fbotw(:), fboti(:)
  REAL(jprb),    ALLOCATABLE :: dtauw_v1(:), dtaui_v1(:)
  REAL(jprb),    ALLOCATABLE :: dtauw_tl(:), dtaui_tl(:), fmp_tl(:), dreffw_tl(:), dreffi_tl(:), fbotw_tl(:), fboti_tl(:)
  REAL(jprb),    ALLOCATABLE ::             ddtaui_tl(:)
  REAL(jprb),    ALLOCATABLE :: xx(:)   , yy(:), sigo(:,:,:), yy_v1(:)
  REAL(jprb),    ALLOCATABLE :: xx_tl(:), yy_tl(:)
  ! --------------------------------------------------------------------------------------

  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN_TL', 0_jpim, ZHOOK_HANDLE)

  ! TODO: aerosol functionality not included

  ! --------------------------------------------------------------------------
  ! Initialisation
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
              dtauw_tl(nlay)              , &   ! layer optical depth of ice cloud
              dtaui_tl(nlay)              , &   ! layer optical depth of ice cloud
             ddtaui_tl(nlay)              , &   ! layer optical depth of ice cloud
              fmp(nlay)                   , &   ! factor that defines onset of mixed-phase cloud
              fmp_tl(nlay)                , &   ! factor that defines onset of mixed-phase cloud
              dreffw(nlay)                , &   ! layer effective water cloud radii
              dreffw_tl(nlay)             , &   ! layer effective water cloud radii
              dreffi(nlay)                , &   ! layer effective ice cloud radii
              dreffi_tl(nlay)             , &   ! layer effective ice cloud radii
              fbotw(nlay)                  , &
              fboti(nlay)                  , &
              fbotw_tl(nlay)               , &
              fboti_tl(nlay)               , &
                          STAT=err)             ! 2-layer param.: factor that defines boarder between 2 layers  
    THROWM(err.NE.0, "Allocation of memory for rttov_mfasis_nn_tl failed")
  ELSE ! IF (mfasisnn_coefs%version >= 1)
    err = errorstatus_fatal
    THROWM(err.NE.0, "MFASIS-NN version not implemented")
  ENDIF

  ! --------------------------------------------------------------------------
  ! Channel loop
  ! --------------------------------------------------------------------------
  DO i = 1, nchanprof

    IF (.NOT. chanflag(i)) CYCLE
    prof = chanprof(i)%prof
    chan = chanprof(i)%chan
    nn = mfasisnn_coefs%channel_nn_index(chan)
    nlev_surf = aux%s(prof)%nearestlev_surf

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
    alpha_deg =  rad2deg*(ACOS(mu*mu0 + SQRT(1._jprb - mu**2)*SQRT(1._jprb - mu0**2)*cad)) !

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

!==================================================================================
!   First linear part (before cloud column loop)
!==================================================================================
    psfc_tl= profiles_tl(prof)%s2m%p
    psfc0_tl= psfc_tl
    params=mfasisnn_coefs%nn(nn)%in(psfc_idx)
    CALL nn_transform_input_tl(psfc, psfc_tl , params%MIN, params%MAX, params%TRANSFORM)

    IF(albedo < 1._jprb) THEN
      albedo_tl= reflectance_tl(i)%refl_out * pi
    ELSE
      albedo_tl= 0._jprb
    ENDIF
    refl_tl=0._jprb
    refl_clr_tl=0._jprb
!==================================================================================
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
          IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
          IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN

            ! layer optical depths
            dtauw(j) = SUM(trans_scatt_ir%opdpext(1:nwcl_max,j,i))
            dtaui(j) = trans_scatt_ir%opdpext(nwcl_max+1,j,i)
            dtauw_v1(j) =dtauw(j)
            dtaui_v1(j) =dtaui(j)
            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
              dtauw(j) = dtauw(j) * (1._jprb - aux%s(prof)%pfraction_surf) 
              dtaui(j) = dtaui(j) * (1._jprb - aux%s(prof)%pfraction_surf)
            ENDIF

            ! layer eff. radii water
            IF ( profiles(prof)%clw_scheme == clw_scheme_deff ) THEN ! DEFF
              dreffw(j) = aux%clw_dg(j,prof)/2._jprb
            ELSE ! OPAC
 
              IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
                ! Modify optical depth in partial layer above surface
                dreffw(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir%opdpext(1:nwcl_max,j,i)) &
                                * (1._jprb - aux%s(prof)%pfraction_surf)
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
!                  fpct = profiles(prof)%p(j) / psfc
            fpct = profiles(prof)%p(j+1) - (tau - tau_thr)/(dtauw(j) + dtaui(j)) * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
            fpct = fpct / psfc
            EXIT
          ENDIF
        ENDDO

        ! --------------------------------------------------------------------------
        ! Transform input variables (by normalization) to produce NN input -> successfully checked wrt LMU
        ! Can we write loop over dimensions in xx and do not do this for each input parameter?
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
        CALL fornado_inference_nl( 1_jpim,                             &
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
! Start linear computations for channel "i" and column "cc"
!==================================================================================
      ! --------------------------------------------------------------------------
      ! Set column weight, optical depth and effective diameters
      ! --------------------------------------------------------------------------
      IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) THEN ! clouds
        colwei_cc_tl = ircld_tl%xcol(cc+1,prof) - ircld_tl%xcol(cc,prof)
        tauw_tl = 0.0_jprb
        taui_tl = 0.0_jprb
        tauwi_top_tl = 0.0_jprb
        tauwi_bot_tl = 0.0_jprb
        reffw_top_tl = 0.0_jprb
        reffi_top_tl = 0.0_jprb
        reffw_bot_tl = 0.0_jprb
        reffi_bot_tl = 0.0_jprb
        reffwi_top_tl = 0.0_jprb
        reffwi_bot_tl = 0.0_jprb
        fpct_tl = 0._jprb

        ! Extract optical depths and effective radii per layer
        dtauw_tl(:) = 0._jprb
        dtaui_tl(:) = 0._jprb
        fmp_tl(:) = 0._jprb
        dreffw_tl(:) = 0._jprb
        dreffi_tl(:) = 0._jprb
        DO j = 1, nlay
          IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
          IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN

            ! layer optical depths
            dtauw_tl(j) = SUM(trans_scatt_ir_tl%opdpext(1:nwcl_max,j,i))
            dtaui_tl(j) = trans_scatt_ir_tl%opdpext(nwcl_max+1,j,i)
            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
!                       od_tl(:,j) = od_tl(:,j) * (1._jprb - aux%s(prof)%pfraction_surf)   
!                                   - od_v1(:,j)  * aux_tl%s(prof)%pfraction_surf    
              dtauw_tl(j) = dtauw_tl(j) * (1._jprb - aux%s(prof)%pfraction_surf)  &
                                  - dtauw_v1(j) * aux_tl%s(prof)%pfraction_surf    
              dtaui_tl(j) = dtaui_tl(j) * (1._jprb - aux%s(prof)%pfraction_surf)  &
                                  - dtaui_v1(j) * aux_tl%s(prof)%pfraction_surf
            ENDIF

            ! layer eff. radii water
            IF ( profiles(prof)%clw_scheme == clw_scheme_deff ) THEN ! DEFF
              dreffw_tl(j) = aux_tl%clw_dg(j,prof)/2._jprb
            ELSE ! OPAC
 
              IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
                ! Modify optical depth in partial layer above surface
!               dreffw   (j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir%opdpext(1:nwcl_max,j,i)) &
!                                           * (1._jprb - aux%s(prof)%pfraction_surf) 
                dreffw_tl(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir_tl%opdpext(1:nwcl_max,j,i)) &
                                                                         * (1._jprb - aux%s(prof)%pfraction_surf)  &
                              -SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir   %opdpext(1:nwcl_max,j,i))  &
                                                                         *         aux_tl%s(prof)%pfraction_surf

              ELSE
!               dreffw   (j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir   %opdpext(1:nwcl_max,j,i)  )
                dreffw_tl(j) = SUM(wcl_opac_deff(1:nwcl_max)/2._jprb * trans_scatt_ir_tl%opdpext(1:nwcl_max,j,i))
              ENDIF
              IF ( dtauw(j) .GT. 0 ) THEN
!                        dreffw_tl(j) = dreffw_tl(j)/dtauw(j) - dreffw(j)/(dtauw(j))**2 * dtauw_tl(j)
                dreffw_tl(j) = dreffw_tl(j)/dtauw(j) - dreffw(j)/(dtauw(j))    * dtauw_tl(j)
              ELSE
                dreffw_tl(j) = 0._jprb !
              ENDIF
            ENDIF
            dreffi_tl(j) = aux_tl%ice_dg(j,prof)/2._jprb  !

            tauw_tl = tauw_tl + dtauw_tl(j)
          ENDIF
        ENDDO ! DO j = 1, nlay
        tau_thr    =  1._jprb
        tau_thr_tl =  0._jprb
        !---------------------------------------------------------------
        call comp_fbot_tl(fmp_tl, tau_thr_tl, dtauw_tl, tau_thr, dtauw, nlay, aa_sc)
        !---------------------------------------------------------------

        ! total optical depths
        tauwi_tl = SUM(   dtaui_tl(:) * fmp(:)     &
                          +  dtaui   (:) * fmp_tl(:) ) ! mixed-phase cloud ice in water cloud
        taui_tl  = SUM(   dtaui_tl(:) * (1._jprb - fmp(:))  &
                          -  dtaui   (:) * fmp_tl(:) ) ! mixed-phase cloud ice in water cloud
        ! 2-layer parameterisation of water cloud
        IF (SIZE(mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams) > 0) THEN
          IF(tauw > 0) THEN
            fac_hi =  mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(3) &
                    - mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(4) * MAX( vza, sza ) / 90._jprb
            fac_lo = 0.5_jprb 
            tau_lo = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(1)
            tau_hi = mfasisnn_coefs%nn(nn)%in(reffw_top_idx)%auxparams(2)
!           logtauw = MIN( MAX( (LOG(tauw) - LOG(tau_lo)) / (LOG(tau_hi) - LOG(tau_lo)), 0._jprb ), 1._jprb ) 
!           tauw_thr = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtau * pi ) ) ) * tauw
            tauw_thr_tl = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtauw * pi ) ) ) * tauw_tl
            IF(logtauw > 0.0_jprb .AND. logtauw <  1.0_jprb ) then
              logtauw_tl  = tauw_tl/(tauw + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
              tauw_thr_tl = tauw_thr_tl - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtauw * pi) * tauw * logtauw_tl 
            ENDIF

            ! compute mean effective radii for both parts
            !---------------------------------------------------------------
            call comp_fbot_tl(fbotw_tl, tauw_thr_tl, dtauw_tl, tauw_thr, dtauw, nlay, aa_sc)
            !---------------------------------------------------------------
!                 tauw_bot = SUM( fbotw(:) * dtauw(:))
!                 tauw_top = tauw - tauw_bot
            tauw_bot_tl = SUM( fbotw_tl(:) * dtauw(:) + fbotw(:) * dtauw_tl(:))
            tauw_top_tl = tauw_tl - tauw_bot_tl

            IF ( tauw_bot > 1E-6 ) THEN
!                    reffw_bot = SUM( fbotw(:)           * dtauw(:) * dreffw(:) ) / tauw_bot
              reffw_bot_tl = SUM( fbotw_tl(:)     * dtauw(:)    * dreffw(:) +       &
                                       fbotw(:)        * dtauw_tl(:) * dreffw(:) +       &
                                       fbotw(:)        * dtauw(:)    * dreffw_tl(:)    ) / tauw_bot     -       &
                                  reffw_bot/tauw_bot * tauw_bot_tl
!                                   SUM( fbotw(:)           * dtauw(:) * dreffw(:) ) / tauw_bot**2 * tauw_bot_tl
            ELSE 
              reffw_bot_tl = 0._jprb !
            ENDIF

            IF ( tauw_top > 1E-6 ) THEN
!                    reffw_top = SUM( (1._jprb-fbotw(:)) * dtauw(:)    * dreffw(:) ) / tauw_top
              reffw_top_tl = SUM(     -fbotw_tl(:)* dtauw(:)    * dreffw(:) +       &
                                    (1._jprb-fbotw(:)) * dtauw_tl(:) * dreffw(:) +       &
                                    (1._jprb-fbotw(:)) * dtauw(:)    * dreffw_tl(:)    ) / tauw_top     -       &
                                  reffw_top/tauw_top * tauw_top_tl
!                                 SUM((1._jprb-fbotw(:)) * dtauw(:)    * dreffw(:) ) / tauw_top**2 * tauw_top_tl
            ELSE
              reffw_top_tl = 0._jprb !
            ENDIF

            ! determine optical depth and mean effective radius of mixed phase ice in the lower water cloud layer
!                 tauwi_bot    = SUM( fbotw(:)    * dtaui(:)   * fmp(:) )
            tauwi_bot_tl = SUM( fbotw_tl(:) * dtaui(:)   * fmp(:)    + &
                                    fbotw(:)    * dtaui_tl(:)* fmp(:)    + &
                                    fbotw(:)    * dtaui(:)   * fmp_tl(:)     )
            IF ( tauwi_bot > 1E-6 ) THEN
!                    reffwi_bot    = SUM( fbotw(:)      * dtaui(:)    * fmp(:)    * dreffi(:) ) / tauwi_bot
              reffwi_bot_tl = SUM( fbotw_tl(:)    * dtaui(:)   * fmp(:)    * dreffi(:) +       &
                                       fbotw(:)        * dtaui_tl(:)* fmp(:)    * dreffi(:) +       &
                                       fbotw(:)        * dtaui(:)   * fmp_tl(:) * dreffi(:) +       &
                                       fbotw(:)        * dtaui(:)   * fmp(:)    * dreffi_tl(:)    ) / tauwi_bot     -       &
                                   reffwi_bot/tauwi_bot * tauwi_bot_tl
            ELSE
!                    reffwi_bot = 0._jprb !
              reffwi_bot_tl = 0._jprb !
            ENDIF
            ! determine optical depth and mean effective radius of mixed phase ice in the upper water cloud layer
!                 tauwi_top = SUM( (1._jprb - fbotw(:)) * dtaui(:) * fmp(:) )
            tauwi_top_tl = SUM(-fbotw_tl(:) * dtaui(:)   * fmp(:)    + &
                        (1._jprb - fbotw(:)) * dtaui_tl(:)* fmp(:)    + &
                        (1._jprb - fbotw(:)) * dtaui(:)   * fmp_tl(:)     )
            IF ( tauwi_top > 1E-6 ) THEN
!                    reffwi_top=SUM( (1._jprb - fbotw(:))* dtaui(:)   * fmp(:)    * dreffi(:) ) / tauwi_top
              reffwi_top_tl = SUM(-fbotw_tl(:)    * dtaui(:)   * fmp(:)    * dreffi(:) +       &
                                   (1._jprb - fbotw(:))* dtaui_tl(:)* fmp(:)    * dreffi(:) +       &
                                   (1._jprb - fbotw(:))* dtaui(:)   * fmp_tl(:) * dreffi(:) +       &
                                   (1._jprb - fbotw(:))* dtaui(:)   * fmp(:)    * dreffi_tl(:)    ) / tauwi_top     -       &
                                   reffwi_top/tauwi_top * tauwi_top_tl

            ELSE
!                    reffwi_top = 0._jprb !
              reffwi_top_tl = 0._jprb !
            ENDIF
          ELSE ! tauw > 0
            reffw_bot_tl = 0._jprb
            reffw_top_tl = 0._jprb
            reffwi_bot_tl = 0._jprb
            reffwi_top_tl = 0._jprb 
          ENDIF ! tauw > 0
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
!                 logtaui = MIN( MAX( (LOG(taui) - LOG(tau_lo)) / (LOG(tau_hi) - LOG(tau_lo)), 0._jprb ), 1._jprb ) 
!                 taui_thr = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtau * pi ) ) ) * taui
            taui_thr_tl = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtaui * pi ) ) ) * taui_tl
            IF(logtaui > 0.0_jprb .AND. logtaui <  1.0_jprb ) then
              logtaui_tl  = taui_tl/(taui + 1E-6) / (LOG(tau_hi) - LOG(tau_lo))
              taui_thr_tl = taui_thr_tl - (fac_lo-fac_hi)*0.5_jprb*pi* SIN(logtaui * pi) * taui * logtaui_tl 
            ENDIF

            ddtaui_tl(:)= dtaui_tl(:)*(1._jprb-fmp) - dtaui(:)*fmp_tl
            ! compute mean effective radii for both parts
            !---------------------------------------------------------------
            call comp_fbot_tl(fboti_tl, taui_thr_tl, ddtaui_tl, taui_thr, dtaui*(1._jprb - fmp(:)), nlay, aa_sc)
            !---------------------------------------------------------------
!                 taui_bot = SUM(fboti(:) * dtaui(:) * (1._jprb - fmp(:)))
!                 taui_top = taui - taui_bot 
            taui_bot_tl = SUM( fboti_tl(:) * dtaui   (:) * (1._jprb - fmp   (:))    &
                                 + fboti(:)    * dtaui_tl(:) * (1._jprb - fmp   (:))    &
                                 - fboti(:)    * dtaui   (:) *            fmp_tl(:)  )
            taui_top_tl = taui_tl - taui_bot_tl
            


            IF( taui_bot > 1E-6 ) THEN
!                    reffi_bot = SUM( fboti(:)          * dtaui(:) * (1._jprb-fmp) * dreffi(:) ) / taui_bot
              reffi_bot_tl = SUM( fboti_tl(:)     * dtaui(:)   *(1._jprb-fmp) * dreffi(:)   +       &
                                       fboti(:)        * dtaui_tl(:)*(1._jprb-fmp) * dreffi(:)   +       &
                                       fboti(:)        * dtaui(:)   *(    -fmp_tl) * dreffi(:)   +       &
                                       fboti(:)        * dtaui(:)   *(1._jprb-fmp) * dreffi_tl(:)    ) / taui_bot - &
                                  reffi_bot/taui_bot * taui_bot_tl

            ELSE 
!                    reffi_bot = 0._jprb 
              reffi_bot_tl = 0._jprb 
            ENDIF

            IF ( taui_top > 1E-6 ) THEN
!                    reffi_top    = SUM( (1._jprb-fboti(:)) * dtaui(:) * (1._jprb-fmp) * dreffi(:) ) / taui_top
              reffi_top_tl = SUM( (    -fboti_tl(:)) * dtaui(:) * (1._jprb-fmp) * dreffi(:)    +       &
                                       (1._jprb-fboti(:)) * dtaui_tl(:)*(1._jprb-fmp) * dreffi(:)   +       &
                                       (1._jprb-fboti(:)) * dtaui(:)   *(    -fmp_tl) * dreffi(:)   +       &
                                       (1._jprb-fboti(:)) * dtaui(:)   *(1._jprb-fmp) * dreffi_tl(:)    ) / taui_top - &
                                  reffi_top/taui_top * taui_top_tl
            ELSE
              reffi_top_tl = 0._jprb 
            ENDIF
          ELSE
            reffi_bot_tl = 0._jprb
            reffi_top_tl = 0._jprb
          ENDIF ! taui > 0
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
        tau_thr_v1 = (tauw + taui + tauwi)/2._jprb
        tau_thr    = tau_thr_v1/(tau_thr_v1 + 1._jprb)

        tau_thr_tl = (tauw_tl + taui_tl + tauwi_tl)/2._jprb
        tau_thr_tl = tau_thr_tl/(tau_thr_v1 + 1._jprb)  -  &
                        tau_thr_tl * tau_thr_v1/(tau_thr_v1 + 1._jprb)**2

        ! find corresponding pressure
        tau = 0._jprb
        tau_tl = 0._jprb
        DO j = 1, nlay
          tau = tau + dtauw(j) + dtaui(j)
          tau_tl = tau_tl + dtauw_tl(j) + dtaui_tl(j)
          IF ( tau > tau_thr) THEN 
            fpct    = profiles   (prof)%p(j+1) -   (tau    - tau_thr   )/(dtauw(j) + dtaui(j))  &
                                                           * (profiles(prof)%p(j+1) - profiles(prof)%p(j))
            fpct = fpct / psfc

            IF (opts%interpolation%lgradp) THEN
              fpct_tl = profiles_tl(prof)%p(j+1) -(  (tau_tl - tau_thr_tl)/(dtauw(j) + dtaui(j)) &
                                                           * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
                                                        -(tau    - tau_thr   )/(dtauw(j) + dtaui(j))**2    &
                                                           * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
                                                                          * (dtauw_tl(j) + dtaui_tl(j))    &
                                                        +(tau    - tau_thr   )/(dtauw(j) + dtaui(j))       &
                                                       * (profiles_tl(prof)%p(j+1) - profiles_tl(prof)%p(j)) &
                                                      )
            ELSE
              fpct_tl = -(  (tau_tl - tau_thr_tl)/(dtauw(j) + dtaui(j))    * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
                               -(tau    - tau_thr   )/(dtauw(j) + dtaui(j))**2 * (profiles(prof)%p(j+1) - profiles(prof)%p(j)) &
                                                 * (dtauw_tl(j) + dtaui_tl(j)))
            ENDIF
            fpct_tl = fpct_tl / psfc - fpct * psfc0_tl/psfc
            EXIT
          ENDIF
        ENDDO
        ! --------------------------------------------------------------------------
        ! Transform input variables (by normalization) to produce NN input -> successfully checked wrt LMU
        ! Can we write loop over dimensions in xx and do not do this for each input parameter?
        ! --------------------------------------------------------------------------

        params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
        CALL nn_transform_input_tl(tauw, tauw_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(taui_idx)
        CALL nn_transform_input_tl(taui, taui_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
        CALL nn_transform_input_tl(reffw_top, reffw_top_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
        CALL nn_transform_input_tl(reffw_bot, reffw_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
        CALL nn_transform_input_tl(reffi_top, reffi_top_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
        CALL nn_transform_input_tl(reffi_bot, reffi_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
        CALL nn_transform_input_tl(tauwi_top, tauwi_top_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
        CALL nn_transform_input_tl(tauwi_bot, tauwi_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
        CALL nn_transform_input_tl(reffwi_top, reffwi_top_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
        CALL nn_transform_input_tl(reffwi_bot, reffwi_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
        params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
        CALL nn_transform_input_tl(fpct, fpct_tl , params%MIN, params%MAX, params%TRANSFORM)

        ALLOCATE(xx_tl(mfasisnn_coefs%nn(nn)%n_input))
        ALLOCATE(yy_tl(mfasisnn_coefs%nn(nn)%n_output))
        xx_tl = (/tauw_tl, taui_tl, tauwi_top_tl, tauwi_bot_tl, &
                     reffw_top_tl, reffi_top_tl, reffw_bot_tl, reffi_bot_tl, &
                    reffwi_top_tl, reffwi_bot_tl, psfc_tl, fpct_tl, 0._jprb, 0._jprb, 0._jprb/)

        yy_tl(:) = 0._jprb
        CALL fornado_inference_d2( 1_jpim,                             &
               mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
               mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
               mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
               mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
              !  mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
               sigo, xx_tl, yy_tl)
        ! --------------------------------------------------------------------------
        DO m = 1, mfasisnn_coefs%nn(nn)%n_output
          ! mfasisnn_coefs%nn(nn)%out(m)%TRANSFORM
          IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_A0' ) THEN
            params=mfasisnn_coefs%nn(nn)%out(m)
            CALL nn_transform_out_tl(yy_v1(1) , yy_tl(1), params%MIN, params%MAX, params%TRANSFORM)
          ENDIF
          IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_DH' ) THEN
            params=mfasisnn_coefs%nn(nn)%out(m)
            CALL nn_transform_out_tl(yy_v1(2) , yy_tl(2), params%MIN, params%MAX, params%TRANSFORM)
          ENDIF
          IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_D1' ) THEN
            params=mfasisnn_coefs%nn(nn)%out(m)
            CALL nn_transform_out_tl(yy_v1(3) , yy_tl(3), params%MIN, params%MAX, params%TRANSFORM)
          ENDIF
        ENDDO
  
        ! --------------------------------------------------------------------------
        ! Compute refl for model profile albedo (Jonkheid et al.) -> successfully checked wrt LMU
        ! --------------------------------------------------------------------------
        IF ( yy(3) > 1E-4 ) THEN
          eta = (yy(3) - yy(2))
          eta_tl = (yy_tl(3) - yy_tl(2))
          gamma = ( yy(3) + yy(2) ) * yy(2)
          gamma_tl = ( yy_tl(3) + yy_tl(2) ) * yy(2) + &
                       ( yy   (3) + yy   (2) ) * yy_tl(2)
!          refl_cc = yy(1) + albedo * gamma/(yy(3) - albedo*eta)
          hvar=albedo *gamma   /(yy(3) - albedo*eta)**2 
          refl_cc_tl = yy_tl(1) + albedo_tl*gamma   /(yy(3) - albedo*eta)  &
                                     + albedo   *gamma_tl/(yy(3) - albedo*eta)  &
                                     + hvar * ( -yy_tl(3) + albedo_tl*eta + albedo*eta_tl )
        ELSE
!          refl_cc = yy(1) + albedo * (yy(3) + yy(2))
          refl_cc_tl = yy_tl(1) + albedo_tl * (yy(3) + yy(2))  &
                                     + albedo    * (yy_tl(3) + yy_tl(2))
        ENDIF

        DEALLOCATE(xx_tl,yy_tl, yy, yy_v1, sigo)

        ! --------------------------------------------------------------------------
        ! Add column contribution to total reflectance
        ! --------------------------------------------------------------------------
!        refl = refl + refl_cc * colwei_cc
        refl_tl = refl_tl + refl_cc_tl * colwei_cc + refl_cc * colwei_cc_tl
        
      ELSE
        err = errorstatus_fatal
        THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
      ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column


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

      ! --------------------------------------------------------------------------
      ! Transform input variables (by normalization) to produce NN input -> successfully checked wrt LMU
      ! Can we write loop over dimensions in xx and do not do this for each input parameter?
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
      CALL fornado_inference_nl( 1_jpim,                             &
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
      IF( yy(3) > 1E-4 ) THEN
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

    IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) THEN
      colwei_clr_tl = ircld_tl%xcolclr(prof) ! clear column
 
      tauw_tl = 0._jprb
      taui_tl = 0._jprb
      tauwi_top_tl = 0._jprb
      tauwi_bot_tl = 0._jprb
      reffw_top_tl = 0._jprb
      reffi_top_tl = 0._jprb
      reffw_bot_tl = 0._jprb
      reffi_bot_tl = 0._jprb
      reffwi_top_tl = 0._jprb
      reffwi_bot_tl = 0._jprb
      fpct_tl = 0._jprb


      params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
      CALL nn_transform_input_tl(tauw, tauw_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(taui_idx)
      CALL nn_transform_input_tl(taui, taui_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
      CALL nn_transform_input_tl(reffw_top, reffw_top_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
      CALL nn_transform_input_tl(reffw_bot, reffw_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
      CALL nn_transform_input_tl(reffi_top, reffi_top_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
      CALL nn_transform_input_tl(reffi_bot, reffi_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
      CALL nn_transform_input_tl(tauwi_top, tauwi_top_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
      CALL nn_transform_input_tl(tauwi_bot, tauwi_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
      CALL nn_transform_input_tl(reffwi_top, reffwi_top_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
      CALL nn_transform_input_tl(reffwi_bot, reffwi_bot_tl , params%MIN, params%MAX, params%TRANSFORM)
      params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
      CALL nn_transform_input_tl(fpct, fpct_tl , params%MIN, params%MAX, params%TRANSFORM)

      
      
      ALLOCATE(xx_tl(mfasisnn_coefs%nn(nn)%n_input))
      ALLOCATE(yy_tl(mfasisnn_coefs%nn(nn)%n_output))
      xx_tl = (/tauw_tl, taui_tl, tauwi_top_tl, tauwi_bot_tl, &
                     reffw_top_tl, reffi_top_tl, reffw_bot_tl, reffi_bot_tl, &
                    reffwi_top_tl, reffwi_bot_tl, psfc_tl, fpct_tl, 0._jprb, 0._jprb, 0._jprb/)
      yy_tl(:) = 0._jprb
      CALL fornado_inference_d2( 1_jpim,                             &
             mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
             mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
             mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
             mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
            !  mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
             sigo, xx_tl, yy_tl)
      ! --------------------------------------------------------------------------
      DO m = 1, mfasisnn_coefs%nn(nn)%n_output
        ! mfasisnn_coefs%nn(nn)%out(m)%TRANSFORM
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_A0' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_tl(yy_v1(1) , yy_tl(1), params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_DH' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_tl(yy_v1(2) , yy_tl(2), params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
        IF ( mfasisnn_coefs%nn(nn)%out(m)%name == 'REFL_D1' ) THEN
          params=mfasisnn_coefs%nn(nn)%out(m)
          CALL nn_transform_out_tl(yy_v1(3) , yy_tl(3), params%MIN, params%MAX, params%TRANSFORM)
        ENDIF
      ENDDO

      ! --------------------------------------------------------------------------
      ! Compute refl for model profile albedo (Jonkheid et al.) -> successfully checked wrt LMU
      ! --------------------------------------------------------------------------
      IF ( yy(3) > 1E-4 ) THEN
        eta = (yy(3) - yy(2))
        eta_tl = (yy_tl(3) - yy_tl(2))
        gamma = ( yy(3) + yy(2) ) * yy(2)
        gamma_tl = ( yy_tl(3) + yy_tl(2) ) * yy(2) + &
               ( yy   (3) + yy   (2) ) * yy_tl(2)
!        refl_clr = yy(1) + albedo * gamma/(yy(3) - albedo*eta)
        hvar=albedo *gamma   /(yy(3) - albedo*eta)**2 
        refl_clr_tl = yy_tl(1) + albedo_tl*gamma   /(yy(3) - albedo*eta)  &
                             + albedo   *gamma_tl/(yy(3) - albedo*eta)  &
                             + hvar * ( -yy_tl(3) + albedo_tl*eta + albedo*eta_tl )
      ELSE
!        refl_clr = yy(1) + albedo * (yy(3) + yy(2))
        refl_clr_tl = yy_tl(1) + albedo_tl * (yy(3) + yy(2))  &
                             + albedo    * (yy_tl(3) + yy_tl(2))
      ENDIF

      DEALLOCATE(xx_tl,yy_tl, yy, yy_v1, sigo)

      ! --------------------------------------------------------------------------
      ! Add column contribution to total reflectance
      ! --------------------------------------------------------------------------

!      refl = refl + refl_clr * colwei
      refl_tl = refl_tl + refl_clr_tl * colwei_clr + refl_clr * colwei_clr_tl
      
    ELSE
      err = errorstatus_fatal
      THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
    ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column

!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!HHH  BOTTOM TL linear clear air HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
!HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH


    ! --------------------------------------------------------------------------
    ! Fill radiance structure 
    ! TL only!!!!!!!
    ! --------------------------------------------------------------------------
    radiance_tl%refl(i) = refl_tl
    radiance_tl%refl_clear(i) = refl_clr_tl

    radiance_tl%total(i) = refl_tl * solar_spectrum(i) * pi_r  &
       * COS(profiles(prof)%sunzenangle * deg2rad)
    radiance_tl%clear(i) =  radiance_tl%refl_clear(i)  * solar_spectrum(i) * pi_r  &
       * COS(profiles(prof)%sunzenangle * deg2rad)
    radiance_tl%cloudy(i) = radiance_tl%total(i)

  ENDDO ! nchanprof

  ! --------------------------------------------------------------------------
  ! Tidy up
  ! --------------------------------------------------------------------------

  DEALLOCATE( dtauw,     &
              dtaui,     &
              dtauw_v1,  &
              dtaui_v1,  &
              dtauw_tl,  &
              dtaui_tl,  &
              ddtaui_tl, &
              fmp,       &
              fmp_tl,    &
              dreffw,    &
              dreffw_tl, &
              dreffi,    &
              dreffi_tl, &
              fbotw,     &
              fboti,     &
              fbotw_tl,  &
              fboti_tl,  &
              stat = err )
  THROWM(err.NE.0, "Deallocation of memory for rttov_mfasis_nn_tl failed")

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN_TL', 1_jpim, ZHOOK_HANDLE)
  CATCH
  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN_TL', 1_jpim, ZHOOK_HANDLE)

contains

  SUBROUTINE  comp_fbot_tl(fbot_tl, tau_thr_tl, dtaul_tl, tau_thr, dtaul, nlay, aa_sc)

    IMPLICIT NONE
!===========================================================
    INTEGER(jpim),    INTENT(IN)    :: nlay
    REAL(jprb),       INTENT(OUT)   :: fbot_tl(nlay)
    REAL(jprb),       INTENT(IN)    :: tau_thr_tl
    REAL(jprb),       INTENT(IN)    :: dtaul_tl(:)
    REAL(jprb),       INTENT(IN)    :: tau_thr
    REAL(jprb),       INTENT(IN)    :: dtaul(:)
    REAL(jprb),       INTENT(IN)    :: aa_sc

    REAL(jprb)                      :: fbot (nlay)
    REAL(jprb)                      :: ffunc(nlay)
    REAL(jprb)                      :: ffunc_tl(nlay)

    REAL(jprb)                      :: fbot_old, ffunc_old
    REAL(jprb)                      :: fbot_old_tl, ffunc_old_tl
    REAL(jprb)                      :: aa, aa_lim
    REAL(jprb)                      :: aa_tl
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
      ffunc_old = 0.5_jprb*(aa+ aa_sc*0.5_jprb*pi - aa_sc*COS(aaa(0)/aa_sc) )
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
    fbot_tl(:)   = 0._jprb
    ffunc_tl(:)  = 0._jprb
    fbot_old_tl  = 0._jprb
    ffunc_old_tl = 0._jprb
    aa_tl = - tau_thr_tl
!     if(aaa(0) > -aa_sc * 0.5_jprb  * pi ) THEN
!       ffunc_old = 0.5_jprb*(aa+ aa_sc*0.5_jprb*pi - aa_sc*COS(aaa(0)/aa_sc) )
!     endif
    if(aaa(0) > -aa_sc * 0.5_jprb  * pi ) THEN
      ffunc_old_tl = 0.5_jprb*aa_tl* (1._jprb + sin(aaa(0)/aa_sc))
    endif
    DO j = 1, nlay
      IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT ! Layer below surf.
      IF(dtaul(j) /= 0) THEN
!         aa = taui_int - tau_thr
        aa_tl = aa_tl + dtaul_tl(j)
        IF(aaa(j) <= -aa_lim) cycle
        IF(aaa(j) >=  aa_lim) THEN
          ffunc_tl(j) = aa_tl
        ELSE
          ffunc_tl(j) = 0.5_jprb*aa_tl* (1._jprb + sin(aaa(j)/aa_sc))
        ENDIF
        fbot_tl(j)   = (ffunc_tl(j) - ffunc_old_tl)/dtaul(j) - &
                        fbot(j) * dtaul_tl(j)/dtaul(j)
        ffunc_old_tl = ffunc_tl(j)
        fbot_old_tl  = fbot_tl(j)

        IF(aaa(j) >=  aa_lim) THEN !set all remaining to one and quit
!         IF(aa >=  aa_lim) THEN !set all remaining to one and quit
          DO jj=j+1, nlay
        fbot_tl(jj) = 0
          ENDDO
          EXIT
        ENDIF
      ELSE
        fbot_tl(j)  = fbot_old_tl
      ENDIF
    ENDDO
  END SUBROUTINE comp_fbot_tl

  !----------------------------------------------------------------------------------------------
  !
  !   FORNADO NEURAL NETWORK INFERENCE CODE -- TANGENT LINEAR VERSION
  !   2019-10 L.Scheck
  !                          using "sigo" from fornado_inference_nl, 2022-08  O.Stiller
  !
  !   to generate a python module from this subroutine use
  !     f2py -c fornado_inference_d.F90 -m fornado_inference_d_f90 --f90flags="-O3"
  !   (for a debug version, use
  !     f2py -c fornado_inference_d.F90 -m fornado_inference_d_f90 --f90flags="-g -fcheck='all'"
  !   (see http://cens.ioc.ee/projects/f2py2e/usersguide/f2py_usersguide.pdf )
  !
  !----------------------------------------------------------------------------------------------
  subroutine fornado_inference_d2( n_samples,                             &
                               n_input, n_output, n_hidden, n_nodes,  &
                                         act_h,    act_o,             &
                               weight_i, weight_h, weight_o,          &
!                                bias_i,   bias_h,   bias_o,            &
                               sigo,    xd,    yd )
!                                sigo, x, xd, y, yd )

    ! tangent linear version of fornado_inference.F90

    implicit none
    integer, parameter :: sp = jprb !CSt: sp = kind(1.0)

    integer(jpim), intent(in) :: n_samples, n_input, n_output, n_hidden, n_nodes

    integer(jpim), intent(in) :: act_h, act_o

    real(sp), dimension( n_input, n_nodes ),             intent(in) :: weight_i
    real(sp), dimension( n_nodes, n_nodes, n_hidden-1 ), intent(in) :: weight_h
    real(sp), dimension( n_nodes, n_output ),            intent(in) :: weight_o

    ! real(sp), dimension( n_nodes ),                      intent(in) :: bias_i
    ! real(sp), dimension( n_nodes, n_hidden-1 ),          intent(in) :: bias_h
    ! real(sp), dimension( n_output ),                     intent(in) :: bias_o

    real(sp), dimension(n_samples,0:n_hidden,n_nodes),   intent(in) :: sigo

    real(sp), dimension( n_samples, n_input  ),          intent(in) ::    xd
! real(sp), dimension( n_samples, n_input  ),          intent(in) :: x, xd
    real(sp), dimension( n_samples, n_output ),       intent(inout) ::    yd

    ! real(sp), dimension( n_samples, n_output )                      ::    y
    ! real(sp), dimension( n_nodes ) :: sigi
    real(sp), dimension( n_nodes ) :: sigid, sigod
    ! logical, dimension( n_nodes ) :: pos

    integer(jpim) :: i_sample, i, j, l

    ! unvectorized version: inner loop over nodes -------------------------------

    ! loop over all input data sets
    do i_sample = 1, n_samples
      ! propagate signal from input layer to first hidden layer ....................
      do i=1,n_nodes
        sigod(i) = 0._jprb
!       sigo(i)  = bias_i(i)
        do j=1,n_input
          sigod(i) = sigod(i) + weight_i(j, i) * xd(i_sample,j)
!          sigo(i)  = sigo(i)  + weight_i(j, i) * x( i_sample,j)
        end do
      end do
      l=0
      ! apply activation function
      call actfunc_d( act_h, sigo(i_sample,l,:), sigod,       sigid )

      ! propagate signal towards last hidden layer .................................
      do l=1, n_hidden-1

        ! progagate from layer l to l+1 (l=1 is first hidden layer)
        do i=1,n_nodes
          sigod(i) = 0._jprb
          do j=1,n_nodes
            sigod(i) = sigod(i) + weight_h(j, i, l) * sigid(j)
          end do
        end do
        
        ! apply activation function
        call actfunc_d( act_h, sigo(i_sample,l,:), sigod,       sigid )

      end do ! layer loop

      ! propagate signal to output layer ...........................................
      do i=1,n_output
        sigod(i) = 0._jprb
!       sigo(i)  = bias_o(i)
        do j=1,n_nodes
          sigod(i) = sigod(i) + weight_o(j, i) * sigid(j)
        end do
      end do
      l= n_hidden
      ! apply activation function
      call actfunc_d( act_o, sigo(i_sample,l,1:n_output), sigod(1:n_output),                yd(i_sample,:) )
    end do ! sample loop

  end subroutine fornado_inference_d2
  !----------------------------------------------------------------------------------------------
  !
  !   FORNADO NEURAL NETWORK INFERENCE CODE -- TANGENT LINEAR VERSION
  !   2019-10 L.Scheck
  !
  !   to generate a python module from this subroutine use
  !     f2py -c fornado_inference_d.F90 -m fornado_inference_d_f90 --f90flags="-O3"
  !   (for a debug version, use
  !     f2py -c fornado_inference_d.F90 -m fornado_inference_d_f90 --f90flags="-g -fcheck='all'"
  !   (see http://cens.ioc.ee/projects/f2py2e/usersguide/f2py_usersguide.pdf )
  !
  !----------------------------------------------------------------------------------------------

  ! pure subroutine actfunc_d( functype, so, sod, si, sid )
  pure subroutine actfunc_d( functype, so, sod,     sid )

    ! activation functions (nonlinear + tangent linear version)

    ! activation functions
    integer, parameter :: sp = jprb !CSt: sp = kind(1.0)

    integer(jpim), intent(in) :: functype

    real(sp), dimension(:),    intent(in) :: so, sod
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
!      si = MAX(0._jprb,so)
      sid = sod * (SIGN(1._jprb,so)+1._jprb)*0.5_jprb

    case (2)               ! elu
      !where( so .gt. 0 )
      !   sid = sod
      !   si  = so
      !elsewhere
      !   sid = sod * exp(so)
      !   si  =       exp(so) - 1._jprb
      !endwhere
!      si = MIN( ABS(so), exp(so) - 1._jprb )
      sid = sod * ( (SIGN(1._jprb,so)+1._jprb)*0.5_jprb + (exp(so) - 1._jprb) * (1._jprb - (SIGN(1._jprb,so)+1._jprb)*0.5_jprb) )


    case (3)               ! softplus
!      si  =              log( exp(so) + 1._jprb )
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
!      si = MIN( ABS(so), -1._jprb + 0.25_jprb*MAX(0._jprb,so+2._jprb)**2 )
      ! (SIGN(1,so  )+1)*0.5 is 1 for so> 0, else 0
      ! (SIGN(1,so+2)+1)*0.5 is 1 for so>-2, else 0
      sid = ((SIGN(1._jprb,so)+1._jprb)*0.5_jprb) * sod + &
            (1._jprb-(SIGN(1._jprb,so)+1._jprb)*0.5_jprb) * (SIGN(1._jprb,so+2._jprb) + &
            1._jprb)*0.5_jprb * 0.5_jprb*(so+2._jprb)*sod
      !     [       so .gt. 0      ]         [[       so .lt. 0       ] .and. [      so .gt. -2      ]]

    case default
      sid = -999.999_jprb
!      si  = -999.999
    end select

  end subroutine actfunc_d

  !----------------------------------------------------------------------------------------------
  !
  !   Transform input variable for NN (train_model.py normalize())
  !
  !----------------------------------------------------------------------------------------------
  SUBROUTINE nn_transform_input_tl(var_in, var_tl, minv, maxv, trafo )
    IMPLICIT NONE
!     REAL(jprb),       INTENT(INOUT) :: var    
    REAL(jprb),       INTENT(IN)    :: var_in    
    REAL(jprb),       INTENT(INOUT) :: var_tl
    REAL(jprb),       INTENT(IN)    :: minv, maxv
    CHARACTER(LEN=32),INTENT(IN)    :: trafo

    REAL(jprb)                      :: var
    IF (var_in < minv .or. var_in > maxv ) var_tl = 0
    var = MIN( MAX( var_in,minv ), maxv )

    IF (TRIM(trafo) == 'LIN') THEN ! uniform distribution between min and max
      var_tl=  var_tl    / (maxv - minv)
!        var = (var - minv) / (maxv - minv)
    ELSE IF (TRIM(trafo) == 'LOG') THEN ! uniform distribution in log(parameter)
      var_tl = (var_tl/var)        / (LOG(maxv) - LOG(minv))
!        var = (LOG(var) - LOG(minv)) / (LOG(maxv) - LOG(minv))
    ENDIF
  END SUBROUTINE nn_transform_input_tl
  !----------------------------------------------------------------------------------------------
  !
  !   Transform output variable for NN (train_model.py denormalize(), added
  !   sqrtnone trafo)
  !
  !----------------------------------------------------------------------------------------------
  SUBROUTINE nn_transform_out_tl( var_in, var_tl, minv, maxv, trafo )
    IMPLICIT NONE
!     REAL(jprb),       INTENT(INOUT) :: var
    REAL(jprb),       INTENT(IN)    :: var_in
    REAL(jprb),       INTENT(INOUT) :: var_tl
    REAL(jprb),       INTENT(IN)    :: minv, maxv
    CHARACTER(LEN=32),INTENT(IN)    :: trafo

    REAL(jprb)                      :: var
    var = var_in
    IF (TRIM(trafo) /= 'SQRTNONE') THEN
      IF (var < minv .or. var > maxv ) var_tl = 0
      var = MIN( MAX( var_in,minv ), maxv )
    ENDIF 

    IF (TRIM(trafo) == 'LIN') THEN
      var_tl=  var_tl    * (maxv - minv)
!        var = minv + var*(maxv - minv)
    ELSE IF (TRIM(trafo) == 'LOG') THEN
      ! change var first to use chain rule for differciati0n
      var = EXP( LOG(minv) + var*( LOG(maxv) - LOG(minv) ) )
      var_tl = var_tl*( LOG(maxv) - LOG(minv) )* var
    ELSE IF (TRIM(trafo) == 'SQRT') THEN
      var_tl = 2._jprb* ( SQRT(minv) + var*( SQRT(maxv) - SQRT(minv) ) ) &
                * ( SQRT(maxv) - SQRT(minv) ) * var_tl
!        var =          ( SQRT(minv) + var*( SQRT(maxv) - SQRT(minv) ) )**2
    ELSE IF (TRIM(trafo) == 'SQRTNONE') THEN
      var_tl = var*2._jprb *var_tl
!        var = var**2
    ENDIF
  END SUBROUTINE nn_transform_out_tl

END SUBROUTINE rttov_mfasis_nn_tl
