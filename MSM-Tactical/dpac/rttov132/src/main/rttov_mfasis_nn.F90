! Description:
!> @file
!!   Prepares inputs for MFASIS-NN fast visible/near-IR scattering model, calls
!!   MFASIS-NN and returns TOA radiances and reflectances.
!
!> @brief
!!   Prepares inputs for MFASIS-NN fast visible/near-IR scattering model, calls
!!   MFASIS-NN and returns TOA radiances and reflectances.
!!
!! @details
!!   Reference for MFASIS:
!!
!!   Scheck L., P. Frerebeau, R. Buras-Schnell, and B. Mayer, 2016: A fast
!!   radiative transfer method for the simulation of visible satellite imagery.
!!   Journal of Quantitative Spectroscopy and Radiative Transfer, 175:54-67.
!!
!!   Reference for MFASIS-NN:
!!   Scheck L., 2021: A neural network based forward operator for visible
!!   satellite images and its adjoint. Journal of Quantitative Spectroscopy
!!   and Radiative Transfer, 274, 107841, https://doi.org/10.1016/j.jqsrt.2021.107841
!!
!!
!! @param[out]    err               status on exit
!! @param[in]     chanprof          specifies channels and profiles to simulate
!! @param[in]     chanflag          flags to indicate which channels with NN available
!! @param[in]     opts              options to configure the simulations
!! @param[in]     profiles          input atmospheric profiles and surface variables
!! @param[in]     coefs             coefficients structure for instrument to simulate
!! @param[in]     ircld             information on cloud columns
!! @param[in]     aux               additional internal profile variables
!! @param[in]     reflectance       surface BRDFs
!! @param[in]     solar_spectrum    TOA solar irradiance for each channel
!! @param[in]     trans_scatt_ir    cloud/aerosol optical depths
!! @param[in,out] radiance          output radiances and corresponding BRFs
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
SUBROUTINE rttov_mfasis_nn( &
             err,              &
             chanprof,         &
             chanflag,         &
             opts,             &
             profiles,         &
             coefs,            &
             ircld,            &
             aux,              &
             reflectance,      &
             solar_spectrum,   &
             trans_scatt_ir,   &
             radiance)

  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !   0.0     20??     Orginal MFASIS-NN code by L.Scheck (LMU-HErZ)
  !   1.0     2022     Implementation into RTTOV (C. Stumpf, F. Baur, O. Stiller, DWD)

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
  ! Any USE statements which are not required by the subroutine interface go here
  USE rttov_types, ONLY : &
    rttov_coef_mfasis_nn,        &
    rttov_mfasis_nn_params

  USE rttov_const, ONLY :        &
    wcl_opac_deff,               &
    pi,                          &
    deg2rad,                     &
    rad2deg,                     &
    pi_r,                        &
    clw_scheme_deff,             &
    mfasis_cld,                  &
    qflag_mfasis_zenangle,       &
 !  qflag_mfasis_sumzenangle,    & ! sum is currently not checked
    qflag_mfasis_geometry_bounds,&
    qflag_mfasis_opdpedia_bounds,&
    nwcl_max

  USE rttov_mfasis_nn_mod, ONLY : &
    aa_sc, noinput,              &
    comp_fbot,                   &
    actfunc,                     &
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
  TYPE(rttov_coefs),                  INTENT(IN)    :: coefs
  TYPE(rttov_ircld),                  INTENT(IN)    :: ircld
  TYPE(rttov_profile_aux),            INTENT(IN)    :: aux
  TYPE(rttov_reflectance),            INTENT(IN)    :: reflectance(SIZE(chanprof))
  REAL(jprb),                         INTENT(IN)    :: solar_spectrum(SIZE(chanprof))
  TYPE(rttov_transmission_scatt_ir),  INTENT(IN)    :: trans_scatt_ir
  TYPE(rttov_radiance),               INTENT(INOUT) :: radiance
!INTF_END

#include "rttov_errorreport.interface"

  INTEGER(jpim)              :: nchanprof, prof, chan, nn, ncolms
  INTEGER(jpim)              :: nlay
  INTEGER(jpim)              :: i, j, cc
  TYPE(rttov_coef_mfasis_nn) :: mfasisnn_coefs
  TYPE(rttov_mfasis_nn_params) :: params

  REAL(jprb)                 :: albedo
  REAL(jprb)                 :: mu, mu0, cad, alpha_deg
  REAL(jprb)                 :: colwei_clr, colwei_cc
  REAL(jprb)                 :: refl

  REAL(jphook) :: zhook_handle

  !XXX NEW MFASIS-NN introduced
  INTEGER(jpim)              :: m, nlev_surf
  INTEGER(jpim)              :: tauw_idx, taui_idx, tauwi_top_idx, tauwi_bot_idx
  ! INTEGER(jpim)              :: reffw_idx, reffi_idx
  INTEGER(jpim)              :: reffw_top_idx, reffi_top_idx, reffw_bot_idx, reffi_bot_idx, reffwi_top_idx
  INTEGER(jpim)              :: reffwi_bot_idx, psfc_idx, fpct_idx
  INTEGER(jpim)              :: vza_idx, sza_idx, alpha_idx
  REAL(jprb)                 :: tauw, taui, tauwi, tauwi_top, tauwi_bot
  REAL(jprb)                 :: reffw_top, reffi_top, reffw_bot, reffi_bot, reffwi_top, reffwi_bot
  REAL(jprb)                 :: psfc   , fpct
  REAL(jprb)                 :: psfc_nn, fpct_nn
  REAL(jprb)                 :: sza   , vza   
  REAL(jprb)                 :: sza_nn, vza_nn, alpha_nn
  REAL(jprb)                 :: tauw_nn, taui_nn, reffw_top_nn, reffw_bot_nn 
  REAL(jprb)                 :: reffi_top_nn , reffi_bot_nn , tauwi_top_nn , tauwi_bot_nn , reffwi_top_nn, reffwi_bot_nn
  REAL(jprb)                 :: fac_hi, fac_lo, tau_lo, tau_hi, logtau, tau_thr, tauw_top, taui_top, tau
  REAL(jprb)                 :: tauw_bot, taui_bot
  REAL(jprb)                 :: refl_cc, refl_clr
  REAL(jprb)                 :: eta, gamma
  REAL(jprb),    ALLOCATABLE :: dtauw(:), dtaui(:), fmp(:), dreffw(:), dreffi(:), fbotw(:), fboti(:)
  REAL(jprb),    ALLOCATABLE :: xx(:), yy(:)
  ! --------------------------------------------------------------------------------------

  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN', 0_jpim, ZHOOK_HANDLE)

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
              fmp(nlay)                   , &   ! factor that defines onset of mixed-phase cloud
              dreffw(nlay)                , &   ! layer effective water cloud radii
              dreffi(nlay)                , &   ! layer effective ice cloud radii
              fbotw(nlay)                  ,&    !
              fboti(nlay)                  ,&    !
                          STAT=err)             ! 2-layer param.: factor that defines boarder between 2 layers  
    THROWM(err.NE.0, "Allocation of memory for rttov_mfasis_nn failed")
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


    ! --------------------------------------------------------------------------
    ! Set indices of NN input nodes and 
    ! initialize some NN input parameters (wthout dependence on cloud columns) 
    ! --------------------------------------------------------------------------
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

    DO m = 1, mfasisnn_coefs%nn(nn)%n_input 
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
    alpha_deg =  rad2deg*(ACOS(mu*mu0 + SQRT(1._jprb - mu**2)*SQRT(1._jprb - mu0**2)*cad)) 

    ! --------------------------------------------------------------------------
    ! NN-input variables which are independent of cloud/aerosol profile
    ! --------------------------------------------------------------------------
    vza_nn  =vza
    sza_nn  =sza
    alpha_nn=alpha_deg
    params=mfasisnn_coefs%nn(nn)%in(vza_idx)  
    CALL nn_transform_input(vza_nn ,  params%MIN, params%MAX, params%TRANSFORM &
                            ,radiance%quality(i) , qflag_mfasis_zenangle, err=err) 
    THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
    params=mfasisnn_coefs%nn(nn)%in(sza_idx)  
    CALL nn_transform_input(sza_nn ,  params%MIN, params%MAX, params%TRANSFORM &
                            ,radiance%quality(i) , qflag_mfasis_zenangle, err=err) 
    THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
    params=mfasisnn_coefs%nn(nn)%in(alpha_idx)  
    CALL nn_transform_input(alpha_nn ,params%MIN, params%MAX, params%TRANSFORM &
                            ,radiance%quality(i) , qflag_mfasis_geometry_bounds, err=err) 
    THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")


    psfc   = profiles(prof)%s2m%p
    psfc_nn= psfc
    params=mfasisnn_coefs%nn(nn)%in(psfc_idx)
    CALL nn_transform_input(psfc_nn , params%MIN, params%MAX, params%TRANSFORM, err=err)  
    THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
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

    DO cc = 1, ncolms

    ! --------------------------------------------------------------------------
    ! Set column weight, optical depth and effective diameters
    ! --------------------------------------------------------------------------
      IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) THEN ! clouds
        colwei_cc = ircld%xcol(cc+1,prof) - ircld%xcol(cc,prof)
        ! --------------------------------------------------------------------------
        ! Computation of NN input parameters from model profiles:
        ! Optical depths, effective radii, ...
        ! --------------------------------------------------------------------------
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
            dreffi(j) = aux%ice_dg(j,prof)/2._jprb  

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
            logtau = MIN( MAX( (LOG(tauw) - LOG(tau_lo)) / (LOG(tau_hi) - LOG(tau_lo)), 0._jprb ), 1._jprb ) 
            tau_thr = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtau * pi ) ) ) * tauw


            ! compute mean effective radii for both parts
            !---------------------------------------------------------------
            call comp_fbot(fbotw, tau_thr, dtauw, nlay, aa_sc, nlev_surf)
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
            fac_hi =  mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(3) &
                    - mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(4) * MAX( vza, sza ) / 90._jprb
            fac_lo = 0.5_jprb 
            tau_lo = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(1)
            tau_hi = mfasisnn_coefs%nn(nn)%in(reffi_top_idx)%auxparams(2)
            logtau = MIN( MAX( (LOG(taui) - LOG(tau_lo)) / (LOG(tau_hi) - LOG(tau_lo)), 0._jprb ), 1._jprb ) 
            tau_thr = ( fac_lo - (fac_lo-fac_hi) * 0.5_jprb*( 1._jprb - COS( logtau * pi ) ) ) * taui

            ! compute mean effective radii for both parts
            !---------------------------------------------------------------
            call comp_fbot(fboti, tau_thr, dtaui*(1._jprb - fmp(:)), nlay, aa_sc, nlev_surf)
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

        ! Only set quality flags when particle size is out of bounds
        params=mfasisnn_coefs%nn(nn)%in(tauw_idx)
        CALL nn_transform_input(tauw_nn      ,params%MIN, params%MAX, params%TRANSFORM, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(taui_idx)
        CALL nn_transform_input(taui_nn      ,params%MIN, params%MAX, params%TRANSFORM, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
        CALL nn_transform_input(reffw_top_nn ,params%MIN, params%MAX, params%TRANSFORM &
                                  ,radiance%quality(i) , qflag_mfasis_opdpedia_bounds, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
        CALL nn_transform_input(reffw_bot_nn ,params%MIN, params%MAX, params%TRANSFORM &
                                  ,radiance%quality(i) , qflag_mfasis_opdpedia_bounds, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
        CALL nn_transform_input(reffi_top_nn ,params%MIN, params%MAX, params%TRANSFORM &
                                  ,radiance%quality(i) , qflag_mfasis_opdpedia_bounds, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
        CALL nn_transform_input(reffi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM &
                                  ,radiance%quality(i) , qflag_mfasis_opdpedia_bounds, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
        CALL nn_transform_input(tauwi_top_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
        CALL nn_transform_input(tauwi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
        CALL nn_transform_input(reffwi_top_nn,params%MIN, params%MAX, params%TRANSFORM &
                                  ,radiance%quality(i) , qflag_mfasis_opdpedia_bounds, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
        CALL nn_transform_input(reffwi_bot_nn,params%MIN, params%MAX, params%TRANSFORM &
                                  ,radiance%quality(i) , qflag_mfasis_opdpedia_bounds, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")
        params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
        CALL nn_transform_input(fpct_nn      ,params%MIN, params%MAX, params%TRANSFORM, err=err)
        THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")

        ALLOCATE(xx(mfasisnn_coefs%nn(nn)%n_input))
        ALLOCATE(yy(mfasisnn_coefs%nn(nn)%n_output))

        xx = (/tauw_nn, taui_nn, tauwi_top_nn, tauwi_bot_nn, &
                   reffw_top_nn, reffi_top_nn, reffw_bot_nn, reffi_bot_nn, &
                  reffwi_top_nn, reffwi_bot_nn, psfc_nn, fpct_nn, sza_nn, vza_nn, alpha_nn/)

        yy(:) = 0._jprb

        CALL fornado_inference( 1_jpim,                             &
               mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
               mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
               mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
               mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
               mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
               xx, yy, 1_jpim)

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

        DEALLOCATE(xx, yy)

        ! --------------------------------------------------------------------------
        ! Add column contribution to total reflectance
        ! --------------------------------------------------------------------------
        refl = refl + refl_cc * colwei_cc
        
      ELSE
        err = errorstatus_fatal
        THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
      ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column


    ENDDO  ! DO cc = 1, ncols

    ! --------------------------------------------------------------------------
    ! Compute clear reflectance and add clear column to total reflectabce -> Is this the correct procedure also for MFASIS-NN?
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
      CALL nn_transform_input(tauw_nn      ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(taui_idx)
      CALL nn_transform_input(taui_nn      ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(reffw_top_idx)
      CALL nn_transform_input(reffw_top_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(reffw_bot_idx)
      CALL nn_transform_input(reffw_bot_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(reffi_top_idx)
      CALL nn_transform_input(reffi_top_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(reffi_bot_idx)
      CALL nn_transform_input(reffi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(tauwi_top_idx)
      CALL nn_transform_input(tauwi_top_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(tauwi_bot_idx)
      CALL nn_transform_input(tauwi_bot_nn ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(reffwi_top_idx)
      CALL nn_transform_input(reffwi_top_nn,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(reffwi_bot_idx)
      CALL nn_transform_input(reffwi_bot_nn,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist") 
      params=mfasisnn_coefs%nn(nn)%in(fpct_idx)
      CALL nn_transform_input(fpct_nn      ,params%MIN, params%MAX, params%TRANSFORM, err=err)
      THROWM(err.NE.0, "MFASIS-NN:nn_transform_input: selected trafo does not exist")


      ALLOCATE(xx(mfasisnn_coefs%nn(nn)%n_input))
      ALLOCATE(yy(mfasisnn_coefs%nn(nn)%n_output))

      xx = (/tauw_nn, taui_nn, tauwi_top_nn, tauwi_bot_nn, &
                 reffw_top_nn, reffi_top_nn, reffw_bot_nn, reffi_bot_nn, &
                reffwi_top_nn, reffwi_bot_nn, psfc_nn, fpct_nn, sza_nn, vza_nn, alpha_nn/)

      yy(:) = 0._jprb

      CALL fornado_inference( 1_jpim,                             &
             mfasisnn_coefs%nn(nn)%n_input, mfasisnn_coefs%nn(nn)%n_output, &
             mfasisnn_coefs%nn(nn)%n_hidden, mfasisnn_coefs%nn(nn)%n_nodes_max, &
             mfasisnn_coefs%nn(nn)%actfunc_hl, mfasisnn_coefs%nn(nn)%actfunc_ol, &
             mfasisnn_coefs%nn(nn)%weight_i, mfasisnn_coefs%nn(nn)%weight_h, mfasisnn_coefs%nn(nn)%weight_o, &
             mfasisnn_coefs%nn(nn)%bias_i, mfasisnn_coefs%nn(nn)%bias_h, mfasisnn_coefs%nn(nn)%bias_o, &
             xx, yy, 1_jpim)

      ! --------------------------------------------------------------------------
      ! Transform input variables (by normalization) to produce NN input
      ! --------------------------------------------------------------------------
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
      ! Compute refl for model profile albedo (Jonkheid et al.)
      ! --------------------------------------------------------------------------
      IF ( yy(3) > 1E-4 ) THEN
        eta = (yy(3) - yy(2))
        gamma = ( yy(3) + yy(2) ) * yy(2) 
        refl_clr = yy(1) + albedo * gamma/(yy(3) - albedo*eta)
      ELSE
        refl_clr = yy(1) + albedo * (yy(3) + yy(2))
      ENDIF

      DEALLOCATE(xx, yy)

      refl = refl + colwei_clr* refl_clr

    ELSE
      err = errorstatus_fatal
      THROWM(err.NE.0, "MFASIS-NN not yet implemented for other cases than clouds")
    ENDIF ! IF ( mfasisnn_coefs%file_type .EQ. mfasis_cld ) !clouds, clear-sky column

    ! --------------------------------------------------------------------------
    ! Fill radiance structure
    ! --------------------------------------------------------------------------
    radiance%refl(i) = refl
    radiance%refl_clear(i) = refl_clr

    radiance%total(i) = refl * solar_spectrum(i) * pi_r  &
       * COS(profiles(prof)%sunzenangle * deg2rad)
    radiance%clear(i) =  radiance%refl_clear(i)  * solar_spectrum(i) * pi_r  &
       * COS(profiles(prof)%sunzenangle * deg2rad)
    radiance%cloudy(i) = radiance%total(i)

  ENDDO ! nchanprof

  ! --------------------------------------------------------------------------
  ! Tidy up
  ! --------------------------------------------------------------------------
  DEALLOCATE( dtauw,  &
              dtaui,  &
              fmp,    &
              dreffw, &
              dreffi, &
              fbotw,  &
              fboti,  &
              stat = err )
  THROWM(err.NE.0, "Deallocation of memory for rttov_mfasis_nn failed")

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN', 1_jpim, ZHOOK_HANDLE)

  CATCH
 
  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_NN', 1_jpim, ZHOOK_HANDLE)


  CONTAINS

  !----------------------------------------------------------------------------------------------
  !
  !   FORNADO NEURAL NETWORK INFERENCE
  !   2019-10 L.Scheck
  !
  !   to generate a python module from this subroutine use
  !     f2py -c fornado_inference.F90 -m fornado_inference_f90 --f90flags="-O3 -DVECTORIZE"
  !   (for a debug version, use
  !     f2py -c fornado_inference.F90 -m fornado_inference_f90 --f90flags="-g -fcheck='all' -DDBG -DVECTORIZE"
  !   (see http://cens.ioc.ee/projects/f2py2e/usersguide/f2py_usersguide.pdf )
  !
  !----------------------------------------------------------------------------------------------

  subroutine fornado_inference( n_samples,                             &
                    n_input, n_output, n_hidden, n_nodes,  &
                             act_h,    act_o,             &
                    weight_i, weight_h, weight_o,          &
                    bias_i,   bias_h,   bias_o,            &
                    x, y, nchunk )

    ! fast replacement for tensorflows predict method

    implicit none
    integer, parameter :: sp = jprb !CSt: sp = kind(1.0)

    integer(jpim), intent(in) :: n_samples, n_input, n_output, n_hidden, n_nodes

    integer(jpim), intent(in) :: act_h, act_o

    real(sp), dimension( n_input, n_nodes ),             intent(in) :: weight_i
    real(sp), dimension( n_nodes, n_nodes, n_hidden-1 ), intent(in) :: weight_h
    real(sp), dimension( n_nodes, n_output ),            intent(in) :: weight_o

    real(sp), dimension( n_nodes ),                      intent(in) :: bias_i
    real(sp), dimension( n_nodes, n_hidden-1 ),          intent(in) :: bias_h
    real(sp), dimension( n_output ),                     intent(in) :: bias_o

    real(sp), dimension( n_samples, n_input  ),          intent(in) :: x
    real(sp), dimension( n_samples, n_output ),       intent(inout) :: y

    integer(jpim), optional, intent(in) :: nchunk

#ifdef VECTORIZE
    real(sp), dimension( :, : ), allocatable :: sigi, sigo
    integer(jpim) :: i, j, k, np, nc, k_start, k_end, k_len, n_chunk
#else
    real(sp), dimension( n_nodes ) :: sigi, sigo
    integer(jpim) :: i_sample, i, j, l
#endif

#ifdef VECTORIZE
    ! vectorized version: inner loop over samples ------------------------------

    ! process input in chunks of size n_chunk
    if( present(nchunk) ) then
      n_chunk = nchunk
    else
      n_chunk = 2048
    end if
    allocate( sigi(n_chunk,n_nodes) )
    allocate( sigo(n_chunk,n_nodes) )

    do k_start = 1, n_samples, n_chunk
      k_end = min( k_start + n_chunk - 1, n_samples )
      k_len = 1 + k_end - k_start
      !print *, 'CHUNK ', k_start, k_end, k_len

      ! from input to first hidden layer ........................................
      np = n_input
      nc = n_nodes

      ! loop over nodes of current layer
      do i = 1, nc
        do k = 1, k_len
          sigo(k,i) = bias_i(i)
        end do

        ! loop over nodes of previous layer
        do j = 1, np

          ! loop over samples
          do k = 1, k_len
            sigo(k,i) = sigo(k,i) + weight_i(j,i) * x(k_start+k-1,j)
          end do
        end do
      end do

      ! apply activation function
      call actfunc( act_h, sigo(1:k_len,1:nc), sigi(1:k_len,1:nc) )

      ! propagate signal towards last hidden layer ..............................
      np = n_nodes
      nc = n_nodes
      do l = 1, n_hidden - 1

        ! loop over nodes of current layer
        do i = 1, nc
          do k = 1, k_len
            sigo(k,i) = bias_h(i,l)
          end do

          ! loop over nodes of previous layer
          do j = 1, np

            ! loop over samples
            do k = 1, k_len
              sigo(k,i) = sigo(k,i) + weight_h(j,i,l) * sigi(k,j)
            end do
          end do
        end do

        ! apply activation function
        call actfunc( act_h, sigo(1:k_len,1:nc), sigi(1:k_len,1:nc) )

      end do ! hidden layer loop

      ! propagate signal to output layer ..........................................
      np = n_nodes
      nc = n_output

      ! loop over nodes of current layer
      do i = 1, nc
        do k = 1, k_len
          sigo(k,i) = bias_o(i)
        end do

        ! loop over nodes of previous layer
        do j = 1, np

          ! loop over samples
          do k = 1, k_len
            sigo(k,i) = sigo(k,i) + weight_o(j,i) * sigi(k,j)
          end do
        end do
      end do

      ! apply activation function
      call actfunc( act_o, sigo(1:k_len,1:nc), sigi(1:k_len,1:nc) )

      y(k_start:k_end,:) = sigi(1:k_len,1:nc)

    end do ! chunk loop

    deallocate( sigi )
    deallocate( sigo )

#else
    ! unvectorized version: inner loop over nodes -------------------------------

    ! loop over all input data sets
    do i_sample = 1, n_samples

      ! propagate signal from input layer to first hidden layer
      !sigo = matmul( transpose(weight_i), x(i_sample,:) ) + bias_i
      do i = 1, n_nodes
        sigo(i) = bias_i(i)
        do j = 1, n_input
          sigo(i) = sigo(i) + weight_i(j,i) * x(i_sample,j)
        end do
      end do

      ! apply activation function
      call actfunc( act_h, sigo, sigi )

      ! propagate signal towards last hidden layer
      do l=1, n_hidden-1

        sigo = matmul( transpose(weight_h(:,:,l)), sigi ) + bias_h(:,l)

        ! apply activation function
        call actfunc( act_h, sigo, sigi )
      end do

      sigo(1:n_output) = matmul( transpose(weight_o(:,:)), sigi ) + bias_o(:)
      ! apply activation function
      call actfunc( act_o, sigo(1:n_output), y(i_sample,:) )

    end do
#endif
  end subroutine fornado_inference


END SUBROUTINE rttov_mfasis_nn
