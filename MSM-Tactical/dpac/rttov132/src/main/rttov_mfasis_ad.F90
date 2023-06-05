! Description:
!> @file
!!   AD of MFASIS fast visible/near-IR scattering model.
!
!> @brief
!!   AD of MFASIS fast visible/near-IR scattering model.
!!
!!
!! @param[out]    err               status on exit
!! @param[in]     chanprof          specifies channels and profiles to simulate
!! @param[in]     chanflag          flags to indicate which channels with LUT available
!! @param[in]     opts              options to configure the simulations
!! @param[in]     profiles          input atmospheric profiles and surface variables
!! @param[in,out] profiles_ad       input profile increments
!! @param[in]     profiles_int      profiles in internal units
!! @param[in,out] profiles_int_ad   profile increments in internal units
!! @param[in]     coefs             coefficients structure for instrument to simulate
!! @param[in]     ircld             information on cloud columns
!! @param[in,out] ircld_ad          cloud column increments
!! @param[in]     aux               additional internal profile variables
!! @param[in,out] aux_ad            additional internal profile variable increments
!! @param[in]     reflectance       surface BRDFs
!! @param[in,out] reflectance_ad    surface BRDF increments
!! @param[in]     solar_spectrum    TOA solar irradiance for each channel
!! @param[in]     trans_scatt_ir    cloud/aerosol optical depths
!! @param[in,out] trans_scatt_ir_ad cloud/aerosol optical depth increments
!! @param[in]     mfasis_refl       quantities computed by rttov_mfasis used by TL/AD/K
!! @param[in,out] radiance_ad       input gradient wrt radiances
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
!    Copyright 2017, EUMETSAT, All Rights Reserved.
!
SUBROUTINE rttov_mfasis_ad( &
              err,              &
              chanprof,         &
              chanflag,         &
              opts,             &
              profiles,         &
              profiles_ad,      &
              profiles_int,     &
              profiles_int_ad,  &
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
              mfasis_refl,      &
              radiance_ad)

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
    rttov_radiance,              &
    rttov_mfasis_refl
!INTF_OFF
  USE rttov_types, ONLY : &
    rttov_coef_mfasis,           &
    rttov_mfasis_axis

  USE rttov_const, ONLY : &
    wcl_opac_deff,               &
    pi,                          &
    deg2rad,                     &
    pi_r,                        &
    clw_scheme_deff,             &
    mfasis_cld,                  &
!     mfasis_aer,                  &
    mfasis_dim_albedo,           &
    mfasis_dim_kfourier,         &
    mfasis_dim_lfourier,         &
    mfasis_dim_opdp,             &
    mfasis_dim_effdia,           &
    mfasis_dim_scaangle,         &
    gas_id_watervapour,          &
    gas_mass,                    &
    mair,                        &
    gravity,                     &
    nwcl_max

  USE yomhook, ONLY : lhook, dr_hook, jphook
!INTF_ON
  IMPLICIT NONE
  INTEGER(jpim),                      INTENT(OUT)   :: err
  TYPE(rttov_chanprof),               INTENT(IN)    :: chanprof(:)
  LOGICAL(jplm),                      INTENT(IN)    :: chanflag(SIZE(chanprof))
  TYPE(rttov_options),                INTENT(IN)    :: opts
  TYPE(rttov_profile),                INTENT(IN)    :: profiles(:)
  TYPE(rttov_profile),                INTENT(INOUT) :: profiles_ad(:)
  TYPE(rttov_profile),                INTENT(IN)    :: profiles_int(:)
  TYPE(rttov_profile),                INTENT(INOUT) :: profiles_int_ad(:)
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
  TYPE(rttov_mfasis_refl),            INTENT(IN)    :: mfasis_refl(0:,:)
  TYPE(rttov_radiance),               INTENT(INOUT) :: radiance_ad
!INTF_END

#include "rttov_errorreport.interface"

  INTEGER(jpim)              :: nchanprof, prof, ncolms, npar, chan
  INTEGER(jpim)              :: nlay, nlev, ndim, nid
  INTEGER(jpim)              :: d, n, i, j, k, cc, par, jj
  TYPE(rttov_coef_mfasis)    :: mfasis_coefs
  REAL(jprb),    ALLOCATABLE :: ip   (:)
  REAL(jprb),    ALLOCATABLE :: ip_ad(:), iw_ad(:)

  REAL(jprb),    ALLOCATABLE :: q_mxr(:),dvap(:), dvap_1(:)
  REAL(jprb),    ALLOCATABLE :: q_mxr_ad(:),dvap_ad(:)
  REAL(jprb),    ALLOCATABLE :: array1(:)                  
  REAL(jprb)                 :: wvint_bot, wvint_top 
  REAL(jprb)                 :: wvint_bot_ad, wvint_top_ad 
  REAL(jprb)                 :: iw_wv_ad(3)
  INTEGER(jpim)              :: n_wvdim


  INTEGER(jpim), ALLOCATABLE :: di(:)
  REAL(jprb),    ALLOCATABLE :: od   (:,:), od_eff   (:), ed   (:,:), ed_eff(:), ed_aux(:), od_mpcorr(:,:)
  ! versions of nonlinear quantities needed for TL/AD/K:
  REAL(jprb),    ALLOCATABLE :: od_v1(:,:), od_eff_v1(:), od_mpcorr_v1(:,:), ed_eff_v1(:)
  REAL(jprb),    ALLOCATABLE :: od_ad(:,:), od_eff_ad(:), ed_ad(:,:), ed_eff_ad(:), od_mpcorr_ad(:,:)
  REAL(jprb),    ALLOCATABLE :: odlay(:), odlay_ad(:)

  REAL(jprb)                 :: albedo
  REAL(jprb)                 :: albedo_ad
  REAL(jprb)                 :: theta, theta0, mu, mu0
  TYPE(rttov_mfasis_axis)    :: axis
  REAL(jprb)                 :: colwei_clr, colwei_cc
  REAL(jprb)                 :: refl_ad, rflcol_ad, refl_clr_ad, colwei_ad
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  REAL(jprb)                 :: od1_int
  REAL(jprb)                 :: od1_int_ad, ooo1_ad
  REAL(jprb),    ALLOCATABLE :: ooo1(:)
  REAL(jprb),    ALLOCATABLE :: fac_mx(:)
  REAL(jprb),    ALLOCATABLE :: fac_mx_ad(:)
  REAL(jprb)                 :: odint, a, zenmax, ampl, sigma, delta
  REAL(jprb)                 :: odint_ad
  REAL(jprb),    ALLOCATABLE :: ed_eff_sscat(:), fac_rm(:)
  REAL(jprb),    ALLOCATABLE :: ed_eff_sscat_ad(:), fac_rm_ad(:)
  REAL(jprb),    ALLOCATABLE :: w_sscat(:,:)
  REAL(jprb),    ALLOCATABLE :: w_sscat_ad(:,:)

  REAL(jphook) :: zhook_handle
  ! --------------------------------------------------------------------------------------

  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_AD', 0_jpim, ZHOOK_HANDLE)

  ! TODO: Adapt to water vapor correction using three LUTs
  ! TODO: Test aerosol functionality

  ! --------------------------------------------------------------------------
  ! Initialisation
  ! --------------------------------------------------------------------------
  nchanprof = SIZE(chanprof)
  nlay = profiles(1)%nlayers
  nlev = nlay+1

  ! Check whether cloud or aerosol simulation (currently both simultaneously not supported)
  IF (opts%rt_ir%addclouds ) THEN
    mfasis_coefs = coefs%coef_mfasis_cld
  ELSE
    mfasis_coefs = coefs%coef_mfasis_aer
  ENDIF

  npar = mfasis_coefs%nparticles        ! # of particles: 2 for clouds (water and ice)
                                        !                 or # of aerosols considered
  ndim = mfasis_coefs%ndims             ! # of LUT dimensions

  ! Find number of dimensions that are not interpolated (fourier coefs and albedo)
  n = 0_jpim
  DO d = 1, ndim
    IF (mfasis_coefs%lut_axes(d)%dim_type /= mfasis_dim_kfourier .AND. &
        mfasis_coefs%lut_axes(d)%dim_type /= mfasis_dim_lfourier .AND. &
        mfasis_coefs%lut_axes(d)%dim_type /= mfasis_dim_albedo ) CYCLE
    n = n + 1
  END DO

  nid = ndim - n ! Total number of dimensions to be interpolated (total - albedo - fourier indices)

  ALLOCATE( ip     (nid) , &      ! interpolation point
            ip_ad  (nid) , &      ! interpolation point
            iw_ad  (nid) , &      ! interpolation weight
            di     (nid), STAT=err )  ! original dimension index in nid arrays
  THROWM(err.NE.0,"Allocation of memory for rttov_mfasis_ad failed")


  ! Populate di and set some relevant dimensions indices
  di(:) = -1_jpim ! Indices of dimensions to interpolate

  n = 1_jpim
  DO d = 1, ndim
    SELECT CASE (mfasis_coefs%lut_axes(d)%dim_type)
    CASE (mfasis_dim_kfourier, mfasis_dim_lfourier, mfasis_dim_albedo)

    CASE DEFAULT
      di(n) = d
      n = n + 1
    END SELECT
  ENDDO


  ALLOCATE( od_eff    (npar)    , & ! Effective total optical depth per type of particle
            od_eff_v1 (npar)    , & ! version of od_eff for adjoint computations
            od_eff_ad (npar)    , & ! Effective total optical depth per type of particle
            od     (npar, nlay) , & ! Effective optical depth per type of particle and layer
            od_v1  (npar, nlay) , & ! od saved for adjoint computations
            od_ad  (npar, nlay) , & ! Effective optical depth per type of particle and layer
            od_mpcorr(npar, nlay)   , &! Effective mixed-phase corrected optical depth per type of particle and layer
            od_mpcorr_v1(npar, nlay), &! od_mpcorr saved for adjoint computations
            od_mpcorr_ad(npar, nlay), &! Effective mixed-phase corrected optical depth per type of particle and layer
            odlay   (nlay)          , &   ! Effective mixed-phase corrected optical depth of water and ice
            odlay_ad(nlay)          , &   ! Effective mixed-phase corrected optical depth of water and ice
            STAT=err)
  THROWM(err.NE.0,"Allocation of memory for rttov_mfasis_ad failed")

  ! For the time being we assume effective diameters will be out of LUT for aerosols
  IF ( mfasis_coefs%file_type .EQ. 1 ) THEN
    ALLOCATE( ed_eff (npar)      , & ! Total effective diameter per type of particle
              ed_aux (npar)      , & ! Effective diameters to be used with 0 opdp to avoid flagging
              ed_eff_v1 (npar)   , & ! Total effective diameter per type of particle
              ed_eff_ad(npar)    , & ! Total effective diameter per type of particle
              fac_mx (nlay),      &         ! fraction of cloud considered as (potentially) mixed cloud
              fac_mx_ad (nlay),   &         ! fraction of cloud considered as (potentially) mixed cloud
              ooo1 (nlay),        &         ! discriminator for switching on mixed layer summation
              ed   (npar, nlay)     , & ! Effective diameter per type of particle and layer
              ed_ad(npar, nlay)     , &
              w_sscat(npar, nlay)   , & ! Weighting factor for radius mixing approach (1.6um channel)
              w_sscat_ad(npar, nlay), & ! Weighting factor for radius mixing approach (1.6um channel)
              ed_eff_sscat(npar)    , & ! Effective diameter contribution in radius mixing approach (1.6um channel)
              ed_eff_sscat_ad(npar) , & ! Effective diameter contribution in radius mixing approach (1.6um channel)
              fac_rm(npar)          , & ! Weighting factor for radius mixing approach (1.6um channel)
              fac_rm_ad(npar)       , & ! Weighting factor for radius mixing approach (1.6um channel)
              q_mxr(nlev)           , &
              q_mxr_ad(nlev)        , &
              dvap (nlay)           , &
              dvap_1(nlay)          , &
              dvap_ad (nlay)        , &
              array1(0:nlay+1)      , &
              STAT=err) ! 
                           
    THROWM(err.NE.0,"Allocation of memory for rttov_mfasis_ad failed")

    ! Effective diameters to be used with 0 opdp to avoid incorrect flagging for clear column
    n = 1_jpim
    DO d = 1, ndim
      IF (mfasis_coefs%lut_axes(d)%dim_type .NE. mfasis_dim_effdia) CYCLE
      ed_aux(n) = mfasis_coefs%lut_axes(d)%values(1)
      n = n + 1
    ENDDO
  ENDIF
  fac_mx (:) = 0
  ooo1 (:)   = 0

!=====================================================================================
! Make sure all local linear variables are initialized as zeros
!=====================================================================================
  ip_ad(:)   = 0
  iw_ad(:)   = 0
  iw_wv_ad(:)= 0
  wvint_bot_ad=0._jprb
  wvint_top_ad=0._jprb


  od_ad     (:,:)  = 0
  od_eff_ad (:)    = 0
  ed_ad     (:,:)  = 0
  ed_eff_ad (:)    = 0
  fac_mx_ad (:)    = 0

  albedo_ad  = 0
  refl_ad    = 0
  rflcol_ad    = 0
  refl_clr_ad  = 0
  colwei_ad    = 0

  od1_int_ad        = 0
  od_mpcorr_ad(:,:) = 0
  ooo1_ad           = 0


  odlay(:) = 0
  odlay_ad(:) = 0
  odint = 0
  odint_ad = 0

  w_sscat(:,:) = 0
  w_sscat_ad(:,:) = 0
  ed_eff_sscat(:) = 0
  ed_eff_sscat_ad(:) = 0
  fac_rm(:) = 0
  fac_rm_ad(:) = 0       
!=====================================================================================

  ! --------------------------------------------------------------------------
  ! Channel loop
  ! --------------------------------------------------------------------------
  DO i = 1, nchanprof ! channel loop
    IF (.NOT. chanflag(i)) CYCLE
    prof = chanprof(i)%prof

    chan = chanprof(i)%chan

    n_wvdim=size(mfasis_coefs%lut(chan)%qint,2)
    dvap(:)=0._jprb
    dvap_ad(:)=0._jprb
    IF(n_wvdim==3) THEN
      q_mxr(:) =  profiles_int(prof)%q(:)* gas_mass(gas_id_watervapour)/  &
                  (mair * 1.E06_jprb +profiles_int(prof)%q(:)*gas_mass(gas_id_watervapour))

      dvap  (1:nlay)  = 100._jprb * (profiles(prof)%p(2:nlev)-profiles(prof)%p(1:nlev-1))* &
                                           0.5_jprb * (q_mxr(2:nlev)+q_mxr(1:nlev-1))/gravity
      dvap_1(1:nlay)  = dvap(1:nlay) 

      jj = aux%s(prof)%nearestlev_surf - 1
      IF(jj>=1 .AND. jj<=nlay) THEN
        dvap   (jj)=dvap   (jj)*(1._jprb - aux   %s(prof)%pfraction_surf)
      ENDIF
    ENDIF


    colwei_clr = ircld%xcolclr(prof) ! clear column

    ip(:) = -1

    IF ( mfasis_coefs%file_type == mfasis_cld ) THEN
      ncolms = ircld%ncolumn(prof)
    ELSE
      ncolms = 1_jpim
    ENDIF

    ! --------------------------------------------------------------------------
    ! Angles (in radians except alpha_deg)
    ! --------------------------------------------------------------------------
    theta = profiles(prof)%zenangle * deg2rad
    theta0 = profiles(prof)%sunzenangle * deg2rad
    mu  = COS(theta)
    mu0 = COS(theta0)


!=====================================================================================
!  linear computations independent of column cc
!=====================================================================================
!================================
! To be consistent with other RTTOV calls we only use input AD/K increments in
! radiance%total and we assume all other radiance arrays are zero
    refl_ad    = refl_ad     + solar_spectrum(i) * pi_r   &
      * COS(profiles(prof)%sunzenangle * deg2rad) * radiance_ad%total(i)

    colwei_ad  =colwei_ad   + mfasis_refl(0,i)%refl*refl_ad
    refl_clr_ad=refl_clr_ad + colwei_clr     *refl_ad

    ircld_ad%xcolclr(prof)=ircld_ad%xcolclr(prof) + colwei_ad
    colwei_ad=0._jprb

    iw_wv_ad(1:n_wvdim) = iw_wv_ad(1:n_wvdim) + mfasis_refl(0,i)%refl_wv(1:n_wvdim)*refl_clr_ad

    DO d = 1, nid
      IF ( mfasis_coefs%lut_axes(di(d))%dim_type .EQ. mfasis_dim_scaangle ) THEN
        albedo_ad = albedo_ad + mfasis_refl(0,i)%refl_lin_coef(d)* refl_clr_ad
      ELSE
        iw_ad(d)    = iw_ad(d)   +mfasis_refl(0,i)%refl_lin_coef(d)* refl_clr_ad
      ENDIF
    ENDDO
    refl_clr_ad = 0


    DO d = 1, nid
      IF ( mfasis_coefs%lut_axes(di(d))%dim_type .EQ. mfasis_dim_opdp ) THEN
        iw_ad(d) = 0._jprb
      ENDIF
    ENDDO

!=============================================================================================================
!   IF(n_wvdim==3) THEN
!     wvint_bot_tl=0._jprb
!     wvint_top_tl=0._jprb
!     DO j = 1, nlay
!       wvint_top_tl = wvint_top_tl +  dvap_tl(j)
!     ENDDO
!   ENDIF
!   iw_wv_tl(:) = 0._jprb
!   if( n_wvdim==3) THEN
!       iw_wv_tl(2)=                  wvint_bot_tl  / &
!            (mfasis_coefs%lut(chan)%qint(1,2) - mfasis_coefs%lut(chan)%qint(1,1))
!       iw_wv_tl(3)=                  wvint_top_tl  / &
!            (mfasis_coefs%lut(chan)%qint(2,3) - mfasis_coefs%lut(chan)%qint(2,1))
!       iw_wv_tl(1) =       - iw_wv_tl(2) - iw_wv_tl(3)
!    ENDIF
!=============================================================================================================
     IF( n_wvdim==3) THEN
       iw_wv_ad(2) = iw_wv_ad(2) - iw_wv_ad(1)
       iw_wv_ad(3) = iw_wv_ad(3) - iw_wv_ad(1)
       iw_wv_ad(1) = 0
       wvint_top_ad = wvint_top_ad + iw_wv_ad(3) / &
              (mfasis_coefs%lut(chan)%qint(2,3) - mfasis_coefs%lut(chan)%qint(2,1))
       wvint_bot_ad = wvint_bot_ad + iw_wv_ad(2)  / &
               (mfasis_coefs%lut(chan)%qint(1,2) - mfasis_coefs%lut(chan)%qint(1,1))
     ENDIF
     iw_wv_ad(:) = 0._jprb
     IF(n_wvdim==3) THEN
       DO j = nlay, 1, -1
!        wvint_top_tl = wvint_top_tl +  dvap_tl(j)
         dvap_ad(j) = dvap_ad(j) + wvint_top_ad
       ENDDO
       wvint_bot_ad=0._jprb
       wvint_top_ad=0._jprb
     ENDIF




!=====================================================================================

    DO cc = 1, ncolms
!==================================================================================
! First compute nonlinear quatities needed in linear computations (for given channel "i" and column "cc")
!==================================================================================
      ! --------------------------------------------------------------------------
      ! Set column weight, optical depth and effective diameters
      ! --------------------------------------------------------------------------
      IF ( mfasis_coefs%file_type .EQ. mfasis_cld ) THEN ! clouds
        colwei_cc = ircld%xcol(cc+1,prof) - ircld%xcol(cc,prof)
        od   (:,:) = 0._jprb
        ed   (:,:) = 0._jprb
        od1_int = 0._jprb
        fac_mx(:)= 0._jprb
        od_mpcorr(:,:)=0._jprb
        wvint_bot=0._jprb
        wvint_top=0._jprb

        DO j = 1, nlay
          IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
          IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN
            od   (1,j) = SUM(trans_scatt_ir%opdpext(1:nwcl_max,j,i)) ! water clouds
            od   (2,j) = trans_scatt_ir%opdpext(nwcl_max+1,j,i)        ! ice clouds
            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
              od_v1(:,j) = od   (:,j)
              od   (:,j) = od   (:,j) * (1 - aux%s(prof)%pfraction_surf)
            ENDIF

            !-----------------------------------------------------------------------
            ! mixed-phase correction per layer: add ice optical depth below water
            ! cloud top to water optical depth and subtract from ice optical depth
            !-----------------------------------------------------------------------
            od1_int = od1_int + od(1,j)
            ooo1(j)=od1_int-opts%dev%od1_thresh
            fac_mx(j)=(1._jprb + TANH(ooo1(j)/opts%dev%o_del1))/2._jprb

            od_mpcorr(1,j) = od(1,j) + fac_mx(j)*od(2,j)
            od_mpcorr(2,j) = od(2,j) - fac_mx(j)*od(2,j)
            od_mpcorr_v1(1,j) =  od_mpcorr(1,j)
            od_mpcorr_v1(2,j) =  od_mpcorr(2,j)
            IF (od_mpcorr_v1(2,j) < 0) od_mpcorr(2,j) = 0._jprb

            !-----------------------------------------------------------------------
            ! determine effective diameter
            !-----------------------------------------------------------------------
            IF ( coefs%coef_mfasis_cld%clw_scheme == clw_scheme_deff ) THEN ! Deff water clouds scheme
              ed   (1,j) = aux%clw_dg(j,prof)
            ELSE ! OPAC water cloud parametrization
              ed   (1,j) = SUM(wcl_opac_deff(1:nwcl_max) * trans_scatt_ir%opdpext(1:nwcl_max,j,i))
              IF ( od(1,j) .GT. 0. ) THEN
                ! TODO: take into account optical depth surface correction in eff diameter calculation
                ed   (1,j) =  ed   (1,j)/SUM(trans_scatt_ir%opdpext(1:nwcl_max,j,i))
              ELSE
                ed   (1,j) = 0._jprb
              ENDIF
            ENDIF
            ed   (2,j) = aux%ice_dg(j,prof)
          ELSEIF(j>1) THEN
            fac_mx(j) = fac_mx(j-1)
          ENDIF
          IF(n_wvdim==3) THEN
            wvint_bot = wvint_bot + fac_mx(j)          * dvap(j)
            wvint_top = wvint_top + (1._jprb-fac_mx(j))* dvap(j)
          ENDIF
        ENDDO
        !-----------------------------------------------------------------------
        ! determine integrated optical depths and effective diameter
        !-----------------------------------------------------------------------
        ! water: use original optical depths for computing the effective diameter, then overwrite od_eff(1)
        od_eff(1) = SUM(od(1,:))
        od_eff_v1(1) = od_eff(1)
        IF ( od_eff_v1(1) .GT. 0. ) THEN
          ed_eff(1) = SUM( od(1,:)*ed(1,:))/od_eff_v1(1)
          ed_eff_v1(1) = ed_eff(1)
        ELSE
          ed_eff(1) = ed_aux(1)
          ed_eff_v1(1) = ed_eff(1)
        ENDIF
        od_eff(1) = SUM(od_mpcorr(1,:))

        ! ice: use mixed-phase corrected optical depths for computing the effective diameter
        od_eff(2) = SUM(od_mpcorr(2,:))
        od_eff_v1(2) = od_eff(2)
        IF ( od_eff_v1(2) .GT. 0. ) THEN
          ed_eff(2) = SUM( od_mpcorr(2,:)*ed(2,:))/od_eff_v1(2)
          ed_eff_v1(2) = ed_eff(2)
        ELSE
          ed_eff(2) = ed_aux(2)
          ed_eff_v1(2) = ed_eff(2)
        ENDIF

        !-----------------------------------------------------------------------
        ! special treatment for nir channels: radius mixing approach
        ! (currently only for 1.6um channel)
        !-----------------------------------------------------------------------
        IF (mfasis_coefs%channel_deff_mixing(chan) .EQ. 1) THEN
          ! compute weights for computing single-scattering effective diameter 
          ! (scattering takes place mainly in upper cloud layers)
          ! R = N^{-1} \int_0^{TOA} r(k,z) * beta(k,z) * exp(-(od(1,z)+od(2,z))/mu) * exp(-(od(1,z)+od(2,z))/mu0) dz
          !   = N^{-1} \int_0^{TOA} r(k,z) * beta(k,z) * exp(-(od(1,z)+od(2,z))/a) dz
          ! note: since exp(-(od(1,z)+od(2,z))/a) is nonlinear, use mean of function for integrating
          ! 1/(tau_j - tau_{j-1}) * int_0^{tau_j - tau_{j-1}} exp(-(x+tau_{j-1})/a) dx = 
          !     = a/(tau_j - tau_{j-1}) * ( exp((tau_j - tau_{j-1})/a) - 1) * exp(-tau_j/a)
          !     = a/(od(1,j) + od(2,j)) * ( exp((od(1,j) + od(2,j) - odint)/a) - exp(-odint/a) )
          ! -> R = N^{-1} \int_0^{TOA} r(k,z) * beta(k,z) * (a*/(od(1,j) + od(2,j))) * &
          !        ( exp((od(1,j) + od(2,j) - odint)/a) - exp(-odint/a) ) * dz
          ! use beta(k,z) * dz = od(k,j)/dz * dz = od(k,j)

          a = ( mu*mu0 ) / ( mu+mu0 ) ! factor from integral
          w_sscat(:,:) = 0._jprb
          ed_eff_sscat(:) = 0._jprb
          odint = 0._jprb

          ! ice phase (neglect water phase for the moment, has only little effect)
          IF (od_eff(2) > 0.1_jprb) THEN ! exclude special treatment for very thin clouds
            DO j = 1, nlay
              IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN ! this check is probably redundant, od_eff > 0!
                odlay(j) = od_mpcorr(1,j) + od_mpcorr(2,j)
                odint = odint + odlay(j) ! total integraged optical depth (mpc corr.)
                IF ( odlay(j) > 1E-10 .and. a > 1E-10 ) THEN
                  w_sscat(2,j) = (a*od_mpcorr(2,j)/odlay(j)) * ( EXP((odlay(j) - odint)/a) - EXP(-odint/a) )
                ENDIF
              ENDIF
            ENDDO
          ENDIF
          IF (SUM(w_sscat(2,:)) > 0) THEN
            ed_eff_sscat(2) = SUM( ed(2,:) * w_sscat(2,:)) / SUM(w_sscat(2,:))
          ENDIF

          !-----------------------------------------------------------------------
          ! Radius mixing parameterized with Gaussian function:
          zenmax = MAX(theta,theta0)
          fac_rm(:) = 0._jprb
          ! Ice phase:
          ampl = 0.6790 - 0.7840*zenmax + 1.1925*zenmax**2
          sigma = 0.4927 - 0.2109*zenmax + 0.4710*zenmax**2
          delta = 1E-6 ! avoid numerical problems for od_eff(2) ~ 0
          fac_rm(2) = ampl/(sigma*SQRT(2._jprb*pi)) * DEXP(-0.5*((LOG(od_eff(2)+delta) - 3.15)/sigma)**2)
          
          ed_eff(2) = fac_rm(2)*ed_eff_sscat(2) + (1-fac_rm(2))*ed_eff(2)
        ENDIF

      ELSE  ! aerosols
        colwei_cc = 1._jprb     ! single column for aerosols
        od   (:,:)= 0._jprb
        DO k = 1, npar
          par = mfasis_coefs%aer_types(k)
          DO j = 1, nlay
            IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
            od(k,j)    = trans_scatt_ir%opdpext(par,j,i)
            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
              od_v1(:,j) = od   (:,j)
              od   (k,j) = od   (k,j) * (1 - aux%s(prof)%pfraction_surf)
            ENDIF
          ENDDO
          od_eff   (k) = SUM(od   (k,:))
        ENDDO
      ENDIF
      ! --------------------------------------------------------------------------
      ! Fill into the interpolation point array the rest of dimensions
      ! --------------------------------------------------------------------------
      j = 1
      k = 1
      DO d = 1, nid
        SELECT CASE (mfasis_coefs%lut_axes(di(d))%dim_type)
        CASE (mfasis_dim_opdp)
          ip   (d) = od_eff   (j)
          j = j + 1
        CASE (mfasis_dim_effdia)
          ! Check IF stored as radius or diamater in LUT
          IF (mfasis_coefs%lut_axes(di(d))%name(1:1) .EQ. "R") THEN
            ip(d) = ed_eff(k)/2
          ELSE
            ip(d) = ed_eff(k)
          ENDIF
          k = k + 1
        END SELECT
      ENDDO

!==================================================================================
! Start linear computations for channel "i" and column "cc"
!==================================================================================
      rflcol_ad = rflcol_ad+  colwei_cc        * refl_ad
      colwei_ad = colwei_ad+   mfasis_refl(cc,i)%refl * refl_ad                  ! rflcol

      iw_wv_ad(1:n_wvdim) = iw_wv_ad(1:n_wvdim) + mfasis_refl(cc,i)%refl_wv(1:n_wvdim) * rflcol_ad

      DO d = 1, nid
        IF ( mfasis_coefs%lut_axes(di(d))%dim_type .EQ. mfasis_dim_scaangle ) THEN
          albedo_ad = albedo_ad +mfasis_refl(cc,i)%refl_lin_coef(d)* rflcol_ad
        ELSE
          iw_ad(d)  = iw_ad(d) +mfasis_refl(cc,i)%refl_lin_coef(d)* rflcol_ad
        ENDIF
      ENDDO
      rflcol_ad = 0

!     iw_wv_tl(:) = 0._jprb
!     IF( n_wvdim==3) THEN
!         iw_wv_tl(2)=                  wvint_bot_tl  / &
!              (mfasis_coefs%lut(chan)%qint(1,2) - mfasis_coefs%lut(chan)%qint(1,1))
!         iw_wv_tl(3)=                  wvint_top_tl  / &
!              (mfasis_coefs%lut(chan)%qint(2,3) - mfasis_coefs%lut(chan)%qint(2,1))
!         iw_wv_tl(1) =       - iw_wv_tl(2) - iw_wv_tl(3)
!      ENDIF

      IF( n_wvdim==3) THEN
        iw_wv_ad(2) = iw_wv_ad(2) - iw_wv_ad(1) 
        iw_wv_ad(3) = iw_wv_ad(3) - iw_wv_ad(1) 
        iw_wv_ad(1) = 0
        wvint_top_ad = wvint_top_ad + iw_wv_ad(3) / &
               (mfasis_coefs%lut(chan)%qint(2,3) - mfasis_coefs%lut(chan)%qint(2,1))
        wvint_bot_ad = wvint_bot_ad + iw_wv_ad(2)  / &
               (mfasis_coefs%lut(chan)%qint(1,2) - mfasis_coefs%lut(chan)%qint(1,1))
      ENDIF
      iw_wv_ad(:) = 0._jprb


      DO d = 1, nid
        IF (mfasis_coefs%lut_axes(di(d))%dim_type .EQ. mfasis_dim_scaangle) CYCLE ! Skip scattering angle (already done)
        axis = mfasis_coefs%lut_axes(di(d))

        IF ( ip(d) .GE. axis%values(axis%nvalues) ) THEN
          iw_ad(d)  = 0._jprb
        ELSE
!         DO j = 1, axis%nvalues - 1
          DO j = axis%nvalues - 1 ,1 ,-1
            IF ( ip(d) .GT. axis%values(j) ) THEN
              IF ( mfasis_coefs%lut_axes(di(d))%dim_type .EQ. mfasis_dim_opdp .AND. j .NE. 1 ) THEN
                ! optical depths ( lin. int. in log)
                ip_ad(d) = ip_ad(d) + iw_ad(d)/ip(d)                   &
                  / ( LOG(axis%values(j+1)) - LOG(axis%values(j)) )
                iw_ad(d) = 0
              ELSE            ! effective radii and alpha, linear interpolation
                ip_ad(d) = ip_ad(d) + iw_ad(d)               &
                  / ( axis%values(j+1) - axis%values(j) )
                iw_ad(d) = 0
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        iw_ad(d)  = 0._jprb
      ENDDO

!     ! --------------------------------------------------------------------------
!     ! Fill into the interpolation point array the rest of dimensions
!     ! --------------------------------------------------------------------------

      j = 1
      k = 1
      DO d = 1, nid
        SELECT CASE (mfasis_coefs%lut_axes(di(d))%dim_type)
        CASE (mfasis_dim_opdp)
          od_eff_ad(j)= od_eff_ad(j) + ip_ad(d)
          ip_ad(d) = 0
          j = j + 1
        CASE (mfasis_dim_effdia)
          ! Check if stored as radius or diamater in LUT
          IF (mfasis_coefs%lut_axes(di(d))%name(1:1) .EQ. "R") THEN
            ed_eff_ad(k) = ed_eff_ad(k) + ip_ad(d)/2
          ELSE
            ed_eff_ad(k) = ed_eff_ad(k) + ip_ad(d)
          ENDIF
          ip_ad(d) = 0
          k = k + 1
        END SELECT
      ENDDO

!==================================================================================
!  IF CLOUD ELSE AEROSOL
!==================================================================================
      ! --------------------------------------------------------------------------
      ! Set column weight, optical depth and effective diameters
      ! --------------------------------------------------------------------------
      IF ( mfasis_coefs%file_type .EQ. mfasis_cld ) THEN ! clouds
        !-----------------------------------------------------------------------
        ! special treatment for nir channels: radius mixing approach
        ! (currently only for 1.6um channel)
        !-----------------------------------------------------------------------
        IF (mfasis_coefs%channel_deff_mixing(chan) .EQ. 1) THEN
          ! compute weights for computing single-scattering effective diameter 
          ! (scattering takes place mainly in upper cloud layers)
          ! R = N^{-1} \int_0^{TOA} r(k,z) * beta(k,z) * exp(-(od(1,z)+od(2,z))/mu) * exp(-(od(1,z)+od(2,z))/mu0) dz
          !   = N^{-1} \int_0^{TOA} r(k,z) * beta(k,z) * exp(-(od(1,z)+od(2,z))/a) dz
          ! note: since exp(-(od(1,z)+od(2,z))/a) is nonlinear, use mean of function for integrating
          ! 1/(tau_j - tau_{j-1}) * int_0^{tau_j - tau_{j-1}} exp(-(x+tau_{j-1})/a) dx = 
          !     = a/(tau_j - tau_{j-1}) * ( exp((tau_j - tau_{j-1})/a) - 1) * exp(-tau_j/a)
          !     = a/(od(1,j) + od(2,j)) * ( exp((od(1,j) + od(2,j) - odint)/a) - exp(-odint/a) )
          ! -> R = N^{-1} \int_0^{TOA} r(k,z) * beta(k,z) * (a*/(od(1,j) + od(2,j))) * &
          !        ( exp((od(1,j) + od(2,j) - odint)/a) - exp(-odint/a) ) * dz
          ! use beta(k,z) * dz = od(k,j)/dz * dz = od(k,j)

          !-----------------------------------------------------------------------
          ! Radius mixing parameterized with Gaussian function:

          fac_rm_ad(2) = fac_rm_ad(2) + (ed_eff_sscat(2) - ed_eff_v1(2))*ed_eff_ad(2)
          ed_eff_sscat_ad(2) = ed_eff_sscat_ad(2) + fac_rm(2)*ed_eff_ad(2)
          ed_eff_ad(2) = (1-fac_rm(2))*ed_eff_ad(2)

          ! neglect water phase for the moment

          ! Ice phase:
          od_eff_ad(2) = od_eff_ad(2) - ampl/(sigma*SQRT(2._jprb*pi)) * DEXP(-0.5*((LOG(od_eff(2)+delta) - 3.15)/sigma)**2) &
                         * (LOG(od_eff(2)+delta) - 3.15)/sigma * 1/sigma * 1/(od_eff(2)+delta) * fac_rm_ad(2)
          fac_rm_ad(2) = 0._jprb         

          fac_rm_ad(:) = 0._jprb

          IF (SUM(w_sscat(2,:)) > 0) THEN
            ed_ad(2,:) = ed_ad(2,:) + w_sscat(2,:)/(SUM(w_sscat(2,:)))*ed_eff_sscat_ad(2)
            w_sscat_ad(2,:) = w_sscat_ad(2,:) + (ed(2,:)-ed_eff_sscat(2))/(SUM(w_sscat(2,:)))*ed_eff_sscat_ad(2)
            ed_eff_sscat_ad(2) = 0._jprb
          ENDIF

          ! ice phase (neglect water phase for the moment, has only little effect)
          IF (od_eff(2) > 0.1_jprb) THEN ! exclude special treatment for very thin clouds
            DO j = nlay, 1, -1 ! j = 1, nlay
              IF ( ircld%icldarr(cc,j,prof) .EQ. 1 ) THEN ! this check is probably redundant, od_eff > 0!
                IF ( odlay(j) > 1E-10 .and. a > 1E-10 ) THEN
                  od_mpcorr_ad(2,j) = od_mpcorr_ad(2,j) + a/odlay(j) * (EXP((odlay(j) - odint)/a) - EXP(-odint/a)) * w_sscat_ad(2,j)
                  odlay_ad(j) = odlay_ad(j) + ( - a* od_mpcorr(2,j)/(odlay(j)**2) * (EXP((odlay(j) - odint)/a) - EXP(-odint/a)) &
                                     + od_mpcorr(2,j)/odlay(j) * EXP((odlay(j) - odint)/a) ) * w_sscat_ad(2,j)
                  odint_ad = odint_ad + od_mpcorr(2,j)/odlay(j) * ( -EXP((odlay(j) - odint)/a) + EXP(-odint/a)) * w_sscat_ad(2,j)
                  w_sscat_ad(2,j) = 0._jprb
                ENDIF

                odlay_ad(j) = odlay_ad(j) + odint_ad
                odint_ad = odint_ad

                od_mpcorr_ad(1,j) = od_mpcorr_ad(1,j) + odlay_ad(j)
                od_mpcorr_ad(2,j) = od_mpcorr_ad(2,j) + odlay_ad(j)
                odlay_ad(j) = 0._jprb
              ENDIF
            ENDDO
          ENDIF

          odint_ad = 0._jprb

          ed_eff_sscat_ad(:) = 0._jprb
          w_sscat_ad(:,:) = 0._jprb
        ENDIF

        !-----------------------------------------------------------------------
        ! determine integrated optical depths and effective diameter
        !-----------------------------------------------------------------------
        ! ice: use mixed-phase corrected optical depths for computing the effective diameter
        IF ( od_eff_v1(2) .GT. 0. ) THEN
          od_mpcorr_ad(2,:) = od_mpcorr_ad(2,:) + ed(2,:) * ed_eff_ad(2) /od_eff_v1(2)
          ed_ad(2,:) = ed_ad(2,:) + od_mpcorr(2,:) * ed_eff_ad(2) /od_eff_v1(2)
          od_eff_ad(2) = od_eff_ad(2) - ed_eff_v1(2) * ed_eff_ad(2) /od_eff_v1(2)
          ed_eff_ad(2) = 0._jprb
        ELSE
          ed_eff_ad(2) = 0._jprb
        ENDIF
        od_mpcorr_ad(2,:) = od_mpcorr_ad(2,:) + od_eff_ad(2)
        od_eff_ad(2) = 0._jprb

        ! water: use original optical depths for computing the effective diameter, then overwrite od_eff(1)
        od_mpcorr_ad(1,:) = od_mpcorr_ad(1,:) + od_eff_ad(1)
        od_eff_ad(1) = 0._jprb
        IF ( od_eff_v1(1) .GT. 0. ) THEN
          od_ad(1,:) = od_ad(1,:) + ed(1,:) * ed_eff_ad(1) /od_eff_v1(1)
          ed_ad(1,:) = ed_ad(1,:) + od(1,:) * ed_eff_ad(1) /od_eff_v1(1)
          od_eff_ad(1) = od_eff_ad(1) - ed_eff_v1(1) * ed_eff_ad(1) /od_eff_v1(1)
          ed_eff_ad(1) = 0._jprb
        ELSE
          ed_eff_ad(1) = 0._jprb
        ENDIF
        od_ad(1,:) = od_ad(1,:) + od_eff_ad(1)
        od_eff_ad(1) = 0._jprb
        
        DO j = nlay, 1, -1
          IF (j > aux%s(prof)%nearestlev_surf - 1) CYCLE    ! Layer is entirely below surface pressure so nothing more to do
!         IF(n_wvdim==3) THEN
!             wvint_bot_tl = wvint_bot_tl + fac_mx(j)          * dvap_tl(j) + fac_mx_tl(j) * dvap(j)
!             wvint_top_tl = wvint_top_tl + (1._jprb-fac_mx(j))* dvap_tl(j) - fac_mx_tl(j) * dvap(j)
!         ENDIF
          IF(n_wvdim==3) THEN
            dvap_ad(j)  = dvap_ad  (j) + fac_mx(j) * wvint_bot_ad 
            fac_mx_ad(j)= fac_mx_ad(j) + dvap(j)   * wvint_bot_ad

            dvap_ad(j)  = dvap_ad(j)   + (1._jprb-fac_mx(j))* wvint_top_ad
            fac_mx_ad(j)= fac_mx_ad(j) - dvap(j)            * wvint_top_ad
          ENDIF

          IF ( ircld%icldarr(cc,j,prof) .NE. 1 ) THEN
            IF(j>1) THEN
              fac_mx_ad(j-1) = fac_mx_ad(j)
            ELSE
              fac_mx_ad(j)=0
            ENDIF
          ELSE !(ircld%icldarr(cc,j,prof) .EQ. 1 )
            !-----------------------------------------------------------------------
            ! determine effective diameter
            !-----------------------------------------------------------------------
            aux_ad%ice_dg(j,prof) = aux_ad%ice_dg(j,prof) + ed_ad(2,j)
            ed_ad(2,j) = 0._jprb
            IF ( coefs%coef_mfasis_cld%clw_scheme == clw_scheme_deff ) THEN ! Deff water clouds scheme
              aux_ad%clw_dg(j,prof) = aux_ad%clw_dg(j,prof) + ed_ad(1,j)
              ed_ad(1,j) = 0._jprb
            ELSE ! OPAC water cloud parametrization
              ! TODO: take into account optical depth surface correction in eff diameter calculation
              IF ( od(1,j) .GT. 0._jprb ) THEN
                trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i)=trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) &
                                                          -ed(1,j)/SUM(trans_scatt_ir%opdpext(1:nwcl_max,j,i)) &
                                                           * ed_ad(1,j)
                ed_ad(1,j) =  ed_ad(1,j)/SUM(trans_scatt_ir%opdpext(1:nwcl_max,j,i))
              ELSE
                ed_ad(1,j) = 0._jprb
              ENDIF
              trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) = trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) +  &
                                                          wcl_opac_deff(1:nwcl_max) * ed_ad(1,j)
              ed_ad(1,j) = 0
            ENDIF

            !-----------------------------------------------------------------------
            ! mixed-phase correction per layer: add ice optical depth below water
            ! cloud top to water optical depth and subtract from ice optical depth
            !-----------------------------------------------------------------------
            IF (od_mpcorr_v1(2,j) < 0) od_mpcorr_ad(2,j) = 0._jprb
            
            od_ad(2,j) = od_ad(2,j) + (1-fac_mx(j))*od_mpcorr_ad(2,j)
            fac_mx_ad(j) = fac_mx_ad(j) - od(2,j)*od_mpcorr_ad(2,j)
            od_mpcorr_ad(2,j) = 0

            od_ad(1,j) = od_ad(1,j) + od_mpcorr_ad(1,j)
            fac_mx_ad(j) = fac_mx_ad(j) + od(2,j)*od_mpcorr_ad(1,j)
            od_ad(2,j) = od_ad(2,j) + fac_mx(j)*od_mpcorr_ad(1,j)
            od_mpcorr_ad(1,j) = 0

            IF(abs( ooo1(j)/opts%dev%o_del1 ) < 5._jprb) THEN
              ooo1_ad = ooo1_ad + fac_mx_ad(j)/opts%dev%o_del1 / (COSH(ooo1(j)/opts%dev%o_del1)**2) /2._jprb
              fac_mx_ad(j)= 0
            ENDIF

            fac_mx_ad(j)= 0._jprb

            od1_int_ad = od1_int_ad + ooo1_ad
            ooo1_ad    = 0

            od_ad(1,j) = od_ad(1,j) + od1_int_ad
            od1_int_ad = 0._jprb

            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
              aux_ad%s(prof)%pfraction_surf = aux_ad%s(prof)%pfraction_surf &
                               - SUM(od_v1(:,j) * od_ad(:,j))
              od_ad(:,j) = od_ad(:,j) * (1 - aux%s(prof)%pfraction_surf)
            ENDIF
            trans_scatt_ir_ad%opdpext(nwcl_max+1,j,i) = trans_scatt_ir_ad%opdpext(nwcl_max+1,j,i) + od_ad(2,j)
            od_ad(2,j) = 0
            trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) = trans_scatt_ir_ad%opdpext(1:nwcl_max,j,i) + od_ad(1,j)
            od_ad(1,j) = 0
          ENDIF
        ENDDO

        od1_int_ad  = 0._jprb
        od_mpcorr_ad(:,:)= 0._jprb
        odint_ad = 0._jprb
        fac_mx_ad(:)= 0._jprb
        w_sscat_ad(:,:) = 0._jprb
        ed_eff_sscat_ad(:) = 0._jprb
        fac_rm_ad(:) = 0._jprb
        wvint_bot_ad= 0._jprb
        wvint_top_ad= 0._jprb
        od_ad(:,:) = 0._jprb
        ed_ad(:,:) = 0._jprb
        ircld_ad%xcol(cc+1,prof) = ircld_ad%xcol(cc+1,prof) + colwei_ad
        ircld_ad%xcol(cc,prof)   = ircld_ad%xcol(cc,prof)   - colwei_ad
        colwei_ad = 0
        od_eff_ad(:) = 0._jprb
        ed_eff_ad(:) = 0._jprb
        odlay_ad(:) = 0._jprb

      ELSE  ! Aerosols (not yet tested!!!!)
      ! TODO: Test aerosol functionality

        colwei_cc = 1._jprb     ! single column for aerosols
        od   (:,:)= 0._jprb
        DO k = 1, npar
          par = mfasis_coefs%aer_types(k)
          od_ad(k,:) = od_ad(k,:) + od_eff_ad(k)
          od_eff_ad(k) = 0
          DO j = 1, nlay
            IF (j > aux%s(prof)%nearestlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
            od(k,j)    = trans_scatt_ir%opdpext(par,j,i)
            IF (j == aux%s(prof)%nearestlev_surf - 1) THEN
              ! Modify optical depth in partial layer above surface
              aux_ad%s(prof)%pfraction_surf = aux_ad%s(prof)%pfraction_surf - od_v1(k,j) * od_ad(k,j)
              od_ad(k,j) = (1 - aux%s(prof)%pfraction_surf) * od_ad(k,j)
            ENDIF
            trans_scatt_ir_ad%opdpext(par,j,i) = trans_scatt_ir_ad%opdpext(par,j,i) + od_ad(k,j)
            od_ad(k,j) = 0
          ENDDO
        ENDDO
        colwei_ad = 0._jprb     ! single column for aerosols A1
        od_ad(:,:)= 0._jprb     !                            A2
      ENDIF !ENDIF CLOUD ELSE AEROSOL

    ENDDO ! END of cloud column loop

!====================================================================================
    refl_ad     = 0._jprb
    refl_clr_ad = 0._jprb

    iw_ad(:)  = 0._jprb  ! interpolation weight
    ip_ad(:) =  0._jprb         ! interpolation point
    ! --------------------------------------------------------------------------
    ! Albedo
    ! --------------------------------------------------------------------------
    albedo   =     reflectance(i)%refl_out * pi
    IF (albedo < 1._jprb) THEN
      reflectance_ad(i)%refl_out = reflectance_ad(i)%refl_out + albedo_ad * pi
      albedo_ad= 0
    ELSE
      albedo_ad= 0._jprb
    ENDIF

!   dvap_tl(:)=0._jprb
!   IF(n_wvdim==3) THEN
!     q_mxr(:) =  profiles_int(prof)%q(:)* gas_mass(gas_id_watervapour)/  &
!                 (mair * 1.E06_jprb +profiles_int(prof)%q(:)*gas_mass(gas_id_watervapour))

!     q_mxr_tl(:)= profiles_int_tl(prof)%q(:)*gas_mass(gas_id_watervapour) * (1._jprb - q_mxr(:) ) &
!                 /(mair * 1.E06_jprb +profiles_int(prof)%q(:)*gas_mass(gas_id_watervapour))

!
!     dvap(1:nlay)  = 100._jprb * (profiles(prof)%p(2:nlev)-profiles(prof)%p(1:nlev-1))* &
!                                          0.5_jprb * (q_mxr(2:nlev)+q_mxr(1:nlev-1))/gravity
!     dvap_tl(1:nlay)=100._jprb *((profiles_tl(prof)%p(2:nlev)-profiles_tl(prof)%p(1:nlev-1))* &
!                                          0.5_jprb * (q_mxr(2:nlev)+q_mxr(1:nlev-1))/gravity  &
!                                 + (profiles(prof)%p(2:nlev)-profiles(prof)%p(1:nlev-1))* &
!                                    0.5_jprb * (q_mxr_tl(2:nlev)+q_mxr_tl(1:nlev-1))/gravity  &
!                                )  
!
!     jj = aux%s(prof)%nearestlev_surf - 1
!     IF(jj>=1 .AND. jj<=nlay) THEN
!       dvap_tl(jj)=dvap_tl(jj)*(1._jprb - aux   %s(prof)%pfraction_surf)           &
!                       - dvap   (jj) * aux_tl%s(prof)%pfraction_surf
!       dvap   (jj)=dvap   (jj)*(1._jprb - aux   %s(prof)%pfraction_surf)
!     ENDIF

    q_mxr_ad(:) = 0
    IF(n_wvdim==3) THEN

      jj = aux%s(prof)%nearestlev_surf - 1
      IF(jj>=1 .AND. jj<=nlay) THEN
        aux_ad%s(prof)%pfraction_surf =               aux_ad%s(prof)%pfraction_surf  &
                                         - dvap_1(jj) * dvap_ad(jj) 

        dvap_ad(jj)=dvap_ad(jj)*(1._jprb - aux   %s(prof)%pfraction_surf)  
      ENDIF

!------------------------------------------------------------------------------------------------------
!       IF (opts%interpolation%lgradp) THEN
!         dvap_tl(1:nlay)=100._jprb *((profiles_tl(prof)%p(2:nlev)-profiles_tl(prof)%p(1:nlev-1))* &
!                                              0.5_jprb * (q_mxr(2:nlev)+q_mxr(1:nlev-1))/gravity  &
!                                     + (profiles(prof)%p(2:nlev)-profiles(prof)%p(1:nlev-1))* &
!                                        0.5_jprb * (q_mxr_tl(2:nlev)+q_mxr_tl(1:nlev-1))/gravity  &
!                                    )
!      ELSE
!        dvap_tl(1:nlay)=100._jprb *((profiles(prof)%p(2:nlev)-profiles(prof)%p(1:nlev-1))* &
!                                        0.5_jprb * (q_mxr_tl(2:nlev)+q_mxr_tl(1:nlev-1))/gravity  &
!                                    )
!      ENDIF

      array1(0)      = 0
      array1(nlay+1) = 0

      IF (opts%interpolation%lgradp) THEN
        array1(1:nlay) = 100._jprb * 0.5_jprb * (q_mxr(2:nlev)+q_mxr(1:nlev-1))/gravity * dvap_ad(1:nlay)
        profiles_ad(prof)%p(1:nlev)  = profiles_ad(prof)%p(1:nlev) + array1(0:nlay) - array1(1:nlay+1)
      ENDIF

      array1(1:nlay) = 100._jprb * 0.5_jprb * (profiles(prof)%p(2:nlev)-profiles(prof)%p(1:nlev-1))/gravity &
                          * dvap_ad(1:nlay)
      q_mxr_ad(1:nlev) = q_mxr_ad(1:nlev) +  array1(0:nlay) + array1(1:nlay+1)
!------------------------------------------------------------------------------------------------------

      profiles_int_ad(prof)%q(:) = profiles_int_ad(prof)%q(:) +  &
                        q_mxr_ad(:)* gas_mass(gas_id_watervapour) * (1._jprb - q_mxr(:) ) & 
                    /(mair * 1.E06_jprb +profiles_int(prof)%q(:)*gas_mass(gas_id_watervapour))
      q_mxr_ad(:) = 0
    ENDIF
    dvap_ad(:)=0._jprb

  ENDDO ! end of channel loop


  ! --------------------------------------------------------------------------
  ! Tidy up
  ! --------------------------------------------------------------------------

  DEALLOCATE( ip, ip_ad, iw_ad, di, od, od_ad, od_eff, od_eff_ad, odlay, odlay_ad, od_v1, od_eff_v1, STAT=err)
  THROWM(err.NE.0,"Deallocation of memory for rttov_mfasis_ad failed")
  IF (mfasis_coefs%file_type .EQ. mfasis_cld) THEN
    DEALLOCATE(ed, ed_ad, ed_eff, ed_aux, ed_eff_ad, fac_mx, fac_mx_ad, &
               q_mxr, q_mxr_ad, dvap, dvap_1, dvap_ad, array1, STAT=err)
    THROWM(err.NE.0,"Deallocation of memory for rttov_mfasis_ad failed")
    DEALLOCATE(w_sscat, w_sscat_ad, fac_rm, fac_rm_ad, ed_eff_sscat, ed_eff_sscat_ad, STAT=err)
    THROWM(err.NE.0,"Deallocation of memory for rttov_mfasis_tl failed")
  ENDIF

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_AD', 1_jpim, ZHOOK_HANDLE)

  CATCH

  IF (LHOOK) CALL DR_HOOK('RTTOV_MFASIS_AD', 1_jpim, ZHOOK_HANDLE)

END SUBROUTINE rttov_mfasis_ad
