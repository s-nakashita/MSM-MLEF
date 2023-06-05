! Description:
!> @file
!!   Subroutines for AD of SURFEM-Ocean MW sea surface emissivity model
!
!> @brief
!!   Subroutines for AD of SURFEM-Ocean MW sea surface emissivity model
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
MODULE rttov_surfem_ocean_ad_mod

  USE parkind1, ONLY : jpim, jprb
  USE rttov_const, ONLY : pi
  USE rttov_surfem_ocean_mod, ONLY : epsilon, e_neutral, net_surfem

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: rttov_surfem_ocean_ad

CONTAINS

  !> Returns the adjoint surfem ocean emissivity model
  !! @param[in]     f_GHz         frequency (GHz)
  !! @param[in]     theta         zenith angle (degrees)
  !! @param[in]     windspeed     wind speed (m/s)
  !! @param[in]     SST           skin temperature (K)
  !! @param[in]     SSS           salinity (practical salinity unit)
  !! @param[in]     phi           relative wind direction (degrees)
  !! @param[in]     ev_ad         V-pol emissivity adjoint
  !! @param[in]     eh_ad         H-pol emissivity adjoint
  !! @param[in]     e3_ad         3rd Stokes vector emissivity adjoint
  !! @param[in]     e4_ad         4th Stokes vector emissivity adjoint
  !! @param[in,out] windspeed_ad  wind speed adjoint
  !! @param[in,out] SST_ad        skin temperature adjoint
  !! @param[in,out] SSS_ad        salinity adjoint
  !! @param[in,out] phi_ad        relative wind direction adjoint
  SUBROUTINE rttov_surfem_ocean_ad(f_GHz, theta, windspeed, SST, SSS, phi, &
       ev_ad, eh_ad, e3_ad, e4_ad, &
       windspeed_ad, SST_ad, SSS_ad, phi_ad )

    USE rttov_surfem_ocean_coef_mod, ONLY : &
        surfem_nn_min_sst,       &
        surfem_nn_max_sst,       &
        surfem_nn_min_sss,       &
        surfem_nn_max_sss,       &
        surfem_nn_min_windspeed, &
        surfem_nn_max_windspeed, &
        surfem_nn_min_freq,      &
        surfem_nn_max_freq      

    REAL(jprb), INTENT(IN)    :: f_GHz, theta, windspeed, SST, SSS, phi
    REAL(jprb), INTENT(IN)    :: ev_ad, eh_ad, e3_ad, e4_ad
    REAL(jprb), INTENT(INOUT) :: windspeed_ad, SST_ad, SSS_ad, phi_ad

    ! Local copies of input variables clipped to neural network training limits
    REAL(jprb) :: SSTl, SSSl, windspeedl, f_GHzl
    REAL(jprb) :: SSTl_ad, SSSl_ad, windspeedl_ad

    COMPLEX(jprb) :: dielec, dielec_ad
    REAL(jprb)    :: e_Nv, e_Nh
    REAL(jprb)    :: e_Nv_ad, e_Nh_ad
    REAL(jprb)    :: e_v0_ad, e_h0_ad
    REAL(jprb)    :: e_iso_ad(2)
    REAL(jprb)    :: e_aniso(8),  e_aniso_ad(8)

    ! call direct code to get values of dielec, e_Nv, e_Nh and e_aniso
    CALL epsilon(f_GHz, SST, SSS, dielec)
    CALL e_neutral(dielec, theta, e_Nv, e_Nh)

    ! Ensure NN training limits are not exceeded
    f_GHzl = MAX(MIN(f_GHz, surfem_nn_max_freq), surfem_nn_min_freq)
    SSTl = MAX(MIN(SST, surfem_nn_max_sst), surfem_nn_min_sst)
    SSSl = MAX(MIN(SSS, surfem_nn_max_sss), surfem_nn_min_sss)
    windspeedl = MAX(MIN(windspeed, surfem_nn_max_windspeed), surfem_nn_min_windspeed)

    CALL net_surfem(f_GHzl, theta, windspeedl, SSTl, SSSl, e_Nv, e_Nh, e_aniso)

    dielec_ad = 0._jprb
    e_aniso_ad = 0._jprb
    e_Nv_ad = 0._jprb
    e_Nh_ad = 0._jprb
    e_v0_ad = 0._jprb
    e_h0_ad = 0._jprb

    ! output emissitivities 
    CALL e_total_ad(phi, e_aniso, ev_ad, eh_ad, e3_ad, e4_ad, &
                       phi_ad, e_aniso_ad, e_Nv_ad, e_Nh_ad, e_v0_ad, e_h0_ad)

    e_iso_ad(1) = e_v0_ad
    e_iso_ad(2) = e_h0_ad

    SSTl_ad = 0._jprb
    SSSl_ad = 0._jprb
    windspeedl_ad = 0._jprb

    ! anisotropic emissivity via neural network
    CALL net_surfem_ad(f_GHzl, theta, windspeedl, SSTl, SSSl, e_Nv, e_Nh, e_aniso_ad, &
          windspeedl_ad, SSTl_ad, SSSl_ad, e_Nv_ad, e_Nh_ad)

    ! isotropic emissivity via neural network
    CALL net_surfem_ad(f_GHzl, theta, windspeedl, SSTl, SSSl, e_Nv, e_Nh, e_iso_ad, &
          windspeedl_ad, SSTl_ad, SSSl_ad, e_Nv_ad, e_Nh_ad)

    IF (SST < surfem_nn_min_sst .OR. SST > surfem_nn_max_sst) THEN
      SSTl_ad = 0
    ELSE
      SST_ad = SST_ad + SSTl_ad
    ENDIF
    IF (SSS < surfem_nn_min_sss .OR. SSS > surfem_nn_max_sss) THEN
      SSSl_ad = 0
    ELSE
      SSS_ad = SSS_ad + SSSl_ad
    ENDIF
    IF (windspeed < surfem_nn_min_windspeed .OR. &
        windspeed > surfem_nn_max_windspeed) THEN
      windspeedl_ad = 0
    ELSE
      windspeed_ad = windspeed_ad + windspeedl_ad
    ENDIF

    ! flat sea emissivity (Neutral) from dielectric constant of sea water
    CALL e_neutral_ad(theta, dielec, e_Nv_ad, e_Nh_ad, dielec_ad)

    ! dielectric constant of sea water
    CALL epsilon_ad(f_GHz, SST, SSS, dielec_ad, SST_ad, SSS_ad)

  END SUBROUTINE rttov_surfem_ocean_ad

  ! dielectric constant
  SUBROUTINE epsilon_ad(f_GHz, SSTk, SSSi, dielec_ad, SSTk_ad, SSSi_ad)

    USE rttov_surfem_ocean_coef_mod, ONLY : &
        f0, es_coef, ai_coef,       &
        sigma35_coef, R15_coef,     &
        alpha0_coef, alpha1_coef,   &
        bi_new1_coef, bi_new2_coef, &
        c1_coef, bi_coef,           &
        surfem_dielec_min_sst,      &
        surfem_dielec_max_sst,      &
        surfem_dielec_min_sss,      &
        surfem_dielec_max_sss

    USE rttov_const, ONLY : t0

    REAL(jprb),     INTENT(IN)    :: f_GHz      !frequency in GHz
    REAL(jprb),     INTENT(IN)    :: SSTk       !Sea surface temperature in Kelvin
    REAL(jprb),     INTENT(IN)    :: SSSi       !Sea surface salinity in psu
    COMPLEX(jprb),  INTENT(IN)    :: dielec_ad  !adjoint of dielectric
    REAL(jprb),     INTENT(INOUT) :: SSTk_ad    !Sea surface temperature perturbation
    REAL(jprb),     INTENT(INOUT) :: SSSi_ad    !Sea surface salinity in perturbation

    REAL(jprb) :: SST, SSS, SST2, SST3, SST4, SSS2
    REAL(jprb) :: SST_ad, SSS_ad
    REAL(jprb) :: es, e1, einf, nu1, nu2 ! Fresh water parameters
    REAL(jprb) :: sigma35, R15, alpha0, alpha1, RTR15  ! Salt water
    REAL(jprb) :: es_s, e1_s, einf_s, nu1_s, nu2_s ! Salt water parameters
    REAL(jprb) :: c1, c2, c3, c4 ! conversion coef from fresh to salt water

    REAL(jprb) :: es_ad, e1_ad, einf_ad, nu1_ad, nu2_ad ! Fresh water perturbations
    REAL(jprb) :: sigma35_ad, R15_ad, alpha0_ad, alpha1_ad, RTR15_ad, sigma_ad  ! Salt water perts
    REAL(jprb) :: es_s_ad, e1_s_ad, einf_s_ad, nu1_s_ad, nu2_s_ad ! Salt water param perts
    REAL(jprb) :: c1_ad, c2_ad, c3_ad, c4_ad ! conversion coef from fresh to salt water perts

    REAL(jprb) :: es_s_exp, c1_tmp, c2_tmp
    COMPLEX(jprb), PARAMETER :: i = (0, 1)   ! sqrt(-1) 

    ! direct code (from beginning)

    ! clip input variables if they are outside of reasonable limits
    SST = MAX(MIN(SSTk, surfem_dielec_max_sst), surfem_dielec_min_sst) - t0 ! K -> deg C
    SSS = MAX(MIN(SSSi, surfem_dielec_max_sss), surfem_dielec_min_sss)

    !Combined local variables 
    SST2        = SST * SST
    SST3        = SST2 * SST
    SST4        = SST3 * SST
    SSS2        = SSS * SSS

    !Fresh water parameters
    es          = (es_coef(1) - es_coef(2) * SST) / (es_coef(3) + SST)
    e1          = ai_coef(1) + (ai_coef(2) * SST) + (ai_coef(3) * SST2)
    nu1         = (45.0_jprb + SST) / ( ai_coef(4) + (ai_coef(5) * SST) + (ai_coef(6) * SST2) )
    einf        = ai_coef(7) + (ai_coef(8) * SST)
    nu2         = (45.0_jprb + SST) / ( ai_coef(9) + (ai_coef(10) * SST) + (ai_coef(11) * SST2) )

    !Salt water 
    sigma35     = sigma35_coef(1) + (sigma35_coef(2) * SST) + (sigma35_coef(3) * SST2) - &
         (sigma35_coef(4) * SST3) + (sigma35_coef(5)*SST4)
    R15         = SSS * ( R15_coef(1) + (R15_coef(2) * SSS) + (R15_coef(3) * SSS2) ) / & 
         ( R15_coef(4) + (R15_coef(5) * SSS) + SSS2 )
    alpha0      = ( alpha0_coef(1) + (alpha0_coef(2) * SSS) - (alpha0_coef(3) * SSS2) ) / &
         ( alpha0_coef(4) + (alpha0_coef(5) * SSS) + SSS2 )
    alpha1      = alpha1_coef(1) - (alpha1_coef(2) * SSS) + alpha1_coef(3) * SSS2
    RTR15       = 1.0_jprb + ( SST - 15.0_jprb ) * alpha0 / ( alpha1 + SST )

    ! Salt water parameters 
    es_s_exp    = (bi_new1_coef(1) * SSS) + (bi_new1_coef(2) * SSS2) + (bi_new1_coef(3) * SSS * SST)
    es_s        = es * EXP(es_s_exp)

    ! conversion coefs from fresh to salt water
    IF ( SST <= 30.0_jprb ) THEN
       c1_tmp   = bi_new2_coef(1) + (bi_new2_coef(2) * SST) + (bi_new2_coef(3) * SST2) + &
            (bi_new2_coef(4) * SST3) + (bi_new2_coef(5) * SST4)
       c1       = 1.0_jprb + (SSS * c1_tmp)
    ELSE
       c1       = 1.0_jprb + SSS * ( c1_coef(1) + c1_coef(2) * (SST - 30.0_jprb) )
    ENDIF
    nu1_s       = nu1*c1
    c2_tmp      = (bi_coef(7)*SSS) + (bi_coef(8)*SSS2) + (bi_coef(9)*SSS*SST)
    c2          = EXP(c2_tmp) 
    e1_s        = e1*c2
    c3          = 1.0_jprb + SSS * ( bi_coef(10) + (0.5_jprb * bi_coef(11)) * (SST + 30.0_jprb) )
    nu2_s       = (nu2*c3)
    c4          = 1.0_jprb  + SSS * (bi_coef(12) + (bi_coef(13) * SST))
    einf_s      = (einf*c4)  

    !=== adjoint======

    es_s_ad     = 0._jprb  
    e1_s_ad     = 0._jprb  
    nu1_s_ad    = 0._jprb  
    nu2_s_ad    = 0._jprb  
    einf_s_ad   = 0._jprb  
    einf_ad     = 0._jprb  
    c4_ad       = 0._jprb  
    c3_ad       = 0._jprb  
    c2_ad       = 0._jprb  
    c1_ad       = 0._jprb
    nu1_ad      = 0._jprb
    nu2_ad      = 0._jprb
    sigma_ad    = 0._jprb
    sigma35_ad  = 0._jprb
    R15_ad      = 0._jprb
    RTR15_ad    = 0._jprb
    alpha0_ad   = 0._jprb
    alpha1_ad   = 0._jprb
    e1_ad       = 0._jprb
    es_ad       = 0._jprb
    SST_ad      = 0._jprb
    SSS_ad      = 0._jprb

    es_s_ad = es_s_ad + REAL(dielec_ad) * (1 + (f_GHz/nu1_s)**2) / (1.0_jprb + (f_GHz/nu1_s)**2)**2 + &
                        AIMAG(dielec_ad) * (f_Ghz/nu1_s) * (1 + (f_GHz/nu1_s)**2) / (1.0_jprb + (f_GHz/nu1_s)**2)**2

    e1_s_ad = e1_s_ad - REAL(dielec_ad) * (1 + (f_GHz/nu1_s)**2) / (1.0_jprb + (f_GHz/nu1_s)**2)**2 - &
                        AIMAG(dielec_ad) * (f_Ghz/nu1_s) * (1 + (f_GHz/nu1_s)**2) / (1.0_jprb + (f_GHz/nu1_s)**2)**2

    nu1_s_ad = nu1_s_ad + REAL(dielec_ad) * 2 * (es_s - e1_s) * (f_Ghz/nu1_s) * (f_GHz/nu1_s**2) / &
                              (1.0_jprb + (f_GHz/nu1_s)**2)**2 - &
                          AIMAG(dielec_ad) * (es_s - e1_s) * (1 - (f_Ghz/nu1_s)**2) * (f_GHz/nu1_s**2) / &
                              (1.0_jprb + (f_GHz/nu1_s)**2)**2


    e1_s_ad = e1_s_ad + REAL(dielec_ad) * (1 + (f_GHz/nu2_s)**2) / (1.0_jprb + (f_GHz/nu2_s)**2)**2 + &
                        AIMAG(dielec_ad) * (f_Ghz/nu2_s) * (1 + (f_GHz/nu2_s)**2) / (1.0_jprb + (f_GHz/nu2_s)**2)**2

    einf_s_ad = einf_s_ad - REAL(dielec_ad) * (1 + (f_GHz/nu2_s)**2) / (1.0_jprb + (f_GHz/nu2_s)**2)**2 - &
                            AIMAG(dielec_ad) * (f_Ghz/nu2_s) * (1 + (f_GHz/nu2_s)**2) / (1.0_jprb + (f_GHz/nu2_s)**2)**2

    nu2_s_ad = nu2_s_ad + REAL(dielec_ad) * 2 * (e1_s - einf_s) * (f_Ghz/nu2_s) * (f_GHz/nu2_s**2) / &
                              (1.0_jprb + (f_GHz/nu2_s)**2)**2 - &
                          AIMAG(dielec_ad) * (e1_s - einf_s) * (1 - (f_Ghz/nu2_s)**2) * (f_GHz/nu2_s**2) / &
                              (1.0_jprb + (f_GHz/nu2_s)**2)**2

    einf_s_ad   = einf_s_ad + REAL(dielec_ad)
    sigma_ad    = sigma_ad + AIMAG(dielec_ad) * f0 / f_GHz

    einf_ad     = einf_ad + (einf_s_ad*c4) 
    c4_ad       = c4_ad + (einf*einf_s_ad)

    SSS_ad      = SSS_ad + (c4_ad * (bi_coef(12) + (bi_coef(13) * SST)))
    SST_ad      = SST_ad + (SSS * bi_coef(13) * c4_ad)

    nu2_ad      = nu2_ad + (nu2_s_ad*c3) 
    c3_ad       = c3_ad + (nu2*nu2_s_ad)

    SSS_ad      = SSS_ad + (c3_ad * (bi_coef(10) + (0.5_jprb * bi_coef(11)) * (SST + 30.0_jprb)))
    SST_ad      = SST_ad + (SSS * 0.5_jprb * bi_coef(11) * c3_ad)

    e1_ad       = e1_ad + (e1_s_ad*c2) 
    c2_ad       = c2_ad + (e1*e1_s_ad)

    SSS_ad      = SSS_ad + (bi_coef(7)*c2_ad + bi_coef(8)*2.0_jprb*SSS*c2_ad + bi_coef(9)*c2_ad*SST) * c2
    SST_ad      = SST_ad + bi_coef(9) * SSS * c2_ad * c2

    nu1_ad      = nu1_ad + (nu1_s_ad*c1) 
    c1_ad       = c1_ad + (nu1*nu1_s_ad)

    ! conversion coefs from fresh to salt water
    IF ( SST <= 30.0_jprb ) THEN
      SSS_ad    = SSS_ad + (c1_ad * c1_tmp) 
      SST_ad    = SST_ad + SSS * (bi_new2_coef(2) * c1_ad + bi_new2_coef(3) * 2.0_jprb * SST * c1_ad + &
          bi_new2_coef(4) * 3.0_jprb * SST2 * c1_ad + bi_new2_coef(5) * 4.0_jprb * SST3 * c1_ad)
    ELSE
      SSS_ad     = SSS_ad + (c1_ad * (c1_coef(1) + c1_coef(2) * (SST - 30.0_jprb)))
      SST_ad     = SST_ad + (SSS * c1_coef(2) * c1_ad)
    ENDIF

    ! Salt water parameters 
    es_ad       = es_ad + (es_s_ad * EXP(es_s_exp))
    SSS_ad      = SSS_ad + (((bi_new1_coef(1) * es_s_ad) + (bi_new1_coef(2) * 2.0_jprb * SSS * es_s_ad ) + &
                  (bi_new1_coef(3) * es_s_ad * SST))  * es_s)
    SST_ad      = SST_ad + (bi_new1_coef(3) * SSS * es_s_ad * es_s)

    sigma35_ad  = sigma35_ad + (sigma_ad * R15 * RTR15) 
    R15_ad      = R15_ad + (sigma35 * sigma_ad * RTR15) 
    RTR15_ad    = RTR15_ad + (sigma35 * R15 * sigma_ad)

    SST_ad      = SST_ad + ((((RTR15_ad * alpha0) * (alpha1 + SST)) - (RTR15_ad * ( SST - 15.0_jprb ) * alpha0)) / ((alpha1 + SST)**2))
    alpha0_ad   = alpha0_ad + (((RTR15_ad * (SST - 15.0_jprb)) * (alpha1 + SST)) / ((alpha1 + SST)**2))
    alpha1_ad   = alpha1_ad - ((RTR15_ad * (( SST - 15.0_jprb ) * alpha0)) / ((alpha1 + SST)**2))

    SSS_ad      =  SSS_ad - alpha1_coef(2) * alpha1_ad + alpha1_coef(3) * 2.0_jprb * SSS * alpha1_ad

    SSS_ad      = SSS_ad + alpha0_ad * ( (alpha0_coef(2) - 2.0_jprb * alpha0_coef(3) * SSS) * &
                   (alpha0_coef(4) + alpha0_coef(5) * SSS + SSS2) - (alpha0_coef(5) + 2.0_jprb * SSS) * &
                   (alpha0_coef(1) + alpha0_coef(2) * SSS - alpha0_coef(3) * SSS2) )  / &
                   (alpha0_coef(4) + alpha0_coef(5) * SSS + SSS2)**2

    SSS_ad      = SSS_ad + ((R15_ad * R15_coef(1) + 2.0_jprb * SSS * R15_ad * R15_coef(2) + 3.0_jprb * SSS2 * R15_ad * R15_coef(3)) * &
                   (R15_coef(4) + R15_coef(5) * SSS + SSS2) - &
                   (R15_coef(5) * R15_ad + 2.0_jprb * SSS * R15_ad) * SSS * (R15_coef(1) + R15_coef(2) * SSS + R15_coef(3) * SSS2)) / &
                   (R15_coef(4) + (R15_coef(5) * SSS) + SSS2)**2

    !! == Salt water ==
    SST_ad      = SST_ad + ((sigma35_coef(2) * sigma35_ad) + (sigma35_coef(3) * 2.0_jprb * SST * sigma35_ad) - &
                  (sigma35_coef(4) * 3.0_jprb * SST2 * sigma35_ad) + (sigma35_coef(5) * 4.0_jprb * SST3 * sigma35_ad))

    SST_ad      = SST_ad + (nu2_ad * (ai_coef(9) + ai_coef(10) * SST + ai_coef(11) * SST2) - &
                  (ai_coef(10) * nu2_ad + ai_coef(11) * 2.0_jprb * SST * nu2_ad) * (45.0_jprb + SST)) / &
                  (ai_coef(9) + ai_coef(10) * SST + ai_coef(11) * SST2)**2

    SST_ad      = SST_ad + (ai_coef(8) * einf_ad)

    SST_ad      = SST_ad + (nu1_ad * (ai_coef(4) + ai_coef(5) * SST + ai_coef(6) * SST2) - &
                  (ai_coef(5) * nu1_ad + ai_coef(6) *  2.0_jprb * SST * nu1_ad) * (45.0_jprb + SST)) / &
                  (ai_coef(4) + ai_coef(5) * SST + ai_coef(6) * SST2)**2

    SST_ad      = SST_ad + (ai_coef(2) * e1_ad) + (ai_coef(3) * 2.0_jprb * SST * e1_ad)

    !Fresh water parameters
    SST_ad      = SST_ad - es_ad * (es_coef(2) + es) / (es_coef(3) + SST)

    ! clip input variables if they are outside of reasonable limits
    IF (SSTk < surfem_dielec_min_sst .OR. SSTk > surfem_dielec_max_sst) THEN
      SST_ad = 0
    ELSE
      SSTk_ad = SSTk_ad + SST_ad
    ENDIF
    IF (SSSi < surfem_dielec_min_sss .OR. SSSi > surfem_dielec_max_sss) THEN
      SSS_ad = 0
    ELSE
      SSSi_ad = SSSi_ad + SSS_ad
    ENDIF

  END SUBROUTINE epsilon_ad

  ! Estimation of e neutral (flat sea no wind) from dielectric constant
  SUBROUTINE e_neutral_ad(theta, dielec, e_Nv_ad, e_Nh_ad, dielec_ad)

    REAL(jprb),     INTENT(IN)  :: theta     !incidence angle in degrees
    COMPLEX(jprb),  INTENT(IN)  :: dielec    !complex dielectric constant of sea water
    REAL(jprb),     INTENT(IN)  :: e_Nv_ad   !TL of vertical component of e Neutral
    REAL(jprb),     INTENT(IN)  :: e_Nh_ad   !TL of horizontal component of e Neutral
    COMPLEX(jprb),  INTENT(INOUT)  :: dielec_ad !adjcomplex dielectric constant of sea water

    COMPLEX(jprb) :: f_v ! vertical fresnel reflection coefficient unsquared
    COMPLEX(jprb) :: f_h ! horizontal fresnel reflection coefficient unsquared
    COMPLEX(jprb)    :: f_v_ad ! vertical fresnel reflection coefficient unsquared
    COMPLEX(jprb)    :: f_h_ad ! horizontal fresnel reflection coefficient unsquared
    REAL(jprb)    :: fres_v_ad ! vertical fresnel reflection coefficient
    REAL(jprb)    :: fres_h_ad ! horizontal fresnel reflection coefficient

    REAL(jprb)    :: cos_theta
    REAL(jprb)    :: sin_theta
    COMPLEX(jprb) :: sqrt_d, sqrt_d_ad

    ! direct code 
    cos_theta  = COS(theta*pi/180._jprb)
    sin_theta  = SIN(theta*pi/180._jprb)
    sqrt_d    = (SQRT(dielec - sin_theta * sin_theta))

    f_v = (dielec * cos_theta - sqrt_d) / &
          (dielec * cos_theta + sqrt_d) 
    f_h = (cos_theta - sqrt_d) / &
          (cos_theta + sqrt_d) 

    !! ==== adjoint =========

    fres_v_ad = 0._jprb
    fres_h_ad = 0._jprb
    f_v_ad    = 0._jprb
    f_h_ad    = 0._jprb
    sqrt_d_ad = 0._jprb

    fres_h_ad = fres_h_ad - e_Nh_ad
    fres_v_ad = fres_v_ad - e_Nv_ad

    f_h_ad    = f_h_ad + 2.0_jprb * fres_h_ad * f_h
    f_v_ad    = f_v_ad + 2.0_jprb * fres_v_ad * f_v

    sqrt_d_ad = sqrt_d_ad - 2._jprb * f_h_ad * cos_theta / CONJG((cos_theta + sqrt_d)**2)

    dielec_ad = dielec_ad + 2._jprb * f_v_ad * cos_theta * CONJG(sqrt_d) / &
                            CONJG((dielec * cos_theta + sqrt_d)**2)
    sqrt_d_ad = sqrt_d_ad - 2._jprb * f_v_ad * CONJG(dielec) * cos_theta / &
                            CONJG((dielec * cos_theta + sqrt_d)**2)

    dielec_ad = dielec_ad + 0.5_jprb * sqrt_d_ad / CONJG(sqrt_d)

    END SUBROUTINE e_neutral_ad

  !> Return AD of emissivities for given frequency, zenith angle, T-skin
  !! salinity and wind speed
  !! @param[in]     f_GHz         frequency (GHz)
  !! @param[in]     theta         zenith angle (degrees)
  !! @param[in]     windspeed     wind speed (m/s)
  !! @param[in]     SST           skin temperature (K)
  !! @param[in]     SSS           salinity (practical salinity unit)
  !! @param[in]     e_Nv          vertical component of e Neutral
  !! @param[in]     e_Nh          horizontal component of e Neutral
  !! @param[in]     e_out_ad      emissivity component increments
  !! @param[in,out] windspeed_ad  wind speed increment
  !! @param[in,out] SST_ad        skin temperature increment
  !! @param[in,out] SSS_ad        salinity increment
  !! @param[in,out] e_Nv_ad       neutral V-pol emissivity increment
  !! @param[in,out] e_Nh_ad       neutral H-pol emissivity increment
  SUBROUTINE net_surfem_ad(f_GHz, theta, windspeed, SST, SSS, e_Nv, e_Nh, e_out_ad, &
       windspeed_ad, SST_ad, SSS_ad, e_Nv_ad, e_Nh_ad)

    USE rttov_surfem_ocean_coef_mod, ONLY : &
        surfem_nh, surfem_ni, surfem_no_iso, iso_net, aniso_net

    REAL(jprb), INTENT(IN)    :: f_GHz, theta, windspeed, SST, SSS, e_Nv, e_Nh
    REAL(jprb), INTENT(IN)    :: e_out_ad(:)
    REAL(jprb), INTENT(INOUT) :: windspeed_ad, SST_ad, SSS_ad, e_Nv_ad, e_Nh_ad

    ! neural network variables
    REAL(jprb) :: x(surfem_ni)
    REAL(jprb) :: tran_1(surfem_ni)
    REAL(jprb) :: prop_1(surfem_nh)

    REAL(jprb), POINTER :: ti_ymin(:), ti_xoffset(:), ti_gain(:)
    REAL(jprb), POINTER :: b1(:), w1(:,:), w2(:,:)
    REAL(jprb), POINTER :: to_gain(:)

    REAL(jprb) :: x_ad(surfem_ni)
    REAL(jprb) :: tran_1_ad(surfem_ni)
    REAL(jprb) :: prop_1_ad(surfem_nh)
    REAL(jprb) :: prop_1_tmp_ad(surfem_nh)
    REAL(jprb) :: prop_2_ad(surfem_nh)

    INTEGER(jpim) :: i, j, surfem_no

    !==== direct - neural network  =======
    x(:) = (/ f_GHz, theta, windspeed, SST, SSS, e_Nv, e_Nh /)

    surfem_no = SIZE(e_out_ad)

    IF (surfem_no == surfem_no_iso) THEN
      ti_ymin    => iso_net%ti_ymin
      ti_xoffset => iso_net%ti_xoffset
      ti_gain    => iso_net%ti_gain

      b1         => iso_net%b1
      w1         => iso_net%w1
      w2         => iso_net%w2

      to_gain    => iso_net%to_gain
    ELSE
      ti_ymin    => aniso_net%ti_ymin
      ti_xoffset => aniso_net%ti_xoffset
      ti_gain    => aniso_net%ti_gain

      b1         => aniso_net%b1
      w1         => aniso_net%w1
      w2         => aniso_net%w2

      to_gain    => aniso_net%to_gain
    ENDIF

    ! Step 1: Transform input 
    DO i = 1, surfem_ni
      tran_1(i) = ti_ymin(1) + (x(i) - ti_xoffset(i)) * ti_gain(i)
    ENDDO

    ! Step 2: Propagate through the first layer => x1  = W1 * aux + b1
    DO i = 1, surfem_nh
      prop_1(i) = b1(i)
      DO j = 1, surfem_ni ! sum up all input parameters for each node 
        prop_1(i) = prop_1(i) + W1(j,i) * tran_1(j)  
      ENDDO
    ENDDO

    !adjoint anisotropic neural nework

    x_ad(:)       = 0._jprb
    tran_1_ad     = 0._jprb 
    prop_1_ad     = 0._jprb 
    prop_2_ad     = 0._jprb 
    prop_1_tmp_ad = 0._jprb

    ! Step 5. Output Y transformed back with mapminmax => Y = ((aux - oymi ) ./ ogai ) + ooff
    DO i = 1, surfem_no 
      prop_2_ad(i) = prop_2_ad(i) + (e_out_ad(i) / to_gain(i))
    ENDDO

    ! Step 4: Propagate through the second layer => aux  = W2 * aux + b2
    DO i = 1, surfem_no 
      DO j = 1, surfem_nh
        prop_1_tmp_ad(j) = prop_1_tmp_ad(j) + (W2(j,i) * prop_2_ad(i))
      ENDDO
      prop_2_ad(i) = 0._jprb
    ENDDO

    ! Step 3: Transform with tansig(tanh).
    DO i = 1, surfem_nh
      prop_1_ad(i) = prop_1_ad(i) + (4._jprb * prop_1_tmp_ad(i) * EXP(-2._jprb * prop_1(i)) / &
          ((1._jprb + EXP(-2._jprb * prop_1(i)))**2))
      DO j = 1, surfem_ni ! sum up all input parameters for each node 
        tran_1_ad(j) = tran_1_ad(j) + (W1(j,i) * prop_1_ad(i))
      ENDDO ! Step 2: Propagate through the first layer => x1  = W1 * aux + b1
      prop_1_ad(i) = 0._jprb
    ENDDO

    ! Step 1: Transform input 
    DO i = 1, surfem_ni
      x_ad(i) = x_ad(i) + (tran_1_ad(i) * ti_gain(i))
    ENDDO

    windspeed_ad = windspeed_ad + x_ad(3)
    SST_ad = SST_ad + x_ad(4)
    SSS_ad = SSS_ad + x_ad(5)
    e_Nv_ad = e_Nv_ad + x_ad(6)
    e_Nh_ad = e_Nh_ad + x_ad(7)

  END SUBROUTINE net_surfem_ad

  !> Calculate AD of final emissivity component values
  !! @param[in]     phi         phi (reletive wind direction) degrees
  !! @param[in]     e_aniso     anisotropic emissivities 
  !! @param[in]     ev_ad       V-pol emissivity increment
  !! @param[in]     eh_ad       H-pol emissivity increment
  !! @param[in]     e3_ad       3rd Stokes vector emissivity increment
  !! @param[in]     e4_ad       4th Stokes vector emissivity increment
  !! @param[in]     phi_ad      phi increment
  !! @param[in,out] e_aniso_ad  anisotropic emissivity increments
  !! @param[in,out] e_Nv_ad     neutral V-pol emissivity increment
  !! @param[in,out] e_Nh_ad     neutral V-pol emissivity increment
  !! @param[in,out] e_v0_ad     isotropic V-pol emissivity increment
  !! @param[in,out] e_h0_ad     isotropic H-pol emissivity increment
  SUBROUTINE e_total_ad(phi, e_aniso, ev_ad, eh_ad, e3_ad, e4_ad, &
       phi_ad, e_aniso_ad, e_Nv_ad, e_Nh_ad, e_v0_ad, e_h0_ad)

    REAL(jprb), INTENT(IN)    :: phi, e_aniso(8)
    REAL(jprb), INTENT(IN)    :: ev_ad, eh_ad, e3_ad, e4_ad
    REAL(jprb), INTENT(INOUT) :: phi_ad, e_aniso_ad(8)
    REAL(jprb), INTENT(INOUT) :: e_Nv_ad, e_Nh_ad, e_v0_ad, e_h0_ad

    REAL(jprb) :: e_v1, e_h1, e_31, e_41, e_v2, e_h2, e_32, e_42
    REAL(jprb) :: e_v1_ad, e_h1_ad, e_31_ad, e_41_ad, e_v2_ad, e_h2_ad, e_32_ad, e_42_ad
    REAL(jprb) :: phi_r, phi_r_ad ! phi in radians

    ! direct
    e_v1 = e_aniso(1)
    e_h1 = e_aniso(2)
    e_31 = e_aniso(3)
    e_41 = e_aniso(4)
    e_v2 = e_aniso(5)
    e_h2 = e_aniso(6)
    e_32 = e_aniso(7)
    e_42 = e_aniso(8)

    phi_r = phi * pi / 180._jprb 

    !=====adjoint==========

    e_v1_ad = 0.0_jprb 
    e_v2_ad = 0.0_jprb 
    e_h1_ad = 0.0_jprb 
    e_h2_ad = 0.0_jprb 
    e_31_ad = 0.0_jprb 
    e_32_ad = 0.0_jprb  
    e_41_ad = 0.0_jprb 
    e_42_ad = 0.0_jprb 
    phi_r_ad = 0.0_jprb 

    e_Nv_ad = e_Nv_ad + ev_ad
    e_v0_ad = e_v0_ad + ev_ad
    e_v1_ad = e_v1_ad + (ev_ad * COS(phi_r))
    e_v2_ad = e_v2_ad + (ev_ad * COS(2._jprb*phi_r))
    phi_r_ad = phi_r_ad - e_v1 * ev_ad * SIN(phi_r) - e_v2 * ev_ad * 2._jprb * SIN(2._jprb*phi_r)

    e_Nh_ad = e_Nh_ad + eh_ad
    e_h0_ad = e_h0_ad + eh_ad
    e_h1_ad = e_h1_ad + (eh_ad * COS(phi_r))
    e_h2_ad = e_h2_ad + (eh_ad * COS(2._jprb*phi_r))
    phi_r_ad = phi_r_ad - e_h1 * eh_ad * SIN(phi_r) - e_h2 * eh_ad * 2._jprb * SIN(2._jprb*phi_r)

    e_31_ad = e_31_ad + (e3_ad * SIN(phi_r)) 
    e_32_ad = e_32_ad + (e3_ad * SIN(2._jprb*phi_r)) 
    phi_r_ad = phi_r_ad + e_31 * e3_ad * COS(phi_r) + e_32 * e3_ad * 2._jprb*COS(2._jprb*phi_r)

    e_41_ad = e_41_ad + (e4_ad * SIN(phi_r))
    e_42_ad = e_42_ad + (e4_ad * SIN(2._jprb*phi_r))
    phi_r_ad = phi_r_ad + e_41 * e4_ad * COS(phi_r) + e_42 * e4_ad * 2._jprb*COS(2._jprb*phi_r)

    e_aniso_ad(1) = e_aniso_ad(1) + e_v1_ad
    e_aniso_ad(2) = e_aniso_ad(2) + e_h1_ad
    e_aniso_ad(3) = e_aniso_ad(3) + e_31_ad
    e_aniso_ad(4) = e_aniso_ad(4) + e_41_ad
    e_aniso_ad(5) = e_aniso_ad(5) + e_v2_ad
    e_aniso_ad(6) = e_aniso_ad(6) + e_h2_ad
    e_aniso_ad(7) = e_aniso_ad(7) + e_32_ad
    e_aniso_ad(8) = e_aniso_ad(8) + e_42_ad

    phi_ad = phi_ad + (phi_r_ad * pi / 180._jprb)

  END SUBROUTINE e_total_ad

END MODULE rttov_surfem_ocean_ad_mod
