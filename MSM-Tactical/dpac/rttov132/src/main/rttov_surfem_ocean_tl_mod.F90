! Description:
!> @file
!!   Subroutines for TL of SURFEM-Ocean MW sea surface emissivity model
!
!> @brief
!!   Subroutines for TL of SURFEM-Ocean MW sea surface emissivity model
!!
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
MODULE rttov_surfem_ocean_tl_mod

  USE parkind1, ONLY : jpim, jprb
  USE rttov_const, ONLY : pi

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: rttov_surfem_ocean_tl

CONTAINS

  !> Return TL of H-pol , V-pol and S3 and S4 emissivities for given frequency, zenith angle, 
  !! Tskin, salinity, wind speed and reletive wind direction
  !! @param[in]     f_GHz         frequency (GHz)
  !! @param[in]     theta         zenith angle (degrees)
  !! @param[in]     windspeed     wind speed (m/s)
  !! @param[in]     SST           skin temperature (K)
  !! @param[in]     SSS           salinity (practical salinity unit)
  !! @param[in]     phi           relative wind direction (degrees)
  !! @param[in]     windspeed_tl  wind speed perturbation
  !! @param[in]     SST_tl        skin temperature perturbation
  !! @param[in]     SSS_tl        salinity perturbation
  !! @param[in]     phi_tl        relative wind direction perturbation
  !! @param[out]    ev            Vertical polarisation emissivity 
  !! @param[out]    eh            Horizontal polarisation emissivity 
  !! @param[out]    e3            3rd Stokes vector emissivity
  !! @param[out]    e4            4th Stokes vector emissivity
  !! @param[out]    ev_tl         V-pol emissivity perturbation
  !! @param[out]    eh_tl         H-pol emissivity perturbation
  !! @param[out]    e3_tl         3rd Stokes vector emissivity perturbation
  !! @param[out]    e4_tl         4th Stokes vector emissivity perturbation
  SUBROUTINE rttov_surfem_ocean_tl(f_GHz, theta, windspeed, SST, SSS, phi, &
       windspeed_tl, SST_tl, SSS_tl, phi_tl, &
       ev, eh, e3, e4, &
       ev_tl, eh_tl, e3_tl, e4_tl)

    USE rttov_surfem_ocean_coef_mod, ONLY : &
        surfem_nn_min_sst,       &
        surfem_nn_max_sst,       &
        surfem_nn_min_sss,       &
        surfem_nn_max_sss,       &
        surfem_nn_min_windspeed, &
        surfem_nn_max_windspeed, &
        surfem_nn_min_freq,      &
        surfem_nn_max_freq      

    REAL(jprb), INTENT(IN)  :: f_GHz, theta, windspeed, SST, SSS, phi
    REAL(jprb), INTENT(IN)  :: windspeed_tl, SST_tl, SSS_tl, phi_tl
    REAL(jprb), INTENT(OUT) :: ev, eh, e3, e4
    REAL(jprb), INTENT(OUT) :: ev_tl, eh_tl, e3_tl, e4_tl

    ! Local copies of input variables clipped to neural network training limits
    REAL(jprb) :: SSTl, SSSl, windspeedl, f_GHzl
    REAL(jprb) :: SSTl_tl, SSSl_tl, windspeedl_tl

    COMPLEX(jprb) :: dielec, dielec_tl
    REAL(jprb)    :: e_Nv, e_Nh
    REAL(jprb)    :: e_Nv_tl, e_Nh_tl
    REAL(jprb)    :: e_v0, e_h0
    REAL(jprb)    :: e_v0_tl, e_h0_tl
    REAL(jprb)    :: e_iso(2), e_iso_tl(2)
    REAL(jprb)    :: e_aniso(8), e_aniso_tl(8)

    ! calculate dielectric constant of sea water
    CALL epsilon_tl(f_GHz, SST, SSS, SST_tl, SSS_tl, dielec, dielec_tl)

    ! calculate flat sea emissivity (Neutral)
    CALL e_neutral_tl(dielec, dielec_tl, theta, e_Nv, e_Nh, e_Nv_tl, e_Nh_tl)

    ! Ensure NN training limits are not exceeded
    f_GHzl     = MAX(MIN(f_GHz, surfem_nn_max_freq), surfem_nn_min_freq)
    SSTl       = MAX(MIN(SST, surfem_nn_max_sst), surfem_nn_min_sst)
    SSSl       = MAX(MIN(SSS, surfem_nn_max_sss), surfem_nn_min_sss)
    windspeedl = MAX(MIN(windspeed, surfem_nn_max_windspeed), surfem_nn_min_windspeed)

    IF (SST < surfem_nn_min_sst .OR. SST > surfem_nn_max_sst) THEN
      SSTl_tl = 0
    ELSE
      SSTl_tl = SST_tl
    ENDIF
    IF (SSS < surfem_nn_min_sss .OR. SSS > surfem_nn_max_sss) THEN
      SSSl_tl = 0
    ELSE
      SSSl_tl = SSS_tl
    ENDIF
    IF (windspeed < surfem_nn_min_windspeed .OR. &
        windspeed > surfem_nn_max_windspeed) THEN
      windspeedl_tl = 0
    ELSE
      windspeedl_tl = windspeed_tl
    ENDIF

    ! calculate isotropic emissivity via neural network
    CALL net_surfem_tl(f_GHzl, theta, windspeedl, SSTl, SSSl, e_Nv, e_Nh, &
         windspeedl_tl, SSTl_tl, SSSl_tl, e_Nv_tl, e_Nh_tl, &
         e_iso, e_iso_tl)
    e_v0 = e_iso(1)
    e_h0 = e_iso(2)
    e_v0_tl = e_iso_tl(1)
    e_h0_tl = e_iso_tl(2)

    ! calculate anisotropic emissivity via neural network
    CALL net_surfem_tl(f_GHzl, theta, windspeedl, SSTl, SSSl, e_Nv, e_Nh, &
         windspeedl_tl, SSTl_tl, SSSl_tl, e_Nv_tl, e_Nh_tl, &
         e_aniso, e_aniso_tl)

    ! calculate output emissitivities
    CALL e_total_tl(e_Nv, e_Nh, e_v0, e_h0, e_aniso, phi, &
         e_Nv_tl, e_Nh_tl, e_v0_tl, e_h0_tl, e_aniso_tl, phi_tl, &
         ev, eh, e3, e4, & 
         ev_tl, eh_tl, e3_tl, e4_tl)

  END SUBROUTINE rttov_surfem_ocean_tl

  SUBROUTINE epsilon_tl(f_GHz, SSTk, SSSi, SSTk_tl, SSSi_tl, dielec, dielec_tl)

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

    ! Computes the dielectric constant of sea water according to the model
    ! by Meissner and Wentz (2004) including corrections by Meissner and Wentz
    ! 2012 and Meissner et al. 2014.
    ! Translated from matlab to fortran by E. Turner (2022) 
    !
    !       Inputs:
    !               f_GHz     : frequency in GHz
    !               SSTk      : Sea surface temperature in kelvin
    !               SSSi      : Sea surface salinity in psu
    !               SSTk_tl   : Sea surface temperature in kelvin
    !               SSSi_tl   : Sea surface salinity in psu
    !
    !       Outputs:
    !               dielec    : complex dielectric constant of sea water
    !               dielec_tl : tl of complex dielectric constant of sea water
    !

    REAL(jprb),       INTENT(IN)    :: f_GHz       !frequency in GHz
    REAL(jprb),       INTENT(IN)    :: SSTk        !Sea surface temperature in Kelvin
    REAL(jprb),       INTENT(IN)    :: SSSi        !Sea surface salinity in psu
    REAL(jprb),       INTENT(IN)    :: SSTk_tl     !Sea surface temperature perturbation
    REAL(jprb),       INTENT(IN)    :: SSSi_tl     !Sea surface salinity in perturbation
    COMPLEX(jprb),    INTENT(OUT)   :: dielec      !complex dielectric constant of sea water
    COMPLEX(jprb),    INTENT(OUT)   :: dielec_tl   !TL of complex dielectric constant of sea water

    ! Local variables
    REAL(jprb) :: SST, SSS, SST2, SST3, SST4, SSS2
    REAL(jprb) :: es, e1, einf, nu1, nu2 ! Fresh water parameters
    REAL(jprb) :: sigma35, R15, alpha0, alpha1, RTR15, sigma  ! Salt water
    REAL(jprb) :: es_s, e1_s, einf_s, nu1_s, nu2_s ! Salt water parameters
    REAL(jprb) :: c1, c2, c3, c4 ! conversion coef from fresh to salt water

    REAL(jprb) :: SST_tl, SSS_tl, SST2_tl, SST3_tl, SST4_tl, SSS2_tl, SSS3_tl
    REAL(jprb) :: es_tl, e1_tl, einf_tl, nu1_tl, nu2_tl ! Fresh water perturbations
    REAL(jprb) :: sigma35_tl, R15_tl, alpha0_tl, alpha1_tl, RTR15_tl, sigma_tl  ! Salt water perts
    REAL(jprb) :: es_s_tl, e1_s_tl, einf_s_tl, nu1_s_tl, nu2_s_tl ! Salt water param perts
    REAL(jprb) :: c1_tl, c2_tl, c3_tl, c4_tl ! conversion coef from fresh to salt water perts

    REAL(jprb) :: es_s_exp, es_s_exp_tl
    REAL(jprb) :: c1_tmp, c1_tmp_tl, c2_tmp, c2_tmp_tl
    
    COMPLEX(jprb), PARAMETER :: i = (0, 1)   ! sqrt(-1) 

    ! clip input variables if they are outside of reasonable limits
    SST = MAX(MIN(SSTk, surfem_dielec_max_sst), surfem_dielec_min_sst) - t0 ! K -> deg C
    SSS = MAX(MIN(SSSi, surfem_dielec_max_sss), surfem_dielec_min_sss)
    IF (SSTk < surfem_dielec_min_sst .OR. SSTk > surfem_dielec_max_sst) THEN
      SST_tl = 0
    ELSE
      SST_tl = SSTk_tl
    ENDIF
    IF (SSSi < surfem_dielec_min_sss .OR. SSSi > surfem_dielec_max_sss) THEN
      SSS_tl = 0
    ELSE
      SSS_tl = SSSi_tl
    ENDIF

    ! combined local variables 
    SST2        = SST * SST
    SST2_tl     = 2.0_jprb * SST * SST_tl
    SST3        = SST2 * SST
    SST3_tl     = 3.0_jprb * SST2 * SST_tl
    SST4        = SST3 * SST
    SST4_tl     = 4.0_jprb * SST3 * SST_tl
    SSS2        = SSS * SSS
    SSS2_tl     = 2.0_jprb * SSS * SSS_tl
    SSS3_tl     = 3.0_jprb * SSS2 * SSS_tl

    !Fresh water parameters
    es          = (es_coef(1) - es_coef(2) * SST) / (es_coef(3) + SST)
    es_tl       = -SST_tl * (es_coef(2) + es) / (es_coef(3) + SST)

    e1          = ai_coef(1) + (ai_coef(2) * SST) + (ai_coef(3) * SST2)
    e1_tl       = (ai_coef(2) * SST_tl) + (ai_coef(3) * SST2_tl)

    nu1         = (45.0_jprb + SST) / ( ai_coef(4) + (ai_coef(5) * SST) + (ai_coef(6) * SST2) )
    nu1_tl      = ((SST_tl * (ai_coef(4) + (ai_coef(5) * SST) + (ai_coef(6) * SST2))) - &
                  (((ai_coef(5) * SST_tl) + (ai_coef(6) * SST2_tl))*(45.0_jprb + SST))) / &
                  ((ai_coef(4) + (ai_coef(5) * SST) + (ai_coef(6) * SST2))**2)

    einf        = ai_coef(7) + (ai_coef(8) * SST)
    einf_tl     = ai_coef(8) * SST_tl

    nu2         = (45.0_jprb + SST) / ( ai_coef(9) + (ai_coef(10) * SST) + (ai_coef(11) * SST2) )
    nu2_tl      = ((SST_tl * (ai_coef(9) + (ai_coef(10) * SST) + (ai_coef(11) * SST2))) - &
                  (((ai_coef(10) * SST_tl) + (ai_coef(11) * SST2_tl))*(45.0_jprb + SST))) / &
                  ((ai_coef(9) + (ai_coef(10) * SST) + (ai_coef(11) * SST2))**2)

!! == Salt water ==
    sigma35     = sigma35_coef(1) + (sigma35_coef(2) * SST) + (sigma35_coef(3) * SST2) - &
                  (sigma35_coef(4) * SST3) + (sigma35_coef(5)*SST4)
    sigma35_tl  = (sigma35_coef(2) * SST_tl) + (sigma35_coef(3) * SST2_tl) - &
                  (sigma35_coef(4) * SST3_tl) + (sigma35_coef(5) * SST4_tl)

    R15         = SSS * ( R15_coef(1) + (R15_coef(2) * SSS) + (R15_coef(3) * SSS2) ) / & 
                  ( R15_coef(4) + (R15_coef(5) * SSS) + SSS2 )
    R15_tl      = ((SSS_tl * R15_coef(1) + SSS2_tl * R15_coef(2) + SSS3_tl * R15_coef(3)) * &
                   (R15_coef(4) + R15_coef(5) * SSS + SSS2) - &
                   (R15_coef(5) * SSS_tl + SSS2_tl) * &
                   (SSS * (R15_coef(1) + R15_coef(2) * SSS + R15_coef(3) * SSS2))) / &
                  (R15_coef(4) + R15_coef(5) * SSS + SSS2)**2

    alpha0      = ( alpha0_coef(1) + (alpha0_coef(2) * SSS) - (alpha0_coef(3) * SSS2) ) / &
                  ( alpha0_coef(4) + (alpha0_coef(5) * SSS) + SSS2 )
    alpha0_tl   = SSS_tl *  (( ( alpha0_coef(2)  - (2.0_jprb * alpha0_coef(3) * SSS) ) * &
                  ( alpha0_coef(4) + (alpha0_coef(5) * SSS) + SSS2 ) ) - ( ( alpha0_coef(5) + (2.0_jprb * SSS) ) * &
                  ( alpha0_coef(1) + (alpha0_coef(2) * SSS) - (alpha0_coef(3) * SSS2) ) ))  / &
                  (( alpha0_coef(4) + (alpha0_coef(5) * SSS) + SSS2 )**2)

    alpha1      =  alpha1_coef(1) - (alpha1_coef(2) * SSS) + alpha1_coef(3) * SSS2
    alpha1_tl   =  -alpha1_coef(2) * SSS_tl + alpha1_coef(3) * SSS2_tl

    ! alpha0 and alpha1 are functions of SSS so we've got a multivariate deriv!
    RTR15       = 1.0_jprb + ( SST - 15.0_jprb ) * alpha0 / ( alpha1 + SST )
    RTR15_tl    = ((SST_tl * alpha0 + alpha0_tl * (SST - 15.0_jprb)) * (alpha1 + SST) - &
                  (alpha1_tl + SST_tl) * (SST - 15.0_jprb) * alpha0) / &
                  (alpha1 + SST)**2

    sigma       = sigma35 * R15 * RTR15
    sigma_tl    = (sigma35_tl * R15 * RTR15) + (sigma35 * R15_tl * RTR15) + (sigma35 * R15 * RTR15_tl)

    ! Salt water parameters 
    es_s_exp    = (bi_new1_coef(1) * SSS) + (bi_new1_coef(2) * SSS2) + (bi_new1_coef(3) * SSS * SST)
    es_s        = es * EXP(es_s_exp)
    es_s_exp_tl = (bi_new1_coef(1) * SSS_tl) + (bi_new1_coef(2) * SSS2_tl) + &
                  (bi_new1_coef(3) * SSS_tl * SST) + (bi_new1_coef(3) * SSS * SST_tl)
    es_s_tl     = (es_tl * EXP(es_s_exp)) + (es_s_exp_tl * es_s)

    ! conversion coefs from fresh to salt water

    IF ( SST <= 30.0_jprb ) THEN
      c1_tmp    = bi_new2_coef(1) + (bi_new2_coef(2) * SST) + (bi_new2_coef(3) * SST2) + &
                  (bi_new2_coef(4) * SST3) + (bi_new2_coef(5) * SST4)
      c1_tmp_tl = (bi_new2_coef(2) * SST_tl) + (bi_new2_coef(3) * SST2_tl) + &
                  (bi_new2_coef(4) * SST3_tl) + (bi_new2_coef(5) * SST4_tl)
      c1        = 1.0_jprb + (SSS * c1_tmp)
      c1_tl     = (SSS_tl * c1_tmp) + (SSS * c1_tmp_tl)
    ELSE
      c1        = 1.0_jprb + SSS * ( c1_coef(1) + c1_coef(2) * (SST - 30.0_jprb) )
      c1_tl     = (SSS_tl * (c1_coef(1) + c1_coef(2) * (SST - 30.0_jprb))) + (SSS * c1_coef(2) * SST_tl)
    ENDIF

    nu1_s       = nu1*c1
    nu1_s_tl    = (nu1_tl*c1) + (nu1*c1_tl)

    c2_tmp      = (bi_coef(7)*SSS) + (bi_coef(8)*SSS2) + (bi_coef(9)*SSS*SST)
    c2_tmp_tl   = (bi_coef(7)*SSS_tl) + (bi_coef(8)*SSS2_tl) + (bi_coef(9)*SSS_tl*SST) + (bi_coef(9)*SSS*SST_tl)
    c2          = EXP(c2_tmp) 
    c2_tl       = c2_tmp_tl * c2

    e1_s        = e1*c2
    e1_s_tl     = (e1_tl*c2) + (e1*c2_tl)

    c3          = 1.0_jprb + SSS * ( bi_coef(10) + (0.5_jprb * bi_coef(11)) * (SST + 30.0_jprb) )
    c3_tl       = (SSS_tl * (bi_coef(10) + (0.5_jprb * bi_coef(11)) * (SST + 30.0_jprb))) + &
                  (SSS * 0.5_jprb * bi_coef(11) * SST_tl)

    nu2_s       = (nu2*c3)
    nu2_s_tl    = (nu2_tl*c3) + (nu2*c3_tl)

    c4          = 1.0_jprb  + SSS * (bi_coef(12) + (bi_coef(13) * SST))
    c4_tl       = (SSS_tl * (bi_coef(12) + (bi_coef(13) * SST))) + (SSS * bi_coef(13) * SST_tl)

    einf_s      = (einf*c4)  
    einf_s_tl   = (einf_tl*c4) + (einf*c4_tl) 

    dielec = (es_s - e1_s) * (1.0_jprb + i * f_GHz/nu1_s) / (1.0_jprb + (f_GHz/nu1_s)**2) + &
             (e1_s - einf_s) * (1.0_jprb + i * f_GHz/nu2_s) / (1.0_jprb + (f_GHz/nu2_s)**2) + &
             einf_s + &
             i * sigma * f0 / f_GHz

    dielec_tl = ((es_s_tl - e1_s_tl) * (1.0_jprb - i * f_GHz/nu1_s) - &
                 (es_s - e1_s) * i * nu1_s_tl * f_GHz/nu1_s**2) * (1.0_jprb + i * (f_GHz/nu1_s))**2 / &
                (1.0_jprb + (f_GHz/nu1_s)**2)**2 + &
                ((e1_s_tl - einf_s_tl) * (1.0_jprb - i * f_GHz/nu2_s) - &
                 (e1_s - einf_s) * i * nu2_s_tl * f_GHz/nu2_s**2) * (1.0_jprb + i * (f_GHz/nu2_s))**2 / &
                (1.0_jprb + (f_GHz/nu2_s)**2)**2 + &
                einf_s_tl + &
                i * sigma_tl * f0 / f_GHz
  END SUBROUTINE epsilon_tl

  ! Estimation of e neutral (flat sea no wind) from dielectric constant
  SUBROUTINE e_neutral_tl(dielec, dielec_tl, theta, e_Nv, e_Nh, e_Nv_tl, e_Nh_tl)

    COMPLEX(jprb),  INTENT(IN)  :: dielec    ! of complex dielectric constant of sea water
    COMPLEX(jprb),  INTENT(IN)  :: dielec_tl !TL of complex dielectric constant of sea water
    REAL(jprb),     INTENT(IN)  :: theta     !incidence angle in degrees
    REAL(jprb),     INTENT(OUT) :: e_Nv      !vertical component of e Neutral
    REAL(jprb),     INTENT(OUT) :: e_Nh      !horizontal component of e Neutral
    REAL(jprb),     INTENT(OUT) :: e_Nv_tl   !TL of vertical component of e Neutral
    REAL(jprb),     INTENT(OUT) :: e_Nh_tl   !TL of horizontal component of e Neutral

    COMPLEX(jprb) :: f_v ! vertical fresnel reflection coefficient unsquared
    COMPLEX(jprb) :: f_h ! horizontal fresnel reflection coefficient unsquared
    COMPLEX(jprb) :: f_v_tl ! vertical fresnel reflection coefficient unsquared
    COMPLEX(jprb) :: f_h_tl ! horizontal fresnel reflection coefficient unsquared
    REAL(jprb) :: fres_v ! vertical fresnel reflection coefficient
    REAL(jprb) :: fres_h ! horizontal fresnel reflection coefficient
    REAL(jprb) :: fres_v_tl ! vertical fresnel reflection coefficient
    REAL(jprb) :: fres_h_tl ! horizontal fresnel reflection coefficient

    REAL(jprb)    :: cos_theta
    REAL(jprb)    :: sin_theta
    COMPLEX(jprb) :: sqrt_d, sqrt_d_tl 

    cos_theta  = COS(theta*pi/180._jprb)
    sin_theta  = SIN(theta*pi/180._jprb)

    sqrt_d    = SQRT(dielec - sin_theta * sin_theta)
    sqrt_d_tl = 0.5_jprb * dielec_tl / sqrt_d

    f_v       = ((dielec * cos_theta) - sqrt_d) / ((dielec * cos_theta) + sqrt_d) 
    f_v_tl    = 2 * (dielec_tl * cos_theta * sqrt_d - sqrt_d_tl * dielec * cos_theta) / &
                (dielec * cos_theta + sqrt_d)**2

    f_h       = (cos_theta - sqrt_d) / (cos_theta + sqrt_d)
    f_h_tl    = -2 * sqrt_d_tl * cos_theta / &
                (cos_theta + sqrt_d)**2

    fres_v    = REAL(f_v)**2 + AIMAG(f_v)**2
    fres_v_tl = 2.0_jprb * REAL(f_v_tl) * REAL(f_v) + 2.0_jprb * AIMAG(f_v_tl) * AIMAG(f_v)

    fres_h    = REAL(f_h)**2 + AIMAG(f_h)**2
    fres_h_tl = 2.0_jprb * REAL(f_h_tl) * REAL(f_h) + 2.0_jprb * AIMAG(f_h_tl) * AIMAG(f_h)

    e_Nv      = 1.0_jprb - fres_v
    e_Nv_tl   = -fres_v_tl

    e_Nh      = 1.0_jprb - fres_h
    e_Nh_tl   = -fres_h_tl

  END SUBROUTINE e_neutral_tl


  ! X is a vector of [f_GHz_vec,theta_vec,windspeed_vec,SST_vec,SSS_vec,evn,ehn]

  ! 1. Input X transformed with mapminmax = Process matrices by mapping row minimum and maximum &
  !    values to [-1 1] => aux = iymi + (X - ioff).* igai [y = (ymax-ymin)*(x-xmin)/(xmax-xmin) + ymin]
  ! 2. Propagate through the first layer => x1  = W1 * aux + b1
  ! 3. Transform with tansig(tanh) => aux = 2 ./ (1+exp(-2 * x1 ) ) -1 (activation function)
  ! 4. Propagate through output layer => aux = W2 * aux + b2 (needed?)
  ! 5. Output Y transformed back with mapminmax => Y = ((aux - oymi ) ./ ogai ) + ooff

  !> Return TL of emissivities for given frequency, zenith angle, T-skin
  !! salinity and wind speed
  !! @param[in]     f_GHz         frequency (GHz)
  !! @param[in]     theta         zenith angle (degrees)
  !! @param[in]     windspeed     wind speed (m/s)
  !! @param[in]     tskin         skin temperature (K)
  !! @param[in]     salinity      salinity (practical salinity unit)
  !! @param[in]     e_Nv          vertical component of e Neutral
  !! @param[in]     e_Nh          horizontal component of e Neutral
  !! @param[in]     windspeed_tl  wind speed (m/s)
  !! @param[in]     tskin_tl      skin temperature (K)
  !! @param[in]     salinity_tl   salinity (practical salinity unit)
  !! @param[in]     e_Nv_tl       vertical component of e Neutral
  !! @param[in]     e_Nh_tl       horizontal component of e Neutral
  !! @param[out]    e_out         calculated emissivity components
  !! @param[out]    e_out_tl      calculated emissivity component perturbations
  SUBROUTINE net_surfem_tl(f_GHz, theta, windspeed, tskin, salinity, e_Nv, e_Nh, &
      windspeed_tl, tskin_tl, salinity_tl, e_Nv_tl, e_Nh_tl, e_out, e_out_tl)

    USE rttov_surfem_ocean_coef_mod, ONLY : &
        surfem_nh, surfem_ni, surfem_no_iso, iso_net, aniso_net

    REAL(jprb), INTENT(IN)  :: f_GHz, theta, windspeed, tskin, salinity, e_Nv, e_Nh
    REAL(jprb), INTENT(IN)  :: windspeed_tl, tskin_tl, salinity_tl, e_Nv_tl, e_Nh_tl
    REAL(jprb), INTENT(OUT) :: e_out(:), e_out_tl(:)

    REAL(jprb) :: x(surfem_ni)
    REAL(jprb) :: tran_1(surfem_ni)
    REAL(jprb) :: prop_1(surfem_nh)
    REAL(jprb) :: prop_1_tmp(surfem_nh)
    REAL(jprb) :: prop_2(surfem_nh)


    REAL(jprb), POINTER :: ti_ymin(:), ti_xoffset(:), ti_gain(:)
    REAL(jprb), POINTER :: b1(:), w1(:,:), b2(:), w2(:,:)
    REAL(jprb), POINTER :: to_ymin(:), to_xoffset(:), to_gain(:)

    REAL(jprb) :: x_tl(surfem_ni)
    REAL(jprb) :: tran_1_tl(surfem_ni)
    REAL(jprb) :: prop_1_tl(surfem_nh)
    REAL(jprb) :: prop_1_tmp_tl(surfem_nh)
    REAL(jprb) :: prop_2_tl(surfem_nh)

    INTEGER(jpim) :: i, j, surfem_no

    x(:) = (/ f_GHz, theta, windspeed, tskin, salinity, e_Nv, e_Nh /)
    x_tl(:) = (/ 0._jprb, 0._jprb, windspeed_tl, tskin_tl, salinity_tl, e_Nv_tl, e_Nh_tl /)

    surfem_no = SIZE(e_out)

    IF (surfem_no == surfem_no_iso) THEN
      ti_ymin    => iso_net%ti_ymin
      ti_xoffset => iso_net%ti_xoffset
      ti_gain    => iso_net%ti_gain

      b1         => iso_net%b1
      w1         => iso_net%w1
      b2         => iso_net%b2
      w2         => iso_net%w2

      to_ymin    => iso_net%to_ymin
      to_xoffset => iso_net%to_xoffset
      to_gain    => iso_net%to_gain
    ELSE
      ti_ymin    => aniso_net%ti_ymin
      ti_xoffset => aniso_net%ti_xoffset
      ti_gain    => aniso_net%ti_gain

      b1         => aniso_net%b1
      w1         => aniso_net%w1
      b2         => aniso_net%b2
      w2         => aniso_net%w2

      to_ymin    => aniso_net%to_ymin
      to_xoffset => aniso_net%to_xoffset
      to_gain    => aniso_net%to_gain
    ENDIF

    ! Step 1: Transform input 
    DO i = 1, surfem_ni
      tran_1(i) = ti_ymin(1) + (x(i) - ti_xoffset(i)) * ti_gain(i)
      tran_1_tl(i) = x_tl(i) * ti_gain(i)
    ENDDO

    ! Step 2: Propagate through the first layer => x1  = W1 * aux + b1
    DO i = 1, surfem_nh
      prop_1(i) = b1(i)
      prop_1_tl(i) = 0._jprb
      DO j = 1, surfem_ni ! sum up all input parameters for each node 
        prop_1(i) = prop_1(i) + W1(j,i) * tran_1(j)  
        prop_1_tl(i) = prop_1_tl(i) + W1(j,i) * tran_1_tl(j) 
      ENDDO 
      ! Step 3: Transform with tansig(tanh)
      prop_1_tmp(i) = 2._jprb / (1._jprb + EXP(-2._jprb * prop_1(i))) - 1._jprb
      prop_1_tmp_tl(i) = 4._jprb * prop_1_tl(i) * EXP(-2._jprb * prop_1(i)) / &
                      ((1._jprb + EXP(-2._jprb * prop_1(i)))**2)
    ENDDO

    ! Step 4: Propagate through the second layer => aux  = W2 * aux + b2
    DO i = 1, surfem_no
      prop_2(i) = b2(i)
      prop_2_tl(i) = 0._jprb
      DO j = 1, surfem_nh
        prop_2(i) = prop_2(i) + W2(j,i) * prop_1_tmp(j)
        prop_2_tl(i) = prop_2_tl(i) + W2(j,i) * prop_1_tmp_tl(j)
      ENDDO
    ENDDO

    ! Step 5. Output Y transformed back with mapminmax => Y = ((aux - oymi ) ./ ogai ) + ooff
    DO i = 1, surfem_no
      e_out(i) = ((prop_2(i) - to_ymin(1)) / to_gain(i)) + to_xoffset(i) 
      e_out_tl(i) = prop_2_tl(i) / to_gain(i)
    ENDDO

  END SUBROUTINE net_surfem_tl

  SUBROUTINE e_total_tl(e_Nv, e_Nh, e_v0, e_h0, e_aniso, phi, &
      e_Nv_tl, e_Nh_tl, e_v0_tl, e_h0_tl, e_aniso_tl, phi_tl, &
      ev, eh, e3, e4, ev_tl, eh_tl, e3_tl, e4_tl)

    USE rttov_surfem_ocean_coef_mod, ONLY : surfem_no_aniso

    REAL(jprb), INTENT(IN)  :: e_Nv, e_Nh, e_v0, e_h0, e_aniso(surfem_no_aniso)
    REAL(jprb), INTENT(IN)  :: phi ! relative wind direction in degrees
    REAL(jprb), INTENT(IN)  :: e_Nv_tl, e_Nh_tl, e_v0_tl, e_h0_tl, e_aniso_tl(surfem_no_aniso)
    REAL(jprb), INTENT(IN)  :: phi_tl 
    REAL(jprb), INTENT(OUT) :: ev, eh, e3, e4
    REAL(jprb), INTENT(OUT) :: ev_tl, eh_tl, e3_tl, e4_tl

    REAL(jprb) :: e_v1, e_h1, e_31, e_41, e_v2, e_h2, e_32, e_42
    REAL(jprb) :: e_v1_tl, e_h1_tl, e_31_tl, e_41_tl, e_v2_tl, e_h2_tl, e_32_tl, e_42_tl
    REAL(jprb) :: phi_r, phi_r_tl ! phi in radians

    e_v1 = e_aniso(1)
    e_h1 = e_aniso(2)
    e_31 = e_aniso(3)
    e_41 = e_aniso(4)
    e_v2 = e_aniso(5)
    e_h2 = e_aniso(6)
    e_32 = e_aniso(7)
    e_42 = e_aniso(8)

    e_v1_tl = e_aniso_tl(1)
    e_h1_tl = e_aniso_tl(2)
    e_31_tl = e_aniso_tl(3)
    e_41_tl = e_aniso_tl(4)
    e_v2_tl = e_aniso_tl(5)
    e_h2_tl = e_aniso_tl(6)
    e_32_tl = e_aniso_tl(7)
    e_42_tl = e_aniso_tl(8)

    phi_r = phi * pi / 180._jprb 
    phi_r_tl = phi_tl * pi / 180._jprb 

    ev    = e_Nv + e_v0 + e_v1 * COS(phi_r) + e_v2 * COS(2._jprb*phi_r)
    ev_tl = e_Nv_tl + e_v0_tl + (e_v1_tl * COS(phi_r)) - (e_v1 * phi_r_tl * SIN(phi_r)) + &
            (e_v2_tl * COS(2._jprb*phi_r)) - (e_v2 * phi_r_tl * 2._jprb*SIN(2._jprb*phi_r))

    eh    = e_Nh + e_h0 + e_h1 * COS(phi_r) + e_h2 * COS(2._jprb*phi_r)
    eh_tl = e_Nh_tl + e_h0_tl + (e_h1_tl * COS(phi_r)) - (e_h1 * phi_r_tl * SIN(phi_r)) + &
            (e_h2_tl * COS(2._jprb*phi_r)) - (e_h2 * phi_r_tl * 2._jprb*SIN(2._jprb*phi_r))

    e3    = e_31 * SIN(phi_r) + e_32 * SIN(2._jprb*phi_r)
    e3_tl = (e_31_tl * SIN(phi_r)) + (e_31 * phi_r_tl * COS(phi_r)) + &
            (e_32_tl * SIN(2._jprb*phi_r)) + (e_32 * phi_r_tl * 2._jprb*COS(2._jprb*phi_r))

    e4    = e_41 * SIN(phi_r) + e_42 * SIN(2._jprb*phi_r)
    e4_tl = (e_41_tl * SIN(phi_r)) + (e_41 * phi_r_tl * COS(phi_r)) + &
            (e_42_tl * SIN(2._jprb*phi_r)) + (e_42 * phi_r_tl * 2._jprb*COS(2._jprb*phi_r))

  END SUBROUTINE e_total_tl

END MODULE rttov_surfem_ocean_tl_mod
