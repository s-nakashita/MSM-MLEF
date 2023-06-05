! Description:
!> @file
!!   Returns sea surface BRDFs from the internal RTTOV BRDF model.
!
!> @brief
!!   Returns sea surface BRDFs from the internal RTTOV BRDF model.
!!
!! @details
!!   This returns sea surface BRDF values from the RTTOV sea surface BRDF model
!!   selected in the opts structure.
!!
!!   The profiles(:) argument must be populated with the necessary variables
!!   used by the BRDF model:
!!      satellite zenith and azimuth angles,
!!      solar zenith and azimuth angles,
!!      10m wind u/v, wind fetch
!!
!!   This subroutine calls the internal BRDF subroutines within RTTOV.
!!   This means that if the surface type is set to land or sea-ice, then
!!   the fixed default land/sea-ice BRDF will be returned for those profiles.
!!
!!   BRDF values are calculated for every channel/profile where calcrefl
!!   is true. Values in the brdf array argument are untouched in where
!!   the corresponding elements of calcrefl are false. If you only require sea
!!   surface BRDF values, the optional arguments are not required.
!!
!!   If the diffuse_refl argument is passed then these values are also returned.
!!   The diffuse reflectance is a directional-hemispherical albedo which is
!!   divided by pi to obtain a Lambertian BRDF for surface-reflected
!!   downward-scattered solar radiation.
!!
!!   To obtain the same diffuse_refl values as those from RTTOV where calcrefl
!!   is false, the corresponding values of brdf and diffuse_refl must match
!!   the refl_in and diffuse_refl_in members of the reflectance(:) input
!!   argument to RTTOV.
!!
!!   The values of BRDF and diffuse_refl computed by RTTOV are summarised
!!   below: they vary depending on calcrefl, surface type and channel
!!   wavelength (less than 3 microns or 3-5 microns).
!!
!!   For mixed thermal+solar channels (3-5 microns), RTTOV uses emissivity
!!   values for some calculations. Therefore to obtain the same values as RTTOV
!!   from this subroutine for these channels you should provide the emissivity
!!   argument suitably populated (e.g. after calling rttov_get_sea_emis).
!!
!!   Reflectances are only returned for channels at wavelengths below 5 microns.
!!
!!   NB The BRDF calculation uses the local satellite and solar path angles in
!!   the near-surface layer. If the elevation is non-zero or refraction is
!!   enabled (addrefrac=true) then these angles differ slightly to the input
!!   zenith angles in the profiles structure. Therefore under these conditions
!!   the BRDFs returned by this routine will differ very slightly to those
!!   computed within RTTOV.
!!
!!   Summary of BRDF and diffuse reflectance values returned by this routine:
!!
!!   Direct solar-surface-satellite BRDF values (brdf argument):
!!
!!   Where calcrefl is true:
!!     Sea surfaces:
!!       - for all channels, BRDF is computed using sun-glint model
!!     Land/sea-ice surfaces:
!!       - for channels below 3 microns, BRDF is a fixed value
!!       - for channels at 3-5 microns, BRDF is (1-emissivity)/pi (if
!!           emissivity is supplied, otherwise the same fixed value as above)
!!
!!   Where calcrefl is false (all surface types):
!!       - for all channels, BRDF remains unchanged
!!
!!   Surface reflectance for downward-scattered radiation (diffuse_refl)
!!   argument:
!!
!!   Where calcrefl is true:
!!     Sea surfaces:
!!       - for channels below 3 microns, diffuse_refl is from fixed spectra
!!       - for channels at 3-5 microns, diffuse_refl is set to (1-emissivity)/pi
!!           if emissivity argument is supplied, otherwise unchanged
!!     Land/sea-ice surfaces:
!!       - for channels below 3 microns, diffuse_refl is brdf
!!       - for channels at 3-5 microns, diffuse_refl is set to (1-emissivity)/pi
!!           if emissivity argument is supplied, otherwise unchanged
!!
!!   Where calcrefl is false (all surface types):
!!       - for channels below 3 microns, if input diffuse_refl > 0 then values
!!           remains unchanged, otherwise is set to brdf value
!!       - for channels at 3-5 microns, diffuse_refl remains unchanged
!!
!!
!! @param[out]    err               status on exit
!! @param[in]     opts              options to configure the simulations
!! @param[in]     chanprof          specifies channels and profiles to simulate
!! @param[in]     profiles          input atmospheric profiles and surface variables
!! @param[in]     coefs             coefficients structure for instrument to simulate
!! @param[in]     calcrefl          logical flags indicating which BRDF values to calculate
!! @param[in,out] brdf              brdf values (direct solar-surface-satellite reflectance)
!! @param[in,out] diffuse_refl      diffuse reflectance values (Lambertian albedo), optional
!! @param[in]     emissivity        surface emissivities, used to calculate reflectances in some cases, optional
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
SUBROUTINE rttov_get_sea_brdf( &
                err,            &! out
                opts,           &! in
                chanprof,       &! in
                profiles,       &! in
                coefs,          &! in
                calcrefl,       &! in
                brdf,           &! inout
                diffuse_refl,   &! inout, optional
                emissivity)      ! in, optional
!INTF_OFF
#include "throw.h"
!INTF_ON
  USE parkind1, ONLY : jpim, jprb, jplm
  USE rttov_types, ONLY : &
          rttov_chanprof, &
          rttov_profile,  &
          rttov_coefs,    &
          rttov_options
!INTF_OFF
  USE rttov_const, ONLY : deg2rad, pi_r
  USE rttov_types, ONLY : &
          rttov_profile_aux, &
          rttov_raytracing,  &
          rttov_sunglint,    &
          rttov_emissivity,  &
          rttov_reflectance
  USE yomhook, ONLY : lhook, dr_hook, jphook
!INTF_ON
  IMPLICIT NONE

  INTEGER(jpim),        INTENT(OUT)                     :: err
  TYPE(rttov_options),  INTENT(IN)                      :: opts
  TYPE(rttov_chanprof), INTENT(IN)                      :: chanprof(:)
  TYPE(rttov_profile),  INTENT(IN)                      :: profiles(:)
  TYPE(rttov_coefs),    INTENT(IN)                      :: coefs
  LOGICAL(jplm),        INTENT(IN)                      :: calcrefl(SIZE(chanprof))
  REAL(jprb),           INTENT(INOUT)                   :: brdf(SIZE(chanprof))
  REAL(jprb),           INTENT(INOUT), OPTIONAL, TARGET :: diffuse_refl(SIZE(chanprof))
  REAL(jprb),           INTENT(IN),    OPTIONAL         :: emissivity(SIZE(chanprof))
!INTF_END

#include "rttov_errorreport.interface"
#include "rttov_alloc_sunglint.interface"
#include "rttov_calcsurfrefl.interface"
#include "rttov_fresnel.interface"
#include "rttov_refsun.interface"

  TYPE(rttov_profile_aux) :: aux
  TYPE(rttov_raytracing)  :: raytracing
  TYPE(rttov_sunglint)    :: sunglint
  TYPE(rttov_emissivity)  :: emis(SIZE(chanprof))
  TYPE(rttov_reflectance) :: refl(SIZE(chanprof))
  REAL(jprb)              :: fresnrefl(SIZE(chanprof))
  REAL(jprb)              :: refl_norm(SIZE(chanprof))
  REAL(jprb), POINTER     :: diffuse_refl_p(:)
  LOGICAL(jplm)           :: solar(SIZE(chanprof))
  INTEGER(jpim)           :: nchanprof, nprofiles, nlevels, nlayers, i, chan
  REAL(jphook) :: zhook_handle
!----------------------------------------------------------------------------
  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_GET_SEA_BRDF', 0_jpim, ZHOOK_HANDLE)

  !-----------------------------
  ! Initialisation
  !-----------------------------
  ! Check for solar-compatible coefs
  IF (coefs%coef%fmv_model_ver < 9) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, 'rtcoef file does not support solar simulations')
  ENDIF

  ! This routine does not work for PC simulations
  IF (ASSOCIATED(coefs%coef_htfrtc%sensor_freq) .OR. opts%htfrtc_opts%htfrtc) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, 'rttov_get_sea_brdf does not work for HTFRTC coefficients')
  ENDIF
  IF (opts%rt_ir%pc%addpc) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, 'rttov_get_sea_brdf does not work for PC-RTTOV')
  ENDIF

  nchanprof = SIZE(chanprof)
  nprofiles = SIZE(profiles)
  nlevels = profiles(1)%nlevels
  nlayers = profiles(1)%nlayers

  IF (PRESENT(diffuse_refl)) THEN
    diffuse_refl_p => diffuse_refl
  ELSE
    ALLOCATE(diffuse_refl_p(nchanprof))
  ENDIF

  IF (PRESENT(emissivity)) emis%emis_out = emissivity

  CALL rttov_alloc_sunglint(err, sunglint, opts, nprofiles, &
                            coefs%coef%ws_nomega, 1_jpim, &
                            .FALSE._jplm, .TRUE._jplm)

  ALLOCATE(aux%s(nprofiles), stat=err)
  THROWM(err.NE.0, 'Error allocating data')

  DO i = 1, nchanprof
    solar(i) = coefs%coef%ss_val_chn(chanprof(i)%chan) > 0 .AND. &
               (.NOT. coefs%coef%ff_val_chn(chanprof(i)%chan) == 0)
  ENDDO

  ! The near-surface local path angles are equal to the input zenith angles if
  ! plane-parallel geometry has been selected, or if the surface elevation is
  ! zero and atmospheric refraction is turned off. If these conditions are not
  ! met, the BRDF values will be slightly different to the values obtained from
  ! RTTOV. The full calculation would be much slower.

  ALLOCATE(raytracing%pathsat(nlayers,nprofiles), &
           raytracing%pathsun(nlayers,nprofiles), stat=err)
  THROWM(err.NE.0, 'Error allocating data')

  aux%s(:)%nearestlev_surf = nlevels
  DO i = 1, nprofiles
    raytracing%pathsat(nlayers,i) = 1._jprb / COS(profiles(i)%zenangle * deg2rad)
    raytracing%pathsun(nlayers,i) = 1._jprb / COS(profiles(i)%sunzenangle * deg2rad)
  ENDDO

  !-------------------------------------------
  ! Compute BRDFs
  !-------------------------------------------

  ! For BRDF we are only modifying elements where calcrefl is true
  WHERE (calcrefl(:)) brdf(:) = 0._jprb

  ! For diffuse_refl we modify elements where calcrefl is true, but also those
  ! where calcrefl is false and the input diffuse_refl is <= 0 for channels
  ! below 3 microns only. We must pass the input diffuse_refl values to the
  ! RTTOV routines.
  IF (PRESENT(diffuse_refl)) refl(:)%diffuse_refl_in = diffuse_refl(:)

  CALL rttov_refsun(opts, profiles, coefs%coef, aux, sunglint, raytracing)

  CALL rttov_fresnel(chanprof, calcrefl, profiles, solar, &
                     coefs%coef, sunglint, fresnrefl)

  CALL rttov_calcsurfrefl(coefs%coef, profiles, sunglint, fresnrefl, solar, &
                          chanprof, refl_norm, calcrefl, emis, refl, diffuse_refl_p)

  ! For BRDF copy only calcrefl true elements back to output array
  WHERE (calcrefl(:)) brdf(:) = refl(:)%refl_out

  ! The RTTOV routine computes an albedo, so divide by 1/pi to obtain BRDF.
  ! All elements of diffuse_refl_p were modified where calcrefl is true, and
  ! where calcrefl is false, only those for channels below 3 microns where
  ! input diffuse_refl value was <= 0.
  diffuse_refl_p(:) = diffuse_refl_p(:) * pi_r

  ! For mixed thermal+solar channels, we need to compute diffuse_refl if
  ! emissivity was supplied as this is not done in rttov_calcsurfrefl
  IF (PRESENT(emissivity) .AND. PRESENT(diffuse_refl)) THEN
    DO i = 1, nchanprof
      IF (calcrefl(i)) THEN
        chan = chanprof(i)%chan
        IF (coefs%coef%ss_val_chn(chan) == 1) diffuse_refl(i) = (1._jprb - emissivity(i)) * pi_r
      ENDIF
    ENDDO
  ENDIF

  DEALLOCATE(raytracing%pathsat, raytracing%pathsun, stat=err)
  THROWM(err.NE.0, 'Error deallocating data')

  DEALLOCATE(aux%s, stat=err)
  THROWM(err.NE.0, 'Error deallocating data')

  CALL rttov_alloc_sunglint(err, sunglint, opts, nprofiles, 0_jpim, 0_jpim)

  IF (.NOT. PRESENT(diffuse_refl)) THEN
    DEALLOCATE(diffuse_refl_p)
  ENDIF

  IF (LHOOK) CALL DR_HOOK('RTTOV_GET_SEA_BRDF',1_jpim,ZHOOK_HANDLE)
  CATCH
  IF (LHOOK) CALL DR_HOOK('RTTOV_GET_SEA_BRDF',1_jpim,ZHOOK_HANDLE)
END SUBROUTINE rttov_get_sea_brdf
