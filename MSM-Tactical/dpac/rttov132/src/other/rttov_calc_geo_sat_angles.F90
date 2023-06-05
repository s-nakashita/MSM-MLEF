! Description:
!> @file
!!   Calculate geostationary satellite zenith and azimuth angles in supplied
!!   array of profiles given populated lat, lon, and sub-satellite longitude.
!
!> @brief
!!   Calculate geostationary satellite zenith and azimuth angles in supplied
!!   array of profiles given populated lat, lon, and sub-satellite longitude.
!!
!! @details
!!   Optionally the satellite height and sub-satellite latitude can be given
!!   but if omitted they default to 35800 km and 0 degrees respectively.
!!
!!   This routine only computes angles for geostationary sensors, and the
!!   azimuth angle calculation ignores any specified geo_sat_lat.
!!
!!   The return status in err should be checked: the routine will report an
!!   error if any calculated zenith angle exceeds RTTOV's maximum because
!!   such angles will generate errors when running RTTOV. The opts and coefs
!!   arguments are used to determine the maximum legal zenith angle.
!!
!!
!! @param[out]    err             status on exit
!! @param[in]     opts            RTTOV options structure
!! @param[in]     coefs           RTTOV coefficients structure
!! @param[inout]  profiles        array of profile structures
!! @param[in]     geo_sat_lon     longitude of sub-satellite point (degrees)
!! @param[in]     geo_sat_lat     latitude of sub-satellite point (degrees), optional, default 0 deg
!! @param[in]     geo_sat_height  satellite height (km), optional, default 35800 km
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
SUBROUTINE rttov_calc_geo_sat_angles(err,         &
                                     opts,        &
                                     coefs,       &
                                     profiles,    &
                                     geo_sat_lon, &
                                     geo_sat_lat, &
                                     geo_sat_height)
  USE parkind1, ONLY : jpim, jprb
  USE rttov_types, ONLY : &
      rttov_options, &
      rttov_coefs,   &
      rttov_profile
!INTF_OFF
#include "throw.h"
  USE rttov_const, ONLY : &
      deg2rad,     &
      rad2deg,     &
      earthradius, &
      zenmaxv9,    &
      zenmax
!INTF_ON

  IMPLICIT NONE

  INTEGER(jpim),       INTENT(OUT)          :: err
  TYPE(rttov_options), INTENT(IN)           :: opts
  TYPE(rttov_coefs),   INTENT(IN)           :: coefs
  TYPE(rttov_profile), INTENT(INOUT)        :: profiles(:)
  REAL(jprb),          INTENT(IN)           :: geo_sat_lon
  REAL(jprb),          INTENT(IN), OPTIONAL :: geo_sat_lat
  REAL(jprb),          INTENT(IN), OPTIONAL :: geo_sat_height
!INTF_END

#include "rttov_errorreport.interface"

  INTEGER(jpim) :: prof
  REAL(jprb)    :: sat_lat, sat_height
  REAL(jprb)    :: max_sat_zen, r_ratio
  REAL(jprb)    :: lat, lon
  REAL(jprb)    :: cos_dlon, sin_dlon, sin_lat
  REAL(jprb)    :: cos_alpha, sin_alpha
  REAL(jprb)    :: tan_satzen_denom, tan_satzenithang
  REAL(jprb)    :: sat_zen, sat_azim

  TRY

  !----------------------------------------------------------------------------
  ! Prepare data
  !----------------------------------------------------------------------------

  IF (PRESENT(geo_sat_lat)) THEN
    sat_lat = geo_sat_lat
  ELSE
    sat_lat = 0._jprb
  ENDIF
  IF (PRESENT(geo_sat_height)) THEN
    sat_height = geo_sat_height
  ELSE
    sat_height = 35800._jprb
  ENDIF

  ! Ratio of Earth radius to Earth radius plus satellite height
  r_ratio = earthradius / (sat_height + earthradius)

  ! RTTOV's maximum zenith angle depends on the coefficient file
  IF (opts%htfrtc_opts%htfrtc .OR. coefs%coef%fmv_model_ver >= 9) THEN
    max_sat_zen = zenmaxv9
  ELSE
    max_sat_zen = zenmax
  ENDIF

  ! Use a slightly smaller value to avoid generating illegal zenith angles
  max_sat_zen = max_sat_zen * (1._jprb - 1.e-5_jprb)


  !----------------------------------------------------------------------------
  ! Loop over profiles and compute angles for each one
  !----------------------------------------------------------------------------

  DO prof = 1, SIZE(profiles)

    ! Ensure valid latitude and longitude

    IF (profiles(prof)%latitude < -90._jprb .OR. &
        profiles(prof)%latitude > 90._jprb) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Latitude must be in [-90, +90] degrees')
    ENDIF
    lat = profiles(prof)%latitude
    lon = MODULO(profiles(prof)%longitude, 360._jprb)

    cos_dlon = COS((lon - geo_sat_lon) * deg2rad)

    !--------------------------------------------------------------------------
    ! Calculate satellite zenith angle
    !--------------------------------------------------------------------------

    cos_alpha = SIN(lat * deg2rad) * SIN(sat_lat * deg2rad) + &
                COS(lat * deg2rad) * COS(sat_lat * deg2rad) * cos_dlon

    tan_satzen_denom = cos_alpha - r_ratio

    ! Where tan_satzen_denom > 0 locations are above the horizon (zenith angles < 90 degrees)

    IF (tan_satzen_denom > 0) THEN

      ! Zenith angle calculation

      sin_alpha = SQRT(1. - cos_alpha * cos_alpha)
      tan_satzenithang = sin_alpha / tan_satzen_denom
      sat_zen = ATAN(tan_satzenithang) * rad2deg

    ELSE

      sat_zen = 90

    ENDIF

    IF (sat_zen > max_sat_zen) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, "Satellite zenith angle exceeds RTTOV maximum")
    ENDIF

    profiles(prof)%zenangle = sat_zen


    !--------------------------------------------------------------------------
    ! Calculate satellite azimuth angle for idealised GEO at latitude zero
    !--------------------------------------------------------------------------

    sin_lat = SIN(lat * deg2rad)
    sin_dlon = SIN((lon - geo_sat_lon) * deg2rad)

    IF (sin_dlon == 0._jprb .and. -sin_lat * cos_dlon == 0._jprb) THEN
      sat_azim = 360 ! nadir, azimith is undefined
    ELSE
      sat_azim = ATAN2(sin_dlon, -sin_lat * cos_dlon) * rad2deg
    ENDIF

    ! Modify azimuth angle to RTTOV definition (clockwise from N, due E=+90 deg)

    IF (sat_azim < 0) sat_azim = sat_azim + 360
    sat_azim = 360 - sat_azim

    profiles(prof)%azangle = sat_azim

  ENDDO

  CATCH

END SUBROUTINE rttov_calc_geo_sat_angles
