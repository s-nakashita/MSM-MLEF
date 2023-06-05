!-------------------------------------------------------------------------------
! Description:
!
!   Calculate geostationary satellite zenith and azimuth angles for observations
!   user-specified sub-satellite lat/lon and satellite height.
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2022, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_calc_geo_sat_angles(coeffs, model, obs)

use radsim_mod_constants, only : &
  real32, &
  real64, &
  deg2rad, &
  rad2deg, &
  r_earth, &
  output_default, &
  qcflag_sat_zen_angle

use radsim_mod_types, only : &
  model_type, &
  obs_type

use radsim_mod_cfg, only : &
  output_mode, &
  obs_grid, &
  calc_geo_sat_angles, &
  geo_sat_height, &
  geo_sat_lat, &
  geo_sat_lon, &
  htfrtc

use rttov_const, only : &
  zenmax, &
  zenmaxv9

use rttov_types, only : &
  rttov_coefs

implicit none

! Subroutine arguments

type(rttov_coefs), intent(in)    :: coeffs
type(model_type),  intent(in)    :: model
type(obs_type),    intent(inout) :: obs

! Local variables

integer :: i
logical :: calc_zen, calc_azim
real(real32), pointer :: obs_lat(:), obs_lon(:)
real(real64) :: max_sat_zen, r_ratio
real(real64) :: lat, lon
real(real64) :: cos_dlon, sin_dlon, sin_lat
real(real64) :: cos_alpha, sin_alpha
real(real64) :: tan_satzen_denom, tan_satzenithang
real(real64) :: sat_zen, sat_azim

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Allocate satellite zenith and azimuth arrays
!-------------------------------------------------------------------------------

! Don't overwrite any existing angle data

calc_zen = .not. associated(obs % satzen)
if ( calc_zen ) then
  allocate(obs % satzen(obs % nobs))
  obs % satzen  = 0.
end if

calc_azim = .not. associated(obs % satazim)
if ( calc_azim ) then
  allocate(obs % satazim(obs % nobs))
  obs % satazim = 0.
end if

if ( .not. (calc_zen .or. calc_azim) ) return

if ( output_mode >= output_default ) then
  print '(a)', 'Calculating geostationary satellite zenith and/or azimuth angles'
end if

!-------------------------------------------------------------------------------
! Prepare data
!-------------------------------------------------------------------------------

! Take latitudes and longitudes from obs data or from model grid
if ( obs_grid ) then
  obs_lat => obs % lat
  obs_lon => obs % lon
else
  obs_lat => model % grid % lat
  obs_lon => model % grid % lon
end if

! Ratio of Earth radius to Earth radius plus satellite height
r_ratio = r_earth / (geo_sat_height + r_earth)

! RTTOV's maximum zenith angle depends on the coefficient file
if ( htfrtc .or. coeffs % coef % fmv_model_ver >= 9 ) then
  max_sat_zen = zenmaxv9
else
  max_sat_zen = zenmax
endif

! Use a slightly smaller value to avoid problems related to single vs double precision
max_sat_zen = max_sat_zen * (1._real64 - 1.e-5_real64)

! If not already allocated, we need the qcflags array
if ( .not. associated(obs % qcflags) ) then
  allocate(obs % qcflags(obs % nobs))
  obs % qcflags = 0
end if

!-------------------------------------------------------------------------------
! Loop over observations and compute angles for each one
!-------------------------------------------------------------------------------

do i = 1, obs % nobs

  lat = max(min(real(obs_lat(i), real64), 90._real64), -90._real64)
  lon = modulo(real(obs_lon(i), real64), 360._real64)

  cos_dlon = cos((lon - geo_sat_lon) * deg2rad)

  !-----------------------------------------------------------------------------
  ! Calculate satellite zenith angle
  !-----------------------------------------------------------------------------

  if ( calc_zen ) then

    cos_alpha = sin(lat * deg2rad) * sin(geo_sat_lat * deg2rad) + &
                cos(lat * deg2rad) * cos(geo_sat_lat * deg2rad) * cos_dlon

    tan_satzen_denom = cos_alpha - r_ratio

    ! Where tan_satzen_denom > 0 locations are above the horizon (zenith angles < 90 degrees)

    if ( tan_satzen_denom > 0 ) then

      ! Zenith angle calculation

      sin_alpha = sqrt(1. - cos_alpha * cos_alpha)
      tan_satzenithang = sin_alpha / tan_satzen_denom
      sat_zen = atan(tan_satzenithang) * rad2deg

    else

      sat_zen = 90

    end if

    obs % satzen(i) = sat_zen

    ! Set qcflag for cases where sat_zen exceeds RTTOV's maximum
    if ( sat_zen >= max_sat_zen ) then
      obs % qcflags(i) = ibset(obs % qcflags(i), qcflag_sat_zen_angle)
    end if

  end if

  !-----------------------------------------------------------------------------
  ! Calculate satellite azimuth angle for idealised GEO at latitude zero
  !-----------------------------------------------------------------------------

  if ( calc_azim .and. obs % satzen(i) < max_sat_zen ) then
  
    sin_lat = sin(lat * deg2rad)
    sin_dlon = sin((lon - geo_sat_lon) * deg2rad)
    sat_azim = atan2(sin_dlon, -sin_lat * cos_dlon) * rad2deg

    ! Modify azimuth angle to RTTOV definition (clockwise from N, due E=+90 deg)

    if ( sat_azim < 0 ) sat_azim = sat_azim + 360
    sat_azim = 360 - sat_azim

    obs % satazim(i) = sat_azim

  end if

end do

end subroutine radsim_calc_geo_sat_angles
