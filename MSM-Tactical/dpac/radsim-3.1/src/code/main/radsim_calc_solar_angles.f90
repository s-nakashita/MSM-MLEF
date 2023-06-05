!-------------------------------------------------------------------------------
! Description:
!
!   Calculate solar zenith and azimuth angles for observations using lat/lon
!   and date/time.
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2018, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_calc_solar_angles(model_obs, obs)

use radsim_mod_constants, only : &
  real64, &
  deg2rad, &
  rad2deg, &
  output_default

use radsim_mod_types, only : &
  model_type, &
  obs_type

use radsim_mod_cfg, only : &
  output_mode, &
  enable_footprints

implicit none

! Subroutine arguments

type(model_type), intent(in)    :: model_obs
type(obs_type),   intent(inout) :: obs

! Local variables

integer :: i
integer :: hour, minute, datetime(5)
logical :: time_per_obs
real(real64) :: second
real(real64) :: eqoftime, sundeclination
real(real64) :: lat, lon
real(real64) :: timeoffset, truesolartime, hourangle
real(real64) :: sin_lat, cos_lat
real(real64) :: sin_dec, cos_dec
real(real64) :: sin_hour, cos_hour
real(real64) :: cos_theta
real(real64) :: tan_azim_num, tan_azim_den
real(real64) :: solarazim

!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! If obs data file already included angle data there is nothing to do here
!-------------------------------------------------------------------------------

if ( associated(obs % solzen) ) return

if ( output_mode >= output_default ) then
  print '(a)', 'Calculating solar zenith and azimuth angles'
end if

!-------------------------------------------------------------------------------
! Allocate solar zenith and azimuth arrays
!-------------------------------------------------------------------------------

allocate(obs % solzen(obs % nobs))
if ( .not. associated(obs % solazim) ) allocate(obs % solazim(obs % nobs))
obs % solzen  = 0.
obs % solazim = 0.

!-------------------------------------------------------------------------------
! Are dates/times specified per obs?
!-------------------------------------------------------------------------------

time_per_obs = (size(model_obs % validity_time, dim=1) > 1)

!-------------------------------------------------------------------------------
! Loop over observations and compute angles for each one
!-------------------------------------------------------------------------------

do i = 1, obs % nobs

  !-----------------------------------------------------------------------------
  ! Compute equation of time for each obs if obs_grid, or just once otherwise
  !-----------------------------------------------------------------------------

  if ( time_per_obs .or. i == 1 ) then
    if ( enable_footprints ) then
      ! For footprint simulations, model_obs is per simulated profile, but here
      ! we want the date/time per input observation because we are calculating
      ! the solar angles once per observation
      datetime = (/ obs % year(i), obs % month(i), obs % day(i), &
                    obs % hour(i), obs % minute(i) /)
      call equation_of_time(datetime, eqoftime, sundeclination)
      hour   = obs % hour(i)
      minute = obs % minute(i)
    else
      call equation_of_time(model_obs % validity_time(i,:), eqoftime, sundeclination)
      hour   = model_obs % validity_time(i,4)
      minute = model_obs % validity_time(i,5)
    end if

    second = 0.
  end if

  ! Ensure valid latitude and longitude

  if ( enable_footprints ) then
    ! As above, we want the lat/lon of the input obs, not the simulated profiles
    ! because we are calculating solar angles per obs
    lat = max(min(real(obs % lat(i), real64), 90._real64), -90._real64)
    lon = modulo(real(obs % lon(i), real64), 360._real64)
  else
    lat = max(min(real(model_obs % grid % lat(i), real64), 90._real64), -90._real64)
    lon = modulo(real(model_obs % grid % lon(i), real64), 360._real64)
  end if

  !-----------------------------------------------------------------------------
  ! Calculate solar zenith angle
  !-----------------------------------------------------------------------------

  ! Calculate true solar time and solar hour angle, apply time offset for the longitude

  timeoffset    = eqoftime + (4._real64 * lon)                                      ! in minutes
  truesolartime = (hour * 60._real64) + minute + (second / 60._real64) + timeoffset ! in minutes
  hourangle     = (truesolartime / 4._real64 - 180._real64) * deg2rad               ! in radians

  sin_lat  = sin(lat * deg2rad)
  cos_lat  = cos(lat * deg2rad)
  sin_dec  = sin(sundeclination)
  cos_dec  = cos(sundeclination)
  sin_hour = sin(hourangle)
  cos_hour = cos(hourangle)

  ! Cosine of zenith angle

  cos_theta = (sin_lat * sin_dec) + (cos_lat * cos_dec * cos_hour)
  if ( abs(cos_theta) > 1._real64 ) cycle

  ! Assign solar zenith angle in degrees

  obs % solzen(i) = acos(cos_theta) * rad2deg

  !-----------------------------------------------------------------------------
  ! Calculate solar azimuth angle
  !-----------------------------------------------------------------------------

  ! For RTTOV the azimuth angle is measured 0-360 degrees clockwise from 'north'
  ! (add 180 degrees to the result to achieve this)

  tan_azim_num = cos_dec * sin_hour
  tan_azim_den = (sin_lat * cos_dec * cos_hour) - (cos_lat * sin_dec)
  solarazim = atan2(tan_azim_num, tan_azim_den)        ! angle in radians from south
  obs % solazim(i) = solarazim * rad2deg + 180._real64 ! angle in degrees from north

end do

contains

  subroutine equation_of_time(datetime, eqoftime, sundeclination)

    ! Description:
    !
    !   Calculate the equation of time and sun declination for given date/time.
    !

    ! Subroutine arguments

    integer,      intent(in)  :: datetime(5)
    real(real64), intent(out) :: eqoftime
    real(real64), intent(out) :: sundeclination

    ! Local variables

    integer :: a, b, year, month, day, hour, minute
    real(real64) :: second
    real(real64) :: jd, jc
    real(real64) :: nseconds
    real(real64) :: meanobliqecliptic
    real(real64) :: omega
    real(real64) :: obliqecliptic_cor
    real(real64) :: geommeanlongsun
    real(real64) :: geommeananomalysun
    real(real64) :: ecc, z, c1, c2, c3, c4, c5
    real(real64) :: eqofcentre
    real(real64) :: sunapparentlong
    real(real64) :: suntruelong
    real(real64) :: sin_sundec
    real(real64) :: mrad, sinm, sin2m, sin3m

    !---------------------------------------------------------------------------

    year   = datetime(1)
    month  = datetime(2)
    day    = datetime(3)
    hour   = datetime(4)
    minute = datetime(5)
    second = 0.

    ! For Julian day
    if ( month <= 2 ) then
      year  = year - 1
      month = month + 12
    end if

    !---------------------------------------------------------------------------
    ! Calculate the Julian century
    !---------------------------------------------------------------------------

    a = year / 100
    b = 2 - a + (a / 4)

    ! Julian day

    jd = int(365.25_real64 * (year + 4716)) + &
         int(30.6001_real64 * (month + 1)) + day + b - 1524.5_real64

    ! Add on day fraction

    jd = jd + (hour * 3600._real64 + minute * 60._real64 + second) / 86400._real64

    ! Convert to Julian century (365.25 days) relative to the year 2000

    jc = (jd - 2451545._real64) / 36525._real64

    !---------------------------------------------------------------------------
    ! Calculate the mean obliquity of the ecliptic
    ! and the correction to the obliquity of the ecliptic
    !---------------------------------------------------------------------------

    nseconds = 21.448_real64 - &
        jc * (46.8150_real64 + jc * (0.00059_real64 - jc * 0.001813_real64))
    meanobliqecliptic = 23._real64 + &
        (26._real64 + (nseconds / 60._real64)) / 60._real64

    omega = 125.04_real64 - 1934.136_real64 * jc
    obliqecliptic_cor = meanobliqecliptic + 0.00256_real64 * cos(omega * deg2rad)

    !---------------------------------------------------------------------------
    ! Calculate the geometric mean longitude of the sun
    !---------------------------------------------------------------------------

    geommeanlongsun = 280.46646_real64 + &
        jc * (36000.76983_real64 + 0.0003032_real64 * jc)
    ! ensure longitude is between 0 and 360 degrees
    geommeanlongsun = modulo(geommeanlongsun, 360._real64)

    !---------------------------------------------------------------------------
    ! Calculate the mean anomaly
    !---------------------------------------------------------------------------

    geommeananomalysun = 357.52911_real64 + &
        jc * (35999.05029_real64 - 0.0001537_real64 * jc)

    !---------------------------------------------------------------------------
    ! Calculate the eccentricity of the earths orbit
    !---------------------------------------------------------------------------

    ecc = 0.016708634_real64 - &
        jc * (0.000042037_real64 + 0.0000001267_real64 * jc)

    !---------------------------------------------------------------------------
    ! Calculate the equation of time
    !---------------------------------------------------------------------------

    mrad  = geommeananomalysun * deg2rad
    sinm  = sin(mrad)
    sin2m = sin(2._real64 * mrad)
    sin3m = sin(3._real64 * mrad)

    z = tan(0.5_real64 * obliqecliptic_cor * deg2rad)
    z = z * z
    c1 = sin(2._real64 * geommeanlongsun * deg2rad)
    c2 = sinm
    c3 = cos(2._real64 * geommeanlongsun * deg2rad)
    c4 = 2._real64 * c1 * c3
    c5 = sin2m
    eqoftime = (z * c1) - (2._real64 * ecc * c2) + &
               (4._real64 * ecc * z * c2 * c3) - &
               (0.5_real64 * z * z * c4) - (1.25_real64 * ecc * ecc * c5)
    eqoftime = eqoftime * rad2deg * 4._real64

    !---------------------------------------------------------------------------
    ! Calculate the solar declination
    !---------------------------------------------------------------------------

    ! Equation of centre

    eqofcentre = &
        sinm * (1.914602_real64 - &
                jc * (0.004817_real64 + 0.000014_real64 * jc)) + &
        sin2m * (0.019993_real64 - 0.000101_real64 * jc) + &
        sin3m * 0.000289_real64

    ! Sun true longitude

    suntruelong = geommeanlongsun + eqofcentre

    ! Apparent longitude of the sun

    omega = 125.04_real64 - 1934.136_real64 * jc
    sunapparentlong = suntruelong - 0.00569_real64 - &
                      0.00478_real64 * sin(omega * deg2rad)

    ! Solar declination

    sin_sundec = sin(obliqecliptic_cor * deg2rad) * &
                 sin(sunapparentlong * deg2rad)
    sundeclination = asin(sin_sundec)

  end subroutine equation_of_time

end subroutine radsim_calc_solar_angles
