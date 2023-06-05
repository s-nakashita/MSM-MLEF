! Description:
!> @file
!!   Calculate solar zenith and azimuth angles in supplied array of
!!   profiles given populated date, time, lat, lon.
!
!> @brief
!!   Calculate solar zenith and azimuth angles in supplied array of
!!   profiles given populated date, time, lat, lon.
!!
!! @details
!!   When carrying out solar simulations, you can populate your profile
!!   structures including date, time, lat, and lon, and then call this
!!   subroutine to calculate/populate the solar zenith and azimuth angles.
!!
!!
!! @param[out]    err            status on exit
!! @param[inout]  profiles       array of profile structures
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
SUBROUTINE rttov_calc_solar_angles(err, profiles)

  USE parkind1, ONLY : jpim
  USE rttov_types, ONLY : rttov_profile
!INTF_OFF
#include "throw.h"
  USE parkind1, ONLY : jprb
  USE rttov_const, ONLY : deg2rad, rad2deg
!INTF_ON

  IMPLICIT NONE

  INTEGER(jpim),       INTENT(OUT)   :: err
  TYPE(rttov_profile), INTENT(INOUT) :: profiles(:)
!INTF_END

#include "rttov_errorreport.interface"

  INTEGER(jpim) :: prof
  INTEGER(jpim) :: hour, minute, second
  REAL(jprb)    :: eqoftime, sundeclination
  REAL(jprb)    :: lon
  REAL(jprb)    :: timeoffset, truesolartime, hourangle
  REAL(jprb)    :: sin_lat, cos_lat
  REAL(jprb)    :: sin_dec, cos_dec
  REAL(jprb)    :: sin_hour, cos_hour
  REAL(jprb)    :: cos_theta
  REAL(jprb)    :: tan_azim_num, tan_azim_den
  REAL(jprb)    :: solarazim

  TRY

  DO prof = 1, SIZE(profiles)

    !---------------------------------------------------------------------------
    ! Compute equation of time 
    !---------------------------------------------------------------------------

    CALL equation_of_time(profiles(prof)%date, &
                          profiles(prof)%time, &
                          eqoftime, sundeclination)
    hour   = profiles(prof)%time(1)
    minute = profiles(prof)%time(2)
    second = profiles(prof)%time(3)

    ! Ensure valid latitude and longitude

    IF (profiles(prof)%latitude < -90._jprb .OR. &
        profiles(prof)%latitude > 90._jprb) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Latitude must be in [-90, +90] degrees')
    ENDIF
    lon = MODULO(profiles(prof)%longitude, 360._jprb)

    !---------------------------------------------------------------------------
    ! Calculate solar zenith angle
    !---------------------------------------------------------------------------

    ! Calculate true solar time and solar hour angle, apply time offset for the longitude

    timeoffset    = eqoftime + (4._jprb * lon)                                    ! in minutes
    truesolartime = (hour * 60._jprb) + minute + (second / 60._jprb) + timeoffset ! in minutes
    hourangle     = (truesolartime / 4._jprb - 180._jprb) * deg2rad               ! in radians

    sin_lat  = SIN(profiles(prof)%latitude * deg2rad)
    cos_lat  = COS(profiles(prof)%latitude * deg2rad)
    sin_dec  = SIN(sundeclination)
    cos_dec  = COS(sundeclination)
    sin_hour = SIN(hourangle)
    cos_hour = COS(hourangle)

    ! Cosine of zenith angle

    cos_theta = (sin_lat * sin_dec) + (cos_lat * cos_dec * cos_hour)
    IF ( ABS(cos_theta) > 1._jprb ) CYCLE

    ! Assign solar zenith angle in degrees

    profiles(prof)%sunzenangle = ACOS(cos_theta) * rad2deg

    !---------------------------------------------------------------------------
    ! Calculate solar azimuth angle
    !---------------------------------------------------------------------------

    ! For RTTOV the azimuth angle is measured 0-360 degrees clockwise from 'north'
    ! (add 180 degrees to the result to achieve this)

    tan_azim_num = cos_dec * sin_hour
    tan_azim_den = (sin_lat * cos_dec * cos_hour) - (cos_lat * sin_dec)
    solarazim = ATAN2(tan_azim_num, tan_azim_den)               ! angle in radians from south
    profiles(prof)%sunazangle = solarazim * rad2deg + 180._jprb ! angle in degrees from north

  ENDDO

  CATCH

CONTAINS

  SUBROUTINE equation_of_time(date, time, eqoftime, sundeclination)

    ! Description:
    !
    !   Calculate the equation of time and sun declination for given date/time.
    !

    ! Subroutine arguments

    INTEGER(jpim), INTENT(IN)  :: date(3)
    INTEGER(jpim), INTENT(IN)  :: time(3)
    REAL(jprb),    INTENT(OUT) :: eqoftime
    REAL(jprb),    INTENT(OUT) :: sundeclination

    ! Local variables

    INTEGER(jpim) :: a, b, year, month, day, hour, minute, second
    REAL(jprb)    :: jd, jc
    REAL(jprb)    :: nseconds
    REAL(jprb)    :: meanobliqecliptic
    REAL(jprb)    :: omega
    REAL(jprb)    :: obliqecliptic_cor
    REAL(jprb)    :: geommeanlongsun
    REAL(jprb)    :: geommeananomalysun
    REAL(jprb)    :: ecc, z, c1, c2, c3, c4, c5
    REAL(jprb)    :: eqofcentre
    REAL(jprb)    :: sunapparentlong
    REAL(jprb)    :: suntruelong
    REAL(jprb)    :: sin_sundec
    REAL(jprb)    :: mrad, sinm, sin2m, sin3m

    !---------------------------------------------------------------------------

    year   = date(1)
    month  = date(2)
    day    = date(3)
    hour   = time(1)
    minute = time(2)
    second = time(3)

    ! For Julian day
    IF ( month <= 2 ) THEN
      year  = year - 1
      month = month + 12
    ENDIF

    !---------------------------------------------------------------------------
    ! Calculate the Julian century
    !---------------------------------------------------------------------------

    a = year / 100
    b = 2 - a + (a / 4)

    ! Julian day

    jd = int(365.25_jprb * (year + 4716)) + &
         int(30.6001_jprb * (month + 1)) + day + b - 1524.5_jprb

    ! Add on day fraction

    jd = jd + (hour * 3600._jprb + minute * 60._jprb + second) / 86400._jprb

    ! Convert to Julian century (365.25 days) relative to the year 2000

    jc = (jd - 2451545._jprb) / 36525._jprb

    !---------------------------------------------------------------------------
    ! Calculate the mean obliquity of the ecliptic
    ! and the correction to the obliquity of the ecliptic
    !---------------------------------------------------------------------------

    nseconds = 21.448_jprb - &
        jc * (46.8150_jprb + jc * (0.00059_jprb - jc * 0.001813_jprb))
    meanobliqecliptic = 23._jprb + &
        (26._jprb + (nseconds / 60._jprb)) / 60._jprb

    omega = 125.04_jprb - 1934.136_jprb * jc
    obliqecliptic_cor = meanobliqecliptic + 0.00256_jprb * COS(omega * deg2rad)

    !---------------------------------------------------------------------------
    ! Calculate the geometric mean longitude of the sun
    !---------------------------------------------------------------------------

    geommeanlongsun = 280.46646_jprb + &
        jc * (36000.76983_jprb + 0.0003032_jprb * jc)
    ! ensure longitude is between 0 and 360 degrees
    geommeanlongsun = MODULO(geommeanlongsun, 360._jprb)

    !---------------------------------------------------------------------------
    ! Calculate the mean anomaly
    !---------------------------------------------------------------------------

    geommeananomalysun = 357.52911_jprb + &
        jc * (35999.05029_jprb - 0.0001537_jprb * jc)

    !---------------------------------------------------------------------------
    ! Calculate the eccentricity of the earths orbit
    !---------------------------------------------------------------------------

    ecc = 0.016708634_jprb - &
        jc * (0.000042037_jprb + 0.0000001267_jprb * jc)

    !---------------------------------------------------------------------------
    ! Calculate the equation of time
    !---------------------------------------------------------------------------

    mrad  = geommeananomalysun * deg2rad
    sinm  = SIN(mrad)
    sin2m = SIN(2._jprb * mrad)
    sin3m = SIN(3._jprb * mrad)

    z = TAN(0.5_jprb * obliqecliptic_cor * deg2rad)
    z = z * z
    c1 = SIN(2._jprb * geommeanlongsun * deg2rad)
    c2 = sinm
    c3 = COS(2._jprb * geommeanlongsun * deg2rad)
    c4 = 2._jprb * c1 * c3
    c5 = sin2m
    eqoftime = (z * c1) - (2._jprb * ecc * c2) + &
               (4._jprb * ecc * z * c2 * c3) - &
               (0.5_jprb * z * z * c4) - (1.25_jprb * ecc * ecc * c5)
    eqoftime = eqoftime * rad2deg * 4._jprb

    !---------------------------------------------------------------------------
    ! Calculate the solar declination
    !---------------------------------------------------------------------------

    ! Equation of centre

    eqofcentre = &
        sinm * (1.914602_jprb - &
                jc * (0.004817_jprb + 0.000014_jprb * jc)) + &
        sin2m * (0.019993_jprb - 0.000101_jprb * jc) + &
        sin3m * 0.000289_jprb

    ! Sun true longitude

    suntruelong = geommeanlongsun + eqofcentre

    ! Apparent longitude of the sun

    omega = 125.04_jprb - 1934.136_jprb * jc
    sunapparentlong = suntruelong - 0.00569_jprb - &
                      0.00478_jprb * SIN(omega * deg2rad)

    ! Solar declination

    sin_sundec = SIN(obliqecliptic_cor * deg2rad) * &
                 SIN(sunapparentlong * deg2rad)
    sundeclination = ASIN(sin_sundec)

  END SUBROUTINE equation_of_time

END SUBROUTINE rttov_calc_solar_angles
