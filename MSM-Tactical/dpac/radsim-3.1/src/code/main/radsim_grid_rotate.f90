!-------------------------------------------------------------------------------
! Description:
!
!   Rotate a set of lat,lon coordinates. This is to be used when a regular grid
!   is defined with a rotated pole. To convert from rotated coordinates back to
!   standard lat,lon coordinates set the optional argument back=.true. (this is
!   the default if absent). To transform from standard -> rotated coordinates
!   set back=.false.
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

subroutine radsim_grid_rotate(pole_lat, pole_lon, lat, lon, back)

use radsim_mod_constants, only : &
  real32, &
  real64, &
  pi, &
  deg2rad, &
  rad2deg

implicit none

! Subroutine arguments

real,              intent(in)    :: pole_lat
real,              intent(in)    :: pole_lon
real(real32),      intent(inout) :: lat(:)
real(real32),      intent(inout) :: lon(:)
logical, optional, intent(in)    :: back

! Local variables

real(real64) :: theta_y, theta_z
real(real64) :: phi(size(lat))
real(real64) :: phi_in(size(lat))
real(real64) :: lambda(size(lon))
integer :: i
integer :: order(2)

!-------------------------------------------------------------------------------
! 1. Set up transformation variables
!-------------------------------------------------------------------------------

! Set rotation angles and transformation order. The 'back' argument is to
! convert rotated coordinates back to standard lat-lon coordinates.

if ( .not. present(back) .or. back ) then
  order = (/2,1/)
  theta_y = -(90.0-pole_lat) * deg2rad
  theta_z = -(pole_lon + 180.0) * deg2rad
else
  order = (/1,2/)
  theta_y = (90.0-pole_lat) * deg2rad
  theta_z = (pole_lon + 180.0) * deg2rad
end if

! Convert input coordinates to radians

phi_in = lat * deg2rad
lambda = lon * deg2rad

!-------------------------------------------------------------------------------
! 2. Apply transformations
!-------------------------------------------------------------------------------

! Results appear in the working coordinate arrays.

do i = 1, 2

  ! Set working longitude coordinates to be in the range -pi -> pi as this is
  ! the range of the acos function.

  where(lambda < -pi)
    lambda = lambda + 2*pi
  elsewhere(lambda > pi)
    lambda = lambda - 2*pi
  end where

  if ( order(i) == 1 ) then

    ! Apply z-axis rotation

    lambda = lambda - theta_z

  else

    ! Apply y-axis rotation

    phi = asin(sin(phi_in)*cos(theta_y) - cos(lambda)*cos(phi_in)*sin(theta_y))
    lambda = sign(1.0_8,lambda) * &
      acos((cos(lambda)*cos(phi_in)*cos(theta_y) + sin(phi_in)*sin(theta_y))/cos(phi))

  end if

end do

!-------------------------------------------------------------------------------
! 3. Set output arrays
!-------------------------------------------------------------------------------

! Convert to degrees

lat = phi * rad2deg
lon = lambda * rad2deg

! Make sure longitude is in the correct range

where(lon < 0.0)
  lon = lon + 360.0
elsewhere(lon > 360.0)
  lon = lon - 360.0
end where

end subroutine radsim_grid_rotate
