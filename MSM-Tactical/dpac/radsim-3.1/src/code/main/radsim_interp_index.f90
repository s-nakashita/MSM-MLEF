!-------------------------------------------------------------------------------
! Description:
!
!   Interpolate a model field to observation positions.
!
!   For regular lat/lon grids a standard bi-linear interpolation technique is
!   used. It is assumed that the observation position is within the model
!   domain.
!
!   For unstructured grids a simple nearest neighbour is used.
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

subroutine radsim_interp_index( &
  grid,        &
  lon_out,     &
  lat_out,     &
  grid_cell_x, &
  grid_cell_y, &
  mask         )

use radsim_mod_constants, only : &
  real32, &
  real64, &
  deg2rad

use radsim_mod_types, only : &
  grid_type

implicit none

type(grid_type),      intent(in)  :: grid
real(real32),         intent(in)  :: lon_out(:)
real(real32),         intent(in)  :: lat_out(:)
integer, allocatable, intent(out) :: grid_cell_x(:)
integer, allocatable, intent(out) :: grid_cell_y(:)
logical, optional,    intent(in)  :: mask(:)

integer :: i, j
integer :: ngrid, nobs
real(real32) :: lon, lat
real(real64) :: cosobslat, sinobslat
real(real64), allocatable :: cosgridlat(:), singridlat(:), cosalpha(:)
!-------------------------------------------------------------------------------

if ( grid % type == 101 ) then
  ngrid = size(grid % lat)
  allocate(cosgridlat(ngrid), singridlat(ngrid), cosalpha(ngrid))
  cosgridlat = cos(grid % lat * deg2rad)
  singridlat = sin(grid % lat * deg2rad)
else
  ngrid = grid % nrows * grid % ncols
end if
nobs = size(lon_out)
allocate(grid_cell_x(nobs))
allocate(grid_cell_y(nobs))
grid_cell_x = 0
grid_cell_y = 0

do i = 1, nobs

  ! Skip any masked points

  if ( present(mask) .and. mask(i) ) cycle

  !-----------------------------------------------------------------------------
  ! Determine grid cell
  !-----------------------------------------------------------------------------

  ! Vertices of the grid cell are model grid points. Grid cell 1 is defined as
  ! spanning point 1->2 along each axis. For a global model, the last grid cell
  ! spans (in the x-direction) points ncols->1.

  ! For grids with an initial reference point and uniform grid spacing, the
  ! grid cell can be calculated by comparing coordinates against the reference
  ! point.

  ! For other grids defined by vectors of lat,lon values we loop through the
  ! values until a match is found.

  lon = mod(lon_out(i), 360.)
  lat = lat_out(i)

  if ( grid % type == 101 ) then

    !----------------------------------------------
    ! Unstructured grid - find nearest neighbour
    !----------------------------------------------

    ! We can make no assumptions about the grid lat/lon order so we need to
    ! check all points for each obs

    ! We calculate the distance between the obs and each grid point assuming a
    ! spherical Earth of radius r. Let alpha be the central angle alpha between
    ! two points at longitude and latitude lon1, lat1 and lon2, lat2, then the
    ! great-circle distance d between them is given by:

    ! d = r.alpha
    ! cos(alpha) = sin(lat1).sin(lat2) + cos(lat1).cos(lat2).cos(lon1-lon2)

    ! Here we are only interested in finding the smallest distance i.e. the
    ! smallest alpha, which corresponds to the largest cos(alpha).

    ! The sin/cos of the grid latitudes are computed outside the obs loop for
    ! efficiency.

    cosobslat = cos(lat * deg2rad)
    sinobslat = sin(lat * deg2rad)
    cosalpha = sinobslat * singridlat + &
               cosobslat * cosgridlat * cos((lon - grid % lon) * deg2rad)
    grid_cell_x(i) = maxloc(cosalpha, 1)

  else if ( .not. associated(grid % lambda) ) then

    !----------------------------------------------
    ! Grid with reference point and uniform spacing
    !----------------------------------------------

    if ( lon < grid % lon1 ) then
      lon = lon + 360.0
    else if ( lon - grid % lon1 >= 360.0 ) then
      lon = lon - 360.0
    end if
    grid_cell_x(i) = int( (lon - grid % lon1) / grid % dlon ) + 1
    grid_cell_y(i) = int( (lat - grid % lat1) / grid % dlat ) + 1

  else

    !---------------------------------
    ! Grid with vectors of coordinates
    !---------------------------------

    ! There are no lon, lat increments in this case because it may be a
    ! non-uniform grid.

    ! Scan through lon, lat values until the correct grid cell is found. Note
    ! that the form of the test (< or >) changes according to grid direction.

    ! First check if the longitude values are in [-180 - 180) and if so adjust
    ! the lon value if necessary

    if ( minval(grid % lambda) < 0. ) then
      if ( lon >= 180. ) lon = lon - 360.
    end if

    if ( grid % xdir >= 0 ) then
      do j = 1, size(grid % lambda)
        if ( lon < grid % lambda(j) ) then
          grid_cell_x(i) = j-1
          exit
        end if
      end do
    else
      do j = 1, size(grid % lambda)
        if ( lon > grid % lambda(j) ) then
          grid_cell_x(i) = j-1
          exit
        end if
      end do
    end if

    if ( grid % ydir >= 0 ) then
      do j = 1, size(grid % phi)
        if ( lat < grid % phi(j) ) then
          grid_cell_y(i) = j-1
          exit
        end if
      end do
    else
      do j = 1, size(grid % phi)
        if ( lat > grid % phi(j) ) then
          grid_cell_y(i) = j-1
          exit
        end if
      end do
    end if

  end if

end do

if ( grid % type == 101 ) then
  deallocate(cosgridlat, singridlat, cosalpha)
end if

end subroutine radsim_interp_index
