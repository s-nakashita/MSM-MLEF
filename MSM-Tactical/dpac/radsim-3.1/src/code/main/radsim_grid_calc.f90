!-------------------------------------------------------------------------------
! Description:
!
!   Calculate coordinates (in degrees) of a regular lat-lon grid (may be known
!   as a cylindrical, equirectangular or Plate-Caree projection).
!
!   Output longitude is in the range 0->360 degrees.
!
!   Grid coordinates are stored as 1D-arrays to be consistent with model fields.
!   They may be in either row-major-order or column-major-order. If a grid is
!   in row-major-order the grid points traverse each row in turn, i.e., all the
!   columns in row1, followed by all the columns in row2 etc.
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

subroutine radsim_grid_calc(grid)

use radsim_mod_types, only : &
  grid_type

implicit none

type(grid_type), intent(inout) :: grid

integer :: i, j, nprofs
real :: lambda(grid % ncols)  ! longitude
real :: phi(grid % nrows)     ! latitude

!-------------------------------------------------------------------------------

! Use existing row vector if defined, else calculate

if ( associated(grid % lambda) ) then
  lambda = grid % lambda
  grid % xdir = nint(sign(1.0, lambda(2) - lambda(1)))
else
  lambda = grid % lon1 + (/ (j*grid % dlon, j=0, grid % ncols-1) /)
  grid % xdir = nint(sign(1.0, grid % dlon))
end if

! Use existing column vector if defined, else calculate

if ( associated(grid % phi) ) then
  phi = grid % phi
  grid % ydir = nint(sign(1.0, phi(2) - phi(1)))
else
  phi = grid % lat1 + (/ (j*grid % dlat, j=0, grid % nrows-1) /)
  grid % ydir = nint(sign(1.0, grid % dlat))
end if

! Allocate space for grid values

if (.not. associated(grid % lon)) allocate(grid % lon(grid % ncols*grid % nrows))
if (.not. associated(grid % lat)) allocate(grid % lat(grid % ncols*grid % nrows))

! Spread row and column values across the grid, according to the specified order
! (row order is row-major-order in matrix terminology).

nprofs = 0
if ( grid % row_order ) then
  do i = 1, grid % nrows
    grid % lon(nprofs+1:nprofs+grid % ncols) = lambda(1:grid % ncols)
    grid % lat(nprofs+1:nprofs+grid % ncols) = phi(i)
    nprofs = nprofs + grid % ncols
  end do
else
  do i = 1, grid % ncols
    grid % lon(nprofs+1:nprofs+grid % nrows) = lambda(i)
    grid % lat(nprofs+1:nprofs+grid % nrows) = phi(1:grid % ncols)
    nprofs = nprofs + grid % nrows
  end do
end if

! Set longitude to be in range 0->360

where( grid % lon < 0.0 )
  grid % lon = grid % lon + 360.0
elsewhere( grid % lon > 360.0 )
  grid % lon = grid % lon - 360.0
end where

end subroutine radsim_grid_calc
