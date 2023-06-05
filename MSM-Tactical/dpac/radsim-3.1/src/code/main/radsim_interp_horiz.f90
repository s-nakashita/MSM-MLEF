!-------------------------------------------------------------------------------
! Description:
!
!   Interpolate a model field to observation positions. A standard bi-linear
!   interpolation technique is used. It is assumed that the observation
!   position is within the model domain.
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

subroutine radsim_interp_horiz( &
  MDI,         &
  grid,        &
  field_in,    &
  lon_out,     &
  lat_out,     &
  grid_cell_x, &
  grid_cell_y, &
  model_index, &
  t_weight,    &
  field_out,   &
  mask         )

use radsim_mod_constants, only : &
  real32, &
  real64

use radsim_mod_types, only : &
  grid_type

use radsim_mod_cfg, only : &
  temporal_data

implicit none

real(real64),      intent(in)  :: MDI
type(grid_type),   intent(in)  :: grid
real(real64),      intent(in)  :: field_in(:,:)
real(real32),      intent(in)  :: lon_out(:)
real(real32),      intent(in)  :: lat_out(:)
integer,           intent(in)  :: grid_cell_x(:)
integer,           intent(in)  :: grid_cell_y(:)
integer,           intent(in)  :: model_index(:)
real(real64),      intent(in)  :: t_weight(:)
real(real64),      intent(out) :: field_out(:)
logical, optional, intent(in)  :: mask(:)

integer :: i, j, it, nt
integer :: cell_no
integer :: cell_dx
integer :: cell_dy
real(real32) :: xdiff
real(real32) :: ydiff
real(real64) :: x_weight
real(real64) :: y_weight
real(real64) :: t_w
real(real64) :: cell_values(4)
real(real32) :: lon
real(real32) :: lat
real(real32) :: dlon
real(real32) :: dlat

!-------------------------------------------------------------------------------

nt = 1
if ( temporal_data ) nt = 2

do i = 1, size(lon_out)

  ! Skip any masked points

  if ( present(mask) .and. mask(i) ) then
    field_out(i) = MDI
    cycle
  end if

  ! Skip any invalid points. There are n-1 cells along each axis except for a
  ! global model grid where the x (longitude axis) wraps around and therefore
  ! has n cells. However we do want to include points on the boundary if fields
  ! are for some regional area.

  if ( grid_cell_x(i) <= 0 .or. grid_cell_x(i) > grid % ncols .or. &
       grid_cell_y(i) <= 0 .or. grid_cell_y(i) > grid % nrows      ) then
    field_out(i) = MDI
    cycle
  end if

  ! Set cell parameters depending on grid orientation

  if ( grid % row_order ) then
    cell_no = grid % ncols * (grid_cell_y(i)-1) + grid_cell_x(i)
    cell_dx = 1
    cell_dy = grid % ncols
  else
    cell_no = grid % nrows * (grid_cell_x(i)-1) + grid_cell_y(i)
    cell_dx = grid % nrows
    cell_dy = 1
  end if

  ! Set or calculate grid spacing. This may be fixed or else should be
  ! calculated for each point. Account for points on the boundary for variable
  ! grids.

  if ( .not. associated(grid % lambda) ) then
    dlon = grid % dlon
    dlat = grid % dlat
  else
    if ( cell_no + cell_dx > size(grid % lon) ) then
      dlon = grid % lon(cell_no-cell_dx) - grid % lon(cell_no)
    else
      dlon = grid % lon(cell_no+cell_dx) - grid % lon(cell_no)
    end if

    if ( cell_no + cell_dy > size(grid % lat) ) then
      dlat = grid % lat(cell_no-cell_dy) - grid % lat(cell_no)
    else
      dlat = grid % lat(cell_no+cell_dy) - grid % lat(cell_no)
    end if
  end if

  dlon = abs(dlon)
  dlat = abs(dlat)

  ! Calculate weights. Make sure they are in the range 0-1. Rounding error can
  ! sometimes result in them being outside the range with knock-on effects such
  ! as negative values for quantities that cannot be negative.

  lon = lon_out(i)
  lat = lat_out(i)

  xdiff = abs(lon - grid % lon(cell_no))
  ydiff = abs(lat - grid % lat(cell_no))
  if ( xdiff >= dlon ) xdiff = xdiff - 360.0

  x_weight = min(max(xdiff / dlon, 0.0), 1.0)
  y_weight = min(max(ydiff / dlat, 0.0), 1.0)

  field_out(i) = 0.0_real64

  ! Be careful of boundaries: if we are on the latitude boundary we want to
  ! include the point, but not go beyond the model domain. Note that for
  ! longitudes, we either wrap around for global fields, or for regional fields
  ! if we are on the boundary, the x_weight takes care of the interpolation.
  if ( grid_cell_y(i) == grid % nrows ) cell_dy = 0

  do j = 1, nt
    it = model_index(i) + j - 1

    ! Set field values at grid-cell vertices, being careful at the x boundaries
    ! for global fields with wrapped longitude

    cell_values(1) = field_in(cell_no,it)                   ! x,y
    cell_values(3) = field_in(cell_no+cell_dy,it)           ! x,y+dy

    ! The clauses for grid_cell_x == ncols handle the wrap-around for global
    ! fields. For a regional field, if we are at ncols then x_weight == 0 so the
    ! cell_values(2,4) are ignored below in the interpolation as required. We
    ! must also be careful to avoid going outside the field_in array bounds.
    if ( grid_cell_x(i) < grid % ncols ) then
      cell_values(2) = field_in(cell_no+cell_dx,it)         ! x+dx,y
      cell_values(4) = field_in(cell_no+cell_dx+cell_dy,it) ! x+dx,y+dy
    else if ( grid % row_order ) then
      cell_values(2) = field_in(cell_no-grid % ncols+1,it)  ! x+dx,y
      if ( grid_cell_y(i) == grid % nrows ) then
        cell_values(4) = cell_values(2)                     ! x+dx,y+dy
      else
        cell_values(4) = field_in(cell_no+1,it)             ! x+dx,y+dy
      end if
    else
      cell_values(2) = field_in(grid_cell_y(i),it)          ! x+dx,y
      cell_values(4) = field_in(grid_cell_y(i)+cell_dy,it)  ! x+dx,y+dy
    end if

    ! Calculate weighted field value

    if ( j == 1 ) then
      t_w = t_weight(i)
    else
      t_w = 1.0_real64 - t_weight(i)
    end if

    field_out(i) = field_out(i) + t_w * &
      ((1.0_real64-x_weight) * (1.0_real64-y_weight) * cell_values(1) + &
                   x_weight  * (1.0_real64-y_weight) * cell_values(2) + &
       (1.0_real64-x_weight) * y_weight              * cell_values(3) + &
                   x_weight  * y_weight              * cell_values(4))
  end do

end do

end subroutine radsim_interp_horiz
