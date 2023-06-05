!-------------------------------------------------------------------------------
! Description:
!
!   Interpolate a model field to observation positions for unstructured grids.
!   A simple nearest neighbour technique is used. This may be improved in the
!   future.
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2019, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_interp_unstructured( &
  MDI,         &
  field_in,    &
  nobs,        &
  grid_cell_x, &
  model_index, &
  t_weight,    &
  field_out,   &
  mask         )

use radsim_mod_constants, only : &
  real32, &
  real64

use radsim_mod_cfg, only : &
  temporal_data

implicit none

real(real64),      intent(in)  :: MDI
real(real64),      intent(in)  :: field_in(:,:)
integer,           intent(in)  :: nobs
integer,           intent(in)  :: grid_cell_x(:)
integer,           intent(in)  :: model_index(:)
real(real64),      intent(in)  :: t_weight(:)
real(real64),      intent(out) :: field_out(:)
logical, optional, intent(in)  :: mask(:)

integer :: i, j, it, nt
integer :: cell_no
real(real64) :: t_w

!-------------------------------------------------------------------------------

nt = 1
if ( temporal_data ) nt = 2

do i = 1, nobs

  ! Skip any masked points

  if ( present(mask) .and. mask(i) ) then
    field_out(i) = MDI
    cycle
  end if

  ! Skip any invalid points

  if ( grid_cell_x(i) <= 0 ) then
    field_out(i) = MDI
    cycle
  end if

  ! Interpolation is simple: grid_cell_x(i) contains the index of the nearest
  ! grid point

  cell_no = grid_cell_x(i)

  field_out(i) = 0.0_real64

  do j = 1, nt
    it = model_index(i) + j - 1

    ! Calculate weighted field value

    if ( j == 1 ) then
      t_w = t_weight(i)
    else
      t_w = 1.0_real64 - t_weight(i)
    end if

    field_out(i) = field_out(i) + t_w * field_in(cell_no,it)
  end do

end do

end subroutine radsim_interp_unstructured
