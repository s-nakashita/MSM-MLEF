!-------------------------------------------------------------------------------
! Description:
!
!   Calculate pressure levels for the Met Office Unified Model.
!
!   The method is as follows:
!
!   1) Calculate level heights for both rho and theta-levels
!
!      Level heights are a linear function of surface height. The coefficients
!      (usually found in fieldsfile or PP file headers) are supplied as
!      subroutine arguments.
!
!   2) Linearly interpolate exner pressure for rho-levels with height
!   3) Calculate theta-level pressure from exner pressure
!
! Reference:
!
!   Unified Model Documentation Paper F3, Met Office internal publication.
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

subroutine radsim_calc_meto_plevels( &
  zsea_theta, &
  zsea_rho, &
  c_theta, &
  c_rho, &
  model)

use radsim_mod_cfg, only : &
  output_mode

use radsim_mod_constants, only : &
  real64, &
  kappa, &
  p0, &
  status_error, &
  output_verbose, &
  output_debug

use radsim_mod_types, only : &
  model_type

implicit none

include 'radsim_error_report.interface'

real(real64),     intent(in)    :: zsea_theta(:)
real(real64),     intent(in)    :: zsea_rho(:)
real(real64),     intent(in)    :: c_theta(:)
real(real64),     intent(in)    :: c_rho(:)
type(model_type), intent(inout) :: model

integer :: i, j
integer :: nlevels, nlevels_rho
real(real64), allocatable :: z_theta(:)
real(real64), allocatable :: z_rho(:)
real(real64), allocatable :: exner(:)

!-------------------------------------------------------------------------------

if ( .not. associated(model % p) .and. associated(model % ph) ) then

  print '(a)', 'Converting rho-level pressures to theta-level pressures'

  nlevels_rho = size(model % ph, dim=2)
  if ( associated(model % theta) ) then
    nlevels = size(model % theta, dim=2)
  else
    nlevels = size(model % t, dim=2)
  end if

  if ( output_mode >= output_verbose ) then
    print '(a,i0)', ' Number of rho levels = ', nlevels_rho
    print '(a,i0)', ' Number of theta levels = ', nlevels
  end if

  if ( nlevels_rho == nlevels ) then
    call radsim_error_report( &
      'Number of rho-levels is the same as theta-levels', &
      status_error)
  end if

  allocate(model % z(model%nprofs, nlevels))
  allocate(model % p(model%nprofs, nlevels))
  model % z = model % rmdi
  model % p = model % rmdi
  allocate(z_theta(nlevels))
  allocate(z_rho(nlevels_rho))
  allocate(exner(nlevels_rho))

  if ( output_mode >= output_debug ) then
    print '(a,20(4f20.6))', ' zsea_theta = ', zsea_theta
    print '(a,71f20.6)', ' c_theta = ', c_theta
    print '(a,71f20.6)', ' zsea_rho = ', zsea_rho
    print '(a,71f20.6)', ' c_rho = ', c_rho
  end if

  do i = 1, model % nprofs

    ! Calculate heights (Ref: Appendix A of UMDP F3)

    z_theta = zsea_theta + c_theta * model % zsurf(i)
    z_rho(1:nlevels) = zsea_rho(1:nlevels) + c_rho(1:nlevels) * model % zsurf(i)

    ! The uppermost level may have a missing upper layer boundary (i.e.,
    ! zsea_rho and c_rho values at the top). The definition of this boundary is
    ! that it is an equal distance above the level as the lower layer boundary
    ! is below it.

    if ( nlevels_rho > nlevels ) then
      z_rho(nlevels_rho) = 2.0_real64 * z_theta(nlevels) - z_rho(nlevels)
    end if

    ! Calculate exner pressures for rho-levels

    do j = 1, nlevels_rho
      exner(j) = (model % ph(i,j) / p0) ** kappa
    end do

    do j = 1, nlevels
      model % p(i,j) = p0 * &
        ( (exner(j)   * (z_rho(j+1) - z_theta(j)) + &
           exner(j+1) * (z_theta(j) - z_rho(j))     ) &
          / (z_rho(j+1) - z_rho(j))                   ) ** (1.0_real64 / kappa)
    end do

    model % z(i,:) = z_theta

  end do

  if ( allocated(z_theta) ) deallocate(z_theta)
  if ( allocated(z_rho) ) deallocate(z_rho)
  if ( allocated(exner) ) deallocate(exner)

end if

end subroutine radsim_calc_meto_plevels
