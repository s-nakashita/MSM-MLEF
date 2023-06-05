!-------------------------------------------------------------------------------
! Description:
!
!   Convert model fields to those required for running RTTOV. Possible
!   conversions are:
!
!   - t from theta
!   - q2 from td2
!   - q from rh and q2 from rh2
!   - rh from q and rh2 from q2
!   - lsm from zsurf
!   - Rain/snow flux (kg/m^2/s) to kg/kg
!   - ozone mixing ratio in mg/kg to kg/kg
!
! Copyright:
!
!    This software was developed within the context of the EUMETSAT Satellite
!    Application Facility on Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!    Met Office, UK, by one or more partners within the NWP SAF. The partners
!    in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!    Copyright 2018, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_convert_fields(model)

use radsim_mod_cfg, only : &
  write_profiles, &
  mmr_snowrain, &
  ozone_mgperkg

use radsim_mod_constants, only : &
  real64, &
  kappa, &
  p0, &
  status_error, &
  rho_rain, &
  rho_snow, &
  conv_rain, &
  conv_snow

use radsim_mod_types, only : &
  model_type

use rttov_const, only : &
  mair, &
  rgc

implicit none

include 'radsim_error_report.interface'
include 'radsim_qsat.interface'

type(model_type), intent(inout) :: model

integer :: i, j
integer :: nprofs
integer :: nlevels
real(real64) :: mr2de

!-------------------------------------------------------------------------------

nprofs = model % nprofs
nlevels = model % nlevels
print '(a)', 'Converting fields:'

!-------------------------------------------------------------------------------
! Calculate T from theta
!-------------------------------------------------------------------------------

if ( .not. associated(model % t) .and. associated(model % theta) ) then

  print '(a)', ' Converting theta to t'

  nlevels = size(model % theta, dim=2)
  allocate(model % t(nprofs,nlevels))
  model % t = model % theta * (model % p/p0) ** kappa

end if

if ( associated(model % theta) ) deallocate(model % theta)

!-------------------------------------------------------------------------------
! Calculate humidity
!-------------------------------------------------------------------------------

!--------------------------
! Calculate q2 from td2
!--------------------------

! q is by definition qsat at the dewpoint so we can calculate this directly

if ( .not. associated(model % q2) .and. associated(model % td2) ) then

  print '(a)', ' Calculating q2 from td2'

  if ( .not. associated(model % pstar) ) then
    call radsim_error_report( &
      'No surface pressure: Cannot convert', status_error)
  end if

  allocate(model % q2(nprofs))

  call radsim_qsat(model % pstar, model % td2, model % q2)

end if

if ( associated(model % td2) ) deallocate(model % td2)

!------------------------
! Calculate q from RH
!------------------------

! We can obtain this by calculating qsat and multiplying by RH. We are assuming
! here that RH is calculated wrt ice below 0degC. This may not always be the
! case but there is no way of knowing.

! Surface

if ( .not. associated(model % q2) .and. associated(model % rh2) ) then

  print '(a)', ' Calculating q2 from rh2'

  allocate(model % q2(nprofs))

  call radsim_qsat(model % pstar, model % t2, model % q2)
  model % q2 = model % q2 * model % rh2 * 0.01_real64

end if

! Levels

if ( .not. associated(model % q) .and. associated(model % rh) ) then

  print '(a)', ' Calculating q from rh'

  allocate(model % q(nprofs, nlevels))

  do j = 1, nlevels
    call radsim_qsat(model % p(:,j), model % t(:,j), model % q(:,j))
  end do

  model % q = model % q * model % rh * 0.01_real64

end if

if ( .not. write_profiles ) then
  if ( associated(model % rh2) ) deallocate(model % rh2)
  if ( associated(model % rh) ) deallocate(model % rh)
end if

!------------------------
! Calculate RH from q
!------------------------

! Only for diagnostic purposes, otherwise not needed

if ( .not. associated(model % rh) .and. write_profiles ) then

  print '(a)', ' Calculating rh from q'

  allocate(model % rh(nprofs, nlevels))

  do j = 1, nlevels
    call radsim_qsat(model % p(:,j), model % t(:,j), model % rh(:,j))
  end do

  model % rh = 100.0_real64 * model % q / model % rh

  ! Avoid generating negative RH values at high altitudes which sometimes arise
  ! from the qsat calculation presumably due to unrealistic model temperatures
  ! in the high atmosphere.
  where (model % rh < 0.1_real64) model % rh = 0.1_real64
end if

!-------------------------------------------------------------------------------
! Set LSM from surface height
!-------------------------------------------------------------------------------

! If no LSM is provided then it is inferred that a surface elevation of 0 is
! a sea point, otherwise it is land.

if ( .not. associated(model % lsm) .and. associated(model % zsurf) ) then

  print '(a)', ' Deriving lsm from zsurf'

  allocate(model % lsm(nprofs))

  where ( model % zsurf == 0.0 )
    model % lsm = 0.0
  elsewhere
    model % lsm = 1.0_real64
  end where

end if

!-------------------------------------------------------------------------------
! Rain/snow flux to kg/kg
!-------------------------------------------------------------------------------

! This conversion is taken from the RTTOV v13 source

if ( .not. mmr_snowrain ) then

  mr2de =  rgc / mair

  if ( associated(model % rain) ) then

    print '(a)', ' Converting from rain flux to kg/kg'

    model % rain(:,:) = model % rain(:,:) / rho_rain
    do j = 1, nlevels
      do i = 1, nprofs
        if ( model % rain(i,j) > 0. ) then
          model % rain(i,j) = (model % rain(i,j) * conv_rain(1))**conv_rain(2)
        end if
      end do
    end do
    model % rain(:,:) = model % rain(:,:) * &
      mr2de * model % t(:,:) / model % p(:,:)

  end if

  if ( associated(model % snow) ) then

    print '(a)', ' Converting from snow flux to kg/kg'

    model % snow(:,:) = model % snow(:,:) / rho_snow
    do j = 1, nlevels
      do i = 1, nprofs
        if ( model % snow(i,j) > 0. ) then
          model % snow(i,j) = (model % snow(i,j) * conv_snow(1))**conv_snow(2)
        end if
      end do
    end do
    model % snow(:,:) = model % snow(:,:) * &
      mr2de * model % t(:,:) / model % p(:,:)
  
  end if

  mmr_snowrain = .true. ! Units are now kg/kg

end if

!-------------------------------------------------------------------------------
! Convert from mg/kg to kg/kg
!-------------------------------------------------------------------------------

if ( ozone_mgperkg .and. associated(model % o3) ) then

  print '(a)', ' Converting o3 from mg/kg to kg/kg'

  model % o3(:,:) = model % o3(:,:) / 1.e6_real64

  ozone_mgperkg = .false.

end if

end subroutine radsim_convert_fields
