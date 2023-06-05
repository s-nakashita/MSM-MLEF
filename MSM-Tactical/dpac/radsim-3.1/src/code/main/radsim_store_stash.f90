!-------------------------------------------------------------------------------
! Description:
!
!   Store UM STASH field
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

subroutine radsim_store_stash( &
  stashcode, & ! in
  field,     & ! in
  model      ) ! inout

use radsim_mod_constants, only : &
  real64, &
  status_error

use radsim_mod_types, only : &
  model_type

implicit none

! Subroutine arguments
integer,          intent(in)    :: stashcode
real(real64),     intent(in)    :: field(:,:)
type(model_type), intent(inout) :: model

! Local variables
integer :: nprofs, nlevels
character(len=80) :: message

!-------------------------------------------------------------------------------

nprofs = size(field, dim=1)
nlevels = size(field, dim=2)

select case(stashcode)
  case(4)     ! theta
    allocate(model % theta(nprofs,nlevels))
    model % theta = field(:,1:nlevels)
  case(10)    ! q
    allocate(model % q(nprofs,nlevels))
    model % q = field(:,1:nlevels)
  case(12)    ! qcf
    allocate(model % ciw(nprofs,nlevels))
    model % ciw = field(:,1:nlevels)
  case(24)    ! Tskin
    allocate(model % tskin(nprofs))
    model % tskin = field(:,1)
  case(30)    ! Land mask
    allocate(model % lsm(nprofs))
    model % lsm = transfer(field(:,1), model % lsm)
  case(31)    ! Seaice
    allocate(model % seaice(nprofs))
    model % seaice = field(:,1)
  case(33)    ! Orog
    allocate(model % zsurf(nprofs))
    model % zsurf = field(:,1)
  case(60)    ! Ozone
    allocate(model % o3(nprofs,nlevels))
    model % o3 = field(:,1:nlevels)
  case(186)   ! Rain-rate
    allocate(model % rain(nprofs,nlevels))
    model % rain = field(:,1:nlevels)
  case(187)   ! Snow-rate
    allocate(model % snow(nprofs,nlevels))
    model % snow = field(:,1:nlevels)
  case(254)   ! qcl
    allocate(model % clw(nprofs,nlevels))
    model % clw = field(:,1:nlevels)
  case(265)   ! Area cloud fraction (large-scale cloud)
    allocate(model % cfrac(nprofs,nlevels))
    model % cfrac = field(:,1:nlevels)
  case(266)   ! Bulk cloud fraction (large-scale cloud)
    allocate(model % cfrac(nprofs,nlevels))
    model % cfrac = field(:,1:nlevels)
  ! case(267)   ! Liquid cloud fraction (large-scale cloud)
  !   allocate(model % cfrac_liq(nprofs,nlevels))
  !   model % cfrac_liq = field(:,1:nlevels)
  ! case(268)   ! Frozen cloud fraction (large-scale cloud)
  !   allocate(model % cfrac_ice(nprofs,nlevels))
  !   model % cfrac_ice = field(:,1:nlevels)
  case(407)   ! p on rho levels
    allocate(model % ph(nprofs,nlevels))
    model % ph = field(:,1:nlevels)
  case(408)   ! p on theta levels
    allocate(model % p(nprofs,nlevels))
    model % p = field(:,1:nlevels)
  case(409)   ! pstar
    allocate(model % pstar(nprofs))
    model % pstar = field(:,1)
  case(3209)  ! u at 10m
    allocate(model % u10(nprofs))
    model % u10 = field(:,1)
  case(3210)  ! v at 10m
    allocate(model % v10(nprofs))
    model % v10 = field(:,1)
  case(3236)  ! T2
    allocate(model % t2(nprofs))
    model % t2 = field(:,1)
  case(3237)  ! q2
    allocate(model % q2(nprofs))
    model % q2 = field(:,1)
  case(3245)  ! rh2
    allocate(model % rh2(nprofs))
    model % rh2 = field(:,1)
  ! case(5212)   ! Convective cloud amount
  !   allocate(model % cfrac_conv(nprofs,nlevels))
  !   model % cfrac_conv = field(:,1:nlevels)
  ! case(5213)   ! Convective cloud condensed water
  !   allocate(model % conv_cloud(nprofs,nlevels))
  !   model % conv_cloud = field(:,1:nlevels)
  ! case(5163)   ! Convective increment
  !   allocate(model % conv_inc(nprofs,nlevels))
  !   model % conv_inc = field(:,1:nlevels)
  case(16004) ! T
    allocate(model % t(nprofs,nlevels))
    model % t = field(:,1:nlevels)
  case default
    write(message, '(a,i0)') 'Unknown STASH code ', stashcode
    call radsim_error_report(message, status_error)
end select

end subroutine radsim_store_stash
