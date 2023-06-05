!-------------------------------------------------------------------------------
! Description:
!
!   Use the barometric formula and a reference atmosphere to calculate either
!   pressure or height as a function of the other. Optional temperature output
!   is also available.
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

subroutine radsim_calc_pz(calc_p, calc_z, p, z, T)

use radsim_mod_constants, only : &
  gravity, M, R,     &
  std_atmos_H, &
  std_atmos_T, &
  std_atmos_L, &
  std_atmos_P

implicit none

real, parameter :: k = gravity*M/R

logical,        intent(in)    :: calc_p
logical,        intent(in)    :: calc_z
real,           intent(inout) :: p(:)
real,           intent(inout) :: z(:)
real, optional, intent(out)   :: T(:)

integer :: i, j
integer :: nlevels
integer :: layer
real :: zl
real, pointer :: Href(:), Tref(:), Lref(:), Pref(:)

!-------------------------------------------------------------------------------

nlevels = size(p)
Href => std_atmos_H
Tref => std_atmos_T
Lref => std_atmos_L
Pref => std_atmos_P

!----------------------
! 1. Calculate P from z
!----------------------

if ( calc_p ) then

  do j = 1, nlevels

    layer = 0

    do i = 1, 7
      if ( z(j) <= Href(i) ) then
        layer = i-1
        exit
      end if
    end do

    zl = z(j) - Href(layer)

    if ( Lref(layer) /= 0.0 ) then
      p(j) = pref(layer) * &
        ( Tref(layer)/(Tref(layer)+Lref(layer)*zl) )**(k/Lref(layer))
    else
      p(j) = Pref(layer) * exp( -k*zl / Tref(layer) )
    end if

    if ( present(T) ) then
      T(j) = Tref(layer) + zl*Lref(layer)
    end if

  end do

end if

!----------------------
! 1. Calculate z from P
!----------------------

if ( calc_z ) then

  do j = 1, nlevels

    layer = 0

    do i = 1, 7
      if ( p(j) > Pref(i) ) then
        layer = i-1
        exit
      end if
    end do

    if ( Lref(layer) /= 0.0 ) then
      z(j) = (Tref(layer) / Lref(layer) ) * &
        ( (p(j)/Pref(layer))**(-Lref(layer)/k) -1.0 )
    else
      z(j) = -Tref(layer) * log(p(j)/Pref(layer)) / k
    end if

    zl = z(j) + Href(layer)

    if ( present(T) ) then
      T(j) = Tref(layer) + zl*Lref(layer)
    end if

  end do

end if

end subroutine radsim_calc_pz
