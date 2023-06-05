!-------------------------------------------------------------------------------
! Description:
!
!   Calculate saturation vapour pressure (Pa).
!
!   This is done using Magnus' formula:
!
!   e = a*exp(b*T/(T+c))
!
! References:
!
!   http://old.ecmwf.int/research/ifsdocs/CY28r1/Physics/index.html
!   Unified Model Documentation Paper 29, Met Office internal publication.
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

subroutine radsim_esat(t, e)

use radsim_mod_constants, only : &
  real64

implicit none

real(real64), intent(in)  :: t(:)
real(real64), intent(out) :: e(:)

! Magnus' formula constants from WMO (2008)
real(real64), parameter :: a = 611.2_real64
real(real64), parameter :: bw = 17.62_real64
real(real64), parameter :: cw = 243.12_real64
real(real64), parameter :: bi = 22.46_real64
real(real64), parameter :: ci = 272.62_real64

where ( t > 273.15_real64 )
  e = a * exp( bw * (t - 273.15_real64) / ( (t - 273.15_real64) + cw) )
elsewhere
  e = a * exp( bi * (t - 273.15_real64) / ( (t - 273.15_real64) + ci) )
end where

end subroutine radsim_esat
