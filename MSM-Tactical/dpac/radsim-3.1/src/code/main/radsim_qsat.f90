!-------------------------------------------------------------------------------
! Description:
!
!   Calculate specific humidity q according to the standard definition:
!
!   q = epsilon * e / (p - (1-epsilon) * e)
!
!   Where:
!     epsilon = Rd/Rv      (Rd, Rv are the specific gas constants of dry air
!                           and water vapour respectively)
!     e = saturation vapour pressure (Pa)
!     p = pressure (Pa)
!
!   Note this is saturation specific humidity rather than saturation mixing
!   ratio.
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

subroutine radsim_qsat(p, t, qsat)

use radsim_mod_constants, only : &
  real64, &
  epsilon

implicit none

include 'radsim_esat.interface'

real(real64), intent(in)  :: p(:)
real(real64), intent(in)  :: t(:)
real(real64), intent(out) :: qsat(:)

real(real64) :: e(size(p))

call radsim_esat(t, e)
qsat = epsilon * e / (p - (1.0_real64 - epsilon) * e)

end subroutine radsim_qsat
