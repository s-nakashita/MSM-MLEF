!-------------------------------------------------------------------------------
! Description:
!
!   Calculate a column integrated content (e.g., LWP) given a profile containing
!   mixing ratios.
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

subroutine radsim_calc_wp(p,q,wp)

use radsim_mod_constants, only : &
  g, &
  real64

implicit none

real(real64), intent(in)  :: p(:)  ! pressure levels
real(real64), intent(in)  :: q(:)  ! mixing ratio on pressure levels
real(real64), intent(out) :: ip    ! integrated path

integer :: i
real(real64) :: dp, meanq

!-------------------------------------------------------------------------------

wp = 0.0

do i = 1, size(p) - 1
  dp = abs(p(i) - p(i+1)) * 100.0
  if ( q(i) < 0.0 ) cycle
  meanq = 0.5 * (q(i) + q(i+1))
  ip = ip + dp * meanq
end do

ip = ip / g

end subroutine radsim_calc_wp
