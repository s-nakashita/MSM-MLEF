!-------------------------------------------------------------------------------
! Description:
!
!   Check Met Office UM fieldsfile or PP file packing code. Fail if the code
!   indicates packed data.
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


subroutine radsim_check_ff_packing(lbpack)

use radsim_mod_constants, only : &
  status_error, &
  status_warning

implicit none

include 'radsim_error_report.interface'

integer, intent(in) :: lbpack

integer :: i
integer :: packing(5)

do i = 1, 5
  packing(i) = mod(lbpack,10**i)/10**(i-1)
end do

if ( packing(1) /= 0 ) then
  print '(a,i0)', 'Packing          = ', packing(1)
  print '(a,i0)', 'Data compression = ', packing(2)
  print '(a,i0)', 'Compression      = ', packing(3)
  print '(a,i0)', 'Number format    = ', packing(4)
  call radsim_error_report( &
    'Decoding of packed data is not supported.' // &
    ' Please use the UM ieee tool to do this first', &
    status_error)
else if ( packing(4) /= 3 ) then
  print '(a,i0)', 'Packing          = ', packing(1)
  print '(a,i0)', 'Data compression = ', packing(2)
  print '(a,i0)', 'Compression      = ', packing(3)
  print '(a,i0)', 'Number format    = ', packing(4)
  call radsim_error_report('Data is not IEEE format', status_warning)
end if

end subroutine radsim_check_ff_packing
