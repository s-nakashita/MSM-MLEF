!-------------------------------------------------------------------------------
! Description:
!
!   Report an error. There are two status levels - warning and error. The
!   latter is used to abort the program.
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

subroutine radsim_error_report( &
  message, &
  status )

use iso_fortran_env, only : &
  error_unit, &
  output_unit

use radsim_mod_constants, only : &
  status_warning, &
  status_error

implicit none

character(len=*), intent(in) :: message
integer,          intent(in) :: status

!-------------------------------------------------------------------------------

! Flush output buffer first so that stdout and stderr are in order

flush(output_unit)

if ( status == status_warning ) then
  write(error_unit,'(2a)') 'WARNING: ', trim(message)
else if ( status == status_error ) then
  write(error_unit,'(2a)') 'ERROR: ', trim(message)
  stop 1
else
  write(error_unit,'(a,i0)') 'Bad status given for error report: ', status
  write(error_unit,'(a)') trim(message)
  write(error_unit,'(a)') 'Continuing...'
end if

! Flush error message so that subsequent output is in order

flush(error_unit)

end subroutine radsim_error_report
