!-------------------------------------------------------------------------------
! Description:
!
!   Miscellaneous functions.
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

module radsim_mod_functions

implicit none

contains

  integer function time_in_minutes(date_time)
    ! Calculate time in minutes since a reference date/time for
    ! a given date and time (year, month, day, hour, minute)
    integer, intent(in) :: date_time(5)   ! (/y,m,d,h,m/)

    integer :: i, y
    integer :: ndays
    integer, parameter :: ref_year = 1950 ! Reference time: 00:00 01/01/1950
    integer, parameter :: days_per_month(12) = &
      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

    ! Days
    ndays = date_time(3)

    ! Days in full months since start of year
    do i = 1, date_time(2) - 1
      if (i == 2) then
        y = date_time(1)
        if (mod(y, 4) == 0 .and. (mod(y, 400) == 0 .or. .not. mod(y, 100) == 0)) then
          ndays = ndays + 29
        else
          ndays = ndays + 28
        endif
      else
        ndays = ndays + days_per_month(i)
      endif
    enddo

    ! Days in full years since ref_year
    do i = ref_year, date_time(1) - 1
      if (mod(i, 4) == 0 .and. (mod(i, 400) == 0 .or. .not. mod(i, 100) == 0)) then
        ndays = ndays + 366
      else
        ndays = ndays + 365
      endif
    enddo

    ! Convert ndays to minutes and add hours and minutes
    time_in_minutes = ndays * 24 * 60 + date_time(4) * 60 + date_time(5)

  end function time_in_minutes

  subroutine date_time_plus_minutes(minutes, date_time)
    ! Add the given number of minutes to the given date and time (year, month, 
    ! day, hour, minute)

    integer, intent(in)    :: minutes
    integer, intent(inout) :: date_time(5)   ! (/y,m,d,h,m/)

    integer :: mins, ndays, y
    integer, parameter :: days_per_month(12) = &
      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

    mins = minutes

    ! In each iteration:
    ! - check if minutes in date_time plus remaing mins sum to less than 60
    !   - if so add them and quit
    !   - else decrease mins by enough to bring date_time to the next hour
    !   - then either increment the hour or reset hour to 0 and increment the
    !     day, month, year as necessary
    ! - repeat until mins is small enough so the first test passes (i.e. we
    !   are not pushed into the next hour)
    do
      if (date_time(5) + mins < 60) then
        date_time(5) = date_time(5) + mins
        exit
      else
        mins = mins - (60 - date_time(5))
        date_time(5) = 0
        if (date_time(4) < 23) then
          date_time(4) = date_time(4) + 1
        else
          date_time(4) = 0
          ndays = days_per_month(date_time(2))
          y = date_time(1)
          if (date_time(2) == 2 .and. &
              mod(y, 4) == 0 .and. (mod(y, 400) == 0 .or. .not. mod(y, 100) == 0)) then
            ndays = 29
          endif
          if (date_time(3) < ndays) then
            date_time(3) = date_time(3) + 1
          else
            date_time(3) = 1
            if (date_time(2) < 12) then
              date_time(2) = date_time(2) + 1
            else
              date_time(2) = 1
              date_time(1) = date_time(1) + 1
            endif
          endif
        endif
      endif
    enddo

  end subroutine date_time_plus_minutes

end module radsim_mod_functions
