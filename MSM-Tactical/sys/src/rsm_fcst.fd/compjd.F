!
!***********************************************************************
!
      subroutine compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compjd      computes julian day and fraction
!   prgmmr: kenneth campana  org: w/nmc23    date: 89-07-07
!
! abstract: computes julian day and fraction 
!   from year, month, day and time utc.
!
! program history log:
!   77-05-06  ray orzol,gfdl
!   98-05-15  iredell   y2k compliance
!
! usage:    call compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!   input argument list:
!     jyr      - year (4 digits)-intial fcst time.
!     jmnth    - month-initial fcst time.
!     jday     - day-initial fcst time.
!     jhr      - z-time of initial fcst time.
!     jmn      - minutes (zero passed from calling program).
!   output argument list:
!     jd       - julian day.
!     fjd      - fraction of the julian day.
!
! subprograms called:
!   iw3jdn     compute julian day number
!
! attributes:
!   language: fortran.
!
!$$$
      use machine , only :kind_rad
      implicit none
!
      integer jyr,jmnth,jday,jhr,jmn,jd
      integer iw3jdn
      
      real (kind=kind_rad) fjd

      jd=iw3jdn(jyr,jmnth,jday)
      if(jhr.lt.12) then
        jd=jd-1
        fjd=0.5+jhr/24.+jmn/1440.
      else
        fjd=(jhr-12)/24.+jmn/1440.
      endif
      end
