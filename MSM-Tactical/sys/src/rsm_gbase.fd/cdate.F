!
!***********************************************************************
!
      subroutine cdate(jd,fjd,munth,im,id,iyear,ihr,xmin)
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdate       computes day,month,yr from julian day
!   prgmmr: kenneth campana  org: w/nmc23    date: 89-07-07
!
! abstract: computes month,day,year from julian day.
!
! program history log:
!   77-06-07  robert white,gfdl
!   98-05-15  iredell   y2k compliance
!
! usage:    call cdate(jd,fjd,munth,im,id,iyear,ihr,xmin)
!   input argument list:
!     jd       - julian day for current fcst hour.
!     fjd      - fraction of the julian day.
!   output argument list:
!     munth    - month (character).
!     im       - month (integer).
!     id       - day of the month.
!     iyear    - year.
!     ihr      - hour of the day.
!     xmin     - minute of the hour.
!
! subprograms called:
!   w3fs26     year, month, day from julian day number
!
! attributes:
!   language: fortran.
!
!$$$
      use machine , only :kind_rad
      implicit none
!
      character*4 month(12),munth
      integer jd,im,id,iyear,ihr
      integer jda,mfjd,idaywk,idayyr

      real (kind=kind_rad) fjd,xmin

      data month /'jan.','feb.','mar.','apr.','may ','june',            &
     &            'july','aug.','sep.','oct.','nov ','dec.'/
!jw
       save month
      if(fjd.ge.0.5) then
        jda=jd+1
        mfjd=nint(fjd*1440.)
        ihr=mfjd/60-12
        xmin=mfjd-(ihr+12)*60
      else
        jda=jd
        mfjd=nint(fjd*1440.)
        ihr=mfjd/60+12
        xmin=mfjd-(ihr-12)*60
      endif
      call w3fs26(jda,iyear,im,id,idaywk,idayyr)
      munth=month(im)
      end
