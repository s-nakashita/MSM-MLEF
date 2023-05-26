!
!***********************************************************************
!
      subroutine fcstim(fhour,imon,iday,iztim,jdnmc,fjdnmc,             &
     &                  rsin1,rcos1,rcos2,jd,fjd)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fcstim      set forecast orbit parms and julian day.
!   prgmmr: kenneth campana  org: w/nmc23    date: 89-07-07
!
! abstract: for a given forecast hour and initial julian day,
!   three orbit parameters and the forecast julian day are computed.
!
! program history log:
!   98-05-15  iredell   y2k compliance
!
! usage:    call fcstim(fhour,imon,iday,iztim,jdnmc,fjdnmc,
!    1                  rsin1,rcos1,rcos2,jd,fjd)
!   input argument list:
!     fhour    - forecast hour
!     imon     - not used
!     iday     - not used
!     iztim    - not used
!     jdnmc    - initial julian day.
!     fjdnmc   - initial fraction of the julian day.
!     rlag     - day of perihelion?
!     year     - days in year
!   output argument list:
!     rsin1    - orbit parameter
!     rcos1    - orbit parameter
!     rcos2    - orbit parameter
!     jd       - forecast julian day.
!     fjd      - forecast fraction of the julian day.
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
      integer imon,iday,iztim,jdnmc,jd
      integer jda,iyear,im,id,idaywk,idayyr
      real (kind=kind_rad) tpi
      parameter (tpi=2.e0*3.141593e+0)

      real (kind=kind_rad) fhour,fjdnmc,rsin1,rcos1,rcos2,fjd
      real (kind=kind_rad) xda,dyinc,dyfcst,rlag,year,rang

      if(fjdnmc.ge.0.5) then
        jda=jdnmc+1
        xda=fjdnmc-0.5
      else
        jda=jdnmc
        xda=fjdnmc+0.5
      endif
      call w3fs26(jda,iyear,im,id,idaywk,idayyr)
      dyinc=fhour/24
      dyfcst=idayyr+xda+dyinc
      rlag=14.8125
      year=365.25
      rang=tpi*(dyfcst-rlag)/year
      rsin1=sin(rang)
      rcos1=cos(rang)
      rcos2=cos(2*rang)
      jd=jdnmc+fjdnmc+dyinc
      fjd=jdnmc+fjdnmc+dyinc-jd
      end
