      subroutine getspfh(km,p,rh,t,sh)
!
!   change from getrh
!
! Subprogram: getrh      Compute saturation humidity and relative humidity
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram computes the saturation specific humidity and the
!           relative humidity.  The relative humidity is constrained to be
!           between 0 and 100.
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call getrh(km,p,sh,t,shs,rh)
!   Input argument list:
!     km       integer number of levels
!     p        real (km) pressure (Pa)
!     sh       real (km) specific humidity (kg/kg)
!     t        real (km) temperature (K)
!   Output argument list:
!     shs      real (km) saturation specific humidity (kg/kg)
!     rh       real (km) relative humidity (percent)
!
! Modules used:
!   funcphys       Physical functions
!
! Files included:
!   physcons.h     Physical constants
!
! Subprograms called:
!   fpvs           compute saturation vapor pressure
!
! Attributes:
!   Language: Fortran 90
!
!$$$
      use funcphys
      use physcons
      implicit none
      integer,intent(in):: km
      real,intent(in):: p(km),rh(km),t(km)
      real,intent(out):: sh(km)
      real :: shs(km)
      real pr,tr,es
      integer k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      do k=1,km
       pr=p(k)
       tr=t(k)
       es=fpvs(tr)
       es=min(es,pr)
       shs(k)=con_eps*es/(pr+con_epsm1*es)
!      rh(k)=1.e2*min(max(sh(k)/shs(k),0.),1.)
       sh(k)=rh(k)/100.0*shs(k)
      enddo
      end 
