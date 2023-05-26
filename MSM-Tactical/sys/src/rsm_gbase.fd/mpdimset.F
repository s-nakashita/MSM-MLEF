      subroutine mpdimset
!$$$  subprogram documentation block
!
! subprogram: 	mpdimset
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: preset all starting point and length for 
!           all pe for global spectral model.
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call mpdimset(jcap,levs,lonf,latg)
!
!    input argument lists:
!	jcap	- integer spectral wavenumber
!	levs	- integer vertical layer number
!	lonf	- integer gaussian grid for longitude
!	latg	- integer gaussian grid for latitude
!
!    output argument list:
! 
! subprograms called:
!   dimset	- to compute all dimension, starting point for all pe
!
! attributes:
!    language: fortran 90
!$$$
!
#include <define.h>
#ifdef MP
      implicit none
#include <paramodel.h>
#include <npesi.h>
#include <comrmpi.h>
#include <commpi.h>
#include <commpildef.h>
      integer levmax,lonmax,latmax,lntmax,lnpmax,lwvmax
!
      call dimset(jcap,levs,lonf,latg,                                   &         
     &            npes,ncol,nrow,                                        &
     &            glevstr,glevlen,levmax,                                &
     &            glonstr,glonlen,lonmax,                                &
     &            glatstr,glatlen,latmax,                                &
     &            glwvstr,glwvlen,lwvmax,                                &
     &            glntstr,glntlen,lntmax,                                &
     &            glnpstr,glnplen,lnpmax,                                &
     &            lwvdef,latdef)
!
      if( levmax .gt. levsp ) then
        print *,' levmax levsp ',levmax,levsp
        print *,' Error in mpdimset for levmax '
        call rmpabort
      endif
!
      if( lonmax .gt. lonfp ) then
        print *,' lonmax lonfp ',lonmax,lonfp
        print *,' Error in mpdimset for lonmax '
        call rmpabort
      endif
!
      if( latmax .gt. latgp ) then
        print *,' latmax latgp ',latmax,latgp
        print *,' Error in mpdimset for latmax '
        call rmpabort
      endif
!
      if( lwvmax .gt. jcapp ) then
        print *,' lwvmax jcapp ',lwvmax,jcapp
        print *,' Error in mpdimset for lwvmax '
        call rmpabort
      endif
!
      if( lntmax .gt. lntp ) then
        print *,' lntmax lntp ',lntmax,lntp
        print *,' Error in mpdimset for lntmax '
        call rmpabort
      endif
!
      if( lnpmax .gt. llnp ) then
        print *,' lnpmax llnp ',lnpmax,llnp
        print *,' Error in mpdimset for lnpmax '
        call rmpabort
      endif
!
#endif
      return
      end
