      subroutine rmpdimset
!$$$  subprogram documentation block
!
! subprogram: 	rmpdimset
!            
! prgmmr: Yifeng Cui, 06-04-2002
!
! abstract: preset all starting point and length for 
!           all pe for  regional spectral model.
!           
!
! usage:	call rmpdimset()
!
!    input argument lists:
!    
!
!    output argument list:
! 
! subprograms called:
!   rdimset	- to compute all dimension, starting point for all pe
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
      integer           levmax,                                           &  
     &                  lonmax,                                           &  
     &                  latmax,                                           &  
     &                  lwvmax,                                           &  
     &                  lntmax,                                           &  
     &                  lnpmax      
!
      call rdimset(iwav1,jwav1,levr,igrd1,jgrd1,                          &  
     &                  npes,ncol,nrow,                                   &
     &                  levstr,levlen,levmax,                             &
     &                  lonstr,lonlen,lonmax,                             &
     &                  latstr,latlen,latmax,                             &
     &                  lwvstr,lwvlen,lwvmax,                             &
     &                  lntstr,lntlen,lntmax,                             &
     &                  lnpstr,lnplen,lnpmax)
!
      if( levmax .gt. levrp ) then
        print *,' levmax levrp ',levmax,levrp      
        print *,' Error in rmpdimset for levmax '
        call rmpabort
      endif
!
      if( lonmax .gt. igrd1p ) then
        print *,' lonmax igrd1p ',lonmax,igrd1p
        print *,' Error in rmpdimset for lonmax '
        call rmpabort
      endif
!
      if( latmax .gt. jgrd1p ) then
        print *,' latmax jgrd1p ',latmax,jgrd1p      
        print *,' Error in rmpdimset for latmax '
        call rmpabort
      endif
!
!  original is iwv1p, but this seems to be a problem
!
      if( lwvmax .gt. iwav1p ) then
        print *,' lwvmax iwav1p ',lwvmax,iwav1p      
        print *,' Error in rmpdimset for lwvmax '
        call rmpabort
      endif
!
      if( lntmax .gt. lnwavp ) then
        print *,' lntmax lnwavp ',lntmax,lnwavp      
        print *,' Error in rmpdimset for lntmax '
        call rmpabort
      endif
!
      if( lnpmax .gt. llwavp ) then
        print *,' lnpmax llwavp ',lnpmax,llwavp      
        print *,' Error in rmpdimset for lnpmax '
        call rmpabort
      endif
!
#endif
      return
      end
