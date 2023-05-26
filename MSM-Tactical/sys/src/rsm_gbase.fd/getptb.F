      subroutine getptb(grid,bgrd,sbgrd,ws,km)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  getptb
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 94-02-06
!
! abstract:  obtained regional perturbation grid by substracting full 
!            field from its base field.
!
! program history log:
!
! usage:    call getptb(grid,bgrd,ws,km)
!   input argument list:
!     grid      - full grid values with dimension of (lngrd,km)
!     bgrd      - base field values with dimension of (lngrdb,km)
!     ws        - interpolation coefficient for base field.
!     km        - the second dimension of grid and bgrd
!
!   output argument list:
!     grid     - perturbation field values of regional grids.
!    sbgrd     - interpolated base field values with dimension of (lngrd,km)
!
!   input files: none
!
!   output files: none
!
!   subprograms called: 
!       pint1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
#include <paramodel.h>
#include <rsparltb.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define LNGRDS  lngrdp
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define LNGRDS  lngrd
#endif
!cc
      dimension grid(LNGRDS,km)                                         &
     &         ,bgrd(lngrdb,km),ws(bgf,1-border:border)                 &
     &         ,sbgrd(LNGRDS,km)
      common /repro/ syn(IGRD1S,levr)
#ifdef MP
      istr=lonstr(mype)
      ilen=lonlen(mype)
      jstr=latstr(mype)
      jlen=latlen(mype)
#else
      istr=1
      ilen=igrd1
      jstr=1
      jlen=jgrd1
#endif
      do j=1,jlen
        lat=jstr+j-1
!        if ( lat.eq.1 .and.mype.eq.0) then
!         print *,'in master,before pint1 bgrd(1:5)=',(bgrd(i,1),i=1,5)  &
!     &    ,'ib1=',ib1,ib2,jb1,jb2,IGRD1S,ilen
!        endif
        call pint1(bgrd,syn,ws,lat,km,IGRD1S,ib1,ib2,                   &
     &             jb1,jb2,jbx,istr,ilen,                               &
     &             bgf,border,jgrd1)
!        if ( lat.eq.1 .and.mype.eq.0) then
!         print *,'in master, after pint1 syn(1:5)=',(syn(i,1),i=1,5)
!     &    ,'grid=',(grid(i,1),i=1,5)
!        endif
        jj=(j-1)*IGRD1S
        do k=1,km
         do i=1,ilen
          grid(i+jj,k) = grid(i+jj,k) - syn(i,k)
          sbgrd(i+jj,k) = syn(i,k)
         enddo
        enddo
      enddo
      return
      end
