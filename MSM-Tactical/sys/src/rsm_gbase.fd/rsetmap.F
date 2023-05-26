      subroutine rsetmap(flat,flon,fm2,fm2x,fm2y,delx,dely)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsetmap
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  pass in latitudes longitudes map factor, and compute the
!            mean map factor or the largest map factor and it derivatives
!            in x and y directions.
!
! program history log:
!
! usage:    call rsetmap(flat,flon,fm2,fm2x,fm2y,delx,dely)
!   input argument list:
!     flat  - latitudes of regional grids
!     flon  - longitudes of regional grids
!     fm2 - map factor square
!     fm2x  - x derivative of map factor square
!     fm2y  - y derivative of map factor square
!
!   output argument list:
!     delx     - grid spacing in x direction
!     dely     - grid spacing in y direction
!
!   common block:
!   rscommap
!
!   input files: none
!
!   output files: none
!
!   subprograms called: none
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!.....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....use u v as input, update u v tendency by div vor tendency.....
!.....
      use physcons, omega => con_omega, pi => con_pi
!ccj
#include <paramodel.h>
#include <rscomloc.h>
#include <rscommap.h>
!cc
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define LNGRDS lngrdp
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define LNGRDS lngrd
#endif
!....
      dimension flat(LNGRDS), flon(LNGRDS), fm2(LNGRDS),                 &  
     & fm2x(LNGRDS), fm2y(LNGRDS)
#ifdef MP
      real, allocatable :: xm2full(:,:)
#endif
!.................................................................
!
#ifdef MP
      me=mype
#else
      me=0
#endif
      if(me.eq.0)print *,' ----- begin setmap ----'
!
#ifdef MP
      allocate (xm2full(igrd1,jgrd1))
#endif
!      pi = asin( 1.0 ) * 2.0e0
!
!testjun      domga = 2.0e0 * 2.0e0 * pi / 86400.0e0
      domga = 2.0e0 *omega
      deg = 180.0 / pi
      ran =  pi   / 180.
!.................................................................
! --------- setup regional lat/lon and map factor -----
!
      delx = rdelx
      dely = rdely
      if(me.eq.0)print *,' delx dely ',delx,dely
!
      xm2m = 0.0
!
! =========
!           lat loop
!
      do 1000 lat = 1,JGRD1S
      jlat = (lat-1)*IGRD1S
!
      do 100 i=1,IGRD1S
      ijlat = i + jlat
      rlat(i,lat) = flat(ijlat)
      rlon(i,lat) = flon(ijlat)
      xm2(i,lat)  =  fm2(ijlat)
      xm (i,lat)  =  sqrt(fm2(ijlat))
      xm2px(i,lat) = fm2x(ijlat)
      xm2py(i,lat) = fm2y(ijlat)
!
      xm2m = max( xm2m, xm2(i,lat) )
      corf (i,lat) = domga*sin(rlat(i,lat))
#ifdef NONHYD
      corf2(i,lat) = domga*cos(rlat(i,lat))*cos(rlat(i,lat))
#endif
 100  continue
!
1000  continue
!
#ifdef MP
      call rmpgp2f(xm2,igrd1p,jgrd1p,xm2full,igrd1,jgrd1,1)
      xm2m=0.0
      if (mype.eq.master) then
        do j=1,jgrd1
        do i=1,igrd1
           xm2m=max(xm2m,xm2full(i,j))
        enddo
        enddo
      endif
      call rmpbcastr(xm2m,1)
#endif
      do 150 lat=1,JGRD1S
      do 150 i=1,IGRD1S
      xm2p(i,lat) = xm2(i,lat)  - xm2m
 150  continue
#ifdef MP
      deallocate(xm2full)
#endif
!
      return
      end
