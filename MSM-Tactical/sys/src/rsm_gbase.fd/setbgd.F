#include <define.h>
      subroutine setbgd(blat,blon,delx,dely,dlamda0,                     &  
     &                  ngf,ib1,ib2,jb1,jb2                              &
#ifdef MP
     &                 ,mype,master                                      &
#endif
     &                  )
!
! abstract:  setup grid map factor etc for base grid.
!
! program history log:
!
! usage:    call setbgd(blat,blon,delx,dely,dlamda0,
!          1            ngf,ib1,ib2,jb1,jb2)
!   input argument list:
!     blat      - latitude
!     blon      - logitude
!     delx      - grid spacing in x direction
!     dely      - grid spacing in y direction
!     dlamda0   - angle between 0 longitude and x positive axies
!     ngf       - the factor between regional and base grid spacing
!     ib1       - first bound of first dimension of blat and blon
!     ib2       - second bound of first dimension of blat and blon
!     jb1       - first bound of second dimension of blat and blon
!     jb2       - second bound of second dimension of blat and blon
!
!   output argument list:
!
!   input files: none
!
!   output files: none
!
!   subprograms called: 
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use physcons, rerth => con_rerth
!ccj
!c#include <paramodel.h>
#include <rscomloc.h>
!cc
      dimension blat(ib1:ib2,jb1:jb2),blon(ib1:ib2,jb1:jb2)
!
#ifdef MP
      if (mype.eq.master) then
#endif
      print 1234
 1234 format(' ==== in routine setbgd === ')
#ifdef MP
      endif
#endif
!
      hfpi = dasin(1.0d0)
      qtpi = hfpi * 0.5
      pi = 2.0 * hfpi
      twopi = 2.0 * pi
      rad = pi / 180.
      twothird=2.0/3.0
      delx = rdelx
      dely = rdely
!
! --------- setup regional lat/lon and map factor -----
!
      nproj = rproj
!
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
! ++++++++++++++++++++++++++++++++++++
! do polar projection
! ++++++++++++++++++++++++++++++++++++
      truth  = rtruth * rad
      truth = nproj * truth
      orient  = rorient * rad
      dlamda0 = orient + hfpi
      a2 =  rerth * ( 1.0 + sin( truth ) )
      radlat = rcenlat * rad
      radlon = rcenlon * rad - dlamda0
      radlat = nproj * radlat
      radlon = nproj * radlon
      yyy = a2 * cos( radlat )/(1. + sin( radlat ) )
      cenlat = rcenlat
      if( abs(cenlat) .eq. 90. ) yyy = 0.0
      y00 = yyy * sin( radlon ) - ( rbtmgrd -1.) * dely                  &  
     &                          - twothird * dely
      x00 = yyy * cos( radlon ) - ( rlftgrd -1.) * delx                  &
     &                          - twothird * delx
!      print *,' delx x00 y00 ',delx,x00,y00
!
! =========
!           lat loop
!
      do 100 j = jb1,jb2
!
      y = y00 + j*dely*ngf
!
      do 100 i = ib1,ib2
      x = x00 + i*delx*ngf
      if( x .gt. 0.e0 ) then
         flon = atan(y/x)
      else if ( x .lt. 0.e0 ) then
         flon = pi + atan(y/x)
      else
         flon = hfpi
         if( y .lt. 0.e0 ) flon = flon * 3.0
      endif
      flon = nproj*flon + dlamda0
      flon = mod(flon,twopi)
      if(flon.lt.0.e0) flon = twopi + flon
!
      rsoa2 = sqrt( x*x + y*y )/a2
      blatx = hfpi - 2.0e0 * atan(rsoa2)
      blat(i,j) = nproj * blatx
      blon(i,j) = flon
!
 100  continue
!
      else if ( nproj.eq.0 ) then
! ++++++++++++++++++++++++++++++++
! do mercater
! ++++++++++++++++++++++++++++++++
      truth  = rtruth * rad
      cenlat = rcenlat * rad
      cenlon = rcenlon * rad 
      a2 =  rerth * cos( truth ) 
      x0 = 0.0
      y0 = a2 * log( abs( tan( qtpi + 0.5 * cenlat ) ) )
      x00 = -( rlftgrd - 1. ) * delx - twothird * delx
      y00 = -( rbtmgrd - 1. ) * dely - twothird * dely
      dlamda0 = 0.0
!      ! add 20220720
!      wlon = cenlon + x00 / a2
!      if(wlon.lt.0.e0) wlon = twopi + wlon
!
! =========
!           lat loop
!
      do 200 j = jb1,jb2
!
      y = y00 + j*dely*ngf + y0
!
       do 200 i = ib1,ib2
         x = x00 + i*delx*ngf + x0
         flon = x / a2 + cenlon
         flon = mod(flon,twopi)
! comment out 20220720
!         if(flon.lt.0.e0) flon = twopi + flon
!
         blat(i,j) = 2.0 *( atan( exp( y/a2 ) ) - qtpi )
! comment out 20220720
!         ! add by zyf 20140604
!         ! fix problem which domain covers 0E.
!         ! modified 20220720
!         if (((ib1.le.0).and.(flon.lt.blon(ib1,j))) &
!         &   .or. &
!         &   ((ib1.gt.0).and.(flon.lt.wlon))) then
!             blon(i,j) = flon + twopi
!         else
             blon(i,j) = flon
!         endif
!
 200  continue
!
      endif
!
      return
      end
