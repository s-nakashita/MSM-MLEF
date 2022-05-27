      subroutine xy2ll(x,y,blat,blon)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setbgd
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  setup grid map factor etc for base grid.
!
! program history log:
!
! usage:    call setbgd(blat,blon,delx,dely,dlamda0,
!          1            ngf,ib1,ib2,jb1,jb2)
!   input argument list:
!     blat  - latitude
!     blon  - logitude
!     delx  - grid spacing in x direction
!     dely  - grid spacing in y direction
!     dlamda0 - angle between 0 longitude and x positive axies
!     ngf - the factor between regional and base grid spacing
!     ib1 - first bound of first dimension of blat and blon
!     ib2 - second bound of first dimension of blat and blon
!     jb1 - first bound of second dimension of blat and blon
!     jb2 - second bound of second dimension of blat and blon
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
#include <rscomloc.h>
!cc
!
!     print 1234
!1234 format(' ==== in routine ll2xy === ')
!
!     parameter(twopi=2.0*pi,hfpi=0.5*pi,qtpi=0.5*hfpi)
!     parameter(rad=pi/180.)
!
      hfpi = dasin(1.0d0)
      qtpi = hfpi * 0.5
      pi = 2.0 * hfpi
      twopi = 2.0 * pi
      rad = pi / 180.
!
! --------- setup regional lat/lon and map factor -----
!
! if proj=0  do mercater projection
! if proj=1  do north polar projection
! if proj=-1 do south polar projection
!
      nproj = cproj
!
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
! ++++++++++++++++++++++++++++++++++++++
! polar projection
! ++++++++++++++++++++++++++++++++++++++
        truth  = ctruth * rad
        truth  = nproj * truth
        orient  = corient * rad
        dlamda0 = orient + hfpi
        a2 =  rerth * ( 1.0 + sin(truth) )
        radlat = ccenlat * rad
        radlon = ccenlon * rad - dlamda0
        radlat = nproj * radlat
        radlon = nproj * radlon
        yyy = a2 * cos( radlat )/(1. + sin( radlat ) )
        cenlat = ccenlat
        if( abs(cenlat) .eq. 90. ) yyy = 0.0
        y00 = yyy * sin( radlon ) - ( cbtmgrd -1.) * cdely
        x00 = yyy * cos( radlon ) - ( clftgrd -1.) * cdelx
!
! =========
        if( x .gt. 0.e0 ) then
          blon = atan(y/x)
        else if ( x .lt. 0.e0 ) then
          blon = pi + atan(y/x)
        else
          blon = hfpi
          if( y .lt. 0.e0 ) blon = blon * 3.0
        endif
        blon = nproj * blon + dlamda0
        blon = mod(blon,twopi)
        if(blon.lt.0.e0) blon = twopi + blon
!
        rsoa2 = sqrt( x*x + y*y )/a2
        blat = hfpi - 2.0e0 * atan(rsoa2)
        blat = nproj * blat
!
      else if ( nproj.eq.0 ) then
!
! ++++++++++++++++++++++++++++
! do mercater
! ++++++++++++++++++++++++++++
        truth  = ctruth * rad
        cenlat = ccenlat * rad
        cenlon = ccenlon * rad 
        a2 =  rerth * cos( truth ) 
        x0 = 0.0
        y0 = a2 * log( abs( tan( qtpi + 0.5 * cenlat ) ) )
        x00 = - ( clftgrd - 1.0 ) * cdelx
        y00 = - ( cbtmgrd - 1.0 ) * cdely
        dlamda0 = 0.0
!
        blon = x / a2 + cenlon
        blon = mod(blon,twopi)
        if(blon.lt.0.e0) blon = twopi + blon
        blat = 2.0 *( atan( exp( y/a2 ) ) - qtpi )
!
      else if (nproj.eq.4) then
!        corient=(clon2-clon1)/2.0
!        ctruth=(clat2-clat1)/2.0
!        truth  = ctruth * rad
!        cenlon = corient * rad
!        cenlon = mod(cenlon,twopi)
!        if(cenlon.lt.0.e0) cenlon = twopi + cenlon
!        cenlat = ctruth * rad
!        a2 =  rerth * cos( truth )
!        blon = x / a2 + cenlon
!        blon = mod(blon,twopi)
!        if(blon.lt.0.e0) blon = twopi + blon
!        blat = y / a2 + cenlat
        blon = x
        blon = mod(blon,twopi)
        if(blon.lt.0.e0) blon = twopi + blon
        blat = y

      endif
!
      return
      end
