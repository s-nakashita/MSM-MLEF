      subroutine setlatlon(flat,flon,delx,dely,dlamda0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setlatlon
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  setup grid latitude, longitude, and map factor etc for
!            regional grid.
!
! program history log:
!
! usage:    call setlatlon(flat,flon,delx,dely,dlamda0)
!   input argument list:
!     flat      - latitude
!     flon      - logitude
!     delx      - grid spacing in x direction
!     dely      - grid spacing in y direction
!     dlamda0   - angle between 0 longitude and x positive axies
!
!   output argument list:
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
      use physcons, rerth => con_rerth
!ccj
#include <paramodel.h>
#include <rscomloc.h>
!cc
!
      dimension flat(lngrd),flon(lngrd)
!c
      print 1234
 1234 format(' ==== in routine setlatlon === ')
!
      hfpi = pi * 0.5
      hfpi = dasin(1.0d0)
      qtpi = hfpi * 0.5
      pi = 2.0 * hfpi
      twopi = 2.0 * pi
      rad = pi / 180.
      delx = rdelx
      dely = rdely
!
! --------- setup regional lat/lon and map factor -----
!
! if proj=0  do mercater projection
! if proj=1  do north polar projection
! if proj=-1 do south polar projection
!
      nproj = rproj
!
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
! ++++++++++++++++++++++++++++++++++++++
! polar projection
! ++++++++++++++++++++++++++++++++++++++
      truth  = rtruth * rad
      truth  = nproj * truth
      orient  = rorient * rad
      dlamda0 = orient + hfpi
      a2 =  rerth * ( 1.0 + sin(truth) )
      radlat = rcenlat * rad
      radlon = rcenlon * rad - dlamda0
      radlat = nproj * radlat
      radlon = nproj * radlon
      yyy = a2 * cos( radlat )/(1. + sin( radlat ) )
      cenlat = rcenlat
      if( abs(cenlat) .eq. 90. ) yyy = 0.0
      y00 = yyy * sin( radlon ) - ( rbtmgrd -1.) * dely
      x00 = yyy * cos( radlon ) - ( rlftgrd -1.) * delx
      print *,' delx x00 y00 ',delx,x00,y00
!
! =========
!           lat loop
      do 100 j = 1,jgrd1
      lats = j
      ijlats = (lats-1)*igrd1
      ys = y00 + (lats-1)*dely
!
      do 100 i=1,igrd1
      x = x00 + (i-1)*delx
      if( x .gt. 0.e0 ) then
         flons = atan(ys/x)
      else if ( x .lt. 0.e0 ) then
         flons = pi + atan(ys/x)
      else
         flons = hfpi
         if( ys .lt. 0.e0 ) flons = flons * 3.0
      endif
      flons = nproj * flons + dlamda0
      flons = mod(flons,twopi)
      if(flons.lt.0.e0) flons = twopi + flons
!
      rsoa2 = sqrt( x*x + ys*ys )/a2
      flats = hfpi - 2.0e0 * atan(rsoa2)
      flat(ijlats+i) = nproj * flats
      flon(ijlats+i) = flons
!
 100  continue
!
      else if ( nproj.eq.0 ) then
!
! ++++++++++++++++++++++++++++
! do mercater
! ++++++++++++++++++++++++++++
      truth  = rtruth * rad
      cenlat = rcenlat * rad
      cenlon = rcenlon * rad 
      a2 =  rerth * cos( truth ) 
      x0 = 0.0
      y0 = a2 * log( abs( tan( qtpi + 0.5 * cenlat ) ) )
      x00 = - ( rlftgrd - 1.0 ) * delx
      y00 = - ( rbtmgrd - 1.0 ) * dely
      dlamda0 = 0.0
      print *,'a2=',a2
!
      do 200 j = 1,jgrd1
      lats = j
      ijlats = (lats-1)*igrd1
      ys = y00 + (lats-1)*dely + y0
!
       do 200 i=1,igrd1
         x = x00 + (i-1)*delx + x0
         flons = x / a2 + cenlon
         flons = mod(flons,twopi)
! comment out 20220720
!         if(flons.lt.0.e0) flons = twopi + flons
!
         flats = 2.0 *( atan( exp( ys/a2 ) ) - qtpi )
         flat(ijlats+i) = flats
         flon(ijlats+i) = flons
! add by zyf 20140604
! fix problem which domain covers 0E.
! comment out 20220720
!         if (flons.lt.flon(ijlats+1)) flon(ijlats+i)=flons+twopi
!
 200  continue
!
      endif
       print *,'nproj=',nproj
      call maxmin(flat,lngrd,1,1,1,'in setlatlon,flat')
      call maxmin(flon,lngrd,1,1,1,'in setlatlon,flon')
!
!
      return
      end
