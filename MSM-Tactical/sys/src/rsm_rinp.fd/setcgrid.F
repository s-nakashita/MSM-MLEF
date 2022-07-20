      subroutine setcgrid(clat,clon)
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
! usage:    call setlatlon(clat,clon,delx,dely,dlamda0)
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
      dimension clat(clngrd),clon(clngrd)
!c
      print 1234
 1234 format(' ==== in routine setcgrid === ')
!
      hfpi = pi * 0.5
      hfpi = dasin(1.0d0)
      qtpi = hfpi * 0.5
      pi = 2.0 * hfpi
      twopi = 2.0 * pi
      rad = pi / 180.
      delx = cdelx
      dely = cdely
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
      y00 = yyy * sin( radlon ) - ( cbtmgrd -1.) * dely
      x00 = yyy * cos( radlon ) - ( clftgrd -1.) * delx
      print *,' delx x00 y00 ',delx,x00,y00
!
! =========
!           lat loop
      do 100 j = 1,cjgrd1
      lats = j
      ijlats = (lats-1)*cigrd1
      ys = y00 + (lats-1)*dely
!
      do 100 i=1,cigrd1
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
      clat(ijlats+i) = nproj * flats
      clon(ijlats+i) = flons
!
 100  continue
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
      x00 = - ( clftgrd - 1.0 ) * delx
      y00 = - ( cbtmgrd - 1.0 ) * dely
      dlamda0 = 0.0
      print *,'a2=',a2
!
      do 200 j = 1,cjgrd1
      lats = j
      ijlats = (lats-1)*cigrd1
      ys = y00 + (lats-1)*dely + y0
!
       do 200 i=1,cigrd1
         x = x00 + (i-1)*delx + x0
         flons = x / a2 + cenlon
         flons = mod(flons,twopi)
! comment out 20220720
!         if(flons.lt.0.e0) flons = twopi + flons
!
         flats = 2.0 *( atan( exp( ys/a2 ) ) - qtpi )
         clat(ijlats+i) = flats
         clon(ijlats+i) = flons
!
 200  continue
!
      else if ( nproj.eq.4) then
        if (clon1.lt.0.e0) clon1=clon1+360.0
        if (clon2.lt.0.e0) clon2=clon2+360.0
        if (clon1.gt.clon2) clon2=clon2+360.0
        delx=(clon2-clon1)/(cigrd1-1)
        dely=(clat2-clat1)/(cjgrd1-1)
!
        do j=1,cjgrd1
        lats = j
        ijlats = (lats-1)*cigrd1
          do i=1,cigrd1
            clon(ijlats+i) = ((i-1)*delx+clon1)*rad
            clon(ijlats+i) = mod(clon(ijlats+i),twopi)
! comment out 20220720
!            if (clon(ijlats+i).lt.0.e0) clon(ijlats+i)=                 &
!     &                                  twopi+clon(ijlats+i)
!            ! fix bug which domain covers 0E.
!            if (clon(ijlats+i).lt.clon(ijlats+1)) then
!                    clon(ijlats+i)=clon(ijlats+i)+twopi
!            endif
            clat(ijlats+i) = ((j-1)*dely+clat1)*rad
          enddo
        enddo
      endif
       print *,'nproj=',nproj
      call maxmin(clat,clngrd,1,1,1,'in setcgrid,blat')
      call maxmin(clon,clngrd,1,1,1,'in setcgrid,blon')
!
!
      return
      end
