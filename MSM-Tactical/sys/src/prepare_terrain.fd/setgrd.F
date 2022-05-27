      subroutine setgrd(rlat,rlon,delx,dely,dlamda0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setgrd
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  setup grid latitude, longitude, and map factor etc for
!            regional grid.
!
! program history log:
!
! usage:    call setgrd(flat,flon,delx,dely,dlamda0)
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
!$$$
      use comrloc
      parameter( rerth = 6371220. )
!
      dimension rlat(im,jm),rlon(im,jm)
!c
      print 1234
 1234 format(' ==== in routine setgrd === ')
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
      do 100 j = 1,jm
      lat = j
      y = y00 + (lat-1)*dely
!
      do 100 i=1,im
      x = x00 + (i-1)*delx
      if( x .gt. 0. ) then
         flon = atan(y/x)
      else if ( x .lt. 0. ) then
         flon = pi + atan(y/x)
      else
         flon = hfpi
         if( y .lt. 0. ) flon = flon * 3.0
      endif
      flon = nproj * flon + dlamda0
      flon = mod(flon,twopi)
      if(flon.lt.0.) flon = twopi + flon
!
      rsoa2 = sqrt( x*x + y*y )/a2
      flat = hfpi - 2.00 * atan(rsoa2)
      rlat(i,j) = nproj * flat
      rlon(i,j) = flon
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
!
      do 200 j = 1,jm
      lat = j
      y = y00 + (lat-1)*dely + y0
!
       do 200 i=1,im
         x = x00 + (i-1)*delx + x0
         flon = x / a2 + cenlon
         flon = mod(flon,twopi)
         if(flon.lt.0.0) flon = twopi + flon
!
         flat = 2.0 *( atan( exp( y/a2 ) ) - qtpi )
         rlat(i,j) = flat
         rlon(i,j) = flon
!
 200  continue
!
      endif
!
!
      return
      end
