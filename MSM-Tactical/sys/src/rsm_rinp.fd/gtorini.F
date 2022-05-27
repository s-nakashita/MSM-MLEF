      subroutine gtorini(gclat,flat,flon)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  g2rini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare global grid to regional grid interpolation by storing
!            the interpolation coefficient and grid arrangement.
!
! program history log:
!
! usage:     g2rini(gclat,flat,flon)
!   input argument list:
!     gclat - gaussian latitude of global in dimension of latg_
!     flat  - latitude of regional grids in dimension of lngrd_
!     flon  - longitude of regional grids in dimension of lngrd_
!
!   output argument list:
!
!   common blocks:
! comg2r
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
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
!cc
!
      dimension flat(lngrd),flon(lngrd),gclat(latg)
!
      common/comgtori/      n11(lngrd)
      common/comgtor/                                                    &         
     &          xc0(lngrd),xc1(lngrd),xc2(lngrd),xc3(lngrd)              &
     &         ,yc0(lngrd),yc1(lngrd),yc2(lngrd),yc3(lngrd)
!
      print 1234
 1234 format(' = in routine gtorini for horizontal interpolation = ')
!
      nlat = latg
      nlon = lonf
      hfpi = pi / 2.0
      twopi = pi * 2.0
! estimated dlat
      dlat = pi / float(nlat-1)
      dlon = twopi / float(nlon)
      n = 0
      do 1000 i=1,lngrd
      colat = hfpi - flat(i)
      lat =  colat / dlat + 1
      lon =  flon(i) / dlon + 1
! check lon
      if( lon .ge. 1  .and.  lon .le. nlon ) go to 2
       print *,' ***** error in lon ****'
       print *,' i ',i
       print *,' lon ',lon
       print *,' dlon ',dlon
       print *,' flon ',flon(i)
       stop
 2    continue
      lonp1 = lon + 1
! circle condition used for lonp1
      if( lon .eq. nlon ) lonp1 = 1
! check lat
      if( lat .ge. 1  .and.  lat .le. nlat ) go to 4
       print *,' ***** error in lat ****'
       print *,' i ',i
       print *,' lat ',lat
       print *,' dlat ',dlat
       print *,' flat ',flat(i)
       stop
 4    continue
      if( lat .eq. nlat ) lat=lat-1
 5    latp1 = lat + 1
      if( colat.ge.gclat(lat) .and. colat.le.gclat(latp1) ) go to 9
      if( colat.lt.gclat(lat) ) lat=lat-1
      if( lat .le. 0 ) then
         lat = 1
         latp1 = 2
         go to 9 
      endif
      if( colat.gt.gclat(latp1) ) lat=lat+1
      if( lat .ge. nlat ) then
         lat = nlat-1
         latp1 = nlat
         go to 9 
      endif
      go to 5
 9    continue
!
      ddlat = gclat(latp1) - gclat(lat)
      ddlon = dlon
      xlat = gclat(lat)
      xlon = (lon - 1) * ddlon
      dflat1 = (    colat - xlat   )/ddlat
! modify dflat1 for polar points
      if(lat.eq.1) dflat1=max(0.,dflat1)
      if(latp1.eq.nlat) dflat1=min(1.,dflat1)
!
      dflon1 = (flon(i) - xlon   )/ddlon
      n = n + 1
!
      n11(n) = lon   + (lat   -1)*lonf
      yr = dflat1 - 0.5
      b1 = 0.5*(yr*yr-0.25)
      yc3(n) = b1*(0.5+yr/3.)
      yc2(n) = (0.5+yr)*(1.-b1)
      yc1(n) = (0.5-yr)*(1.-b1)
      yc0(n) = b1*(0.5-yr/3.)
      if ( lat.eq.1) then
        yc3(n)=0.
        yc0(n)=0.
        yc1(n)=1.-dflat1
        yc2(n)=dflat1
      endif
      if ( lat.eq.nlat-1) then
        yc3(n)=0.
        yc0(n)=0.
        yc1(n)=1.-dflat1
        yc2(n)=dflat1
      endif

      xr = dflon1 - 0.5
      b1 = 0.5*(xr*xr-0.25)
      xc3(n) = b1*(0.5+xr/3.)
      xc2(n) = (0.5+xr)*(1.-b1)
      xc1(n) = (0.5-xr)*(1.-b1)
      xc0(n) = b1*(0.5-xr/3.)
!
1000  continue
!
!
      return
      end
