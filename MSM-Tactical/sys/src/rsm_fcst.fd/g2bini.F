      subroutine g2bini(gclat,flat,flon)
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
!     gclat     - gaussian latitude of global in dimension of latg
!     flat      - latitude of regional grids in dimension of lngrd
!     flon      - longitude of regional grids in dimension of lngrd
!
!   output argument list:
!
!   common blocks:
!       comg2r
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
#include <rsparltb.h>
!cc
      dimension flat(lngrdb),flon(lngrdb)                               &
     &         ,gclat(latg)
!
      common/comg2b/                                                    &
     &          n00(lngrdb),n10(lngrdb)                                 &
     &         ,n11(lngrdb),n01(lngrdb)                                 &
     &         ,d00(lngrdb),d10(lngrdb)                                 &
     &         ,d11(lngrdb),d01(lngrdb)
!
      save icall
      data icall/0/
!
      if( icall .eq. 0 ) then
      icall = 1
!      print 1234
 1234 format(' = in routine g2bini for horizontal interpolation = ')
!
      nlat = latb
      nlon = lonb
      hfpi = pi / 2.0
      twopi = pi * 2.0
! estimated dlat
      dlat = pi / float(nlat-1)
      dlon = twopi / float(nlon)
      n = 0
      do 1000 i=1,lngrdb
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
      dflat  = 1. - dflat1
      dflon  = 1. - dflon1
      n = n + 1
      d00(n) = dflon  * dflat
      d10(n) = dflon1 * dflat
      d11(n) = dflon1 * dflat1
      d01(n) = dflon  * dflat1
      n00(n) = lon   + (lat   -1)*nlon
      n10(n) = lonp1 + (lat   -1)*nlon
      n11(n) = lonp1 + (latp1 -1)*nlon
      n01(n) = lon   + (latp1 -1)*nlon
!
1000  continue
!
!     print *,' d00 to d11 should be all positive.'
!     call maxmin(d00,lngrdb,1,1,1,'d00')
!     call maxmin(d10,lngrdb,1,1,1,'d10')
!     call maxmin(d01,lngrdb,1,1,1,'d01')
!     call maxmin(d11,lngrdb,1,1,1,'d11')
!
      else
      print *,' g2bini already called '
      endif
!
      return
      end
