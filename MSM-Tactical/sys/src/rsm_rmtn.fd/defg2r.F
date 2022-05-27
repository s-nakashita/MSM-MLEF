!cccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine defg2r(flat,flon,imd,jmd,lngrd,                         &    
     &        n00,n01,n10,n11,d00,d01,d10,d11)
!cccccccccccccccccccccccccccccccccccccccccccccccc
!
      dimension flat(lngrd),flon(lngrd)                                  &
     &         ,n00(lngrd),n01(lngrd),n10(lngrd),n11(lngrd)              &
     &         ,d00(lngrd),d01(lngrd),d10(lngrd),d11(lngrd)
!
      hfpi=dasin(1.0d0)
      pi=2.0*hfpi
      twopi=2.0*pi
      qtpi=0.5*hfpi
      rad=pi/180.
!
      nlon = imd
      nlat = jmd
!
      dlatdeg=180./nlat
      dlondeg=360./nlon
      dlat = pi * dlatdeg / 180.
      dlon = pi * dlondeg / 180.
      glat0 = dlat / 2.0
      glon0 = dlon / 2.0
!
      n = 0
      do 1000 i=1,lngrd
      solat = hfpi + flat(i)
      lat =  (solat  -glat0) / dlat + 1
      lon =  (flon(i)-glon0) / dlon + 1
! check lat
      if( lat .le. 0 ) then
         lat = 1
      endif
      if( lat .ge. nlat ) then
         lat = nlat-1
      endif
      latp1 = min( lat + 1, nlat )
      latp2 = min( lat + 2, nlat )
      latm1 = max( lat - 1, 1    )
!
      xlat = glat0+(lat-1)*dlat
      dflat1 = (solat-xlat)/dlat
! modify dflat1 for polar points
      if(lat.eq.1) dflat1=max(0.,dflat1)
      if(latp1.eq.nlat) dflat1=min(1.,dflat1)
!
      xlon = glon0+(lon-1)*dlon
      dflon1 = (flon(i) - xlon   )/dlon
      dflat  = 1. - dflat1
      dflon  = 1. - dflon1
      n = n + 1
      d00(n) = dflon  * dflat
      d10(n) = dflon1 * dflat
      d11(n) = dflon1 * dflat1
      d01(n) = dflon  * dflat1
! check lon after d00 d01 d10 d11 setup.
      lonp1 = lon + 1
      lonp2 = lon + 2
      lonm1 = lon - 1
      if( lon   .gt. nlon ) lon=lon-nlon
      if( lonm1 .gt. nlon ) lonm1=lonm1-nlon
      if( lonp1 .gt. nlon ) lonp1=lonp1-nlon
      if( lonp2 .gt. nlon ) lonp2=lonp2-nlon
      if( lon   .lt. 1 ) lon=lon+nlon
      if( lonm1 .lt. 1 ) lonm1=lonm1+nlon
      if( lonp1 .lt. 1 ) lonp1=lonp1+nlon
      if( lonp2 .lt. 1 ) lonp2=lonp2+nlon
!
!      naa(n) = lonm1 + (latm1 -1)*nlon
!      na0(n) = lonm1 + (lat   -1)*nlon
!      na1(n) = lonm1 + (latp1 -1)*nlon
!      na2(n) = lonm1 + (latp2 -1)*nlon
!      n0a(n) = lon   + (latm1 -1)*nlon
      n00(n) = lon   + (lat   -1)*nlon
      n01(n) = lon   + (latp1 -1)*nlon
!      n02(n) = lon   + (latp2 -1)*nlon
!      n1a(n) = lonp1 + (latm1 -1)*nlon
      n10(n) = lonp1 + (lat   -1)*nlon
      n11(n) = lonp1 + (latp1 -1)*nlon
!      n12(n) = lonp1 + (latp2 -1)*nlon
!      n2a(n) = lonp2 + (latm1 -1)*nlon
!      n20(n) = lonp2 + (lat   -1)*nlon
!      n21(n) = lonp2 + (latp1 -1)*nlon
!      n22(n) = lonp2 + (latp2 -1)*nlon
!
1000  continue
!
      print *,' d00 to d11 should be all positive.'
      call maxmin(d00,lngrd,1,1,1,'d00')
      call maxmin(d10,lngrd,1,1,1,'d10')
      call maxmin(d01,lngrd,1,1,1,'d01')
      call maxmin(d11,lngrd,1,1,1,'d11')
!
      return
      end
