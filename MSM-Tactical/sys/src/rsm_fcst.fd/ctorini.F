      subroutine ctorini(inic,flat,flon)
!
      use machine , only : kind_io4
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
!cc
      dimension flat(lngrd),flon(lngrd)
      dimension clat(cigrd1,cjgrd1)                                     &
     &         ,clon(cigrd1,cjgrd1)
      real(kind=kind_io4) clats(cigrd1,cjgrd1),clons(cigrd1,cjgrd1)
!
      common/comctori/      n11(lngrd)
      common/comctor/                                                   &
     &          xc0(lngrd),xc1(lngrd),xc2(lngrd),xc3(lngrd)             &
     &         ,yc0(lngrd),yc1(lngrd),yc2(lngrd),yc3(lngrd)
!
      print 1234
 1234 format(' = in routine b2rini for horizontal interpolation = ')
!
      hfpi = pi / 2.0
      twopi = pi * 2.0
      rewind inic
      nskip=3*levr+ntotal*levr+4+3
#ifdef N2R
      nskip=nskip+3*levr+1
#endif
      do nc=1,nskip
        read(inic)
      enddo
      read(inic) clats
      read(inic) clons
      call sgl2ful(clats,clat,cigrd1*cjgrd1)
      call sgl2ful(clons,clon,cigrd1*cjgrd1)
      call maxmin(clat,cigrd1*cjgrd1,1,1,1,'lat from c')
      call maxmin(clon,cigrd1*cjgrd1,1,1,1,'lon from c')
      call maxmin(flat,lngrd,1,1,1,'lat from r')
      call maxmin(flon,lngrd,1,1,1,'lon from r')
      call ll2xy(clat(1,1),clon(1,1),x00,y00)
      call ll2xy(clat(cigrd1,cjgrd1),clon(cigrd1,cjgrd1),x11,y11)
      cdelx=(x11-x00)/(cigrd1-1)
      cdely=(y11-y00)/(cjgrd1-1)
      print *,' c(1,1)=',x00,y00
      print *,' c(im,jm)=',x11,y11
      print *,' c delx dely =',cdelx,cdely
!
      n = 0
      do 1000 i=1,lngrd
        call ll2xy(flat(i),flon(i),x,y)
        xlon=(x-x00)/cdelx+1
        xlat=(y-y00)/cdely+1
        lon=xlon
        lat=xlat
        if( lon.lt.1 .or. lon.gt.cigrd1 ) then
           print *,x,' out side of ',x00,' and ',x11
           stop
        else if( lat.lt.1 .or. lat.gt.cjgrd1 ) then
           print *,y,' out side of ',y00,' and ',y11
           stop
        endif
        lonp1 = lon + 1
        latp1 = lat + 1
        dflat1 = xlat-lat
        dflon1 = xlon-lon
        dflat  = 1. - dflat1
        dflon  = 1. - dflon1
        n = n + 1
!
        n11(n) = lon   + (lat   -1)*cigrd1
        yr = dflat1 - 0.5
        b1 = 0.5*(yr*yr-0.25)
        yc3(n) = b1*(0.5+yr/3.)
        yc2(n) = (0.5+yr)*(1.-b1)
        yc1(n) = (0.5-yr)*(1.-b1)
        yc0(n) = b1*(0.5-yr/3.)
        xr = dflon1 - 0.5
        b1 = 0.5*(xr*xr-0.25)
        xc3(n) = b1*(0.5+xr/3.)
        xc2(n) = (0.5+xr)*(1.-b1)
        xc1(n) = (0.5-xr)*(1.-b1)
        xc0(n) = b1*(0.5-xr/3.)
!
1000    continue
!
!
      return
      end
