#include <define.h>
      subroutine ctobini(inic,blat,blon                                 &
#ifdef MP
     &           ,mype,master                                           &
#endif
     &            )
!
      use machine , only : kind_io4
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
#include <rsparltb.h>
!cc
      dimension blat(lngrdb),blon(lngrdb)                               &
     &         ,clat(cigrd1,cjgrd1)                                     &
     &         ,clon(cigrd1,cjgrd1)
      real(kind=kind_io4) clats(cigrd1,cjgrd1),clons(cigrd1,cjgrd1)
!
      common/comctobi/      n11(lngrdb)
      common/comctob/                                                   &
     &          xc0(lngrdb),xc1(lngrdb),xc2(lngrdb),xc3(lngrdb)         &
     &         ,yc0(lngrdb),yc1(lngrdb),yc2(lngrdb),yc3(lngrdb)
!
#ifdef MP
      if (mype.eq.master) then
#endif
      print 1234
 1234 format(' = in routine ctobini for horizontal interpolation = ')
!
      hfpi = pi / 2.0
      twopi = pi * 2.0
      rewind inic
      nskip=3*levr+(1+ntoz+ncldb)*levr+4+3
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
      call maxmin(blat,lngrdb,1,1,1,'lat from b')
      call maxmin(blon,lngrdb,1,1,1,'lon from b')
      call ll2xy(clat(1,1),clon(1,1),x00,y00)
      call ll2xy(clat(cigrd1,cjgrd1),clon(cigrd1,cjgrd1),x11,y11)
      cdelx=(x11-x00)/(cigrd1-1)
      cdely=(y11-y00)/(cjgrd1-1)
      print *,' c(1,1)=',x00,y00
      print *,' c(im,jm)=',x11,y11
      print *,' c delx dely =',cdelx,cdely
#ifdef MP
      endif
      call rmpbcastr(x00,1)
      call rmpbcastr(y00,1)
      call rmpbcastr(x11,1)
      call rmpbcastr(y11,1)
      call rmpbcastr(cdelx,1)
      call rmpbcastr(cdely,1)
      call rmpbcastr(clat,cigrd1*cjgrd1)
      call rmpbcastr(clon,cigrd1*cjgrd1)
#endif
!
      n = 0
      do 1000 i=1,lngrdb
        call ll2xy(blat(i),blon(i),x,y)
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
        dblat1 = xlat-lat
        dblon1 = xlon-lon
        dblat  = 1. - dblat1
        dblon  = 1. - dblon1
        n = n + 1
!
        n11(n) = lon   + (lat   -1)*cigrd1
        yr = dblat1 - 0.5
        b1 = 0.5*(yr*yr-0.25)
        yc3(n) = b1*(0.5+yr/3.)
        yc2(n) = (0.5+yr)*(1.-b1)
        yc1(n) = (0.5-yr)*(1.-b1)
        yc0(n) = b1*(0.5-yr/3.)
        xr = dblon1 - 0.5
        b1 = 0.5*(xr*xr-0.25)
        xc3(n) = b1*(0.5+xr/3.)
        xc2(n) = (0.5+xr)*(1.-b1)
        xc1(n) = (0.5-xr)*(1.-b1)
        xc0(n) = b1*(0.5-xr/3.)
!
1000  continue
!
      return
      end
