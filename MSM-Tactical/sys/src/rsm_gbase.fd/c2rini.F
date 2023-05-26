      subroutine c2rini(inic,rlat,rlon)
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
!   language: fortran 90.
!
!$$$
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
!cc
!
      dimension rlat(lngrd),rlon(lngrd)                                 &
     &         ,clat(cigrd1,cjgrd1)                                     &
     &         ,clon(cigrd1,cjgrd1)
      real(kind=kind_io4) clats(cigrd1,cjgrd1),clons(cigrd1,cjgrd1)
!
      common/comc2r/                                                    &
     &          n00(lngrd),n10(lngrd)                                   &
     &         ,n11(lngrd),n01(lngrd)                                   &
     &         ,d00(lngrd),d10(lngrd)                                   &
     &         ,d11(lngrd),d01(lngrd)
!
      save icall
      data icall/0/
!
      if( icall .eq. 0 ) then
      icall = 1
      print 1234
 1234 format(' = in routine c2rini for horizontal interpolation = ')
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
      call maxmin(rlat,lngrd,1,1,1,'lat from r')
      call maxmin(rlon,lngrd,1,1,1,'lon from r')
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
        call ll2xy(rlat(i),rlon(i),x,y)
        lon=(x-x00)/cdelx+1
        lat=(y-y00)/cdely+1
        if( lon.lt.1 .or. lon.gt.cigrd1 ) then
           print *,x,' out side of ',x00,' and ',x11
           stop
        else if( lat.lt.1 .or. lat.gt.cjgrd1 ) then
           print *,y,' out side of ',y00,' and ',y11
           stop
        endif
        call ll2xy(clat(lon  ,lat  ),clon(lon  ,lat  ),xc00,yc00)
        call ll2xy(clat(lon+1,lat+1),clon(lon+1,lat+1),xc11,yc11)
        call ll2xy(clat(lon+1,lat  ),clon(lon+1,lat  ),xc10,yc10)
        call ll2xy(clat(lon  ,lat+1),clon(lon  ,lat+1),xc01,yc01)
        r00=(x-xc00)*(x-xc00)+(y-yc00)*(y-yc00)
        r10=(x-xc10)*(x-xc10)+(y-yc10)*(y-yc10)
        r01=(x-xc01)*(x-xc01)+(y-yc01)*(y-yc01)
        r11=(x-xc11)*(x-xc11)+(y-yc11)*(y-yc11)
        if(r00.eq.0.0) then
          rsum=1.0
          r00=1.0
          r01=0.0
          r10=0.0
          r11=0.0
        elseif(r01.eq.0.0) then
          rsum=1.0
          r00=0.0
          r01=1.0
          r10=0.0
          r11=0.0
        elseif(r11.eq.0.0) then
          rsum=1.0
          r00=0.0
          r01=0.0
          r10=0.0
          r11=1.0
        elseif(r10.eq.0.0) then
          rsum=1.0
          r00=0.0
          r01=0.0
          r10=1.0
          r11=0.0
        else
          r00=1./r00
          r01=1./r01
          r10=1./r10
          r11=1./r11
          rsum=r00+r01+r10+r11
        endif
!
      n = n + 1
      d00(n) = r00 / rsum
      d10(n) = r10 / rsum
      d11(n) = r11 / rsum
      d01(n) = r01 / rsum
      n00(n) = lon   + (lat   -1)*cigrd1
      n10(n) = lon+1 + (lat   -1)*cigrd1
      n11(n) = lon+1 + (lat+1 -1)*cigrd1
      n01(n) = lon   + (lat+1 -1)*cigrd1
!
1000  continue
!
!     print *,' d00 to d11 should be all positive.'
!     call maxmin(d00,lngrd,1,1,1,'d00')
!     call maxmin(d10,lngrd,1,1,1,'d10')
!     call maxmin(d01,lngrd,1,1,1,'d01')
!     call maxmin(d11,lngrd,1,1,1,'d11')
!
      else
      print *,' c2rini already called '
      endif
!
      return
      end
