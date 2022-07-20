#include <define.h>
      subroutine c2bini(inic,blat,blon                                  &
#ifdef MP
     &           ,mype,master                                           &
#endif
     &            )
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
!     gclat     - gaussian latitude of global in dimension of latg_                 
!     flat      - latitude of regional grids in dimension of lngrd_                  
!     flon      - longitude of regional grids in dimension of lngrd_                 
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
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
#include <rsparltb.h>
!cc
      dimension blat(lngrdb),blon(lngrdb)				&
     &         ,clat(cigrd1,cjgrd1)				        &
     &         ,clon(cigrd1,cjgrd1)                                    
      real(kind=kind_io4) clats(cigrd1,cjgrd1),clons(cigrd1,cjgrd1)
!
      common/comc2b/                                                    &
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
#ifdef MP
      if (mype.eq.master) then
#endif
      print 1234
 1234 format(' = in routine c2bini for horizontal interpolation = ')
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
        lon=(x-x00)/cdelx+1
        lat=(y-y00)/cdely+1
        if( lon.lt.1 .or. lon.gt.cigrd1 ) then
#ifdef MP
           print *, mype
#endif
           print *,x,' out side of ',x00,' and ',x11
           print *,i,' blon=',blon(i)
           stop
        else if( lat.lt.1 .or. lat.gt.cjgrd1 ) then
#ifdef MP
           print *, mype
#endif
           print *,y,' out side of ',y00,' and ',y11
           print *,i,' blat=',blat(i)
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
!     call maxmin(d00,lngrdb,1,1,1,'d00')
!     call maxmin(d10,lngrdb,1,1,1,'d10')
!     call maxmin(d01,lngrdb,1,1,1,'d01')
!     call maxmin(d11,lngrdb,1,1,1,'d11')
!
      else
#ifdef MP
      if (mype.eq.master) then
#endif
      print *,' c2bini already called '
#ifdef MP
      endif
#endif
      endif
!
      return
      end
