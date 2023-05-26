#include <define.h>
#ifdef MP
      subroutine rkenputr(ix,jx,lon1s,lon2s,                             & 
     &                       lones,                                      &
#else
      subroutine rkenputr(ix,jx,lon1,lon2,                               &
#endif
     &           latt1,latt2,sdec,                                       &
     &           slmskr,tsear,flwup,fswup,fswdn,                         &
     &           sswup,sswdn,slwup,slwdn,                                &
     &           cldsa,mtopa,mbota,                                      &
     &           itimsw,itimlw)
!cj  &           cldsa,mtopa,mbota,fluxr,                                &
!ccj
#include <paramodel.h>
#include <rscomgpd.h>
!
#ifdef MP
      integer ix,jx,lon1s,lon2s, lones,latt1,latt2
#else
      integer ix,jx,lon1,lon2,latt1,latt2
#endif
      integer itimsw,itimlw
      real sdec
      real slmskr(ix,jx),tsear(ix,jx),                                   &
     &     flwup(ix,jx),fswup(ix,jx),fswdn(ix,jx),                       &   
     &     sswup(ix,jx),sswdn(ix,jx),                                    &
     &     slwup(ix,jx),slwdn(ix,jx),                                    &
     &     cldsa(ix,5,jx)
      integer mtopa(ix,3,jx),mbota(ix,3,jx)                       
!cj  &     fluxr(ix,27,jx),                                    &
!cj  &     coszro(ix,jx),cldary(ix,levr,jx)

!
       do 300 igpt=1,npoint
         if(jgpd(igpt).ge.latt1.and.jgpd(igpt).le.latt2) then
#ifdef MP
         if((igpd(igpt).ge.lon1s.and.igpd(igpt).le.lon2s)) then
!     &      (igpd(igpt).ge.lon1n.and.igpd(igpt).le.lon2n)) then
           jlat=jgpd(igpt)-latt1+1
           if(igpd(igpt).ge.lon1s.and.igpd(igpt).le.lon2s) then
             ilon=igpd(igpt)-lon1s+1
!           else
!             ilon=igpd(igpt)-lon1n+1+lones
           endif
#else
         if(igpd(igpt).ge.lon1.and.igpd(igpt).le.lon2) then
           jlat=jgpd(igpt)-latt1+1
           ilon=igpd(igpt)-lon1+1
#endif
!  
           svdata( 25,igpt,itnum)= cldsa(ilon,3,jlat)
           svdata( 26,igpt,itnum)= cldsa(ilon,2,jlat)
           svdata( 27,igpt,itnum)= cldsa(ilon,1,jlat)
           svdata( 38,igpt,itnum)= cldsa(ilon,4,jlat)
           if(isshrt.lt.1.and.itimsw.eq.1) then
            svdata( 41,igpt,itnum)= igpd(igpt)
            svdata( 42,igpt,itnum)= jgpd(igpt)
            svdata( 43,igpt,itnum)= slmskr(ilon,jlat)
            svdata( 44,igpt,itnum)= tsear (ilon,jlat)

            svdata( 45,igpt,itnum)= sswdn(ilon,jlat)
            svdata( 46,igpt,itnum)= sswup(ilon,jlat)
            svdata( 48,igpt,itnum)= fswdn(ilon,jlat)
            svdata( 59,igpt,itnum)= slwup(ilon,jlat)   !add longwave up at sfc
            svdata( 49,igpt,itnum)= fswup(ilon,jlat)
            svdata( 60,igpt,itnum)= 0.-flwup(ilon,jlat) !add net long wave at top
            svdata( 51,igpt,itnum)= mtopa(ilon,3,jlat)
            svdata( 52,igpt,itnum)= mtopa(ilon,2,jlat)
            svdata( 53,igpt,itnum)= mtopa(ilon,1,jlat)
            svdata( 54,igpt,itnum)= mbota(ilon,3,jlat)
            svdata( 55,igpt,itnum)= mbota(ilon,2,jlat)
            svdata( 56,igpt,itnum)= mbota(ilon,1,jlat)
!            svdata( 57,igpt,itnum)= coszro(ilon,jlat)                        !commented by binbin
!            svdata( 58,igpt,itnum)= asin(sdec)*180.e0/3.14159265e0  !commented by binbin
           endif
           if(isshrt.lt.1.and.itimlw.eq.1) then
            svdata( 47,igpt,itnum)= slwdn(ilon,jlat)
            svdata( 50,igpt,itnum)= flwup(ilon,jlat)
            svdata( 61,igpt,itnum)= fswdn(ilon,jlat)-fswup(ilon,jlat) !add net short wave at top
           endif
!j           if (ilshrt.lt.1.and.itimsw.eq.1) then
!j            do 345 kc=1,levr
!j             cvcl = float(int(cldary(ilon,kc,jlat))/10)*1.e-3
!j             if (cvcl.gt.0.0e0) then
!j              svdata(kc+rslvark+7*levr,igpt,itnum) = cvcl
!j             else
!j              svdata(kc+rslvark+7*levr,igpt,itnum) =
!j     1              amod(cldary(ilon,kc,jlat),2.e0)
!j             endif
!j345         continue
!j           endif
!c
!         svdata(45,igpt,itnum)=fluxr(ilon,4,jlat)    !shortwave downward flux at sfc
!         svdata(46,igpt,itnum)=fluxr(ilon,3,jlat)    !shortwave upward flux at sfc
!         svdata(47,igpt,itnum)=fluxr(ilon,19,jlat)   !longwave downward flux at sfc
!         svdata(48,igpt,itnum)=fluxr(ilon,20,jlat)   !longwave upward flux at sfc
!         svdata(49,igpt,itnum)=0.-fluxr(ilon,1,jlat) !longwave net flux at top,  down +, up -
!         svdata(50,igpt,itnum)=fluxr(ilon,18,jlat)   !shortwave net flux at top, down +, up -
!     &                       - fluxr(ilon,2,jlat)
!         
         endif
         endif
300    continue
!
      return
      end
