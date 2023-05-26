!modified by binbin zhou: jan. 2003 
!  1. add presure profile   
!  2. change 51->6: from number of layers to presure
!  3. change presure unit from mb to pa
#include <define.h>
#ifdef MP
      subroutine rkenput(ix,jx,lon1s,lon2s,                              &   
     &                       lones,                                      &
#else
      subroutine rkenput(ix,jx,lon1,lon2,                                &
#endif
     &           latt1,latt2,rcl,                                        &
     &           slmsk1,psexp,tg31,sheleg1,radsl,dlwsf1,                 &
     &           tsea1,qss,gflx,zorl1,cd,cdq,                            &
     &           evap,hflx,stsoil,                                       &
     &           canopy1,drain,smsoil,runof,cld1d,                       &
     &           u10,v10,t2,q2,                                          &
     &           hpbl1,gamt,gamq,                                        &
     &           dqsfc1,dtsfc1,dusfc1,dvsfc1,                            &
     &           dusfcg,dvsfcg,                                          &
     &           rainc,rainl,                                            &
     &           u,v,t,q,hsw,hlw1,vvel,sl,                               &
     &           vfrac,tmpmin,tmpmax,                                    &
     &           bengsh,geshem)
!
      use physcons, jcal => con_jcal
!ccj
#include <paramodel.h>
#include <rscomgpd.h>
!
#ifdef MP
      integer ix,jx,km,lon1s,lon2s, lones,latt1,latt2
#else
      integer ix,jx,km,lon1,lon2,latt1,latt2
#endif
      parameter(km=levr)
      parameter(cnwatt=-jcal*1.e4/60.)
      real slmsk1(ix,jx),psexp(ix,jx),tg31(ix,jx),                       & 
     &     sheleg1(ix,jx),radsl(ix,jx),                                  &
     &     dlwsf1(ix,jx),tsea1(ix,jx),qss(ix,jx),gflx(ix,jx),            &
     &     zorl1(ix,jx),cd(ix,jx),                                       &
     &     cdq(ix,jx),evap(ix,jx),                                       &
     &     hflx(ix,jx),stsoil(ix,lsoil,jx),                              &
     &     canopy1(ix,jx),drain(ix,jx),smsoil(ix,lsoil,jx),              &
     &     runof(ix,jx),cld1d(ix,jx),                                    &
     &     u10(ix,jx),v10(ix,jx),t2(ix,jx),q2(ix,jx),                    &
     &     hpbl1(ix,jx),gamt(ix,jx),gamq(ix,jx),                         &
     &     dqsfc1(ix,jx),dtsfc1(ix,jx),dusfc1(ix,jx),dvsfc1(ix,jx),      &
     &     dusfcg(ix,jx),dvsfcg(ix,jx),                                  &
     &     rainc(ix,jx),rainl(ix,jx),                                    &
     &     u(ix,km,jx),v(ix,km,jx),t(ix,km,jx),q(ix,km*ntotal,jx),       &
     &     hsw(ix,km,jx),hlw1(ix,km,jx),vvel(ix,km,jx),sl(km),           &
     &     vfrac(ix,jx),tmpmin(ix,jx),tmpmax(ix,jx),                     &
     &     bengsh(ix,jx),geshem(ix,jx) 
!
       do 1550 igpt=1,npoint
         if(jgpd(igpt).ge.latt1.and.jgpd(igpt).le.latt2) then
#ifdef MP
         if((igpd(igpt).ge.lon1s.and.igpd(igpt).le.lon2s) ) then
!     &      (igpd(igpt).ge.lon1n.and.igpd(igpt).le.lon2n)) then
           jlat=jgpd(igpt)-latt1+1
           if(igpd(igpt).ge.lon1s.and.igpd(igpt).le.lon2s) then
             ilon=igpd(igpt)-lon1s+1
           endif
#else
         if(igpd(igpt).ge.lon1.and.igpd(igpt).le.lon2) then
           jlat=jgpd(igpt)-latt1+1
           ilon=igpd(igpt)-lon1+1
#endif
!  
       svdata( 11,igpt,itnum)=bengsh(ilon,jlat) !cumu convective rain(unit:meters), old: non-cumu
       svdata( 12,igpt,itnum)=geshem(ilon,jlat) !cumu large(grid) scale rain(unit:meters), old: non-cumu
!
       svdata(  1,igpt,itnum)= igpd(igpt)            !ith grid in x direction
       svdata(  2,igpt,itnum)= jgpd(igpt)            !jth grid in y direction
       svdata(  3,igpt,itnum)= slmsk1(ilon,jlat)     !sea&land mask:sea=0, land=1, ice on ocean=2
       svdata(  4,igpt,itnum)= psexp(ilon,jlat)*10.  !presure on surface (mb)
       svdata(  5,igpt,itnum)= tsea1(ilon,jlat)      !surface (skin) temperature (k)
       svdata(  6,igpt,itnum)= stsoil(ilon,1,jlat)   !soil t in soil layer 1 (k)
       svdata(  7,igpt,itnum)= stsoil(ilon,2,jlat)   !soil t in soil layer 2 (k)
       svdata(  8,igpt,itnum)= tg31(ilon,jlat)       !soil t in deep soil (k)
       svdata(  9,igpt,itnum)= smsoil(ilon,1,jlat)   !soil moisture (kg/kg)
       svdata( 10,igpt,itnum)= sheleg1(ilon,jlat)    !cumu snow depth (mm) since beginning, left 1 hr cumu to post-process 
!-rkn  11-12 accumulated above
       svdata(13,igpt,itnum)= 0.0                    !new:sea level presure, unavailable
       svdata(14,igpt,itnum)= tmpmin(ilon,jlat)      !new:min temperature at surface (k)
       svdata(15,igpt,itnum)= tmpmax(ilon,jlat)      !new:max temperature at surface (k)
       svdata(16,igpt,itnum)= gpdgzs(igpt)           !new:geographic height: m
!
!-rkn  dqsfc, dtsfc, gflux are accumulated to within 3600*itnum seconds
!-rkn  eg. svdata(17,igpt,1)=dqsfc(itnum=1)
!-rkn      svdata(17,igpt,2)=dqsfc(itnum=1)+dqsfc(itnum=2)
!-rkn      svdata(17,igpt,3)=dqsfc(itnum=1)+dqsfc(itnum=2)+dqsfc(itnum=3)
!-rkn  same thing for radiation flux    
!
       svdata(17,igpt,itnum)= dqsfc1(ilon,jlat)     !latent heat flux (w/m2), acumu since beginning, old is no-cumu
       svdata(18,igpt,itnum)= dtsfc1(ilon,jlat)     !sensible heat flux (w/m2),acumu since beginning, old is no-cumu
       svdata(19,igpt,itnum)= gflx(ilon,jlat)       !ground heat flux(w/m2), acumu since beginning, old is no-acumu
       svdata(20,igpt,itnum)= 0.0                   !new:potential latent heat flux(w/m**2), unavailable
       svdata(21,igpt,itnum)= 0.0                   !new:soil moisture available(%), unavailable
       svdata(22,igpt,itnum)= zorl1(ilon,jlat)      !roughness length      (cm)
       svdata(23,igpt,itnum)= cd  (ilon,jlat)       !momentum exchange coef(none),eta: sfc. exchange coef (m/s)
       svdata(24,igpt,itnum)= 0.0                   !new:flux of snow phase change (w/m*2), unavailable
!
!-rkn  25-27 used in rgrrad2
!-rkn  25-27 used in grrad (set in rsloopr)
!  svdata(25,igpt,itnum)= cldsa(ilon,jlat,3)        !high  cloud covrage(%)
!  svdata(26,igpt,itnum)= cldsa(ilon,jlat,2)        !mid   cloud covrage(%)
!  svdata(27,igpt,itnum)= cldsa(ilon,jlat,1)        !lower cloud covrage(%)
!
       svdata(28,igpt,itnum)= 0.0                   !new:10m potential tempurature, unavailable
       svdata(29,igpt,itnum)= 0.0                   !new:10m specific humidity, unavailable
       svdata(30,igpt,itnum)= t2  (ilon,jlat)       !2 m temperature  (k)
       svdata(31,igpt,itnum)= q2  (ilon,jlat)       !2 m specific humidity (kg/kg)
       svdata(32,igpt,itnum)= canopy1(ilon,jlat)    !canopy water content
       svdata(33,igpt,itnum)= drain(ilon,jlat)      !soil water drain (mm/s), no-acumu (hard to acumu in code)
       svdata(34,igpt,itnum)= u10 (ilon,jlat)       !10 m u (m/s)
       svdata(35,igpt,itnum)= v10 (ilon,jlat)       !10 m v (m/s)
!      svdata(36,igpt,itnum)= sl(int(svdata( 56,igpt, !presure of low cloud base (mb), note svdata(56,) was computed
!    &                itnum)))*10.0*psexp(ilon,jlat)  !as cloud base level in grrad, here just use it to transfer
       svdata(37,igpt,itnum)= idstat(igpt)           !new: station id
!
!-rkn  38 - 39  do not use.
! 41 - 50 computed in grrad (set in rsloopr)
!    svdata( 41,igpt,itnum)= id                   
!    svdata( 42,igpt,itnum)= lat
!    svdata( 43,igpt,itnum)= slmskr(id)
!    svdata( 44,igpt,itnum)= tsear (id)
!   do igpt=1,npoint
!    svdata(45,igpt,itnum)=shortwave downward flux at sfc,cumu,old-verion is no-cumu
!    svdata(46,igpt,itnum)=shortwave upward flux at sfc,cumu,old-verion is no-cumu
!    svdata(47,igpt,itnum)=longwave downward flux at sfc,cumu,old-verion is no-cumu
!    svdata(59,igpt,itnum)=longwave upward flux at sfc,cumu,old-verion is no-cumu
!    svdata(60,igpt,itnum)=longwave net flux at top,  down +,cumu, old is no-cumu
!    svdata(61,igpt,itnum)=shortwave net flux at top, down +,cumu, old is no-cumu
!   &                   - fluxr(ilon,jlat(igpt),jpgd(jgpt), 2)
!    svdata(48,igpt,itnum)=short wave downward flux at top, will not be put into bufr
!    svdata(49,igpt,itnum)=short wave upward at top, will not be put into bufr
!    svdata(50,igpt,itnum)=longwave upward flux at top, will not be put into bufr
!   end do
!
       svdata(62, igpt,itnum)= smsoil(ilon,1,jlat)*10/200                & !new: total soil moisture (kg/kg)
     &                       + smsoil(ilon,2,jlat)*190/200
       svdata(63,igpt,itnum)= 0.0                      !new:snow water equivalent, unavailable
       svdata(64,igpt,itnum)= 0.0                      !new:snow melt (mm), acumu since begin, unavailable
       svdata(65,igpt,itnum)= runof (ilon,jlat)        !new:surface runoff (mm/s), no-acumu (hard to acumu)
       svdata(66,igpt,itnum)= vfrac(ilon,jlat)         !new:vegetation coverage (%)
       svdata(67,igpt,itnum)= smsoil(ilon,2,jlat)      !soil moisture in layer 2  (kg/kg)
       svdata(68,igpt,itnum)= 0.0                      !snow ration from explicity cloud scheme, unavailable
!
!-rkn  62 - 69 not used
!
!-rkn 70-77 are computed outside of rsm model, done in bufr process rsm_bufr.f
!-rkn  svdata(70,j,k)=csno                                  !type of snow
!-rkn  svdata(71,j,k)=cice                                  !type of ice  
!-rkn  svdata(72,j,k)=cfzr                                  !type of frozen rain
!-rkn  svdata(73,j,k)=crai                                  !type of rain
!-rkn  svdata(75,j,k)=ust                                   !u of storm
!-rkn  svdata(76,j,k)=vst                                   !v of storm
!-rkn  svdata(77,j,k)=heli                                  !helicity of storm
!
!-rkn  78-79 not used
!
       if(ilshrt.lt.2) then
       rr=sqrt(rcl)
       do 331 k=1,levr
       svdata(k+rslvark+0*levr,igpt,itnum)=u(ilon,k,jlat)*rr
       svdata(k+rslvark+1*levr,igpt,itnum)=v(ilon,k,jlat)*rr
       svdata(k+rslvark+2*levr,igpt,itnum)=t(ilon,k,jlat)
       svdata(k+rslvark+3*levr,igpt,itnum)=q(ilon,k,jlat)
       if(ilshrt.lt.1) then
        svdata(k+rslvark+4*levr,igpt,itnum)=hsw(ilon,k,jlat)
        svdata(k+rslvark+5*levr,igpt,itnum)=hlw1(ilon,k,jlat)
        svdata(k+rslvark+6*levr,igpt,itnum)=vvel(ilon,k,jlat)
       endif
! binbin:  new add presure profile
       svdata(k+rslvark+10*levr,igpt,itnum)=psexp(ilon,jlat)*            & !unit: mb
     &                                       10.0*sl(k)
       kk=k+levr*2
       svdata(k+rslvark+11*levr,igpt,itnum)=q(ilon,kk,jlat)      !new: cloud water, unavailable
       svdata(k+rslvark+12*levr,igpt,itnum)=0.0                  !new: conv latent heat rate, unavailable
       svdata(k+rslvark+13*levr,igpt,itnum)=0.0                 !new: stable latent heat rate, unavailable
       svdata(k+rslvark+14*levr,igpt,itnum)=0.0                 !new: cloud cover in a layer, unavailable
       svdata(k+rslvark+15*levr,igpt,itnum)=0.0                 !new: turb ke in a layer , unavailable
331    continue
       endif
!
         endif
         endif
 1550    continue
!
      return
      end
