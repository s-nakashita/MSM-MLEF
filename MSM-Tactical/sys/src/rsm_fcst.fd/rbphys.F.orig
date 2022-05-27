#include <define.h>
#define doDCYC2
#define doPROGTM
#define doMONINP
#define doGWDPS
#define doOZPHYS
#define doSASCNV
#define doLRGSCL
#define doPROGT2
      subroutine rbphys(dtp,dtf,xkt2,                                    &   
#ifndef NONHYD
     & plamgrs,pphigrs,                                                  &
#endif
     & ugrs,vgrs,pgr,tgrs,qgrs,                                          &
#ifndef NONHYD
     & xgrs,                                                             &
#endif
     & gt0,gq0,gu0,gv0,                                                  &
#ifdef NONHYD
     & pgrs,wgrs,gp0,gw0,tgrh,gh0,                                       &
#endif

#ifdef RAS
     & rannum,flipv, ncrnd,                                              &
#endif
#ifdef RKN
     & psexp,radsl,                                                      &
     & dlwsf1,qss,                                                       &
     & gflx,cd,                                                          &
     & cdq,evap,                                                         &
     & hflx,drain,                                                       &
     & runof,cld1d,                                                      &
     & gamt,gamq,                                                        &
     & dqsfc1,dtsfc1,                                                    &
     & dusfc1,dvsfc1,                                                    &
     & dusfcg,dvsfcg,                                                    &
     & rainc,rainl,                                                      &
     & hsw,vvel,                                                         &
     & stsoil,smsoil,                                                    &
#endif
     & phy_f3dv,phy_f2dv, me,                                            &
     & lonlens,igrd1s,                                                   &
     & lat)
!x   & lat,ptotj,pwatj)
!fpp$ noconcur r
!
      use machine , only : kind_phys
      use funcphys , only : fpkap
      use physcons, rocp => con_rocp, fv => con_fvirt                    & 
     &,             grav => con_g, cp => con_cp, rd => con_rd            &
     &,             rv => con_rv, hvap => con_hvap,  hfus => con_hfus    &
     &,             rerth => con_rerth                                   &
#ifdef NONHYD
     &,             ccv => con_cv                                        &
#endif
     &,             pi => con_pi                                         &
     &,             cvap => con_cvap, t0c => con_t0c, rdorv => con_eps   &
     &,             rhoair0 => con_rhoair0, rhoh2o => con_rhoh2o         &
     &,             cliq => con_cliq, cice => con_csol, psat => con_psat
      use module_mp_wsm3
      use module_mp_wsm5
      use module_mp_wsm6
!
!ccj
#include <paramodel.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscomloc.h>
!
#include <rscommap.h>
#include <rscomgpd.h>
#include <rscombgt.h>
!
      dimension  xkt2(igrd1s)
!
      integer ncw,lat,jcapr
!cc
!...................................................................
! warning: gt0,gq0,gu0,gv0 may overlay tgrs,qgrs,ugrs,vgrs respectively.
#ifndef NONHYD
      dimension  plamgrs(igrd1s)
      dimension  pphigrs(igrd1s)
#endif
      dimension     ugrs(igrd1s,levr)
      dimension     vgrs(igrd1s,levr)
      dimension      pgr(igrd1s)
      dimension     tgrs(igrd1s,levr)
      dimension     qgrs(igrd1s,levh),qgrsx(igrd1s,levr,ntotal)
#ifndef NONHYD
      dimension     xgrs(igrd1s,levr)
#endif
      dimension      gt0(igrd1s,levr)
      dimension      gq0(igrd1s,levh)
      dimension      gu0(igrd1s,levr)
      dimension      gv0(igrd1s,levr)
#ifdef NONHYD
      dimension     pgrs(igrd1s,levr)
      dimension     wgrs(igrd1s,levr+1)
      dimension      gp0(igrd1s,levr)
      dimension      gw0(igrd1s,levr+1)
      dimension     tgrh(igrd1s,levr)
      dimension      gh0(igrd1s,levr)
#endif
      dimension      sik(levr+1)
      dimension      slk(levr)
!ccj
      dimension  phy_f3dv(igrd1s,levr,num_p3d),                          &
     &           phy_f2dv(igrd1s,num_p2d)
!c
      real(kind=kind_phys) prsl(igrd1s,levr),  prsi(igrd1s,levr+1)       &   
     &,    prslk(igrd1s,levr)                                            &
     &,    prsik(igrd1s,levr+1)                                          &
     &,    phii(igrd1s,levr+1), phil(igrd1s,levr)                        &
     &,    delp(igrd1s,levr)
!c
!...................................................................
      real,parameter ::    epsq=1.e-12, hsub=hvap+hfus
      dimension kbot(igrd1s),ktop(igrd1s),kuo(igrd1s),                   &   
     &dudt(igrd1s,levr),dvdt(igrd1s,levr),                               &
     &dtdt(igrd1s,levr),dqdt(igrd1s,levh),dqdtx(igrd1s,levr,ntotal),     &
#ifdef NONHYD
     & dwdt(igrd1s,levr+1),                                             &
#endif
     & hsw(igrd1s,levr),gflx(igrd1s),                                    &
     & rain(igrd1s),rainc(igrd1s),rainl(igrd1s),rain1(igrd1s),           &
     & evapc(igrd1s),wind(igrd1s),kpbl(igrd1s),                          &
     & w2(igrd1s,levr),vvel(igrd1s,levr)
! for wsm
      real                 ::  ww(igrd1s,levr)
      real                 ::  zl(igrd1s,levr)
      real                 ::  zi(igrd1s,levr+1)
      real                 ::  delz(igrd1s,levr)
      real                 ::  prsl_pa(igrd1s,levr)
      real                 ::  rainncv(igrd1s)
      real                 ::  snow1(igrd1s,levr)
      real                 ::  graupel1(igrd1s,levr)
      real                 ::  snowncv(igrd1s,levr)
      real                 ::  graupelncv(igrd1s,levr)

!.................................................................
      dimension  psexp(igrd1s),                                          &
     &          snowmt(igrd1s),                                          &
     &             fm1(igrd1s),   fh1(igrd1s),                           &
     &              cd(igrd1s),   cdq(igrd1s),                           &
     &             qss(igrd1s), radsl(igrd1s)
      dimension dusfcg(igrd1s),dvsfcg(igrd1s)
      dimension dusfc1(igrd1s),dvsfc1(igrd1s),                           &
     &          dtsfc1(igrd1s),dqsfc1(igrd1s),                           &
     &          dlwsf1(igrd1s),ulwsf1(igrd1s)
      dimension smsoil(igrd1s,lsoil),stsoil(igrd1s,lsoil)
      dimension soiltyp(igrd1s),sigmaf(igrd1s),vegtype(igrd1s)
      dimension rb(igrd1s),rhscnpy(igrd1s)
      dimension ai(igrd1s,lsoil),bi(igrd1s,lsoil),drain(igrd1s)
      dimension cci(igrd1s,lsoil),rhsmc(igrd1s,lsoil),runof(igrd1s)
      dimension zsoil(igrd1s,lsoil),cld1d(igrd1s)
      dimension evap(igrd1s),hflx(igrd1s),stress(igrd1s)
!c    dimension evap(igrd1s),hflx(igrd1s),rnet(igrd1s)
      dimension t850(igrd1s),ep1d(igrd1s)
      dimension gamt(igrd1s),gamq(igrd1s)
      dimension ustar1(igrd1s)
!c
      integer lonfequiv
      dimension var(igrd1s),oc(igrd1s),oa4(igrd1s,4),clx(igrd1s,4)
      real(kind=kind_phys) theta(igrd1s),gamma(igrd1s)
      real(kind=kind_phys) sigma(igrd1s),elvmax(igrd1s)
      real(kind=kind_phys) qr_col(igrd1s,levr), fc_ice(igrd1s,levr)
      real(kind=kind_phys) work1(igrd1s),work2(igrd1s)
      real(kind=kind_phys) rhc(igrd1s,levr),clstp
      real(kind=kind_phys) sr(igrd1s),xncw(igrd1s)
      real(kind=kind_phys) work3(igrd1s,levr),ptem,ptem1
      real(kind=kind_phys) qmin, rhzbot, rhztop
      real(kind=kind_phys) rhbbot, rhbtop, rhpbl
      real(kind=kind_phys) dxmax, dxmin, dxinv, cb2mb, tempv
#ifdef RAS
      logical flipv
#endif
      parameter (qmin=1.0e-10, rhzbot=0.85, rhztop=0.85)   ! zhao micro
      parameter (rhpbl=0.95,   rhbbot=0.95, rhbtop=0.90)   ! brad micro
!     parameter (              rhbbot=0.95, rhbtop=0.85)   ! brad micro
!     parameter (qmin=1.0e-10, rhcbot=0.85, rhctop=0.85)
!     parameter (qmin1=1.0e-30)
!     parameter (dxmax=log(1.0/7200.0), dxmin=log(1.0/192.0)
!     parameter (dxmax=-8.8818363, dxmin=-5.2574954
!    &,          dxinv=1.0/(dxmax-dxmin))
      parameter (dxmax=-9.5468126, dxmin=-5.2574954                      &  
     &,          dxinv=1.0/(dxmax-dxmin))
      parameter (cb2mb=10.0)
      real(kind=kind_phys) work4, dpshc, p850
      real(kind=kind_phys) frain, qi, qw, qr, wc, f_rain, f_ice
!c
      real(kind=kind_phys) rclx(igrd1s),rcs(igrd1s)
!cc
      integer soiltyp, vegtype
      integer me, lonlens, igrd1s, ipr
!ccj
#ifdef RAS
      integer ncrnd
      real(kind=kind_phys) rannum(ncrnd), rarea(igrd1s)
#endif
      real(kind=kind_phys) clw(igrd1s,levr,2)
      real(kind=kind_phys) xmu(igrd1s)
!cc
      integer kinver(igrd1s)
      logical ldiag
      logical lprnt, invrsn(igrd1s)
!........................................
!ccj
      lprnt = .false.
      ipr = 1
!
      nvdiff = ntotal
!cccccccccccccccccccccccccccccc
!c
      do k=1,levr+1
         sik(k)=si(k)**rocp
      enddo
      do k=1,levr
         slk(k)=sl(k)**rocp
      enddo
!c
      dpshc = 30.0                ! in cbar
!
      do i=1,lonlens
          psexp(i)= exp(pgr(i))
          work4            = fpkap(1000.0*psexp(i))
          prsik(i,1)     = work4
          do k=1,levr
            prsl(i,k)    = psexp(i)     * sl(k)
            prsl_pa(i,k) = prsl(i,k)*1.e3
            prsi(i,k)    = psexp(i)     * si(k)
            prslk(i,k)   = work4        * slk(k)
            prsik(i,k+1) = work4        * sik(k+1)
          enddo
          prsi(i,levr+1) = psexp(i) * si(levr+1)
      enddo
      do k=1,levr
        do i=1,lonlens
          phii(i,k) = 0.0
          phil(i,k) = 0.0
        enddo
      enddo
      do i=1,lonlens
        phii(i,levr+1) = 0.0
      enddo
!
!  get dry tep.
!
      do 270 k=1,levr
      do 270 j=1,lonlens
#ifdef T
      i=j+(lat-1)*igrd1s
      wt(i,k,11)=tgrs(j,k)
#endif
      tgrs(j,k)=tgrs(j,k)/(1.+fv*max(qgrs(j,k),qmin))
#ifdef NONHYD
      tgrh(j,k)=tgrh(j,k)/(1.+fv*max(qgrs(j,k),qmin))
#endif
#ifdef T
      wt(i,k,11)=tgrs(j,k)-wt(i,k,11)
#endif
  270 continue

#ifndef NONHYD
      call get_prs(lonlens,igrd1s,levr,rocp,cp,fv,tgrs,qgrs             &
     &,            prsi,prsik,prsl,prslk,phii,phil,delp)
#else
      call get_prs(lonlens,igrd1s,levr,rocp,cp,fv,tgrh,qgrs              &  
     &,            prsi,prsik,prsl,prslk,phii,phil,delp)
#endif
!
   do k = 1,levr 
     do i = 1,lonlens
       zl(i,k)=phil(i,k)/grav
       zi(i,k)=phii(i,k)/grav
     enddo
   enddo
   do i = 1,lonlens
     zi(i,levr+1)=phii(i,levr+1)/grav
   enddo
   do k = 1,levr 
     do i = 1,lonlens
       delz(i,k) = zi(i,k+1)-zi(i,k)
     enddo
   enddo
#ifndef NONHYD
   do k = 1,levr 
     do i = 1,lonlens
       ww(i,k) = -xgrs(i,k)*delz(i,k)/delp(i,k)
     enddo
   enddo
#endif /* NONHYD */
!
!ccj
      p850 = 850. * .1
!
      rbs2 = 1.e0
      jcapr= 126.*106000./ rdelx
!
!  frain is the factor for centered difference scheme correction of rain
!  amount.
!
      frain = dtf/dtp
      ldiag=.false.
!
!  transfer vegetation fraction from global to local location
!  as well as vegetation type and soil type
!
      do j = 1, lonlens
        soiltyp(j) = int(stype(j,lat)+.5)
!       sigmaf(j) = max(vfrac(j,lat),.3)
        sigmaf(j) = 0.5 + vfrac(j,lat) * 0.5
        vegtype(j) = int(vtype(j,lat)+.5)
        fm1(j) = ffmm(j,lat)
        fh1(j) = ffhh(j,lat)
        ustar1(j) = uustar(j,lat)
        if(slmsk(j,lat).eq.1.) then
          if(soiltyp(j).eq.0)  soiltyp(j) = 2
          if(vegtype(j).eq.0)  vegtype(j) = 7
        endif
        if(slmsk(j,lat).eq.2.) then
          soiltyp(j) = 9
          vegtype(j) = 13
        endif
      enddo
!
!  transfer soil moisture and temperature from global to local variables
!
      do k = 1, lsoil
        do j = 1, lonlens
          smsoil(j,k) = smc(j,lat,k)
          stsoil(j,k) = stc(j,lat,k)
        enddo
      enddo
!
      do 240 k=1,levr
      do 240 j=1,lonlens
      dudt(j,k)=0.e0
      dvdt(j,k)=0.e0
      dtdt(j,k)=0.e0
  240 continue
#ifdef NONHYD
      do k=1,levr+1
        do j=1,lonlens
          dwdt(j,k)=0.e0
        enddo
      enddo
#endif
      do 250 k=1,levh
      do 250 j=1,lonlens
      dqdt(j,k)=0.e0
  250 continue
      rcl=1.0
      do i=1,lonlens
        rclx(i)   = 1.0
        rcs(i)   = sqrt(rclx(i))
        work1(i) = prsik(i,1) / prslk(i,1)
#ifdef RAS
!       garea(i) = rerth * rerth * (pi+pi)*pi/(rcs(i)*lonf*latg)
        rarea(i) = rdelx * rdely / xm2(i,lat)
!        if(me.eq.0.and.i.eq.lonlens/2) then
!          print*,' lat= ',lat,' rarea= ',rarea(i),
!     &           ' delx= ',rdelx/sqrt(xm2(i,lat))
!        endif
#endif
      enddo
!.................................................................
!
!  get surface pressure.
!
      do 280 j=1,lonlens
      psurf(j,lat)=psexp(j)
      if(lssav) psmean(j,lat)=psmean(j,lat)+psexp(j)*dtf
  280 continue

#ifdef doDCYC2
!
!  initialize dtdt with heating rate from dcyc2 and get radsl for progtm
!
      call dcyc2t3(lonlens,igrd1s,levr,solhr,slag,                       &   
     &           sinlar(1,lat),coslar(1,lat),                            &
     &           sdec,cdec,rlon(1,lat),coszer(1,lat),                    &
     &           sfcdlw(1,lat),sfcnsw(1,lat),tgrs(1,1),                  &
     &           tsea(1,lat),tsflw(1,lat),swh(1,1,lat),hlw(1,1,lat),     &
     &           dlwsf1,ulwsf1,radsl,dtdt,xmu)
!
!       print *,'in rbphys, aft dcyc2t3, me=',me
      if(lssav) then
      do 285 j=1,lonlens
      dlwsfc(j,lat)=dlwsfc(j,lat)+dlwsf1(j)*dtf
      ulwsfc(j,lat)=ulwsfc(j,lat)+ulwsf1(j)*dtf
285   continue
      endif
!
      do 286 k=1,levr
      do 286 j=1,lonlens
      hsw(j,k)=dtdt(j,k)-hlw(j,k,lat)
#ifdef T
      i=j+(lat-1)*igrd1s
      wt(i,k, 9)=hsw(j,k)
      wt(i,k,10)=hlw(j,k,lat)
#endif
286   continue
#endif  
! end of doDCYC2
!
!cc
!     phy_f2dv(:,num_p2d) = 0.0  ! disable downdraft effect on evap
!       if (me .eq. 0) print *,'--- in rbphys, before progtm'
!       if (me .eq. 0) print *,'-- tsea  ', tsea(:,lat)
#ifdef doPROGTM
      call progtm(lonlens,igrd1s,lsoil,psexp,                            &   
     & ugrs(1,1),vgrs(1,1),tgrs(1,1),qgrs(1,1),phil(1,1),                &
     & sheleg(1,lat),tsea(1,lat),qss,                                    &
     & smsoil,stsoil,evapc,soiltyp,sigmaf,                               &
     & vegtype,canopy(1,lat),dlwsf1,radsl,                               &
     & snowmt,dtf,zorl(1,lat),                                           &
     & tg3(1,lat),gflx,f10m(1,lat),u10m(1,lat),v10m(1,lat),              &
     & t2m(1,lat),q2m(1,lat),                                            &
     & zsoil,cd,cdq,rb,rhscnpy,rhsmc,ai,bi,cci,                          &
     & rclx,prsl(1,1),work1,slmsk(1,lat),                                &
     & drain,evap,hflx,stress,ep1d,                                      &
     & fm1,fh1,ustar1,wind,phy_f2dv(1,num_p2d))
!
!       if (me .eq. 0) print *,'--- in rbphys, after progtm'
!       if (me .eq. 0) print *,'-- tsea  ', tsea(:,lat)
!       print *,'in rbphys,progtm,me=',me,'lat=',lat
      do i=1,lonlens
        phy_f2dv(i,num_p2d) = 0.0
      enddo
!
      if(lssav) then
      do 290 j=1,lonlens
      gflux(j,lat)=gflux(j,lat)+gflx(j)*dtf
      tmpmax(j,lat) = max(tmpmax(j,lat),t2m(j,lat))
      tmpmin(j,lat) = min(tmpmin(j,lat),t2m(j,lat))
      spfhmax(j,lat) = max(spfhmax(j,lat),q2m(j,lat))
      spfhmin(j,lat) = min(spfhmin(j,lat),q2m(j,lat))
      ep(j,lat) = ep(j,lat) + ep1d(j) * dtf
      uustar(j,lat) = ustar1(j)
      ffmm(j,lat) = fm1(j)
      ffhh(j,lat) = fh1(j)
  290 continue
      endif
!
!     compute coefficient of evaporation in evapc
!
      do 320 j=1,lonlens
      if (evapc(j).gt.1.e0) evapc(j)=1.0e0
  320 continue
!
!     over snow cover or ice or sea, coef of evap =1.0.e0
!
      do 330 j=1,lonlens
      if ((sheleg(j,lat).gt.0.e0) .or.                                 &  
     &     (slmsk(j,lat).ne.1.e0))                                     &
     &      evapc(j)=1.e0
  330 continue
#endif
! end of doPRGTM
!
!  do vertical diffusion
!
!ccj
#ifdef doMONINP
      do kc1 = 1,ntotal
         kc = (kc1-1)*levr
      do k=1,levr
      do i=1,lonlens
         dqdtx(i,k,kc1)=dqdt(i,kc+k)
         qgrsx(i,k,kc1)=qgrs(i,kc+k)
      enddo
      enddo
      enddo
!c
      call moninp(lonlens,igrd1s,levr,nvdiff,                            &  
     &  dvdt,dudt,dtdt,dqdtx,                                            &
     &  ugrs(1,1),vgrs(1,1),tgrs(1,1),qgrsx,                             &
     &  prsik(1,1),rb,fm1,fh1,                                           &
     &  tsea(1,lat),qss,hflx,evap,stress,wind,kpbl,                      &
     &  prsi,delp,prsl,prslk,phii,phil,rclx,dtp,                         &
     &  dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl(1,lat),gamt,gamq)
!        print *,'in rbphys,moninp,me=',me,'lat=',lat

      do kc1 = 1,ntotal
         kc = (kc1-1)*levr
      do k=1,levr
      do i=1,lonlens
         dqdt(i,kc+k)=dqdtx(i,k,kc1)
         qgrs(i,kc+k)=qgrsx(i,k,kc1)
      enddo
      enddo
      enddo
!cc
      if(lssav) then
      do 340 j=1,lonlens
      dusfc(j,lat)=dusfc(j,lat)+dusfc1(j)*dtf
      dvsfc(j,lat)=dvsfc(j,lat)+dvsfc1(j)*dtf
      dtsfc(j,lat)=dtsfc(j,lat)+dtsfc1(j)*dtf
      dqsfc(j,lat)=dqsfc(j,lat)+dqsfc1(j)*dtf
340   continue
      endif
#ifdef A
      do k=1,levr
      do j=1,lonlens
        i=j+(lat-1)*igrd1s
#endif
#ifdef T
        wt(i,k,4)=dtdt(j,k)-wt(i,k,9)-wt(i,k,10)
#endif
#ifdef Q
        wq(i,k,3)=dqdt(j,k)
#endif
#ifdef U
        wu(i,k,5)=dudt(j,k)
#endif
#ifdef V
        wv(i,k,5)=dvdt(j,k)
#endif
#ifdef A
      enddo
      enddo
#endif
#endif
! end of doMONINP
!
!ccj
#ifdef doGWDPS
      do i = 1, lonlens
        var(i) = hprime(i,lat,1)
      enddo
!
#ifndef GTOPO30             
! SCC 10/02.05
      if (nmtvr .eq. 6) then
       do i=1,lonlens
         oc(i) = hprime(i,lat,2)
       enddo
       do k = 1, 4
         do i=1,lonlens
           oa4(i,k) = hprime(i,lat,k+2)
           clx(i,k) = 0.0
         enddo
       enddo
! --- lm mb (*j*) 1997 gwd
      elseif(nmtvr .eq. 10) then
       do i=1,lonlens
         oc(i) = hprime(i,lat,2)
       enddo
       do k = 1, 4
         do i=1,lonlens
           oa4(i,k) = hprime(i,lat,k+2)
           clx(i,k) = hprime(i,lat,k+6)
         enddo
       enddo
! --- lm mb (*j*)
      elseif(nmtvr .eq. 14) then
! --- get the kim fields (until this is changed)
       do i=1,lonlens
         oc(i) = hprime(i,lat,2)
       enddo
       do k = 1, 4
         do i=1,lonlens
           oa4(i,k) = hprime(i,lat,k+2)
           clx(i,k) = hprime(i,lat,k+6)
         enddo
       enddo
       do i=1,lonlens
          theta(i)  = hprime(i,lat,11)
          gamma(i)  = hprime(i,lat,12)
          sigma(i)  = hprime(i,lat,13)
          elvmax(i) = hprime(i,lat,14)
       enddo
      else
#endif
        oc  = 0.0
        oa4 = 0.0
        clx = 0.0
        theta = 0
        gamma = 0
        sigma = 0
        elvmax = 0
#ifndef GTOPO30      
! SCC 10/02/05
      endif
#endif
!
      lonfequiv=int(192.0*200.e3/rdelx)
!
!!      call gwdps(lonlens, igrd1s, igrd1s,                                &
!!     &           levr,   dvdt, dudt,                                     &
!!     &           ugrs(1,1),vgrs(1,1),tgrs(1,1),qgrs(1,1),                &
!!     &           kpbl,   prsi, delp,  prsl, prslk,                       &
!!     &           phii,   phil, rclx,  dtp,                               &
!!!!   &           pgr,    kpbl, prsi,  del, prsl, prslk, rcl, dtp,        &
!!     &           kdt,    var,  oc, oa4, clx,                             &
!!     &           theta,sigma,gamma,elvmax,dusfcg, dvsfcg,                &
!!     &           grav,  cp, rd, rv, lonfequiv,                           &
!!     &           nmtvr, me, lprnt,ipr)
!
      call gwdps(lonlens,igrd1s,igrd1s,levr,dvdt,dudt,                   &   
     & ugrs(1,1),vgrs(1,1),tgrs(1,1),qgrs(1,1),                          &
     & kpbl,prsi,delp,prsl,prslk,phil,rclx,dtp,                          &
     & kdt,  var, oc, oa4, clx,dusfcg, dvsfcg,                           &
     & grav,   cp, rd, rv, lonfequiv,lprnt)
!       print *,'aft gwdps, me=',me,'lat=',lat
!c lonfequiv is an equivalent number of lonf of mrf in rsm
! for mrf, cleff = 0.5e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
! where imx=lonf
!  scale cleff between im=384*2 and 192*2 for t126/t170 and t62
!ccc
!
      if(lssav) then
      do 341 j=1,lonlens
      dugwd(j,lat)=dugwd(j,lat)+dusfcg(j)*dtf
      dvgwd(j,lat)=dvgwd(j,lat)+dvsfcg(j)*dtf
341   continue
      endif
#ifdef NONHYD
!
! ustr = u / m    vstr = v /m
! dwdt=m**2(d ustr/dt*dzdx + d vstr/dt*dzdy)
! dwdt=m(dudt*dgzdx+dvdt*dgzdy)/g
!
      do j=1,lonlens
      dwdt(j,1)=dudt(j,1)*gzdx(j,lat)+dvdt(j,1)*gzdy(j,lat)
      dwdt(j,1)=dwdt(j,1)*xm(j,lat) / grav
      enddo
#endif
!
#ifdef A
      do k=1,levr
      do j=1,lonlens
        i=j+(lat-1)*igrd1s
#endif
#ifdef U
        wu(i,k,7)=dudt(j,k)-wu(i,k,5)
#endif
#ifdef V
        wv(i,k,7)=dvdt(j,k)-wv(i,k,5)
#endif
#ifdef A
      enddo
      enddo
#endif
#endif
! end of doGWDPS
!
!.......................................................
!...      get vertical motion (cb/sec) in vvel
!.......................................................
#ifdef NONHYD
      do k=1,levr
      do j=1,lonlens
!hmhj vvel=-(pg)/(rT) dz/dt  201812
        wmean=-0.5*(wgrs(j,k)+wgrs(j,k+1))
        tempv= tgrh(j,k)*(1.+fv*max(qgrs(j,k),qmin))
        vvel(j,k)=wmean*prsl(j,k)*grav/(rd*tempv)
      enddo
      enddo
#else
      call omegast3(lonlens,igrd1s,levr,                                 &  
     &            pphigrs(1),plamgrs(1),                                 &
     &            ugrs(1,1),vgrs(1,1),                                   &
     &            xgrs(1,1),del,rbs2,vvel,                               &
     &            psexp,sl)
#endif
!       print *,'aft omegac, me=',me,'lat=',lat
!
!   w2 is used to store wind speed
!
!     do k=1,levr
!      do j=1,lonlens
!         w2(j,k)=rcs*sqrt(ugrs(j,k)**2+vgrs(j,k)**2)
!     enddo
!     enddo
!
!testjun
      do 350 k=1,levr
      do 350 j=1,lonlens
      gt0(j,k)=tgrs(j,k)+dtdt(j,k)*dtp
      gu0(j,k)=ugrs(j,k)+dudt(j,k)*dtp
      gv0(j,k)=vgrs(j,k)+dvdt(j,k)*dtp
#ifdef NONHYD
      gh0(j,k)=tgrh(j,k)+dtdt(j,k)*dtp
#endif
  350 continue

#ifdef NONHYD
      do k=1,levr+1
        do j=1,lonlens
          gw0(j,k)=wgrs(j,k)+dwdt(j,k)*dtp
        enddo
      enddo
#endif
!ccj
!testjun
!      call maxmin(dqdt,lonlens*levh,1,1,1,'dqdt in rbphys')
      do 360 k=1,levh
      do 360 j=1,lonlens
      gq0(j,k)=qgrs(j,k)+dqdt(j,k)*dtp
  360 continue
!      do 360 k=1,levh
!      do 360 j=1,lonlens
!      gq0(j,k)=qgrs(j,k)
!  360 continue
!
!  ozone physics
!
#ifdef doOZPHYS
      if(ntoz .eq. 1)then
        call ozphys(lat,lonlens,igrd1s,dtp,prsl,                         &  
     &              gq0(1,1+levr))
      endif
#endif
!cc
!
!  call to gwater
!.....
!
#ifndef NONHYD
      call get_phi(lonlens,igrd1s,levr,cp,fv,gt0(1,1),gq0(1,1),          &
     &             prsik,prslk,phii,phil)
#else
      call get_phi(lonlens,igrd1s,levr,cp,fv,gh0(1,1),gq0(1,1),          &
     &             prsik,prslk,phii,phil)
#endif
!
!ccj
#ifdef doSASCNV
      do k=1,levr
        do i=1,lonlens
          clw(i,k,1) = 0.0
          clw(i,k,2) = -999.9
        enddo
      enddo
      do i=1,lonlens
        kuo(i)   = 0
        ktop(i)  = 1
        kbot(i)  = levr
      enddo
!
      if(ncld.gt.0) then
        ncw = (1+ntoz)*levr
      else
        ncw = 0
      endif
!cc
#ifdef A
      do k=1,levr
      do j=1,lonlens
        i=j+(lat-1)*igrd1s
#endif
#ifdef T
        wt(i,k,6)=gt0(j,k)
#endif
#ifdef Q
        wq(i,k,5)=gq0(j,k)
#endif
#ifdef A
      enddo
      enddo
#endif
!ccj
!
!     calling convective parameterization
!
      if (ncld .gt. 0) then
!
        if (num_p3d .eq. 3) then    ! call brad ferrier microphysics
!
!***          algorithm to separate different hydrometeor species
!
          do k=1,levr
            kk=k+ncw
            do i=1,lonlens
              wc     = gq0(i,kk)
              qi     = 0.
              qr     = 0.
              qw     = 0.
              f_ice  = max(0.0, min(1.0, phy_f3dv(i,k,1)))
              f_rain = max(0.0, min(1.0, phy_f3dv(i,k,2)))
!
              qi = f_ice*wc
              qw = wc-qi
              if (qw .gt. 0.0) then
                qr = f_rain*qw
                qw = qw-qr
              endif
!
!             if (f_ice .ge. 1.) then
!               qi = wc
!             else if (f_ice .le. 0.) then
!               qw = wc
!             else
!               qi = f_ice*wc
!               qw = wc-qi
!             endif
!
!             if (qw.gt.0. .and. f_rain.gt.0.) then
!               if (f_rain .ge. 1.) then
!                 qr = qw
!                 qw = 0.
!               else
!                 qr = f_rain*qw
!                 qw = qw-qr
!               endif
!             endif
!
              qr_col(i,k) = qr
!             clw(i,k)    = qi + qw
              clw(i,k,1)  = qi
              clw(i,k,2)  = qw
!
!
!***          array to track fraction of "cloud" in the form of ice
!
!             if (qi+qw .gt. epsq) then
!               fc_ice(i,k) = qi / (qi+qw)
!             else
!               fc_ice(i,k) = 0.
!             endif

            enddo
          enddo
        else
          do k=1,levr
            kk=k+ncw
            do i=1,lonlens
              clw(i,k,1) = gq0(i,kk)
            enddo
          enddo
        endif
      endif
!!
#ifndef RAS
!4x4
!        if ( me.eq.0 ) then
!         print *,'me=0,lat=',lat,'gq0(1:5,13)=',gq0(1:5,13),             &
!     &                         'gt0(1:5,13)=',gt0(1:5,13),               &
!     &   'delp=',delp(1:5,1),'prsl=',prsl(1:5,1),'psexp=',psexp(1:5),    &
!     &   'phil=',phil(1:5,1),'clw=',clw(1:5,1,1),clw(1:5,1,2),'rcs=',    &
!     &   rcs(1:5),'cld1s=',cld1d(1:5),'rain1=',rain1(1:5),'kbot=',       &
!     &   kbot(1:5),'ktop=',ktop(1:5),'kuo=',kuo(1:5),'slmsk=',           &
!     &   slmsk(1:5,lat),'vvel=',vvel(1:5,1),'xkt2=',xkt2(1:5)            
!       endif

      do i=1,lonlens
!      call sascnv(lonlens,igrd1s,levr,                                   &  
!     &            jcapr,dtp,delp,prsl,psexp,phil,                        &
!     &            clw,gq0(1,1),gt0,gu0,gv0,rcs,cld1d,                    &
!     &            rain1,kbot,ktop,kuo,slmsk(1,lat),                      &
!     &            vvel,xkt2,ncld)
      call sascnv(1,igrd1s,levr,                                         &  
     &            jcapr,dtp,delp(i,1),prsl(i,1),psexp(i),phil(i,1),      &
     &            clw(i,1,1),gq0(i,1),gt0(i,1),gu0(i,1),gv0(i,1),rcs(i), &
     &            cld1d(i),                                              &
     &            rain1(i),kbot(i),ktop(i),kuo(i),slmsk(i,lat),          &
     &            vvel(i,1),xkt2(i),ncld)
      enddo

!        if ( me.eq.0 ) then
!         print *,'me=0,lat=',lat,'gq0(1:5,13)=',gq0(1:5,13),             &
!     &    'gt0(1:5,13)=',gt0(1:5,13)   
!        endif
!
#else
      call rascnv(lonlens,igrd1s,levr, dtp, ncrnd, rannum                &
     &,           gt0,    gq0,   gu0,    gv0, clw, 0                     &
     &,           prsi,   prsl,   prsik,  prslk, phil,  phii             &
     &,           kpbl,   cd,     rain1,  kbot,  ktop,  kuo              &
     &,           phy_f2dv(1,num_p2d), flipv, cb2mb                      &
     &,           me, 1, 1, rarea, lprnt)
      cld1d = 0.
#endif
!j
      do k=1,levr
        do i=1,lonlens
          if (clw(i,k,2) .le. -999.0) clw(i,k,2) = 0.0
        enddo
      enddo
!
      if (ncld .gt. 0) then
        if (num_p3d .eq. 3) then    ! call brad ferrier microphysics
!
!***          extract cloud water & ice from fc_ice
!
          do k=1,levr
            kk=k+ncw
            do i=1,lonlens
!             qi = clw(i,k)*fc_ice(i,k)
!             qw = clw(i,k) - qi
!
              qi = clw(i,k,1)
              qw = clw(i,k,2)
!
!***          algorithm to combine different hydrometeor species
!
!             gq0(i,kk) = max(epsq, qi+qw+qr_col(i,k))
              gq0(i,kk) = qi + qw + qr_col(i,k)
              if (qi .le. epsq) then
                phy_f3dv(i,k,1) = 0.
              else
                phy_f3dv(i,k,1) = qi/gq0(i,kk)
              endif
              if (qr_col(i,k) .le. epsq) then
                phy_f3dv(i,k,2) = 0.
              else
                phy_f3dv(i,k,2) = qr_col(i,k) / (qw+qr_col(i,k))
              endif
            enddo
          enddo
        else
          do k=1,levr
            kk=k+ncw
            do i=1,lonlens
!             gq0(i,kk) = clw(i,k) + gq0(i,kk)
              gq0(i,kk) = clw(i,k,1) + clw(i,k,2)
            enddo
          enddo
        endif
      else
        do k=1,levr
          do i=1,lonlens
              clw(i,k,1) = clw(i,k,1) + clw(i,k,2)
          enddo
        enddo
      endif
!
      if(lssav) then
      do  j=1,lonlens
         cldwrk(j,lat) = cldwrk(j,lat) + cld1d(j) * dtf
      enddo
      endif
!
      do 235 j=1,lonlens
      rainc(j)=frain*rain1(j)
      if(lssav) bengsh(j,lat)=bengsh(j,lat)+rainc(j)
235   continue
#ifdef A
      do k=1,levr
      do j=1,lonlens
        i=j+(lat-1)*igrd1s
#endif
#ifdef T
        wt(i,k,6)=(gt0(j,k)-wt(i,k,6))/dt2
#endif
#ifdef Q
        wq(i,k,5)=(gq0(j,k)-wq(i,k,5))/dt2
#endif
#ifdef T
        wt(i,k,7)=gt0(j,k)
#endif
#ifdef Q
        wq(i,k,6)=gq0(j,k)
#endif
#ifdef A
      enddo
      enddo
#endif
#endif
! end of doSASCNV

!ccj
#ifdef doLRGSCL
!
      call cnvc90(clstp,lonlens,igrd1s,rainc,kbot,ktop,                  &   
     &           levr,prsi,                                              &
     &           acv(1,lat),acvb(1,lat),acvt(1,lat),                     &
     &           cv(1,lat), cvb(1,lat), cvt(1,lat))
!
      call shalcv(lonlens,igrd1s,levr,dtp,delp,prsi,prsl,                &
     &              prslk,kuo,gq0(1,1),gt0(1,1),dpshc)
!
#ifdef A
      do k=1,levr
      do j=1,lonlens
        i=j+(lat-1)*igrd1s
#endif
#ifdef T
        wt(i,k,7)=(gt0(j,k)-wt(i,k,7))/dt2
#endif
#ifdef Q
        wq(i,k,6)=(gq0(j,k)-wq(i,k,6))/dt2
#endif
#ifdef T
        wt(i,k,8)=gt0(j,k)
#endif
#ifdef Q
        wq(i,k,7)=gq0(j,k)
#endif
#ifdef A
      enddo
      enddo
#endif
!ccj
      ncw = (1+ntoz)*levr
      if (ncld .eq. 0) then
        call lrgscl(lonlens,igrd1s,levr,dtp,gt0(1,1),gq0(1,1),           &   
     &                 prsl,delp,prslk,rain1,clw(1,1,1))
      elseif (ncld .eq. 1) then
!
!       to call moist convective adjustment above pbl
!
!       call mstcnv(lonlens,igrd1s,levr,dtp,gt0,gq0,prsl,del,prslk,rain1
!    &,                                                 lprnt)
!       rainc = rainc + frain * rain1
!       if(lssav) bengsh(ilon) = bengsh(ilon)
!    &                              + rain1 * frain
!
!       moist convective adjustment over
!
        nlonsequiv=int((78500.0/rdelx)*512.) !nlonsequiv is an equivalence of nlons(i) in mrf
        do i=1,lonlens
          work1(i) = (log(1.0 / (rcs(i)*nlonsequiv)) - dxmin) * dxinv
          work2(i) = 1.0 - work1(i)
        enddo
!
        invrsn = .false.
        kinver = 0
        do k=1,levr/2
          do i=1,lonlens
            if (prsl(i,k) .gt. 60.0 .and. (.not. invrsn(i))) then
               ptem = gt0(i,k+1) - gt0(i,k)
               if (ptem .gt. 0.2 .and. gt0(i,k) .gt. 278.0) then
                 invrsn(i) = .true.
                 kinver(i) = k
               endif
            endif
          enddo
        enddo
!      if ( levr .gt. 100) then
!--------------------------------------------------------------------------
!
      if (num_p3d .eq. 3) then    ! call brad ferrier microphysics
!
        do k=1,levr
          do i=1,lonlens
            kk = kpbl(i)
            if (k .le. kk) then
              ptem  = 0.0
              ptem1 = rhbbot
            else
 !            ptem  = (rhbtop-rhbbot) / (prsik(i,levr+1)-prsik(i,kk))
              ptem  = (rhbtop-rhpbl) / (prsik(i,levr+1)-prsik(i,kk))
              ptem1 = rhpbl
            endif
            rhc(i,k) = ptem1 + ptem * (prslk(i,k)-prslk(i,kk))
            rhc(i,k) = 0.999 * work1(i) + rhc(i,k) * work2(i)
          enddo
        enddo
!
        do k=1,levr/2
          do i=1,lonlens
!           if (k .le. kinver(i) .and. slmsk(i,lat) .eq. 0.0) then
            if (k .le. kinver(i)) then
              rhc(i,k) = min(0.85, rhc(i,k))
!             rhc(i,k) = 0.85
            endif
          enddo
        enddo
!
!       if (me .eq. 0 .and. kdt .eq. 1 .and. ilon .eq. 1)
!    &      print *,' rhc=',rhc
        do i=1,lonlens
!         xncw(i) = 100.0 * work1(i) + 10.0 * work2(i)
!         xncw(i) = 200.0 * work1(i) + 30.0 * work2(i)
!         xncw(i) = 250.0 * work1(i) + 25.0 * work2(i)
!         xncw(i) = 250.0 * work1(i) + 20.0 * work2(i) ! 20021217
          xncw(i) = 100.0 * work1(i) + 20.0 * work2(i)
!!        xncw(i) = 250.0 * work1(i) + 15.0 * work2(i)
!         xncw(i) = 250.0 * work1(i) + 10.0 * work2(i)
!         xncw(i) = 250.0 * work1(i) + 50.0 * work2(i)
        enddo
!       if (me .eq. 0 .and. kdt .eq. 1 .and. ilon .eq. 1)
!    &          print *,' xncw=',xncw
!
        call gsmdrive(lonlens, igrd1s, levr, dtp, prsl, delp             & 
     &,               gt0, gq0(1,1), gq0(1,1+ncw), slmsk(1,lat)          &
     &,               phy_f3dv(1,1,1),  phy_f3dv(1,1,2)                  &
     &,               phy_f3dv(1,1,3), rain1, sr, grav                   &
     &,               hvap, hsub, cp, rhc, xncw, me, lprnt,ipr)
!
      elseif (num_p3d .eq. 4) then ! call zhao/carr/sundqvist microphysics
!
        do k=1,levr
          do i=1,lonlens
            ptem   = (rhztop-rhzbot) / (prslk(i,levr)-prslk(i,1))
            rhc(i,k) = rhzbot + ptem * (prslk(i,k)-prslk(i,1))
            rhc(i,k) = 0.999 * work1(i) + rhc(i,k) * work2(i)
            rhc(i,k) = min(0.999,rhc(i,k))
          enddo
        enddo
!
!   the following commented by moorthi on 11/01/2002
!       do k=1,levr/2
!         do i=1,lonlens
!           if (k .le. kinver(i) .and. slmsk(i,lat) .eq. 0.0) then
!           if (k .le. kinver(i)) then
!             rhc(i,k) = 0.80
!           endif
!         enddo
!       enddo
!       if (me .eq. 0 .and. kdt .eq. 1 .and. ilon .eq. 1)
!    &      print *,' rhc=',rhc
!
        call gscond(lonlens, igrd1s, levr,                               &    
     &              dtp, prsl, psexp,                                    &
     &              gq0(1,1), gq0(1,1+ncw), gt0,                         &
     &              num_p3d,num_p2d,phy_f3dv(:,:,1:num_p3d),             &
     &              phy_f2dv(:,1:num_p2d),                               & 
!     &              phy_f3dv(1,1,1), phy_f3dv(1,1,2), phy_f2dv(1,1),     &
!     &              phy_f3dv(1,1,3), phy_f3dv(1,1,4), phy_f2dv(1,2),     &
     &              rhc,lprnt)
        call precpd(lonlens, igrd1s, levr,                               &
     &              dtp, delp, prsl, psexp,                              &
     &              gq0(1,1), gq0(1,1+ncw), gt0, rain1, rhc, lprnt)
      endif
!ccc
!
      elseif (ncld .eq. 2) then
      call phys_mps_wsm3_p(gt0(1,1),gq0(1,1),gq0(1,1+ncw),ncld,                  &
     &             ww(1,1),prsl_pa(1,1),                                         &
     &             delz(1,1),dtp,grav,cp,cvap,rd,rv,t0c,                         &
     &             fv,rdorv,qmin,hsub,hvap,hfus,rhoair0,rhoh2o,                  &
     &             cliq,cice,psat,                                               &
     &             lat, rain1,rainncv,snow1(1,1),snowncv(1,1),                   &
     &             sr,                                                           &
     &             1,igrd1s, 1,1, 1,levr,                                        &
     &             1,igrd1s, 1,1, 1,levr,                                        &
     &             1,lonlens, 1,1, 1,levr) 
!      call cloud3(lonlens,igrd1s, levr,dtp,psexp,                        &
!     &            gt0(1,1),gq0(1,1),gq0(1,ncw+1),ncld,sl,delp,slk,       &
!     &            rain1,lat,vvel,kdt,fhour)

      elseif (ncld .eq. 4) then
      call phys_mps_wsm5_p(gt0(1,1),gq0(1,1),gq0(1,1+ncw),ncld,prsl_pa(1,1),     &
     &             delz(1,1),dtp,grav,cp,cvap,rd,rv,t0c,                         &
     &             fv,rdorv,qmin,hsub,hvap,hfus,rhoair0,rhoh2o,                  &
     &             cliq,cice,psat,                                               &
     &             lat, rain1,rainncv,sr,                                        &
     &             1,igrd1s, 1,1, 1,levr,                                        &
     &             1,igrd1s, 1,1, 1,levr,                                        &
     &             1,lonlens, 1,1, 1,levr,                                       &
     &             snow1(1,1), snowncv(1,1))
!     call cloud5(1, 1, levr,dtp,psexp,
!    1            gt0,gq0(1,1),gq0(1,ncw+1),ncld,sl,delp,slk,
!    2            rain1,lat,vvel,kdt,fhour)

      elseif (ncld .eq. 5) then
      call phys_mps_wsm6_p(gt0(1,1),gq0(1,1),gq0(1,1+ncw),ncld,prsl_pa(1,1),    &
     &             delz(1,1),dtp,grav,cp,cvap,rd,rv,t0c,                        &
     &             fv,rdorv,qmin,hsub,hvap,hfus,rhoair0,rhoh2o,                 &
     &             cliq,cice,psat,                                              &
     &             lat, rain1,rainncv,sr,                                       &
     &             1,igrd1s, 1,1, 1,levr,                                       &
     &             1,igrd1s, 1,1, 1,levr,                                       &
     &             1,lonlens, 1,1, 1,levr,                                      &
     &             snow1(1,1), snowncv(1,1), graupel1(1,1), graupelncv(1,1))
!     call cloud6(1, 1, levr,dtp,psexp,
!    1            gt0,gq0(1,1),gq0(1,ncw+1),ncld,sl,delp,slk,
!    2            rain1,lat,vvel,kdt,fhour)

      endif
!cc
#ifdef A
      do k=1,levr
      do j=1,lonlens
        i=j+(lat-1)*igrd1s
#endif
#ifdef T
        wt(i,k,8)=(gt0(j,k)-wt(i,k,8))/dt2
#endif
#ifdef Q
        wq(i,k,7)=(gq0(j,k)-wq(i,k,7))/dt2
#endif
#ifdef A
      enddo
      enddo
#endif
!
      do 440 j=1,lonlens
      rainl(j)=frain*rain1(j)
      rain(j)=rainc(j)+rainl(j)
      if(lssav) geshem(j,lat)=geshem(j,lat)+rain(j)
  440 continue
#endif
! end of doLRGSCL
!      if ( levr .gt. 100) then
!--------------------------------------------------------------------------
!-----------------------------------------------------------------------
!      endif
!
!  estimate t850 for rain-snow decision
!
      do i=1,lonlens
        t850(i) = gt0(i,1)
      enddo
      do k = 1, levr - 1
        do i=1,lonlens
          if(prsl(i,k) .gt. p850 .and. prsl(i,k+1) .le. p850) then
            t850(i) = gt0(i,k) - (prsl(i,k)-p850)                        &   
     &              / (prsl(i,k)-prsl(i,k+1)) * (gt0(i,k)-gt0(i,k+1))
          endif
        enddo
      enddo
!
!  factor=weighted mean tep.
      do 450 j=1,lonlens
      if(t850(j).le.273.16) then
        if(slmsk(j,lat).ne.0.) then
          sheleg(j,lat)=sheleg(j,lat)+1.e3*rain(j)
          rain(j)=0.
        endif
      endif
  450 continue
!
!
!  update soil moisture and canopy water after precipitation has been
!  computed
!
#ifdef doPROGT2
      call progt2(lonlens,igrd1s,lsoil,rhscnpy,rhsmc,ai,bi,cci,smsoil,   &  
     &  slmsk(1,lat),canopy(1,lat),rain,runof,snowmt,                    &
     &  zsoil,soiltyp,sigmaf,dtf)
!
!  total runoff is composed of drainage into water table and
!  runoff at the surface and is accumulated in unit of meters
!
!  add srunoff soilm snwdph by zyf
!
      if(lssav) then
      do j = 1, lonlens
        runoff(j,lat) = (drain(j) + runof(j)) * dtf / 1000.              &
     &                + runoff(j,lat)
       srunoff(j,lat) = runof(j) * dtf / 1000. + srunoff(j,lat)
         soilm(j,lat) = -1.0*smsoil(j,1)*zsoil(j,1)
         do k=2,lsoil
           soilm(j,lat) = soilm(j,lat) +                                 &
     &                    smsoil(j,k)*(zsoil(j,k-1)-zsoil(j,k))
         enddo
         if (slmsk(j,lat).eq.1) then
           snwdph(j,lat) = sheleg(j,lat) / 100.0
         else if (slmsk(j,lat).eq.2) then
           snwdph(j,lat) = sheleg(j,lat) / 1000.0 * 3.0
         endif
      enddo
      endif
!
!  return updated smsoil and stsoil to global arrays
!
      do k = 1, lsoil
        do j = 1, lonlens
          smc(j,lat,k) = smsoil(j,k)
          stc(j,lat,k) = stsoil(j,k)
        enddo
      enddo
!
#endif
! end of doPROGT2
!ccj
!
! calc. integral of moisture in pwat
!
      do k=1,levr
        do i=1,lonlens
          work3(i,k) = 0.0
        enddo
      enddo
!
      if (ncld .gt. 0) then
       ncw = (1+ntoz)*levr
       do ic=ncw,levr*(ntotal-1),levr
         do k=1,levr
           do j=1,lonlens
            work3(j,k) = work3(j,k) + gq0(j,ic+k)
           enddo
         enddo
       enddo
      endif
!ccc
!
#ifdef NONHYD
      cpocv=cp/ccv
#endif
      do 493 j=1,lonlens
      pwat(j,lat)=0.
      do 492 k=1,levr
! restore all drt temperatures back to virtual temperatures
      tgrs(j,k)=tgrs(j,k)*(1.+fv*max(qgrs(j,k),qmin))
#ifdef T
      i=j+(lat-1)*igrd1s
      wt(i,k,11)=-wt(i,k,11)+gt0(j,k)
#endif
      gt0(j,k)=gt0(j,k)*(1.+fv*max(gq0(j,k),qmin))
#ifdef T
      wt(i,k,11)=(gt0(j,k)-wt(i,k,11))/dt2
#endif
#ifdef NONHYD
      tgrh(j,k)=tgrh(j,k)*(1.+fv*max(qgrs(j,k),qmin))
      dtv=gt0(j,k)-tgrs(j,k)
      gh0(j,k)=tgrh(j,k)+dtv
!hmhj gp0(j,k)=pgrs(j,k)+(cpocv*dtv/tgrs(j,k))
!hmhj isobaric process even for NONHYD
      gp0(j,k)=pgrs(j,k)
#endif
      pwat(j,lat)=pwat(j,lat)+delp(j,k)*(gq0(j,k)+work3(j,k))
      wvuflx(j,lat)=wvuflx(j,lat)                                        &
     &              +delp(j,k)*(gq0(j,k)+work3(j,k))*gu0(j,k)            &
     &               *(1.e3/grav)*dtf
      wvvflx(j,lat)=wvvflx(j,lat)                                        &
     &              +delp(j,k)*(gq0(j,k)+work3(j,k))*gv0(j,k)            &
     &               *(1.e3/grav)*dtf
  492 continue
      pwat(j,lat)=pwat(j,lat)*(1.e3/grav)
  493 continue
!
!  compute total mass and water mass over the domain
!x    ptotj=0.
!x    pwatj=0.
!x    do 903 j=1,lonlens
!x    ptotj=ptotj+psexp(j)
!x    pwatj=pwatj+pwat(j,lat)*(grav/1.e3)
!x903 continue
!x    ptotj=ptotj/lonlens
!x    pwatj=pwatj/lonlens
!
!      print *,'end of rbphys,me=',me
      return
      end
