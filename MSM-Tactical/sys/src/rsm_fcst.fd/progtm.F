#include <define.h>
      subroutine progtm(im,ix,km,ps,u1,v1,t1,q1,phil1,                   &
     &                  sheleg,tskin,qsurf,                              &
     &                  smc,stc,dm,soiltyp,sigmaf,vegtype,canopy,        &
     &                  dlwflx,                                          &
!    &                  slrad,snowmt,delt,z0rl,plantr,tg3,
     &                  slrad,snowmt,delt,z0rl,tg3,                      &
     &                  gflux,f10m,u10m,v10m,t2m,q2m,zsoil,              &
     &                  cm, ch, rb,rhscnpy,rhsmc,aim,bim,cim,            &
     &                  rcl,prsl1,prslki,slimsk,                         &
     &                drain,evap,hflx,stress,ep,fm,fh,ustar,wind,ddvel)
!    &              drain,evap,hflx,rnet,ep,fm,fh,ustar,wind,ddvel)
!
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, grav => con_g, sbc => con_sbc, hvap => con_hvap      &
     &,             cp => con_cp, hfus => con_hfus, jcal => con_jcal     &
     &,             eps => con_eps, epsm1 => con_epsm1, t0c => con_t0c   &
     &,             rvrdm1 => con_fvirt, rd => con_rd
      implicit none
!
      integer              im, ix, km
!
      real(kind=kind_phys), parameter :: cpinv=1.0/cp, hvapi=1.0/hvap
      real(kind=kind_phys) delt, ztmin1, phil1(ix)
      integer              soiltyp(ix),  vegtype(ix)
      real(kind=kind_phys) ps(ix),       u1(ix),      v1(ix),            &
     &                     t1(ix),       q1(ix),      sheleg(ix),        &
     &                     tskin(ix),    qsurf(ix),   smc(ix,km),        &
     &                     stc(ix,km),   dm(ix),      sigmaf(ix),        &
     &                     canopy(ix),   dlwflx(ix),  slrad(ix),         &
     &                     snowmt(ix),   z0rl(ix),    tg3(ix),           &
     &                     gflux(ix),    f10m(ix),    u10m(ix),          &
     &                     v10m(ix),     t2m(ix),     q2m(ix),           &
     &                     zsoil(ix,km), cm(ix),      ch(ix),            &
     &                     rb(ix),       rhscnpy(ix), rhsmc(ix,km),      &
     &                     aim(ix,km),   bim(ix,km),  cim(ix,km),        &
     &                     rcl(ix),      prsl1(ix),   prslki(ix),        &
     &                     slimsk(ix),   drain(ix),   evap(ix),          &
     &                     hflx(ix),     rnet(ix),    ep(ix),            &
     &                     fm(ix),       fh(ix),      ustar(ix),         &
     &                     wind(ix),     ddvel(ix),   stress(ix)
!
!     locals
!
      integer              k,i
!
      real(kind=kind_phys) canfac(ix),                                   &
     &                     ddz(ix),     ddz2(ix),    delta(ix),          &
     &                     dew(ix),     df1(ix),     dft0(ix),           &
     &                     dft2(ix),    dft1(ix),                        &
     &                     dmdz(ix),    dmdz2(ix),   dtdz1(ix),          &
     &                     dtdz2(ix),   dtv(ix),     ec(ix),             &
     &                     edir(ix),    etpfac(ix),                      &
     &                     factsnw(ix), fh2(ix),     fm10(ix),           &
     &                     fx(ix),      gx(ix),      ztmax1(ix),         &
     &                     hcpct(ix),   hl1(ix),     hl12(ix),           &
     &                     hlinf(ix),   partlnd(ix), ph(ix),             &
     &                     ph2(ix),     pm(ix),      pm10(ix),           &
     &                     psurf(ix),   q0(ix),      qs1(ix),            &
     &                     qss(ix),     rat(ix),     rcap(ix),           &
     &                     rch(ix),     rho(ix),     rs(ix),             &
     &                     rsmall(ix),  slwd(ix),    smcz(ix),           &
     &                     snet(ix),    snoevp(ix),  snowd(ix),          &
     &                     t1o(ix),     t2mo(ix),    term1(ix),          &
     &                     term2(ix),   theta1(ix),  thv1(ix),           &
     &                     tref(ix),    tsurf(ix),   tv1(ix),            &
     &                     tvs(ix),     tsurfo(ix),  twilt(ix),          &
     &                     xx(ix),      xrcl(ix),    yy(ix),             &
     &                     z0(ix),      z0max(ix),   z1(ix),             &
     &                     ztmax(ix),   zz(ix),      ps1(ix)
!
      real(kind=kind_phys) a0,    a0p,      a1,    a1p,     aa,  aa0,    &
     &                     aa1,   adtv,     alpha, arnu,    b1,  b1p,    &
     &                     b2,    b2p,      bb,    bb0,     bb1, bb2,    &
     &                     bfact, ca,       cc,    cc1,     cc2, cfactr, &
     &                     ch2o,  charnock, cice,  convrad, cq,  csoil,  &
     &                     ctfil1,ctfil2,   delt2, df2,     dfsnow,      &
     &                     elocp, eth,      ff,  fms,       cn,          &
     &                     fhs,   funcdf,   funckt,g,       hl0, hl0inf, &
     &                     hl110, hlt,      hltinf,olinf,   rcq, rcs,    &
     &                     rct,   restar,   rhoh2o,rnu,     rsi,         &
     &                     rss,   scanop,   sig2k, sigma,   smcdry,      &
     &                     t12,   t14,      tflx,  tgice,   topt,        &
     &                     val,   vis,      zbot,  snomin,  tem
!
      parameter (charnock=.014,ca=.4)!c ca is the von karman constant
      parameter (g=grav,sigma=sbc)

      parameter (alpha=5.,a0=-3.975,a1=12.32,b1=-7.755,b2=6.041)
      parameter (a0p=-7.941,a1p=24.75,b1p=-8.705,b2p=7.899)
      parameter (vis=1.4e-5)
      parameter (aa1=-1.076,bb1=.7045,cc1=-.05808)
      parameter (bb2=-.1954,cc2=.009999)
      parameter (elocp=hvap/cp,dfsnow=.31,ch2o=4.2e6,csoil=1.26e6)
      parameter (scanop=.5,cfactr=.5,zbot=-3.,tgice=271.2)
      parameter (cice=1880.*917.,topt=298.)
      parameter (rhoh2o=1000.,convrad=jcal*1.e4/60.)
      parameter (ctfil1=.5,ctfil2=1.-ctfil1)
      parameter (rnu=1.51e-5,arnu=.135*rnu)
      parameter (snomin=1.0e-9)
      parameter (ztmin1=-999.0)
!
      logical flag(ix), flagsnw(ix)
      real(kind=kind_phys) kt1(ix),       kt2(ix),      ktsoil,          &   
     &                     et(ix,km),                                    &
     &                     stsoil(ix,km), ai(ix,km),    bi(ix,km),       &
     &                     ci(ix,km),     rhstc(ix,km)
      real(kind=kind_phys) rsmax(13), rgl(13),  rsmin(13), hs(13),       &
     &                     smmax(9),  smdry(9), smref(9),  smwlt(9)
!
!  the 13 vegetation types are:
!
!  1  ...  broadleave-evergreen trees (tropical forest)
!  2  ...  broadleave-deciduous trees
!  3  ...  broadleave and needle leave trees (mixed forest)
!  4  ...  needleleave-evergreen trees
!  5  ...  needleleave-deciduous trees (larch)
!  6  ...  broadleave trees with groundcover (savanna)
!  7  ...  groundcover only (perenial)
!  8  ...  broadleave shrubs with perenial groundcover
!  9  ...  broadleave shrubs with bare soil
! 10  ...  dwarf trees and shrubs with ground cover (trunda)
! 11  ...  bare soil
! 12  ...  cultivations (use parameters from type 7)
! 13  ...  glacial
!
      data rsmax/13*5000./
      data rsmin/150.,100.,125.,150.,100.,70.,40.,                       &    
     &           300.,400.,150.,999.,40.,999./
      data rgl/5*30.,65.,4*100.,999.,100.,999./
      data hs/41.69,54.53,51.93,47.35,47.35,54.53,36.35,                 &
     &        3*42.00,999.,36.35,999./
      data smmax/.421,.464,.468,.434,.406,.465,.404,.439,.421/
      data smdry/.07,.14,.22,.08,.18,.16,.12,.10,.07/
      data smref/.283,.387,.412,.312,.338,.382,.315,.329,.283/
      data smwlt/.029,.119,.139,.047,.010,.103,.069,.066,.029/
!
      save rsmax, rsmin, rgl, hs, smmax, smdry, smref, smwlt
!
      delt2 = delt * 2.
!
!     estimate sigma ** k at 2 m
!
      sig2k = 1. - 4. * g * 2. / (cp * 280.)
!
!  initialize variables. all units are supposedly m.k.s. unless specifie
!  psurf is in pascals
!  wind is wind speed, theta1 is adiabatic surface temp from level 1
!  rho is density, qs1 is sat. hum. at level1 and qss is sat. hum. at
!  surface
!  convert slrad to the civilized unit from langley minute-1 k-4
!  surface roughness length is converted to m from cm
!
!!
!     qs1 = fpvs(t1)
!     qss = fpvs(tskin)
      do i=1,im
        xrcl(i)  = sqrt(rcl(i))
        psurf(i) = 1000. * ps(i)
        ps1(i)   = 1000. * prsl1(i)
!       slwd(i)  = slrad(i) * convrad
        slwd(i)  = slrad(i)
! hmhj test
#ifdef SLRFIX
        slwd(i) = 0.0
#endif
!
!  dlwflx has been given a negative sign for downward longwave
!  snet is the net shortwave flux
!
        snet(i) = -slwd(i) - dlwflx(i)
        wind(i) = xrcl(i) * sqrt(u1(i) * u1(i) + v1(i) * v1(i))          &    
     &              + max(0.0, min(ddvel(i), 30.0))
        wind(i) = max(wind(i),1.)
        q0(i) = max(q1(i),1.e-8)
        tsurf(i) = tskin(i)
        theta1(i) = t1(i) * prslki(i)
        tv1(i) = t1(i) * (1. + rvrdm1 * q0(i))
        thv1(i) = theta1(i) * (1. + rvrdm1 * q0(i))
        tvs(i) = tsurf(i) * (1. + rvrdm1 * q0(i))
        rho(i) = ps1(i) / (rd * tv1(i))
!jfe    qs1(i) = 1000. * fpvs(t1(i))
        qs1(i) = fpvs(t1(i))
        qs1(i) = eps * qs1(i) / (ps1(i) + epsm1 * qs1(i))
        qs1(i) = max(qs1(i), 1.e-8)
        q0(i) = min(qs1(i),q0(i))
!jfe    qss(i) = 1000. * fpvs(tsurf(i))
        qss(i) = fpvs(tskin(i))
        qss(i) = eps * qss(i) / (psurf(i) + epsm1 * qss(i))
!       rs = plantr
        rs(i) = 0.
        if(vegtype(i).gt.0.) rs(i) = rsmin(vegtype(i))
        z0(i) = .01 * z0rl(i)
        canopy(i)= max(canopy(i),0.)
        dm(i) = 1.
        factsnw(i) = 10.
        if(slimsk(i).eq.2.) factsnw(i) = 3.
!
!  snow depth in water equivalent is converted from mm to m unit
!
        snowd(i) = sheleg(i) / 1000.
        flagsnw(i) = .false.
!
!  when snow depth is less than 1 mm, a patchy snow is assumed and
!  soil is allowed to interact with the atmosphere.
!  we should eventually move to a linear combination of soil and
!  snow under the condition of patchy snow.
!
        if(snowd(i).gt..001.or.slimsk(i).eq.2.) rs(i) = 0.
        if(snowd(i).gt..001) flagsnw(i) = .true.
!##dg  if(lat.eq.latd) then
!##dg    print *, ' wind,tv1,tvs,q1,qs1,snow,slimsk=',
!##dg&   wind,tv1,tvs,q1,qs1,snowd,slimsk
!##dg    print *, ' snet, slwd =', snet, slwd(i)
!##dg  endif
        if(slimsk(i).eq.0.) then
          zsoil(i,1) = 0.
        elseif(slimsk(i).eq.1.) then
          zsoil(i,1) = -.10
        else
          zsoil(i,1) = -3. / km
        endif
      enddo
!!
      do k = 2, km
        do i=1,im
          if(slimsk(i).eq.0.) then
            zsoil(i,k) = 0.
          elseif(slimsk(i).eq.1.) then
            zsoil(i,k) = zsoil(i,k-1)+ (-2. - zsoil(i,1)) / (km - 1)
          else
            zsoil(i,k) = - 3. * float(k) / float(km)
          endif
        enddo
      enddo
!!
      do i=1,im
!hmhj   z1(i) = -rd * tv1(i) * log(ps1(i)/psurf(i)) / g
           z1(i) = phil1(i) / grav
        drain(i) = 0.
      enddo
!        print *, ' --z0rl',z0rl
!        print *, ' --z1',z1
!!
      do k = 1, km
        do i=1,im
          et(i,k) = 0.
          rhsmc(i,k) = 0.
          aim(i,k) = 0.
          bim(i,k) = 1.
          cim(i,k) = 0.
          stsoil(i,k) = stc(i,k)
        enddo
      enddo
      do i=1,im
        edir(i) = 0.
        ec(i) = 0.
        evap(i) = 0.
        ep(i) = 0.
        snowmt(i) = 0.
        gflux(i) = 0.
        rhscnpy(i) = 0.
        fx(i) = 0.
        etpfac(i) = 0.
        canfac(i) = 0.
      enddo
!
!  compute stability dependent exchange coefficients
!
!  this portion of the code is presently suppressed
!
      do i=1,im
        if(slimsk(i).eq.0.) then
          ustar(i) = sqrt(g * z0(i) / charnock)
        endif
!
!  compute stability indices (rb and hlinf)
!
!hmhj   z0max(i) = min(z0(i),1. * z1(i))
        z0max(i) = max(1.0e-6, min(z0(i),z1(i)))
        ztmax(i) = z0max(i)
        if(slimsk(i).eq.0.) then
          restar = ustar(i) * z0max(i) / vis
          restar = max(restar,.000001)
!         restar = alog(restar)
!         restar = min(restar,5.)
!         restar = max(restar,-5.)
!         rat(i) = aa1 + bb1 * restar + cc1 * restar ** 2
!         rat(i) = rat(i) / (1. + bb2 * restar
!    &                       + cc2 * restar ** 2)
!  rat taken from zeng, zhao and dickinson 1997
          rat(i) = 2.67 * restar ** .25 - 2.57
          rat(i) = min(rat(i),7.)
          ztmax(i) = z0max(i) * exp(-rat(i))
        endif
      enddo
!hmhj add ztmax1
      do i=1, im
        ztmax(i)  = max(ztmax(i),1.0e-6)
        tem   = z0max(i)/z1(i)
        if (abs(1.0-tem) > 1.0e-6) then
          ztmax1(i) = - log(tem)/(2.0*alpha*(1.-tem))
        else
          ztmax1(i) = 99.0
        endif
        if( z0max(i) < 0.05 .and. snowd(i) < 0.01 ) ztmax1(i) = 99.0
      enddo
!         print *, ' --z0max',z0max
!         print *, ' --ztmax',ztmax
!##dg  if(lat.eq.latd) then
!##dg    print *, ' z0max, ztmax, restar, rat(i) =', 
!##dg &   z0max, ztmax, restar, rat(i)
!##dg  endif
      do i = 1, im
        dtv(i) = thv1(i) - tvs(i)
        adtv = abs(dtv(i))
        adtv = max(adtv,.001)
        dtv(i) = sign(1.,dtv(i)) * adtv
        rb(i) = g * dtv(i) * z1(i) / (.5 * (thv1(i) + tvs(i))            &     
     &          * wind(i) * wind(i))
        rb(i) = max(rb(i),-5000.)
        fm(i) = log((z0max(i)+z1(i)) / z0max(i))
        fh(i) = log((ztmax(i)+z1(i)) / ztmax(i))
        hlinf(i) = rb(i) * fm(i) * fm(i) / fh(i)
!hmhj add 1
        hlinf(i)= min(max(hlinf(i),ztmin1),ztmax1(i))
        fm10(i) = log((z0max(i)+10.) / z0max(i))
        fh2(i) = log((ztmax(i)+2.) / ztmax(i))
      enddo
!         print *, ' --fm',fm
!         print *, ' --fh',fh
!##dg  if(lat.eq.latd) then
!##dg    print *, ' dtv, rb(i), fm(i), fh(i), hlinf =',
!##dg &   dtv, rb, fm(i), fh(i), hlinf
!##dg  endif
!
!  stable case
!
      do i = 1, im
        if(dtv(i).ge.0.) then
          hl1(i) = hlinf(i)
        endif
        if(dtv(i).ge.0..and.hlinf(i).gt..25) then
          hl0inf = z0max(i) * hlinf(i) / z1(i)
          hltinf = ztmax(i) * hlinf(i) / z1(i)
          aa = sqrt(1. + 4. * alpha * hlinf(i))
          aa0 = sqrt(1. + 4. * alpha * hl0inf)
          bb = aa
          bb0 = sqrt(1. + 4. * alpha * hltinf)
          pm(i) = aa0 - aa + log((aa + 1.) / (aa0 + 1.))
          ph(i) = bb0 - bb + log((bb + 1.) / (bb0 + 1.))
          fms = fm(i) - pm(i)
          fhs = fh(i) - ph(i)
          hl1(i) = fms * fms * rb(i) / fhs
!hmhj add 1
          hl1(i) = min(max(hl1(i), ztmin1), ztmax1(i))
        endif
      enddo
!
!  second iteration
!
      do i = 1, im
        if(dtv(i).ge.0.) then
          hl0 = z0max(i) * hl1(i) / z1(i)
          hlt = ztmax(i) * hl1(i) / z1(i)
          aa = sqrt(1. + 4. * alpha * hl1(i))
          aa0 = sqrt(1. + 4. * alpha * hl0)
          bb = aa
          bb0 = sqrt(1. + 4. * alpha * hlt)
          pm(i) = aa0 - aa + log((aa + 1.) / (aa0 + 1.))
          ph(i) = bb0 - bb + log((bb + 1.) / (bb0 + 1.))
          hl110 = hl1(i) * 10. / z1(i)
!hmhj add 1
          hl110 = min(max(hl110, ztmin1), ztmax1(i))
          aa = sqrt(1. + 4. * alpha * hl110)
          pm10(i) = aa0 - aa + log((aa + 1.) / (aa0 + 1.))
          hl12(i) = hl1(i) * 2. / z1(i)
!hmhj add 1
          hl12(i) = min(max(hl12(i), ztmin1), ztmax1(i))
!         aa = sqrt(1. + 4. * alpha * hl12(i))
          bb = sqrt(1. + 4. * alpha * hl12(i))
          ph2(i) = bb0 - bb + log((bb + 1.) / (bb0 + 1.))
        endif
      enddo
!!
!##dg  if(lat.eq.latd) then
!##dg    print *, ' hl1(i), pm, ph =',
!##dg &   hl1(i),  pm, ph
!##dg  endif
!
!  unstable case
!
!
!  check for unphysical obukhov length
!
      do i=1,im
        if(dtv(i).lt.0.) then
          olinf = z1(i) / hlinf(i)
          if(abs(olinf).le.50. * z0max(i)) then
            hlinf(i) = -z1(i) / (50. * z0max(i))
!hmhj add 1
            hlinf(i) = min(max(hlinf(i),ztmin1),ztmax1(i))
          endif
        endif
      enddo
!
!  get pm and ph
!
      do i = 1, im
        if(dtv(i).lt.0..and.hlinf(i).ge.-.5) then
          hl1(i) = hlinf(i)
          pm(i) = (a0 + a1 * hl1(i)) * hl1(i)                            &    
     &            / (1. + b1 * hl1(i) + b2 * hl1(i) * hl1(i))
          ph(i) = (a0p + a1p * hl1(i)) * hl1(i)                          &
     &            / (1. + b1p * hl1(i) + b2p * hl1(i) * hl1(i))
          hl110 = hl1(i) * 10. / z1(i)
!hmhj add 1
          hl110 = min(max(hl110, ztmin1), ztmax1(i))
          pm10(i) = (a0 + a1 * hl110) * hl110                            &
     &            / (1. + b1 * hl110 + b2 * hl110 * hl110)
          hl12(i) = hl1(i) * 2. / z1(i)
!hmhj add 1
          hl12(i) = min(max(hl12(i), ztmin1), ztmax1(i))
          ph2(i) = (a0p + a1p * hl12(i)) * hl12(i)                       &
     &            / (1. + b1p * hl12(i) + b2p * hl12(i) * hl12(i))
        endif
        if(dtv(i).lt.0.and.hlinf(i).lt.-.5) then
          hl1(i) = -hlinf(i)
          pm(i) = log(hl1(i)) + 2. * hl1(i) ** (-.25) - .8776
          ph(i) = log(hl1(i)) + .5 * hl1(i) ** (-.5) + 1.386
          hl110 = hl1(i) * 10. / z1(i)
!hmhj add 1
          hl110 = min(max(hl110, ztmin1), ztmax1(i))
          pm10(i) = log(hl110) + 2. * hl110 ** (-.25) - .8776
          hl12(i) = hl1(i) * 2. / z1(i)
          ph2(i) = log(hl12(i)) + .5 * hl12(i) ** (-.5) + 1.386
        endif
      enddo
!        print *,'---pm',pm
!        print *,'---ph',ph
!
!  finish the exchange coefficient computation to provide fm and fh
!
      do i = 1, im
        fm(i) = fm(i) - pm(i)
        fh(i) = fh(i) - ph(i)
        fm10(i) = fm10(i) - pm10(i)
        fh2(i) = fh2(i) - ph2(i)
        cm(i) = ca * ca / (fm(i) * fm(i))
        ch(i) = ca * ca / (fm(i) * fh(i))
        tem = 0.00001/z1(i)
        cm(i) = max(cm(i), tem)
        ch(i) = max(ch(i), tem)
        stress(i) = cm(i) * wind(i) * wind(i)
        ustar(i)  = sqrt(stress(i))
!       ustar(i) = sqrt(cm(i) * wind(i) * wind(i))
      enddo
!##dg  if(lat.eq.latd) then
!##dg    print *, ' fm, fh, cm, ch(i), ustar =',
!##dg &   fm, fh, cm, ch, ustar
!##dg  endif
!
!  update z0 over ocean
!
      do i = 1, im
        if(slimsk(i).eq.0.) then
          z0(i) = (charnock / g) * ustar(i) ** 2
!  new implementation of z0
!         cc = ustar(i) * z0 / rnu
!         pp = cc / (1. + cc)
!         ff = g * arnu / (charnock * ustar(i) ** 3)
!         z0 = arnu / (ustar(i) * ff ** pp)
!         z0(i) = min(z0(i),.1)
!         z0(i) = max(z0(i),1.e-7)
! redrag for high wind over sea
          z0rl(i) = 100. * max(min(z0(i),0.0014), 1.e-7)
!else
!         z0rl(i) = 100. * max(min(z0(i),0.1), 1.e-7)
        endif
      enddo
!
!  rcp = rho cp ch v
!
      do i = 1, im
        rch(i) = rho(i) * cp * ch(i) * wind(i)
      enddo
!
!  sensible and latent heat flux over open water
!
      do i = 1, im
        if(slimsk(i).eq.0.) then
          evap(i) = elocp * rch(i) * (qss(i) - q1(i))
          dm(i) = 1.
          qsurf(i) = qss(i)
        endif
      enddo
!
!  compute soil/snow/ice heat flux in preparation for surface energy
!  balance calculation
!
      do i = 1, im
        gflux(i) = 0.
        if(slimsk(i).eq.1.) then
          smcz(i) = .5 * (smc(i,1) + .20)
          dft0(i) = ktsoil(smcz(i),soiltyp(i))
        elseif(slimsk(i).eq.2.) then
!  df for ice is taken from maykut and untersteiner
!  df is in si unit of w k-1 m-1
          dft0(i) = 2.2
        endif
      enddo
!!
      do i=1,im
        if(slimsk(i).ne.0.) then
!         if(snowd(i).gt..001) then
          if(flagsnw(i)) then
!
!  when snow covered, ground heat flux comes from snow
!
            tflx = min(t1(i), tsurf(i))
            gflux(i) = -dfsnow * (tflx - stsoil(i,1))                    &    
     &                 / (factsnw(i) * max(snowd(i),.001))
          else
            gflux(i) = dft0(i) * (stsoil(i,1) - tsurf(i))                &   
     &                 / (-.5 * zsoil(i,1))
          endif
          gflux(i) = max(gflux(i),-200.)
          gflux(i) = min(gflux(i),+200.)
        endif
      enddo
      do i = 1, im
        flag(i) = slimsk(i).ne.0.
        partlnd(i) = 1.
        if(snowd(i).gt.0..and.snowd(i).le..001) then
          partlnd(i) = 1. - snowd(i) / .001
        endif
      enddo
      do i = 1, im
        snoevp(i) = 0.
        if(snowd(i).gt..001) partlnd(i) = 0.
      enddo
!
!  compute potential evaporation for land and sea ice
!
      do i = 1, im
        if(flag(i)) then
          t12 = t1(i) * t1(i)
          t14 = t12 * t12
!
!  rcap = fnet - sigma t**4 + gflx - rho cp ch v (t1-theta1)
!
          rcap(i) = -slwd(i) - sigma * t14 + gflux(i)                    &      
     &              - rch(i) * (t1(i) - theta1(i))
!
!  rsmall = 4 sigma t**3 / rch(i) + 1
!
          rsmall(i) = 4. * sigma * t1(i) * t12 / rch(i) + 1.
!
!  delta = l / cp * dqs/dt
!
          delta(i) = elocp * eps * hvap * qs1(i) / (rd * t12)
!
!  potential evapotranspiration ( watts / m**2 ) and
!  potential evaporation
!
          term1(i) = elocp * rsmall(i) * rch(i)*(qs1(i)-q0(i))
          term2(i) = rcap(i) * delta(i)
          ep(i) = (elocp * rsmall(i) * rch(i) * (qs1(i) - q0(i))         &   
     &              + rcap(i) * delta(i))
          ep(i) = ep(i) / (rsmall(i) + delta(i))
        endif
      enddo
!
!  actual evaporation over land in three parts : edir, et, and ec
!
!  direct evaporation from soil, the unit goes from m s-1 to kg m-2 s-1
!
      do i = 1, im
        flag(i) = slimsk(i).eq.1..and.ep(i).gt.0.
      enddo
      do i = 1, im
        if(flag(i)) then
          df1(i) = funcdf(smc(i,1),soiltyp(i))
          kt1(i) = funckt(smc(i,1),soiltyp(i))
        endif
        if(flag(i).and.stc(i,1).lt.t0c) then
          df1(i) = 0.
          kt1(i) = 0.
        endif
        if(flag(i)) then
!         tref = .75 * thsat(soiltyp(i))
          tref(i) = smref(soiltyp(i))
!         twilt = twlt(soiltyp(i))
          twilt(i) = smwlt(soiltyp(i))
          smcdry = smdry(soiltyp(i))
!         fx(i) = -2. * df1(i) * (smc(i,1) - .23) / zsoil(i,1)
!    &            - kt1(i)
          fx(i) = -2. * df1(i) * (smc(i,1) - smcdry) / zsoil(i,1)        &     
     &            - kt1(i)
          fx(i) = min(fx(i), ep(i)/hvap)
          fx(i) = max(fx(i),0.)
!
!  sigmaf is the fraction of area covered by vegetation
!
          edir(i) = fx(i) * (1. - sigmaf(i)) * partlnd(i)
        endif
      enddo
!
!  calculate stomatal resistance
!
      do i = 1, im
        if(flag(i)) then
!
!  resistance due to par. we use net solar flux as proxy at the present time
!
          ff = .55 * 2. * snet(i) / rgl(vegtype(i))
          rcs = (ff + rs(i)/rsmax(vegtype(i))) / (1. + ff)
          rcs = max(rcs,.0001)
          rct = 1.
          rcq = 1.
!
!  resistance due to thermal effect
!
!         rct = 1. - .0016 * (topt - theta1) ** 2
!         rct = max(rct,.0001)
!
!  resistance due to humidity
!
!         rcq = 1. / (1. + hs(vegtype(i)) * (qs1(i) - q0(i)))
!         rcq = max(rcq,.0001)
!
!  compute resistance without the effect of soil moisture
!
          rs(i) = rs(i) / (rcs * rct * rcq)
        endif
      enddo
!
!  transpiration from all levels of the soil
!
      do i = 1, im
        if(flag(i)) then
          canfac(i) = (canopy(i) / scanop) ** cfactr
        endif
        if(flag(i)) then
          etpfac(i) = sigmaf(i)                                          &    
     &           * (1. - canfac(i)) / hvap
          gx(i) = (smc(i,1) - twilt(i)) / (tref(i) - twilt(i))
          gx(i) = max(gx(i),0.)
          gx(i) = min(gx(i),1.)
!
!  resistance due to soil moisture deficit
!
          rss = gx(i) * (zsoil(i,1) / zsoil(i,km))
          rss = max(rss,.0001)
          rsi = rs(i) / rss
!
!  transpiration a la monteith
!
          eth = (term1(i) + term2(i)) /                                  &   
     &          (delta(i) + rsmall(i) * (1. + rsi * ch(i) * wind(i)))
          et(i,1) = etpfac(i) * eth * partlnd(i)
        endif
      enddo
!!
      do k = 2, km
        do i=1,im
          if(flag(i)) then
            gx(i) = (smc(i,k) - twilt(i)) / (tref(i) - twilt(i))
            gx(i) = max(gx(i),0.)
            gx(i) = min(gx(i),1.)
!
!  resistance due to soil moisture deficit
!
          rss = gx(i) * ((zsoil(i,k) - zsoil(i,k-1))/zsoil(i,km))
          rss = max(rss,1.e-6)
          rsi = rs(i) / rss
!
!  transpiration a la monteith
!
          eth = (term1(i) + term2(i)) /                                  &    
     &          (delta(i) + rsmall(i) * (1. + rsi * ch(i) * wind(i)))
            et(i,k) = eth * etpfac(i) * partlnd(i)
          endif
        enddo
      enddo
!!
 400  continue
!
!  canopy re-evaporation
!
      do i=1,im
        if(flag(i)) then
          ec(i) = sigmaf(i) * canfac(i) * ep(i) / hvap
          ec(i) = ec(i) * partlnd(i)
          ec(i) = min(ec(i),canopy(i)/delt)
        endif
      enddo
!
!  sum up total evaporation
!
      do i = 1, im
        if(flag(i)) then
         evap(i) = edir(i) + ec(i)
        endif
      enddo
!!
      do k = 1, km
        do i=1,im
          if(flag(i)) then
            evap(i) = evap(i) + et(i,k)
          endif
        enddo
      enddo
!!
!
!  return evap unit from kg m-2 s-1 to watts m-2
!
      do i=1,im
        if(flag(i)) then
          evap(i) = min(evap(i)*hvap,ep(i))
        endif
      enddo
!##dg  if(lat.eq.latd) then
!##dg    print *, 'fx(i), sigmaf, edir(i), etpfac=', fx(i)*hvap,sigmaf,
!##dg&          edir(i)*hvap,etpfac*hvap
!##dg    print *, ' et =', (et(k)*hvap,k=1,km)
!##dg    print *, ' canfac(i), ec(i), evap', canfac(i),ec(i)*hvap,evap
!##dg  endif
!
!  evaporation over bare sea ice
!
      do i = 1, im
!       if(slimsk(i).eq.2.and.snowd(i).le..001) then
        if(slimsk(i).eq.2.) then
          evap(i) = partlnd(i) * ep(i)
        endif
      enddo
!
!  treat downward moisture flux situation
!  (evap was preset to zero so no update needed)
!  dew is converted from kg m-2 to m to conform to precip unit
!
      do i = 1, im
        flag(i) = slimsk(i).ne.0..and.ep(i).le.0.
        dew(i) = 0.
      enddo
      do i = 1, im
        if(flag(i)) then
          dew(i) = -ep(i) * delt / (hvap * rhoh2o)
          evap(i) = ep(i)
          dew(i) = dew(i) * partlnd(i)
          evap(i) = evap(i) * partlnd(i)
          dm(i) = 1.
        endif
      enddo
!
!  snow covered land and sea ice
!
      do i = 1, im
        flag(i) = slimsk(i).ne.0..and.snowd(i).gt.0.
      enddo
!
!  change of snow depth due to evaporation or sublimation
!
!  convert evap from kg m-2 s-1 to m s-1 to determine the reduction of s
!
!        print *, 'in progtm, 0, tsurf=',tsurf
      do i = 1, im
        if(flag(i)) then
          bfact = snowd(i) / (delt * ep(i) / (hvap * rhoh2o))
          bfact = min(bfact,1.)
!
!  the evaporation of snow
!
          if(ep(i).le.0.) bfact = 1.
          if(snowd(i).le..001) then
!           evap = (snowd(i)/.001)*bfact*ep(i) + evap
!           snoevp(i) = bfact * ep(i) * (1. - partlnd(i))
!           evap = evap + snoevp(i)
            snoevp(i) = bfact * ep(i)
!           evap   = evap + snoevp(i) * (1. - partlnd(i))
            evap(i)=evap(i)+snoevp(i)*(1.-partlnd(i))
          else
!           evap(i) = bfact * ep(i)
            snoevp(i) = bfact * ep(i)
            evap(i) = snoevp(i)
          endif
          tsurf(i) = t1(i) +                                             &   
     &          (rcap(i) - gflux(i) - dfsnow * (t1(i) - stsoil(i,1))     &
     &           /(factsnw(i) * max(snowd(i),.001))                      &
!    &           + theta1 - t1
!    &           - bfact * ep(i)) / (rsmall(i) * rch(i)
     &           - snoevp(i)) / (rsmall(i) * rch(i)                      &
     &           + dfsnow / (factsnw(i)* max(snowd(i),.001)))
!           if ( i .eq.im ) then
!             print *,'t1, rcap, gflux, dfsnow, stsoil, fact',
!     1         t1(i),rcap(i),gflux(i),dfsnow,stsoil(i,1),factsnw(i)
!             print *,'snowd,snoevp,rsmall,rch',
!     1         snowd(i),snoevp(i),rsmall(i),rch(i)
!           endif
     
!         snowd(i) = snowd(i) - bfact * ep(i) * delt / (rhoh2o * hvap)
          snowd(i) = snowd(i) - snoevp(i) * delt / (rhoh2o * hvap)
          snowd(i) = max(snowd(i),0.)
        endif
      enddo
!        print *, 'in progtm, 1, tsurf=',tsurf
!
!  snow melt (m)
!
 500  continue
      do i = 1, im
        flag(i) = slimsk(i).ne.0..and.snowd(i).gt..0
      enddo
      do i = 1, im
        if(flag(i).and.tsurf(i).gt.t0c) then
          snowmt(i) = rch(i) * rsmall(i) * delt                          &    
     &              * (tsurf(i) - t0c) / (rhoh2o * hfus)
          snowmt(i) = min(snowmt(i),snowd(i))
          snowd(i) = snowd(i) - snowmt(i)
          snowd(i) = max(snowd(i),0.)
          tsurf(i) = max(t0c,tsurf(i)                                    &
     &             -hfus*snowmt(i)*rhoh2o/(rch(i)*rsmall(i)*delt))
        endif
      enddo
!
!  we need to re-evaluate evaporation because of snow melt
!    the skin temperature is now bounded to 0 deg c
!
!     qss = fpvs(tsurf)
      do i = 1, im
!       if (snowd(i) .gt. 0.0) then
        if (snowd(i) .gt. snomin) then
!jfe      qss(i) = 1000. * fpvs(tsurf(i))
          qss(i) = fpvs(tsurf(i))
          qss(i) = eps * qss(i) / (psurf(i) + epsm1 * qss(i))
          evap(i) = elocp * rch(i) * (qss(i) - q0(i))
        endif
      enddo
!
!  prepare tendency terms for the soil moisture field without precipitat
!  the unit of moisture flux needs to become m s-1 for soil moisture
!   hence the factor of rhoh2o
!
      do i = 1, im
        flag(i) = slimsk(i).eq.1.
        if(flag(i)) then
          df1(i) = funcdf(smcz(i),soiltyp(i))
          kt1(i) = funckt(smcz(i),soiltyp(i))
        endif
        if(flag(i).and.stc(i,1).lt.t0c) then
          df1(i) = 0.
          kt1(i) = 0.
        endif
        if(flag(i)) then
          rhscnpy(i) = -ec(i) + sigmaf(i) * rhoh2o * dew(i) / delt
          smcz(i) = max(smc(i,1), smc(i,2))
          dmdz(i) = (smc(i,1) - smc(i,2)) / (-.5 * zsoil(i,2))
          rhsmc(i,1) = (df1(i) * dmdz(i) + kt1(i)                        &    
     &        + (edir(i) + et(i,1))) / (zsoil(i,1) * rhoh2o)
          rhsmc(i,1) = rhsmc(i,1) - (1. - sigmaf(i)) * dew(i) /          &
     &                 ( zsoil(i,1) * delt)
          ddz(i) = 1. / (-.5 * zsoil(i,2))
!
!  aim, bim, and cim are the elements of the tridiagonal matrix for the
!  implicit update of the soil moisture
!
          aim(i,1) = 0.
          bim(i,1) = df1(i) * ddz(i) / (-zsoil(i,1) * rhoh2o)
          cim(i,1) = -bim(i,1)
        endif
      enddo
!!
      do k = 2, km
        if(k.lt.km) then
          do i=1,im
            if(flag(i)) then
              df2 = funcdf(smcz(i),soiltyp(i))
              kt2(i) = funckt(smcz(i),soiltyp(i))
            endif
            if(flag(i).and.stc(i,k).lt.t0c) then
              df2 = 0.
              kt2(i) = 0.
            endif
            if(flag(i)) then
              dmdz2(i) = (smc(i,k) - smc(i,k+1))                         &     
     &                   / (.5 * (zsoil(i,k-1) - zsoil(i,k+1)))
              smcz(i) = max(smc(i,k), smc(i,k+1))
              rhsmc(i,k) = (df2 * dmdz2(i) + kt2(i)                      &   
     &             - df1(i) * dmdz(i) - kt1(i) + et(i,k))                &
     &                     / (rhoh2o*(zsoil(i,k) - zsoil(i,k-1)))
              ddz2(i) = 2. / (zsoil(i,k-1) - zsoil(i,k+1))
              cim(i,k) = -df2 * ddz2(i)                                  &  
     &                / ((zsoil(i,k-1) - zsoil(i,k))*rhoh2o)
            endif
          enddo
        else
          do i = 1, im
            if(flag(i)) then
              kt2(i) = funckt(smc(i,k),soiltyp(i))
            endif
            if(flag(i).and.stc(i,k).lt.t0c) kt2(i) = 0.
            if(flag(i)) then
              rhsmc(i,k) = (kt2(i)                                       &   
     &             - df1(i) * dmdz(i) - kt1(i) + et(i,k))                &   
     &                     / (rhoh2o*(zsoil(i,k) - zsoil(i,k-1)))
              drain(i) = kt2(i)
              cim(i,k) = 0.
            endif
          enddo
        endif
        do i = 1, im
          if(flag(i)) then
            aim(i,k) = -df1(i) * ddz(i)                                  &
     &                / ((zsoil(i,k-1) - zsoil(i,k))*rhoh2o)
            bim(i,k) = -(aim(i,k) + cim(i,k))
            df1(i) = df2
            kt1(i) = kt2(i)
            dmdz(i) = dmdz2(i)
            ddz(i) = ddz2(i)
          endif
        enddo
      enddo
!!
 600  continue
!
!  update soil temperature and sea ice temperature
!
      do i=1,im
        flag(i) = slimsk(i).ne.0.
      enddo
!
!  surface temperature is part of the update when snow is absent
!
      do i=1,im
!       if(flag(i).and.snowd(i).le..001) then
        if(flag(i).and..not.flagsnw(i)) then
          yy(i) = t1(i) +                                                &   
     &          (rcap(i)-gflux(i)                                        &
     &           - evap(i)) / (rsmall(i) * rch(i))
          zz(i) = 1. + dft0(i) / (-.5 * zsoil(i,1) * rch(i) * rsmall(i))
          xx(i) = dft0(i) * (stsoil(i,1) - yy(i)) /                      &
     &            (.5 * zsoil(i,1) * zz(i))
        endif
!       if(flag(i).and.snowd(i).gt..001) then
        if(flag(i).and.flagsnw(i)) then
          yy(i) = stsoil(i,1)
!
!  heat flux from snow is explicit in time
!
          zz(i) = 1.
          xx(i) = dfsnow * (stsoil(i,1) - tsurf(i))                      &  
     &            / (-factsnw(i) * max(snowd(i),.001))
        endif
      enddo
!
!  compute the forcing and the implicit matrix elements for update
!
!  ch2o is the heat capacity of water and csoil is the heat capacity of
!
      do i = 1, im
        if(flag(i)) then
          smcz(i) = max(smc(i,1), smc(i,2))
          dtdz1(i) = (stsoil(i,1) - stsoil(i,2)) / (-.5 * zsoil(i,2))
          if(slimsk(i).eq.1.) then
            dft1(i) = ktsoil(smcz(i),soiltyp(i))
            hcpct(i) = smc(i,1) * ch2o + (1. - smc(i,1)) * csoil
          else
            dft1(i) = dft0(i)
            hcpct(i) = cice
          endif
          dft2(i) = dft1(i)
          ddz(i) = 1. / (-.5 * zsoil(i,2))
!
!  ai, bi, and ci are the elements of the tridiagonal matrix for the
!  implicit update of the soil temperature
!
          ai(i,1) = 0.
          bi(i,1) = dft1(i) * ddz(i) / (-zsoil(i,1) * hcpct(i))
          ci(i,1) = -bi(i,1)
          bi(i,1) = bi(i,1)                                              &   
     &            + dft0(i) / (.5 * zsoil(i,1) **2 * hcpct(i) * zz(i))
!         ss = dft0(i) * (stsoil(i,1) - yy(i))
!    &         / (.5 * zsoil(i,1) * zz(i))
!         rhstc(1) = (dft1(i) * dtdz1(i) - ss)
          rhstc(i,1) = (dft1(i) * dtdz1(i) - xx(i))                      &   
     &                 / (zsoil(i,1) * hcpct(i))
        endif
      enddo
!!
      do k = 2, km
        do i=1,im
          if(slimsk(i).eq.1.) then
            hcpct(i) = smc(i,k) * ch2o + (1. - smc(i,k)) * csoil
          elseif(slimsk(i).eq.2.) then
            hcpct(i) = cice
          endif
        enddo
        if(k.lt.km) then
          do i = 1, im
            if(flag(i)) then
              dtdz2(i) = (stsoil(i,k) - stsoil(i,k+1))                   &   
     &                   / (.5 * (zsoil(i,k-1) - zsoil(i,k+1)))
              smcz(i) = max(smc(i,k), smc(i,k+1))
              if(slimsk(i).eq.1.) then
                dft2(i) = ktsoil(smcz(i),soiltyp(i))
              endif
              ddz2(i) = 2. / (zsoil(i,k-1) - zsoil(i,k+1))
              ci(i,k) = -dft2(i) * ddz2(i)                               &
     &                / ((zsoil(i,k-1) - zsoil(i,k)) * hcpct(i))
            endif
          enddo
        else
!
!  at the bottom, climatology is assumed at 2m depth for land and
!  freezing temperature is assumed for sea ice at z(km)
          do i = 1, im
            if(slimsk(i).eq.1.) then
              dtdz2(i) = (stsoil(i,k) - tg3(i))                          &
     &              / (.5 * (zsoil(i,k-1) + zsoil(i,k)) - zbot)
              dft2(i) = ktsoil(smc(i,k),soiltyp(i))
              ci(i,k) = 0.
            endif
            if(slimsk(i).eq.2.) then
              dtdz2(i) = (stsoil(i,k) - tgice)                           &
     &                   / (.5 * zsoil(i,k-1) - .5 * zsoil(i,k))
              dft2(i) = dft1(i)
              ci(i,k) = 0.
            endif
          enddo
        endif
        do i = 1, im
          if(flag(i)) then
            rhstc(i,k) = (dft2(i) * dtdz2(i) - dft1(i) * dtdz1(i))       &
     &                 / ((zsoil(i,k) - zsoil(i,k-1)) * hcpct(i))
            ai(i,k) = -dft1(i) * ddz(i)                                  &
     &                / ((zsoil(i,k-1) - zsoil(i,k)) * hcpct(i))
            bi(i,k) = -(ai(i,k) + ci(i,k))
            dft1(i) = dft2(i)
            dtdz1(i) = dtdz2(i)
            ddz(i) = ddz2(i)
          endif
        enddo
      enddo
!!
 700  continue
!
!  solve the tri-diagonal matrix
!
      do k = 1, km
        do i=1,im
          if(flag(i))  then
            rhstc(i,k) = rhstc(i,k) * delt2
            ai(i,k) = ai(i,k) * delt2
            bi(i,k) = 1. + bi(i,k) * delt2
            ci(i,k) = ci(i,k) * delt2
          endif
        enddo
      enddo
!  forward elimination
      do i=1,im
        if(flag(i)) then
          ci(i,1) = -ci(i,1) / bi(i,1)
          rhstc(i,1) = rhstc(i,1) / bi(i,1)
        endif
      enddo
!!
      do k = 2, km
        do i=1,im
          if(flag(i)) then
            cc = 1. / (bi(i,k) + ai(i,k) * ci(i,k-1))
            ci(i,k) = -ci(i,k) * cc
            rhstc(i,k) = (rhstc(i,k) - ai(i,k) * rhstc(i,k-1)) * cc
          endif
        enddo
      enddo
!!
!  backward substituttion
      do i=1,im
        if(flag(i)) then
          ci(i,km) = rhstc(i,km)
        endif
      enddo
!!
      do k = km-1, 1
        do i=1,im
          if(flag(i)) then
            ci(i,k) = ci(i,k) * ci(i,k+1) + rhstc(i,k)
          endif
        enddo
      enddo
!
!  update soil and ice temperature
!
      do k = 1, km
        do i=1,im
          if(flag(i)) then
            stsoil(i,k) = stsoil(i,k) + ci(i,k)
          endif
        enddo
      enddo
!
!  update surface temperature for snow free surfaces
!
      do i=1,im
!       if(slimsk(i).ne.0..and.snowd(i).le..001) then
        if(slimsk(i).ne.0..and..not.flagsnw(i)) then
          tsurf(i) = (yy(i) + (zz(i) - 1.) * stsoil(i,1)) / zz(i)
        endif
!       if(slimsk(i).eq.2..and.snowd(i).le..001) then
        if(slimsk(i).eq.2..and..not.flagsnw(i)) then
          tsurf(i) = min(tsurf(i),t0c)
        endif
      enddo
!!
      do k = 1, km
        do i=1,im
          if(slimsk(i).eq.2) then
            stsoil(i,k) = min(stsoil(i,k),t0c)
          endif
        enddo
      enddo
!
!  time filter for soil and skin temperature
!
      do i=1,im
        if(slimsk(i).ne.0.) then
          tskin(i) = ctfil1 * tsurf(i) + ctfil2 * tskin(i)
        endif
      enddo
      do k = 1, km
        do i=1,im
          if(slimsk(i).ne.0.) then
            stc(i,k) = ctfil1 * stsoil(i,k) + ctfil2 * stc(i,k)
          endif
        enddo
      enddo
!
!  gflux calculation
!
      do i=1,im
        flag(i) = slimsk(i).ne.0.  .and.flagsnw(i)
      enddo
      do i = 1, im
        if(flag(i)) then
          gflux(i) = -dfsnow * (tskin(i) - stc(i,1))                     &    
     &               / (factsnw(i) * max(snowd(i),.001))
        endif
      enddo
      do i = 1, im
!       if(slimsk(i).ne.0..and.snowd(i).le..001) then
        if(slimsk(i).ne.0..and..not.flagsnw(i)) then
          gflux(i) = dft0(i) * (stc(i,1) - tskin(i))                     &   
     &               / (-.5 * zsoil(i,1))
        endif
      enddo
!
!  calculate sensible heat flux
!
      do i = 1, im
        hflx(i) = rch(i) * (tskin(i) - theta1(i))
      enddo
!
!  the rest of the output
!
      do i = 1, im
        qsurf(i) = q1(i) + evap(i) / (elocp * rch(i))
        dm(i) = 1.
!
!  convert snow depth back to mm of water equivalent
!
        sheleg(i) = snowd(i) * 1000.
      enddo
!
      do i = 1, im
        f10m(i) = fm10(i) / fm(i)
        f10m(i) = min(f10m(i),1.)
        u10m(i) = f10m(i) * xrcl(i) * u1(i)
        v10m(i) = f10m(i) * xrcl(i) * v1(i)
         t2m(i) = tskin(i) * (1. - fh2(i) / fh(i))                       &
     &          + theta1(i) * fh2(i) / fh(i)
         t2m(i) = t2m(i) * sig2k
        if(evap(i).ge.0.) then
!
!  in case of evaporation, use the inferred qsurf to deduce q2m
!
          q2m(i) = qsurf(i) * (1. - fh2(i) / fh(i))                      &   
     &         + q1(i) * fh2(i) / fh(i)
        else
!
!  for dew formation situation, use saturated q at tskin
!
!jfe      qss(i) = 1000. * fpvs(tskin(i))
          qss(i) = fpvs(tskin(i))
          qss(i) = eps * qss(i) / (psurf(i) + epsm1 * qss(i))
          q2m(i) = qss(i) * (1. - fh2(i) / fh(i))+ q1(i)*fh2(i)/fh(i)
        endif
!jfe    qss(i) = 1000. * fpvs(t2m(i))
        qss(i) = fpvs(t2m(i))
!       qss(i) = 1000. * t2mo(i)
        qss(i) = eps * qss(i) / (psurf(i) + epsm1 * qss(i))
        q2m(i) = min(q2m(i),qss(i))
      enddo
!!
!     do i = 1, im
!       rnet(i) = -slwd(i) - sigma * tskin(i) **4
!     enddo
!!
!
      do i=1,im
        tem     = 1.0 / rho(i)
        hflx(i) = hflx(i) * tem * cpinv
        evap(i) = evap(i) * tem * hvapi
      enddo
!
!##dg  if(lat.eq.latd) then
!c       rbal = -slwd-sigma*tskin**4+gflux
!c    &         -evap - hflx
!##dg    print 6000,hflx,evap,gflux,
!##dg&             stc(1), stc(2),tskin,rnet,slwd
!##dg    print *, ' t1 =', t1
 6000 format(8(f8.2,','))
!c     print *, ' ep, etp,t2m(i) =', ep, etp,t2m(i)
!c     print *, ' fh, fh2 =', fh, fh2
!c     print *, ' ph, ph2 =', ph, ph2
!c     print *, ' ch, rch =', ch, rch
!c     print *, ' term1, term2 =', term1, term2
!c     print *, ' rs(i), plantr =', rs(i), plantr
!##dg  endif
      return
      end
