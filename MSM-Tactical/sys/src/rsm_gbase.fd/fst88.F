!     *****************************************************************
!          subroutine fst88 is the main computation module of the
!     long-wave radiation code. in it all "emissivity" calculations,
!     including calls to table lookup subroutines. also,after calling
!     subroutine "spa88", final combined heating rates and ground
!     flux are obtained.
!     *****************************************************************
!              inputs:
!        betinw,betawd,ab15wd              bdwide
!        betad,bo3rnd,ao3rnd               bandta
!        cldfac                            cldcom
!        qh2o,p,delp2,delp,t,var1,var2,    kdacom
!        var3,var4,cntval                  kdacom
!        totvo2,toto3,totphi,empl,emx1     kdacom
!        tphio3,emx2                       kdacom
!        temp,press                        radisw
!        nclds,ktop,kbtm,camt              radisw
!        ind,indx2,kmaxv,source,dsrce      tabcom
!        skc1r,skc3r,kmaxvm,nrep1,nrep2    tabcom
!        nst1,nst2,nrp1,nrp2               tabcom
!        co2nbl,co21                       tfcom
!        co2sp1,co2sp2                     tfcom
!              outputs:
!        heatra,grnflx,topflx              lwout
!
!          called by  :    radmn or main pgm
!          calls      :    clo88,e1e288,e3v88,spa88,nlte
!
!        passed variables:
!              in e3v88:
!        emd     =  e3 function for h2o lines (0-560,1200-2200 cm-1)
!                     computed in e3v88
!        tpl     =  temperature input for e3 calculation in e3v88
!        empl    =  h2o amount,input for e3 calculation in e3v88
!                   (computed in lwr88; stored in kdacom.h)
!              in e1e288:
!        e1cts1  =  e1 function for the (i+1)th level using the
!                   temperature of the ith data level,computed over
!                   the frequency range 0-560,1200-2200 cm-1. (e1cts1-
!                   e1ctw1) is used in obtaining the flux at the top
!                   in the 0-160,1200-2200 cm-1 range (flx1e1).
!        e1cts2  =  e1 function for the ith level, using the temp. of
!                   the ith data level,computed over the frequency range
!                   0-560,1200-2200 cm-1. (e1cts2-e1ctw2) is also used
!                   in obtaining the flux at the top in the 0-160,.
!                   1200-2200 cm-1 range.
!        e1flx   =  e1 fctn. for the ith level,using the temperature at
!                   the top of the atmosphere. computed over the freq.
!                   range 0-560,1200-2200 cm-1. used for q(approx) term.
!                   (in module block tfcom)
!        e1ctw1  =  like e1cts1,but computed over the 160-560 cm-1 range
!                   and used for q(approx,cts) calculation
!        e1ctw2  =  like e1cts2,but computed over the 160-560 cm-1 range
!                   and used for q(approx,cts) calculation
!        fxo     =  temperature index used for e1 function and also
!                   used for source function calc. in fst88.
!        dt      =  temp. diff.between model temps. and temps. at
!                   tabular values of e1 and source fctns. used in
!                   fst88 and in e1 function calc.
!        fxoe2   =  temperature index used for e2 function
!        dte2    =  temp. diff. between model temp. and temps. at
!                   tabular values of e2 function.
      subroutine fst88(heatra,grnflx,topflx,                            &
     &                 qh2o,press,p,delp,delp2,temp,t,                  &
     &                 cldfac,                                          &
!    &                 cldfac,nclds,ktop,kbtm,camt,                     &
     &                 co21,co2nbl,co2sp1,co2sp2,                       &
     &                 var1,var2,var3,var4,cntval,                      &
     &                 toto3,tphio3,totphi,totvo2,                      &
     &                 emx1,emx2,empl                                   &
     &,                l, lp1, lp1v, llp1, imax                         &
     &,                source,dsrce)
!
!fpp$ noconcur r
!
      use machine , only : kind_rad
      use hcon
      use rnddta
      implicit none
!
      integer l, lp1, lp1v, llp1, imax
!
      real (kind=kind_rad) source(28,nbly), dsrce(28,nbly)
!
      real (kind=kind_rad) qh2o(imax,lp1), press(imax,lp1)              &
     &,                     p(imax,lp1),    delp(imax,l),  delp2(imax,l)&
     &,                     temp(imax,lp1), t(imax,lp1)                 &
     &,                     cldfac(imax,lp1,lp1)                        &
     &,                     co21(imax,lp1,lp1), co2nbl(imax,l)          &
     &,                     co2sp1(imax,lp1),   co2sp2(imax,lp1)        &
     &,                     var1(imax,l),       var2(imax,l)            &
     &,                     var3(imax,l),       var4(imax,l)            &
     &,                     cntval(imax,lp1),   topflx(imax)            &
     &,                     heatra(imax,l),     grnflx(imax)
!
      real (kind=kind_rad) gxcts(imax),      flx1e1(imax)               &
     &,                     avephi(imax,lp1), emiss(imax,lp1)           &
     &,                     emissb(imax,lp1)                            &
     &,                    toto3(imax,lp1),   tphio3(imax,lp1)          &
     &,                    totphi(imax,lp1),  totvo2(imax,lp1)          & 
     &,                    emx1(imax),        emx2(imax)                &
     &,                    empl(imax,llp1)
!
      real (kind=kind_rad) excts(imax,l),   ctso3(imax,l), cts(imax,l)  &
     &,                     e1flx(imax,lp1), co2sp(imax,lp1)            &
     &,                     to3spc(imax,l),  to3sp(imax,lp1)
      real (kind=kind_rad) oss(imax,lp1),   css(imax,lp1)               &
     &,                     ss1(imax,lp1),   ss2(imax,lp1)              &
     &,                     tc(imax,lp1),    dtc(imax,lp1)              &
     &,                     sorc(imax,lp1,nbly), csour(imax,lp1)
!cc
      real (kind=kind_rad) avvo2(imax,lp1), over1d(imax,lp1)            &
     &,                     to31d(imax,lp1), cont1d(imax,lp1)           &
     &,                     avmo3(imax,lp1), avpho3(imax,lp1)           &
     &,                     c(imax,llp1),    c2(imax,llp1)              &
     &,                     emspec(imax,2)
!
!---dimension of variables equivalenced to those in vtemp---
      real (kind=kind_rad) vtmp3(imax,lp1)                              &
     &,                     alp(imax,llp1)                              &! tpl used in place of csub
     &,                     delpr1(imax,lp1), delpr2(imax,lp1)          &
     &,                     emisdg(imax,lp1), contdg(imax,lp1)          &
     &,                     to3dg(imax,lp1),  flxnet(imax,lp1)          &
     &,                     vsum1(imax,lp1)
!
!---dimension of variables passed to other subroutines---
!   (and not found in module blocks)
      real (kind=kind_rad) e1cts1(imax,lp1), e1cts2(imax,l)             &
     &,                     e1ctw1(imax,lp1), e1ctw2(imax,l)            &
     &,                     tpl(imax,llp1)        ! tpl used as emd as well
!   it is possible to equivalence emd,tpl to the above variables,
!   as they get called at different times
      real (kind=kind_rad) fxo(imax,lp1),   dt(imax,lp1)                &
     &,                     fxoe2(imax,lp1), dte2(imax,lp1)             &
     &,                     fxosp(imax,2),   dtsp(imax,2)
!
!     dimension of local variables
      real (kind=kind_rad) rlog(imax,l),     flx(imax,lp1)              &
     &,                     totevv(imax,lp1), cnttau(imax,lp1)
!
      real (kind=kind_rad) vtmp, fac1, tem, tmp3, du, fyo, dt3          &
     &,                     ww1, ww2, fxo3, csub2
      integer lm1, ll, llm1, llm2, i, k, item, k1, kk, klen             &
     &,       kk1, kkk, kp, ival, it
!
!          first section is table lookup for source function and
!     derivative (b and db/dt).also,the nlte co2 source function
!     is obtained
!
!---in calcs. below, decrementing the index by 9
!   accounts for the tables beginning at t=100k.
!   at t=100k.
!
      lm1  = l - 1
      ll   = llp1 - 1
      llm1 = ll - 1
      llm2 = ll - 2
!
!                                 ******* e1 source *******
      do k=1,lp1
        do i=1,imax
          vtmp         = aint(temp(i,k)*hp1)
          fxo(i,k)     = vtmp      - 9.0
          dt(i,k)      = temp(i,k) - ten*vtmp      
!
          item         = fxo(i,k)
!
!       source function for 14 combined bands
!         band 9  - (560-670 cm-1)  band 10 - (670-800 cm-1)
!         band 11 - (800-900 cm-1)  band 12 - (900-990 cm-1)
!         band 13 - (990-1070 cm-1) band 14 - (1070-1200 cm-1)
!
          sorc(i,k,1)  = source(item,1)  + dt(i,k)*dsrce(item,1)  ! band 1
          sorc(i,k,2)  = source(item,2)  + dt(i,k)*dsrce(item,2)  ! band 2
          sorc(i,k,3)  = source(item,3)  + dt(i,k)*dsrce(item,3)  ! band 3
          sorc(i,k,4)  = source(item,4)  + dt(i,k)*dsrce(item,4)  ! band 4
          sorc(i,k,5)  = source(item,5)  + dt(i,k)*dsrce(item,5)  ! band 5
          sorc(i,k,6)  = source(item,6)  + dt(i,k)*dsrce(item,6)  ! band 6
          sorc(i,k,7)  = source(item,7)  + dt(i,k)*dsrce(item,7)  ! band 7
          sorc(i,k,8)  = source(item,8)  + dt(i,k)*dsrce(item,8)  ! band 8
          sorc(i,k,9)  = source(item,9)  + dt(i,k)*dsrce(item,9)  ! band 9
          sorc(i,k,10) = source(item,10) + dt(i,k)*dsrce(item,10) ! band 10
          sorc(i,k,11) = source(item,11) + dt(i,k)*dsrce(item,11) ! band 11
          sorc(i,k,12) = source(item,12) + dt(i,k)*dsrce(item,12) ! band 12
          sorc(i,k,13) = source(item,13) + dt(i,k)*dsrce(item,13) ! band 13
          sorc(i,k,14) = source(item,14) + dt(i,k)*dsrce(item,14) ! band 14
        enddo
      enddo
!
!---temp. indices for e2 (kp=1 layer not used in flux calculations)
      do k=1,l
        do i=1,imax
          vtmp       = aint(t(i,k+1)*hp1)
          fxoe2(i,k) = vtmp   - 9.0
          dte2(i,k)  = t(i,k+1) - ten*vtmp
        enddo
      enddo
!---special case to handle kp=lp1 layer and special e2 calcs.
      do i=1,imax
        fxoe2(i,lp1) = fxo(i,l)
        dte2(i,lp1)  = dt(i,l)
        fxosp(i,1)   = fxoe2(i,lm1)
        fxosp(i,2)   = fxo(i,lm1)
        dtsp(i,1)    = dte2(i,lm1)
        dtsp(i,2)    = dt(i,lm1)
      enddo
!
!        the following subroutine obtains nlte source function for co2
!
!     call nlte
!
!---obtain special source functions for the 15 um band (csour)
!   and the window region (ss1).  also
!---compute temp**4 (tc) and vertical temperature differences
!   (oss,css,ss2,dtc). all these will be used later in flux computations.
!
      do k=1,lp1
        do i=1,imax
          ss1(i,k)   = sorc(i,k,11) + sorc(i,k,12) + sorc(i,k,14)
          csour(i,k) = sorc(i,k,9) + sorc(i,k,10)
          vtmp       = temp(i,k) * temp(i,k)
          tc(i,k)    = vtmp * vtmp
        enddo
      enddo
      do k=1,l
        k1 = k + 1
        do i=1,imax
          oss(i,k1) = sorc(i,k1,13) - sorc(i,k,13)
          css(i,k1) = csour(i,k1)   - csour(i,k)
          dtc(i,k1) = tc(i,k1)      - tc(i,k)
          ss2(i,k1) = ss1(i,k1)     - ss1(i,k)
        enddo
      enddo
!
!
!---the followimg is a drastic rewrite of the radiation code to
!    (largely) eliminate three-dimensional arrays. the code works
!    on the following principles:
!
!          let k = fixed flux level, kp = varying flux level
!          then flux(k)=sum over kp : (deltab(kp)*tau(kp,k))
!               over all kp, from 1 to lp1.
!
!          we can break down the calculations for all k as follows:
!
!          for all k k=1 to lp1:
!              flux(k)=sum over kp : (deltab(kp)*tau(kp,k))  (1)
!                      over all kp, from k+1 to lp1
!          and
!              for kp from k+1 to lp1:
!                 flux(kp) = deltab(k)*tau(k,kp)              (2)
!
!          now if tau(k,kp)=tau(kp,k) (symmetrical arrays)
!          we can compute a 1-dimensional array tau1d(kp) from
!          k+1 to lp1, each time k is incremented.
!          equations (1) and (2) then become:
!
!             tau1d(kp) = (values for tau(kp,k) at the particular k)
!             flux(k) = sum over kp : (deltab(kp)*tau1d(kp))   (3)
!             flux(kp) = deltab(k)*tau1d(kp)                   (4)
!
!         the terms for tau (k,k) and other special terms (for
!         nearby layers) must, of course, be handled separately, and
!         with care.
!
!      compute "upper triangle" transmission functions for
!      the 9.6 um band (to3sp) and the 15 um band (over1d). also,
!      the
!      stage 1...compute o3 ,over transmission fctns and avephi
!---do k=1 calculation (from flux layer kk to the top) separately
!   as vectorization is improved,and ozone cts transmissivity
!   may be extracted here.
!
      do k=1,l
        do i=1,imax
          avephi(i,k) = totphi(i,k+1)
        enddo
      enddo
!
!---in order to properly evaluate emiss integrated over the (lp1)
!   layer, a special evaluation of emiss is done. this requires
!   a special computation of avephi, and it is stored in the
!   (otherwise vacant) lp1th position
!
      do i=1,imax
        avephi(i,lp1) = avephi(i,lm1) + emx1(i)
      enddo
!
!   compute fluxes for k=1
!
      call e1e290(e1cts1,e1cts2,e1flx,e1ctw1,e1ctw2,emiss,              &
     &            fxo,dt,fxoe2,dte2,avephi                              &
     &,           l, lp1, imax, em1v, em1vw, t1, t2, t4)
!
      do k=1,l
        k1 = k + 1
        do i=1,imax
          fac1        = bo3rnd(2)*tphio3(i,k1)/toto3(i,k1)
          to3spc(i,k) = haf*(fac1*                                      &
     &        (sqrt(one+(four*ao3rnd(2)*toto3(i,k1))/fac1)-one))
!
!   for k=1, to3sp is used instead of to31d (they are equal in this
!   case); to3sp is passed to spa90, while to31d is a work-array.
!
          to3sp(i,k)  = exp(hm1ez*(to3spc(i,k)+sko3r*totvo2(i,k1)))
          over1d(i,k) = exp(hm1ez*(sqrt(ab15wd*totphi(i,k1))+           &
     &                                         skc1r*totvo2(i,k1)))
!
!---because all continuum transmissivities are obtained from the
!  2-d quantity cnttau (and its reciprocal totevv) we store both
!  of these here. for k=1, cont1d equals cnttau
!
          cnttau(i,k) = exp(hm1ez*totvo2(i,k1))
          totevv(i,k) = 1. / max(cnttau(i,k),1.0e-25)
!         totevv(i,k) = 1. / cnttau(i,k) ! commenteed by moorthi 03/08/2000
        enddo
      enddo
      do k=1,l
        do i=1,imax
          co2sp(i,k+1) = over1d(i,k)*co21(i,1,k+1)
        enddo
      enddo
      do k=1,l
        k1 = k + 1
        do i=1,imax
          co21(i,k1,1) = co21(i,k1,1)*over1d(i,k)
        enddo
      enddo
!---rlog is the nbl amount for the 15 um band calculation
      do i=1,imax
        rlog(i,1) = over1d(i,1)*co2nbl(i,1)
      enddo
!---the terms when kp=1 for all k are the photon exchange with
!   the top of the atmosphere, and are obtained differently than
!   the other calculations
      do k=2,lp1
        do i=1,imax
          tem  = tc(i,1)*e1flx(i,k)        + ss1(i,1)*cnttau(i,k-1)     &
     &         + sorc(i,1,13)*to3sp(i,k-1) + csour(i,1)*co2sp(i,k)
          flx(i,k)  = tem * cldfac(i,1,k)
        enddo
      enddo

      do i=1,imax
        flx(i,1)  = tc(i,1)*e1flx(i,1) + ss1(i,1) + sorc(i,1,13)        &
     &            + csour(i,1)
      enddo
!---the kp terms for k=1...
      do kp=2,lp1
        do i=1,imax
          tem      = oss(i,kp)*to3sp(i,kp-1) + ss2(i,kp)*cnttau(i,kp-1) &
     &             + css(i,kp)*co21(i,kp,1)  + dtc(i,kp)*emiss(i,kp-1)
          flx(i,1)  = flx(i,1)  + tem * cldfac(i,kp,1)
        enddo
      enddo
!
!          subroutine spa88 is called to obtain exact cts for water
!     co2 and o3, and approximate cts co2 and o3 calculations.
!
      call spa88(excts,ctso3,gxcts,sorc,csour,                          &
     &           cldfac,temp,press,var1,var2,                           &
     &           p,delp,delp2,totvo2,to3sp,to3spc,                      &
     &           co2sp1,co2sp2,co2sp                                    &
     &,          l, lp1, imax)
!
!    this section computes the emissivity cts heating rates for 2
!    emissivity bands: the 0-160,1200-2200 cm-1 band and the 800-
!    990,1070-1200 cm-1 band. the remaining cts comtributions are
!    contained in ctso3, computed in spa88.
!
      do i=1,imax
        vtmp3(i,1) = 1.
      enddo
      do k=1,l
        do i=1,imax
          vtmp3(i,k+1) = cnttau(i,k)*cldfac(i,k+1,1)
        enddo
      enddo
      do k=1,l
        do i=1,imax
          cts(i,k) = tc(i,k) * (e1ctw2(i,k)*cldfac(i,k+1,1)             &
     &                        - e1ctw1(i,k)*cldfac(i,k,1)) +            &
     &                          ss1(i,k)*(vtmp3(i,k+1)-vtmp3(i,k))
!
        enddo
      enddo
!
      do k=1,l
        do i=1,imax
          vtmp3(i,k)=tc(i,k)*(cldfac(i,k,1)*(e1cts1(i,k)-e1ctw1(i,k)) - &
     &                        cldfac(i,k+1,1)*(e1cts2(i,k)-e1ctw2(i,k)))
!
        enddo
      enddo
      do i=1,imax
        tem = tc(i,lp1) * (e1cts1(i,lp1)-e1ctw1(i,lp1))
        flx1e1(i) = tem * cldfac(i,lp1,1)
      enddo
      do k=1,l
        do i=1,imax
          flx1e1(i) = flx1e1(i) + vtmp3(i,k)
        enddo
      enddo
!
!---now repeat flux calculations for the k=2..lm1  cases.
!   calculations for flux level l and lp1 are done separately, as all
!   emissivity and co2 calculations are special cases or nearby layers.
!
      do k=2,lm1
        klen = k
        do kk=1,lp1-k
          do i=1,imax
            avephi(i,kk+k-1) = totphi(i,kk+k) - totphi(i,k)
          enddo
        enddo
        do i=1,imax
          avephi(i,lp1) = avephi(i,lm1) + emx1(i)
        enddo
!
!---compute emissivity fluxes (e2) for this case. note that
!   we have omitted the nearby later case (emiss(i,k,k)) as well
!   as all cases with k=l or lp1. but these cases have always
!   been handled as special cases, so we may as well compute
!    their fluxes separastely.
!
        call e290(emissb,emiss,avephi,klen,fxoe2,dte2                   &
     &,                 l, lp1, imax, t1, t2, t4)
!
        do kk=1,lp1-k
          kkk = kk + k
          kk1 = kkk - 1
          do i=1,imax
            avmo3(i,kk1)  = toto3(i,kkk)  - toto3(i,k)
            avpho3(i,kk1) = tphio3(i,kkk) - tphio3(i,k)
            avvo2(i,kk1)  = totvo2(i,kkk) - totvo2(i,k)
            cont1d(i,kk1) = cnttau(i,kk1) * totevv(i,k-1)
          enddo
        enddo
!
!dir$ ivdep
        do kk=1,lp1-k
          kkk = kk  + k
          kk1 = kkk - 1
          do i=1,imax
            fac1 = bo3rnd(2)*avpho3(i,kk1)/avmo3(i,kk1)
            vtmp = haf*(fac1*(sqrt(one+(four*ao3rnd(2)*avmo3(i,kk1))    &
     &                                  /fac1)-one))
            to31d(i,kk1)  = exp(hm1ez*(vtmp+sko3r*avvo2(i,kk1)))
            over1d(i,kk1) = exp(hm1ez*(sqrt(ab15wd*avephi(i,kk1))+      &
     &                                 skc1r*avvo2(i,kk1)))
            co21(i,kkk,k) = over1d(i,kk1)*co21(i,kkk,k)
          enddo
        enddo
        do kp=k+1,lp1
          do i=1,imax
            co21(i,k,kp) = over1d(i,kp-1)*co21(i,k,kp)
          enddo
        enddo
!---rlog is the nbl amount for the 15 um band calculation
        do i=1,imax
          rlog(i,k) = over1d(i,k)*co2nbl(i,k)
        enddo
!---the kp terms for arbirrary k..
        do kp=k+1,lp1
          do i=1,imax
            tem       = oss(i,kp)*to31d(i,kp-1)+ss2(i,kp)*cont1d(i,kp-1)&
     &                + css(i,kp)*co21(i,kp,k) +dtc(i,kp)*emiss(i,kp-1)
            flx(i,k)  = flx(i,k)  + tem * cldfac(i,kp,k)
          enddo
        enddo
        do kp=k+1,lp1
          do i=1,imax
            tem        = oss(i,k)*to31d(i,kp-1)+ss2(i,k)*cont1d(i,kp-1) &
     &                 + css(i,k)*co21(i,k,kp) +dtc(i,k)*emissb(i,kp-1)
            flx(i,kp)  = flx(i,kp)  + tem * cldfac(i,k,kp)
          enddo
        enddo
      enddo
!
!   now do k=l case. since the kp loop is length 1, many simplifi-
!   cations occur. also, the co2 quantities (as well as the emiss
!  quantities) are computed in the nbl sedction; therefore, we want
!  only over,to3 and cont1d (over(i,l),to31d(i,l) and cont1d(i,l)
!  according to the notation. thus no call is made to the e290
!  subroutine.
!         the third section calculates boundary layer and nearby layer
!     corrections to the transmission functions obtained above. methods
!     are given in ref. (4).
!          the following ratios are used in various nbl calculations:
!
!   the remaining calculations are for :
!                        1) the (k,k) terms, k=2,lm1;
!                        2) the (l,l) term
!                        3) the (l,lp1) term
!                        4) the (lp1,l) term
!                        5) the (lp1,lp1) term.
!     each is uniquely handled; different flux terms are computed
!     differently
!
!
!          fourth section obtains water transmission functions
!     used in q(approx) calculations and also makes nbl corrections:
!     1) emiss (i,j) is the transmission function matrix obtained
!     by calling subroutine e1e288;
!     2) "nearby layer" corrections (emiss(i,i)) are obtained
!     using subroutine e3v88;
!     3) special values at the surface (emiss(l,lp1),emiss(lp1,l),
!     emiss(lp1,lp1)) are calculated.
!
!
!      obtain arguments for e1e288 and e3v88:
!
      do i=1,imax
        tpl(i,1)    = temp(i,l)
        tpl(i,lp1)  = haf*(t(i,lp1) + temp(i,l))
        tpl(i,llp1) = haf*(t(i,l)   + temp(i,l))
!
!---e2 functions are required in the nbl calculations for 2 cases,
!   denoted (in old code) as (l,lp1) and (lp1,lp1)
        avephi(i,1) = var2(i,l)
        avephi(i,2) = var2(i,l) + empl(i,l)
      enddo
      do k=2,l
        do i=1,imax
          tpl(i,k)   = t(i,k)
          tpl(i,k+l) = t(i,k)
        enddo
      enddo
!
!     inlining of e2spec
!
!     subroutine e2spec computes the exchange terms in the flux equation
!  for longwave radiation for 2 terms used for nearby layer compu-
!  tations. the method is a table lookup on a pre-
!  computed e2 function (defined in ref. (4)).
!
      do  k=1,2
        do  i=1,imax
          tmp3       = log10(avephi(i,k)) + h16e1
          fyo        = aint(tmp3*ten)
          du         = tmp3 - hp1*fyo
          ival       = h28e1*fyo + fxosp(i,k)
          emiss(i,k) = t1(ival)  + du*t2(ival) + dtsp(i,k)*t4(ival)
        enddo
      enddo
!
!     call e3v88 for nbl h2o transmissivities
!     subroutine e3v88 computes nearby layer transmissivities for
!  h2o using a table lookup of the pre-computed e3 function
! ( described in ref. (4)).
!
!     inlining of e3v88
!
      do k=1,llp1
        do i=1,imax
          fxo3 = aint(tpl(i,k)*hp1)
          tmp3 = log10(empl(i,k)) + h16e1
          dt3  = tpl(i,k) - ten*fxo3
          fyo  = aint(tmp3*ten)
          du   = tmp3 - hp1*fyo
!---obtain index for table lookup; this value will have to be
!   decremented by 9 to account for table temps starting at 100k.
          it   = fxo3 + fyo*h28e1
          ww1  = ten - dt3
          ww2  = hp1 - du
          tpl(i,k) = ww2 * (ww1*em3v(it-9)  + dt3*em3v(it-8))+          &
     &               du  * (ww1*em3v(it+19) + dt3*em3v(it+20))
        enddo
      enddo
!
!   compute nearby layer and special-case transmissivities for emiss
!    using methods for h2o given in ref. (4)
!dir$ ivdep
      do k=2,l
        do i=1,imax
          emisdg(i,k) = tpl(i,k+l) + tpl(i,k)
        enddo
      enddo
!
!   note that emx1/2 (pressure scaled paths) are now computed in
!   lwr88
      do i=1,imax
        emspec(i,1) = (tpl(i,1)*empl(i,1)-tpl(i,lp1)*empl(i,lp1))/      &
     &                 emx1(i) + quartr*(emiss(i,1)+emiss(i,2))
        emisdg(i,lp1)=two*tpl(i,lp1)
        emspec(i,2) = two*(tpl(i,1)*empl(i,1)-tpl(i,llp1)*empl(i,llp1))/&
     &                emx2(i)
      enddo
      do i=1,imax
        fac1 = bo3rnd(2)*var4(i,l)/var3(i,l)
        vtmp = haf*(fac1*(sqrt(one+(four*ao3rnd(2)*var3(i,l))/fac1)     &
     &                                                        -one))
        to31d(i,l)  = exp(hm1ez*(vtmp+sko3r*cntval(i,l)))
        over1d(i,l) = exp(hm1ez*(sqrt(ab15wd*var2(i,l))+                &
     &                           skc1r*cntval(i,l)))
        cont1d(i,l) = cnttau(i,l)*totevv(i,lm1)
        rlog(i,l)   = over1d(i,l)*co2nbl(i,l)
      enddo
      do k=1,l
        k1 = k + 1
        do i=1,imax
          rlog(i,k)    = log(rlog(i,k))
          delpr2(i,k1) = delp(i,k)*(p(i,k1)-press(i,k))
          tpl(i,k)     = -sqrt(delpr2(i,k1))*rlog(i,k)
        enddo
      enddo
      do k=1,lm1
        k1 = k + 1
        do i=1,imax
          delpr1(i,k1) = delp(i,k1)*(press(i,k1)-p(i,k1))
          tpl(i,k+l)   = -sqrt(delpr1(i,k1))*rlog(i,k1)
        enddo
      enddo
      do i=1,imax
        tpl(i,ll)   = -rlog(i,l)
        tpl(i,llp1) = -rlog(i,l)*sqrt(delp(i,l)*(p(i,lp1)-press(i,lm1)))
      enddo
!        the first computation is for the 15 um band,with the
!     for the combined h2o and co2 transmission function.
!
!       perform nbl computations for the 15 um band
!***the statement function sf in prev. versions is now explicitly
!   evaluated.
      do k=1,llp1
        do i=1,imax
          c(i,k)=tpl(i,k)*(hmp66667+tpl(i,k)*(quartr+tpl(i,k)*hm6666m2))
        enddo
      enddo
      do i=1,imax
        co21(i,lp1,lp1) = one+c(i,l)
        co21(i,lp1,l)   = one+(delp2(i,l)*c(i,ll)-(press(i,l)-p(i,l))*  &
     &                    c(i,llm1))/(p(i,lp1)-press(i,l))
        co21(i,l,lp1)   = one+((p(i,lp1)-press(i,lm1))*c(i,llp1)-       &
     &          (p(i,lp1)-press(i,l))*c(i,l))/(press(i,l)-press(i,lm1))
      enddo
      do k=2,l
        do i=1,imax
          co21(i,k,k) = one + haf*(c(i,lm1+k)+c(i,k-1))
        enddo
      enddo
!
!    compute nearby-layer transmissivities for the o3 band and for the
!    one-band continuum band (to3 and emiss2). the sf2 function is
!    used. the method is the same as described for co2 in ref (4).
!dir$ ivdep
      do k=1,lm1
        k1 = k + 1
        do i=1,imax
          tpl(i,k1)  = cntval(i,k1) * delpr1(i,k1)
          tpl(i,k+l) = cntval(i,k)  * delpr2(i,k1)
        enddo
      enddo
!---the sf2 function in prev. versions is now explicitly evaluated
      do k=1,llm2
        do i=1,imax
          tem       = tpl(i,k+1)
          csub2     = sko3r*tem
          c(i,k+1)  = tem  *(hmp5+tem  *(hp166666-tem*h41666m2))
          c2(i,k+1) = csub2*(hmp5+csub2*(hp166666-csub2*h41666m2))
        enddo
      enddo
      do i=1,imax
        contdg(i,lp1) = 1. + c(i,llm1)
        to3dg(i,lp1)  = 1. + c2(i,llm1)
      enddo
      do k=2,l
        do i=1,imax
          contdg(i,k) = one + haf * (c(i,k)  + c(i,lm1+k))
          to3dg(i,k)  = one + haf * (c2(i,k) + c2(i,lm1+k))
        enddo
      enddo
!
!---now obtain fluxes
!
!    for the diagonal terms...
      do k=2,lp1
        do i=1,imax
          tem       = dtc(i,k)*emisdg(i,k) + ss2(i,k)*contdg(i,k)       &
     &              + oss(i,k)*to3dg(i,k)  + css(i,k)*co21(i,k,k)
          flx(i,k)  = flx(i,k)  + tem * cldfac(i,k,k)
        enddo
      enddo
!     for the two off-diagonal terms...
      do i=1,imax
        tem         = css(i,lp1)*co21(i,lp1,l) + dtc(i,lp1)*emspec(i,2) &
     &              + oss(i,lp1)*to31d(i,l)    + ss2(i,lp1)*cont1d(i,l)
        flx(i,l)    = flx(i,l)  + tem  * cldfac(i,lp1,l)
!
        tem         = css(i,l)*co21(i,l,lp1) + oss(i,l)*to31d(i,l)      &
     &              + ss2(i,l)*cont1d(i,l)   + dtc(i,l)*emspec(i,1)
        flx(i,lp1)  = flx(i,lp1)  + tem * cldfac(i,l,lp1)
      enddo
!
!     final section obtains emissivity heating rates,
!     total heating rates and the flux at the ground
!
      do k=1,l
        do i=1,imax
!     .....calculate the total heating rates
          tem         = radcon * delp(i,k)
          vsum1(i,k)  = flx(i,k+1) - flx(i,k) - cts(i,k)                &
     &                - ctso3(i,k) + excts(i,k)
          heatra(i,k) = tem * vsum1(i,k)
!
!        print *,' heatra=',heatra(i,k),' flx=', flx(i,k+1),flx(i,k)    &
!    &,' cts=',cts(i,k),' ctso3=',ctso3(i,k)                            &
!    &,' excts=',excts(i,k),' k=',k,' tem=',radcon*delp(i,k)
!
!
        enddo
      enddo
!
!     .....calculate the flux at each flux level using the flux at the
!    top (flx1e1+gxcts) and the integral of the heating rates (vsum1)
      do i=1,imax
        topflx(i)   = flx1e1(i) + gxcts(i)
        flxnet(i,1) = topflx(i)
      enddo
!---only the surface value of flux (grnflx) is needed unless
!    the thick cloud section is invoked.
!
      do k=1,l
        do i=1,imax
          flxnet(i,k+1) = flxnet(i,k) + vsum1(i,k)
        enddo
      enddo
      do i=1,imax
        grnflx(i) = flxnet(i,lp1)
      enddo
!
!  ***************************************************************
!  *   thick cloud section no longer used ....k.a.c. sep96       *
!  ***************************************************************
!     this is the thick cloud section.optionally,if thick cloud
!     fluxes are to be "convectively adjusted",ie,df/dp is constant,
!     for cloudy part of grid point, the following code is executed.
!***first,count the number of clouds along the lat. row. skip the
!   entire thick cloud computation if there are no clouds.
!kc      icnt=0
!kc      do 1301 i=1,imax
!kc      icnt=icnt+nclds(i)
!kc1301  continue
!kc      if (icnt.eq.0) go to 6999
!---find the maximum number of clouds in the latitude row
!kc      kclds=nclds(1)
!kc      do 2106 i=2,imax
!kc      kclds=max(nclds(i),kclds)
!kc2106  continue
!
!***obtain the pressures and fluxes of the top and bottom of
!   the nc'th cloud (it is assumed that all ktop and kbtm's have
!   been defined!).
!kc      do 1361 kk=1,kclds
!kc      kmin=lp1
!kc      kmax=0
!kc      do 1362 i=1,imax
!kc        j1=ktop(i,kk+1)
!       if (j1.eq.1) go to 1362
!kc        j3=kbtm(i,kk+1)
!kc        if (j3.gt.j1) then
!kc          ptop(i)=p(i,j1)
!kc          pbot(i)=p(i,j3+1)
!kc          ftop(i)=flxnet(i,j1)
!kc          fbot(i)=flxnet(i,j3+1)
!***obtain the "flux derivative" df/dp (delptc)
!kc          delptc(i)=(ftop(i)-fbot(i))/(ptop(i)-pbot(i))
!kc          kmin=min(kmin,j1)
!kc          kmax=max(kmax,j3)
!kc        endif
!kc1362  continue
!kc      kmin=kmin+1
!***calculate the tot. flux chg. from the top of the cloud, for
!   all levels.
!kc      do 1365 k=kmin,kmax
!kc      do 1363 i=1,imax
!       if (ktop(i,kk+1).eq.1) go to 1363
!kc        if(ktop(i,kk+1).lt.k .and. k.le.kbtm(i,kk+1)) then
!kc          z1(i,k)=(p(i,k)-ptop(i))*delptc(i)+ftop(i)
!original flxnet(i,k)=flxnet(i,k)*(one-camt(i,kk+1)) +
!original1            z1(i,k)*camt(i,kk+1)
!kc          flxnet(i,k)=z1(i,k)
!kc        endif
!kc1363  continue
!kc1365  continue
!kc1361  continue
!***using this flux chg. in the cloudy part of the grid box, obtain
!   the new fluxes, weighting the clear and cloudy fluxes:again, only
!    the fluxes in thick-cloud levels will eventually be used.
!     do 6051 k=1,lp1
!     do 6051 i=1,imax
!     flxnet(i,k)=flxnet(i,k)*(one-camt(i,nc)) +
!    1            z1(i,k)*camt(i,nc)
!051  continue
!***merge flxthk into flxnet for appropriate levels.
!     do 1401 k=1,lp1
!     do 1401 i=1,imax
!     if (k.gt.itop(i) .and. k.le.ibot(i)
!    1  .and.  (nc-1).le.nclds(i))  then
!          flxnet(i,k)=flxthk(i,k)
!     endif
!401  continue
!
!******end of cloud loop*****
!kc6001  continue
!kc6999  continue
!***the final step is to recompute the heating rates based on the
!   revised fluxes:
!kc      do 6101 k=1,l
!kc      do 6101 i=1,imax
!kc      heatra(i,k)=radcon*(flxnet(i,k+1)-flxnet(i,k))*delp(i,k)
!kc6101  continue
!     the thick cloud section ends here.
!  ***************************************************************
!  *   thick cloud section no longer used ....k.a.c. sep96       *
!  ***************************************************************
      return
      end
