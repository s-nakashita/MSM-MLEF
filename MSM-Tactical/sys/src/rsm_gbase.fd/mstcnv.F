      subroutine mstcnv(im,ix,km,dt,t1,q1,prsl,del,prslk,rain            &               
     &,                                                 lprnt)
!!
      use machine , only : kind_phys
      use funcphys , only : fpvs, ftdp, fthe, stma, fpkap, ftlcl
      use physcons, hvap => con_hvap, cp => con_cp, rv => con_rv         &
     &,             eps => con_eps, epsm1 => con_epsm1, grav => con_g
      implicit none
!!
!  physical parameters
      real(kind=kind_phys) elocp, el2orc
      parameter(elocp=hvap/cp, el2orc=hvap*hvap/(rv*cp))
!
!
      logical              lprnt
      integer              im,ix,km
      real(kind=kind_phys) dt,rain(im)
!
      real(kind=kind_phys) prsl(ix,km), prslk(ix,km), del(ix,km)
      real(kind=kind_phys) t1(ix,km),   q1(ix,km)
!
!  local variables
!
      real(kind=kind_phys) p(im,km),   to(im,km), qo(im,km),   qs(im,km) &   
     &,                    the(im,km), dq(im,km), rainlvl(im,km)         &
     &,                    es(im,km)
      real(kind=kind_phys) pint(im), delqbar(im), deltbar(im), dqint(im) &
     &,                    ei(im),   thebar(im),  theint(im)             &
     &,                    qevap,    dpovg, rnevap, slklcl, tdpd         &
     &,                    thelcl,   tlcl,  pmax
      integer kmlev(im,km), k, kmax, kk(im), ks(im), ke(im), i
!
      logical flg(im), topflg(im), totflg
!
      kmax   = 0
      do k = 1, km
        pmax   = 0.0
        do i=1,im
          pmax = max(prsl(i,k), pmax)
        enddo
        if(pmax .gt. 60.0) kmax = k + 1
      enddo
      if (lprnt) print *,' kmax=',kmax
!
!seladg3 latd = 57
!seladg3 lond = 1
!seladg3 latd = 48
!seladg3 lond = 176
!
!   surface pressure unit is cb
!
      do i=1,im
        rain(i)    = 0.
        deltbar(i) = 0.
        delqbar(i) = 0.
        flg(i)     = .false.
        ks(i)      = 0
        ke(i)      = kmax + 1
        topflg(i)  = .false.
      enddo
      totflg = .false.
!
!seladg3 if(lat.eq.latd) then
!seladg3   print *, ' t and q before adjustment'
!seladg3   print 6000, (t1(k)-273.16,k=1,kmax)
!seladg3   print 6000, (q1(k)*1.e3,k=1,kmax)
!seladg3   print *, ' ps =', ps
!seladg3 endif
!
!
!  column variables
!  p is pressure of the layer (cb)
!  to is temperature at t+dt (k)... this is after advection and turbulan
!  qo is mixing ratio at t+dt (kg/kg)..q1
!
      do k = 1, kmax
        do i=1,im
          p(i,k)  = 1000.0 * prsl(i,k)
          to(i,k) = t1(i,k)
          qo(i,k) = q1(i,k)
        enddo
      enddo
!
!  model consistent saturation mixing ratio
!
      do k = 1, kmax
        do i=1,im
          es(i,k) = min(p(i,k), fpvs(t1(i,k)))
          qs(i,k) = eps * es(i,k) / (p(i,k) + epsm1 * es(i,k))
          qs(i,k) = max(qs(i,k),1.e-8)
          if(qo(i,k) .gt. qs(i,k)) flg(i) = .true.
        enddo
      enddo
      do i=1,im
        if (flg(i)) totflg = .true.
      enddo
      if (.not. totflg) return
!
      do k = 1, kmax
        do i=1,im
          dq(i,k)  = 0.
          the(i,k) = to(i,k)
        enddo
      enddo
!
!  compute theta-e
!
      do k = 1, kmax
        do i=1,im
          if(flg(i)) then
            the(i,k) = fthe(to(i,k),prslk(i,k))
            if (the(i,k) .eq. 0.) then
              the(i,k) = to(i,k) / prslk(i,k)
            endif
!           the(i,k) = to(i,k) * ((p(i,k) - es(i,k))*0.01) ** (-rocp)
!    &               * exp(elocp * qs(i,k) / to(i,k))
            dq(i,k)  = qo(i,k)- qs(i,k)
!
!  modification of theta-e for super-saturation
!
            the(i,k)= the(i,k) * (1. + hvap*max(dq(i,k),0.)              &   
     &                         / (cp*to(i,k)))
          endif
        enddo
      enddo
!
!seladg3 if(lat.eq.latd.and.flg(lond)) then
!seladg3   print *, ' theta-e, qs and dq before adjustment'
!seladg3   print 6000, (the(k)-273.16,k=1,kmax)
!seladg3   print 6000, (qs(k)*1.e3,k=1,kmax)
!seladg3   print 6000, (dq(k)*1.e3,k=1,kmax)
!seladg3 endif
!
      do k = 1, kmax
        do i=1,im
          kmlev(i,k)   = 0
          rainlvl(i,k) = 0.
        enddo
      enddo
!
!  starting point of adjustment
!
      k = 1
      do i=1,im
        kk(i)     = 0
        dqint(i)  = 0.
        theint(i) = 0.
        thebar(i) = 0.
        pint(i)   = 0.
!
!  for conditionally unstable and supersaturated layers,
!    obtain integrated theta and q-qs
!
!  kmlev keeps track of the number of layers that satisfies
!    the condition for adjustment
!
        if(dq(i,k).gt.0..and.the(i,k).ge.the(i,k+1).and.flg(i)) then
          dqint(i)   = dqint(i)  + dq(i,k) * del(i,k)
          theint(i)  = theint(i) + the(i,k) * del(i,k)
          pint(i)    = pint(i)   + del(i,k)
          kk(i)      = kk(i) + 1
          kmlev(i,k) = kk(i)
        endif
      enddo
      do k = 2, kmax - 1
        do i=1,im
          if(dq(i,k).gt.0..and.the(i,k).ge.the(i,k+1).and.flg(i)) then
            dqint(i)  = dqint(i)  + dq(i,k) * del(i,k)
            theint(i) = theint(i) + the(i,k) * del(i,k)
            pint(i)   = pint(i)   + del(i,k)
            kk(i)     = kk(i) + 1
            kmlev(i,k) = kk(i)
          endif
!      
          if (pint(i) .gt. 0.)thebar(i) = theint(i) / pint(i)
!
!  if the layer below satisfies the condition and the present
!    layer is colder than the adjsuted theta-e,
!    the layer is included if the integrated moisture excess
!    can be maintained
!
          if (kmlev(i,k) .eq.0 .and. kmlev(i,k-1) .gt. 0 .and.           &   
     &       thebar(i) .ge. the(i,k) .and. .not. topflg(i)) then
               dqint(i) = dqint(i) + dq(i,k) * del(i,k)
!         endif
!         if (kmlev(i,k) .eq. 0 .and. kmlev(i,k-1) .gt. 0 .and.
!    &        thebar(i)  .ge. the(i,k) .and. dqint(i) .gt. 0.
!    &       .and. .not. topflg(i)) then
            if (dqint(i) .gt. 0) then
              kk(i) = kk(i) + 1
              kmlev(i,k) = kk(i)
              topflg(i)  = .true.
              ei(i)   = p(i,k) * qo(i,k) / (eps - epsm1 * qo(i,k))
              ei(i)   = min(max(ei(i),1.e-8),es(i,k))
              tdpd    = max(to(i,k)-ftdp(ei(i)),0.)
              tlcl    = ftlcl(to(i,k), tdpd)
              slklcl  = prslk(i,k) * tlcl / to(i,k)
              thelcl  = fthe(tlcl,slklcl)
              if(thelcl.ne.0.) then
                the(i,k) = thelcl
!               the(i,k) = to(i,k) * ((p(i,k) - ei(i))*.01) ** (-rocp)
!    &               * exp(elocp * max(qo(i,k),1.e-8) / to(i,k))
              endif
              theint(i) = theint(i) + the(i,k) * del(i,k)
              pint(i)   = pint(i)   + del(i,k)
            endif
          endif
!
!  reset the integral if the layer is not in the cloud
!
          if (kmlev(i,k) .eq. 0 .and. kmlev(i,k-1) .gt. 0) then
            thebar(i) = theint(i) / pint(i)
            dqint(i)  = 0.
            theint(i) = 0.
            pint(i)   = 0.
            kk(i)     = 0
            ks(i)     = k - 1
            ke(i)     = ks(i) - kmlev(i,k-1) + 1
            flg(i)    = .false.
          endif
        enddo
      enddo
!
!  when within a cloud layer, compute the moist-adiabatic
!    (to and qo) using the averaged theta-e and the resultant rain
!
      do k = 1, kmax
        do i=1,im
          if (k .ge. ke(i) .and. k .le. ks(i)) then
            call stma(thebar(i),prslk(i,k),to(i,k),qo(i,k))
            the(i,k) = thebar(i)
            qs(i,k)  = qo(i,k)
!
            dpovg        = del(i,k) / grav
            rainlvl(i,k) = (q1(i,k) - qo(i,k)) * dpovg
            deltbar(i)   = deltbar(i) + (to(i,k) - t1(i,k)) * dpovg      & 
     &                                                      / prslk(i,k)
            delqbar(i)   = delqbar(i) - rainlvl(i,k)
      if (lprnt) print *,' k=',k,' to=',to(i,k),' qo=',qo(i,k),          &
     & ' rainlvl=',rainlvl(i,k)
          endif
        enddo
      enddo
!
!  evaporation of falling rain
!
      do k = kmax, 1, -1
        do i=1,im
          t1(i,k) = to(i,k)
          q1(i,k) = qo(i,k)
          rain(i) = rain(i) + rainlvl(i,k)
          dq(i,k) = (qo(i,k) - qs(i,k)) / (1. +                          &   
     &                el2orc*qs(i,k)/to(i,k)**2)
          dpovg = del(i,k) / grav
          if (rain(i) .gt. 0. .and. rainlvl(i,k) .le. 0.) then
            qevap   =-dq(i,k)*(1.-exp(-0.32*sqrt(dt*rain(i))))
            rnevap  = min(qevap*dpovg,rain(i))
            q1(i,k) = q1(i,k)+rnevap/dpovg
            t1(i,k) = t1(i,k)-rnevap/dpovg*elocp
            rain(i) = rain(i) - rnevap
            deltbar(i) = deltbar(i) - rnevap * elocp
            delqbar(i) = delqbar(i) + rnevap
          endif
        enddo
      enddo
!
!seladg3 if(lat.eq.latd.and.flg(lond)) then
!seladg3   print *, ' theta-e after adjustment'
!seladg3   print 6000, (the(k)-273.16,k=1,kmax)
!seladg3   print *, ' t and q after adjustment'
!seladg3   print 6000, (t1(k)-273.16,k=1,kmax)
!seladg3   print 6000, (q1(k)*1.e3,k=1,kmax)
!seladg3   print *, ' deltbar, delqbar =', deltbar*cp,delqbar*hvap
!seladg3   print *, ' rain =', hvap*rain
!seladg3 endif
!6000 format(2x,0p,11(f6.2,1h,))
!6100 format(2x,3p11f7.2)
!
      do i=1,im
        rain(i) = max(rain(i),0.)
      enddo
!
      return
      end
