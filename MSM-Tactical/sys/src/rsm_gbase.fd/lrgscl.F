      subroutine lrgscl(im,ix,km,dt,t1,q1,prsl,del,prslk,rain,clw)
!
      use machine     , only : kind_phys
      use funcphys , only : fpvs, ftdp, fthe, stma, ftlcl
      use physcons, hvap => con_hvap, cp => con_cp, rv => con_rv        &
     &,             eps => con_eps, epsm1 => con_epsm1, rocp => con_rocp &
     &,             grav => con_g
      implicit none
!
      integer ix , im, km
      real(kind=kind_phys) t1(ix,km),  q1(ix,km),    prsl(ix,km),       &
     &                     del(ix,km), prslk(ix,km), rain(ix),          &
     &                     clw(ix,km), dt
!
!  local variables
!
      integer              k, kmax, i
      real(kind=kind_phys) dpovg,  ei,    el2orc,                       &
     &                     elocp,                                       &
     &                     pk,     qcond, qevap,                        &
     &                     rnevap, slklcl,tdpd,                         &
     &                     thelcl, tlcl,  val0,  val1
!
!
!  physical parameters
      parameter(elocp=hvap/cp,   el2orc=hvap*hvap/(rv*cp))
!
!
      real(kind=kind_phys) to(ix,km),   qo(ix,km),   qs(ix,km),         &
     &                     the(ix,km),  dq(ix,km),   rainlvl(ix,km),    &
     &                     es(ix,km),   dqint(ix),   pint(ix),          &
     &                     delqbar(ix), deltbar(ix), thebar(ix),        &
     &                     theint(ix),  p(ix,km)
      integer              kmlev(ix,km), ke(ix), kk(ix), ks(ix)
      logical flg(ix), topflg(ix), totflg
!
      kmax = km
      do k = 1, km
        do i=1,im
          if (prsl(i,k) .gt. 60.0) kmax = k + 1
        enddo
      enddo
!
!   surface pressure unit is cb
!
      do i=1,im
!       psk(i)     = fpkap(ps(i))
        rain(i)    = 0.
        deltbar(i) = 0.
        delqbar(i) = 0.
        flg(i)     = .false.
        topflg(i)  = .false.
        ke(i)      = kmax + 1
        ks(i)      = 0
      enddo
      totflg = .false.
!
!  column variables
!  prsl is pressure of the layer (cb)
!  to is temperature at t+dt (k)... this is after advection and turbulan
!  qo is mixing ratio at t+dt (kg/kg)..q1
!
      do k = 1, kmax
       do i=1,im
!       pfld(i,k) = ps(i) * sl(k)
        p(i,k)  = 1000.0 * prsl(i,k)
        to(i,k) = t1(i,k)
        qo(i,k) = q1(i,k)
        enddo
      enddo
!
!  model consistent saturation mixing ratio
!
!     es(:,:) = 0.001 * fpvs(t1(1:im,:))        ! fpvs in pa
      do k = 1, kmax
        do i=1,im
          es(i,k) = min(p(i,k), fpvs(t1(i,k)))
          qs(i,k) = eps * es(i,k) / (p(i,k) + epsm1*es(i,k))
          qs(i,k) = max(qs(i,k),1.e-8)
        enddo
      enddo
      do k = 1, kmax
        do i=1,im
          if(qo(i,k).gt.qs(i,k)) flg(i) = .true.
        enddo
      enddo
!!
      do i=1,im
        if(flg(i)) totflg = .true.
      enddo
      if(.not.totflg) return
!!
      do k = 1, kmax
        do i = 1, im
          dq(i,k) = 0.
          the(i,k) = to(i,k)
        enddo
      enddo
!
!  compute theta-e
!
      do k = 1, kmax
        do i = 1, im
          if(flg(i)) then
!           pk = psk(i) * slk(k)
            pk = prslk(i,k)
            the(i,k) = fthe(to(i,k),pk)
            if(the(i,k).eq.0.) then
              the(i,k) = to(i,k) / pk
            endif
!         the(i,k) = to(i,k) * ((prsl(i,k)-es(i,k))*.01) ** (-rocp)      &
!    &             * exp(elocp * qs(i,k) / to(i,k))
            dq(i,k) = qo(i,k)- qs(i,k)
!
!  modification of theta-e for super-saturation
!
            the(i,k)= the(i,k) * (1. + hvap*max(dq(i,k),0.)             &
     &              /(cp*to(i,k)))
          endif
        enddo
      enddo
      do k = 1, kmax
        do i = 1, im
          kmlev(i,k) = 0
          rainlvl(i,k) = 0.
        enddo
      enddo
!
!  starting point of adjustment
!
      k = 1
      do i = 1, im
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
          kk(i)      = kk(i)     + 1
          kmlev(i,k) = kk(i)
        endif
      enddo
      do k = 2, kmax - 1
        do i = 1, im
          if(dq(i,k).gt.0..and.the(i,k).ge.the(i,k+1).and.flg(i)) then
            dqint(i)   = dqint(i)  + dq(i,k) * del(i,k)
            theint(i)  = theint(i) + the(i,k) * del(i,k)
            pint(i)    = pint(i)   + del(i,k)
            kk(i)      = kk(i)     + 1
            kmlev(i,k) = kk(i)
          endif
        enddo
        do i = 1, im
          if(pint(i).gt.0.)thebar(i) = theint(i) / pint(i)
!
!  if the layer below satisfies the condition and the present
!    layer is colder than the adjsuted theta-e,
!    the layer is included if the integrated moisture excess
!    can be maintained
!
          if(kmlev(i,k).eq.0.and.kmlev(i,k-1).gt.0.and.                 &
     &       thebar(i).ge.the(i,k).and..not.topflg(i)) then
               dqint(i) = dqint(i) + dq(i,k) * del(i,k)
          endif
          if(kmlev(i,k).eq.0.and.kmlev(i,k-1).gt.0.and.                 &
     &       thebar(i).ge.the(i,k).and.dqint(i).gt.0.                   &
     &       .and..not.topflg(i)) then
            kk(i)      = kk(i) + 1
            kmlev(i,k) = kk(i)
            topflg(i) = .true.
!           pk        = psk(i) * slk(k)
            ei        = p(i,k) * qo(i,k) / (eps - epsm1*qo(i,k))
            ei        = min(max(ei,1.e-8),es(i,k))
            tdpd      = max(to(i,k)-ftdp(ei),0.)
            tlcl      = ftlcl(to(i,k), tdpd)
            slklcl    = prslk(i,k) * tlcl / to(i,k)
            thelcl    = fthe(tlcl,slklcl)
            if(thelcl.ne.0.) then
              the(i,k) = thelcl
!             the(i,k) = to(i,k) * ((prsl(i,k) - ei)*.01) ** (-rocp)    &
!    &             * exp(elocp * max(qo(i,k),1.e-8) / to(i,k))
            endif
            theint(i) = theint(i) + the(i,k) * del(i,k)
            pint(i)   = pint(i) + del(i,k)
          endif
        enddo
!
!  reset the integral if the layer is not in the cloud
!
        do i = 1, im
          if(kmlev(i,k).eq.0.and.kmlev(i,k-1).gt.0) then
            thebar(i) = theint(i) / pint(i)
            dqint(i) = 0.
            theint(i) = 0.
            pint(i) = 0.
            kk(i) = 0
            ks(i) = k - 1
            ke(i) = ks(i) - kmlev(i,k-1) + 1
            flg(i) = .false.
          endif
        enddo
      enddo
!
!  when within a cloud layer, compute the moist-adiabatic
!    (to and qo) using the averaged theta-e and the resultant rain
!
      do k = 1, kmax
        do i = 1, im
          if(k.ge.ke(i).and.k.le.ks(i)) then
!           pk = psk(i) * slk(k)
            pk = prslk(i,k)
!           to(i,k)  = ftma(thebar(i),pk,qo(i,k))
            call stma(thebar(i),pk,to(i,k),qo(i,k))
            the(i,k) = thebar(i)
            qs(i,k)  = qo(i,k)
            dpovg    = del(i,k) * (1.0/grav)
            rainlvl(i,k) = (q1(i,k) - qo(i,k)) * dpovg
            deltbar(i)   = deltbar(i) + (to(i,k) - t1(i,k)) * dpovg / pk
            delqbar(i)   = delqbar(i) + (qo(i,k) - q1(i,k)) * dpovg
          endif
!
!  this step takes care of stable heating
!
          if(kmlev(i,k).eq.0.and.dq(i,k).gt.0.) then
            qcond   = (qo(i,k)-qs(i,k)) /                               &
     &                (1.+el2orc*qs(i,k)/(to(i,k)*to(i,k)))
            qo(i,k) = qo(i,k) - qcond
            to(i,k) = to(i,k) + qcond * elocp
!           pk = psk(i) * slk(k)
            pk = prslk(i,k)
!           to(i,k) = ftma(the(i,k),pk,qo(i,k))
            dpovg    = del(i,k) * (1.0/grav)
            rainlvl(i,k) = (q1(i,k) - qo(i,k)) * dpovg
            deltbar(i)   = deltbar(i) + (to(i,k) - t1(i,k)) * dpovg / pk
            delqbar(i)   = delqbar(i) + (qo(i,k) - q1(i,k)) * dpovg
            qs(i,k) = qo(i,k)
          endif
        enddo
      enddo
!
!  evaporation of falling rain
!
      do k = kmax, 1, -1
        do i = 1, im
          t1(i,k) = to(i,k)
          q1(i,k) = qo(i,k)
          dpovg   = del(i,k) * (1.0/grav)
          rain(i) = rain(i) + rainlvl(i,k) + clw(i,k) * dpovg
          dq(i,k) = (qo(i,k) - qs(i,k)) /                               &
     &              (1. + el2orc*qs(i,k)/(to(i,k)*to(i,k)))
          if(rain(i).gt.0..and.rainlvl(i,k).le.0.) then
            qevap      = -dq(i,k)*(1.-exp(-0.32*sqrt(dt*rain(i))))
            rnevap     = min(qevap*dpovg,rain(i))
            q1(i,k)    = q1(i,k)+rnevap/dpovg
            t1(i,k)    = t1(i,k)-rnevap/dpovg*elocp
            rain(i)    = rain(i)-rnevap
            deltbar(i) = deltbar(i) - rnevap * elocp
            delqbar(i) = delqbar(i) + rnevap
          endif
        enddo
      enddo
      do i = 1, im
        rain(i) = max(rain(i),0.)
      enddo
!!
      return
      end
