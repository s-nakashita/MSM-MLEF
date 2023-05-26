!fpp$ noconcur r
      subroutine moninp(im,ix,km,ndiff,dv,du,tau,rtg,                    & 
     &     u1,v1,t1,q1,                                                  &
     &     psk,rbsoil,fm,fh,tsea,qss,heat,evap,stress,spd1,kpbl,         &
!    &     psk,rbsoil,cd,ch,fm,fh,tsea,qss,dphi,spd1,kpbl,               &
     &     prsi,del,prsl,prslk,phii,phil,rcl,deltim,                     &
     &     dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq)
!
      use machine     , only : kind_phys
      use physcons, grav => con_g, rd => con_rd, cp => con_cp            &
     &,             hvap => con_hvap, rog => con_rog, fv => con_fvirt
      implicit none
!
!     arguments
!
      integer ix, im, km, ndiff, kpbl(ix)
!
      real(kind=kind_phys) deltim
      real(kind=kind_phys) dv(ix,km),     du(ix,km),                     &
     &                     tau(ix,km),    rtg(ix,km,ndiff),              &
     &                     u1(ix,km),     v1(ix,km),                     &
     &                     t1(ix,km),     q1(ix,km,ndiff),               &
     &                     psk(ix),       rbsoil(ix),                    &
!    &                     cd(ix),        ch(ix),                        &
     &                     fm(ix),        fh(ix),                        &
     &                     tsea(ix),      qss(ix),                       &
     &                                    spd1(ix),                      &
!    &                     dphi(ix),      spd1(ix),                      &
     &                     prsi(ix,km+1), del(ix,km),                    &
     &                     prsl(ix,km),   prslk(ix,km),                  &
     &                     phii(ix,km+1), phil(ix,km),                   &
     &                     rcl(ix),       dusfc(ix),                     &
     &                     dvsfc(ix),     dtsfc(ix),                     &
     &                     dqsfc(ix),     hpbl(ix),                      &
     &                     hgamt(ix),     hgamq(ix)
!
!    locals
!
      integer i,iprt,is,iun,k,kk,kmpbl,lond
!     real(kind=kind_phys) betaq(im), betat(im),   betaw(im),
      real(kind=kind_phys) evap(im),  heat(im),    phih(im),             & 
     &                     phim(im),  rbdn(im),    rbup(im),             &
     &                     the1(im),  stress(im),  beta(im),             &
     &                     the1v(im), thekv(im),   thermal(im)      ,    &
     &                     thesv(im), ustar(im),   wscale(im)
!    &                     thesv(im), ustar(im),   wscale(im),  zl1(im)
!
      real(kind=kind_phys) rdzt(im,km-1),                                &
     &                     zi(im,km+1),     zl(im,km),                   &
     &                     dku(im,km-1),    dkt(im,km-1), dko(im,km-1),  &
     &                     al(im,km-1),     ad(im,km),                   &
     &                     au(im,km-1),     a1(im,km),                   &
     &                     a2(im,km),       theta(im,km),                &
     &                     at(im,km*(ndiff-1)) 
      logical              pblflg(im),   sfcflg(im), stable(im)
!
      real(kind=kind_phys) aphi16,  aphi5,  bet1,   bvf2,                &
     &                     cfac,    conq,   cont,   conw,                &
     &                     conwrc,  dk,     dkmax,  dkmin,               &
     &                     dq1,     dsdz2,  dsdzq,  dsdzt,               &
     &                     dsig,    dt,     dthe1,  dtodsd,              &
     &                     dtodsu,  dw2,    dw2min, g,                   &
     &                     gamcrq,  gamcrt, gocp,   gor, gravi,          &
     &                     hol,     pfac,   prmax,  prmin, prinv,        &
     &                     prnum,   qmin,   qtend,  rbcr,                &
     &                     rbint,   rdt,    rdz,    rdzt1,               &
     &                     ri,      rimin,  rl2,    rlam,                &
     &                     rone,   rzero,  sfcfrac,                      &
     &                     sflux,   shr2,   spdk2,  sri,                 &
     &                     tem,     ti,     ttend,  tvd,                 &
     &                     tvu,     utend,  vk,     vk2,                 &
     &                     vpert,   vtend,  xkzo,   zfac,                &
     &                     zfmin,   zk,     tem1
!c
      parameter(g=grav)
      parameter(gor=g/rd,gocp=g/cp)
      parameter(cont=1000.*cp/g,conq=1000.*hvap/g,conw=1000./g)
      parameter(rlam=150.,vk=0.4,vk2=vk*vk,prmin=1.0,prmax=4.)
      parameter(dw2min=0.0001,dkmin=1.0,dkmax=1000.,rimin=-100.)
      parameter(rbcr=0.5,cfac=7.8,pfac=2.0,sfcfrac=0.1)
      parameter(qmin=1.e-8,xkzo=1.0,zfmin=1.e-8,aphi5=5.,aphi16=16.)
!     parameter(gamcrt=3.,gamcrq=2.e-3)
      parameter(gamcrt=3.,gamcrq=0.)
      parameter(iun=84)
!
!
!-----------------------------------------------------------------------
!
 601  format(1x,' moninp lat lon step hour ',3i6,f6.1)
 602      format(1x,'    k','        z','        t','       th',             &
     &     '      tvh','        q','        u','        v',                  &
     &     '       sp')
 603      format(1x,i5,8f9.1)
 604      format(1x,'  sfc',9x,f9.1,18x,f9.1)
 605      format(1x,'    k      zl    spd2   thekv   the1v'                  &
     &         ,' thermal    rbup')
 606      format(1x,i5,6f8.2)
 607      format(1x,' kpbl    hpbl      fm      fh   hgamt',                 &
     &         '   hgamq      ws   ustar      cd      ch')
 608      format(1x,i5,9f8.2)
 609      format(1x,' k pr dkt dku ',i5,3f8.2)
 610      format(1x,' k pr dkt dku ',i5,3f8.2,' l2 ri t2',                   &
     &         ' sr2  ',2f8.2,2e10.2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
      if (ix .lt. im) stop
!
      iprt = 0
      if(iprt.eq.1) then
!cc   latd = 0
      lond = 0
      else
!cc   latd = 0
      lond = 0
      endif
!
      gravi = 1.0 / grav
      dt    = 2. * deltim
      rdt   = 1. / dt
      kmpbl = km / 2
!
      do k=1,km
        do i=1,im
          zi(i,k) = phii(i,k) * gravi
          zl(i,k) = phil(i,k) * gravi
        enddo
      enddo
!
      do k=1,kmpbl
        do i=1,im
          theta(i,k) = t1(i,k) * psk(i) / prslk(i,k)
        enddo
      enddo
!
      do k = 1,km-1
        do i=1,im
          rdzt(i,k) = gor * prsi(i,k+1) / (prsl(i,k) - prsl(i,k+1))
        enddo
      enddo
!
      do i = 1,im
         dusfc(i) = 0.
         dvsfc(i) = 0.
         dtsfc(i) = 0.
         dqsfc(i) = 0.
         hgamt(i) = 0.
         hgamq(i) = 0.
         wscale(i) = 0.
         kpbl(i) = 1
         hpbl(i) = zi(i,2)
         pblflg(i) = .true.
         sfcflg(i) = .true.
         if(rbsoil(i).gt.0.0) sfcflg(i) = .false.
      enddo
!!
      do i=1,im
         rdzt1    = gor * prsl(i,1) / del(i,1)
!        bet1     = dt*rdzt1*spd1(i)/t1(i,1)
         beta(i)  = dt*rdzt1/t1(i,1)
!        betaw(i) = bet1*cd(i)
!        betat(i) = bet1*ch(i)
!        betaq(i) = dphi(i)*betat(i)
      enddo
!
      do i=1,im
!        zl1(i) = 0.-(t1(i,1)+tsea(i))/2.*log(prsl(i,1)/prsi(i,1))*rog
!        ustar(i) = sqrt(cd(i)*spd1(i)**2)
         ustar(i) = sqrt(stress(i))
      enddo
!
      do i=1,im
         thesv(i)   = tsea(i)*(1.+fv*max(qss(i),qmin))
         the1(i)    = theta(i,1)
         the1v(i)   = the1(i)*(1.+fv*max(q1(i,1,1),qmin))
         thermal(i) = the1v(i)
!        dthe1      = (the1(i)-tsea(i))
!        dq1        = (max(q1(i,1,1),qmin) - max(qss(i),qmin))
!        heat(i)    = -ch(i)*spd1(i)*dthe1
!        evap(i)    = -ch(i)*spd1(i)*dq1
      enddo
!
!
!     compute the first guess of pbl height
!
      do i=1,im
         stable(i) = .false.
!        zl(i,1) = zl1(i)
         rbup(i) = rbsoil(i)
      enddo
      do k = 2, kmpbl
        do i = 1, im
          if(.not.stable(i)) then
             rbdn(i)   = rbup(i)
!            zl(i,k)   = zl(i,k-1) - (t1(i,k)+t1(i,k-1))/2 *                &
!    &                   log(prsl(i,k)/prsl(i,k-1)) * rog
             thekv(i)  = theta(i,k)*(1.+fv*max(q1(i,k,1),qmin))
             spdk2     = max(rcl(i)*(u1(i,k)**2+v1(i,k)**2),1.)
             rbup(i)   = (thekv(i)-the1v(i))*(g*zl(i,k)/the1v(i))/spdk2
             kpbl(i)   = k
             stable(i) = rbup(i).gt.rbcr
          endif
        enddo
      enddo
!
      do i = 1,im
         k = kpbl(i)
         if(rbdn(i).ge.rbcr) then
            rbint = 0.
         elseif(rbup(i).le.rbcr) then
            rbint = 1.
         else
            rbint = (rbcr-rbdn(i))/(rbup(i)-rbdn(i))
         endif
         hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
         if(hpbl(i).lt.zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
      enddo
!!
      do i=1,im
           hol = max(rbsoil(i)*fm(i)*fm(i)/fh(i),rimin)
           if(sfcflg(i)) then
              hol = min(hol,-zfmin)
           else
              hol = max(hol,zfmin)
           endif
!
!          hol = hol*hpbl(i)/zl1(i)*sfcfrac
           hol = hol*hpbl(i)/zl(i,1)*sfcfrac
           if(sfcflg(i)) then
!             phim = (1.-aphi16*hol)**(-1./4.)
!             phih = (1.-aphi16*hol)**(-1./2.)
              tem  = 1.0 / (1. - aphi16*hol)
              phih(i) = sqrt(tem)
              phim(i) = sqrt(phih(i))
           else
              phim(i) = (1.+aphi5*hol)
              phih(i) = phim(i)
           endif
           wscale(i) = ustar(i)/phim(i)
           wscale(i) = min(wscale(i),ustar(i)*aphi16)
           wscale(i) = max(wscale(i),ustar(i)/aphi5)
      enddo
!
!     compute the surface variables for pbl height estimation
!     under unstable conditions
!
      do i = 1,im
         sflux  = heat(i) + evap(i)*fv*the1(i)
         if(sfcflg(i).and.sflux.gt.0.0) then
           hgamt(i)   = min(cfac*heat(i)/wscale(i),gamcrt)
           hgamq(i)   = min(cfac*evap(i)/wscale(i),gamcrq)
           vpert      = hgamt(i) + fv*the1(i)*hgamq(i)
           vpert      = min(vpert,gamcrt)
           thermal(i) = thermal(i) + max(vpert,0.)
           hgamt(i)   = max(hgamt(i),0.0)
           hgamq(i)   = max(hgamq(i),0.0)
         else
           pblflg(i) = .false.
         endif
      enddo
!
      do i = 1,im
         if(pblflg(i)) then
            kpbl(i) = 1
            hpbl(i) = zi(i,2)
         endif
      enddo
!
!     enhance the pbl height by considering the thermal
!
      do i = 1, im
         if(pblflg(i)) then
            stable(i) = .false.
            rbup(i) = rbsoil(i)
         endif
      enddo
      do k = 2, kmpbl
        do i = 1, im
          if(.not.stable(i).and.pblflg(i)) then
            rbdn(i)   = rbup(i)
!           zl(i,k)   = zl(i,k-1) - (t1(i,k)+t1(i,k-1))/2 *                &
!    &                  log(prsl(i,k)/prsl(i,k-1)) * rog
            thekv(i)  = theta(i,k)*(1.+fv*max(q1(i,k,1),qmin))
            spdk2     = max(rcl(i)*(u1(i,k)**2+v1(i,k)**2),1.)
            rbup(i)   = (thekv(i)-thermal(i))*(g*zl(i,k)/the1v(i))/spdk2
            kpbl(i)   = k
            stable(i) = rbup(i).gt.rbcr
          endif
        enddo
      enddo
!
      do i = 1,im
         if(pblflg(i)) then
            k = kpbl(i)
            if(rbdn(i).ge.rbcr) then
               rbint = 0.
            elseif(rbup(i).le.rbcr) then
               rbint = 1.
            else
               rbint = (rbcr-rbdn(i))/(rbup(i)-rbdn(i))
            endif
            hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
            if(hpbl(i).lt.zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
            if(kpbl(i).le.1) pblflg(i) = .false.
         endif
      enddo
!!
!
!     compute diffusion coefficients below pbl
!
      do k = 1, kmpbl
         do i=1,im
            if(kpbl(i).gt.k) then
               prinv = 1.0 / (phih(i)/phim(i)+cfac*vk*.1)
               prinv = min(prinv,prmax)
               prinv = max(prinv,prmin)
               zfac = max((1.-(zi(i,k+1)-zl(i,1))/                       &
     &                (hpbl(i)-zl(i,1))), zfmin)
               dku(i,k) = xkzo + wscale(i)*vk*zi(i,k+1)                  &
     &                         * zfac**pfac
               dkt(i,k) = dku(i,k)*prinv
               dko(i,k) = (dku(i,k)-xkzo)*prinv
               dku(i,k) = min(dku(i,k),dkmax)
               dku(i,k) = max(dku(i,k),dkmin)
               dkt(i,k) = min(dkt(i,k),dkmax)
               dkt(i,k) = max(dkt(i,k),dkmin)
               dko(i,k) = max(0.0, min(dkmax, dko(i,k)))
            endif
         enddo
      enddo
!
!     compute diffusion coefficients over pbl (free atmosphere)
!
      do k = 1, km-1
         do i=1,im
            if(k.ge.kpbl(i)) then
!              ti   = 0.5*(t1(i,k)+t1(i,k+1))
               ti   = 2.0 / (t1(i,k)+t1(i,k+1))
!              rdz  = rdzt(i,k)/ti
               rdz  = rdzt(i,k) * ti
!              rdz  = rdzt(i,k)
               dw2  = rcl(i)*((u1(i,k)-u1(i,k+1))**2                     &
     &                      + (v1(i,k)-v1(i,k+1))**2)
               shr2 = max(dw2,dw2min)*rdz**2
               tvd  = t1(i,k)*(1.+fv*max(q1(i,k,1),qmin))
               tvu  = t1(i,k+1)*(1.+fv*max(q1(i,k+1,1),qmin))
!              bvf2 = g*(gocp+rdz*(tvu-tvd))/ti
               bvf2 = g*(gocp+rdz*(tvu-tvd)) * ti
               ri   = max(bvf2/shr2,rimin)
               zk   = vk*zi(i,k+1)
!              rl2  = (zk*rlam/(rlam+zk))**2
!              dk   = rl2*sqrt(shr2)
               rl2  = zk*rlam/(rlam+zk)
               dk   = rl2*rl2*sqrt(shr2)
               if(ri.lt.0.) then ! unstable regime
                  sri = sqrt(-ri)
                  dku(i,k) = xkzo + dk*(1+8.*(-ri)/(1+1.746*sri))
!                 dkt(i,k) = xkzo + dk*(1+8.*(-ri)/(1+1.286*sri))
                  tem      =        dk*(1+8.*(-ri)/(1+1.286*sri))
                  dkt(i,k) = xkzo + tem
                  dko(i,k) =        tem
               else             ! stable regime
!                 dkt(i,k)  = xkzo + dk/(1+5.*ri)**2
                  tem       =        dk/(1+5.*ri)**2
                  dkt(i,k)  = xkzo + tem
                  dko(i,k)  =        tem
                  prnum     = 1.0 + 2.1*ri
                  prnum     = min(prnum,prmax)
                  dku(i,k)  = (dkt(i,k)-xkzo)*prnum + xkzo
               endif
!
               dku(i,k) = min(dku(i,k),dkmax)
               dku(i,k) = max(dku(i,k),dkmin)
               dkt(i,k) = min(dkt(i,k),dkmax)
               dkt(i,k) = max(dkt(i,k),dkmin)
               dko(i,k) = max(0.0, min(dkmax, dko(i,k)))
!
!cc   if(i.eq.lond.and.lat.eq.latd) then
!cc   prnum = dku(k)/dkt(k)
!cc   write(iun,610) k,prnum,dkt(k),dku(k),rl2,ri,
!cc   1              bvf2,shr2
!cc   endif
!
            endif
         enddo
      enddo
!
!     compute tridiagonal matrix elements for heat and moisture
!
      do i=1,im
         ad(i,1) = 1.
         a1(i,1) = t1(i,1)   + beta(i) * heat(i)
         a2(i,1) = q1(i,1,1) + beta(i) * evap(i)
      enddo
!
      do k = 1,km-1
        do i = 1,im
          dtodsd = dt/del(i,k)
          dtodsu = dt/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
          rdz    = rdzt(i,k)*2./(t1(i,k)+t1(i,k+1))
!         rdz    = rdzt(i,k)
          tem1   = dsig * dkt(i,k) * rdz
          if(pblflg(i).and.k.lt.kpbl(i)) then
!            dsdzt = dsig*dkt(i,k)*rdz*(gocp-hgamt(i)/hpbl(i))
!            dsdzq = dsig*dkt(i,k)*rdz*(-hgamq(i)/hpbl(i))
             tem   = 1.0 / hpbl(i)
             dsdzt = tem1 * (gocp-hgamt(i)*tem)
             dsdzq = tem1 * (-hgamq(i)*tem)
             a2(i,k)   = a2(i,k)+dtodsd*dsdzq
             a2(i,k+1) = q1(i,k+1,1)-dtodsu*dsdzq
          else
!            dsdzt = dsig*dkt(i,k)*rdz*(gocp)
             dsdzt = tem1 * gocp
             a2(i,k+1) = q1(i,k+1,1)
          endif
!         dsdz2 = dsig*dkt(i,k)*rdz*rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
          a1(i,k)   = a1(i,k)+dtodsd*dsdzt
          a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
        enddo
      enddo
!
!     solve tridiagonal problem for heat and moisture
!
      call tridin(im,km,1,al,ad,au,a1,a2,au,a1,a2)
!
!     recover tendencies of heat and moisture
!
      do  k = 1,km
         do i = 1,im
            ttend      = (a1(i,k)-t1(i,k))*rdt
            qtend      = (a2(i,k)-q1(i,k,1))*rdt
            tau(i,k)   = tau(i,k)+ttend
            rtg(i,k,1) = rtg(i,k,1)+qtend
            dtsfc(i)   = dtsfc(i)+cont*del(i,k)*ttend
            dqsfc(i)   = dqsfc(i)+conq*del(i,k)*qtend
         enddo
      enddo
!
!     compute tridiagonal matrix elements for momentum
!
      do i=1,im
!        ad(i,1) = 1.+betaw(i)
         ad(i,1) = 1.0 + beta(i) * stress(i) / spd1(i)
         a1(i,1) = u1(i,1)
         a2(i,1) = v1(i,1)
!        ad(i,1) = 1.0
!        tem     = 1.0 + beta(i) * stress(i) / spd1(i)
!        a1(i,1) = u1(i,1) * tem
!        a2(i,1) = v1(i,1) * tem
      enddo
!
      do k = 1,km-1
        do i=1,im
          dtodsd    = dt/del(i,k)
          dtodsu    = dt/del(i,k+1)
          dsig      = prsl(i,k)-prsl(i,k+1)
          rdz       = rdzt(i,k)*2./(t1(i,k)+t1(i,k+1))
!         rdz       = rdzt(i,k)
          dsdz2     = dsig*dku(i,k)*rdz*rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
          a1(i,k+1) = u1(i,k+1)
          a2(i,k+1) = v1(i,k+1)
        enddo
      enddo
!
!     solve tridiagonal problem for momentum
!
      call tridi2(im,km,al,ad,au,a1,a2,au,a1,a2)
!
!     recover tendencies of momentum
!
      do k = 1,km
         do i = 1,im
            conwrc = conw*sqrt(rcl(i))
            utend = (a1(i,k)-u1(i,k))*rdt
            vtend = (a2(i,k)-v1(i,k))*rdt
            du(i,k)  = du(i,k)+utend
            dv(i,k)  = dv(i,k)+vtend
            dusfc(i) = dusfc(i)+conwrc*del(i,k)*utend
            dvsfc(i) = dvsfc(i)+conwrc*del(i,k)*vtend
         enddo
      enddo
!!
!
!     compute tridiagonal matrix elements for tracers
!
      if (ndiff .ge. 2) then
        do i=1,im
         ad(i,1) = 1.
        enddo
        do k = 2, ndiff
          is = (k-2) * km
          do i = 1, im
            at(i,1+is) = q1(i,1,k)
          enddo
        enddo
!
        do k = 1,km-1
          do i = 1,im
            dtodsd = dt/del(i,k)
            dtodsu = dt/del(i,k+1)
            dsig   = prsl(i,k)-prsl(i,k+1)
            rdz    = rdzt(i,k)*2./(t1(i,k)+t1(i,k+1))
            tem1   = dsig * dko(i,k) * rdz
            dsdz2     = tem1 * rdz
            au(i,k)   = -dtodsd*dsdz2
            al(i,k)   = -dtodsu*dsdz2
            ad(i,k)   = ad(i,k)-au(i,k)
            ad(i,k+1) = 1.-al(i,k)
          enddo
        enddo
        do kk = 2, ndiff
          is = (kk-2) * km
          do k = 1, km - 1
            do i = 1, im
              at(i,k+1+is) = q1(i,k+1,kk)
            enddo
          enddo
        enddo
!
!     solve tridiagonal problem for tracers
!
        call tridit(im,km,ndiff-1,al,ad,au,at,au,at)
!
!     recover tendencies of tracers
!
        do kk = 2, ndiff
          is = (kk-2) * km
          do k = 1, km 
            do i = 1, im
              qtend = (at(i,k+is)-q1(i,k,kk))*rdt
              rtg(i,k,kk) = rtg(i,k,kk) + qtend
            enddo
          enddo
        enddo
      endif
!!
      return
      end
