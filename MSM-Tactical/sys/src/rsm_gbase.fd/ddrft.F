
      subroutine ddrft(                                                 &
     &                  k, kd                                           &
     &,                 tla, alfint                                     &
     &,                 tol, qol, hol, prl, qst, hst, gam, gaf, hbl, qbl&
     &,                 qrb, qrt, buy, kbl, idh, eta, rnn               &
     &,                 alm, wfn, train, ddft                           &
     &,                 etd, hod, qod, evp, dof, cldfrd, wcb            &
     &,                 gms, gsd, ghd,lprnt)                   

!
!***********************************************************************
!******************** cumulus downdraft subroutine *********************
!****************** based on cheng and arakawa (1997)  ****** **********
!************************ subroutine ddrft  ****************************
!************************  30 april 1998  ******************************
!*******************  s. moorthi and m.j. suarez ***********************
!***********************************************************************
!***********************************************************************
!************* shrinivas.moorthi@noaa.gov (301) 763 8000(x7233) ********
!***************  max.suarez@gsfc.nasa.gov (301) 286 7373 **************
!***********************************************************************
!***********************************************************************
!23456789012345678901234567890123456789012345678901234567890123456789012
!
!===>  tol(k)     input   temperature            kelvin
!===>  qol(k)     input   specific humidity      non-dimensional

!===>  prl(k+1)   input   pressure @ edges       mb

!===>  k     input   the rise & the index of the subcloud layer
!===>  kd    input   detrainment level ( 1<= kd < k )          
!     
      use machine , only : kind_phys
      use module_ras
      implicit none
!
!  input arguments
!
      integer k, kd
      real(kind=kind_phys) alfint(k)
      integer kbl, kb1



      logical skpdd, skpup

      real(kind=kind_phys) hol(kd:k),   qol(kd:k),   gaf(kd:k+1)        &
     &,                    hst(kd:k),   qst(kd:k),   tol(kd:k)          &
     &,                    buy(kd:k+1), qrb(kd:k),   qrt(kd:k)          &
     &,                    gam(kd:k+1), rnn(kd:k)                       &
     &,                    eta(kd:k+1), prl(kd:k+1)
!
      real(kind=kind_phys)    hbl,     qbl,        pris                 &
     &,                       train,   wfn,        alm
!
!     temporary work space
!
      real(kind=kind_phys) gms(kd:k+1)
      real(kind=kind_phys) tx1,    tx2,  tx3, tx4                       &
     &,                    tx5,    tx6,  tx7, tx8, tx9
      logical unsat

      real(kind=kind_phys) tl, pl, ql, qs, dqs, st1,  hb, qb, tb        &
     &,                    qqq, picon, piinv, del_eta                   &
     &,                    tem, tem1, tem2, tem3, tem4, st2             &
     &,                    errmin, errmi2, errh, errw, erre, tem5       &
     &,                    tem6, hbd, qbd
      integer i, l,  n, ix, kd1, ii                                     &
     &,       kp1, it, km1, ktem, kk, kk1, lm1, ll, lp1                 &
     &,       ip1, jj

!
      parameter (errmin=0.0001, errmi2=0.1*errmin)
!     parameter (errmin=0.00001, errmi2=0.1*errmin)
!
      real(kind=kind_phys) tla,    stla,  ctl2, ctl3
      real(kind=kind_phys) gmf,    pi,    onpg, ctla, vtrm, vtpexp      &
     &,    rpart,  qrmin,  aa1,    bb1,   cc1,  dd1                     &
     &,    wc2min, wcmin,  wcbase, f2,    f3,   f5, gmf1, gmf5          &
     &,    qraf,   qrbf,   cmpor                
!    &,    sialf
!
!     parameter (onpg=1.0+0.5, gmf=1.0/onpg, rpart=0.5)
      parameter (onpg=1.0+0.5, gmf=1.0/onpg, rpart=1.0)
!     parameter (onpg=1.0+0.5, gmf=1.0/onpg, rpart=0.0)
!     parameter (aa1=1.0, bb1=1.5, cc1=1.1, dd1=0.85, f3=cc1, f5=2.5)
      parameter (aa1=2.0, bb1=1.5, cc1=1.1, dd1=0.85, f3=cc1, f5=2.5)
!     parameter (aa1=1.0, bb1=1.0, cc1=1.0, dd1=1.0, f3=cc1,  f5=1.0)
      parameter (qrmin=1.0e-6, wc2min=0.01, gmf1=gmf/aa1, gmf5=gmf/f5)
!     parameter (qrmin=1.0e-6, wc2min=1.00, gmf1=gmf/aa1, gmf5=gmf/f5)
!     parameter (sialf=0.5)
!
      parameter (pi=3.1415926535897931, piinv=1.0/pi)
      integer itr, itrmu, itrmd, ktpd, itrmin
      parameter (itrmu=25, itrmd=25, itrmin=10)
!     parameter (itrmu=14, itrmd=18, itrmin=7)
!     parameter (itrmu=10, itrmd=10, itrmin=5)
      real(kind=kind_phys) qrp(kd:k+1), wvl(kd:k+1), al2
!
      real(kind=kind_phys) rnf(kd:k),   etd(kd:k+1), wcb(kd:k)          &
     &,                    hod(kd:k+1), qod(kd:k+1), evp(kd:k)          &
     &,                    ror(kd:k+1), stlt(kd:k)                      &
     &,                    ghd(kd:k),   gsd(kd:k)                       &
     &,                    rnt,        rnb                              &
     &,                    errq,       cldfrd,     rntp
      integer idw, idh, idn(k), idnm
      real(kind=kind_phys) elm(k)
!     real(kind=kind_phys) em(k*k), elm(k)
      real(kind=kind_phys) edz, ddz, ce, qhs, fac, facg, asin,          &
     &                     rsum1, rsum2, rsum3, cee
      logical ddft, updret, ddlgk
!
      real(kind=kind_phys) aa(kd:k,kd:k+1), qw(kd:k,kd:k)               &
     &,                    bud(kd:k), vt(2), vrw(2), trw(2)             &
     &,                    gqw(kd:k)                                    &
     &,                    qa(3),     wa(3),    dof, dofw    

!***********************************************************************

      real(kind=kind_phys) qrpf, vtpf
      logical lprnt
!fpp$ expand (qrpf, qrabf, vtpf)
!fpp$ noconcur r

!

!     if(lprnt) print *,' k=',k,' kd=',kd,' in downdrft'

      kd1    = kd + 1
      kp1    = k  + 1
      km1    = k  - 1
      kb1    = kbl - 1
!
      cmpor  = cmb2pa / rgas
!
!     vtp    = 36.34*sqrt(1.2)* (0.001)**0.1364
      vtpexp = -0.3636
!     piinv  = 1.0 / pi
      picon  = pi * onebg * 0.5
!
!
!     compute rain water budget of the updraft (cheng and arakawa, 1997)
!
      cldfrd = 0.0
      rntp   = 0.0
      dof    = 0.0
      errq   = 10.0
      rnb    = 0.0
      rnt    = 0.0
      tx2    = prl(kbl)
!
      tx1      = (prl(kd) + prl(kd1)) * 0.5
      ror(kd)  = cmpor*tx1 / (tol(kd)*(1.0+nu*qol(kd)))
!     gms(kd)  = vtp * ror(kd) ** vtpexp
      gms(kd)  = vtp * vtpf(ror(kd))
!
      qrp(kd)  = qrmin
!
      tem      = tol(k) * (1.0 + nu * qol(k))
      ror(k+1) = 0.5 * cmpor * (prl(k+1)+prl(k)) / tem
      gms(k+1) = vtp * vtpf(ror(k+1))
      qrp(k+1) = qrmin
      buy(kd)  = max(buy(kd),one_m1)
!     buy(kd)  = max(buy(kd), 0.1)
!
      do l=kd1,k
        tem = 0.5 * (tol(l)+tol(l-1))                                   &
     &      * (1.0 + (0.5*nu) * (qol(l)+qol(l-1)))
        ror(l) = cmpor * prl(l) / tem
!       gms(l) = vtp * ror(l) ** vtpexp
        gms(l) = vtp * vtpf(ror(l))
        qrp(l) = qrmin
        buy(l) = max(buy(l),one_m1)
!       buy(l) = max(buy(l), 0.1)
      enddo
!
      call angrad(tx1, alm, stla, ctl2, al2, pi, tla, tx2, wfn, tx3)
!     if (lprnt) print *,' tla=',tla,' al2=',al2,' tx1=',tx1
!
!    following ucla approach for rain profile
!
      f2      = 2.0*bb1*onebg/(pi*0.2)
      wcmin   = sqrt(wc2min)
      wcbase  = wcmin
!

      stla = f2     * stla * al2
      ctl2 = dd1    * ctl2
      ctl3 = 0.1364 * ctl2
!
      do l=kd,k
        rnf(l)   = 0.0
        wvl(l)   = 0.0
        stlt(l)  = 0.0
        gqw(l)   = 0.0
        do n=kd,k
          qw(n,l) = 0.0
        enddo
      enddo
!
!-----qw(n,l) = d(w(n)*w(n))/dqr(l)
!
      kk = kbl
      wvl(kk)    = wcbase
      qw(kd,kd)  = -qrb(kd)  * gmf1
      ghd(kd)    = eta(kd)   * eta(kd)
      gqw(kd)    = qw(kd,kd) * ghd(kd)
      gsd(kd)    = 1.0 / ghd(kd)
!
      gqw(kk)    = -  qrb(kk-1) * (gmf1+gmf1)
!
      wvl(kk)    = wcbase
      wcb(kk)    = wcbase * wcbase
      tx1        = wcb(kk)
      gsd(kk)    = 1.0
      ghd(kk)    = 1.0
!
      tem        = gmf1 + gmf1
      do l=kb1,kd1,-1
         ghd(l)  = eta(l) * eta(l)
         gsd(l)  = 1.0 / ghd(l)
         gqw(l)  = - ghd(l) * (qrb(l-1)+qrt(l)) * tem
         qw(l,l) = - qrt(l) * tem
!
         tx1     = tx1 + buy(l) * tem
         wcb(l)  = tx1 * gsd(l)
      enddo
!
      tem1        = (qrb(kd) + qrt(kd1) + qrt(kd1)) * gmf1
      gqw(kd1)    = - ghd(kd1) * tem1
!     qw(l,kd1)   = - qrt(kd1) * tem
      qw(kd1,kd1) = - qrt(kd1) * tem
      wcb(kd)     = (tx1 + buy(kd)*tem) * gsd(kd)
!
      do l=kd1,kbl
        do n=kd,l-1
           qw(n,l) = gqw(l) * gsd(n)
        enddo
      enddo
      qw(kbl,kbl) = 0.0
!
      wvl(kbl)    = wcbase
      stlt(kbl)   = 1.0 / wcbase
!
      do l=kd,k+1
        do n=kd,k
          aa(n,l) = 0.0
        enddo
      enddo
!
      skpup = .false.
!
      do itr=1,itrmu               ! rain profile iteration starts!
        if (.not. skpup) then
!
!-----calculating the vertical velocity
!
          tx1      = 0.0
          ghd(kbl) = 1.0 / qrp(kbl)
          do l=kb1,kd,-1
            tx1     = tx1    + qrp(l+1) * gqw(l+1)
            st1     = wcb(l) + qw(l,l)  * qrp(l)                        &
     &                       + tx1      * gsd(l)
            if (st1 .gt. 0.0) then
              wvl(l)  = sqrt(st1)
            else
              wvl(l) = 0.5 * (wvl(l) + wvl(l+1))
            endif
!           wvl(l)  = sqrt(max(st1,wc2min))
            stlt(l) = 1.0 / wvl(l)
            ghd(l)  = 1.0 / qrp(l)
          enddo
!
!     if (lprnt) then
!     print *,' itr=',itr,' itrmu=',itrmu
!     print *,' wvl=',(wvl(l),l=kd,kbl)
!     print *,' qrp=',(qrp(l),l=kd,kbl)
!     print *,' ghd=',(ghd(l),l=kd,kbl)
!     print *,' rnf=',(rnf(l),l=kd,kbl)
!     endif
!
!-----calculating trw, vrw and of
!
!         vt(1)   = gms(kd) * qrp(kd)**0.1364
          vt(1)   = gms(kd) * qrpf(qrp(kd))
          trw(1)  = eta(kd) * qrp(kd) * stlt(kd)
          tx6     = trw(1) * vt(1)
          vrw(1)  = f3*wvl(kd) - ctl2*vt(1)
          bud(kd) = stla * tx6 * qrb(kd) * 0.5
          rnf(kd) = bud(kd)
          dof     = 1.1364 * bud(kd) * ghd(kd)
          dofw    = -bud(kd) * stlt(kd)
!
          rnt     = trw(1) * vrw(1)
          tx2     = 0.0
          tx4     = 0.0
          rnb     = rnt
          tx1     = 0.5
          tx8     = 0.0
!
          if (rnt .ge. 0.0) then
            tx3 = (rnt-ctl3*tx6) * ghd(kd)
            tx5 = ctl2 * tx6 * stlt(kd)
          else
            tx3 = 0.0
            tx5 = 0.0
            rnt = 0.0
            rnb = 0.0
          endif
!
          do l=kd1,kb1
            ktem    = max(l-2, kd)
            ll      = l - 1
!
!           vt(2)   = gms(l) * qrp(l)**0.1364
            vt(2)   = gms(l) * qrpf(qrp(l))
            trw(2)  = eta(l) * qrp(l) * stlt(l)
            vrw(2)  = f3*wvl(l) - ctl2*vt(2)
            qqq     = stla * trw(2) * vt(2)
            st1     = tx1  * qrb(ll)
            bud(l)  = qqq * (st1 + qrt(l))
!
            qa(2)   = dof
            wa(2)   = dofw
            dof     = 1.1364 * bud(l) * ghd(l)
            dofw    = -bud(l) * stlt(l)
!
            rnf(ll) = rnf(ll) + qqq * st1
            rnf(l)  =           qqq * qrt(l)
!
            tem3    = vrw(1) + vrw(2)
            tem4    = trw(1) + trw(2)
!
            tx6     = .25 * tem3 * tem4
            tem4    = tem4 * ctl3
!
!-----by qr above
!
!           tem1    = .25*(trw(1)*tem3 - tem4*vt(1))*tx7
            tem1    = .25*(trw(1)*tem3 - tem4*vt(1))*ghd(ll)
            st1     = .25*(trw(1)*(ctl2*vt(1)-vrw(2))                   &
     &                  * stlt(ll) + f3*trw(2))
!-----by qr below
            tem2    = .25*(trw(2)*tem3 - tem4*vt(2))*ghd(l)
            st2     = .25*(trw(2)*(ctl2*vt(2)-vrw(1))                   &
     &                 * stlt(l)  + f3*trw(1))
!
!      from top to  the kbl-2 layer
!
            qa(1)   = tx2
            qa(2)   = qa(2) + tx3 - tem1
            qa(3)   = -tem2
!
            wa(1)   = tx4
            wa(2)   = wa(2) + tx5 - st1
            wa(3)   = -st2
!
            tx2     = tem1
            tx3     = tem2
            tx4     = st1
            tx5     = st2
!
            vt(1)   = vt(2)
            trw(1)  = trw(2)
            vrw(1)  = vrw(2)
!
            if (wvl(ktem) .eq. wcmin) wa(1) = 0.0
            if (wvl(ll)   .eq. wcmin) wa(2) = 0.0
            if (wvl(l)    .eq. wcmin) wa(3) = 0.0
            do n=ktem,kbl
              aa(ll,n) = (wa(1)*qw(ktem,n) * stlt(ktem)                 &
     &                 +  wa(2)*qw(ll,n)   * stlt(ll)                   &
     &                 +  wa(3)*qw(l,n)    * stlt(l) ) * 0.5
            enddo
            aa(ll,ktem) = aa(ll,ktem) + qa(1)
            aa(ll,ll)   = aa(ll,ll)   + qa(2)
            aa(ll,l)    = aa(ll,l)    + qa(3)
            bud(ll)     = (tx8 + rnn(ll)) * 0.5                         &
     &                    - rnb + tx6 - bud(ll)
            aa(ll,kbl+1) = bud(ll)
            rnb = tx6
            tx1 = 1.0
            tx8 = rnn(ll)
          enddo
          l  = kbl
          ll = l - 1
!         vt(2)   = gms(l) * qrp(l)**0.1364
          vt(2)   = gms(l) * qrpf(qrp(l))
          trw(2)  = eta(l) * qrp(l) * stlt(l)
          vrw(2)  = f3*wvl(l) - ctl2*vt(2)
          st1     = stla * trw(2) * vt(2) * qrb(ll)
          bud(l)  = st1

          qa(2)   = dof
          wa(2)   = dofw
          dof     = 1.1364 * bud(l) * ghd(l)
          dofw    = -bud(l) * stlt(l)
!
          rnf(ll) = rnf(ll) + st1
!
          tem3    = vrw(1) + vrw(2)
          tem4    = trw(1) + trw(2)
!
          tx6     = .25 * tem3 * tem4
          tem4    = tem4 * ctl3
!
!-----by qr above
!
          tem1    = .25*(trw(1)*tem3 - tem4*vt(1))*ghd(ll)
          st1     = .25*(trw(1)*(ctl2*vt(1)-vrw(2))                     &
     &                * stlt(ll) + f3*trw(2))
!-----by qr below
          tem2    = .25*(trw(2)*tem3 - tem4*vt(2))*ghd(l)
          st2     = .25*(trw(2)*(ctl2*vt(2)-vrw(1))                     &
     &                 * stlt(l)  + f3*trw(1))
!
!      for the layer next to the top of the boundary layer
!
          qa(1)   = tx2
          qa(2)   = qa(2) + tx3 - tem1
          qa(3)   = -tem2
!
          wa(1)   = tx4
          wa(2)   = wa(2) + tx5 - st1
          wa(3)   = -st2
!
          tx2     = tem1
          tx3     = tem2
          tx4     = st1
          tx5     = st2
!
          idw     = max(l-2, kd)
!
          if (wvl(idw) .eq. wcmin) wa(1) = 0.0
          if (wvl(ll)  .eq. wcmin) wa(2) = 0.0
          if (wvl(l)   .eq. wcmin) wa(3) = 0.0
!
          kk = idw
          do n=kk,l
            aa(ll,n) = (wa(1)*qw(kk,n) * stlt(kk)                       &
     &               +  wa(2)*qw(ll,n) * stlt(ll)                       &
     &               +  wa(3)*qw(l,n)  * stlt(l) ) * 0.5

          enddo
!
          aa(ll,idw) = aa(ll,idw) + qa(1)
          aa(ll,ll)  = aa(ll,ll)  + qa(2)
          aa(ll,l)   = aa(ll,l)   + qa(3)
          bud(ll)    = (tx8+rnn(ll)) * 0.5 - rnb + tx6 - bud(ll)
!
          aa(ll,l+1) = bud(ll)
!
          rnb        = trw(2) * vrw(2)
!
!      for the top of the boundary layer
!
          if (rnb .lt. 0.0) then
             kk    = kbl
             tem   = vt(2) * trw(2)
             qa(2) = (rnb - ctl3*tem) * ghd(kk)
             wa(2) = ctl2 * tem * stlt(kk)
          else
             rnb   = 0.0
             qa(2) = 0.0
             wa(2) = 0.0
          endif
!
          qa(1) = tx2
          qa(2) = dof + tx3 - qa(2)
          qa(3) = 0.0
!
          wa(1) = tx4
          wa(2) = dofw + tx5 - wa(2)
          wa(3) = 0.0
!
          kk = kbl
          if (wvl(kk-1) .eq. wcmin) wa(1) = 0.0
          if (wvl(kk)   .eq. wcmin) wa(2) = 0.0
!
          do ii=1,2
             n = kk + ii - 2
             aa(kk,n) = (wa(1)*qw(kk-1,n) * stlt(kk-1)                  &
     &                +  wa(2)*qw(kk,n)   * stlt(kk)) * 0.5
          enddo
          fac = 0.5
          ll  = kbl
          l   = ll + 1
          lm1 = ll - 1
          aa(ll,lm1)  = aa(ll,lm1) + qa(1)
          aa(ll,ll)   = aa(ll,ll)  + qa(2)
          bud(ll)     = 0.5*rnn(lm1) - tx6 + rnb - bud(ll)
          aa(ll,ll+1) = bud(ll)
!
!-----solving the budget equations for dqr
!
          do l=kd1,kbl
            lm1  = l - 1
            unsat = abs(aa(lm1,lm1)) .lt. abs(aa(l,lm1))
            do  n=lm1,kbl+1
               if (unsat) then
                  tx1       = aa(lm1,n)
                  aa(lm1,n) = aa(l,n)
                  aa(l,n)   = tx1
               endif
            enddo
            tx1 = aa(l,lm1) / aa(lm1,lm1)
            do  n=l,kbl+1
               aa(l,n) = aa(l,n) - tx1 * aa(lm1,n)
            enddo
          enddo     
!
!-----back substitution and check if the solution converges
!
          kk = kbl
          kk1 = kk + 1
          aa(kk,kk1) = aa(kk,kk1) / aa(kk,kk)      !   qr correction !
          tx2        = abs(aa(kk,kk1)) * ghd(kk)   !   error measure !
!
          kk = kbl + 1
          do l=kb1,kd,-1
             lp1   = l + 1
             tx1  = 0.0
             do n=lp1,kbl
               tx1  = tx1 + aa(l,n) * aa(n,kk)
             enddo
             aa(l,kk) = (aa(l,kk) - tx1) / aa(l,l)       ! qr correction !
             tx2      = max(tx2, abs(aa(l,kk))*ghd(l))   ! error measure !
          enddo
!
          if (tx2 .gt. 1.0) then
            tem = 0.5
          else
            tem = 1.0
          endif
          do l=kd,kbl
!            qrp(l) = max(qrp(l)+aa(l,kbl+1), qrmin)
             qrp(l) = max(qrp(l)+aa(l,kbl+1)*tem, qrmin)
          enddo
!
          if (itr .lt. itrmin) then
             tem = abs(errq-tx2) 
             if (tem .ge. errmi2 .and. tx2 .ge. errmin) then 
               errq  = tx2                              ! further iteration !
             else 
               skpup = .true.                           ! converges      !
               errq  = 0.0                              ! rain profile exists!
!     print *,' here1',' tem=',tem,' tx2=',tx2,' errmi2=',
!    *errmi2,' errmin=',errmin
             endif 
          else
             tem = errq - tx2
             if (tem .lt. zero .and. errq .gt. 0.5) then
               skpup = .true.                           ! no convergence !
               errq = 10.0                              ! no rain profile!
             elseif (abs(tem).lt.errmi2 .or. tx2.lt.errmin) then
               skpup = .true.                           ! converges      !
               errq = 0.0                               ! rain profile exists!
!     print *,' here2'
             else
               errq = tx2                               ! further iteration !
             endif
          endif
!
!         if (lprnt) print *,' errq=',errq

        endif                                           ! skpup  endif!
!
      enddo                                          ! end of the itr loop!!
!     if(lprnt) then
!       print *,' qrp=',(qrp(l),l=kd,kbl)
!       print *,'rnf=',(rnf(l),l=kd,kbl),' rnt=',rnt,' rnb=',rnb
!     endif
!
      if (errq .lt. 0.1) then
        ddft = .true.
        rnb  = - rnb
        do l=kd1,kb1-1
          if (wvl(l)-wcbase .lt. 1.0e-9) ddft = .false.
        enddo
      else
        ddft = .false.
      endif
!
!     caution !! below is an adjustment to rain flux to maintain
!                conservation of precip!
!
      if (ddft) then
        tx1 = 0.0
        do l=kd,kb1
          tx1 = tx1 + rnf(l)
        enddo
!     print *,' tx1+rnt+rnb=',tx1+rnt+rnb
        tx1 = train / (tx1+rnt+rnb)
        if (abs(tx1-1.0) .lt. 0.2) then
           rnt = max(rnt*tx1,zero)
           rnb = rnb * tx1
        else
           ddft = .false.
        endif
      endif
!
      dof = 0.0
      if (.not. ddft) return     ! rain profile did not converge!
!

      do l=kd,kb1
         rnf(l) = rnf(l) * tx1

      enddo
!     if (lprnt) print *,' train=',train
!     if (lprnt) print *,' rnf=',rnf
!
!     adjustment is over
!
!     downdraft
!
      do l=kd,k
        wcb(l) = 0.0
      enddo
!
      skpdd = .not. ddft
!
      errq  = 10.0
      if (.not. skpdd) then
!
!     calculate downdraft properties
!

        kk = max(kb1,kd1)
        do l=kk,k
          stlt(l) = stlt(l-1)
        enddo
        tem1 = 1.0 / bb1
        do l=kd,k
          evp(l) = 0.0

          if (l .lt. kbl) then
            tem     = stla * tem1
            stlt(l) = eta(l) * stlt(l) * tem / ror(l)
          else
            stlt(l) = 0.0
          endif
        enddo

        rsum1 = 0.0
        rsum2 = 0.0

!
        idn      = 99
        do l=kd,k+1
          etd(l)  = 0.0
          wvl(l)  = 0.0
          qrp(l)  = 0.0
        enddo
        do l=kd,k
          evp(l)  = 0.0
          buy(l)  = 0.0
        enddo
        hod(kd)  = hol(kd)
        qod(kd)  = qol(kd)
        tx1      = min(stlt(kd)*qrb(kd)*one, one)    ! sigma at the top
        rntp     = 0.0
        tx5      = tx1
        qa(1)    = 0.0
!     if(lprnt) print *,' stlt=',stlt(kd),' qrb=',qrb(kd)
!    *,' tx1=',tx1,' ror=',ror(kd),' gms=',gms(kd),' rpart=',rpart
!    *,' rnt=',rnt
!
!       here we assume rpart of detrained rain rnt goes to pd
!
        if (rnt .gt. 0.0) then
          rntp    = (1.0 - rpart) * rnt
          qrp(kd) = (rpart*rnt / (ror(kd)*tx1*gms(kd))) ** (1.0/1.1364)
          buy(kd) = - ror(kd) * tx1 * qrp(kd)
        endif
!
!     l-loop for the downdraft iteration from kd1 to k+1 (bottom surface)
!
!     bud(kd) = ror(kd)
      idnm = 1
      do l=kd1,k+1

          ddlgk = idn(idnm) .eq. 99
          if (.not. ddlgk) cycle
          if (l .le. k) then
            st1   = 1.0 - alfint(l)
            wa(1) = alfint(l)*hol(l-1) + st1*hol(l)
            wa(2) = alfint(l)*qol(l-1) + st1*qol(l)
            wa(3) = alfint(l)*tol(l-1) + st1*tol(l)
            qa(2) = alfint(l)*hst(l-1) + st1*hst(l)
            qa(3) = alfint(l)*qst(l-1) + st1*qst(l)
          else
            wa(1) = hol(k)
            wa(2) = qol(k)
            wa(3) = tol(k)
            qa(2) = hst(k)
            qa(3) = qst(k)
          endif
!
          fac = 2.0
          if (l .eq. kd1) fac = 1.0

          facg    = fac * 0.5 * gmf5     !  12/17/97
!
!         ddlgk   =  idn(idnm) .eq. 99
          bud(kd) = ror(l)

!         if (ddlgk) then
            tx1    = tx5
            wvl(l) = max(wvl(l-1),one_m1)

            qrp(l) = max(qrp(l-1),qrp(l))
!
!           vt(1)  = gms(l-1) * qrp(l-1) ** 0.1364
            vt(1)  = gms(l-1) * qrpf(qrp(l-1))
            rnt    = ror(l-1) * (wvl(l-1)+vt(1))*qrp(l-1)
!     if(lprnt) print *,' l=',l,' qa=',qa(1), ' tx1rnt=',rnt*tx1,
!    *' wvl=',wvl(l-1)
!    *,' qrp=',qrp(l-1),' tx5=',tx5,' tx1=',tx1,' rnt=',rnt

!

!           tem    = max(alm, 2.5e-4) * max(eta(l), 1.0)
            tem    = max(alm,one_m6) * max(eta(l), one)
!           tem    = max(alm, 1.0e-5) * max(eta(l), 1.0)
            trw(1) = picon*tem*(qrb(l-1)+qrt(l-1))
            trw(2) = 1.0 / trw(1)
!
            vrw(1) = 0.5 * (gam(l-1) + gam(l))
            vrw(2) = 1.0 / (vrw(1) + vrw(1))
!
            tx4    =  (qrt(l-1)+qrb(l-1))*(onebg*fac*500.00)
!
            dofw   = 1.0 / (wa(3) * (1.0 + nu*wa(2)))      !  1.0 / tvbar!
!
            etd(l) = etd(l-1)
            hod(l) = hod(l-1)
            qod(l) = qod(l-1)
!
            errq   = 10.0

!
            if (l .le. kbl) then
              tx3 = stlt(l-1) * qrt(l-1) * (0.5*fac)
              tx8 = stlt(l)   * qrb(l-1) * (0.5*fac)
              tx9 = tx8 + tx3
            else
              tx3 = 0.0
              tx8 = 0.0
              tx9 = 0.0
            endif
!
            tem  = wvl(l-1) + vt(1)
            if (tem .gt. 0.0) then
              tem1 = 1.0 / (tem*ror(l-1))
              tx3 = vt(1) * tem1 * ror(l-1) * tx3
              tx6 = tx1 * tem1
            else
              tx6 = 1.0
            endif
!         endif
!
          if (l .eq. kd1) then
            if (rnt .gt. 0.0) then
              tem    = max(qrp(l-1),qrp(l))
              wvl(l) = tx1 * tem * qrb(l-1)*(facg*5.0)
            endif
            wvl(l) = max(one_m2, wvl(l))
            trw(1) = trw(1) * 0.5
            trw(2) = trw(2) + trw(2)
          else
            if (ddlgk) evp(l-1) = evp(l-2)
          endif
!
!       no downdraft above level idh
!

          if (l .lt. idh) then

            etd(l)   = 0.0
            hod(l)   = wa(1)
            qod(l)   = wa(2)
            evp(l-1) = 0.0
            wvl(l)   = 0.0
            qrp(l)   = 0.0
            buy(l)   = 0.0
            tx5      = tx9
            errq     = 0.0
            rntp     = rntp + rnt * tx1
            rnt      = 0.0
            wcb(l-1) = 0.0
          endif
!         bud(kd) = ror(l)
!
!       iteration loop for a given level l begins
!
          do itr=1,itrmd
!
!           unsat =  ddlgk .and. (errq .gt. errmin)
            unsat =  errq .gt. errmin
            if (unsat) then
!
!             vt(1)  = gms(l) * qrp(l) ** 0.1364
              vt(1)  = gms(l) * qrpf(qrp(l))
              tem    =  wvl(l) + vt(1)
!
              if (tem .gt. 0.0) then
                st1    = ror(l) * tem * qrp(l) + rnt
                if (st1 .ne. 0.0) st1 = 2.0 * evp(l-1) / st1
                tem1   = 1.0 / (tem*ror(l))
                tem2   = vt(1) * tem1 * ror(l) * tx8
              else
                tem1   = 0.0
                tem2   = tx8
                st1    = 0.0
              endif
!
              tem = ror(l)*wvl(l) - ror(l-1)*wvl(l-1)
              if (tem .gt. 0.0) then
                tx5 = (tx1 - st1 + tem2 + tx3) / (1.0+tem*tem1)
              else
                tx5 = (tx1 - tem*tx6 - st1 + tem2 + tx3)
              endif
!
!             qqq = 1.0 + tem * tem1 * (1.0 - sialf)
!
!             if (qqq .gt. 0.0) then
!               tx5   = (tx1 - sialf*tem*tx6 - st1 + tem2 + tx3) / qqq
!             else
!               tx5   = (tx1 - tem*tx6 - st1 + tem2 + tx3)
!             endif
!
!     if(lprnt) print *,' tx51=',tx5,' tx1=',tx1,' st1=',st1,' tem2='
!     if(tx5 .le. 0.0 .and. l .gt. kd+2)
!    * print *,' tx51=',tx5,' tx1=',tx1,' st1=',st1,' tem2='
!    *,tem2,' tx3=',tx3,' tem=',tem,' tem1=',tem,' wvl=',wvl(l-1),wvl(l)
!    *,' l=',l,' itr=',itr,' evp=',evp(l-1),' vt=',vt(1)
!    *,' qrp=',qrp(l),' rnt=',rnt,' kd=',kd

              tx5   = max(tx5,zero)

!
              tem1   = etd(l)
              etd(l) = ror(l) * tx5 * max(wvl(l),zero)
!

              del_eta = etd(l) - etd(l-1)

!               tem       = del_eta * trw(2)
!               tem2      = max(min(tem, 1.0), -1.0)
!               if (abs(tem) .gt. 1.0 .and. etd(l) .gt. 0.0 ) then
!                 del_eta = tem2 * trw(1)
!                 etd(l)  = etd(l-1) + del_eta
!               endif
!               if (wvl(l) .gt. 0.0) tx5 = etd(l) / (ror(l)*wvl(l))
!
                erre  = etd(l) - tem1
!
                tem  = max(abs(del_eta), trw(1))
                tem2 = del_eta / tem
                tem1 = sqrt(max((tem+del_eta)*(tem-del_eta),zero))
!               tem1 = sqrt(max((trw(1)+del_eta)*(trw(1)-del_eta),0.0))

                edz  = (0.5 + asin(tem2)*piinv)*del_eta + tem1*piinv

              ddz   = edz - del_eta
              wcb(l-1) = etd(l) + ddz
!
              tem1  = hod(l)
              if (del_eta .gt. 0.0) then
                qqq    = 1.0 / (etd(l) + ddz)
                hod(l) = (etd(l-1)*hod(l-1) + del_eta*hol(l-1)          &
     &                                            + ddz*wa(1)) * qqq
                qod(l) = (etd(l-1)*qod(l-1) + del_eta*qol(l-1)          &
     &                                            + ddz*wa(2)) * qqq
              elseif((etd(l-1) + edz) .gt. 0.0) then
                qqq    = 1.0 / (etd(l-1) + edz)
                hod(l) = (etd(l-1)*hod(l-1) + edz*wa(1)) * qqq
                qod(l) = (etd(l-1)*qod(l-1) + edz*wa(2)) * qqq
              endif
              errh  = hod(l) - tem1
              errq  = abs(errh/hod(l))  + abs(erre/max(etd(l),one_m5))
              dof   = ddz
              vt(2) = qqq

!
              ddz  = dof
              tem4 = qod(l)
              tem1 = vrw(1)
!
              qhs  = qa(3) + 0.5 * (gaf(l-1)+gaf(l))                    &
     &                           * (hod(l)-qa(2))
!
!                                           first iteration       !
!
              st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
              tem2 = ror(l) * qrp(l)
              call qrabf(tem2,qraf,qrbf)
              tem6 = tx5 * (1.6 + 124.9 * qraf) * qrbf * tx4
!
              ce   = tem6 * st2 / ((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!
              tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*qod(l))
              tem3   = (1.0 + tem1) * qhs * (qod(l)+ce)
              tem    = max(tem2*tem2 - 4.0*tem1*tem3,zero)
              qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))
!

!
!                                            second iteration   !
!
              st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
              ce   = tem6 * st2 / ((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!             cee  = ce * (etd(l)+ddz)
!


              tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*tem4)
              tem3   = (1.0 + tem1) * qhs * (tem4+ce)
              tem    = max(tem2*tem2 - 4.0*tem1*tem3,zero)
              qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))
!                                              evaporation in layer l-1
!

              evp(l-1) = (qod(l)-tem4) * (etd(l)+ddz)
!                                              calculate pd (l+1/2)
              qa(1)    = tx1*rnt + rnf(l-1) - evp(l-1)
!
!     if(lprnt) print *,' etd=',etd(l),' tx5=',tx5,' rnt=',rnt
!    *,' rnf=',rnf(l-1),' evp=',evp(l-1),' itr=',itr,' l=',l

!
              if (qa(1) .gt. 0.0) then
              if (etd(l) .gt. 0.0) then
                tem    = qa(1) / (etd(l)+ror(l)*tx5*vt(1))
                qrp(l) = max(tem,zero)
              elseif (tx5 .gt. 0.0) then
                qrp(l) = (max(zero,qa(1)/(ror(l)*tx5*gms(l))))           &
     &                                          ** (1.0/1.1364)
              else
                qrp(l) = 0.0
              endif
              else
                qrp(l) = 0.5 * qrp(l)
              endif
!                                              compute buoyancy
              tem1   = wa(3)+(hod(l)-wa(1)-alhl*(qod(l)-wa(2)))         &
     &                                                  * (1.0/cp)
              tem1   = tem1 * (1.0 + nu*qod(l))
              ror(l) = cmpor * prl(l) / tem1
              tem1   = tem1 * dofw
!!!           tem1   = tem1 * (1.0 + nu*qod(l)) * dofw

              buy(l) = (tem1 - 1.0 - qrp(l)) * ror(l) * tx5
!                                              compute w (l+1/2)

              tem1   = wvl(l)
!             if (etd(l) .gt. 0.0) then
              wvl(l) = vt(2) * (etd(l-1)*wvl(l-1) - facg                &
     &                 * (buy(l-1)*qrt(l-1)+buy(l)*qrb(l-1)))
!             endif
!
              if (wvl(l) .lt. 0.0) then
!               wvl(l) = max(wvl(l), 0.1*tem1)
                wvl(l) = 0.5*tem1
              else
                wvl(l) = 0.5*(wvl(l)+tem1)
              endif

!
!             wvl(l) = max(0.5*(wvl(l)+tem1), 0.0)

              errw   = wvl(l) - tem1
!
              errq   = errq + abs(errw/max(wvl(l),one_m5))

!     if(lprnt .or. tx5 .eq. 0.0) then
!     if(tx5 .eq. 0.0 .and. l .gt. kbl) then
!        print *,' errq=',errq,' itr=',itr,' l=',l,' wvl=',wvl(l)
!    &,' tx5=',tx5,' idnm=',idnm,' etd1=',etd(l-1),' etd=',etd(l)
!    &,' kbl=',kbl
!     endif
!
!     if(lprnt) print *,' itr=',itr,' itrmin=',itrmin,' itrmd=',itrmd
              if (itr .ge. min(itrmin,itrmd/2)) then
!     if(lprnt) print *,' itr=',itr,' etd1=',etd(l-1),' errq=',errq
                if (etd(l-1) .eq. 0.0 .and. errq .gt. 0.2) then
!     if(lprnt) print *,' bud=',bud(kd),' wa=',wa(1),wa(2)
                  ror(l)   = bud(kd)
                  etd(l)   = 0.0
                  wvl(l)   = 0.0
                  errq     = 0.0
                  hod(l)   = wa(1)
                  qod(l)   = wa(2)
                  tx5      = tx1 + tx9

!     if(lprnt) print *,' tx1=',tx1,' rnt=',rnt,' rnf=',rnf(l-1)
!    *,' evp=',evp(l-1),' l=',l
                  evp(l-1) = 0.0
                  tem      = max(tx1*rnt+rnf(l-1),zero)
                  qa(1)    = tem - evp(l-1)
!                 if (qa(1) .gt. 0.0) then
!     if(lprnt) print *,' ror=',ror(l),' tx5=',tx5,' tx1=',tx1
!    *,' tx9=',tx9,' gms=',gms(l),' qa=',qa(1)
!     if(lprnt) call mpi_quit(13)
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0)
!    *  print *,' atx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' l=',l,' qa=',qa(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' idnm=',idnm,' idn=',idn(idnm)
!    *,' errq=',errq
                  qrp(l)   = (qa(1) / (ror(l)*tx5*gms(l)))              &
     &                                            ** (1.0/1.1364)
!                 endif
                  buy(l)   = - ror(l) * tx5 * qrp(l)
                  wcb(l-1) = 0.0
                endif
!
                del_eta = etd(l) - etd(l-1)
                if(del_eta .lt. 0.0 .and. errq .gt. 0.1) then
                  ror(l)   = bud(kd)
                  etd(l)   = 0.0
                  wvl(l)   = 0.0
                  tx5      = tx1 + tx9
                  cldfrd   = tx5
!
                  del_eta  = - etd(l-1)
                  edz      = 0.0
                  ddz      = -del_eta
                  wcb(l-1) = ddz

!
                  hod(l)   = hod(l-1)
                  qod(l)   = qod(l-1)

!
                  tem4     = qod(l)
                  tem1     = vrw(1)
!
                  qhs      = qa(3) + 0.5 * (gaf(l-1)+gaf(l))            &
     &                                   * (hod(l)-qa(2))

!
!                                           first iteration       !
!
                  st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
                  tem2 = ror(l) * qrp(l-1)
                  call qrabf(tem2,qraf,qrbf)
                  tem6 = tx5 * (1.6 + 124.9 * qraf) * qrbf * tx4
!
                  ce   = tem6*st2/((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!

                  tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*qod(l))
                  tem3   = (1.0 + tem1) * qhs * (qod(l)+ce)
                  tem    = max(tem2*tem2 -four*tem1*tem3,zero)
                  qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))
!
!                                            second iteration   !
!
                  st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
                  ce   = tem6*st2/((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!                 cee  = ce * (etd(l)+ddz)
!


                  tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*tem4)
                  tem3   = (1.0 + tem1) * qhs * (tem4+ce)
                  tem    = max(tem2*tem2 -four*tem1*tem3,zero)
                  qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))

!                                              evaporation in layer l-1
!
                  evp(l-1) = (qod(l)-tem4) * (etd(l)+ddz)

!                                               calculate pd (l+1/2)
!                 rnn(l-1) = tx1*rnt + rnf(l-1) - evp(l-1)
                  qa(1)    = tx1*rnt + rnf(l-1)
                  evp(l-1) = min(evp(l-1), qa(1))
                  qa(1)    = qa(1) - evp(l-1)
                  qrp(l)   = 0.0
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0)
!    *  print *,' btx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' l=',l,' qa=',qa(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' del_eta=',del_eta
                  if (qa(1) .gt. 0.0) then
                    qrp(l) = (qa(1) / (ror(l)*tx5*gms(l)))              &
     &                                         ** (1.0/1.1364)
                  endif
                  errq   = 0.0
!                                              compute buoyancy
                  tem1   = wa(3)+(hod(l)-wa(1)-alhl*(qod(l)-wa(2)))     &
     &                                                  * (1.0/cp)
                  tem1   = tem1 * (1.0 + nu*qod(l)) * dofw

                  buy(l) = (tem1 - 1.0 - qrp(l)) * ror(l) * tx5
!
                endif
              endif
            endif
!

          enddo                ! end of the iteration loop  for a given l!
          if (l .le. k) then
            if (etd(l-1) .eq. 0.0                                       &
     &         .and. errq .gt. errmin*10.0 .and. l .le. kbl) then
!    &         .and. errq .gt. errmin*10.0) then
               ror(l)   = bud(kd)
               hod(l)   = wa(1)
               qod(l)   = wa(2)
!              tx5      =       tx9     ! does not make too much difference!
               tx5      = tx1 + tx9
               evp(l-1) = 0.0
!              evp(l-1) = cee * (1.0 - qod(l)/qa(3))
               qa(1)    = tx1*rnt + rnf(l-1)
               evp(l-1) = min(evp(l-1), qa(1))
               qa(1)    = qa(1) - evp(l-1)
!              qrp(l)   = 0.0
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0) then
!       print *,' ctx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' l=',l,' qa=',qa(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' del_eta=',del_eta
!     endif
!              if (qa(1) .gt. 0.0) then
                 qrp(l) = (qa(1) / (ror(l)*tx5*gms(l)))                  &
     &                                         ** (1.0/1.1364)
!              endif
               etd(l)   = 0.0
               wvl(l)   = 0.0
               st1      = 1.0 - alfint(l)

               errq     = 0.0
               buy(l)   = - ror(l) * tx5 * qrp(l)
               wcb(l-1) = 0.0
            endif
          endif

!
!
          ll = min(idn(idnm), k+1)
          if (errq .lt. 1.0 .and. l .le. ll) then
            if (etd(l-1) .gt. 0.0 .and. etd(l) .eq. 0.0) then
             idn(idnm) = l
             wvl(l)    = 0.0
             if (l .lt. kbl .or. tx5 .gt. 0.0) idnm  = idnm + 1
             errq      = 0.0
            endif
            if (etd(l) .eq. 0.0 .and. l .gt. kbl) then
              idn(idnm) = l
              if (tx5 .gt. 0.0) idnm  = idnm + 1
            endif
          endif

!       if (lprnt) then
!       print *,' errq=',errq,' idn=',idn(idnm),' idnm=',idnm
!       print *,' l=',l,' qrp=',qrp(l),' etd=',etd(l),' qa=',qa(1)
!    *,' evp=',evp(l-1),' rnf=',rnf(l-1)
!       endif

! 
!     if downdraft properties are not obtainable, (i.e.solution does
!      not converge) , no downdraft is assumed
!
           if (errq .gt. errmin*100.0 .and. idn(idnm) .eq. 99)          &
     &                          ddft = .false.
!
!
        dof = 0.0
        if (.not. ddft) return
!
!     if (ddlgk .or. l .le. idn(idnm)) then
!     rsum2 = rsum2 + evp(l-1)
!     print *,' rsum1=',rsum1,' rsum2=',rsum2,' l=',l,' qa=',qa(1)
!    *,' evp=',evp(l-1)
!     else
!     rsum1 = rsum1 + rnf(l-1)
!     print *,' rsum1=',rsum1,' rsum2=',rsum2,' l=',l,' rnf=',rnf(l-1)
!     endif

      enddo                      ! end of the l loop of downdraft !

        tx1 = 0.0

        dof = qa(1)
!
!     print *,' dof=',dof,' rntp=',rntp,' rnb=',rnb
!     print *,' total=',(rsum1+dof+rntp+rnb)

      endif                       ! skpdd endif
!

       rnn(kd) = rntp
       tx1     = evp(kd)
       tx2     = rntp + rnb + dof

!     print *,' tx2=',tx2
       ii = idh
       if (ii .ge. kd1+1) then
          rnn(kd)   = rnn(kd) + rnf(kd)
          tx2       = tx2 + rnf(kd)
          rnn(ii-1) = 0.0
          tx1       = evp(ii-1)
        endif
!     print *,' tx2=',tx2
        do l=kd,k
          ii = idh

          if (l .gt. kd1 .and. l .lt. ii) then
            rnn(l-1) = rnf(l-1)
            tx2      = tx2 + rnn(l-1)

!         elseif (l .ge. ii .and. l .lt. idn(idnm)) then
          elseif (l .ge. ii .and. l .le. idn(idnm)) then

!           do jj=2,idnm
!             if (l .ge. idn(jj-1) .and. l .lt. idn(jj)) then
                rnn(l)   = 0.0
                tx1      = tx1 + evp(l)
!             endif
!           enddo

          elseif (l .gt. idn(idnm)) then
            etd(l)   = 0.0
            hod(l)   = 0.0
            qod(l)   = 0.0
            evp(l-1) = 0.0
            rnn(l-1) = rnf(l-1)
            tx2      = tx2 + rnn(l-1)
          endif
!     print *,' tx2=',tx2,' l=',l,' rnn=',rnn(l-1)
        enddo
        if (k+1 .gt. idn(idnm)) then
          etd(k+1) = 0.0
          hod(k+1) = 0.0
          qod(k+1) = 0.0
          evp(k)   = 0.0
          rnn(k)   = rnf(k)
          tx2      = tx2 + rnn(k)
        endif
!
!      for downdraft case the rain is that falls thru the bottom

        l = kbl

        rnn(l)  = rnb
        cldfrd  = tx5

!
!     caution !! below is an adjustment to rain flux to maintain
!                conservation of precip!

!
!     if (lprnt) print *,' train=',train,' tx2=',tx2,' tx1=',tx1

        if (tx1 .gt. 0.0) then
          tx1 = (train - tx2) / tx1
        else
          tx1 = 0.0
        endif

        tx5      = evp(kbl)

!       evp(kbl) = evp(kbl) * tx1

        tx3      = rnn(kbl) + evp(kbl) + dof
        tx2      = rnn(kbl)
        tx4      = evp(kbl)

!       do l=kd,kb1
        do l=kd,k

          tx5    = tx5 + evp(l)
          evp(l) = evp(l) * tx1
          tx3    = tx3 + evp(l) + rnn(l)
          tx2    = tx2 + rnn(l)
          tx4    = tx4 + evp(l)
        enddo
!
!***********************************************************************
!***********************************************************************

      return
      end
