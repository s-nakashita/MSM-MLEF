      subroutine cloud(                                                 &
     &                  k, kd, m                                        &
     &,                 rasalf, fracbl, max_neg_bouy                    &
     &,                 alfint, alfinq, rhfacl, rhfacs, garea           &
     &,                 toi, qoi, roi, prs, prsm, phil, phih            &
     &,                 qli, qii, kpbl, dsfc                            &
     &,                 cd,lprnt                                        &
     &,                 tcu, qcu, rcu, pcu, flx                         &
     &,                 cup, revap, dt                                  &
     &,                 wfnc, wrkfun, calkbl, crtfun, tla, dndrft, pdd)  

!
!***********************************************************************
!******************** relaxed  arakawa-schubert ************************
!****************** plug compatible scalar version *********************
!************************ subroutine cloud  ****************************
!************************  23 june 1999     ****************************
!**************************  version 2.0  ******************************
!*******************  s. moorthi and m.j. suarez ***********************
!***********************************************************************
!*reference:
!-----------
!     noaa technical report nws/ncep 99-01:
!     documentation of version 2 of relaxed-arakawa-schubert
!     cumulus parameterization with convective downdrafts, june 1999.
!
!***********************************************************************
!
!===>    updates cloud tendencies due to a single cloud
!===>    detraining at level kd.
!
!***********************************************************************
!************* shrinivas.moorthi@noaa.gov (301) 763 8000(x7233) ********
!***************  max.suarez@gsfc.nasa.gov (301) 286 7373 **************
!***********************************************************************
!***********************************************************************
!23456789012345678901234567890123456789012345678901234567890123456789012
!
!===>  toi(k)     inout   temperature             kelvin
!===>  qoi(k)     inout   specific humidity       non-dimensional
!===>  roi(k,m)   inout   tracer                  arbitrary
!===>  qli(k)     inout   liquid water            non-dimensional
!===>  qii(k)     inout   ice                     non-dimensional

!===>  prs(k+1)   input   pressure @ edges        mb
!===>  prsm(k)    input   pressure @ layers       mb
!===>  phih(k+1)  input   geopotential @ edges  in mks units
!===>  phil(k)    input   geopotential @ layers in mks units
!===>  prj(k+1)   input   (p/p0)^kappa  @ edges   non-dimensional
!===>  prjm(k)    input   (p/p0)^kappa  @ layers  non-dimensional

!===>  k      input   the rise & the index of the subcloud layer
!===>  kd     input   detrainment level ( 1<= kd < k )          
!===>  m      input   number of tracers. may be zero.
!===>  dndrft input   logical .true. or .false.
!===>  pdd    input   pressure level above which downdrfat can exist hpa
!
!===>  tcu(k  )   update  temperature tendency       deg
!===>  qcu(k  )   update  water vapor tendency       (g/g)
!===>  rcu(k,m)   update  tracer tendencies          nd
!===>  pcu(k-1)   update  precip @ base of layer     kg/m^2
!===>  flx(k  )   update  mass flux @ top of layer   kg/m^2
!===>  cup        update  precipitation at the surface kg/m^2
!
      use machine , only : kind_phys
      use module_ras
      implicit none
!
!  input arguments

      logical revap, dndrft, wrkfun, calkbl, crtfun, calcup
      logical lprnt
      integer k, kd, m


      real(kind=kind_phys) toi(k),    qoi(k ),  prs(k+1), prsm(k)       &
     &,                    qli(k),    qii(k)                            &
     &,                    phih(k+1), roi(k,m), phil(k)           
      real(kind=kind_phys) cd,        ufn,     dsfc
      integer kpbl,   kbl,     kb1

      real(kind=kind_phys) rasalf, fracbl, max_neg_bouy, alfint(k),     &
     &                     rhfacl, rhfacs, garea
      real(kind=kind_phys) alfinq(k), pdd
 
!  update arguments

      real(kind=kind_phys) tcu(k),  qcu(k), rcu(k,m)                    &
     &,                    tcd(k),  qcd(k), pcu(k), flx(k), cup

!  temporary work space

      real(kind=kind_phys) hol(kd:k),  qol(kd:k),   gaf(kd:k+1)         &
     &,                    hst(kd:k),  qst(kd:k),   tol(kd:k)           &
     &,                    gmh(kd:k),  gms(kd:k+1), gam(kd:k+1)         &
     &,                    akt(kd:k),  akc(kd:k),   bkc(kd:k)           &
     &,                    ltl(kd:k),  rnn(kd:k),   fco(kd:k)           &
     &,                                pri(kd:k)                        &
     &,                    qil(kd:k),  qll(kd:k)                        &
     &,                    zet(kd:k),  xi(kd:k),    rns(kd:k)           &
     &,                    q0u(kd:k),  q0d(kd:k),   vtf(kd:k)           &
     &,                    dlb(kd:k+1),dlt(kd:k+1), eta(kd:k+1)         &
     &,                    prl(kd:k+1)

      real(kind=kind_phys) alm,   det,    hcc,  clp                     &
     &,                    hsu,   hsd,    qtl,  qtv                     &
     &,                    akm,   wfn,    hos,  qos                     &
     &,                    amb,   tx1,    tx2,  tx3                     &
     &,                    tx4,   tx5,    qis,  qls                     &
     &,                    hbl,   qbl,    rbl(m)                        &
     &,                    qlb,   qib,    pris                          &
     &,                    wfnc,  tx6,    acr                           &
     &,                    tx7,   tx8,    tx9,  rhc                     &
     &,                    hstkd, qstkd,  ltlkd, q0ukd, q0dkd, dlbkd    &
     &,                    qtp, qw00, qi00                              &
     &,                    hstold

!     integer ia,  i1,  i2, id1, id2
!     integer ib,  i3

      logical unsat, ep_wfn

      logical lowest, skpdd

      real(kind=kind_phys) tl, pl, ql, qs, dqs, st1, sgn, tau,          &
     &                     qtvp, hb, qb, tb, qqq, st2,                  &
     &                     hccp, ds, dh, ambmax, x00, epp, qtlp,        &
     &                     dpi, dphib, dphit, del_eta, detp,            &
     &                     tem, tem1, tem2, tem3, tem4,                 &
     &                     errmin, errmi2, errh, errw, erre, tem5,      &
     &                     tem6, hbd, qbd, st1s
      parameter (errmin=0.0001, errmi2=0.1*errmin)
      integer i, l,  n,  kd1, ii                                        &
     &,       kp1, it, km1, ktem, kk, kk1, lm1, ll, lp1, kbls, kmxh

!     real avt, avq, avr, avh
!
!     reevaporation
!
!     real(kind=kind_phys), parameter ::
!    &                   clfa = -0.452550814376093547e-03
!    &,                  clfb =  0.161398573159240791e-01
!    &,                  clfc = -0.163676268676807096
!    &,                  clfd =  0.447988962175259131
!    &,                  point3 = 0.3, point01=0.01

      real(kind=kind_phys), parameter :: rainmin=1.0e-9
      real(kind=kind_phys), parameter :: oneopt9=1.0/0.09
      real(kind=kind_phys), parameter :: oneopt4=1.0/0.04

      real(kind=kind_phys) clfrac, dt, clf, clvfr

      real(kind=kind_phys) actevap,arearat,deltaq,mass,massinv,potevap  &
     &,                    teq,qsteq,dqdt,qeq                 
!
!     temporary workspace and parameters needed for downdraft
!
      real(kind=kind_phys) tla, gmf
!
      real(kind=kind_phys) buy(kd:k+1), qrb(kd:k),   qrt(kd:k)          &
     &,                    etd(kd:k+1), hod(kd:k+1), qod(kd:k+1)        &
     &,                    ghd(kd:k),   gsd(kd:k),   evp(kd:k)          &
     &,                    etz(kd:k)                                    &
     &,                    train, dof, cldfrd                           &
     &,                    fac, rsum1, rsum2, rsum3, dpneg
      integer idh
      logical ddft, updret
!     real(kind=kind_phys) eps, epsm1, rvi, facw, faci, hsub, tmix, den
!     real(kind=kind_phys) eps, epsm1, rv, rd, depth
!     real(kind=kind_phys) eps, epsm1, rv, rd, fpvs, depth
!
!
!***********************************************************************
!
!fpp$ expand (qsatcn, crtwrk)
!fpp$ noconcur r
!
      do l=1,k
        tcd(l) = 0.0
        qcd(l) = 0.0
      enddo
!
      kp1     = k  + 1
      km1     = k  - 1
      kd1     = kd + 1
!
!     if (lprnt) print *,' prs=',prs(kd:k+1)
!     if (lprnt) print *,' phil=',phil(kd:k)
!     if (lprnt) print *,' phih=',phih(kd:k+1)
!     if (lprnt) print *,' toi=',toi
!     if (lprnt) print *,' qoi=',qoi
!
!     do l=kd1,k
!       alfint(l) = (prjm(l)-prj(l)) / (prjm(l)-prjm(l-1))
!       alfinq(l) = alfint(l)
!     enddo
!
!     dlt(kp1) = prj(kp1)
      cldfrd   = 0.0
      dof      = 0.0
      prl(kp1) = prs(kp1)
!
      do l=kd,k
        rnn(l) = 0.0
        zet(l) = 0.0
        xi(l)  = 0.0
!
!       dlt(l) = prj(l)
        tol(l) = toi(l)
        qol(l) = qoi(l)
        prl(l) = prs(l)
        buy(l) = 0.0
      enddo
!
      do l=kd, k
        dpi    = one / (prl(l+1) - prl(l))
        pri(l) = gravfac * dpi
!       pri(l) = gravfac * dpi(l)
!
!       prh(l) = (dlt(l+1)*prl(l+1) - dlt(l)*prl(l))*dpi * rkpp1i
!       pl     = (prl(l+1) + prl(l))*half
!       pl     = 1000.0*(prh(l)) ** (1.0/rkap)
!       tl     = pol(l)*prh(l)
!
        pl     = prsm(l)
!       prh(l) = prjm(l)
!
        tl     = tol(l)
!     if (lprnt) print *,' l=',l,' prl=',prl(l+1),prl(l),' pl=',pl,
!    &' dpi=',dpi,' prsm=',prsm(l)

        akt(l) = (prl(l+1) - pl) * dpi
!       akt(l) = (prl(l+1) - pl) * dpi(l)
!       akt(l) = half
!     if (lprnt) print *,' l=',l,' prl=',prl(l+1),prl(l),' pl=',pl,
!    &' dpi=',dpi,' prsm=',prsm(l),' akt=',akt(l)
!
        call qsatcn(tl, pl, qs, dqs,lprnt)
!     if(lprnt)print*,' qs=',qs,' tl=',tl,' pl=',pl
!    1,               ' dqs=',dqs,' qol=',qol(l)
!
!       tx1  = 10.0 * fpvs(tl)                  ! fpvs is in centibars!
!       tx2  = 1.0 / max(pl + epsm1 * tx1, 1.0e-10)
!
!        qs  = min(eps*tx1*tx2, 1.0)
!        dqs = pl * qs * alhl *  tx2 / (tl*tl*rv)
!
        qst(l) = qs
        gam(l) = dqs * elocp
        st1    = one + gam(l)
!       gaf(l) = alhli * gam(l) / st1
        gaf(l) = (one/alhl) * (gam(l)/(one + gam(l)))
 
        ql     = max(min(qs*rhmax,qol(l)), one_m10)
!       ql     = max(min(qs*rhmax,qol(l)), 1.0e-10)
        qol(l) = ql
 
        tem    = cp * tl
        ltl(l) = tem * st1 / (one+nu*(qst(l)+tl*dqs))
!       ltl(l) = tem * (one+gam(l))/(one+nu*(qst(l)+tl*dqs))
        vtf(l) = 1.0 + nu * ql
        eta(l) = one / (ltl(l) * vtf(l))

        hol(l) = tem + ql * alhl - alhf * qii(l)
        hst(l) = tem + qs * alhl
!     if(lprnt)print*,' l=',l,' hst=',hst(l),' tem=',tem
!    1,               ' qs=',qs,' alhl=',alhl
!     if (lprnt) print *,' l=',l,' tem=',tem,' ql=',ql,' alhl=',alhl
!    &,' alhf=',alhf,' qii=',qii(l),' cp=',cp,' tl=',tl
!    &,' qs=',qs,' qol=',qol(l),' rhmax=',rhmax,' hol=',hol(l)
!    &,' pl=',pl
  
      enddo
!
      eta(k+1) = zero
      gms(k)   = zero
!
      akt(kd)  = half
      gms(kd)  = zero
!
      clp      = zero
!
      gam(k+1) = gam(k)
      gaf(k+1) = gaf(k)
!
      do l=k,kd1,-1
!       tem1   = cp * tol(l) * vtf(l) / prh(l)
 
!       dphib  = (prj(l+1) - prh(l)) * tem1
!       dphit  = (prh(l  ) - prj(l)) * tem1
!
        dphib  = phil(l) - phih(l+1)
        dphit  = phih(l) - phil(l)
!
        dlb(l) = dphib * eta(l)
        dlt(l) = dphit * eta(l)
!
        qrb(l) = dphib
        qrt(l) = dphit
!
        eta(l) = eta(l+1) + dphib

!     if (lprnt) print *,' l=',l,' dphib=',dphib,' dphit=',dphit
!    &,' eta=',eta(l),' hol_new=',hol(l)+eta(l)
!    &,' cp=',cp,' tol=',tol(l),' vtf=',vtf(l)
        hol(l) = hol(l) + eta(l)
        hstold=hst(l)
        hst(l) = hst(l) + eta(l)
!     if(lprnt)print*,' l=',l,' hst=',hst(l),' eta=',eta(l)
!    1,               ' hstold=',hstold

        eta(l) = eta(l) + dphit
      enddo
!
!     for the cloud top layer
!
      l = kd
!     tem    = cp * tol(l) * (prj(l+1) - prh(l)) / prh(l)
!     dphib  = tem * vtf(l)

      dphib  = phil(l) - phih(l+1)
!
      dlb(l) = dphib * eta(l)
!
      qrb(l) = dphib
      qrt(l) = dphib
!
      eta(l) = eta(l+1) + dphib

      hol(l) = hol(l) + eta(l)
      hst(l) = hst(l) + eta(l)
!
!     if (lprnt) print *,' in cloud for kd=',kd,' k=',k
!     if (lprnt) print *,' l=',l,' hol=',hol(l),' hst=',hst(l)
!     if (lprnt) print *,' tol=',tol
!     if (lprnt) print *,' qol=',qol
!     if (lprnt) print *,' hol=',hol
!     if (lprnt) print *,' hst=',hst
!
!     to determine kbl internally -- if kbl is defined externally
!     the following two loop should be skipped
!
!     if (lprnt) print *,' calkbl=',calkbl
      if (calkbl) then
         ktem = max(kd, k-kblmx-2)
         kmxh = k
!        do l=km1,ktem,-1
!     if(lprnt) print *,' l=',l,' kmxh=',kmxh,' prl=',prl(l)
!    &, prl(k),' hol=',hol(l),hol(kmxh)
!          if (prl(k) - prl(l) .gt. 100.0) exit
!          if (hol(l) .gt. hol(kmxh)) kmxh = l
!     if(lprnt) print *,' l=',l,' kmxh=',kmxh,' prl=',prl(l)
!        enddo
         do l=kmxh,ktem+1,-1
           kbls = l
           if (hst(l-1) .gt. hst(l)) exit
         enddo
         kbl   = kmxh
         tx1   = zero
         unsat = .false.
         do l=kmxh-1,ktem,-1
           tem = hol(k) - hol(l)
           tx3 = (hol(l) - hol(l+1)) / (prl(l+2) - prl(l))

!     if (lprnt) print *,' l=',l,' kbl=',kbl,' tx3=',tx3,' tx1=',tx1
           if (tx3 .lt. tx1 .and. tem .lt. hcrit) then
             tx1   = tx3
             kbl   = l
!            kbl   = l+1
             unsat = .true.
           elseif (unsat .and.                                          &
     &           ( ((kbl .lt. k-1) .and. tx3 .gt. 0.5*tx1)              &
     &              .or. tem .gt. hcrit) ) then
             tx1 = -1.0e20
           endif
         enddo
!     if(lprnt) print *,' kbl=',kbl,' kbls=',kbls,' kmxh=',kmxh
!
!        ii = min(kbl,kbls)
         ii = kbl
         do l=ktem,kmxh-1
           if (hol(kmxh) .gt. hst(l)) kbl = l+1
         enddo
!        if(lprnt) print *,' kblhst=',kbl,' ii=',ii

         if (prl(k+1) - prl(ii) .gt. 50.0 .and. ii .gt. kbl) kbl = ii
!        if(lprnt) print *,' kbl2=',kbl,' ii=',ii
         if (kbl .ne. ii) then
!          kbl = min(k, max(kbl+1, kd-1))
!!!        kbl = min(k, max(kbl, kd-1))
           if (prl(k+1)-prl(kbl) .gt. bldmax) kbl = max(kbl,ii)
         endif
!        if (ii .gt. kbl) then
!           if (hol(kmxh)-hol(kbl) .gt. hcrit) kbl = ii
!        endif
!
!        ii = kbl
!        do l=ii,k
!          if (hol(k) .gt. hst(l)) kbl = l
!        enddo
!!!      kbl = min(k, max(kbl, kd-1))
!
         kbl  = min(k, max(kbl,k-kblmx))
!!!!!    kbl = k - 2

         kpbl = kbl
!     if(lprnt)print*,' 1st kbl=',kbl,' kblmx=',kblmx,' kd=',kd
!     if(lprnt)print*,' tx3=',tx3,' tx1=',tx1,' tem=',tem
!    1,               ' hcrit=',hcrit
      else
         kbl  = kpbl
!     if(lprnt)print*,' 2nd kbl=',kbl
      endif
!     if(lprnt)print*,' after calkbl l=',l,' hol=',hol(l)
!    1,               ' hst=',hst(l)
!
      kbl      = max(kbl,kd)
      kb1      = kbl - 1
!!
!!
!     do kbl=k,kd1,-1
!       st1  = 1.0 / (prl(k+1) - prl(kbl))
!       tem1 = (prl(k+1)-prl(k)) * st1
!       hbl = hol(k) * tem1
!
!       do l=km1,kbl,-1
!         tem2 = (prl(k+1)-prl(l)) * st1
!         tem  = tem2 - tem1
!         hbl  = hbl + hol(l) * tem
!         tem1 = tem2
!       enddo
!     if(lprnt) print *,' hbl=',hbl,' kbl=',kbl
!       kb1 = kbl - 1
!       st2 = 0.5 * (hst(kbl)+hst(kb1))
!       if (st2 .le. hbl) exit
!     enddo
!     if (hst(kbl) .le. hbl) kbl = kbl + 1
!     kbl = min(kbl+1, k)
!     kb1 = kbl - 1
!!    if (lprnt) print *,' hbl=',hbl,' hst=',st2,' kbl=',kbl,' kb1=',kb1
!     if(lprnt)print *,' hbl=',hbl,' hst=',hst(l),' kbl=',kbl
!    1,                ' kb1=',kb1
!     if (prl(k+1)-prl(kbl) .gt. bldmax) return
!!
!!
!
!     if (lprnt) print *,' kbl=',kbl
      if(kb1 .le. kd)then
        if(lprnt)print*,' kb1=',kb1,' kd=',kd,' exit cloud'
        return
      endif
      if(prl(k+1)-prl(kbl) .gt. bldmax)then
        if(lprnt)print*,' prl(k+1)=',prl(k+1),' prl(kbl)=',prl(kbl)         &
     &,               ' bldmax=',bldmax,' k+1=',k+1,' kbl=',kbl             &
     &,               ' exit cloud'
        return
      endif
!
!     if (lprnt) print *,' kbl=',kbl
!
      pris     = one / (prl(k+1)-prl(kbl))
      tx1      = eta(kbl)
!
      gms(kbl) = 0.0
      xi(kbl)  = 0.0
      zet(kbl) = 0.0
!     depth    = eta(kd) - eta(kbl)
!
      do l=k,kd,-1
        if (l .ge. kbl) then
          eta(l) = (prl(k+1)-prl(l)) * pris
        else
          zet(l) = (eta(l) - tx1) * onebg
          xi(l)  =  zet(l) * zet(l) * qudfac
          eta(l) =  zet(l) - zet(l+1)
          gms(l) =  xi(l)  - xi(l+1)
        endif
      enddo
!
      hbl = hol(k) * eta(k)
      qbl = qol(k) * eta(k)
      qlb = qli(k) * eta(k)
      qib = qii(k) * eta(k)
!     tx1 = qol(k) / qst(k) * eta(k)
      tx1 = qst(k) * eta(k)
!
      do l=km1,kbl,-1
         tem = eta(l) - eta(l+1)
         hbl = hbl + hol(l) * tem
!     if(lprnt)print*,' l=',l,' qbl=',qbl,' qol=',qol(l)
!    1,               ' tem=',tem
         qbl = qbl + qol(l) * tem
         qlb = qlb + qli(l) * tem
         qib = qib + qii(l) * tem
!        tx1 = tx1 + qol(l) / qst(l) * tem
         tx1 = tx1 + qst(l) * tem
      enddo
!     if (lprnt) print *,' hbl=',hbl,' qbl=',qbl
!                                   find min value of hol in tx2
      tx2 = hol(kd)
      idh = kd1
      do l=kd1,kb1
        if (hol(l) .lt. tx2) then
           tx2 = hol(l)
           idh = l             ! level of minimum moist static energy!
        endif
      enddo
      idh = 1
      idh = max(kd1, idh)
!
      tem1 = hbl - hol(kd)
      tem  = hbl - hst(kd1)                                             &
     &             - ltl(kd1) *( nu *(qol(kd1)-qst(kd1)))
      lowest = kd .eq. kb1
!

!     tx1   = qbl / tx1
      tx1   = rhfacs - qbl / tx1       !     average rh
!     tx1   = rhfacs - tx1             !     average of each layer rh
      unsat = (tem .gt. zero .or. (lowest .and. tem1 .ge. zero))        &
     &         .and. (tx1 .lt. rhram)                                   &
     &         .and. (kbl .gt. kd)
!     if(lprnt) print *,' unsat=',unsat,' tem=',tem,' tem1=',tem1
!    &,' tx1=',tx1,' rhram=',rhram,' kbl=',kbl,' kd=',kd,' lowest='
!    &,lowest,' rhfacs=',rhfacs,' ltl=',ltl(kd1),' qol=',qol(kd1)
!    &,' qst=',qst(kd1),' hst=',hst(kd1),' nu=',nu

!
!===>  if no sounding meets first condition, return
!     if(lprnt .and. (.not. unsat)) print *,' tx1=',tx1,' rhfacs='
!    &,rhfacs, ' tem=',tem,' hst=',hst(kd1)

      if (.not. unsat) return
!
!     tem1   = tx1 - rhfacs
!     rhc    = max(zero, min(one, exp(20.0*tem1) ))
      rhc    = max(zero, min(one, exp(-20.0*tx1) ))
!
      do n=1,m
        rbl(n) = roi(k,n) * eta(k)
      enddo
      do n=1,m
        do l=km1,kbl,-1
          rbl(n) = rbl(n) + roi(l,n)*(eta(l)-eta(l+1))
        enddo
      enddo
!
      tx4    = 0.0
      tx5    = 0.0
!
      tx3      = qst(kbl) - gaf(kbl) * hst(kbl)
      qil(kbl) = max(zero, min(one, (tcr-tcl-tol(kbl))*tcrf))
!     qil(kbl) = max(0.0, min(1.0, (tcr-tol(kbl))*tcrf))
!
      do l=kb1,kd1,-1
        tem      = qst(l) - gaf(l) * hst(l)
        tem1     = (tx3 + tem) * 0.5
        st2      = (gaf(l)+gaf(l+1)) * 0.5
!
        fco(l+1) =            tem1 + st2 * hbl
!     if(lprnt) print *,' fco=',fco(l+1),' tem1=',tem1,' st2=',st2
!    &,' hbl=',hbl,' tx3=',tx3,' tem=',tem,' gaf=',gaf(l),' l=',l
        rnn(l+1) = zet(l+1) * tem1 + st2 * tx4
        gmh(l+1) = xi(l+1)  * tem1 + st2 * tx5
!
        tx3      = tem
        tx4      = tx4 + eta(l) * hol(l)
        tx5      = tx5 + gms(l) * hol(l)
!
        qil(l)   = max(zero, min(one, (tcr-tcl-tol(l))*tcrf))
!       qil(l)   = max(0.0, min(1.0, (tcr-tol(l))*tcrf))
        qll(l+1) = (0.5*alhf) * st2 * (qil(l)+qil(l+1)) + one
      enddo
!
!     for the cloud top -- l=kd
!
      l = kd
!
      tem      = qst(l) - gaf(l) * hst(l)
      tem1     = (tx3 + tem) * 0.5
      st2      = (gaf(l)+gaf(l+1)) * 0.5
!
      fco(l+1) =            tem1 + st2 * hbl
      rnn(l+1) = zet(l+1) * tem1 + st2 * tx4
      gmh(l+1) = xi(l+1)  * tem1 + st2 * tx5
!
      fco(l)   = tem + gaf(l) * hbl
      rnn(l)   = tem * zet(l) + (tx4 + eta(l)*hol(l)) * gaf(l)
      gmh(l)   = tem * xi(l)  + (tx5 + gms(l)*hol(l)) * gaf(l)
!
!   replace fco for the bottom
!
      fco(kbl) = qbl
      qis      = qii(kd)
      qls      = qli(kd)
      qil(kd)  =  max(zero, min(one, (tcr-tcl-tol(kd))*tcrf))
!     qil(kd)  =  max(0.0, min(1.0, (tcr-tol(kd))*tcrf))
      qll(kd1) = (0.5*alhf) * st2 * (qil(kd) + qil(kd1)) + one
      qll(kd ) = alhf * gaf(kd) * qil(kd) + one
!
!     if (lprnt) print *,' fco=',fco(kd:kbl)
!     if (lprnt) print *,' qil=',qil(kd:kbl)
!     if (lprnt) print *,' qll=',qll(kd:kbl)
!
      st1  = qil(kd)
      st2  = c0i * st1
      tem  = c0  * (1.0-st1)
      tem2 = st2*qi0 + tem*qw0
!
      do l=kd,kb1
         tx2    = akt(l) * eta(l)
         tx1    = tx2 * tem2
         q0u(l) = tx1
         fco(l) = fco(l+1) - fco(l) + tx1
         rnn(l) = rnn(l+1) - rnn(l) + eta(l)*qol(l) + tx1*zet(l)
         gmh(l) = gmh(l+1) - gmh(l) + gms(l)*qol(l) + tx1*xi(l)
!
!        tem    = c0 * eta(l)
!        tem    = (c0*(1.0-qil(l)) + c0i*qil(l)) * eta(l)
!        bkc(l) = qll(l+1) - tem * (1.0-akt(l))
!        akt(l) = qll(l)   + tem * akt(l)
!
!        tx1    = st2    + tem
         tem1   = (1.0-akt(l)) * eta(l)
!        bkc(l) = qll(l+1) - tx1 * tem1
!        akt(l) = qll(l)   + tx1 * tx2
!     if(lprnt) print *,' qll=',qll(l),' st2=',st2,' tem=',tem
!    &,' tx2=',tx2,' akt=',akt(l),' eta=',eta(l)
         akt(l) = qll(l)   + (st2 + tem) * tx2
!     if(lprnt) print *,' akt==',akt(l),' l==',l
         akc(l) = 1.0 / akt(l)
!
         st1    = 0.5 * (qil(l)+qil(l+1))
         st2    = c0i * st1
         tem    = c0  * (1.0-st1)
         tem2   = st2*qi0 + tem*qw0
!
         bkc(l) = qll(l+1) - (st2 + tem) * tem1
!
         tx1    = tem1*tem2
         q0d(l) = tx1
         fco(l) = fco(l) + tx1
         rnn(l) = rnn(l) + tx1*zet(l+1)
         gmh(l) = gmh(l) + tx1*xi(l+1)
      enddo
!     if(lprnt) print *,' akt=',akt(kd:kb1)
!     if(lprnt) print *,' akc=',akc(kd:kb1)

      qw00 = qw0
      qi00 = qi0
      ii = 0
  777 continue
!     if (lprnt) print *,' after 777 ii=',ii,' ep_wfn=',ep_wfn
      ep_wfn = .false.
      rnn(kbl) = 0.0
      tx3      = 0.0
      tx4      = 0.0
      tx5      = 0.0
      do l=kb1,kd1,-1
        tem    = bkc(l-1)       * akc(l)
!     if (lprnt) print *,' tx3=',tx3,' fco=',fco(l),' akc=',akc(l)
!    &,' bkc=',bkc(l-1), ' l=',l
        tx3    = (tx3 + fco(l)) * tem
        tx4    = (tx4 + rnn(l)) * tem
        tx5    = (tx5 + gmh(l)) * tem
      enddo
      if (kd .lt. kb1) then
!!       st1   = tx3 / bkc(kd)
         hsd   = hst(kd1)                                               &
     &         + ltl(kd1) *  nu *(qol(kd1)-qst(kd1))
!!   &         + ltl(kd1) * (nu *(qol(kd1)-qst(kd1))
!!   &                  +  st1 * (1.0 + nu*qol(kd1)) )
!!   &         - (0.5*alhf) * st1 * (qil(kd1)+qil(kd))
      else
         hsd   = hbl
      endif
!
!     if (lprnt) print *,' tx3=',tx3,' fco=',fco(kd),' akc=',akc(kd)
      tx3 = (tx3 + fco(kd)) * akc(kd)
      tx4 = (tx4 + rnn(kd)) * akc(kd)
      tx5 = (tx5 + gmh(kd)) * akc(kd)
      alm = alhf*qil(kd) - ltl(kd) * vtf(kd)
!
      hsu = hst(kd) + ltl(kd) * nu * (qol(kd)-qst(kd))
!     if (lprnt) print *,' hsu=',hsu,' hst=',hst(kd),
!    &' ltl=',ltl(kd),' qol=',qol(kd),' qst=',qst(kd)
!
!===> vertical integrals needed to compute the entrainment parameter
!
      tx1 = alm * tx4
      tx2 = alm * tx5

      do l=kd,kb1
        tau = hol(l) - hsu
        tx1 = tx1 + tau * eta(l)
        tx2 = tx2 + tau * gms(l)
      enddo
!
!     modify hsu to include cloud liquid water and ice terms
!
!     if (lprnt) print *,' hsu=',hsu,' alm=',alm,' tx3=',tx3
      hsu   = hsu - alm * tx3
!
      clp   = zero
      alm   = -100.0
      hos   = hol(kd)
      qos   = qol(kd)
      unsat = hbl .gt. hsu .and. abs(tx1) .gt. 1.0e-4
!     if (lprnt) print *,' ii=',ii,' unsat=',unsat,' hsu=',hsu
!    &,' hbl=',hbl,' tx1=',tx1,' hsd=',hsd


!***********************************************************************


       st1  = half*(hsu + hsd)
       if (unsat) then
!
!  standard case:
!   cloud can be neutrally bouyant at middle of level kd w/ +ve lambda.
!   epp < .25 is required to have real roots.
!
       clp = 1.0
       st2 = hbl - hsu
!     if(lprnt) print *,' tx2=',tx2,' tx1=',tx1,' st2=',st2
!
       if (tx2 .eq. 0.0) then
         alm = - st2 / tx1
         if (alm .gt. almax) alm = -100.0
       else
         x00 = tx2 + tx2
         epp = tx1 * tx1 - (x00+x00)*st2
         if (epp .gt. 0.0) then
           x00  = 1.0 / x00
           tem  = sqrt(epp)
           tem1 = (-tx1-tem)*x00
           tem2 = (-tx1+tem)*x00
           if (tem1 .gt. almax) tem1 = -100.0
           if (tem2 .gt. almax) tem2 = -100.0
           alm  = max(tem1,tem2)
!     if (lprnt) print *,' tem1=',tem1,' tem2=',tem2,' alm=',alm
!    &,' tx1=',tx1,' tem=',tem,' epp=',epp,' x00=',x00,' st2=',st2
         endif
       endif
!     if (lprnt) print *,' almf=',alm,' ii=',ii,' qw00=',qw00
!    &,' qi00=',qi00
!
!  clip case:
!   non-entrainig cloud detrains in lower half of top layer.
!   no clouds are allowed to detrain below the top layer.
!
       elseif ( (hbl .le. hsu) .and.                                    &
     &          (hbl .gt. st1   )     ) then
         alm = zero
         clp = (hbl-st1) / (hsu-st1)
       endif
!
      unsat = .true.
      if (almin1 .gt. 0.0) then
        if (alm .ge. almin1) unsat = .false.
      else
        lowest   = kd .eq. kb1
        if ( (alm .gt. zero) .or.                                       &
     &      (.not. lowest .and. alm .eq. zero) ) unsat = .false.
      endif
!
!     if (alm*depth/grav .ge. 1.0) unsat = .true.
!
!===>  if no sounding meets second condition, return
!
      if (unsat) then
         if (ii .gt. 0 .or. (qw00 .eq. 0.0 .and. qi00 .eq. 0.0)) return
         clp = 1.0
         ep_wfn = .true.
         go to 888
      endif
!
!     if (lprnt) print *,' hstkd=',hst(kd),' qstkd=',qst(kd)
!    &,' ii=',ii,' clp=',clp
      if(clp.gt.zero .and. clp.lt.one) then
        st1     = half*(one+clp)
        st2     = one - st1
        st1s    = st1
        hstkd   = hst(kd)
        qstkd   = qst(kd)
        ltlkd   = ltl(kd)
        q0ukd   = q0u(kd)
        q0dkd   = q0d(kd)
        dlbkd   = dlb(kd)
!
        hst(kd) = hst(kd)*st1 + hst(kd1)*st2
        hos     = hol(kd)*st1 + hol(kd1)*st2
        qst(kd) = qst(kd)*st1 + qst(kd1)*st2
        qos     = qol(kd)*st1 + qol(kd1)*st2
        ltl(kd) = ltl(kd)*st1 + ltl(kd1)*st2
!
        dlb(kd) = dlb(kd)*clp
        eta(kd) = eta(kd)*clp
        gms(kd) = gms(kd)*clp
        q0u(kd) = q0u(kd)*clp
        q0d(kd) = q0d(kd)*clp
      endif
!
!     if (almin2 .ne. 0.0) then
!       st1 = 0.0
!       if (almin1 .ne. almin2) st1 = 1.0 / max(1.0e-10,(almin2-almin1))
!       if (almin1 .ne. almin2) st1 = 1.0 / max(one_m10,(almin2-almin1))
!         if (alm .lt. almin2) then
!            clp = clp * (alm - almin1) * st1
!            clp = clp * (0.1 + 0.9*(alm - almin1) * st1)
!            clp = clp * max(0.0, min(1.0,(0.3 + 0.7*(alm-almin1)*st1)))
!            clp = clp * (1.0 + 0.0*(alm - almin1) * st1)
!         endif
!     endif
!
!
!***********************************************************************
!
!    critical workfunction is included in this version
!
      if (crtfun) then
        acr = 0.0
        tem = prl(kd1) - (prl(kd1)-prl(kd)) * clp * half
        call crtwrk(tem, st1)
!       acr = (prl(k) - tem) * st1
        acr = (prl(kbl) - tem) * st1
      else
        acr = 0.0
      endif
!
!===>  normalized massflux
!
!  eta is the thickness coming in and the mass flux going out.
!  gms is the thickness of the square; it is later reused for gamma_s
!
!     eta(k) = one

      do l=kb1,kd,-1
        eta(l) = eta(l+1) + alm * (eta(l) + alm * gms(l))
      enddo

!     if (lprnt) print *,' eta=',eta,' ii=',ii
!
!===>  cloud workfunction
!
      wfn   = zero
      akm   = zero
      det   = zero
      hcc   = hbl
      unsat = .false.
      qtl   = qst(kb1) - gaf(kb1)*hst(kb1)
      qtv   = qbl
      tx1   = hbl
!
      tem   = qst(kbl) - gaf(kbl)*hst(kbl)
      qtv   = 0.5 * ((tem+qtl) + (gaf(kbl)+gaf(kb1))*hbl)
      det   = max(zero, qbl-qtv)
!     det   = max(0.0, qbl-qtv)
      qtv   = qbl - det
!
      tx2   = 0.0
      dpneg = 0.0
!
      do l=kb1,kd1,-1
         del_eta = eta(l) - eta(l+1)
         hccp = hcc + del_eta*hol(l)
!
         qtlp = qst(l-1) - gaf(l-1)*hst(l-1)
         qtvp = 0.5 * ((qtlp+qtl)*eta(l)                                &
     &              + (gaf(l)+gaf(l-1))*hccp)
         st1  = eta(l)*q0u(l) + eta(l+1)*q0d(l)
         detp = (bkc(l)*det - (qtvp-qtv) + del_eta*qol(l) + st1)        &
     &                                                    * akc(l)
!     if(lprnt) print *,' detp=',detp,' bkc=',bkc(l),' det=',det
!    &,' qtvp=',qtvp,' qtv=',qtv,' del_eta=',del_eta,' qol='
!    &,qol(l),' st1=',st1,' akc=',akc(l)
!
         tem1   = akt(l)   - qll(l)
         tem2   = qll(l+1) - bkc(l)
         rns(l) = tem1*detp  + tem2*det - st1

         qtp    = 0.5 * (qil(l)+qil(l-1))
         tem2   = min(qtp*(detp-eta(l)*qw00),                           &
     &               (1.0-qtp)*(detp-eta(l)*qi00))
         st1    = min(tx2,tem2)
         tx2    = tem2
!
         if (rns(l) .lt. zero .or. st1 .lt. zero) ep_wfn = .true.
         if (detp .le. zero) unsat = .true.
!        if (detp .le. zero .or. rns(l) .lt. zero) unsat = .true.

         st1  = hst(l) - ltl(l)*nu*(qst(l)-qol(l))


         tem2 = hccp   + detp   * qtp * alhf
!        tem2 = hccp   + detp   * (qil(l-1)+qil(l)) * (alhf*0.5)
!
!     if(lprnt) print *,' hst=',hst(l),' ltl=',ltl(l),' nu=',nu
!    &,' qst=',qst(l),' qol=',qol(l),' hccp=',hccp,' detp=',detp
!    *,' qtp=',qtp,' alhf=',alhf,' vtf=',vtf(l)

         st2  = ltl(l) * vtf(l)
         tem5 = qli(l) + qii(l)
         tem3 = (tx1  - eta(l+1)*st1 - st2*(det-tem5*eta(l+1))) * dlb(l)
         tem4 = (tem2 - eta(l  )*st1 - st2*(detp-tem5*eta(l)))  * dlt(l)

!     if (lprnt) then
!       print *,' tem3=',tem3,' tx1=',tx1,' st1=',st1,' eta1=',eta(l+1)
!    &, ' st2=',st2,' det=',det,' tem5=',tem5,' dlb=',dlb(l)
!       print *,' tem4=',tem4,' tem2=',tem2,' detp=',detp
!    &, ' eta=',eta(l),' dlt=',dlt(l),' rns=',rns(l),' l=',l
!      endif

         st1  = tem3 + tem4

!     if (lprnt) print *,' wfn=',wfn,' st1=',st1,' l=',l,' ep_wfn=',
!    &ep_wfn,' akm=',akm
         wfn = wfn + st1       
         akm = akm - min(st1,zero)
!     if (lprnt) print *,' wfn=',wfn,' akm=',akm
         if (st1 .lt. zero .and. wfn .lt. zero) then
           dpneg = dpneg + prl(l+1) - prl(l)
         endif

!        buy(l) = eta(l+1)*tem3 + eta(l)*tem4
         buy(l) = 0.5 * (eta(l+1) + eta(l)) * st1
!
         hcc = hccp
         det = detp
         qtl = qtlp
         qtv = qtvp
         tx1 = tem2

      enddo

      del_eta = eta(kd) - eta(kd1)
      hccp    = hcc + del_eta*hos
!
      qtlp = qst(kd) - gaf(kd)*hst(kd)
      qtvp = qtlp*eta(kd) + gaf(kd)*hccp
      st1  = eta(kd)*q0u(kd) + eta(kd1)*q0d(kd)
      detp = (bkc(kd)*det - (qtvp-qtv) + del_eta*qos + st1)                 &
     &                                               * akc(kd)
!
      tem1    = akt(kd)  - qll(kd)
      tem2    = qll(kd1) - bkc(kd)
      rns(kd) = tem1*detp  + tem2*det - st1
!
      if (rns(kd) .lt. zero) ep_wfn = .true.
      if (detp.le.zero) unsat = .true.
!     if (detp.le.zero .or. rns(kd) .lt. zero) unsat = .true.
!
  888 continue
!     if (lprnt) print *,' ep_wfn=',ep_wfn,' ii=',ii,' rns=',rns(kd)
!    &,' clp=',clp,' hst(kd)=',hst(kd)
      if (ep_wfn) then
        if ((qw00 .eq. 0.0 .and. qi00 .eq. 0.0)) return
        if (ii .eq. 0) then
          ii  = 1
          if (clp .gt. 0.0 .and. clp .lt. 1.0) then
            hst(kd) = hstkd
            qst(kd) = qstkd
            ltl(kd) = ltlkd
            q0u(kd) = q0ukd
            q0d(kd) = q0dkd
            dlb(kd) = dlbkd
          endif
          do l=kd,kb1
            fco(l) = fco(l) - q0u(l) - q0d(l)
            rnn(l) = rnn(l) - q0u(l)*zet(l) - q0d(l)*zet(l+1)
            gmh(l) = gmh(l) - q0u(l)*xi(l)  - q0d(l)*zet(l+1)
            eta(l) = zet(l) - zet(l+1)
            gms(l) = xi(l)  - xi(l+1)
            q0u(l) = 0.0
            q0d(l) = 0.0
          enddo
          qw00 = 0.0
          qi00 = 0.0
!     if (lprnt) print *,' returning to 777 : ii=',ii,' qw00=',qw00,qi00
!    &,' clp=',clp,' hst(kd)=',hst(kd)
          go to 777
        else
          unsat = .true.
        endif
      endif
!
!
      st1 = hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
      st2 = ltl(kd)  * vtf(kd)
!     st2 = ltl(kd)  * (1.0+nu*qol(kd))
      tem5 = (qls + qis) * eta(kd1)
      st1  = half * (tx1-eta(kd1)*st1-st2*(det-tem5))*dlb(kd)
!
!     st1 = half * (hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
!    *    + hst(kd1) - ltl(kd1)*nu*(qst(kd1)-qol(kd1)))
!     st2 = half * (ltl(kd)  * (1.0+nu*qol(kd))
!     st2 = half * (ltl(kd)  * vtf(kd) +  ltl(kd1) * vtf(kd1))
!
!     if (lprnt) print *,' st1=',st1,' st2=',st2,' ltl=',ltl(kd)
!    *,ltl(kd1),' qos=',qos,qol(kd1)
 

!     st1 = half*(tx1-eta(kd1)*st1-st2*det)*dlb(kd)
!     tem5 = half * (qli(kd1)+qii(kd1)+qls+qis) * eta(kd1)
!     st1  = half * (tx1-eta(kd1)*st1-st2*(det-tem5))*dlb(kd)
!     if (lprnt) print *,' tx1=',tx1,' eta=',eta(kd1),' det=',det
!    *,' tem5=',tem5,' dlb=',dlb(kd)
!

      wfn = wfn + st1
      akm = akm - min(st1,zero)   ! commented on 08/26/02 - does not include top
!

!     buy(kd) = eta(kd1) * st1
      buy(kd) = 0.5 * (eta(kd1) + eta(kd)) * st1
!
!     if (lprnt) print *,' wfn=',wfn,' akm=',akm,' st1=',st1
!    &,' dpneg=',dpneg
      det = detp
      hcc = hccp
      akm = akm / wfn


!***********************************************************************
!
!     if only to calculate workfunction save it and return
!
      if (wrkfun) then
        if (wfn .ge. 0.0) wfnc = wfn
        return
      elseif (.not. crtfun) then
        acr = wfnc
      endif
!
!===>  third check based on cloud workfunction
!
      calcup = .false.

!     tem  =  min(cd*25.0, max_neg_bouy)
!     tem  =  min(cd*50.0, max_neg_bouy)
      tem  =  min(cd*100.0, max_neg_bouy)
      if (wfn .gt. acr .and.  (.not. unsat)                             &
     & .and. dpneg .lt. 200.0  .and. akm .le. tem) then
!    &                 .and. akm .le. tem) then
!
        calcup = .true.
      endif
!     if (lprnt) print *,' calcup=',calcup,' akm=',akm,' tem=',tem
!    *,' unsat=',unsat,' clp=',' rhc=',rhc
!
!===>  if no sounding meets third condition, return
!
      if (.not. calcup) return
!
      if (almin2 .ne. 0.0) then
        st1 = 0.0
!       if (almin1 .ne. almin2) st1 = 1.0 / max(1.0e-10,(almin2-almin1))
        if (almin1 .ne. almin2) st1 = 1.0 / max(one_m10,(almin2-almin1))
          if (alm .lt. almin2) then
!            clp = clp * (alm - almin1) * st1
!            clp = clp * (0.1 + 0.9*(alm - almin1) * st1)
             clp = clp * max(0.0, min(1.0,(0.3 + 0.7*(alm-almin1)*st1)))
!            clp = clp * (1.0 + 0.0*(alm - almin1) * st1)
          endif
      endif
!
!
      clp = clp * rhc
      do l=kd,kb1
        rnn(l) = rns(l)
      enddo
      do l=kbl,k 
        rnn(l) = 0.0 
      enddo
!
!     if downdraft is to be invoked, do preliminary check to see
!     if enough rain is available and then call ddrft.
!
      ddft = .false.
      if (dndrft) then
!
        train = 0.0
        if (clp .gt. 0.0) then
          do l=kd,kb1
            train = train + rnn(l)
          enddo
        endif

        pl = (prl(kd1) + prl(kd))*half
        tem = min(pdd, prl(kbl)-300.0)
        if (train .gt. 1.0e-4 .and. pl .le. tem) ddft  = .true.
!       if (train .gt. 1.0e-4 .and. pl .le. pdd) ddft  = .true.
!
        if (ddft) then
!         do l=kbl-3,kd,-1
          do l=kbl-3,kd1,-1
            if (buy(l) .lt. 0.1) then
              ddft = .false.
              exit
            endif
          enddo
        endif
!       do l=kd,km1
!         if (l .lt. kbl-2 .and. ddft) then
!           if (buy(l) .lt. 0.1) ddft = .false.
!         endif
!       enddo
      endif
!
      if (ddft) then
!
!     call downdraft scheme based on (cheng and arakawa, 1997)
!
        call ddrft(                                                     &
     &              k, kd                                               &
     &,             tla, alfint                                         &
     &,             tol, qol, hol,   prl, qst, hst, gam, gaf, hbl, qbl  &
     &,             qrb, qrt, buy,   kbl, idh, eta, rnn                 &
     &,             alm, wfn, train, ddft                               &
     &,             etd, hod, qod,   evp, dof, cldfrd, etz              &
     &,             gms, gsd, ghd,lprnt)               
!    &,             tx1, tx2, tx3, tx4, tx5, tx6, tx7, tx8, tx9)

      endif
!
!  no downdraft case (including case with no downdraft soln)
!  ---------------------------------------------------------
!
      if (.not. ddft) then
        do l=kd,k+1
          etd(l) = 0.0
          hod(l) = 0.0
          qod(l) = 0.0
        enddo
        do l=kd,k
          evp(l) = 0.0
          etz(l) = 0.0
        enddo

      endif
!
!
!===> calculate gammas  i.e. tendencies per unit cloud base massflux
!           includes downdraft terms!

!     avh = 0.0

!     tx1 = qib
!     tx2 = qlb
!     do l=kb1,kd,-1
!        del_eta = eta(l) - eta(l+1)
!        tx1     = tx1 + del_eta * qii(l)
!        tx2     = tx2 + del_eta * qli(l)
!     enddo
!
!     tem1 = max(0.0, min(1.0, (prl(kd)-400.0)*0.02))
!     tem1 = max(0.0, min(1.0, (prl(kd)-300.0)*0.02))
!     tem1 = max(0.0, min(1.0, (prl(kd)-400.0)*0.01))
!     tem1 = max(0.0, min(0.5, (prl(kd)-150.0)*0.002))
      tem1 = max(zero, min(half, (prl(kd)-four_p2)*one_m2))
!!!   tem1 = max(0.0, min(0.5, (prl(kd)-400.0)*0.01))
!     tem1 = max(0.0, min(0.25, (prl(kd)-400.0)*0.01))
!     tem1 = 0.0
!     if (kd1 .eq. kbl) tem1 = 0.0
!
      tem = det * qil(kd)
      st1 = (hcc+alhf*tem-eta(kd)*hst(kd)) / (1.0+gam(kd))
      ds  = eta(kd1) * (hos- hol(kd)                                    &
     &    - alhl*(qos - qol(kd)) + alhf*(qis-qii(kd)) )
      dh  = eta(kd1) * (hos- hol(kd))
!     gms(kd) = (ds + st1) * pri(kd)
!     gms(kd) = (ds + st1 - tem1*det*alhl) * pri(kd)
      gms(kd) = (ds + st1 - tem1*(det*alhl+tem*alhf)) * pri(kd)
      gmh(kd) = pri(kd) * (hcc-eta(kd)*hos + dh)
!     if (lprnt) print *,' gmhkd=',gmh(kd),' gmskd=',gms(kd)
!

!      tendency for suspended environmental ice and/or liquid water
!
      tem2    = 1.0 - tem1
!     qil(kd) =     (tem + eta(kd1)*(qis-qii(kd))
      qil(kd) =     (tem2*tem + eta(kd1)*(qis-qii(kd))                  &
     &                        - eta(kd)*qis ) * pri(kd)
!    &                   + tx1 - eta(kd)*qis ) * pri(kd)
!     qll(kd) = (det-tem + eta(kd1)*(qls-qli(kd))
      qll(kd) = (tem2*(det-tem) + eta(kd1)*(qls-qli(kd))                &
     &                          - eta(kd)*qls ) * pri(kd)
!    &                   + tx2 - eta(kd)*qls ) * pri(kd)
!
      ghd(kd) = 0.0
      gsd(kd) = 0.0
!
      do l=kd1,k
       st1 = one - alfint(l)
       st2 = one - alfinq(l)
       if (l .lt. kbl) then
         hb       = alfint(l)*hol(l-1) + st1*hol(l)
         qb       = alfint(l)*qol(l-1) + st1*qol(l)

         tem      = alfinq(l)*qii(l-1) + st2*qii(l)
         tem2     = alfinq(l)*qli(l-1) + st2*qli(l)
 
         tem1     = eta(l) * (tem - qii(l))
         tem3     = eta(l) * (tem2 - qli(l))

         tem5     = etd(l) * (hod(l) - hb)
         tem6     = etd(l) * (qod(l) - qb)
!
         dh       = eta(l) * (hb - hol(l)) + tem5
         ds       = dh - alhl * (eta(l) * (qb - qol(l)) + tem6)

         gmh(l)   = dh * pri(l)
         gms(l)   = ds * pri(l)
!     if (lprnt) print *,' gmh=',gmh(l),' gms=',gms(l)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l),' eta=',eta(l)
!    &,' hb=',hb,' hol=',hol(l),' l=',l
!
         ghd(l)   = tem5 * pri(l)
         gsd(l)   = (tem5 - alhl * tem6) * pri(l)
!
         qil(l)   = tem1 * pri(l)
         qll(l)   = tem3 * pri(l)

         tem1     = eta(l) * (qii(l-1) - tem)
         tem3     = eta(l) * (qli(l-1) - tem2)

         dh       = eta(l) * (hol(l-1) - hb) - tem5
         ds       = dh - alhl * eta(l) * (qol(l-1) - qb)                    &
     &                 + alhl * (tem6 - evp(l-1))

         gmh(l-1) = gmh(l-1) + dh * pri(l-1)
         gms(l-1) = gms(l-1) + ds * pri(l-1)
!
!     if (lprnt) print *,' gmh1=',gmh(l-1),' gms1=',gms(l-1)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l-1)
!    &,' hb=',hb,' hol=',hol(l-1)
!
         ghd(l-1) = ghd(l-1) - tem5 * pri(l-1)
         gsd(l-1) = gsd(l-1) - (tem5-alhl*(tem6-evp(l-1))) * pri(l-1)

         qil(l-1) = qil(l-1) + tem1 * pri(l-1)
         qll(l-1) = qll(l-1) + tem3 * pri(l-1)
       elseif (l .eq. kbl) then
!        hb       = alfint(l)*hol(l-1) + st1*hbl
!        qb       = alfint(l)*qol(l-1) + st1*qbl
         hb       = alfint(l)*hol(l-1) + st1*hol(l)
         qb       = alfint(l)*qol(l-1) + st1*qol(l)

!        hb       = hbl
!        qb       = qbl
         hbd      = alfint(l)*hol(l-1) + st1*hol(l)
         qbd      = alfint(l)*qol(l-1) + st1*qol(l)

         tem      = alfinq(l)*qii(l-1) + st2*qib
         tem2     = alfinq(l)*qli(l-1) + st2*qlb

         tem1     = eta(l) * (tem - qib)
         tem3     = eta(l) * (tem2 - qlb)

         tem5     =  etd(l) * (hod(l) - hbd)
         tem6     =  etd(l) * (qod(l) - qbd)

         tem4     = gravfac * pris
         tx1      = eta(l) * (hb - hbl) * tem4
         tx2      = tx1 - alhl * eta(l) * (qb - qbl) * tem4
         dh       = tem5

         ds       =  dh - alhl * (tem6 + evp(l))


         gmh(l)   = tx1 + dh * pri(l)
         gms(l)   = tx2 + ds * pri(l)
!
         ghd(l)   = tem5 * pri(l)
         gsd(l)   = (tem5 - alhl * (tem6+evp(l))) * pri(l)
!
         qil(l)   = tem1 * tem4
         qll(l)   = tem3 * tem4

         tem1     = eta(l) * (qii(l-1) - tem)
         tem3     = eta(l) * (qli(l-1) - tem2)

         dh       = eta(l) * (hol(l-1) - hb) - tem5
         ds       = dh - alhl * eta(l) * (qol(l-1) - qb)                    &
     &                 + alhl * (tem6 - evp(l-1))

         gmh(l-1) = gmh(l-1) + dh * pri(l-1)
         gms(l-1) = gms(l-1) + ds * pri(l-1)
!
         ghd(l-1) = ghd(l-1) - tem5 * pri(l-1)
         gsd(l-1) = gsd(l-1) - (tem5-alhl*(tem6-evp(l-1)))                   &
     &                                  * pri(l-1)

         qil(l-1) = qil(l-1) + tem1 * pri(l-1)
         qll(l-1) = qll(l-1) + tem3 * pri(l-1)
       else
         hbd      = alfint(l)*hol(l-1) + st1*hol(l)
         qbd      = alfint(l)*qol(l-1) + st1*qol(l)
         tem5     =  etd(l) * (hod(l) - hbd)
         tem6     =  etd(l) * (qod(l) - qbd)
         dh       =  tem5
         ds       =  dh - alhl * (tem6 + evp(l))
!
         gmh(l)   = tx1 + dh * pri(l)
         gms(l)   = tx2 + ds * pri(l)
         ghd(l)   = dh * pri(l)
         gsd(l)   = ds * pri(l)
!
         dh       = - tem5
         ds       = dh  + alhl * tem6
         gmh(l-1) = gmh(l-1) + dh * pri(l-1)
         gms(l-1) = gms(l-1) + ds * pri(l-1)
!
         ghd(l-1) = ghd(l-1) + dh * pri(l-1)
         gsd(l-1) = gsd(l-1) + ds * pri(l-1)
!
         qil(l)   = qil(l-1)
         qll(l)   = qll(l-1)
       endif

!       avh = avh + gmh(l-1)*(prs(l)-prs(l-1))

      enddo

      hbd  = hol(k)
      qbd  = qol(k)
      tem5 =  etd(k) * (hod(k+1) - hbd)
      tem6 =  etd(k) * (qod(k+1) - qbd)
      dh   = - tem5
      ds   = dh  + alhl * tem6
      tem1 = dh * pri(k)
      tem2 = ds * pri(k)
      gmh(k) = gmh(k) + tem1
      gms(k) = gms(k) + tem2
      ghd(k) = ghd(k) + tem1
      gsd(k) = gsd(k) + tem2
!
!     avh = avh + gmh(k)*(prs(kp1)-prs(k))
!
      tem4   = - gravfac * pris
      tx1    = dh * tem4
      tx2    = ds * tem4
!
      do l=kbl,k
        gmh(l) = gmh(l) + tx1
        gms(l) = gms(l) + tx2
        ghd(l) = ghd(l) + tx1
        gsd(l) = gsd(l) + tx2
!       avh = avh + tx1*(prs(l+1)-prs(l))
      enddo
!
!     if (lprnt) then
!        print *,' gmh=',gmh
!        print *,' gms=',gms
!     endif
!
!***********************************************************************
!***********************************************************************

!===>  kernel (akm) calculation begins

!===>  modify sounding with unit mass flux
!
!     testmb = 0.01

      do l=kd,k

         tem1   = gmh(l)
         tem2   = gms(l)
!        tem1   = gmh(l) - ghd(l)
!        tem2   = gms(l) - gsd(l)
         hol(l) = hol(l) +  tem1*testmb
!        qol(l) = qol(l) + (tem1-tem2) * tstmba
         qol(l) = qol(l) + (tem1-tem2                                   &
     &                   +  alhf*qil(l))  * (testmb/alhl)
         hst(l) = hst(l) +  tem2*(one+gam(l))*testmb
         qst(l) = qst(l) +  tem2*gam(l)*(testmb/alhl)
!        qst(l) = qst(l) +  tem2*gam(l)*tstmba
      enddo
!

      if (alm .gt. 0.0) then
!     hos = hos + (gmh(kd)-ghd(kd))  * testmb
      hos = hos + gmh(kd)  * testmb
      qos = qos + (gmh(kd)-gms(kd)                                      &
     &          + alhf*qil(kd)) * (testmb/alhl)
      else
        st2 = 1.0 - st1s
        hos = hos + (st1s*gmh(kd)+st2*gmh(kd1))  * testmb
        qos = qos + (st1s * (gmh(kd)-gms(kd)+alhf*qil(kd))              &
     &            +  st2  * (gmh(kd1)-gms(kd1)+alhf*qil(kd1)))          &
     &            * (testmb/alhl)
        hst(kd) = hst(kd) + (st1s*gms(kd)*(one+gam(kd))                 &
     &                    +  st2*gms(kd1)*(one+gam(kd1))) * testmb
        qst(kd) = qst(kd) + (st1s*gms(kd)*gam(kd)                       &
     &                    +  st2*gms(kd1)*gam(kd1)) * (testmb/alhl)
      endif

!
      tem = prl(k+1) - prl(k)
      hbl = hol(k) * tem
      qbl = qol(k) * tem
      do l=km1,kbl,-1
        tem = prl(l+1) - prl(l)
        hbl = hbl + hol(l) * tem
        qbl = qbl + qol(l) * tem
      enddo
      hbl = hbl * pris
      qbl = qbl * pris

!     if (lprnt) print *,' hbla=',hbl,' qbla=',qbl

!***********************************************************************

!===>  cloud workfunction for modified sounding, then kernel (akm)
!
      akm = zero
      tx1 = zero
      qtl = qst(kb1) - gaf(kb1)*hst(kb1)
      qtv = qbl
      hcc = hbl
      tx2 = hcc
      tx4 = (alhf*0.5)*max(zero,min(one,(tcr-tcl-tol(kb1))*tcrf))
!     tx4 = (alhf*0.5)*max(0.0,min(1.0,(tcr-tol(kb1))*tcrf))
!
      tem   = qst(kbl) - gaf(kbl)*hst(kbl)
      qtv   = 0.5 * ((tem+qtl) + (gaf(kbl)+gaf(kb1))*hbl)
      tx1   = max(zero, qbl-qtv)
!     tx1   = max(0.0, qbl-qtv)
      qtv   = qbl - tx1
!

      do l=kb1,kd1,-1
         del_eta = eta(l) - eta(l+1)
         hccp = hcc + del_eta*hol(l)
!
         qtlp = qst(l-1) - gaf(l-1)*hst(l-1)
         qtvp = 0.5 * ((qtlp+qtl)*eta(l)                                &
     &                +(gaf(l)+gaf(l-1))*hccp)
         detp = (bkc(l)*tx1 - (qtvp-qtv) + del_eta*qol(l)               &
     &        +  eta(l)*q0u(l) + eta(l+1)*q0d(l)) * akc(l)
         if (detp .le. zero) unsat = .true.

         st1  = hst(l) - ltl(l)*nu*(qst(l)-qol(l))

         tem2 = (alhf*0.5)*max(zero,min(one,(tcr-tcl-tol(l-1))*tcrf))
!        tem2 = (alhf*0.5)*max(0.0,min(1.0,(tcr-tol(l-1))*tcrf))
         tem1 = hccp + detp * (tem2+tx4)

         st2  = ltl(l) * vtf(l)
!        tem5 = qli(l) + qii(l)
         tem5 = qli(l) + qii(l) + (qll(l)+qil(l))*testmb
         akm  = akm +                                                   &
     &     (  (tx2  -eta(l+1)*st1-st2*(tx1-tem5*eta(l+1))) * dlb(l)     &
     &      + (tem1 -eta(l  )*st1-st2*(detp-tem5*eta(l)))  * dlt(l) )
!
         hcc  = hccp
         tx1  = detp
         tx2  = tem1
         qtl  = qtlp
         qtv  = qtvp
         tx4  = tem2
      enddo
!
      if (unsat) return
!
!  eventhough we ignore the change in lambda, we still assume
!  that the cloud-top contribution is zero; as though we still
!  had non-bouyancy there.
!
!
!     del_eta = eta(kd) - eta(kd1)
!     hccp    = hcc + del_eta*hos
!
!     qtlp = qst(kd) - gaf(kd)*hst(kd)
!     qtvp = qtlp*eta(kd) + gaf(kd)*hccp
!     st1  = eta(kd)*q0u(kd) + eta(kd1)*q0d(kd)
!     detp = (bkc(kd)*tx1 - (qtvp-qtv) + del_eta*qos + st1)
!    &                                               * akc(kd)
!
      st1 = hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
      st2 = ltl(kd)  * vtf(kd)
!     st2 = ltl(kd)  * (1.0+nu*qol(kd))
      tem5 = (qls + qis + (qll(kd)+qil(kd))*testmb) * eta(kd1)
      akm  = akm + half * (tx2-eta(kd1)*st1-st2*(tx1-tem5)) * dlb(kd)
!
!     st1 = half * (hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
!    &    +  hst(kd1) - ltl(kd1)*nu*(qst(kd1)-qol(kd1)))
!     st2 = half * (ltl(kd)  * (1.0+nu*qol(kd))
!     st2 = half * (ltl(kd)  * vtf(kd) +  ltl(kd1) * vtf(kd1))
!
!
!     st1 = hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
!     st2 = ltl(kd)  * (1.0+nu*qol(kd))
! 
!     tem5 = half * (qli(kd1)+qii(kd1)+qls+qis)
!     tem5 = half * (qli(kd1)+qii(kd1)+qls+qis
!    &     +  (qll(kd1)+qil(kd1)+qll(kd)+qil(kd))*testmb) * eta(kd1)
!     akm  = akm + half * (tx2-eta(kd1)*st1-st2*(tx1-tem5))*dlb(kd)

!     akm = (akm - wfn) * tstmbi
      akm = (akm - wfn) * (one/testmb)


!***********************************************************************

!===>   mass flux

      tem = max(0.01, min(0.05, (wfn-acr)/acr))
      tem2 = (rasalf*(tem-0.01) + 0.05 - tem) * oneopt4
!
!     tem = max(0.01, min(0.1, (wfn-acr)/acr))
!     tem2 = (rasalf*(tem-0.01) + 0.1 - tem) * oneopt9
!     tem = max(0.01, min(0.1, (wfn-acr)/acr)) * 11.1111111
!     tem2 = rasalf * (tem-0.111111111) + 1.11111111 - tem
!
      amb = - (wfn-acr) / akm
!
!     if(lprnt) print *,' wfn=',wfn,' acr=',acr,' akm=',akm
!    *,' amb=',amb,' kd=',kd

!===>   relaxation and clipping factors
!
      amb = amb * clp * tem2
!     amb = amb * clp * rasalf

      if (ddft) amb = min(amb, one/cldfrd)
       
!===>   sub-cloud layer depth limit on mass flux

      ambmax = (prl(kp1)-prl(kbl))*(fracbl*gravcon)
      amb    = max(min(amb, ambmax),zero)


!     if(lprnt) print *,' amb=',amb,' clp=',clp,' ambmax=',ambmax
!***********************************************************************
!*************************results***************************************
!***********************************************************************

!===>  precipitation and clw detrainment
!
!     avt = 0.0
!     avq = 0.0
!     avr = amb * dof

!
      dsfc = dsfc + amb * etd(k) * (1.0/dt)
!
      do l=kbl,kd,-1
          pcu(l) = pcu(l) + amb*rnn(l)      !  (a40)
!         avr = avr + amb*rnn(l)
!     if(lprnt) print *,' avr=',avr,' rnn=',rnn(l),' l=',l
      enddo
!
!===> temparature and q change and cloud mass flux due to cloud type kd
!
!     tx1 = amb * cpi
!     tx2 = amb * alhli
      tx1 = amb * (one/cp)
      tx2 = amb * (one/alhl)
      do l=kd,k
        st1    = gms(l)*tx1
        toi(l) = toi(l) + st1
        tcu(l) = tcu(l) + st1
        tcd(l) = tcd(l) + gsd(l) * tx1
!
!       st1 = st1 - (alhf/cp) * qil(l) * amb
!       avt = avt + st1 * (prs(l+1)-prs(l))

        flx(l) = flx(l) + eta(l)*amb
!
        qii(l) = qii(l) + qil(l) * amb
        tem    = 0.0

        qli(l) = qli(l) + qll(l) * amb + tem

        st1    = (gmh(l)-gms(l)+alhf*qil(l)) * tx2

        qoi(l) = qoi(l) + st1
        qcu(l) = qcu(l) + st1
        qcd(l) = qcd(l) + (ghd(l)-gsd(l)) * tx2
!
!       avq = avq + st1 * (prs(l+1)-prs(l))
!       avr = avr + (qll(l) + qil(l))*amb
!    &                  * (prs(l+1)-prs(l)) * gravcon
!     if(lprnt) print *,' avr=',avr,' qll=',qll(l),' l=',l
!    &,' qil=',qil(l)

      enddo
!
!      correction for negative condensate!
!     if (advcld) then
!       do l=kd,k
!         if (qli(l) .lt. 0.0) then
!           qoi(l) = qoi(l) + qli(l)
!           toi(l) = toi(l) - (alhl/cp) * qli(l)
!           qli(l) = 0.0
!         endif
!         if (qii(l) .lt. 0.0) then
!           qoi(l) = qoi(l) + qii(l)
!           toi(l) = toi(l) - ((alhl+alhf)/cp) * qii(l)
!           qii(l) = 0.0
!         endif
!       enddo
!     endif

!
!
!     if (lprnt) then
!       print *,' for kd=',kd
!       avt = avt * cp * 100.0*86400.0 / (alhl*dt*grav)
!       avq = avq *  100.0*86400.0 / (dt*grav)
!       avr = avr * 86400.0 / dt
!       print *,' avt=',avt,' avq=',avq,' avr=',avr,' avh='
!    &   ,avh,' alm=',alm,' ddft=',ddft,' kd=',kd
!
!     if (lprnt) then
!       print *,' for kd=',kd
!       print *,' tcu=',(tcu(l),l=kd,k)
!       print *,' qcu=',(qcu(l),l=kd,k)
!     endif
!
      tx1 = 0.0
      tx2 = 0.0
!
!     reevaporation of falling convective rain
!
      if (revap) then
!      afc     = -(1.04e-4*dt)*(3600./dt)**0.578
!      rknob   = 5.0
!      rknob   = 3.0
!      rknob   = 0.0
!      rknob   = 1.9
!      rknob   = 2.0
!
       tem = 0.0
       do l=kd,kbl
!        tem = tem + pcu(l)
         if (l .lt. idh .or. (.not. ddft)) then
           tem = tem + amb * rnn(l)
         endif
       enddo
       tem = tem + amb * dof
       tem = tem * (3600.0/dt)
!      tem1 = 4.0e10/max(garea,one) * sqrt((prl(kbl)-prl(kd))/prl(k+1))
       tem1 = rknob * sqrt(sqrt(4.0e10/max(garea,one)))
!      clfrac = ((clfa*tem + clfb)*tem + clfc)*tem + clfd
!      clfrac = min(point3, max(point01,clfrac))

!      if (lprnt) print *,' clfr0=',clf(tem),' tem=',tem,' tem1=',tem1

       clfrac = max(zero, min(one, clf(tem)*tem1))

!      if (lprnt) print *,' cldfrd=',cldfrd,' amb=',amb
!    &,' clfrac=',clfrac

!      tx3    = amb*eta(kd)*pri(kd)
       cldfrd = min(amb*cldfrd, one)
!      if(lprnt) print *,' cldfrd=',cldfrd,' amb=',amb
!      cldfrd = min(amb*cldfrd, 1.0)
!
!     if(lprnt) print *,' tx3=',tx3,' etakd=',eta(kd),' pri=',pri(kd)
!     if(lprnt) print *,' rnn=',rnn(kd:k)
!
       do l=kd,k
!        clvfr = (prl(l)+prl(l+1)) / (prl(k)+prl(k+1))
         clvfr = 0.5 * (prl(l)+prl(l+1)) / prl(k+1)
!        clvfr = min(1.0, clvfr * clvfr)
!                                                 for l=kd,k
         if (l .ge. idh .and. ddft) then
           tx2 = tx2 + amb * rnn(l)
!          if (l .eq. kbl) tx2 = tx2 + amb * dof
           if (l .eq. kbl) tx1 = tx1 + amb * dof
         else
           tx1 = tx1 + amb * rnn(l)
         endif
         tx4 = zfac * phil(l)
         tx4 = (one - tx4 * (one - half*tx4)) * afc
!
!        clfrac = min(tx3*rknob*1.1, one)
!        clfrac = one
!        clfrac = min(tx3*rknob, one)
!        clfrac = min(tx3*rknob, 1.0)

         if (tx1 .gt. 0. .or. tx2 .gt. 0.0) then
          teq     = toi(l)
          qeq     = qoi(l)
          pl      = 0.5 * (prl(l+1)+prl(l))

          st1     = max(zero, min(one, (tcr-teq)*tcrf))
!         st1     = max(0.0, min(1.0, (tcr-teq)*tcrf))
          st2     = st1*elfocp + (1.0-st1)*elocp

          call qsatcn ( teq,pl,qsteq,dqdt,.false.)
!
!         tx8  = 10.0 * fpvs(teq)                  ! fpvs is in centibars!
!         tx9  = 1.0 / max(pl + epsm1 * tx8, 1.0e-10)
!         qsteq  = min(eps*tx8*tx9, 1.0)
!         dqdt = pl * qsteq * alhl *  tx9 / (teq*teq*rv)
!
          deltaq = 0.5 * (qsteq-qeq) / (1.+st2*dqdt)
!
          qeq    = qeq + deltaq
          teq    = teq - deltaq*st2
!
          tem1   = max(zero, min(one, (tcr-teq)*tcrf))
!         tem1   = max(0.0, min(1.0, (tcr-teq)*tcrf))
          tem2   = tem1*elfocp + (1.0-tem1)*elocp

          call qsatcn ( teq,pl,qsteq,dqdt,.false.)
!
!         tx8  = 10.0 * fpvs(teq)                  ! fpvs is in centibars!
!         tx9  = 1.0 / max(pl + epsm1 * tx8, 1.0e-10)
!         qsteq  = min(eps*tx8*tx9, 1.0)
!         dqdt = pl * qsteq * alhl *  tx9 / (teq*teq*rv)
!
          deltaq = (qsteq-qeq) / (1.+tem2*dqdt)
!
          qeq    = qeq + deltaq
          teq    = teq - deltaq*tem2

          if (qeq .gt. qoi(l)) then
            potevap = (qeq-qoi(l))*(prl(l+1)-prl(l))*gravcon
!           potevap = (qeq-qoi(l))*(prl(l+1)-prl(l))*gravcon * 0.85

!           tem3    = sqrt(pl*0.001)
            tem4    = 0.0
            if (tx1 .gt. 0.0)                                           &
     &      tem4    = potevap * (1. - exp( tx4*tx1**0.57777778 ) )
!    &      tem4    = potevap * (1. - exp( afc*tx4*sqrt(tx1) ) )
!    &      tem4    = potevap * (1. - exp( afc*sqrt(tx1*tem3) ) )
!    &      tem4    = potevap * (1. - exp(-0.32*sqrt(dt*tx1*0.001) ) )
!           actevap = min(tx1, tem4*clfrac)
            actevap = min(tx1, tem4*clfrac*clvfr)
!     if(lprnt) print *,' l=',l,' actevap=',actevap,' tem4=',tem4,
!    &' clfrac='
!    &,clfrac,' potevap=',potevap,'efac=',afc*sqrt(tx1*tem3)
!    &,' tx1=',tx1
            if (tx1 .lt. rainmin*dt) actevap = min(tx1, potevap)
!
            tem4    = 0.0
            if (tx2 .gt. 0.0)                                           &
     &      tem4    = potevap * (1. - exp( tx4*tx2**0.57777778 ) )
!    &      tem4    = potevap * (1. - exp( afc*tx4*sqrt(tx2) ) )
!    &      tem4    = potevap * (1. - exp( afc*sqrt(tx2*tem3) ) )
!    &      tem4    = potevap * (1. - exp(-0.32*sqrt(dt*tx2*0.001) ) )
            tem4    = min(min(tx2, tem4*cldfrd), potevap-actevap)
            if (tx2 .lt. rainmin*dt) tem4 = min(tx2, potevap-actevap)
!
            tx1     = tx1 - actevap
            tx2     = tx2 - tem4
            st1     = (actevap+tem4) * pri(l)
            qoi(l)  = qoi(l) + st1
            qcu(l)  = qcu(l) + st1
!

            st1     = st1 * elocp
            toi(l)  = toi(l) - st1 
            tcu(l)  = tcu(l) - st1
          endif
         endif
       enddo
!
       cup = cup + tx1 + tx2
!      cup = cup + tx1 + tx2 + dof * amb
      else
       do l=kd,k
         tx1 = tx1 + amb * rnn(l)
       enddo
       cup = cup + tx1 + dof * amb
      endif

!     cup = cup + tx1 + tx2 + dof * amb
!     cup = cup + tx1 + tx2
!     if (lprnt) print *,' tx1=',tx1,' tx2=',tx2,' dof=',dof
!    &,' cup=',cup,' amb=',amb
!    &,' amb=',amb,' cup=',cup,' clfrac=',clfrac,' cldfrd=',cldfrd
!    &,' ddft=',ddft,' kd=',kd,' kbl=',kbl,' k=',k
!
!    mixing of passive tracers
!
      do n=1,m

       do l=kd,k
         hol(l) = roi(l,n)
       enddo
!
        hcc     = rbl(n)
        hod(kd) = hol(kd)
!      compute downdraft properties for the tracer
       do l=kd1,k
         st1 = one - alfint(l)
         hb  = alfint(l)  * hol(l-1) + st1 * hol(l)
         if (etz(l-1) .ne. 0.0) then
           del_eta = etd(l) - etd(l-1)
           tem     = 1.0 / etz(l-1)
           if (del_eta .gt. 0.0) then
             hod(l) = (etd(l-1)*(hod(l-1)-hol(l-1))                     &
     &                +  etd(l)  *(hol(l-1)-hb)                         &
     &                +  etz(l-1)*hb) * tem
           else
             hod(l) = (etd(l-1)*(hod(l-1)-hb) + etz(l-1)*hb) * tem
           endif
         else
           hod(l) = hb
         endif
       enddo
             
       do l=kb1,kd,-1
          hcc = hcc + (eta(l)-eta(l+1))*hol(l)
       enddo
!
       gmh(kd) = pri(kd) * (hcc-eta(kd)*hol(kd))
       do l=kd1,k
        st1 = one - alfint(l)
         if (l .lt. kbl) then
           hb       = alfint(l) * hol(l-1) + st1 * hol(l)
           tem5     = etd(l)    * (hod(l) - hb)
           dh       = eta(l)    * (hb - hol(l)) + tem5
           gmh(l  ) = dh * pri(l)
           dh       = eta(l)    * (hol(l-1) - hb) - tem5
           gmh(l-1) = gmh(l-1)  + dh * pri(l-1)
         elseif (l .eq. kbl) then
           hb       = alfint(l) * hol(l-1) + st1 * rbl(n)
           hbd      = alfint(l) * hol(l-1) + st1 * hol(l)
           dh       = etd(l)    * (hod(l) - hbd)
           tem4     = gravfac * pris
           tx1      = eta(l)    * (hb - rbl(n)) * tem4
           gmh(l)   = tx1       + dh * pri(l)
           dh       = eta(l)    * (hol(l-1) - hb) - dh
           gmh(l-1) = gmh(l-1)  + dh * pri(l-1)
         else
           hbd      = alfint(l) * hol(l-1) + st1 * hol(l)
           dh       = etd(l)    * (hod(l) - hbd)
           gmh(l)   = tx1       + dh * pri(l)
           gmh(l-1) = gmh(l-1)  - dh * pri(l-1)
         endif
       enddo
!
       do l=kd,k
         st1      = gmh(l)*amb
         roi(l,n) = hol(l)   + st1
         rcu(l,n) = rcu(l,n) + st1
       enddo
      enddo                             ! tracer loop m

!***********************************************************************
!***********************************************************************
!***********************************************************************

      return
      end
