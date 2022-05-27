      subroutine rascnv(im,    ix,     k,      dt,    ncrnd, rannum     &
     &,                 tin,   qin,    uin,    vin,   ccin,  trac       &
     &,                 prsi,  prsl,   prsik,  prslk, phil,  phii       &
     &,                 kpbl,  cdrag,  rainc,  kbot,  ktop,  kuo        &
     &,                 ddvel, flipv,  facmb,  me,ii,jj, garea          &
     &,                 lprnt)
!
!*********************************************************************
!*********************************************************************
!************         relaxed arakawa-schubert      ******************
!************             parameterization          ******************
!************          plug compatible driver       ******************
!************               23 may 2002             ******************
!************                                       ******************
!************               developed by            ******************
!************                                       ******************
!************             shrinivas moorthi         ******************
!************                                       ******************
!************                  emc/ncep             ******************
!*********************************************************************
!*********************************************************************
!
!
      use machine , only : kind_phys
      use module_ras, pdd => dd_prs
      use module_rascnv
      implicit none
!
      logical flipv, lprnt
!
!      input
!
      integer im, ix, k, ncrnd, me, trac,ii,jj
      integer kbot(im), ktop(im), kuo(im), kpbl(im)
!
      real(kind=kind_phys) tin(ix,k),     qin(ix,k),  uin(ix,k)         &
     &,                    vin(ix,k),     prsi(ix,k+1)                  &
     &,                    prsik(ix,k+1), prsl(ix,k), prslk(ix,k+1)     &
     &,                    phil(ix,k),    phii(ix,k+1)                  &
     &,                    ccin(ix,k,trac+2)                            &
!    &,                    prsik(ix,k+1), clt(ix,k)                     &
     &,                    rainc(im),     cdrag(im),  ddvel(im)         &
     &,                    rannum(ncrnd)
      real(kind=kind_phys) dt, facmb, garea(im)
!
!     locals
!
      real(kind=kind_phys) rain,     toi(k), qoi(k),  uvi(k,trac+2)     &
     &,                    tcu(k),   qcu(k), pcu(k),  clw(k), cli(k)    &
     &,                    flx(k),   qii(k), qli(k),  prs(k+1), psj(k+1)&
     &,                    phi_l(k), phi_h(k+1)                         &
     &,                    rcu(k,2), wfnc
!    &,                    rcu(k,2), rkap,   rkapi,   rkpp1i,   wfnc
      real(kind=kind_phys) daylen,pfac,tla,pl,clwmin
      integer icm,irnd,ib

      parameter (icm=100, daylen=86400.0, pfac=1.0/450.0,clwmin=1.0e-10)
      integer  ic(icm)
!
      real(kind=kind_phys) alfint(k), alfinq(k),   prsm(k),  psjm(k)
!    &,                    dpi(k),    psjp(k+1)
      real(kind=kind_phys) cfac,   savt,  savq, savw(trac+2), st,  sq   &
     &,                    sprs,   sprsm, sprj, sprjm,   wr1, tem, sdpi &
     &,                    dpi,    sphil, sw(trac+2)
!
      integer              kcr,  kfx, ncmx, nc,  ktem, i,   l,  lct, lcb&
     &,                    ntrc, ia,  ll,   km1, kp1,  ipt, lv, kbl, n
!
      logical  dndrft, lprint
!
        km1    = k - 1
        kp1    = k + 1
!
        kcr    = min(mct(ii,jj),krmax(ii,jj))
        ktem   = min(mct(ii,jj),kfmax(ii,jj))
        kfx    = ktem - kcr
!     if(lprnt)print*,' enter rascnv k=',k,' ktem=',ktem,' mct='
!    &,                 mct(ii,jj)
!    &,               ' krmax=',krmax(ii,jj),' kfmax=',kfmax(ii,jj)
!    &,               ' kcr=',kcr, ' cdrag=',cdrag(ipr)
 
        if (kfx .gt. 0) then
           if (botop) then
              do nc=1,kfx
                ic(nc) = ktem + 1 - nc
              enddo
           else
              do nc=kfx,1,-1
               ic(nc) = ktem + 1 - nc
              enddo
           endif
        endif
        ntrc = trac
        if (cumfrc) ntrc = ntrc + 2
!
!     alfint(1) = 0.5
      alfint(1) = 1.0
      do l=2,k
!       alfint(l) = 0.5
        alfint(l) = 1.0
      enddo
!
      ncmx  = kfx + ncrnd
      if (ncrnd .gt. 0) then
         do i=1,ncrnd
           irnd = (rannum(i)-0.0005)*(kcr-krmin(ii,jj)+1)
           ic(kfx+i) = irnd + krmin(ii,jj)
         enddo
      endif
!
!     ia = 1
!
!     print *,' in rascnv: k=',k,'lat=',lat,' lprnt=',lprnt
!     if (lprnt) then
!        if (me .eq. 0) then
!        print *,' tin',(tin(ia,l),l=k,1,-1)
!        print *,' qin',(qin(ia,l),l=k,1,-1)
!     endif
!
      do ipt=1,im
        kuo(ipt)  = 0
        do l=1,k
          ll = l
          if (flipv) ll = kp1 -l ! input variables are bottom to top!
          clw(l)     = 0.0       ! assumes initial value of cloud water
          cli(l)     = 0.0       ! assumes initial value of cloud ice
                                 ! to be zero i.e. no environmental condensate!!!
!         clt(ipt,l) = 0.0
          qii(l)     = 0.0
          qli(l)     = 0.0
!                          initialize heating, drying, cloudiness etc.
          tcu(l)     = 0.0
          qcu(l)     = 0.0
          pcu(l)     = 0.0
          flx(l)     = 0.0
          rcu(l,1)   = 0.0
          rcu(l,2)   = 0.0
!                          transfer input prognostic data into local variable
          toi(l)     = tin(ipt,ll)
          qoi(l)     = qin(ipt,ll)
          uvi(l,trac+1) = uin(ipt,ll)
          uvi(l,trac+2) = vin(ipt,ll)
!
          do n=1,trac
            uvi(l,n) = ccin(ipt,ll,n+2)
          enddo
!
        enddo
!
        if (ccin(ipt,1,2) .le. -999.0) then
          do l=1,k
            ll = l
            if (flipv) ll = kp1 -l ! input variables are bottom to top!
              tem = ccin(ipt,ll,1)                                       &   
     &            * max(zero, min(one, (tcr-toi(l))*tcrf))
              ccin(ipt,ll,2) = ccin(ipt,ll,1) - tem
              ccin(ipt,ll,1) = tem
          enddo
        endif
        if (advcld) then
          do l=1,k
            ll = l
            if (flipv) ll = kp1 -l ! input variables are bottom to top!
            qii(l) = ccin(ipt,ll,1)
            qli(l) = ccin(ipt,ll,2)
          enddo
        endif
!
        kbl  = kpbl(ipt)
        if (flipv) kbl  = max(min(k, kp1-kpbl(ipt)), k/2)
        rain = 0.0
!
        do l=1,kp1
          ll = l
          if (flipv) ll = kp1 + 1 - l      ! input variables are bottom to top!
          prs(ll)   = prsi(ipt, l) * facmb ! facmb is for conversion to mb
          psj(ll)   = prsik(ipt,l)
          phi_h(ll) = phii(ipt,l)
        enddo
!
        do l=1,k
          ll = l
          if (flipv) ll = kp1 - l          ! input variables are bottom to top!
          prsm(ll)  = prsl(ipt, l) * facmb ! facmb is for conversion to mb
          psjm(ll)  = prslk(ipt,l)
          phi_l(ll) = phil(ipt,l)
        enddo
!
!     if(lprint) print *,' prs=',prs
!     if(lprint) print *,' prsm=',prsm
!     if (lprint) then
!        print *,' qns=',qns(ia),' qoi=',qn0(ia,k),'qin=',qin(ia,1)
!        if (me .eq. 0) then
!        print *,' toi',(tn0(ia,l),l=1,k)
!        print *,' qoi',(qn0(ia,l),l=1,k),' kbl=',kbl
!     endif
!
!
!!      psjp(kp1) = psj(kp1) * prs(kp1) * rkpp1i
!       do l=k,kctop(1),-1
!         dpi     = rkpp1i / (prs(l+1) - prs(l))
!         psjm(l) = (psj(l+1)*prs(l+1) - psj(l)*prs(l)) * dpi
!!        psjp(l) = psj(l) * prs(l) * rkpp1i
!!        dpi(l)  = 1.0 / (prs(l+1) - prs(l))
!!        psjm(l) = (psjp(l+1) - psjp(l)) * dpi(l)
!         prsm(l) = 1000.0 * psjm(l) ** (1.0/rkap)
!!        prsm(l) = 1000.0 * psjm(l) ** rkapi
!!        prsm(l) = 0.5 * (prs(l+1)+prs(l))
!       enddo
!
!
        if (calkbl) kbl = k
        do nc=1,ncmx
!
          ib = ic(nc)
          if (ib .gt. kbl) cycle
!
          lct  = kctop(ii,jj,ib)
          lcb  = kctop(ii,jj,ib+1) - 1
!
!        if (me .eq. 0) then
!        write(6,*) ' lct=',lct,' lcb=',lcb,' ib=',ib
!    *,' kctop=',kctop(ib),kctop(ib+1),' prs=',prs(1,lcb),prs(1,lcb+1)
!        endif
!
          if (lct .lt. lcb) then
            wr1     = 1.0 / (prs(lcb+1) - prs(lct))
            tem     = (prs(lcb+1) - prs(lcb)) * wr1
!
            savt       = toi(lcb)
            savq       = qoi(lcb)
!           savw(1)    = uvi(lcb,1)
!           savw(2)    = uvi(lcb,2)
            do n=1,ntrc
              savw(n)  = uvi(lcb,n)
            enddo
            sprs       = prs(lcb)
            sprj       = psj(lcb)
            sprsm      = prsm(lcb)
            sprjm      = psjm(lcb)
            sphil      = phi_l(lcb)
!           sdpi       = dpi(lcb)
!
            toi(lcb)   = toi(lcb)   * tem
            qoi(lcb)   = qoi(lcb)   * tem
!           uvi(lcb,1) = uvi(lcb,1) * tem
!           uvi(lcb,2) = uvi(lcb,2) * tem
            do n=1,ntrc
            uvi(lcb,n) = uvi(lcb,n) * tem
            enddo
            do l=lct, lcb-1
              tem        = (prs(l+1) - prs(l))   * wr1
              toi(lcb)   = toi(lcb)   + toi(l)   * tem
              qoi(lcb)   = qoi(lcb)   + qoi(l)   * tem
!             uvi(lcb,1) = uvi(lcb,1) + uvi(l,1) * tem
!             uvi(lcb,2) = uvi(lcb,2) + uvi(l,2) * tem
              do n=1,ntrc
                uvi(lcb,n) = uvi(lcb,n)  + uvi(l,n) * tem
              enddo
            enddo
            st       = toi(lcb)
            sq       = qoi(lcb)
!           sw(1)    = uvi(lcb,1)
!           sw(2)    = uvi(lcb,2)
            do n=1,ntrc
              sw(n)  = uvi(lcb,n)
            enddo
            prs(lcb) = prs(lct)
            psj(lcb) = psj(lct)
!
!           dpi(lcb)  = 1.0 / (prs(lcb+1) - prs(lct))
!           psjm(lcb) = (psjp(lcb+1) - psjp(lct)) * dpi(lcb)
            dpi       = rkpp1i / (prs(lcb+1) - prs(lct))
            psjm(lcb) = (psj(lcb+1)*prs(lcb+1) - psj(lct)*prs(lct))*dpi
            prsm(lcb) = 1000.0*(psjm(lcb)) ** (1.0/rkap)
!           prsm(lcb) = 1000.0*(psjm(lcb)) ** rkapi
!           prsm(lcb) = 0.5 * (prs(lcb+1)+prs(lct))
!
            phi_l(lcb) = phi_h(lcb+1) + cp * toi(lcb)*(1.0+nu*qoi(lcb)) &
     &                 * (psj(lcb+1) - psjm(lcb)) / psjm(lcb)
          endif
!
          dndrft = pdd .gt. 0.0
!
!     write(0,*)' ib=',ib,' lct=',lct,' lcb=',lcb,' kdd=',kdd,' dndrft='
!    *,       dndrft
!
!     if (me .eq. 0) then
!     print *,' calling cloud type ib=',ib,' kbl=',kbl
!    *,' kpbl=',kpbl,' alfint=',alfint,' frac=',frac
!    *,' ntrc=',ntrc
!
!     if (lprint) then
!     ia = 1
!     print *,' toi=',(toi(ia,l),l=1,k)
!     print *,' qoi=',(qoi(ia,l),l=1,k),' kbl=',kbl
!     print *,' toi=',(toi(l),l=1,k)
!     print *,' qoi=',(qoi(l),l=1,k),' kbl=',kbl
!     print *,' prs=',(prs(l),l=1,k)
!     endif
!
          wfnc = 0.0
          do l=lcb,k
            flx(l)    = 0.0
!           alfint(l) = (psjm(l)-psj(l)) / (psjm(l)-psjm(l-1))
!           alfint(l) = (prsm(l)-prs(l)) / (prsm(l)-prsm(l-1))
            alfinq(l) = alfint(l)
          enddo
!         alfinq(k) = 0.0
!
!
!     if (me .eq. 0) then
!     if(lprint)then
!     print *,' calling cloud type ib= ', ib,' dt=',dt,' k=',k
!    &, 'ipt=',ipt
!     print *,' toi=',(toi(l),l=lcb,k)
!     print *,' qoi=',(qoi(l),l=lcb,k)
!     endif
!     print *,' alft=',alfint
!
          tla = -10.0
!
!     if (lprint) print *,' qliin=',qli
!     if (lprint) print *,' qiiin=',qii
          call cloud(k, lcb, ntrc                                       &
     &,              rasal(lcb), frac,  max_neg_bouy                    &
     &,              alfint, alfinq, rhfacl, rhfacs, garea(ipt)         &
!    &,              alfint, alfinq, rhfacl, rhfacs, garea(ipt)         &
!
     &,              toi, qoi, uvi, prs, prsm, phi_l, phi_h             &
!    &,              toi, qoi, uvi, prs, prsm, psj, psjm
!    &,              toi, qoi, uvi, prs, prsm, psj, psjm, dpi
!    &,              toi, qoi, uvi, prs, psj
     &,              qli, qii, kbl, ddvel(ipt)                          &
     &,              cdrag(ipt),lprnt                                   &
!    &,              idiag, lprnt
     &,              tcu, qcu, rcu, pcu, flx                            &
     &,              rain, revap, dt                                    &
     &,              wfnc, wrkfun, calkbl, crtfun, tla, dndrft, pdd)    
!    &,              wfnc, wrkfun, calkbl, crtfun, tla, dndrft, updret)
!     if (lprint) print *,' rain=',rain
!     if (me .eq. 0) then
!     print *,' after calling cloud type ib= ', ib
!     print *,' toi=',(toi(l),l=1,k)
!     print *,' qoi=',(qoi(l),l=1,k)
!     endif
!     if (lprint) print *,' qliou=',qli
!     if (lprint) print *,' qiiou=',qii
! 
!     compute cloud amounts for the goddard radiation
!
!         if (flx(kbl) .gt. 0.0) then
!           pl   = 0.5 * (prs(lct) + prs(lcb+1))
!           cfac = min(1.0, max(0.0, (850.0-pl)*pfac))
!         else
!           cfac = 0.0
!         endif
!
          if (lct .lt. lcb) then
            st        = toi(lcb)   - st
            sq        = qoi(lcb)   - sq
!           sw(1)     = uvi(lcb,1) - sw(1)
!           sw(2)     = uvi(lcb,2) - sw(2)
            do n=1,ntrc
              sw(n)   = uvi(lcb,n) - sw(n)
            enddo
!
            toi(lcb)   = savt
            qoi(lcb)   = savq
!           uvi(lcb,1) = savw(1)
!           uvi(lcb,2) = savw(2)
            do n=1,ntrc
              uvi(lcb,n) = savw(n)
            enddo
            prs(lcb)   = sprs
            psj(lcb)   = sprj
            prsm(lcb)  = sprsm
            psjm(lcb)  = sprjm
            phi_l(lcb) = sphil
!           dpi(lcb)   = sdpi
        
            do l=lct,lcb
              toi(l)   = toi(l)     + st
              qoi(l)   = qoi(l)     + sq
!             uvi(l,1) = uvi(l,1)   + sw(1)
!             uvi(l,2) = uvi(l,2)   + sw(2)
              do n=1,ntrc
                uvi(l,n) = uvi(l,n)   + sw(n)
              enddo
!             clt(ipt,l) = min(clt(ipt,l) + cfac, 1.0)
            enddo
            do l=lct,lcb-1
              qli(l) = qli(lcb)
              qii(l) = qii(lcb)
            enddo
!         else
!           clt(ipt, lcb) = min(clt(ipt,lcb) + cfac, 1.0)
          endif
!
!   warining!!!!
!   ------------
!   by doing the following, cloud does not contain environmental
!   condensate!
!
          if (.not. advcld) then
            do l=1,k
!             clw(l ) = clw(l) + qli(l) + qii(l)
              clw(l ) = clw(l) + qli(l)
              cli(l ) = cli(l) + qii(l)
              qli(l)  = 0.0
              qii(l)  = 0.0
            enddo
          endif
!
        enddo                      ! end of the nc loop!
!
        rainc(ipt) = rain * 0.001    ! output rain is in meters
!     if(lprint)print*,' convective precip=',rain*1.e3,' mm'
!    1,               ' ipt=',ipt,' ii=',ii,' jj=',jj
!
!     if (lprint) then
!        print *,' toi',(tn0(imax,l),l=1,k)
!        print *,' qoi',(qn0(imax,l),l=1,k)
!     endif
!
        do l=1,k
          ll = l
          if (flipv) ll  = kp1 - l
          tin(ipt,ll)    = toi(l)                   ! temperature
          qin(ipt,ll)    = qoi(l)                   ! specific humidity
          uin(ipt,ll)    = uvi(l,trac+1)            ! u momentum
          vin(ipt,ll)    = uvi(l,trac+2)            ! v momentum
!         clw(l)         = clw(l) + qli(l) + qii(l) ! cloud condensate
!         ccin(ipt,ll,1) = ccin(ipt,ll,1) + clw(l)
          do n=1,trac
            ccin(ipt,ll,n+2) = uvi(l,n)             ! tracers
          enddo
        enddo
        if (advcld) then
          do l=1,k
            ll = l
            if (flipv) ll  = kp1 - l
!           ccin(ipt,ll,1) = qli(l) + qii(l) ! cloud condensate
            ccin(ipt,ll,1) = qii(l)          ! cloud ice
            ccin(ipt,ll,2) = qli(l)          ! cloud water
          enddo
        else
          do l=1,k
            ll = l
            if (flipv) ll  = kp1 - l
!           ccin(ipt,ll,1) = ccin(ipt,ll,1) + clw(l)
            ccin(ipt,ll,1) = ccin(ipt,ll,1) + cli(l)
            ccin(ipt,ll,2) = ccin(ipt,ll,2) + clw(l)
          enddo
        endif
!
!       kuo(ipt)  = 0
!
        ktop(ipt) = kp1
        kbot(ipt) = 0

        do l=km1,1,-1
          if (prsm(l) .lt. 900.0 .and. tcu(l) .ne. 0.0) then
!         if (phi_l(l) .gt. 10000.0 .and. tcu(l) .ne. 0.0) then
             kuo(ipt) = 1
          endif
!  new test for convective clouds ! added in 08/21/96
          if (clw(l)+cli(l) .gt. 0.0 .or.                                &   
     &        qli(l)+qii(l) .gt. clwmin) ktop(ipt) = l
        enddo
        do l=1,km1
          if (clw(l)+cli(l) .gt. 0.0 .or.                                &
     &        qli(l)+qii(l) .gt. clwmin) kbot(ipt) = l
        enddo
        if (flipv) then
          ktop(ipt) = kp1 - ktop(ipt)
          kbot(ipt) = kp1 - kbot(ipt)
        endif
!
!     if (lprint) then
!        print *,' tin',(tin(ia,l),l=k,1,-1)
!        print *,' qin',(qin(ia,l),l=k,1,-1)
!     endif
!
!     velocity scale from the downdraft!
!
        ddvel(ipt) = ddvel(ipt) * ddfac * grav / (prs(k+1)-prs(k))
!
      enddo                            ! end of the ipt loop!
!
      return
      end
