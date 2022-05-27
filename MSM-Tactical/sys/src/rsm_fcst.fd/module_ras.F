      module module_ras
      use machine , only : kind_phys
      use physcons, grav => con_g, cp => con_cp, alhl => con_hvap       &
     &,             alhf => con_hfus, rgas => con_rd, rkap => con_rocp  &
     &,             nu => con_fvirt
      implicit none
      save
!     real, parameter :: nu=0.0
!
      real(kind=kind_phys) zero, half, one, two
      real(kind=kind_phys) four_p2,four
      real(kind=kind_phys) one_m1,one_m2,one_m5,one_m6,one_m10
      parameter (zero=0.0, half=0.5,  one=1.0, two=2.0)
      parameter (four_p2=4.e2,four=4.,one_m10=1.e-10,one_m6=1.e-6       &
     &,          one_m5=1.e-5,one_m2=1.e-2,one_m1=1.e-1)
!
      real(kind=kind_phys), parameter :: cmb2pa = 100.0  ! conversion from mb to pa
      real(kind=kind_phys) onebg, gravcon, gravfac, elocp, elfocp,      &
     &                     rkapi, rkpp1i,  zfac
!
      parameter (onebg   = one / grav,    gravcon = cmb2pa * onebg      &
     &,          gravfac = grav / cmb2pa, elocp   = alhl / cp           &
     &,          elfocp  = (alhl+alhf) / cp                             &
     &,          rkapi   = one / rkap,    rkpp1i  = one / (one+rkap)    &
     &,          zfac    = 0.28888889e-4 * onebg)
!
!     logical, parameter :: advcld=.false.
      logical, parameter :: advcld=.true.
!     integer mct, krmin, krmax, kfmax
      integer, allocatable ::  mct(:,:), krmin(:,:), krmax(:,:)         &
     &,                        kfmax(:,:)
      integer, allocatable :: kctop(:,:,:)
!
      real(kind=kind_phys), allocatable ::  rasal(:)
      real(kind=kind_phys) rhmax,  qudfac, quad_lam, rhram, testmb,     &
     &                     tstmbi, hcrit,  dd_prs,   rknob, afc

!     parameter (dd_prs=600.0, rknob=3.0)
!     parameter (dd_prs=600.0, rknob=2.5)
!     parameter (dd_prs=600.0, rknob=2.0)
!     parameter (dd_prs=600.0, rknob=1.5)
!     parameter (dd_prs=000.0, rknob=0.0)
!     parameter (dd_prs=600.0, rknob=0.5)
      parameter (dd_prs=600.0, rknob=1.0)
!
      parameter (rhmax=1.0   )  !  max relative humidity
      parameter (quad_lam=1.0)  !  mask for quadratic lambda
!     parameter (rhram=0.15)    !  pbl relative humidity ramp
      parameter (rhram=0.05)    !  pbl relative humidity ramp
!     parameter (rhram=0.10)    !  pbl relative humidity ramp
      parameter (hcrit=4000.0)  !  critical moist static energy
      parameter (qudfac=quad_lam*half)
!     parameter (qudfac=quad_lam*0.25)    ! yogesh
      parameter (testmb=0.1, tstmbi=one/testmb)
!
      real(kind=kind_phys) almin1, almin2, almax
!
!     parameter (almin1=0.00e-6, almin2=0.00e-5, almax=1.0e-1)
!     parameter (almin1=0.00e-6, almin2=0.00e-5, almax=2.0e-2)
!     parameter (almin1=0.00e-6, almin2=1.00e-6, almax=2.0e-2)
!     parameter (almin1=5.00e-6, almin2=2.50e-5, almax=2.0e-2)
      parameter (almin1=0.00e-6, almin2=2.50e-5, almax=2.0e-2)
!     parameter (almin1=0.00e-6, almin2=1.00e-5, almax=2.0e-2)
!     parameter (almin1=0.00e-6, almin2=5.00e-4, almax=2.0e-2)
!     parameter (almin1=0.10e-4, almin2=0.15e-4, almax=1.0e-1)
!     parameter (almin1=0.00e-4, almin2=0.40e-4, almax=2.0e-2)
!     parameter (almin1=0.20e-4, almin2=0.40e-4, almax=2.0e-2)
!     parameter (almin1=0.25e-4, almin2=0.50e-4, almax=2.0e-2)
!     parameter (almin1=0.40e-4, almin2=0.50e-4, almax=2.0e-2)
!
      real(kind=kind_phys), parameter :: bldmax = 200.0
!
      integer kblmx
      real(kind=kind_phys) c0, c0i, qi0, qw0
!     parameter (qi0=1.0e-4, qw0=1.0e-4)
!     parameter (qi0=0.0e-5, qw0=0.0e-0)
      parameter (qi0=1.0e-5, qw0=1.0e-5)
!     parameter (qi0=1.0e-5, qw0=1.0e-6)
!     parameter (qi0=0.0e-5, qw0=0.0e-5)
!     parameter (qi0=5.0e-5, qw0=1.0e-5)
!     parameter (qi0=5.0e-5, qw0=2.0e-5)
!     parameter (qi0=2.0e-4, qw0=2.0e-5)
!     parameter (qi0=2.0e-5, qw0=2.0e-5)
!     parameter (qi0=2.0e-4, qw0=1.0e-4)
!     parameter (qi0=2.0e-4, qw0=1.0e-5)
!     parameter (qi0=1.0e-3, qw0=2.0e-5)
!     parameter (qi0=1.0e-3, qw0=2.0e-4)
!     parameter (c0i=5.0e-4)
      parameter (c0i=4.0e-4)
!     parameter (c0i=1.0e-3)
      parameter (c0=1.0e-3)
!     parameter (c0=1.5e-3)
!     parameter (c0=2.0e-3)
!     parameter (c0=1.0e-3, kblmx=10, errmin=0.0001, errmi2=0.1*errmin)
!     parameter (c0=2.0e-3, kblmx=10, errmin=0.0001, errmi2=0.1*errmin)
!
      real(kind=kind_phys) tf, tcr, tcrf, tcl
!     parameter (tf=130.16, tcr=160.16, tcrf=1.0/(tcr-tf))
!     parameter (tf=230.16, tcr=260.16, tcrf=1.0/(tcr-tf))
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf),tcl=2.0)
!
!     for tilting angle specification
!
      real(kind=kind_phys) refp(6), refr(6), tlac(8), plac(8), tlbpl(7) &
     &,                    drdp(5), vtp
!
      data plac/100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0/
!     data tlac/ 37.0,  25.0,  17.0,  12.0,  10.0,  8.0,  6.0,  5.0/
!     data tlac/ 35.0,  24.0,  17.0,  12.0,  10.0,  8.0,  6.0,  5.0/
      data tlac/ 35.0,  25.0,  20.0,  17.5,  15.0,  12.5,  10.0,  5.0/
!     data tlac/ 37.0,  26.0,  18.0,  14.0,  10.0,  8.0,  6.0,  5.0/
!     data tlac/ 25.0,  22.5,  20.0,  17.5,  15.0,  12.5,  10.0,  10.0/
      data refp/500.0, 300.0, 250.0, 200.0, 150.0, 100.0/
!     data refr/ 0.25,   0.5,  0.75,   1.0,   1.5,   2.0/
!     data refr/ 0.5,   1.0,  1.5,   2.0,   3.0,   4.0/
      data refr/ 1.0,   2.0,  3.0,   4.0,   6.0,   8.0/
!
      real(kind=kind_phys) ac(16), ad(16)
!
      integer,parameter:: nqrp=500001
      real(kind=kind_phys) c1xqrp, c2xqrp, tbqrp(nqrp), tbqra(nqrp),    &
     &                     tbqrb(nqrp)
!
      integer,parameter:: nvtp=10001
      real(kind=kind_phys) c1xvtp, c2xvtp, tbvtp(nvtp)
!
      contains
!
      subroutine set_ras_afc(dt)
      implicit none
      real(kind=kind_phys) dt
!     afc = -(1.04e-4*dt)*(3600./dt)**0.578
      afc = -(1.01097e-4*dt)*(3600./dt)**0.57777778
      end subroutine set_ras_afc

      subroutine ras_init(levs, si,  flipv, me                          &
     &,                   id1,  id2, jd1,   jd2, iin, jin, levm)
!
!     this ras_init assumes same number of levels in the vertical
!     but levels can be of reverse order (i.e. flipv=.true.)
!
      implicit none

      integer levs
      integer id1, id2, jd1, jd2, iin, jin, levm
      real(kind=kind_phys) si(levs+1),  sig(levs+1), pcbt,  pctp
      real(kind=kind_phys) pctop(levs+1)
      logical              flipv
!
      data pctp/40.0/, pcbt/930.0/
!
      real(kind=kind_phys) actp,   facm, tem,  pinc, actop, sgc
      real(kind=kind_phys) rasalf, tem1, tem2, pinc10
      integer              lm, i,    ii,   lmm1, mctm, l, me
      parameter (actp=1.7,   facm=1.00)
!     parameter (actp=1.7,   facm=0.75)
!     parameter (actp=1.7,   facm=0.5)
!
      real(kind=kind_phys) ph(15),    a(15)
!
      data ph/150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0    &
     &,       550.0, 600.0, 650.0, 700.0, 750.0, 800.0, 850.0/
!
       data a/ 1.6851, 1.1686, 0.7663, 0.5255, 0.4100, 0.3677           &
     &,       0.3151, 0.2216, 0.1521, 0.1082, 0.0750, 0.0664            &
     &,       0.0553, 0.0445, 0.0633/
!
      data pinc10/5.0/
      logical first
      data first/.true./
!
!     allocate (kctop(levs+1), rasal(levs))
      if (first) then
        allocate (kctop(id1:id2,jd1:jd2,levm+1), rasal(levm))
        allocate (mct(id1:id2,jd1:jd2),   krmin(id1:id2,jd1:jd2)        &
     &,           krmax(id1:id2,jd1:jd2), kfmax(id1:id2,jd1:jd2))
!
!                                   set critical workfunction arrays
        actop = actp*facm
        do l=1,15
          a(l) = a(l)*facm
        enddo
        do l=2,15
          tem   = 1.0 / (ph(l) - ph(l-1))
          ac(l) = (ph(l)*a(l-1) - ph(l-1)*a(l)) * tem
          ad(l) = (a(l) - a(l-1)) * tem
        enddo
        ac(1)  = actop
        ac(16) = a(15)
        ad(1)  = 0.0
        ad(16) = 0.0
!
!       call setes
        call setqrp
        call setvtp
!
        kblmx = levm / 2
!
!       rasalf  = 0.10
        rasalf  = 0.30
!
!       do l=1,lm
        do l=1,levm
          rasal(l) = rasalf
        enddo
!
!
        do i=1,7
          tlbpl(i) = (tlac(i)-tlac(i+1)) / (plac(i)-plac(i+1))
        enddo
        do i=1,5
          drdp(i)  = (refr(i+1)-refr(i)) / (refp(i+1)-refp(i))
        enddo
!
        vtp    = 36.34*sqrt(1.2)* (0.001)**0.1364
!
        if (me .eq. 0) print *,' no downdraft for cloud types'          &
     &,                      ' detraining below ',dd_prs,' hpa level'
!
        first = .false.
      endif
!
      lm    = levs
      mct(iin,jin) = levs
!
      if (flipv) then
        do l=1,lm
          sig(l) = si(lm-l+2)
        enddo
        sig(lm+1) = si(1)
      else
        do l=1,lm
          sig(l) = si(l)
        enddo
        sig(lm+1) = si(lm+1)
      endif
!                                   set other parameters
      lmm1 = lm - 1
!     tem  = 1.0 / prs(lm+1)
!     do l=1,lm+1
!        sig(l) = prs(l) * tem
!     enddo
!
      pinc = pinc10 * (10.0/mct(iin,jin))
      tem  = (pcbt - pctp - (mct(iin,jin)-1)*mct(iin,jin)*pinc/2)        & 
     &                                      / mct(iin,jin)
      pctop(1) = pctp * 0.001
!     print *,' pctop=',pctop
      do l=1,lm
        if(sig(l).le.pctop(1)) kctop(iin,jin,1)=l
      enddo
!
      if (mct(iin,jin) .lt. lmm1) then
        do i=1,mct(iin,jin)
          pctop(i+1) = 0.001 * (pctop(i)*1000.0 + tem + pinc*(i-1))
        enddo
!       if (me .eq. 0) print *,' pctop=',pctop
        tem2 = 0.0
        do i=2,mct(iin,jin)+1
          if (kctop(iin,jin,i-1) .lt. lm) then
            ii       = kctop(iin,jin,i-1) + 1
            if (sig(ii) .le. pctop(mct(iin,jin)+1)) then
              kctop(iin,jin,i) = ii
              do l=ii,lm
                if (sig(l) .le. pctop(i)) then
                  kctop(iin,jin,i) = l
                endif
              enddo
              if (pctop(i) .gt. sig(kctop(iin,jin,i))) then
                tem1 = sig(kctop(iin,jin,i)) - sig(kctop(iin,jin,i-1))
                if (tem1 .lt. tem2) then
                  kctop(iin,jin,i) = min(lm, kctop(iin,jin,i)+1)
                endif
                tem2 = sig(kctop(iin,jin,i)) - sig(kctop(iin,jin,i-1))
              endif
            else
              kctop(iin,jin,i) = lm + 1
            endif
          else
            kctop(iin,jin,i) = lm + 1
          endif
        enddo
      else
        do l=1,lm
          kctop(iin,jin,l) = l
        enddo
      endif
!
      ii = min(lm,mct(iin,jin)+1)
      do i=1,ii
        if (kctop(iin,jin,i) .le. lm) mctm = i
      enddo
!     if (me .eq. 0) print *,' kctop=',(kctop(i),i=1,mctm)              &
!    &,                 ' mct=',mct,' mctm=',mctm
!     print *,' sgc=',sgc
      do i=1,mctm-1
        if (kctop(iin,jin,i) .eq. kctop(iin,jin,i+1)) then
          write(0,*)' for cloud-type = ',i, ' kctop same at top'        &
     &,              ' and bottom - run stopped'
          stop
        endif
      enddo
!
      krmin(iin,jin) = 1
      krmax(iin,jin) = mctm - 1
      kfmax(iin,jin) = mctm - 1
      do l=1,mctm-1
         sgc  = 0.5 * (sig(kctop(iin,jin,l)) + sig(kctop(iin,jin,l+1)))
         if (sgc .le. 0.050) krmin(iin,jin) = l
!        if (sgc .le. 0.850) krmax = l
         if (sgc .le. 0.800) krmax(iin,jin) = l
!        if (sgc .le. 0.760) krmax = l
!        if (sgc .le. 0.930) kfmax = l
!        if (sgc .le. 0.970) kfmax = l
         if (sgc .le. 0.950) kfmax(iin,jin) = l
!     if(me.eq.0)write(0,*)' l=',l,' kctop=',kctop(iin,jin,l)
!    &,                     kctop(iin,jin,l+1),' iin=',iin,' jin=',jin
      enddo
!
!
      if (me .eq. 0) print *,' krmin=',krmin(iin,jin),' krmax='         &
     &,              krmax(iin,jin), ' kfmax=',kfmax(iin,jin),' rasalf='&
     &,              rasalf
!
      end subroutine ras_init
      end module module_ras
