      subroutine cldjms(idimt,idims,kdim,nbin,mcld,                     &
     &                  q,t,vvel,rhrh,qst,                              &
     &                  cv,cvt,cvb,prsi,prsl,slmsk,                     &
     &                  cld,mtop,mbot,cldtot,cldcnv,                    &
     &                  ivva,invr,xlatrd,rhcld,istrat,clw,ncld)
!tune
!fpp$ noconcur r
!....    from yh.rad.mdl93(cldnew28).......
!....     later updated from yh.rad.mdl94(cldmul28)...22jan94
!....     later updated from yh.rad.mdl94(cldml28a)... 1feb94
!....     later updated from yh.rad.mdl94(cldml28b)... 5feb94
!.               subr cldprp replaced
!.               added vertical interp of cld-rh relations(istrat gt 1)
!....     later updated from yh.rad.mdl94(cldml28e)... 11mar94
!.               subr cldprp replaced,gcl adjusted
!....     later updated from cloud6................... 24mar94
!.               subr cldprp , low enhanced to old value..0.14..
!.               subr gclnew , llyr calculation adj to old valu(kl-1)
!.                             llyrl was ok.. ive removed it and
!.                             replaced it by its equivalent, klowb
!....     later updated from cloud6................... 30mar94
!.               subr cldprp , low and middle (not cv) enhanced=0.10
!---------------------------------------------------------------------
!     nov., 1992 - y.h., k.a.c., and a.k.
!        cloud parameterization patterned after slingo and slingo
!        work (jgr, 1991).
!     stratiform clouds are allowed in any layer except the surface
!        and upper stratosphere.  the relative humidity criterion may
!        vary in different model layers.
!yh94
!     output cloud amounts are in cldary(i,k), k=1 is the lowest
!        model layer, stratiform (str) and convective (cnv) types of
!        cloud are compressed into one word: camt = str + 1.0e4*cnv
!        low marine stratus amt are flaged by adding 2.
!yh94
!tune
!..   for istrat = 0, there is rh-cld relation for each layer..
!                      crit rh computed within..
!..   for istrat = 1, rh-cld relation from tables created using
!                     mitchell-hahn tuning technique (a.f. rtneph obs)
!                  ...stratus computed similar to old opnl cldjms.....
!                      except no cloud below layer=klowb..approx 955mb
!tune
!     convective clouds are from model convective scheme and are
!        no longer broken into .75,.25,.25..rather cc itself is used..
!        convective still takes precedence over stratiform in radfs
!                                               .(in radiation use of
!        cc gives improvement to tropical middle cld (as did st+cv))
!
!     clouds are also divided into 3 atmospheric domains (l,m,h)
!          plus a boundary layer cloud for
!        diagnostic purposes.  they are computed from random overlap
!        assumption for separated cloud layers and maximum overlap
!        for adjacent cloud layers.  a total cloud fraction is also
!        computed.
!
!     h,m,l domain pressure tops 'ptop1(k)' vary linearly from
!        'ptopc(k,1)' at 45deg to 'ptopc(k,2)' at the pole
!
!bl 
!     4 cloud relationships used 
!         so that 1-4 is for bl,l,m,h now... clouds down to layer 2
!          everywhere, with no vvel filter for these lower-10percent-atm clds..
!.....  the bl relationships used below llyr, except in marine stratus regions
!         where the old method was used (down to layer 3)..2nov95 .. kac
!.......   old method of marine stratus not used...3mar96
!.....   save bl cldamt (max overlap) in cld( ,5)...11apr96
!bl
!yh0898
!      aug 1998   y-t hou    recoded the default part of slingo
!          cloud scheme patterned after ccm3 (kiehl et al. 1998,j.clim;
!          and 1994,jgr). output rh and conv clouds use seperated
!          arrays cldtot and cldcnv to avoid packing-unpacking process.
!
!--------------------------------------------------------------------
!     input variables:
!        ps (cb)       - surface pressure
!        q  (kg/kg)    - specific humidity
!        t  (deg k)    - absolute temperature
!        vvel(cb/sec)  - vertical velocity
!        cv,cvt,cvb    - conv cld fraction, top, bottom pressure as obtained
!                      - from the convection scheme layers
!        si,sl         - mdl sigma interface and layer mean
!        slmsk         - sea/land mask array(sea:0.,land:1.,snow:2.)
!        ivva          - flag to control vertical velocity adj.
!                        =1: with, =0: without
!        invr          - flag to control lapse rate inversion cld
!                        =1: with, =0: without
!        rhmax         - upper limit of relative humidity to
!                        form overcast cloud (cld fractn = 1.)
!tune
! --------------- modify to as an array (h.-m. h. juang)
!********xlatrd        - current latitude in radians (1st data pt)
!********                 for models with diff lat at each pt, need to
!********                 use the lat of all points....careful.....
!        rhcld         - cloud-rh relations from mitchell+hahn,
!                        using a.f. rtneph analyses
!        istrat        - 0 or 1:for default or 'rhcld' tables
!                        in the stratiform cloud calculation
!tune
!    output variables:
!       cldtot         - vertical column array of stratiform cloud
!       cldcnv         - vertical column array of convective cloud
!       cld            - cld fraction in 3 types of domains (l,m,h)
!bl                          and total in 4th layer and bl takes the 5th
!       mtop,mbot      - top, bottom layers of clouds (l,m,h)
!
!--------------------------------------------------------------------
!
      use machine , only : kind_phys,kind_rad
      use physcons, rocp => con_rocp, grav => con_g, rd => con_rd       &
     &,             pi => con_pi
      use comcd1
      implicit none
!     include 'constant.h'
!
!     real (kind=kind_rad) eps, epsm1, grv
!     parameter (eps=rd/rv, epsm1=rd/rv-1.0, grv=grav)
!
      integer idimt, idims, kdim,ncld
      integer nbin,mcld,ivva,invr,istrat
! --- input variables
!
      real (kind=kind_rad)  cv(idimt),        cvt(idimt),               &
     &                      cvb(idimt),       slmsk(idimt),             &
     &                      prsl(idimt,kdim), prsi(idimt,kdim+1),       &
     &                      t(idims,kdim),    q(idims,kdim),            &
     &                      clw(idimt,kdim),  rhrh(idimt,kdim),         &
     &                      vvel(idimt,kdim), qst(idimt,kdim),          &
     &                      xlatrd(idimt)
! --- output variables
      real (kind=kind_rad) cld(idimt,5),                                &
     &                      cldtot(idimt,kdim),  cldcnv(idimt,kdim)
      integer               mtop(idimt,3), mbot(idimt,3)
!tune
!...    rh-cld relationships for each point
      real (kind=kind_rad) rhcld(idimt,nbin,mcld)
!tune
!
! --- ptopc(k,l): top presure of each cld domain (k=1-4 are sfc,l,m,h;
!       l=1,2 are low-lat (<45 degree) and pole regions)
! ---  workspace ---
!
      integer kdimp,levm1,levm2,klow,k
      integer klev,kc,nx,nhalf,kcvb,kcvt,kk,l,llyr11,kstr,i
      integer kbase, kinver(idimt)
      logical bitx(idimt), bity(idimt), bitm(idimt), bit1, bit2         &
     &,       inversn(idimt)
      real (kind=kind_rad) prsly(idimt,kdim),                           &
     &                     dthdp(idimt,kdim), theta(idimt,kdim),        &
     &                     dtdpm(idimt),      cl1  (idimt),             &
     &                     omeg(idimt),       cl2  (idimt),             &
     &                     ptop1(idimt,4)
      real (kind=kind_rad)  rhh,    rhm,    rhl,    fac,   exnr         &
     &,                     es,     qs,     tmt0,   tmt15, ai, bi, qi   &
     &,                     dptop,  qint,   aa,     bb,     rhcr,  crh  &
     &,                     cfiltr, onemrh, value,  tem,    tem1        &
     &,                     clwmin, clwm,   cr1,    cr2, cr3            &
     &,                     vvc1,   vvc2,   dvvcld                      &
     &,                     crk,     xx                                 &
     &,                     clwt
      integer kcut(idimt), kbt1 (idimt), kth1 (idimt), kbt2 (idimt),    &
     &        kth2(idimt), ityp1(idimt), ityp2(idimt), ksave(idimt),    &
     &        kdomn(idimt)
!bl
!===>    begin here ................................................
      kdimp = kdim + 1
      levm1 = kdim - 1
      levm2 = kdim - 2
      rhh   = 0.95e0                ! critical value for hi  cld
      rhm   = 0.70e0                ! critical value for mid cld
      rhl   = 0.70e0                ! critical value for low cld
!
!...  find top pressure for each cloud domain
      do k=1,4
        dptop = ptopc(k,2) - ptopc(k,1)
        do i=1,idimt
          fac = max(0.0, 4.0*abs(xlatrd(i))/pi-1.0)
          ptop1(i,k) = ptopc(k,1) + dptop * fac
        end do
      end do

! --- low cloud top sigma level, computed for each lat cause
!       domain definition changes with latitude...
      klow=kdim
      do k=kdim,1,-1
        do i=1,idimt
          if (prsi(i,k) .lt. ptop1(i,2) * 1.0e-3) klow = min(klow,k)
        enddo
      enddo
! --- potential temp and layer relative humidity
!
      do k=1,kdim
        do i=1,idimt
          cldtot(i,k) = 0.0
          cldcnv(i,k) = 0.0
          prsly(i,k)  = prsl(i,k) * 10.0
          exnr        = (prsly(i,k)*0.001) ** (-rocp)
          theta(i,k)  = exnr * t(i,k)
        enddo
      enddo

! --- potential temp lapse rate
      do k=1,levm1
        do i=1,idimt
          dthdp(i,k) = (theta(i,k+1) - theta(i,k)) /                    &
     &                 (prsly(i,k+1) - prsly(i,k))
        enddo
      enddo
! ------------------------------------------------------------------
!     find the stratosphere cut off layer for high cloud. it
!      is assumed to be above the layer with dthdp less than
!      -0.25 in the high cloud domain (from looking at 1 case).
! ------------------------------------------------------------------
      do i=1,idimt
        kcut(i) = levm2
      end do
      do k=klow+1,levm2
        bit1 = .false.
        do i=1,idimt
          if (kcut(i).eq.levm2 .and. prsly(i,k).le.ptop1(i,3) .and.     &
     &        dthdp(i,k).lt.-0.25e0) then
            kcut(i) = k
          end if
          bit1    = bit1 .or. kcut(i).eq.levm2
        end do
        if (.not. bit1) go to 85
      end do
  85  continue
!
! --- get convective cloud top and base level, set logical array bitx
!
      bit1 = .false.
      do i=1,idimt
!       bitx(i) = cv(i).gt.0.0e0 .and. cvt(i).ge.cvb(i)
!...   pressure decreases upward
        bitx(i) = cv(i).gt.0.0e0 .and. cvt(i).lt.cvb(i)
        bit1 = bit1 .or. bitx(i)
      end do

      if (bit1 .and. ncld .eq. 0) then
!     if (bit1) then
        do i=1,idimt
          kbt1(i) = 1
          kth1(i) = 1
          if (bitx(i)) then
!           kbt1(i) = nint(cvb(i))
!           kth1(i) = min(levm2, nint(cvt(i)))
!....use layer pressure in mb, as converted to cb
            do k=2,kdim
              tem = prsly(i,k) * 0.1
              if (cvt(i) .le. tem) kth1(i) = k - 1
              if (cvb(i) .le. tem) kbt1(i) = k - 1
            end do
          end if
        end do

! --- put convective cloud into 'cldcnv', no merge at this point..
        do k=2,levm2
          do i=1,idimt
            if (bitx(i) .and. kbt1(i).le.k .and. kth1(i).ge.k) then
              cldcnv(i,k) = cv(i)
!             rhrh(i,k) = max(0.0e0,                                    &
!    &                    rhrh(i,k)-cldcnv(i,k))/(1.0e0-cldcnv(i,k))
            end if
          end do
        end do

!  the following loop commented by moorthi on 12/29/98
!
! --- if mean cvt layer higher than 400mb add anvil cirrus
!       do i=1,idimt
!         if (bitx(i) .and. prsly(i,kth1(i)).le.cvtop) then
!           kk = kth1(i) + 1
!           kk = kth1(i)
!           cldcnv(i,kk) = max(0.0e0, min(1.0e0,                        &
!    &                     2.0e0 * (cv(i) - 0.3e0) ))
!         end if
!       end do
!
      end if
! ------------------------------------------------------------------
      if (istrat.le.0) then
        if (ncld .gt. 0) then
           do k=klowb,levm2
             do i=1,idimt
               onemrh = max(1.e-10,1.-rhrh(i,k))
               value = max(min(1000.*clw(i,k)/onemrh,50.),0.)
               cldtot(i,k) = max(rhrh(i,k)*(1.-exp(-value)),0.)
             enddo
           enddo
        else
! ------------------------------------------------------------------
!       ....default scheme .... patterned after slingo scheme
! ------------------------------------------------------------------
!     calculate large scale stratiform cloud from relative humidity
!     and put into array 'cldtot'
! ------------------------------------------------------------------
      xx = -grav * grav / (0.00035e0*rd)
      do k=klowb,levm2
        do i=1,idimt
          if (prsly(i,k) .ge. ptop1(i,2)) then
! --- low cloud domain
            rhcr = rhl
!           if (nint(slmsk(i)).eq.1)                                    &
!    &        rhcr = rhl - 0.10e0
          else
! --- mid-high cloud domain
            if (prsly(i,k) .ge. ptop1(i,3)) then
              crh = rhm
              if (nint(slmsk(i)) .eq. 1) crh = rhm - 0.10e0
            else
              crh = rhh
              if (nint(slmsk(i)) .eq. 1) crh = rhh - 0.10e0
            end if
            aa = xx*prsly(i,k)*dthdp(i,k) / (t(i,k)*theta(i,k))
            rhcr = 0.999e0 - (1.0e0 - crh)                              &
     &           * (1.0e0 - min(1.0e0, max(0.0e0, aa)) )
          end if
          bb = max(0.0, (rhrh(i,k)-rhcr)/(1.0e0-rhcr))
          cldtot(i,k) = max(cldtot(i,k), min(1.0e0, bb**2))
        end do
      end do

! ------------------------------------------------------------------
!     special treatment on low clouds
! ------------------------------------------------------------------
      do i=1,idimt
        bitx(i) = .false.
        kbt1(i) = klowb-1
      end do

      do k=klowb-1,llyr
      do i=1,idimt
        if (.not.bitx(i)) then
          bitx(i) = nint(slmsk(i)).eq.0                                 &
     &        .and. prsly(i,k).ge.pstrt  .and. dthdp(i,k).le.clapkc     &
     &        .and. rhrh(i,k+1).le.0.6e0 .and. rhrh(i,k+2).le.0.6e0
          kbt1(i) = k
        end if
      end do
      end do

      if (ivva .le. 0) go to 250
! --- vertical velocity adjustment on low clouds
      vvc1 =  0.0000e0
      vvc2 = -0.0006e0                 ! mb/sec
      dvvcld = vvc1 - vvc2
!     dvvcld = vvcld(1) - vvcld(2)
      do k=klowb,klow
        do i=1,idimt
          if (.not.bitx(i) .or. k.gt.llyr) then
            cr1 = min(1.0e0, max(0.0e0,                                 &
     &            (vvcld(1) - 10.0e0*vvel(i,k)) / dvvcld ) )
            cldtot(i,k) = cldtot(i,k) * sqrt(cr1)
          end if
        end do
      end do

 250  if (invr .le. 0) go to 350
! --- t inversion related stratus clouds
      bit1 = .false.
      do i=1,idimt
        bit1 = bit1 .or. bitx(i)
      end do
      if (.not. bit1) go to 350

! --- smooth transition for cloud when dthdp between
!     clapse and clapse+dclps  (-0.05 and -0.06)

      do k=klowb,klow+2
      do i=1,idimt
        if (bitx(i)) then
          cfiltr = max(0.3e0,                                           &
     &             1.0e0 - ((clpse-dthdp(i,kbt1(i))) / dclps) )         
          cldtot(i,k) = min(0.95e0, cldtot(i,k) * cfiltr)
        end if
      end do
      end do

!     do k=klowb-1,klow+3
!       do i=1,idimt
!         k1 = max(klowb-1,kbt1(i)-1)
!         k2 = min(klow+3, kbt1(i)+3)
!         if (k.ge.k1 .and. k.le.k2 .and. vvel(i,k).ge.0.0000e0) then
!           aa = min(1.0e0, max(0.0e0,                                  &
!    &         1.0e0 - (rhl - rhrh(i,k-1)) / (rhl - 0.6e0) ))
!           bb = min(1.0e0, max(0.0e0,                                  &
!    &           (prsly(i,k)-ptop1(i,2)) / (900.0e0-ptop1(i,2)) ))
!           cst = aa * bb * cl2(i)
!           cldtot(i,k) = max(cldtot(i,k), cst)
!         end if
!       end do
!     end do

 350  continue
      end if                          !end  of ncld if
! ------------------------------------------------------------------
      end if                          !end default scheme
! ------------------------------------------------------------------
      if (istrat.gt.0) then
         if (ncld .gt. 0) then        ! liquid water based clouds!
            inversn = .false.
            kinver  = kdim
            do k=2,kdim
              do i=1,idimt
                if (prsly(i,k) .gt. 600.0 .and. (.not. inversn(i))) then
                  tem = t(i,k+1) - t(i,k)
                  if (tem .gt. 0.1 .and. t(i,k) .gt. 278.0) then
                    inversn(i) = .true.
                    kinver(i)  = k
                  endif
                endif
              enddo
            enddo
            clwmin = 0.0e-6
            do k=1,kdim
!             clwt = 1.0e-6 * sl(k)
!             clwt = 2.0e-6 * sl(k)
              do i=1,idimt
                clwt = 1.0e-6 * (prsly(i,k)*0.001)
!               if (clw(i,k) .gt. clwt) then
                if (clw(i,k) .gt. clwt .or.                             &
     &             (inversn(i) .and. k .le. kinver(i)) ) then
                onemrh = max(1.e-10, 1.-rhrh(i,k))
!
                tem   = 1.0 / max(prsly(i,k)*0.001, 0.01)
                clwm  = clwmin * tem
                tem   = min(max(sqrt(sqrt(onemrh*qst(i,k))),0.0001),1.0)
!               tem   = 2500.0 / tem
                tem   = 2000.0 / tem
!               tem   = 1000.0 / tem
!               tem   = 100.0 / tem
!               if (inversn(i) .and. k .le. kinver(i)) tem = tem * 2.0
                if (inversn(i) .and. k .le. kinver(i)) tem = tem * 5.0
                value = max(min(tem*(clw(i,k)-clwm), 50.),0.)
                tem   = sqrt(sqrt(rhrh(i,k)))
                cldtot(i,k) = max(tem*(1.0-exp(-value)), 0.0)
              endif
              enddo
            enddo
! ------------------------------------------------------------------
!     separate clouds into 3 pressure domains (l,m,h). 
!bl    plus bl cloud amount only(5th location).   within each
!     of the domains, assume cloud layers are randomly overlapped.
!     ...random/max til autumn 2001
!     vertical location of each type of cloud is determined by
!     the thickest continuing cloud layers in the domain.
!     convective cloud no-longer used, remove it from diagnostic calc
! -------------------------------------------------------------------
!
! --- loop over 3 cloud domains (l,m,h)
!bl       grabbing bl cloud fraction during l=1
!..
      if (istrat.gt.0) then
!opnl        kstr = llyr
        kstr = llyr+1
      else
        kstr = 2
      end if
!
      do l=1,3
!
!bl    for 1st pass extract the bl cloud..just take random cloud
!                                was max til autumn 2001
        if (l.eq.1) then
          do i=1,idimt
            cld (i,5) = 0.0e0
          end do
!opnl          llyr11=llyr-1
!opnl          do k=2,llyr11
          do k=1,llyr
          do i=1,idimt
            cld(i,5) = cld(i,5)+ cldtot(i,k)-cld(i,5)*cldtot(i,k)
          end do
          end do
        end if
!
        do i=1,idimt
          cld (i,l) = 0.0e0
          mtop(i,l) = 1
          mbot(i,l) = 1
          cl1 (i) = 0.0e0
          cl2 (i) = 0.0e0
          kbt1(i) = 1
          kbt2(i) = 1
          kth1(i) = 0
          kth2(i) = 0
!opnl          bitm(i) = cldtot(i,kstr).gt.0.0e0 .or.
!opnl     &              cldcnv(i,kstr).gt.0.0e0
          bitm(i) = cldtot(i,kstr).gt.0.0e0
        end do
!
        do 700 k=kstr,kdim
!
          bit1 = .false.
          do i=1,idimt
! --- bitx logical array for layer k
            bitx(i) = (prsly(i,k).ge.ptop1(i,l+1)) .and.                &
     &                (prsly(i,k).lt.ptop1(i,l))   .and. bitm(i)
            bit1 = bit1 .or. bitx(i)
! --- bitm logical array for layer k+1
!opnl            bitm(i) = cldtot(i,k+1).gt.0.0e0 .or.                &
!opnl     &                cldcnv(i,k+1).gt.0.0e0
            if (k .lt. kdim) bitm(i) = cldtot(i,k+1).gt.0.0e0
          end do

          if (.not. bit1) go to 700
          do i=1,idimt
!opnl            cr1  = cldtot(i,k)
!opnl            cr2  = cldcnv(i,k)
            if (bitx(i)) then
              if(kth2(i).le.0) then
! --- kth2 le 0 : 1st cld layer.
                kbt2(i) = k
                kth2(i) = 1
              else
! --- kth2 gt 0 : consecutive cld layer.
                kth2(i) = kth2(i) + 1
              endif
! ---  physical cloud as seen by radiation (random)
              cl2(i)=cl2(i) + cldtot(i,k)-cl2(i)*cldtot(i,k)
!opnl              if (cldcnv(i,k).gt.0.0e0) then
!opnl               cl2 (i) = max(cl2(i), cldcnv(i,k))
!opnl              else
!opnl               cl2 (i) = max(cl2(i), cldtot(i,k))
!opnl              end if
            endif
          end do
          bit2 = .false.
!....  bity=true if next lyr=clear or we change cloud domains..
          if (k .ge. kdim) then
            do i=1,idimt
              bity(i) = .true.
              bit2    = .true.
            enddo
          else
            do 650 i=1,idimt
              
              bity(i) = bitx(i) .and. (.not. bitm(i)                    &
     &                           .or.  prsly(i,k+1).lt.ptop1(i,l+1) )
              bit2 = bit2 .or. bity(i)
 650        continue
          endif
          if (.not. bit2) go to 700
! --- at the domain boundary, random overlap.
!     choose the thickest or the largest fraction amt as the cld
!     layer in that domain
          do i=1,idimt
            if (bity(i)) then
              if (cl1(i).gt.0.0e0) then
                kbt1(i) = int( (cl1(i)*kbt1(i) + cl2(i)*kbt2(i))        &
     &                       / (cl1(i) + cl2(i)) )
                kth1(i) = nint( (cl1(i)*kth1(i) + cl2(i)*kth2(i))       &
     &                        / (cl1(i) + cl2(i)) ) + 1
                cl1 (i) = cl1(i) + cl2(i) - cl1(i)*cl2(i)
              else
                kbt1(i) = kbt2(i)
                kth1(i) = kth2(i)
                cl1 (i) = cl2 (i)
              endif
              kbt2(i) = 1
              kth2(i) = 0
              cl2 (i) = 0.0e0
            endif
          end do

 700    continue
! --- finish one domain, save effective clouds
        do i=1,idimt
          cld(i,l) =  cl1(i)
          mtop(i,l) = max(kbt1(i), kbt1(i)+kth1(i)-1)
          mbot(i,l) = kbt1(i)
        end do
      end do         ! end of domain loop (l=1,3)
! -------------------------------------------------------------------
!     calculate total cloud from the multi-lyr cloud array, in a manner
!     as seen by the radiation code...random overlap is used for all lyrs.
! -------------------------------------------------------------------

      do i=1,idimt
        cld(i,4) = 0.e0
      end do
!
      do k=1,kdim
!
        do i=1,idimt
          cr1 = cldtot(i,k)
          cld(i,4) = cld(i,4) + cr1-cld(i,4)*cr1
        end do
      end do
!
      do i=1,idimt
       do k=1,5
        if (cld(i,k).gt.1.) cld(i,k) = 1.
       end do
      end do
         else                           !   old campana clouds!
!
!tune
! ------------------------------------------------------------------
!     calculate stratiform cloud and put into array 'cldtot' using
!       the cloud-rel.humidity relationship from table look-up..where
!       tables obtained using k.mitchell frequency distribution tuning
!bl        (observations are daily means from us af rtneph).....k.a.c.
!bl       tables created without lowest 10 percent of atmos.....k.a.c.
!      (observations are synoptic using -6,+3 window from rtneph)
!       tables are created with lowest 10-percent-of-atmos, and are
!            now used..  25 october 1995 ... kac.
! ------------------------------------------------------------------
!bl.... ... determine marine stratus regions ...not.....
      do 831 i=1,idimt
       bitm(i) = .true.
  831 continue
!  this loop to retrieve cloud from rh rewritten 950113 -mi
!bl      do klev=klowb,levm2
       do klev=2,levm2
        do i=1,idimt
          kdomn(i)=0
          bitx(i)=.false.
        enddo
!mcl3   do kc=mcld,1,-1
        if (klev.gt.llyr) then
         do kc=mcld,2,-1
          do i=1,idimt
!mcl3      if(prsly(i,klev).ge.ptop1(i,kc+1)) kbase(i)=kc
           if(prsly(i,klev).ge.ptop1(i,kc)) kdomn(i)=kc
          enddo
         enddo
!mcl4
        else
         do i=1,idimt
          if (bitm(i)) then
!...if true not a suspected marine stratus region, use bl curves
           kdomn(i)=1
          else
!...if a marine stratus region, reinstate old method (cause very
!        little marine stratus probably went into tuned curves,
!        since rtneph has trouble here)..so use l curves
!....this is by-passed since bitm is set true everywhere..kac
           kdomn(i)=2
          end if  
         enddo
        endif
!mcl4
        nx=0
        nhalf=(nbin+1)/2
        do i=1,idimt
          if(kdomn(i).le.0.or.klev.gt.kcut(i)) then
            cldtot(i,klev)=0.
          elseif(rhrh(i,klev).le.rhcld(i,1,kdomn(i))) then
            cldtot(i,klev)=0.
          elseif(rhrh(i,klev).ge.rhcld(i,nbin,kdomn(i))) then
            cldtot(i,klev)=1.
          else
            bitx(i)=.true.
            ksave(i)=nhalf
            nx=nx+1
          endif
        enddo
        do while (nx.gt.0)
          nhalf=(nhalf+1)/2
          do i=1,idimt
            if(bitx(i)) then
              crk=rhrh(i,klev)
              cr1=rhcld(i,ksave(i),kdomn(i))
              cr2=rhcld(i,ksave(i)+1,kdomn(i))
              if(crk.le.cr1) then
                ksave(i)=max(ksave(i)-nhalf,1)
              elseif(crk.gt.cr2) then
                ksave(i)=min(ksave(i)+nhalf,nbin-1)
              else
                cldtot(i,klev)=0.01*(ksave(i)+(crk-cr1)/(cr2-cr1))
                bitx(i)=.false.
                nx=nx-1
              endif
            endif
          enddo
        enddo
      enddo
! ------------------------------------------------------------------
!     special treatment on low clouds
! ------------------------------------------------------------------
      dvvcld = vvcld(1) - vvcld(2)
!
      do 950 k=2,klow  
!
        do 904 i=1,idimt
          omeg(i) = 10.0e0 * vvel(i,k)
          cl1 (i) = 0.0e0
 904    continue
        if (ivva .le. 0) go to 920
! --- vertical velocity adjustment on low clouds
        bit1 = .false.
        do 906 i=1,idimt
          bitx(i) = prsly(i,k).ge.ptop1(i,2) .and. cldtot(i,k).gt.0.0e0
          bit1 = bit1 .or. bitx(i)
 906    continue
        if (.not. bit1) go to 920
        if(k.gt.llyr) then
          do 910 i=1,idimt
            if (bitx(i)) then
              if(omeg(i).ge.vvcld(1)) then
                cldtot(i,k) = 0.0e0
              else if(omeg(i).gt.vvcld(2)) then
                cr1 = (vvcld(1) - omeg(i)) / dvvcld
!               cldtot(i,k) = cldtot(i,k) * cr1
                cldtot(i,k) = cldtot(i,k) * sqrt(cr1)
              endif
            endif
 910      continue
        endif
!blc --- t inversion related stratus clouds .. not used cause bitm=t
 920    if (invr .lt. 1) go to 950
        if (k.gt.llyr) go to 950
        bit1 = .true.
        do 930 i=1,idimt
          bit1 = bit1 .and. bitm(i)
 930    continue
        if (bit1) go to 950
        do 940 i=1,idimt
         if (.not.bitm(i)) then
!error    if (dthdp(i,kbase(i)).gt.clpse) then
          if (dthdp(i,kdomn(i)).gt.clpse) then
!---   smooth transition for cloud when dthdp between
!           clapse and clapse+dclps  (-0.05 and -0.06)
!error     cfiltr = 1.0e0 - ((clpse - dthdp(i,kbase(i))) / dclps)
           cfiltr = 1.0e0 - ((clpse - dthdp(i,kdomn(i))) / dclps)
           cldtot(i,k) = cldtot(i,k)*cfiltr
          end if
! --- for t inversion type cloud, add flag value of 2.0
!1098     cldary(i,k) = cldary(i,k)+2.0e0
         end if
 940    continue
 950  continue
! ------------------------------------------------------------------
!     separate clouds into 3 pressure domains (l,m,h). 
!bl    plus bl cloud amount only(5th location).   within each
!     of the domains, assume separated cloud layers are randomly
!     overlapped and adjacent cloud layers are maximum overlapped.
!     vertical location of each type of cloud is determined by
!     the thickest continuing cloud layers in the domain.
! -------------------------------------------------------------------
!
! --- loop over 3 cloud domains (l,m,h)
!bl       grabbing bl cloud fraction during l=1
!..
      if (istrat.gt.0) then
        kstr = llyr
      else
        kstr = 2
      end if
!
      do l=1,3
!
!bl    for 1st pass extract the bl cloud..just take max cloud
        if (l.eq.1) then
          do i=1,idimt
            cld (i,5) = 0.0e0
          end do
          llyr11=llyr-1
          do k=2,llyr11
          do i=1,idimt
            cr1  = cldtot(i,k)
            cr2  = cldcnv(i,k)
            cr3 = cr1
            if (cr2.gt.0.e0) cr3 = cr2
            cld(i,5) = max(cld(i,5) , cr3)
          end do
          end do
        end if
!
        do i=1,idimt
          cld (i,l) = 0.0e0
          mtop(i,l) = 1
          mbot(i,l) = 1
          cl1 (i) = 0.0e0
          cl2 (i) = 0.0e0
          kbt1(i) = 1
          kbt2(i) = 1
          kth1(i) = 0
          kth2(i) = 0
          bitm(i) = cldtot(i,kstr).gt.0.0e0 .or.                        &
     &              cldcnv(i,kstr).gt.0.0e0
        end do
!
        do k=kstr,levm2
!
          bit1 = .false.
          do i=1,idimt
! --- bitx logical array for layer k
            bitx(i) = (prsly(i,k).ge.ptop1(i,l+1)) .and.                 &
     &                (prsly(i,k).lt.ptop1(i,l))   .and. bitm(i)
            bit1 = bit1 .or. bitx(i)
! --- bitm logical array for layer k+1
            bitm(i) = cldtot(i,k+1).gt.0.0e0 .or.                        &
     &                cldcnv(i,k+1).gt.0.0e0
          end do

          if (.not. bit1) go to 710
          do i=1,idimt
            cr1  = cldtot(i,k)
            cr2  = cldcnv(i,k)
            if (bitx(i)) then
              if(kth2(i).le.0) then
! --- kth2 le 0 : 1st cld layer.
                kbt2(i) = k
                kth2(i) = 1
              else
! --- kth2 gt 0 : consecutive cld layer.
                kth2(i) = kth2(i) + 1
              endif
! ---  physical cloud as seen by radiation..conv takes precedence
! ---  except anvil cirrus not random overlapped with cv tower as
! ---  in radiation code(so hi may be slight underestimate)....
              if (cldcnv(i,k).gt.0.0e0) then
               cl2 (i) = max(cl2(i), cldcnv(i,k))
              else
               cl2 (i) = max(cl2(i), cldtot(i,k))
              end if
            endif
          end do
          bit2 = .false.
!....  bity=true if next lyr=clear or we change cloud domains..
          do 640 i=1,idimt
            bity(i) = bitx(i) .and. (.not. bitm(i)                      &
     &                         .or.  prsly(i,k+1).lt.ptop1(i,l+1) )
            bit2 = bit2 .or. bity(i)
 640      continue
          if (.not. bit2) go to 710

! --- at the domain boundary or separated cld lyrs, random overlap.
!     choose the thickest or the largest fraction amt as the cld
!     layer in that domain
          do i=1,idimt
            if (bity(i)) then
              if (cl1(i).gt.0.0e0) then
                kbt1(i) = int( (cl1(i)*kbt1(i) + cl2(i)*kbt2(i))        &
     &                       / (cl1(i) + cl2(i)) )
                kth1(i) = nint( (cl1(i)*kth1(i) + cl2(i)*kth2(i))       &
     &                        / (cl1(i) + cl2(i)) ) + 1
                cl1 (i) = cl1(i) + cl2(i) - cl1(i)*cl2(i)
              else
                kbt1(i) = kbt2(i)
                kth1(i) = kth2(i)
                cl1 (i) = cl2 (i)
              endif
              kbt2(i) = 1
              kth2(i) = 0
              cl2 (i) = 0.0e0
            endif
          end do

 710    continue
        end do                   ! end of k-loop
! --- finish one domain, save effective clouds
        do i=1,idimt
          cld(i,l) =  cl1(i)
          mtop(i,l) = max(kbt1(i), kbt1(i)+kth1(i)-1)
          mbot(i,l) = kbt1(i)
        end do
      end do                      ! end of domain loop, l=1,3
! -------------------------------------------------------------------
!     calculate total cloud from the multi-lyr cloud array, in a manner
!     as seen by the radiation code. where, max overlap is used for
!     vertically adjacent cloud layers (both stratiform and convective).
!     for convection, any anvil is considered a separate randomly
!     overlapped cloud..
!
!     cl1, ityp1 store previous layer cloud amount and type,
!     cl2, ityp2 store current layer cloud amount and type.
!     type=0,1,2 for clear layer, stratiform and convective cloud.
! -------------------------------------------------------------------

      do i=1,idimt
        cld(i,4) = 0.e0
        ityp1(i) = 0
        cl1(i) = 0.0e0
      end do
!
      do k=1,kdim
!
        do i=1,idimt
          cl2(i) = 0.0e0
          ityp2(i) = 0
          if (cldtot(i,k) .gt. 0.0e0) then
            cl2(i) = cldtot(i,k)
            ityp2(i) = 1
          end if
          if (cldcnv(i,k) .gt. 0.0e0) then
            cl2(i) = cldcnv(i,k)
            ityp2(i) = 2
          end if
        end do
!
        do i=1,idimt
          if (ityp1(i) .eq. ityp2(i)) then
            if (ityp1(i).eq.2 .and. cl1(i).ne.cl2(i)) then
! --- cnv anvil clouds are treated as separated and radomly
!     overlap
              cld(i,4) = cld(i,4) + cl1(i) - cld(i,4)*cl1(i)
              cl1(i) = cl2(i)
            else if (ityp1(i).eq.1) then
! --- max overlap for non convective adjacent cld layers
              cl1(i) = max(cl1(i), cl2(i))
            end if
          else
! --- diff types cloud, random overlap
            if (cl1(i) .gt. 0.0e0)                                       &     
     &        cld(i,4) = cld(i,4) + cl1(i) - cld(i,4)*cl1(i)
            cl1(i) = cl2(i)
            ityp1(i) = ityp2(i)
          end if
        end do
      end do
!
      do i=1,idimt
        if (cl1(i) .gt. 0.0e0) then
          cr1 = cld(i,4)
          cld(i,4) = cr1 + cl1(i) - cr1*cl1(i)
        end if
      end do
! ------------------------------------------------------------------
        end if                         ! if for liquid water or campana
      end if                           ! istrat gt 0

      return
      end
