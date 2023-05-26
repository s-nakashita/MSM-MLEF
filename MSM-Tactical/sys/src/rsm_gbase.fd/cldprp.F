      subroutine cldprp(idimt,prsl,prsi,t,q                              &  
     &,                 cldtot,cldcnv                                    &
     &,                 icwp,clwp,slmsk,ktop,kbtm,nclds                  &
     &,                 cldlw,taucl,cfac,cldsw,cwp,cip,rew,rei,fice      &
     &,                 tau_rrtm, l, lp1, imax,lprnt)
!fpp$ noconcur r
!---------------------------------------------------------------------
!     feb., 1993 - y.h.
!        cloud radiative properties calculations after davis (1982)
!        and harshvardhan et al. (1987).
!     nov., 1995 - y.h.
!        modified to provide mixed cloud overlapping scheme for
!        chou sw radiation scheme (1992)_.
!     aug., 1998 - y.h.
!        modified to estimate effective radius of water/ice cloud
!        drop, fraction of ice water content, and cloud emissivity
!        for lw radiation calculation (kiehl et al. 1998,j.clim).
!--------------------------------------------------------------------
!     input variables:
!        prsl(i,k)     - layer pressure                  k=1 is toa
!        prsi(i,k)     - interface pressure (cb)         k=1 is toa
!        t (i,k)       - absolute temperature, k=1 is top layer (k)
!        cldtot(i,k)   - stratiform cloud      k=1 is mdl sfc layer
!        cldcnv(i,k)   - convective cloud      k=1 is mdl sfc layer
! ---   modify xlatrd to general for regional and global (h.-m.h. juangk
! ***    xlatrd        - current latitude in radians (1st data pt)
!                         for models with diff lat at each pt, need to
!                         use the lat of each point....careful.....
!        icwp          - flag indicates the method used for cloud
!                        properties, =0 use t-p; =1 use clwp.
!        clwp          - layer cloud water+ice path (g/m**2), k=1 is toa
!        slmsk         - land/sea/ice mask (0:sea.1:land,2:ice)
!    output variables:
! --- clouds for lw rad. k=1 is the sfc, k=2 is the
!      lowest cloud layer, and so on
!        ktop,kbtm(i,k)- cloud top and bottom indeces, ktop and
!                        kbtm values from 1 to l model layers,
!                        with value of 1 being the top mdl layer
!        nclds(i)      - no. of separated cloud layers in a column
!        cldlw(i,k)    - cloud fractions for lw, emissivity adjusted
!  ***   ityp(i,k)     - type of clouds, ityp=1, and 2 are for
!                        the rh, and conv types
! --- clouds for sw rad. k=1 is the top layer or level
!        taucl(i,k)    - cloud optical depth in every model layer
!                        k=1 is the top layer (used for icwp=0 only)
!        cfac(i,k)     - fraction of clear sky view at the layer interfa
!        cldsw(i,k)    - layer cloud fractions for sw
!  - - following are output variables for icwp=1
!        cwp           - layer cloud water path (g/m**2)
!        cip           - layer cloud ice path (g/m**2)
!        rew (i,k)     - effective layer cloud water drop size (micron)
!        rei (i,k)     - effective layer cloud ice drop size (micron)
!        fice(i,k)     - fraction of cloud ice content
!
!--------------------------------------------------------------------
!
!- input array
      use machine , only : kind_rad, kind_phys
      use physcons, grav => con_g, rd => con_rd, fv => con_fvirt
      implicit none
!     include 'constant.h'
!
      integer idimt, imax, l, lp1, icwp
!
      real (kind=kind_rad) cldtot(idimt,l), cldcnv(idimt,l)              &   
     &,                    clwp(idimt,l),   prsl(imax,l)                 &
     &,                    slmsk(idimt),    prsi(imax,lp1)               &
     &,                    t (imax,lp1),    q(imax,l)

! - output array
      real (kind=kind_rad) taucl(imax,l),   cldlw(imax,lp1)              &
     &,                    cldsw(imax,l),   cfac(imax,lp1)               &
     &,                    rew (imax,l),    rei (imax,l)                 &
     &,                    fice(imax,l),    cwp (imax,l)                 &
     &,                    cip (imax,l)
      integer              ktop(imax,lp1),  kbtm(imax,lp1)               &
     &,                    ityp(imax,lp1),  nclds(imax)                  
! ---  workspace ---
      real (kind=kind_rad) xamt(imax), tauc(imax), emis(imax)            &
     &,                     cl1 (imax), cl2 (imax), alfa (imax)
      integer               mtyp(imax), kcld(imax), mbtm (imax)
      logical               bitx(imax), bity(imax), bitw(imax)           &
     &                   ,  bit1,   bit2
      real (kind=kind_rad) delt, wgt, delp, tcld, tau0, awlw, ailw
      real (kind=kind_rad) reimax, reimin, tem, icec
      real (kind=kind_rad) tau_rrtm(imax,l)
      integer i,k,kkcl,mclds,nc,minktp,maxkbt,mkbtp1,kk,nncld            &
     &,       ipts1
      logical lprnt

!===>    begin here ................................................
      do i=1,imax
        kcld(i)    = 2
        mbtm(i)    = 1
        xamt(i)    = 0.0e0
        ityp(i,1)  = 0
        ktop(i,1)  = lp1
        kbtm(i,1)  = lp1
        cldlw(i,1) = 1.0e0
        cfac(i,1)  = 1.0e0
      end do

      do k=2,lp1
        do i=1,imax
          ityp(i,k)  = 0
          ktop(i,k)  = 1
          kbtm(i,k)  = 1
          cldlw(i,k) = 0.0e0
          cfac(i,k)  = 1.0e0
        end do
      end do

      do k=1,l
        do i=1,imax
          cldsw(i,k)   = 0.0e0
          taucl(i,k)   = 0.0e0
          cwp (i,k)    = 0.0e0
          cip (i,k)    = 0.0e0
          rew (i,k)    = 10.0e0
          rei (i,k)    = 10.0e0
          fice(i,k)    = 0.0e0
          tau_rrtm(i,k)= 0.0e0
        end do
      end do

!0499 if (icwp .eq. 1) then ! 0898 - use cloud water/ice content
!       reimax = 80.0
!       reimin = 15.0
!       reimax = 100.0
!       reimin = 50.0
        tem    = (reimax-reimin)/(60.0-20.0)
        do k=1,l
          do i=1,imax
!           delt = 263.16 - t(i,k)
            delt = 273.16 - t(i,k)
            if (nint(slmsk(i)) .eq. 1) then
              rew(i,k) = 5.0e0 + 5.0e0*min(1.0e0, max(0.0e0,             &   
     &                   delt*0.05e0 )) ! - effective radius for water
            end if
!           rei(i,k) = 60.75 + (2.47 - (0.11 - 0.001 * delt) * delt)
!    &                                                       * delt
!           rei(i,k) = reimin + (60.0 - delt) * tem
!           rei(i,k) = max(reimin, min(reimax,rei(i,k)))
!           wgt = min(1.0e0, max(0.0e0,
!    &           (sigl(lp1-k) - 0.1e0) / 0.7e0 ))
!    &           (sigl(lp1-k) - 0.3e0) / 0.5e0 ))
!    &           (sigl(lp1-k) - 0.4e0) / 0.4e0 ))
!           rei(i,k) = 30.0 - 20.0*wgt  ! - effective radius for ice
!           rei(i,k) = 50.0 - 40.0*wgt  ! - effective radius for ice !02/16/2000
!           rei(i,k) = 90.0 - 70.0*wgt  ! - effective radius for ice
            fice(i,k) = min(1.0, max(0.0e0, delt/20.0e0 ))
                                        ! - fraction of ice in cloud
            cip (i,k) = clwp(i,k) * fice(i,k) 
            cwp (i,k) = clwp(i,k) - cip(i,k)
          enddo
        enddo
!       if (lprnt) then
!         print *,' clwp=',clwp
!         print *,' cwp1=',cwp
!         print *,' cip1=',cip
!       endif
!
        do k=1,l
          do i=1,imax
            tem = (grav*prsl(i,k))/ (rd*(prsi(i,k+1)-prsi(i,k)))
            delt = t(i,k) - 273.16
            if (cip(i,k) .gt. 0.0) then
              icec = tem * cip(i,k) / (t(i,k)*(1.0 + fv*q(i,k)))
              if (delt .lt. -50.0) then
                rei(i,k) = (1250.0/9.917) * icec ** 0.109 
              elseif (delt .lt. -40.0) then
                rei(i,k) = (1250.0/9.337) * icec ** 0.08 
              elseif (delt .lt. -30.0) then
                rei(i,k) = (1250.0/9.208) * icec ** 0.055 
              else
                rei(i,k) = (1250.0/9.387) * icec ** 0.031 
              endif
!     if(lprnt .and. k .eq. l) print *,' reil=',rei(i,k),' icec=',icec
!    *,' cip=',cip(i,k),' tem=',tem,' delt=',delt
!             rei(i,k)   = max(10.0, min(rei(i,k), 300.0))
            endif
          enddo
        enddo
!     if(lprnt) print *,' rei1=',rei
!
!0499 end if
!
! --- loop over mdl layers (bottom up)
!
!     do k=2,l
      do k=1,l
!
        do i=1,imax
          cl1(i)  = cldtot(i,k)
          cl2(i)  = cldcnv(i,k)
        end do
!
        bit1 = .false.
        do i=1,imax
          bitx(i) = cl1(i).gt.0.001e0 .or. cl2(i).gt.0.001e0
          bit1    = bit1 .or. bitx(i)
        end do
        if (bit1) then
          do i=1,imax
! --- mtyp=1,2 for rh+stratus, and conv cloud types
            if (cl2(i) .gt. 0.001e0) then
              mtyp(i) = 2
            else if (cl1(i) .gt. 0.001e0) then
              mtyp(i) = 1
            else
              mtyp(i) = 0
            end if
          end do
!
          if (k .lt. l) then
            do i=1,imax
! --- set bitw for clear gap above cloud
!             bitw(i) = bitx(i) .and. cldtot(i,k+1).le.0.001e0
!    &                          .and. cldcnv(i,k+1).le.0.001e0
              bitw(i) = .true.
            end do
          end if
          bit2 = .false.
          do i=1,imax
            bit2 = bit2 .or. bitw(i)
            if (bitx(i)) then
              kkcl = kcld(i)
              if(ityp(i,kkcl) .eq. 0) then
                ityp(i,kkcl) = mtyp(i)
                xamt(i) = cl1(i)
                if (mtyp(i) .eq. 2) xamt(i) = cl2(i)
                mbtm(i) = k
              else if(ityp(i,kkcl).ne.mtyp(i) .or.                       &   
     &               (mtyp(i).eq.2 .and. xamt(i).ne.cl2(i)) ) then
                cldlw(i,kkcl)  = xamt(i)
                ktop(i,kkcl)   = lp1 - (k - 1)
                kbtm(i,kkcl)   = lp1 - mbtm(i)
                ityp(i,kkcl+1) = mtyp(i)
                mbtm(i)        = k
                xamt(i)        = cl1(i)
                if (mtyp(i).eq.2) xamt(i) = cl2(i)
                kcld(i) = kkcl + 1
              else if(mtyp(i).eq.1) then
                xamt(i) = max(xamt(i), cl1(i))
              endif
            end if
          end do
          if (bit2) then
            do i=1,imax
              if (bitw(i)) then
                kkcl = kcld(i)
                cldlw(i,kkcl) = xamt(i)
                ktop(i,kkcl)  = lp1 - k
                kbtm(i,kkcl)  = lp1 - mbtm(i)
                kcld(i)       = kkcl + 1
                mtyp(i)       = 0
                mbtm(i)       = 1
                xamt(i)       = 0.0e0
              end if
            end do
          endif                             ! bit2 endif
        endif                               ! bit1 endif
      enddo                                 ! the k loop ends here!

! --- record num of cld lyrs and find max num of cld lyrs
      mclds = 0
      do i=1,imax
        nclds(i) = kcld(i) - 2
        mclds    = max(mclds, nclds(i))
      end do

!     write(6,221) mclds, ibeg
!221  format(' in cldprp: maxclds =',i4,' ibeg=',i4)
!     if (mclds .eq. 0) return
!
! --- estimate cloud optical properties from t and q  -- (top down)
!
      do nncld=1,mclds
        nc = mclds - nncld + 2
!
        do i=1,imax
          bitx(i) = cldlw(i,nc) .ge. 0.001e0
!         bity(i) = bitx(i) .and. ityp(i,nc).eq.2
          bitw(i) = bitx(i)
        end do

! --- find top pressure for mid cloud (3) domain=function of latitude
        minktp=lp1
        maxkbt=1
        do i=1,imax
          if (bitx(i)) then
            minktp = min(minktp,ktop(i,nc))
            maxkbt = max(maxkbt,kbtm(i,nc))
          end if
        end do
!       write(6,241) nc,minktp, maxkbt
!241    format(3x,'nc, minktp, maxkbt =',3i6/3x,'bitx, bitw :')

! --- find clear sky view at each levels

        do kk=minktp,maxkbt
          do i=1,imax
            if (kk.ge.ktop(i,nc) .and.                                   &  
     &          kk.le.kbtm(i,nc) .and. bitx(i)) then
              cldsw(i,kk) = cldlw(i,nc)
              if (bitw(i)) then
                cfac(i,kk+1) = cfac(i,kk) * (1.0e0 - cldsw(i,kk))
                bitw(i) = .false.
              else
                cfac(i,kk+1) = cfac(i,kk)
              end if
            elseif (kk.gt.kbtm(i,nc) .and. bitx(i)) then
              cfac(i,kk+1) = cfac(i,kk)
            end if
          end do
        end do
!
        mkbtp1 = maxkbt + 1
        do k=mkbtp1,l
          do i=1,imax
            if (bitx(i)) cfac(i,k+1) = cfac(i,mkbtp1)
          end do
        end do
!
        do i=1,imax
          tauc(i) = 0.0e0
        end do

        if (icwp .ne. 1) then

!conv - reduce conv cloud amount for sw rad
          do i=1,imax
            if (ityp(i,nc) .eq. 2) then
!0799         alfa(i) = cldlw(i,nc)
              alfa(i) =                                                  &  
     &            max(0.25e0, 1.0e0-0.125e0*(kbtm(i,nc)-ktop(i,nc)))
            else
!             alfa(i) = sqrt(cldlw(i,nc))
              alfa(i) = 1.0
            end if
          end do
!
! --- calc cld thickness delp and mean temp (celsius)
          do kk=minktp,maxkbt
            do i=1,imax
              if (kk.ge.ktop(i,nc) .and.                                 &  
     &            kk.le.kbtm(i,nc) .and. bitx(i)) then
                delp = 10.0 * (prsi(i,kk+1) - prsi(i,kk))
                tcld = t(i,kk) - 273.16e0
! --- convective cloud
                if (ityp(i,nc) .eq. 2) then
!                 tau0 = delp * 0.05e0    ! yu tai new value
                  tau0 = delp * 0.06e0    ! operational value
!0499 - if conv cld, set to water cloud only
                  fice(i,kk) = 0.0e0
! --- rh clouds
                else
                  if (tcld .le. -10.0e0) then
                    tau0 = delp                                          &   
     &                   * max(0.1e-3, 2.00e-6*(tcld+82.5e0)**2)
                  else
                    tau0 = delp                                          &   
     &                   * min(0.08e0, 6.949e-3*tcld+0.08e0)
                  end if
                end if
                tauc(i)        = tauc(i) + tau0
                taucl(i,kk)    = tau0 * alfa(i)
                tau_rrtm(i,kk) = tau0
              end if
            end do
          end do

! --- calc cld emis and effective cloud cover for lw
           do i=1,imax
            if (bitx(i))                                                 &   
     &        cldlw(i,nc) = cldlw(i,nc)*(1.0e0-exp(-0.75e0*tauc(i)))
          enddo
!
        else                         ! prognostic liquid water
!
! --- calc tauc and emis
          do kk=minktp,maxkbt
            do i=1,imax
              if (kk .ge. ktop(i,nc) .and.                               &  
     &            kk .le. kbtm(i,nc) .and. bitx(i)) then
!               awlw    = 0.070000e0 * cwp(i,kk)
                awlw    = 0.078000e0 * cwp(i,kk)
!               awlw    = 0.090361e0 * cwp(i,kk)
!               ailw    = (0.005e0 + 1.0e0/rei(i,kk)) * cip(i,kk)
                ailw    = (0.0029e0 + 1.0e0/rei(i,kk)) * cip(i,kk)
                tauc(i) = tauc(i) + awlw + ailw
                tau_rrtm(i,kk) = awlw + ailw
              endif
            end do
          end do
          do i=1,imax
            if (bitx(i))                                                 &   
     &        cldlw(i,nc) = cldlw(i,nc) * max(0.0,min(1.0e0,             &
     &                      1.0e0 - exp(-1.66e0*tauc(i)) ) )
            
          enddo
        end if
!
      end do               ! end of nncld loop!
!
!     if(lprnt) print *,' cip2=',cip
!     if(lprnt) print *,' cldsw=',cldsw
      if (icwp .eq. 1) then
        do k=1,l
        do i=1,imax
          if (cldsw(i,k) .lt. 0.001e0) then
            taucl(i,k) = 0.0e0
            cldsw(i,k) = 0.0e0
            cwp  (i,k) = 0.0e0
            cip  (i,k) = 0.0e0
          end if
        end do
        end do
      end if
!     if(lprnt) print *,' cip3=',cip
!
!      write(6,572)
!572   format(/3x,'=== cloud liquid/ice fraction ===')
!      do k=3,l,2
!        write(6,574) k
!574     format(5x,'k =',i4,3x,'i=1,64,12')
!        write(6,576) (fice(i,k),i=1,64,12)
!576     format(10f10.6)
!      enddo
!     write(6,581) ibeg
!581  format(' in cldprp: print lw clouds, ibeg=',i5)
!     do 586 k=2,mclds+1
!       write(6,582) k
!582    format(' from sfc and up, cld num =',i5,' cldlw,emis,top,bot')
!       write(6,583) (cldlw(i,k),emis(i,k),ktop(i,k),
!    1                kbtm(i,k),i=1,imax)
!583    format(6(2f6.3,2i4))
!586  continue
!
!     write(6,591)
!591  format(' in cldprp: print sw clouds')
!     do 596 k=kstrt,l
!       write(6,592) k
!592    format(' from top and dn, lev num =',i5,' cldsw,cfac,tau')
!       write(6,593) (cldsw(i,k),cfac(i,k+1),taucl(i,k),i=1,imax)
!593    format(6(3f6.3,2x))
!596  continue
!
      return
      end
