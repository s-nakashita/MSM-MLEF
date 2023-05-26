!     subroutine spa88 computes exact cts heating rates and fluxes and
!  corresponding cts emissivity quantities for h2o,co2 and o3.
!          inputs:                (module blocks)
!       acomb,bcomb,apcm,bpcm                  bdcomb
!       atpcm,btpcm,betacm                     bdcomb
!       betinw                                 bdwide
!       temp,press                             radisw
!       var1,var2,p,delp,delp2                 kdacom
!       totvo2,to3sp,to3spc                    tfcom
!       co2sp1,co2sp2,co2sp                    tfcom
!       cldfac                                 cldcom
!       sko2d                                  tabcom
!       sorc,csour                             srccom
!           outputs:
!       excts,ctso3                            tfcom
!       gxcts                                  rdflux
!           called by:
!       fst88
!            calls:
!
      subroutine spa88(excts,ctso3,gxcts,sorc,csour,                     &  
     &                 cldfac,temp,press,var1,var2,                      &
     &                 p,delp,delp2,totvo2,to3sp,to3spc,                 &
     &                 co2sp1,co2sp2,co2sp                               &
     &,                l, lp1, imax)
!fpp$ noconcur r
!
      use machine , only : kind_rad
      use hcon
      use rnddta
      implicit none
!
      integer l, lp1, imax
!
      real (kind=kind_rad) sorc(imax,lp1,nbly), csour(imax,lp1)          &  
     &,                     cldfac(imax,lp1,lp1)                         &
     &,                     temp(imax,lp1),      press(imax,lp1)         &
     &,                     var1(imax,l),        var2(imax,l)            &
     &,                     p(imax,lp1),         delp(imax,l)            &
     &,                     delp2(imax,l),       totvo2(imax,lp1)        &
     &,                     to3spc(imax,l),      to3sp(imax,lp1)         &
     &,                     co2sp1(imax,lp1),    co2sp2(imax,lp1)        &
     &,                     co2sp(imax,lp1),     excts(imax,l)           &
     &,                     ctso3(imax,l),       gxcts(imax)
!
      real (kind=kind_rad) phitmp(imax,l),      psitmp(imax,l)           &
     &,                     tt(imax,l),          ctmp(imax,lp1)          &
     &,                     x(imax,l),           y(imax,l)               &
     &,                     topm(imax,l),        topphi(imax,l)          &
     &,                     ctmp3(imax,lp1),     ctmp2(imax,lp1)
!
      real (kind=kind_rad) f, ff, ag, agg, fac1, fac2, tem
      integer lm1, k, i, ib
!
      lm1 = l - 1
!
!---compute temperature quantities for use in program
      do k=1,l
        do i=1,imax
          x(i,k) = temp(i,k) - h25e2
          y(i,k) = x(i,k) * x(i,k)
!
!     initialize some arrays
!
          excts(i,k) =  0.0
        enddo
      enddo
!---initialize ctmp(i,1),ctmp2(i,1),ctmp3(i,1) to unity; these are
!   transmission fctns at the top.
      do i=1,imax
        ctmp(i,1)  = one
        ctmp2(i,1) = 1.
        ctmp3(i,1) = 1.
        gxcts(i)   = 0.0

!       for clear sky
      enddo
!
!***begin loop on frequency bands ***
!
!-----calculation for band 1 (combined band 1)
!-----calculation for band 2 (combined band 2)
!-----calculation for band 3 (combined band 3)
!-----calculation for band 4 (combined band 4)
!-----calculation for band 5 (combined band 5)
!-----calculation for band 6 (combined band 6)
!-----calculation for band 7 (combined band 7)
!-----calculation for band 8 (combined band 8)
!-----calculation for band 9 ( 560-670 cm-1; includes co2) 
!-----calculation for band 10 (670-800 cm-1; includes co2)
!-----calculation for band 11 (800-900 cm-1)
!-----calculation for band 12 (900-990 cm-1)
!-----calculation for band 13 (990-1070 cm-1; includes o3))
!-----calculation for band 14 (1070-1200 cm-1)
!
      do ib=1,14
!
!
!---    calculation for single band (combined band)
!
!---    obtain temperature correction (capphi,cappsi),then multiply
!       by optical path (var1,var2) to compute temperature-corrected
!       optical path and mean pressure for a layer (phitmp,psitmp)
!
        do k=1,l
          do i=1,imax
            f           = h44194m2*(apcm(ib)*x(i,k)+bpcm(ib)*y(i,k))
            ff          = h44194m2*(atpcm(ib)*x(i,k)+btpcm(ib)*y(i,k))
            ag          = (h1p41819+f)*f   + one
            agg         = (h1p41819+ff)*ff + one
!
            ag          = ag * ag      !  ag ** 2
            ag          = ag * ag      !  ag ** 4
            ag          = ag * ag      !  ag ** 8
            agg         = agg * agg
            agg         = agg * agg
            agg         = agg * agg
!
            phitmp(i,k) = var1(i,k) * (ag*ag)  ! ag ** 16
            psitmp(i,k) = var2(i,k) * (agg*agg)
          enddo
        enddo
!---    obtain optical path,mean pressure from the top to the pressure
!       p(k) (topm,topphi)
        do i=1,imax
          topm(i,1)   = phitmp(i,1)
          topphi(i,1) = psitmp(i,1)
        enddo
        do k=2,l
          do i=1,imax
            topm(i,k)   = topm(i,k-1)   + phitmp(i,k)
            topphi(i,k) = topphi(i,k-1) + psitmp(i,k)
          enddo
        enddo
!---    tt is the cloud-free cts transmission function
        if (ib .lt. 5) then
          do k=1,l
            do i=1,imax
              fac1      = acomb(ib)*topm(i,k)
              fac2      = fac1*topm(i,k)/(bcomb(ib)*topphi(i,k))
              tt(i,k)   = exp(hm1ez*fac1/sqrt(1.+fac2))
            enddo
          enddo
        elseif (ib .lt. 9 .or. (ib .gt. 10 .and. ib .ne. 13)) then
          do k=1,l
            do i=1,imax
              fac1      = acomb(ib)*topm(i,k)
              fac2      = fac1*topm(i,k)/(bcomb(ib)*topphi(i,k))
              tt(i,k)   = exp(hm1ez*(fac1/sqrt(1.+fac2)+                 &  
     &                    betacm(ib)*totvo2(i,k+1)*sko2d))
          enddo
          enddo
        elseif (ib .eq. 9) then
          do k=1,l
            do i=1,imax
              fac1      = acomb(ib)*topm(i,k) 
              fac2      = fac1*topm(i,k)/(bcomb(ib)*topphi(i,k)) 
              tt(i,k)   = exp(hm1ez*(fac1/sqrt(1.+fac2)+                 &
     &                    betacm(ib)*totvo2(i,k+1)*sko2d))*co2sp1(i,k+1)
            enddo
          enddo
        elseif (ib .eq. 10) then
          do k=1,l
            do i=1,imax
              fac1      = acomb(ib)*topm(i,k)  
              fac2      = fac1*topm(i,k)/(bcomb(ib)*topphi(i,k))  
              tt(i,k)   = exp(hm1ez*(fac1/sqrt(1.+fac2)+                 &
     &                    betacm(ib)*totvo2(i,k+1)*sko2d))*co2sp2(i,k+1)
            enddo
          enddo
        elseif (ib .eq. 13) then
          do k=1,l
            do i=1,imax
              fac1      = acomb(ib)*topm(i,k)  
              fac2      = fac1*topm(i,k)/(bcomb(ib)*topphi(i,k))  
              tt(i,k)   = exp(hm1ez*(fac1/sqrt(1.+fac2)+                 &
     &                    betacm(ib)*totvo2(i,k+1)*sko2d +to3spc(i,k)))
            enddo
          enddo
        endif
        do k=1,l  
          do i=1,imax
            ctmp(i,k+1)  = tt(i,k)*cldfac(i,k+1,1)  
          enddo
        enddo
!
!---    excts is the cts cooling rate accumulated over frequency bands
        do k=1,l
          do i=1,imax
            excts(i,k)  = excts(i,k)                                     &
     &                  + sorc(i,k,ib) * (ctmp(i,k+1)-ctmp(i,k))
          enddo
        enddo
!---    gxcts is the exact cts top flux accumulated over frequency bands
        do i=1,imax
          tem = tt(i,l)*sorc(i,l,ib)+                                    &  
     &       (haf*delp(i,l)*(tt(i,lm1)*(p(i,lp1)-press(i,l)) +           &
     &       tt(i,l)*(p(i,lp1)+press(i,l)-two*p(i,l)))) *                &
     &       (sorc(i,lp1,ib)-sorc(i,l,ib))
          gxcts(i)  = gxcts(i)  + tem * cldfac(i,lp1,1)
        enddo
!
      enddo                         ! band loop ends here!
!
!
!   obtain cts flux at the top by integration of heating rates and
!   using cts flux at the bottom (current value of gxcts). note
!   that the pressure quantities and conversion factors have not
!   been included either in excts or in gxcts. these cancel out, thus
!   reducing computations!
!
      do k=1,l
        do i=1,imax
          gxcts(i)  = gxcts(i)  - excts(i,k)
        enddo
      enddo
!
!   now scale the cooling rate (excts) by including the pressure
!   factor (delp) and the conversion factor (radcon)
!
!     do i=1,imax*l
!       excts(i,1)  = excts(i,1) *radcon*delp(i,1)
!     enddo
!---this is the end of the exact cts computations; at this point
!   excts has its appropriate value.
!
!*** compute approximate cts heating rates for 15um and 9.6 um bands
!     (ctso3)
      do k=1,l
        do i=1,imax
          ctmp2(i,k+1)  = co2sp(i,k+1) * cldfac(i,k+1,1)
          ctmp3(i,k+1)  = to3sp(i,k) * cldfac(i,k+1,1)
        enddo
      enddo
      do k=1,l
        do i=1,imax
!         ctso3(i,k) = radcon*delp(i,k)*
          ctso3(i,k) = csour(i,k)*(ctmp2(i,k+1)-ctmp2(i,k)) +            &  
     &                 sorc(i,k,13)*(ctmp3(i,k+1)-ctmp3(i,k))
!
        enddo
      enddo
!
      return
      end
