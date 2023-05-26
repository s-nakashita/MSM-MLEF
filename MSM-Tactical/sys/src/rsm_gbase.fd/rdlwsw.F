      subroutine rdlwsw(ix,ix2,prsi,prsl,solc,slmsk,                     &         
     &                  sph,tt,ozn,coszen,cldtot,cldcnv,                 &
     &                  sph2,  tt2,   ozn2,   iovr,                      &
!    &                  sph2,  tt2,   ozn2,   pri2, prl2, iovr,          &
     &                  albvb,albnb,albvd,albnd,                         &
     &                  kprf,idxc,cmix,denn,rh,                          &
     &                  lsswr,lslwr,iswsrc,icwp,clwp,icfc, lw_type,      &
     &                  htlw,slwup,slwdn,tlwup,                          &
     &                  htsw,sswup,sswdn,tswup,tswdn,                    &
     &                  sswfvb,sswfnb,sswfvd,sswfnd                      &
!
     &,                 hsigma, rco2                                     &
     &,                 l,lp1,lp1v,llp1,nsrc,nbd,nvb,nae,nbly,nxc,ndn    &
     &,                 ndm, nvect, sgtmp,co21d,co22d, co21d3,co21d7     &
     &,                 source,dsrce                                     &
     &,                 haer, idm, taur                                  &
     &,                 lprnt, me)
!fpp$ noconcur r
!  include dbrdlwsw ;
!  ********************************************************************
!  *  rewrite the gfdl code "radfs" to call m.d.chou sw radiation
!  *    and to call the gfdl lw radiation         y-t hou dec1994   
!  *
!  *  argument list:
!  *    input
!  *      ix,ix2      - first dimensions of arrays
!  *      prsi        - model pressure (cb) at interface level (k=1 at the top)
!  *      sigl        - model layer mean value of pressure (cb)
!  *      solc        - solar constant in ly/min
!  *      rsin1,rcos1,rcos2
!  *                  - sin and cos lat for ozone interpolations
!  *      slmsk       - land/sea/ice mask (0:sea.1:land,2:ice)
!  *      sph         - sepcific humidity in g/g   (k=1 at the top)
!  *      tt          - temperature in k           (k=1 at the top)
!  *      ozn         - ozone concentration in g/g (k=1 at the top)
!  *      coszen      - cosine of zenith angle
!  *      cldtot      - large scale stratiform cloud (k=1 at sfc)
!  *      cldcnv      - convective cloud             (k=1 at sfc)
!  *      albedo      - surface albedo from climotology
!  *      albvb,albvd - vis band albedoes for beam and diff radiation
!  *      albnb,albnd - nir band albedoes for beam and diff radiation
!  *      kprf        - tropospheric aerosol profile type index
!  *      idxc,cmix   - aerosol component index and mixing ratio
!  *      denn        - aerosol number densities of 1st and 2nd layers
!  *      rh          - relative humidity
!  *    control flags
!  *      lsswr,lslwr
!  *                  - sw, lw radiation calls duration in hour
!  *      iswsrc      - flags for selection of sw absorbers
!  *                    1:aerosols, 2:o2, 3:co2, 4:h2o, 5:o3
!  *                    =0:without; =1 with
!  *    output
!  *      htlw        - lw heating rates in k/sec
!  *      slwup       - sfc upward lw flux in w/m**2
!  *      slwdn       - sfc downward lw flux in w/m**2
!  *      tlwup       - toa upward lw flux in w/m**2
!  *      htsw        - sw heating rates in k/sec
!  *      sswup       - sfc upward sw flux in w/m**2
!  *      sswdn       - sfc downward sw flux in w/m**2
!  *      tswup       - toa upward sw flux in w/m**2
!  *      tswdn       - toa downward sw in w/m**2
!  *      tlwup0      - clear sky toa upward lw flux
!         tswup0      - clear sky toa upward sw flux
!  *      sswdn0      - clear sky sfc downward sw flux
!  *      sswup0      - clear sky sfc upward sw flux
!  *      slwdn0      - clear sky sfc downward lw flux
!  *      sswfvb      - vis beam down sw flux at sfc in w/m**2
!  *      sswfvd      - vis diff down sw flux at sfc in w/m**2
!  *      sswfnb      - nir beam down sw flux at sfc in w/m**2
!  *      sswfnd      - nir diff down sw flux at sfc in w/m**2
!  ********************************************************************
!
!
      use machine , only : kind_rad, kind_phys
      use physcons, rd => con_rd, fv => con_fvirt, grav => con_g,        &  
     &              cp => con_cp
      use module_radlw_main,  only : lwrad
!     use module_radlw_rrtm,  only : lwr01
      implicit none
!
      logical lprnt
      logical lsswr, lslwr
      integer l,lp1,lp1v,llp1,nsrc,nbd,nvb,nae,nbly,nxc,ndn              &
     &,       ndm, nvect, ix, ix2, icwp, icfc, lw_type, me
      integer idm(l,nae)
      real (kind=kind_rad) haer(ndm,nae), dz(ix,l), hz(ix,l+1)           &   
     &,                     taur(l,nbd)
!
! --- input from from calling program
      real (kind=kind_rad)      prsi(ix,lp1),  prsl(ix,l)                &
     &,         sph(ix2,l),     tt(ix2,lp1),   ozn(ix2,l)                &
     &,         cldtot(ix2,l),  cldcnv(ix2,l), coszen(ix)                &
     &,         clwp(ix2,l),    slmsk(ix)                                &
     &,         albvb(ix),      albnb(ix),     albvd(ix), albnd(ix)      &
     &,         cmix(nxc,ix),   denn (ndn,ix), rh(ix,l)                  &
!        needed for rrtm                                            
     &,         sph2(ix2,l),    tt2(ix2,l),    ozn2(ix2,l)
!    &,         pri2(ix,lp1),   prl2(ix,l)
!
      integer   iswsrc(nsrc),   idxc(nxc,ix),  kprf(ix)
!
      real (kind=kind_rad) solc,          hsigma,   rco2
!     real (kind=kind_phys) rd, fv, grav, cp
      real (kind=kind_rad) sgtmp(lp1,2),  co21d(l,6)                     &
     &,  co22d(lp1,lp1,6),  co21d3(lp1,6), co21d7(lp1,6)
!
      real (kind=kind_rad) source(28,nbly), dsrce(28,nbly)
!
                                ! --- output to calling program 
      real (kind=kind_rad) htlw(ix2,l), slwup(ix), slwdn(ix), tlwup(ix)  &
     &,        htsw(ix2,l), sswup(ix), sswdn(ix), tswup(ix), tswdn(ix)   &
                                ! --- four components of downward sw flux
     &,        sswfvb(ix),  sswfnb(ix), sswfvd(ix), sswfnd(ix)           
                                ! --- local arrays for radiative quantities
      real (kind=kind_rad)  tswu0(ix), sswd0(ix), sswu0(ix)              &
     &,                     hlw(ix,l), hsw(ix,l)
                                ! --- local arrays for clouds
      integer nclds(ix), ktop (ix,lp1), kbtm  (ix,lp1)
      real (kind=kind_rad)  cfacsw(ix,lp1), cfaclw(ix,lp1,lp1)           &
     &,                     cldlw(ix,lp1),  cldsw (ix,l),   taucl (ix,l) &
!1098
     &,                     cwp  (ix,l),    cip   (ix,l),   fice  (ix,l) &
     &,                     rew  (ix,l),    rei   (ix,l)
! --- local arrays for others
      real (kind=kind_rad)  prssi(ix,lp1), prssl(ix,lp1), temp(ix,lp1)   &
     &,                     qh2o(ix,l),    qo3(ix,l),     wk1(ix)        &
     &,                     ssolar,        tem
!
      integer ir(ix), i, k, nday, kk, iaa
      real tau_rrtm(ix,l)
!
!...  for rrtm lw code
!     integer iovr, icldw, icldi
      integer iovr
      real (kind=kind_rad) ch4vmr(ix),     cn2ovmr(ix),   o2vmr(ix),     &
     &                     co2vmr(ix),     cf11vmr(ix),   cf12vmr(ix),   &
     &                     cf22vmr(ix),    ccl4vmr(ix),                  &
     &                     sntlw0(ix),     hlw0(ix,l),                   &
!    &                     fxlwu0(ix,lp1), fxlwd0(ix,lp1),               &
!    &                     fxlwup(ix,lp1), fxlwdn(ix,lp1),               &
     &                     tlev(ix,lp1),   tauclw(ix,l),  cllw(ix,l),    &
     &                     alogpl(ix,l),   alogpi(ix,lp1),               &
     &                     tlwu0(ix),      slwd0(ix)
      real (kind=kind_rad) cwp2(ix,l),cip2(ix,l),rew2(ix,l),rei2(ix,l)
!...  for rrtm lw code

!
!       solc,the solar constant is scaled to a more current value.
!       i.e. if solc=2.0 ly/min then ssolar=1.96 ly/min. then
!       convert unit to w/m**2
!
      ssolar = solc * 0.980e0 * 6.976670e2
!
!     ncep surface pressure is in cb.  gfdl lw code is in cgs units.
!     pressure for this code should be in dynes/cm**2.  for the sw code
!     it should be in mb.
!
      do i=1,ix
        prssl(i,lp1) = 1.0e4 * prsi(i,lp1)
        prssi(i,lp1) = 10.0  * prsi(i,lp1)
        temp(i,lp1)  = tt(i,lp1)
        wk1(i)       = 1.0e-6
!       wk1(i)       = 3.0e-6
      enddo
!
      do k=1,l
        do i=1,ix
          prssl(i,k) = 1.0e4 * prsl(i,k)      ! layer pressure 
          prssi(i,k) = 10.0  * prsi(i,k)      ! pressure at interface
          temp(i,k)  = tt(i,k)
!         qh2o(i,k)  = max(3.0e-6,  sph(i,k))
!         qh2o(i,k)  = max(1.0e-10,  sph(i,k))
          qh2o(i,k)  = max(wk1(i),  sph(i,k))
          wk1(i)     = min(1.0e-5,  qh2o(i,k))
          qo3(i,k)   = max(1.0e-10, ozn(i,k))
        enddo
      enddo
!
!******************************************************
!         cloud optical properties section
!******************************************************
!
!     write(6,102) lat,ibeg
!102  format(2x,'calling cldprp for lat, ibeg =',2i6)
       call cldprp(ix2,prsl,prsi,temp,qh2o,cldtot,cldcnv                 &
     &,            icwp,clwp,slmsk,ktop,kbtm,nclds                       &  
     &,            cldlw,taucl,cfacsw,cldsw                              &
     &,            cwp,cip,rew,rei,fice, tau_rrtm                        &
     &,            l, lp1, ix,lprnt)

!
      if (lsswr) then   ! calling m-d chou based sw radiation
!
!
!===> ... set timer for sw rad call
!
!       t00  = second()
!       ovhd = second() - t00
!       tbef = second()
!
!
        nday = 0
        do i=1,ix
          if (coszen(i) .gt. 0.0) then
            nday = nday + 1
            ir(nday) = i
          endif
        enddo
!
!       write(6,22) nday
! 22    format(2x,' in rdlwsw : nday=',i4)
!
        do k=1,l
          do i=1,ix
            hsw(i,k) = 0.0
          enddo
        enddo
        do i=1,ix
          tswup(i)  = 0.0
          tswdn(i)  = 0.0
          sswup(i)  = 0.0
          sswdn(i)  = 0.0
          sswfvb(i) = 0.0
          sswfvd(i) = 0.0
          sswfnb(i) = 0.0
          sswfnd(i) = 0.0
          tswu0(i)  = 0.0
          sswu0(i)  = 0.0
          sswd0(i)  = 0.0
        enddo
!
        if (nday .gt. 0) then
          if (iswsrc(1) .ge. 1) then
!
!      compute dz, and hz
!
            do i=1,ix
              hz(i,l+1) = 0.0
            enddo
            do k=l,2,-1
              do i=1,ix
                hz(i,k) = hz(i,k+1) - log(prssi(i,k)/prssi(i,k+1))       &
     &                  * tt(i,k) * (1.0+fv*qh2o(i,k)) * (rd*0.001/grav)
                dz(i,k) = hz(i,k) - hz(i,k+1)
              enddo
            enddo
            do i=1,ix
              hz(i,1) = hz(i,2) - log(prsl(i,1)/prsi(i,2)) * tt(i,1)     &
     &                          * (1.0+fv*qh2o(i,1)) * (rd*0.002/grav)
              dz(i,1) = hz(i,1) - hz(i,2)
            enddo
          endif
!
          call swr95(ssolar,iswsrc,prssi,                                &
     &               temp,qh2o,qo3,rco2,coszen,taucl,                    &
     &               cldsw,cfacsw,icfc,icwp,cwp,cip,rew,rei,fice,        &
     &               albvb,albvd,albnb,albnd,kprf,idxc,cmix,denn,rh,     &
     &               hsw,tswup,tswdn,sswup,sswdn,                        &
     &               tswu0,sswu0,sswd0,                                  &
     &               sswfvb,sswfvd,sswfnb,sswfnd, nday, ir               &
     &,              l, lp1, ix, nsrc, nbd, nvb, nae, ndm, nxc, ndn      &
     &,              haer, idm, dz, hz, taur, me)
        endif
!
!       taft = second()
!       cput = taft - tbef - ovhd
!       write(6,182) cput
!182    format(/3x,'cpu time used by sw call (in sec) =',e16.8)
!
!===> ... save toa and sfc fluxes in w/m**2
        do k=1,l
          do i=1,ix
            htsw(i,k) = hsw(i,k)
          enddo
        enddo
      endif                   ! end of sw calculation
!
      if (lslwr) then  ! calling gfdl longwave radiation
        if (lw_type .eq. 0) then
!
!         tbef = second()
!
          call clo89(cfaclw,cldlw,nclds,kbtm,ktop                        &  
     &,                    l, lp1, ix, nvect)

!
          call lwr88(hlw,slwdn,tlwup,                                    &
     &           prssl,temp,qh2o,qo3,cfaclw,cldlw,nclds,ktop,kbtm        &
     &,          l, lp1, lp1v, llp1, ix                                  &
!
     &,          sgtmp(1,1),   sgtmp(1,2)                                & 
     &,          co21d(1,1),   co21d(1,2),   co21d(1,3),   co21d(1,4)    &
     &,          co21d(1,5),   co21d(1,6)                                &
!
     &,          co22d(1,1,1), co22d(1,1,2), co22d(1,1,3), co22d(1,1,4)  &
     &,          co22d(1,1,5), co22d(1,1,6)                              &
!
     &,          co21d3(1,1),  co21d3(1,2),  co21d3(1,3),  co21d3(1,4)   &
     &,          co21d3(1,5),  co21d3(1,6)                               &
!
     &,          co21d7(1,1),  co21d7(1,2),  co21d7(1,3),  co21d7(1,4)   &
     &,          co21d7(1,5),  co21d7(1,6)                               &
!
     &,          source,dsrce)
!
!         taft = second()
!         cput = taft - tbef - ovhd
!         write(6,302) cput
!302      format(/3x,'cpu time used by lw call (in sec) =',e16.8)
!
!===> ... save toa and sfc fluxes in w/m**2
          do i=1,ix
            tlwup (i) = 1.0e-3*tlwup(i)
            tem       = temp(i,lp1) * temp(i,lp1)
            slwup (i) = 1.0e-3*hsigma * tem * tem
            slwdn (i) = slwup(i) - 1.0e-3*slwdn(i)
          enddo
          do k=1,l
            do i=1,ix
              htlw(i,k) = hlw(i,k)
            enddo
          enddo
        endif                     ! call to gfdl longwave done
!
        if (lw_type .eq. 1) then  ! calling rrtm longwave radiation code
!
!**************************************************
!         calling aer long wave radiation routine
!**************************************************
!       initialize special trace gases
!         do k=1,l
!           do i=1,ix
!             ch4(i,k)   = 0.
!             cn2o(i,k)  = 0.
!             cfc11(i,k) = 0.
!             cfc12(i,k) = 0.
!           enddo
!         enddo
!
! --- the liq/ice cloud property control flags are set
!     as parameters in module_radlw_cntr_para.
!
!     if (icwp .eq. 0) then ! rh based clouds
!       icldw = 0           ! cld fraction and opt. prop. given
!     else
!       icldw = 3           !  based on hu and stamnes (93)
!
!       icldi = 1           !  based on ebert and curry (92)
!     endif
!
      co2vmr  = rco2
!     ch4vmr  = 1.5e-6
!     cn2ovmr = 3.1e-7
!     o2vmr   = 2.09e-1
!     cf11vmr = 3.520e-10
!     cf12vmr = 6.358e-10
!     cf22vmr = 1.500e-10
!     ccl4vmr = 1.397e-10

!     co2vmr  = 3.55e-4
      ch4vmr  = 0.0
      cn2ovmr = 0.0
      o2vmr   = 0.0
      cf11vmr = 0.0
      cf12vmr = 0.0
      cf22vmr = 0.0
      ccl4vmr = 0.0

!===> ... re-assign variables for aer lw code, where
!         k=1 is boundry layer as in forecast model, and
!         temp is in deg k, pressure in mb, and calculate
!         temperatures and pressures at layer interfaces using
!         linear in ln p for temperatures.
          do i=1,ix
            prssi(i,1) = 10. * prsi(i,lp1)
            tlev (i,1) = tt(i,lp1)
          enddo
          do k=1,l
            kk = lp1 - k
            do i=1,ix
              temp (i,k)   = tt2(i,k)
              prssl(i,k)   = 10.0 * prsl(i,kk)
              prssi(i,k+1) = 10.0 * prsi(i,kk)
              alogpl(i,k)  = log(prssl(i,k))
              alogpi(i,k)  = log(prssi(i,k))
            enddo
          enddo
!...      get interface temperatures (linear in ln p)
          do k=1,l-1
            do i=1,ix
              tlev (i,k+1) = tt2(i,k) + (tt2(i,k+1)-tt2(i,k)) *          & 
     &                      (alogpi(i,k+1)-alogpl(i,k))/                 &
     &                      (alogpl(i,k+1)-alogpl(i,k))
            enddo
          enddo
          do i=1,ix
            tlev (i,lp1) = tt2(i,l)
            wk1(i)       = 1.0e-6
          enddo
!...  flip vertical coordinate when ncecessary so that
!                                   k=1 is bottom layer
          do k=1,l
            kk = lp1 - k
            do i=1,ix
!             qh2o(i,k)  = max(3.0e-6,  sph2(i,k))
              qh2o(i,kk) = max(wk1(i),  sph(i,k))
              wk1(i)     = min(1.0e-5,  qh2o(i,kk))
              qo3(i,k)   = max(1.0e-10, ozn2(i,k))
              tauclw(i,k)= tau_rrtm(i,kk)
              cllw (i,k) = cldsw(i,kk)
              cwp2 (i,k) = cwp(i,kk)
              cip2 (i,k) = cip(i,kk)
              rew2 (i,k) = rew(i,kk)
              rei2 (i,k) = rei(i,kk)
            enddo
          enddo
!

          call lwrad                                                    &
!  ---  inputs:
     &     ( prssl,prssi,temp,tlev,qh2o,qo3,co2vmr,ch4vmr,              &
     &       cn2ovmr,o2vmr,cf11vmr,cf12vmr,cf22vmr,ccl4vmr,             &
     &       cllw,cwp2,cip2,rew2,rei2,tauclw,iovr,icwp,                 &
!aer &       kprf,idxc,cmix,denn,rh,hz,dz,idm,                          &
     &       ix, l, lp1, lprnt, me,                                     &
!  ---  outputs:
     &       hlw,tlwup,slwup,slwdn,tlwu0,slwd0                          &
!  ---  optional:
!    &,      fxlwup,fxlwdn,fxlwu0,fxlwd0,hlw0                           &
     &     )

!
!===> ... heating rates in deg/day, convert to deg/sec in subr grrad
!...      aer data does not need to be flipped
!     it is flipped anyway because here everything is from top
!                                     --     moorthi
          do k=1,l
            kk = lp1 - k
            do i=1,ix
              htlw(i,k) = hlw(i,kk)
            enddo
          enddo
!
        endif                 ! end of rrtm calculation
!
      endif                   ! end of lw calculation
!
      return
      end
