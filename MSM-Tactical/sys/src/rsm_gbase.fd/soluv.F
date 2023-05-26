      subroutine soluv(wz,oz,ta,taucl,csm,zth,fice,                      & 
     &                 kaer,kprf,idxc,cmix,denn,rh,albb,albd,            &
     &                 icwp,cwp,cip,ccly,rew,rei,rewi,reii,              &
     &                 tupfxc,supfxc,sdnfxc,tupfx0,supfx0,sdnfx0,        &
     &                 fnet0,fnetc,dwsfb0,dwsfd0,dwsfbc,dwsfdc,          &
     &                 l,lp1,imax,nbd,nvb,nae,ndm,nxc,ndn,               &
     &                 haer,idm,dz,hz,taur)
!    &,                lprnt)
!fpp$ noconcur r
!*******************************************************************
!  compute solar flux in the uv+visible region
!  the uv+visible region is grouped into 8 bands:
!    uv-c     (.175-.225);(.225-.245,.260-.280);(.245-.260);
!    uv-b     (.280-.295);(.295-.310);(.310-.320);
!    uv-a     (.320-.400);
!    par      (.400-.700)
!
!  input parameters:                            units
!    wz,oz,ta,taucl,csm,fice,kaer,paer,albb,albd
!    icwp,cwp,cip,cclv,rew,rei
!
!  output parameters:
!    fnet0  : clear sky net flux
!    fnetc  : cloudy sky net flux
!    tupfxc : cloudy sky upward flux at toa
!    supfxc : cloudy sky upward flux at sfc
!    sdnfxc : cloudy sky downward flux at sfc
!    tupfx0 : clear sky upward flux at toa
!    supfx0 : clear sky upward flux at sfc
!    sdnfx0 : clear sky downward flux at sfc
!    dwsfb0 : clear sky sfc down dir. flux
!    dwsfd0 : clear sky sfc down dif. flux
!    dwsfbc : cloudy sky sfc down dir. flux
!    dwsfdc : cloudy sky sfc down dif. flux
!
!  fixed input data:
!    fraction of solar flux contained
!       in the 8 bands (ss)                     fraction
!    rayleigh optical thickness (tauray)        /mb
!    ozone absorption coefficient (ak)          /(cm-atm)stp
!
!  the following parameters must be specified by users:
!    cloud asymmetry factor (asycl)             n/d 
!  aerosol parameters are from subprogram aeros:
!
!  program history log:
!   94-06-12   m.d. chou, gla.
!   95-02-09   yu-tai hou      - recode for nmc models
!   98-08-03   yu-tai hou      - updated cloud radiative properties
!            calculation. use slingo method (jas 1989) on water
!            cloud, ebert and curry method (jgr 1992) on ice cloud.
!   99-03-25   yu-tai hou      - updated cloud properties use the
!            most recent chou et al. data (j. clim 1998)
!   99-04-27   yu-tai hou      - updated cloud radiative property
!            calculations use linear t-adjusted method.
!   99-09-13   yu-tai hou      - updated to chou june,1999 version
!
!********************************************************************
!
!
      use machine , only : kind_rad
      implicit none
!
      integer nvbb
!     parameter (nvbb=4)
      parameter (nvbb=8)
!
      integer l, lp1, imax, nbd, nvb, nae, ndm, nxc, ndn                 &  
     &,       kaer, icwp
!
      integer     idm (l,nae), idxc(nxc,imax), kprf(imax)
      real (kind=kind_rad) haer(ndm,nae)                                 &
     &,                     dz(imax,l), hz(imax,l+1), taur(l,nbd)        &
     &,                     cmix(nxc,imax), denn(ndn,imax)
!
! --- input
      real (kind=kind_rad)                                               &
     &  oz(imax,l),   taucl(imax,l), albb(imax), albd(imax)              &
     &, csm(imax,l),  zth(imax,l),   rh(imax,l)                          &
     &, ta(imax,l),   fice(imax,l)                                       &
     &, cwp(imax,l),  cip(imax,l),   rew(imax,l), rei(imax,l)            &
     &, ccly(imax,l), wz(imax,l)                                         &
     &, rewi(imax,l), reii(imax,l)                                      
! --- output
      real (kind=kind_rad)                                               &
     &  fnet0 (imax,lp1), dwsfb0(imax), dwsfd0(imax)                     &
     &, fnetc (imax,lp1), dwsfbc(imax), dwsfdc(imax)                     &
     &, tupfxc(imax),     supfxc(imax), sdnfxc(imax)                     &
     &, tupfx0(imax),     supfx0(imax), sdnfx0(imax)
! --- temporary array
      real (kind=kind_rad)                                               &
     &  upflux(imax,lp1), dwflux(imax,lp1)                               &
     &, dwsfxb(imax),     dwsfxd(imax)                                   &
     &, tauto (imax,l),   ssato (imax,l),   asyto (imax,l)               &
     &, taurs (imax,l),   ssat1 (imax,l),   asyt1 (imax,l)               &
     &, tauaer(imax,l),   ssaaer(imax,l),   asyaer(imax,l)               &
     &, fffcw (imax,l),   ffft1 (imax,l),   fffto (imax,l)               &
     &, asycw (imax,l),   ssacw (imax,l)                                 &
! --- solar flux and absorption coefficients
     &, ss(nvbb),         ak(nvbb),         wk(nvbb)                     &
!0499
! --- t adjusted cld property method
     &, a0w(2), a1w(2), b0w(2), b1w(2), b0i(2), b1i(2), b2i(2)           &
     &, a0i(2), a1i(2), c0w(2), c1w(2), c0i(2), c1i(2), c2i(2)           &
     &, ssaw0(2), ssai0(2), asyw0(2), asyi0(2)                           &
     &, facw(imax,l), faci(imax,l)                                       &
     &, fffrs0,       fpmin, fpmax
!
      logical cloudy(imax)
      integer ncloud
!     logical lprnt
!
!
      data ss / 0.00057, 0.00367, 0.00083, 0.00417,                      &
     &          0.00600, 0.00556, 0.05913, 0.39081 /
      data ak / 30.47, 187.2, 301.9, 42.83,                              &
     &          7.090, 1.250, .0345, .0572 /
      data wk / 7*0.0e0, 0.75e-3 /
      data ssaw0 /.999998,.999998/, ssai0 /.999994,.999995/              &
     &     asyw0 / 0.853,  0.853 /, asyi0 / 0.7991, 0.7998/              &
     &,    fffrs0 / 0.1 /
      data fpmin, fpmax / 1.0e-8, 0.999999 /
!0898 - coeff for water cloud
                         data                                            &
! --- t adjusted water/ice cloud coeff.
     &   a0w / 0.2807e-1,0.2798e-1 /, a1w / 0.1307e+1,0.1309e+1 /        &
     &,  b0w / -.1176e-6,-.1810e-6 /, c0w / 0.8276e+0,0.8272e+0 /        &
     &,  b1w / 0.1770e-6,0.1778e-6 /, c1w / 0.2541e-2,0.2565e-2 /        &
     &,  a0i / -.3011e-4,-.5975e-5 /, a1i / 0.2519e+1,0.2517e+1 /        &
     &,  b0i / 0.1688e-6,0.1721e-6 /, c0i / 0.7473e+0,0.7480e+0 /        &
     &,  b1i / 0.9936e-7,0.9177e-7 /, c1i / 0.1015e-2,0.1015e-2 /        &
     &,  b2i /-.1114e-10,-.1125e-10/, c2i / -.2524e-5,-.2531e-5 /
!
      save ss, ak, wk, asyw0, asyi0, ssaw0, ssai0, fffrs0, fpmin, fpmax
      save a0w,a1w,b0w,b1w,c0w,c1w,                                      &
     &     a0i,a1i,b0i,b1i,c0i,c1i,b2i,c2i
!
      real (kind=kind_rad) tau1,  tau2,  ssa1,  ssa2,  asy1,  asy2       &
     &,                     ssaw1, ssaw2, asyw1, asyw2, tauoz, tauwv     &
     &,                     tem
      integer i, k, iv
!
      do k=1,l
        do i=1,imax
          facw(i,k) = max(0.0, min(10.0,273.15-ta(i,k)))*0.1
          faci(i,k) = max(0.0, min(30.0,263.15-ta(i,k)))/30.0
        enddo
      enddo
      cloudy(:) = .false.
!
      if (nvb .ne. nvbb) then
         print *,' nvb=',nvb,' nvbb=',nvbb,' run stopped'
         stop
      endif
!
      if (icwp .ne. 1) then
        do k=1,l
        do i=1,imax
          if (taucl(i,k) .gt. 0.0) then
            tau2 = fice(i,k) * taucl(i,k)
            tau1 = taucl(i,k) - tau2
!0499 - t-adj prop from specified ssa and asy
            ssa1 = facw(i,k)*ssaw0(1) + (1.0-facw(i,k))*ssaw0(2)
            ssa2 = faci(i,k)*ssai0(1) + (1.0-faci(i,k))*ssai0(2)
            ssaw1 = ssa1 * tau1
            ssaw2 = ssa2 * tau2
!           ssa1 = (1.0-fice(i,k))*(facw(i,k) *ssaw0(1)
!    &                       + (1.0-facw(i,k))*ssaw0(2) )
!           ssa2 =        fice(i,k) *(faci(i,k) *ssai0(1)
!    &                       + (1.0-faci(i,k))*ssai0(2) )
!           ssaw1 = ssa1 * taucl(i,k)
!           ssaw2 = ssa2 * taucl(i,k)
            ssacw(i,k) = ssaw1 + ssaw2
!           asy1 = (1.0-fice(i,k))*(facw(i,k) *asyw0(1)
!    &                       + (1.0-facw(i,k))*asyw0(2) )
!           asy2 =        fice(i,k) *(faci(i,k) *asyi0(1)
!    &                       + (1.0-faci(i,k))*asyi0(2) )
            asy1 = facw(i,k)*asyw0(1) + (1.0-facw(i,k))*asyw0(2)
            asy2 = faci(i,k)*asyi0(1) + (1.0-faci(i,k))*asyi0(2)
            asyw1 = asy1 * ssaw1
            asyw2 = asy2 * ssaw2
            asycw(i,k) = asyw1 + asyw2
            fffcw(i,k) = asy1*asyw1 + asy2*asyw2
            cloudy(i) = .true.
          else
            ssacw(i,k) = 1.0
            asycw(i,k) = 0.0
            fffcw(i,k) = 0.0
          end if
        enddo
        enddo
      else
        do k=1,l
        do i=1,imax
          if (ccly(i,k) .gt. 0.01) then
!0499 --- t-adj prop from ice/water paths
            tau1 = cwp(i,k)*(   facw(i,k) *(a0w(1)+a1w(1)*rewi(i,k))     & 
     &                     +(1.-facw(i,k))*(a0w(2)+a1w(2)*rewi(i,k)))
            tau2 = cip(i,k)*(   faci(i,k) *(a0i(1)+a1i(1)*reii(i,k))     &
     &                     +(1.-faci(i,k))*(a0i(2)+a1i(2)*reii(i,k)))
            taucl(i,k) = tau1 + tau2
            ssa1 = 1.0 - (   facw(i,k) *(b0w(1)+b1w(1)*rew(i,k))         &
     &                 + (1.-facw(i,k))*(b0w(2)+b1w(2)*rew(i,k)) )
            ssa2 = 1.0 - (  faci(i,k) *(b0i(1)                           &
     &                 + (b1i(1)+b2i(1)*rei(i,k))*rei(i,k))              &
     &                 + (1.-faci(i,k))*(b0i(2)                          &
     &                 + (b1i(2)+b2i(2)*rei(i,k))*rei(i,k)) )
            ssaw1 = ssa1 * tau1
            ssaw2 = ssa2 * tau2
            ssacw(i,k) = ssaw1 + ssaw2
            asy1 =     facw(i,k) *(c0w(1)+c1w(1)*rew(i,k))               &
     &           + (1.-facw(i,k))*(c0w(2)+c1w(2)*rew(i,k))
            asy2 =     faci(i,k) *(c0i(1)                                &
     &           + (c1i(1)+c2i(1)*rei(i,k))*rei(i,k) )                   &
     &           + (1.-faci(i,k))*(c0i(2)                                &
     &           + (c1i(2)+c2i(2)*rei(i,k))*rei(i,k) )
            asyw1 = asy1 * ssaw1
            asyw2 = asy2 * ssaw2
            asycw(i,k) = asyw1 + asyw2
            fffcw(i,k) = asy1*asyw1 + asy2*asyw2
            cloudy(i)  = .true.
          else
            taucl(i,k) = 0.0
            ssacw(i,k) = 1.0
            asycw(i,k) = 0.0
            fffcw(i,k) = 0.0
          end if
        enddo
        enddo
      end if
!
      ncloud = 0
      do i=1,imax
        if (cloudy(i)) ncloud = ncloud + 1
      enddo
!
!===> ... integration over spectral bands
!
      do iv=1,nvb
!
!===> ... layer optical depth due to rayleigh scattering
!
        do k=1,l
          do i=1,imax
            taurs(i,k)  = taur(k,iv)
            ssaaer(i,k) = 0.0
            asyaer(i,k) = 0.0
            tauaer(i,k) = 0.0
          enddo
        enddo
        if (kaer .ge. 1) then !==> aerosol optical properties
          call aeros(iv,kprf,idxc,cmix,denn,rh                           &  
     &,              tauaer,ssaaer,asyaer                                &
     &,              l,imax,nae,nbd,ndm,nxc, ndn                         &
     &,              haer,idm,dz,hz)
        endif
!
!===> ... compute total optical thickness, single scattering albedo,
!         and asymmetry factor for clear sky
!
        do k=1,l
        do i=1,imax
          tauoz = ak(iv)*oz(i,k)
          tauwv = wk(iv)*wz(i,k)
          tauto(i,k) = max(fpmin, tauoz+tauwv+tauaer(i,k)+taurs(i,k))
          ssat1(i,k) = ssaaer(i,k)*tauaer(i,k) + taurs(i,k)
          asyt1(i,k) = asyaer(i,k)*ssaaer(i,k)*tauaer(i,k)
          ffft1(i,k) = asyaer(i,k)*asyt1(i,k) + fffrs0*taurs(i,k)
!
          ssato(i,k) = min(fpmax, ssat1(i,k)/tauto(i,k))
          tem        = 1.0 / max(fpmin, ssat1(i,k))
          asyto(i,k) = asyt1(i,k) * tem
          fffto(i,k) = ffft1(i,k) * tem
        enddo
        enddo
!
!===> ... clear sky fluxes calculations
!
        call swflux(tauto,ssato,asyto,fffto,csm,zth,albb,albd,           &  
     &              upflux,dwflux,dwsfxb,dwsfxd, l, lp1, imax)
!    &,             lprnt)
!
        do k=1,lp1
          do i=1,imax
            fnet0(i,k) = fnet0(i,k) + (dwflux(i,k) - upflux(i,k))*ss(iv)
          enddo
        enddo
        do i=1,imax
          tupfx0(i) = tupfx0(i) + upflux(i,1)   * ss(iv)
          supfx0(i) = supfx0(i) + upflux(i,lp1) * ss(iv)
          sdnfx0(i) = sdnfx0(i) + dwflux(i,lp1) * ss(iv)
          dwsfb0(i) = dwsfb0(i) + dwsfxb(i)     * ss(iv)
          dwsfd0(i) = dwsfd0(i) + dwsfxd(i)     * ss(iv)
        enddo
!
        if (ncloud .gt. 0) then
!
!===> ... compute total optical thickness, single scattering albedo,
!         and asymmetry factor for cloudy sky
!
        do k=1,l
          do i=1,imax
            if (taucl(i,k) .gt. 0.0) then
              tauto(i,k) = taucl(i,k) + tauto(i,k)
              ssat1(i,k) = ssacw(i,k) + ssat1(i,k)
              ssato(i,k) = min(fpmax, ssat1(i,k)/tauto(i,k))
              tem        = 1.0  / max(fpmin, ssat1(i,k))
              asyto(i,k) = (asycw(i,k) + asyt1(i,k)) * tem
              fffto(i,k) = (fffcw(i,k) + ffft1(i,k)) * tem
            end if
          enddo
        enddo
!
!===> ... cloudy sky fluxes calculations
!
        call swflux(tauto,ssato,asyto,fffto,csm,zth,albb,albd,           & 
     &              upflux,dwflux,dwsfxb,dwsfxd, l, lp1, imax)
!    &,             lprnt)
!
        do k=1,lp1
          do i=1,imax
            fnetc(i,k) = fnetc(i,k) + (dwflux(i,k) - upflux(i,k))*ss(iv)
          enddo
        enddo
        do i=1,imax
          tupfxc(i) = tupfxc(i) + upflux(i,1)   * ss(iv)
          supfxc(i) = supfxc(i) + upflux(i,lp1) * ss(iv)
          sdnfxc(i) = sdnfxc(i) + dwflux(i,lp1) * ss(iv)
          dwsfbc(i) = dwsfbc(i) + dwsfxb(i)     * ss(iv)
          dwsfdc(i) = dwsfdc(i) + dwsfxd(i)     * ss(iv)
        enddo
        else
          do k=1,lp1
            do i=1,imax
              fnetc(i,k) = fnet0(i,k)
            enddo
          enddo
          do i=1,imax
            tupfxc(i) = tupfx0(i)
            supfxc(i) = supfx0(i)
            sdnfxc(i) = sdnfx0(i)
            dwsfbc(i) = dwsfb0(i)
            dwsfdc(i) = dwsfd0(i)
          enddo
        endif
!
      enddo           !    integration over spectral bands loop end
!
      return
      end
