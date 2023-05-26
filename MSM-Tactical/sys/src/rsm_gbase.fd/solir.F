
      subroutine solir(wh,ta,taucl,csm,zth,ibnd,fice,                    &  
     &                 kaer,kprf,idxc,cmix,denn,rh,albb,albd,            &
     &                 icwp,cwp,cip,ccly,rew,rei,rewi,reii,              &
     &                 tupfxc,supfxc,sdnfxc,tupfx0,supfx0,sdnfx0,        &
     &                 fnet0,fnetc,dwsfb0,dwsfd0,dwsfbc,dwsfdc,          &
     &                 l,lp1,imax,nbd,nvb,nae,ndm,nxc,ndn,               &
     &                 haer,idm,dz,hz,taur)
!    &,                lprnt)
!fpp$ noconcur r
!********************************************************************
!  compute solar flux in the nir region (3 bands, 10-k per band)
!  the nir region has three water vapor bands, ten k for each band.
!    1.   1000-4400 (/cm)         2.27-10.0 (micron)
!    2.   4400-8200               1.22-2.27
!    3.   8200-14300              0.70-1.22
!
!  input parameters:                           units 
!    wh,ta,taucl,csm,ibnd,fice,kaer,paer,albb,albd
!    icwp,cwp,cip,cclv,rew,rei
!  fixed input data:
!    h2o absorption coefficient (xk)           cm**2/gm
!    k-distribution function    (hk)           fraction
!
!  the following parameters must specified by users:
!    cloud single scattering albedo (sacl)     n/d    
!    cloud asymmetry factor (asycl)            n/d   
!  aerosols optical parameters are obtained from calling
!    subprogram aeros
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
!c   99-04-27   yu-tai hou      - updated cloud radiative property
!            calculations use linear t-adjusted method.
!   99-09-13   yu-tai hou      - updated to chou june,1999 version
!
!********************************************************************
!
!
      use machine , only : kind_rad
      implicit none
!
      integer nrb, nk0
      parameter (nrb=4,nk0=10)
!
      integer l, lp1, imax, nbd, nvb, nae, ndm, nxc, ndn                 &  
     &,       kaer, icwp, ibnd
!
      integer     idm (l,nae), idxc(nxc,imax), kprf(imax)
      real (kind=kind_rad) haer(ndm,nae)                                 &
     &,                     dz(imax,l), hz(imax,l+1), taur(l,nbd)        &
     &,                     cmix(nxc,imax), denn(ndn,imax)
!
! --- input
      real (kind=kind_rad)                                               &
     &  wh(imax,l),   taucl(imax,l), csm(imax,l), rh(imax,l)             &
     &, albb(imax),   albd(imax),    zth(imax,l)                         &
     &, cwp(imax,l),  cip(imax,l),   rew(imax,l), rei(imax,l)            &
     &, ccly(imax,l), ta(imax,l),    fice(imax,l)                        &
     &, rewi(imax,l), reii(imax,l)
! --- output
      real (kind=kind_rad)                                               &
     &  fnet0 (imax,lp1), dwsfb0(imax), dwsfd0(imax)                     &
     &, fnetc (imax,lp1), dwsfbc(imax), dwsfdc(imax)                     &
     &, tupfxc(imax),     supfxc(imax), sdnfxc(imax)                     &
     &, tupfx0(imax),     supfx0(imax), sdnfx0(imax)
!
      integer ncloud
      logical cloudy(imax)
!     logical lprnt
!
! --- temporary array
      real (kind=kind_rad)                                               &  
     &  upflux(imax,lp1), dwflux(imax,lp1)                               &
     &, dwsfxb(imax),     dwsfxd(imax)                                   &
     &, tauto (imax,l),   ssato (imax,l),     asyto (imax,l)             &
     &, taurs (imax,l),   ssat1 (imax,l),     asyt1 (imax,l)             &
     &, tauaer(imax,l),   ssaaer(imax,l),     asyaer(imax,l)             &
     &, xk  (nk0),        hk  (nk0,nrb)                                  &
!0499 --- t adjusted cld property method
     &, ssaw0(nrb,2),   ssai0(nrb,2),   asyw0(nrb,2), asyi0(nrb,2)       &
     &, fffcw (imax,l), ffft1 (imax,l), fffto (imax,l)                   &
     &, asycw(imax,l),  ssacw(imax,l)
!
      real (kind=kind_rad)                                               &
     &  a0w(nrb,2), a1w(nrb,2), b0w(nrb,2), b1w(nrb,2)                   &
     &, a0i(nrb,2), a1i(nrb,2), c0w(nrb,2), c1w(nrb,2)                   &
     &, b0i(nrb,2), b1i(nrb,2), b2i(nrb,2), facw(imax,l)                 &
     &, c0i(nrb,2), c1i(nrb,2), c2i(nrb,2), faci(imax,l)                 &
     &, fffrs0,     fpmin,      fpmax
!
      data xk / 0.0010, 0.0133, 0.0422, 0.1334, 0.4217,                  &
     &          1.3340, 5.6230, 31.620, 177.80, 1000.0 /
      data hk / .01074, .00360, .00411, .00421, .00389,                  &
     &          .00326, .00499, .00465, .00245, .00145,                  &
     &          .08236, .01157, .01133, .01143, .01240,                  &
     &          .01258, .01381, .00650, .00244, .00094,                  &
     &          .20673, .03497, .03011, .02260, .01336,                  &
     &          .00696, .00441, .00115, .00026, .00000,                  &
     &          .29983, .05014, .04555, .03824, .02965,                  &
     &          .02280, .02321, .01230, .00515, .00239 /
!
      data ssaw0/.7578,.9869,.9997,.9869, .7570,.9868,.9998,.9916/       &
     &,    asyw0/.8678,.8185,.8354,.8315, .8723,.8182,.8354,.8311/       &
     &,    ssai0/.7283,.9442,.9994,.9620, .7368,.9485,.9995,.9750/       &
     &,    asyi0/.9058,.8322,.8068,.8220, .9070,.8304,.8067,.8174/
      data fffrs0 / 0.1 /
      data fpmin,fpmax /1.0e-8, 0.999999/                                
!
                         data                                            &
!0499 - t-adjusted cld prop coeff, water cloud
     &    a0w / 1.466e-2, 2.276e-2, 2.654e-2, 2.494e-2                   &
     &,         1.528e-2, 2.286e-2, 2.642e-2, 2.517e-2 /                 &
     &,   a1w / 1.617e+0, 1.451e+0, 1.351e+0, 1.392e+0                   &
     &,         1.611e+0, 1.449e+0, 1.353e+0, 1.386e+0 /                 &
     &,   b0w / 1.708e-1, 5.314e-4,-4.594e-6, 6.473e-3                   &
     &,         1.674e-1, 5.427e-4,-3.306e-6, 3.218e-3 /                 &
     &,   b1w / 7.142e-3, 1.258e-3, 2.588e-5, 6.649e-4                   &
     &,         7.561e-3, 1.263e-3, 2.287e-5, 5.217e-4 /                 &
     &,   c0w / 8.266e-1, 7.507e-1, 7.925e-1, 7.811e-1                   &
     &,         8.344e-1, 7.501e-1, 7.922e-1, 7.808e-1 /                 &
     &,   c1w / 4.119e-3, 6.770e-3, 4.297e-3, 5.034e-3                   &
     &,         3.797e-3, 6.812e-3, 4.323e-3, 5.031e-3 /                 
!
                         data                                            &
!0499 - t-adjusted cld prop coeff, ice cloud
     &    a0i / 2.822e-4,-3.248e-5,-3.758e-5,-1.214e-5                   &
     &,         2.712e-4,-4.308e-5,-3.917e-5,-2.456e-5 /                 &
     &,   a1i / 2.491e+0, 2.522e+0, 2.522e+0, 2.520e00                   &
     &,         2.489e+0, 2.523e+0, 2.522e+0, 2.521e00 /                 &
     &,   b0i / 1.853e-1, 2.544e-3,-7.701e-7, 1.461e-2                   &
     &,         1.738e-1, 2.461e-3,-8.979e-7, 7.083e-3 /                 &
     &,   b1i / 1.841e-3, 1.023e-3, 9.849e-6, 4.612e-4                   &
     &,         1.887e-3, 9.436e-4, 8.102e-6, 3.495e-4 /                 &
     &,   b2i /-6.671e-6,-2.266e-6,-.3988e-9,-1.202e-6                   &
     &,        -6.615e-6,-2.107e-6,-.1862e-9,-8.500e-7 /                 &
     &,   c0i / 8.388e-1, 7.572e-1, 7.519e-1, 7.600e-1                   &
     &,         8.414e-1, 7.566e-1, 7.519e-1, 7.566e-1 /                 &
     &,   c1i / 1.519e-3, 1.563e-3, 1.099e-3, 1.275e-3                   &
     &,         1.477e-3, 1.537e-3, 1.097e-3, 1.241e-3 /                 &
     &,   c2i /-6.702e-6,-5.232e-6,-3.081e-6,-4.020e-6                   &
     &,        -6.403e-6,-5.130e-6,-3.070e-6,-3.804e-6 /             
!
      save xk, hk, ssaw0,ssai0, asyw0,asyi0, fffrs0, fpmin, fpmax
      save a0w,a1w,b0w,b1w,c0w,c1w,a0i,a1i,b0i,b1i,b2i,c0i,c1i,c2i
!
      real (kind=kind_rad) tau1,  tau2,  ssa1,  ssa2,  asy1,  asy2       &
     &,                     ssaw1, ssaw2, asyw1, asyw2, tauwv            &
     &,                     tem
      integer i, k, ibb1, ibb2, ib, ib1, ik
!
      do k=1,l
        do i=1,imax
          facw(i,k) = max(0.0, min(10.0,273.15-ta(i,k)))*0.1
          faci(i,k) = max(0.0, min(30.0,263.15-ta(i,k)))/30.0
        enddo
      enddo
!
!===> ... loop over three nir bands
!
      if (ibnd .eq. 1) then
        ibb1 = nrb
        ibb2 = nrb
      else
        ibb1 = 1
        ibb2 = nrb - 1
      end if
      do ib=ibb1,ibb2
        ib1 = nvb + ib
!
!===> ... layer optical depth due to rayleigh scattering
!
        do k=1,l
          do i=1,imax
            taurs(i,k)  = taur(k,ib1)
            ssaaer(i,k) = 0.0
            asyaer(i,k) = 0.0
            tauaer(i,k) = 0.0
          enddo
        enddo
        if (kaer .ge. 1) then !==> aerosol optical properties
          call aeros(ib1,kprf,idxc,cmix,denn,rh                          &  
     &,              tauaer,ssaaer,asyaer                                &
     &,              l,imax,nae,nbd,ndm,nxc, ndn                         &
     &,              haer,idm,dz,hz)
        endif
        cloudy(:) = .false.
!
!0898 ... get cloud properties from cwp and cip
!
      if (icwp .eq. 1) then
        do k=1,l
        do i=1,imax
          if (ccly(i,k) .gt. 0.0) then
! --- t-adj method
            tau1=cwp(i,k)*(  facw(i,k) *(a0w(ib,1)+a1w(ib,1)*rewi(i,k))  &  
     &                 +(1.0-facw(i,k))*(a0w(ib,2)+a1w(ib,2)*rewi(i,k)))
            tau2=cip(i,k)*(  faci(i,k) *(a0i(ib,1)+a1i(ib,1)*reii(i,k))  &
     &                 +(1.0-faci(i,k))*(a0i(ib,2)+a1i(ib,2)*reii(i,k)))
            taucl(i,k) = tau1 + tau2
            ssa1 = 1.0 - (  facw(i,k) *(b0w(ib,1)+b1w(ib,1)*rew(i,k))    &
     &               + (1.0-facw(i,k))*(b0w(ib,2)+b1w(ib,2)*rew(i,k)))
            ssa2 = 1.0 - (  faci(i,k) *(b0i(ib,1)                        &
     &                 + (b1i(ib,1)+b2i(ib,1)*rei(i,k))*rei(i,k))        &
     &               + (1.0-faci(i,k))*(b0i(ib,2)                        &
     &                 + (b1i(ib,2)+b2i(ib,2)*rei(i,k))*rei(i,k)) )
            ssaw1 = ssa1 * tau1
            ssaw2 = ssa2 * tau2
            ssacw(i,k) = ssaw1 + ssaw2
            asy1 =    facw(i,k) *(c0w(ib,1)+c1w(ib,1)*rew(i,k))          &
     &         + (1.0-facw(i,k))*(c0w(ib,2)+c1w(ib,2)*rew(i,k))
            asy2 =    faci(i,k) *(c0i(ib,1)                              &  
     &              + (c1i(ib,1)+c2i(ib,1)*rei(i,k))*rei(i,k))           &
     &         + (1.0-faci(i,k))*(c0i(ib,2)                              &
     &              + (c1i(ib,2)+c2i(ib,2)*rei(i,k))*rei(i,k))
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
      else
        do k=1,l
        do i=1,imax
          if (taucl(i,k) .gt. 0.0) then
            tau2 = fice(i,k) * taucl(i,k)
            tau1 = taucl(i,k) - tau2
            ssa1 = facw(i,k)*ssaw0(ib,1) + (1.0-facw(i,k))*ssaw0(ib,2)
            ssa2 = faci(i,k)*ssai0(ib,1) + (1.0-faci(i,k))*ssai0(ib,2)
            ssaw1 = ssa1 * tau1
            ssaw2 = ssa2 * tau2
!           ssa1 = (1.0-fice(i,k)) * (facw(i,k) * ssaw0(ib,1)
!    &                         + (1.0-facw(i,k))* ssaw0(ib,2))
!           ssa2 =      fice(i,k)  * (faci(i,k) * ssai0(ib,1)
!    &                         + (1.0-faci(i,k))* ssai0(ib,2))
!           ssaw1 = ssa1 * taucl(i,k)
!           ssaw2 = ssa2 * taucl(i,k)
            ssacw(i,k) = ssaw1 + ssaw2
!           asy1 = (1.0-fice(i,k)) * (facw(i,k) * asyw0(ib,1)
!    &                         + (1.0-facw(i,k))* asyw0(ib,2))
!           asy2 =      fice(i,k)  * (faci(i,k) * asyi0(ib,1)
!    &                         + (1.0-faci(i,k))* asyi0(ib,2))
            asy1 = facw(i,k)*asyw0(ib,1) + (1.0-facw(i,k))*asyw0(ib,2)
            asy2 = faci(i,k)*asyi0(ib,1) + (1.0-faci(i,k))*asyi0(ib,2)
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
      end if
!
      ncloud = 0
      do i=1,imax
        if (cloudy(i)) ncloud = ncloud + 1
      enddo
!
!===> ... ik is the index for the k-distribution function (or the
!     absorption coefficient)
!
        do ik=1,nk0
!
         if (hk(ik,ib) .ge. 0.00001) then
!
!===> ... compute tatal optical thickness, single scattering albedo,
!         and asymmetry factor for clear sky
!
           do k=1,l
           do i=1,imax
             tauwv      = xk(ik)*wh(i,k)
             tauto(i,k) = max(fpmin, tauwv+tauaer(i,k)+taurs(i,k))
             ssat1(i,k) = ssaaer(i,k)*tauaer(i,k)+taurs(i,k)
             asyt1(i,k) = asyaer(i,k)*ssaaer(i,k)*tauaer(i,k)
             ffft1(i,k) = asyaer(i,k)*asyt1(i,k) + fffrs0*taurs(i,k)
             ssato(i,k) = min(fpmax, ssat1(i,k)/tauto(i,k))
             tem        = 1.0 / max(fpmin, ssat1(i,k))
             asyto(i,k) = asyt1(i,k) * tem
             fffto(i,k) = ffft1(i,k) * tem
           enddo
           enddo
!
!===> ... clear sky fluxes calculations
!
           call swflux(tauto,ssato,asyto,fffto,csm,zth,albb,albd,        &  
     &                 upflux,dwflux,dwsfxb,dwsfxd, l, lp1, imax)
!    &,                lprnt)
!
           do k=1,lp1
             do i=1,imax
               fnet0 (i,k) = fnet0 (i,k)                                 &
     &                     + (dwflux(i,k) - upflux(i,k))*hk(ik,ib)
             enddo
           enddo
           do i=1,imax
             tupfx0(i) = tupfx0(i) + upflux(i,1)   * hk(ik,ib)
             supfx0(i) = supfx0(i) + upflux(i,lp1) * hk(ik,ib)
             sdnfx0(i) = sdnfx0(i) + dwflux(i,lp1) * hk(ik,ib)
             dwsfb0(i) = dwsfb0(i) + dwsfxb(i)     * hk(ik,ib)
             dwsfd0(i) = dwsfd0(i) + dwsfxd(i)     * hk(ik,ib)
           enddo
           if (ncloud .gt. 0) then
!
!===> ... compute tatal optical thickness, single scattering albedo,
!         and asymmetry factor for cloudy sky
!
           do k=1,l
             do i=1,imax
               if (taucl(i,k) .ge. 0.001) then
                 tauto(i,k) = taucl(i,k) + tauto(i,k)
                 ssat1(i,k) = ssacw(i,k) + ssat1(i,k)
                 ssato(i,k) = min(fpmax, ssat1(i,k)/tauto(i,k))
                 tem        = 1.0 / max(fpmin, ssat1(i,k))
                 asyto(i,k) = (asycw(i,k) + asyt1(i,k)) * tem
                 fffto(i,k) = (fffcw(i,k) + ffft1(i,k)) * tem
               end if
             enddo
           enddo
!
!===> ... cloudy sky fluxes calculations
!
           call swflux(tauto,ssato,asyto,fffto,csm,zth,albb,albd,        &  
     &                 upflux,dwflux,dwsfxb,dwsfxd, l, lp1, imax)
!    &,                lprnt)
!
           do k=1,lp1
             do i=1,imax
               fnetc(i,k) = fnetc(i,k)                                   &
     &                    + (dwflux(i,k) - upflux(i,k))*hk(ik,ib)
             enddo
           enddo
           do i=1,imax
             tupfxc(i) = tupfxc(i) + upflux(i,1)   * hk(ik,ib)
             supfxc(i) = supfxc(i) + upflux(i,lp1) * hk(ik,ib)
             sdnfxc(i) = sdnfxc(i) + dwflux(i,lp1) * hk(ik,ib)
             dwsfbc(i) = dwsfbc(i) + dwsfxb(i)     * hk(ik,ib)
             dwsfdc(i) = dwsfdc(i) + dwsfxd(i)     * hk(ik,ib)
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
         endif
        enddo           ! k-distribution loop ends here
      enddo           ! loop over nir bands ends here
!
      return
      end
