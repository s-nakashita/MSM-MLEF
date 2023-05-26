      subroutine swr95a(s0,isrc,pl,ta,wa,oa,co2,cosz,taucl,              &  
     &            ccly,cfac,icfc,icwp,cwp,cip,rew,rei,fice,              &
     &            albuvb,albuvd,albirb,albird,kprf,idxc,cmix,denn,rh,    &
     &            htrc,tupfxc,tdnflx,supfxc,sdnfxc,                      &
     &            tupfx0,supfx0,sdnfx0,                                  &
     &            sdnfvb,sdnfvd,sdnfnb,sdnfnd,                           &
     &            l, lp1, imax, nsrc, nbd, nvb, nae, ndm, nxc, ndn,      &
     &            haer, idm, dz, hz, taur, me)
!    &,           lprnt)
!fpp$ noconcur r
!$$$  subprogram documentation block
!
! subprogram:    swr95      computes short-wave radiative heating
!   programmer: yu-tai hou  org: w/nmc20      date: 95-02-09
!
! abstract: this code is a modified version of m.d. chou sw
!   radiation code to fit nmc mrf and climate models.  it computes
!   sw atmospheric absorption and scattering effects due to o3,
!   h2o,co2,o2,clouds, and aerosols, etc.
!   it has 8 uv+vis bands and 3 nir bands (10 k-values each).
!
! references: chou (1986, j. clim. appl.meteor.)
!   chou (1990, j. clim.), and chou (1992, j. atms. sci.)
!   chou and suarez (1999, nasa/tm-1999-104606,vol.15)
!
! program history log:
!   94-06-12   m.d. chou, gla.
!   95-02-09   yu-tai hou      - recode for nmc models
!   98-08-03   yu-tai hou      - updated cloud radiative properties
!            calculation. use slingo method (jas 1989) on water
!            cloud, ebert and curry method (jgr 1992) on ice cloud.
!   99-03-25   yu-tai hou      - updated cloud radiative property
!            calculations use chou et al. new method (j. clim 1998)
!   99-04-27   yu-tai hou      - updated cloud radiative property
!            calculations use linear t-adjusted method.
!   99-09-13   yu-tai hou      - updated to chou june,99 version
!
! usage:       call swr95
!
! attributes:
!   language:  fortran 77 & fortran 90
!   machine:   cray c-90, ibm sp, sgi
!
! input parameters:
!   s0     : solar constant
!   isrc   : flags for selecting absorbers
!            1:aerosols, 2:o2, 3:co2, 4:h2o, 5:o3
!            =0:without it,  =1: with it.
!   pl     : model level pressure in mb
!   ta     : model layer temperature in k
!   wa     : layer specific humidity in gm/gm
!   oa     : layer ozone concentration in gm/gm
!   co2    : co2 mixing ration by volume
!   cosz   : cosine of solar zenith angle
!   taucl  : optical depth of cloud layers
!   ccly   : layer cloud fraction
!   cfac   : fraction of clear sky view at the layer interface
!   icfc   : =0 no cloud factor to weigh clear and cloudy fluxes
!            =1 use cloud factor to weigh clear and cloudy fluxes
!   icwp   : flag indicates the method used for cloud properties
!            calculations, =0 use t-p; =1 use cwc/cic.
!   cwp    : layer cloud water path (g/m**2)
!   cip    : layer cloud ice path (g/m**2)
!   rew    : layer water cloud drop effective radius (micron)
!   rei    : layer ice cloud drop effective radius
!   fice   : fraction of cloud ice content
!   albuvb : uv+vis surf direct albedo
!   albuvd : uv+vis surf diffused albedo 
!   albirb : nir surf direct albedo 
!   albird : nir surf diffused albedo
!   paer   : aerosol profiles (fraction)
!
! output parameter:
!   htrc   : heating rates for cloudy sky in  k/day
!   tupfxc : upward flux at toa for cloudy sky  w/m**2
!   tdnflx : dnward flux at toa for all sky  w/m**2
!   supfxc : upward flux at sfc for cloudy sky  w/m**2
!   sdnfxc : dnward flux at sfc for cloudy sky  w/m**2
!   tupfx0 : upward flux at toa for clear sky   w/m**2
!   supfx0 : upward flux at sfc for clear sky   w/m**2
!   sdnfx0 : dnward flux at sfc for clear sky   w/m**2
!   sdnfvb : downward surface vis beam flux     w/m**2
!   sdnfnb : downward surface nir beam flux     w/m**2
!   sdnfvd : downward surface vis diff flux     w/m**2
!   sdnfnd : downward surface nir diff flux     w/m**2
!
! note:
!   for all quantities, k=1 is the top level/layer, except
!   si and sl, for which k=1 is the surface level/layer.
!
!$$$
!
!
      use machine , only : kind_rad,kind_phys
      implicit none
!c      include 'mpi_inc.h'
!
      integer l, lp1, imax, nsrc, nbd, nvb, nae, ndm, nxc, ndn           &  
     &,       icfc, icwp, me
      integer idm (l,nae), idxc(nxc,imax), kprf(imax), isrc(nsrc)
      real (kind=kind_rad) haer(ndm,nae)                                 &
     &,                     dz(imax,l), hz(imax,l+1), taur(l,nbd)        &
     &,                     cmix(nxc,imax), denn(ndn,imax)
!
! ---  input
      real (kind=kind_rad) s0, co2                                       &
     &, pl (imax,lp1), ta(imax,l),   wa(imax,l),    oa(imax,l)           &
     &, taucl(imax,l), ccly(imax,l), cfac(imax,lp1),cosz(imax)           &
     &, albuvb(imax),  albuvd(imax), albirb(imax),  albird(imax)         &
     &, rh(imax,l),    fice(imax,l)                                      &
     &, cwp(imax,l),   cip(imax,l),  rew(imax,l),   rei(imax,l)
!

! ---  output
      real (kind=kind_rad)                                               &
     &  tupfxc(imax), supfxc(imax), sdnfxc(imax), tdnflx(imax)           &
     &, tupfx0(imax), supfx0(imax), sdnfx0(imax), htrc(imax,l)           &
     &, sdnfvb(imax), sdnfvd(imax), sdnfnb(imax), sdnfnd(imax)           &
     &, sdn0vb(imax), sdn0vd(imax), sdn0nb(imax), sdn0nd(imax)

! ---  internal array
      real (kind=kind_rad)                                               &
     &  fnet0(imax,lp1), fnetc(imax,lp1), htr0 (imax,lp1)                &
     &, dflx0(imax,lp1), dflxc(imax,lp1), dp   (imax,l)                  &
     &, scal (imax,l),   swh  (imax,lp1), so2  (imax,lp1)                &
     &, wh   (imax,l),   oh   (imax,l),   swu  (imax,lp1)                &
     &, cf0  (imax),     cf1  (imax),     snt  (imax)                    &
     &, cnt  (imax)
      real (kind=kind_rad) rewi(imax,l), reii(imax,l)
!     logical lprnt
!
      real (kind=kind_rad) zthik(imax,l), csmik(imax,l)
!
      real (kind=kind_rad) taucrt
      integer ifpr, ibnd
      data taucrt / 0.05 /, ifpr / 0 /
      data ibnd / 1 /        !===> ... ibnd=1:use one nir band
!     data ibnd / 2 /        !===> ... ibnd=2:use two nir bands
      save taucrt, ifpr, ibnd
!
      real (kind=kind_rad) tfac, tem, rcf1, ccc, xa, to2                 &  
     &,                     u1, du, w1, dw, fac
      integer i, k, jtop
!
#include <co2tab_sw.h>
!
!===> ... begin here
      if (ifpr .eq. 0) then
        if (me.eq.0) write(6,12) (isrc(i),i=1,nsrc)
  12    format(3x,'aerosol, o2, co2, h2o, o3 =',5i3)
        ifpr = 1
      end if
!
      dflxc(:,:) = 0.0
      do i=1,imax
        swh (i,1) = 0.0
        so2 (i,1) = 0.0
        tupfxc(i) = 0.0
        tupfx0(i) = 0.0
        supfxc(i) = 0.0
        supfx0(i) = 0.0
        sdnfxc(i) = 0.0
        sdnfx0(i) = 0.0
        cf0(i)    = cfac(i,lp1)
        cf1(i)    = 1.0 - cf0(i)
        snt(i)    = 1.0 / cosz(i) ! snt = secant of solar zenith angle
!
        sdnfvb(i) = 0.0
        sdnfvd(i) = 0.0
        sdnfnb(i) = 0.0
        sdnfnd(i) = 0.0
        sdn0vb(i) = 0.0
        sdn0vd(i) = 0.0
        sdn0nb(i) = 0.0
        sdn0nd(i) = 0.0
      enddo
!
      tfac = 0.5 / 300.0
      do k=1,l
        do i=1,imax
!===> ... layer thickness and pressure scaling function for
!         water vapor absorption
          dp  (i,k) = pl(i,k+1) - pl(i,k)
          scal(i,k) = dp(i,k) * (tfac*(pl(i,k)+pl(i,k+1)))**0.8
!===> ... scaled absorber amounts for h2o(wh,swh), unit : g/cm**2
          tem     = 0.00135*(ta(i,k)-240.0)
          wh(i,k) = 1.02 * wa(i,k) * scal(i,k)                           &  
!    &            * exp(0.00135*(ta(i,k)-240.0))
     &            * (1.0 + tem + 0.5*tem*tem) + 1.0e-11
        enddo
      enddo
      do k=1,l
        do i=1,imax
          swh(i,k+1) = swh(i,k) + wh(i,k)
          zthik(i,k) = cosz(i)
          csmik(i,k) = snt(i)
        enddo
      enddo
!
!===> ... initialize fluxes
!
      do k=1,lp1
        do i=1,imax
          fnet0(i,k) = 0.0
          fnetc(i,k) = 0.0
          dflx0(i,k) = 0.0
        enddo
      enddo
!
      if (icfc .eq. 1) then
        do i=1,imax
          cfac(i,lp1) = 0.0
        end do
        do k=1,l
          do i=1,imax
            if (cf1(i) .gt. 0.0) then
              rcf1 = 1.0 / cf1(i)
              cfac(i,k) = (cfac(i,k) - cf0(i)) * rcf1
              ccly(i,k) = ccly(i,k) * rcf1
            end if
          end do
        end do
      end if
!
      if (icwp.ne. 1) then
        do k=1,l
          do i=1,imax
!0900       taucl(i,k) = taucl(i,k) * ccly(i,k)
            taucl(i,k) = taucl(i,k) * ccly(i,k) * sqrt(ccly(i,k))
          end do
        end do
      else
        do k=1,l
          do i=1,imax
!0799       ccc = ccly(i,k) * sqrt(ccly(i,k))
            ccc = ccly(i,k)
            cwp(i,k) = cwp(i,k) * ccc
            cip(i,k) = cip(i,k) * ccc
            rewi(i,k) = 1.0 / rew(i,k)
            reii(i,k) = 1.0 / rei(i,k)
          end do
        end do
      end if
!
      if (isrc(4) .eq. 1) then     !===> ... compute nir fluxes
!                                            ------------------
        call solir(wh,ta,taucl,csmik,zthik,ibnd,fice,                    & 
     &             isrc(1),kprf,idxc,cmix,denn,rh,albirb,albird,         &
     &             icwp,cwp,cip,ccly,rew,rei,rewi,reii,                  &
     &             tupfxc,supfxc,sdnfxc,tupfx0,supfx0,sdnfx0,            &
     &             fnet0,fnetc,sdn0nb,sdn0nd,sdnfnb,sdnfnd,              &
     &             l, lp1, imax, nbd, nvb, nae, ndm, nxc, ndn,           &
     &             haer, idm, dz, hz, taur)
!    &,            lprnt)
      end if
!
      if (isrc(5) .eq. 1) then     !===> ... compute uv+visible fluxes
!                                            -------------------------
!               scaled amounts for o3(wh), unit : (cm-amt)stp for o3.
        xa = 1.02 * 466.7
        do k=1,l
          do i=1,imax
            oh(i,k) = xa * oa(i,k) * dp(i,k) + 1.0e-11
          enddo
        enddo
!
        call soluv(wh,oh,ta,taucl,csmik,zthik,fice,                      &  
     &             isrc(1),kprf,idxc,cmix,denn,rh,albuvb,albuvd,         &
     &             icwp,cwp,cip,ccly,rew,rei,rewi,reii,                  &
     &             tupfxc,supfxc,sdnfxc,tupfx0,supfx0,sdnfx0,            &
     &             fnet0,fnetc,sdn0vb,sdn0vd,sdnfvb,sdnfvd,              &
     &             l, lp1, imax, nbd, nvb, nae, ndm, nxc, ndn,           &
     &             haer, idm, dz, hz, taur)
!    &,            lprnt)
      end if
!
!===> ... compute the absorption due to oxygen,chou(1990,j.climate,209-217)
!         pressure scaled amounts for o2(o2,so2), unit is (cm-atm)stp for o2.
!         the constant 165.22=(1000/980)*23.14percent*(22400/32)
!
      if (isrc(2) .eq. 1) then
        do i=1,imax
          cnt(i) = 165.22 * snt(i)
        end do
        do k=1,l
          do i=1,imax
            so2(i,k+1) = so2(i,k) + cnt(i) * scal(i,k)
          enddo
        enddo
!===> ... compute flux reduction due to oxygen, the constant 0.0633 is
!         the fraction of insolation contained in the oxygen bands.
!         to2 is the broadband transmission function for oxygen
        do k=2,lp1
          do i=1,imax
            to2        = exp(-0.145e-3 * sqrt(so2(i,k)) )
            dflx0(i,k) = 0.0633 * (1.0 - to2)
          enddo
        enddo
      end if
!
!===> ... table look-up for the absorption due to co2
!         compute scaled amounts for co2(wc,so2).
!         the constant 789=(1000/980)*(44/28.97)*(22400/44)
!
      if (isrc(3) .eq. 1) then
        do i=1,imax
          cnt(i)   = co2 * snt(i)
          so2(i,1) = max(so2(i,1), 1.0e-11)
        end do
        do k=1,l
          do i=1,imax
            so2(i,k+1) = so2(i,k) + 789.0 * cnt(i)*scal(i,k)
          enddo
        enddo
!
!===> ... for co2 absorption in spectrum 1.220-2.270 micron
!         both water vapor and co2 absorptions are moderate
!         so2 and swh are the co2 and water vapor amounts
!         integrated from the top of the atmosphere
!
        u1 = -3.0
        du = 0.15
        w1 = -4.0
        dw = 0.15
        do k=2,lp1
          do i=1,imax
            swu(i,k) = log10(so2(i,k))
            swh(i,k) = log10(swh(i,k)*snt(i))
          end do
        end do
!
!===> ... dflx0 is the updated flux reduction
!
        call flxco2(swu,u1,du,nu,swh,w1,dw,nw,cah,dflx0                  &  
     &,                                           l, imax)
!
!===> ... for co2 absorption in spectrum 2.270-10.00 micron
!         where the co2 absorption has a large impact on the
!         heating of middle atmosphere
!
        u1 = 0.250e-3
        du = 0.050e-3
        w1 = -2.0
        dw = 0.05
!===> ... co2 mixing ratio is independent of space
!         swh is the logarithm of pressure
        do k=2,lp1
          do i=1,imax
            swu(i,k) = cnt(i)
            swh(i,k) = log10(pl(i,k))
          end do
        end do
!===> ... dflx0 is the updated flux reduction
!
        call flxco2(swu,u1,du,nx,swh,w1,dw,ny,coa,dflx0                  &  
     &,                                           l, imax)
!
      endif
!
!===> ... adjust for the effect of o2 and co2 on clear sky net flux
!
      if (isrc(2).eq.1 .or. isrc(3).eq.1) then
!       do k=1,lp1
!         do i=1,imax
!           fnet0(i,k) = fnet0(i,k) - dflx0(i,k)
!         enddo
!       enddo
! 
!===> ... adjust for the effect of o2 and co2 on cloud sky net flux
!
        do i=1,imax
          jtop = lp1
!===> ... above clouds
          do k=1,lp1
            dflxc(i,k) = dflx0(i,k)
            if (cfac(i,k) .lt. 1.0) then
              jtop = k
              exit
            end if
          end do
!===> ... below cloud top
          if (jtop .lt. lp1) then
            do k=jtop+1,lp1
              dflxc(i,k) = dflx0(i,k) * (fnetc(i,k)/fnet0(i,k))
            end do
          end if
          do k=1,lp1
            fnet0(i,k) = fnet0(i,k) - dflx0(i,k)
            fnetc(i,k) = fnetc(i,k) - dflxc(i,k)
          end do
        enddo
!
!===> ... adjust for other fluxes
!
        do i=1,imax
          sdnfx0(i) = sdnfx0(i) - dflx0(i,lp1)
          sdnfxc(i) = sdnfxc(i) - dflxc(i,lp1)
          sdn0nb(i) = sdn0nb(i) - dflx0(i,lp1)
          sdnfnb(i) = sdnfnb(i) - dflxc(i,lp1)
        enddo
      end if
!
      if (icfc .eq. 1) then
!===> ... compute final fluxes at top and surface
        do i=1,imax
          sdnfvb(i) = cf0(i)*sdn0vb(i) + cf1(i)*sdnfvb(i)
          sdnfvd(i) = cf0(i)*sdn0vd(i) + cf1(i)*sdnfvd(i)
          sdnfnb(i) = cf0(i)*sdn0nb(i) + cf1(i)*sdnfnb(i)
          sdnfnd(i) = cf0(i)*sdn0nd(i) + cf1(i)*sdnfnd(i)
          tupfxc(i) = cf0(i)*tupfx0(i) + cf1(i)*tupfxc(i)
          supfxc(i) = cf0(i)*supfx0(i) + cf1(i)*supfxc(i)
          sdnfxc(i) = cf0(i)*sdnfx0(i) + cf1(i)*sdnfxc(i)
        enddo
        do k=1,lp1
          do i=1,imax
            fnetc (i,k) = cf0(i)*fnet0(i,k) + cf1(i)*fnetc(i,k)
          enddo
        enddo
      end if
!
!===> ... convert flux unit to w/m**2
!
      do k=1,lp1
        do  i=1,imax
!clear    fnet0 (i,k) = fnet0(i,k) * tdnflx(i)
          fnetc (i,k) = fnetc(i,k) * tdnflx(i)
        enddo
      enddo
      do i=1,imax
        sdnfnb(i) = sdnfnb(i) * tdnflx(i)
        sdnfnd(i) = sdnfnd(i) * tdnflx(i)
        sdnfvb(i) = sdnfvb(i) * tdnflx(i)
        sdnfvd(i) = sdnfvd(i) * tdnflx(i)
        tupfx0(i) = tupfx0(i) * tdnflx(i)
        tupfxc(i) = tupfxc(i) * tdnflx(i)
        supfx0(i) = supfx0(i) * tdnflx(i)
        supfxc(i) = supfxc(i) * tdnflx(i)
        sdnfx0(i) = sdnfx0(i) * tdnflx(i)
        sdnfxc(i) = sdnfxc(i) * tdnflx(i)
      enddo
!
!===> ... fac is the factor for heating rates (in k/day)
!         if use k/sec, result should be devided by 86400.
!
!     fac = 3.6*24./10.031*.98
      fac = 8.4410328
!
      do k=1,l
        do i=1,imax
!clear    htr0(i,k) = (fnet0(i,k)-fnet0(i,k+1)) * fac / dp(i,k)
          htrc(i,k) = (fnetc(i,k)-fnetc(i,k+1)) * fac / dp(i,k)
        enddo
      enddo
!
      return
      end
