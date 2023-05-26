      subroutine swr95(s0,isrc,pl,ta,wa,oa,co2,cosz,taucl,               &
     &            ccly,cfac,icfc,icwp,cwp,cip,rew,rei,fice,              &
     &            albuvb,albuvd,albirb,albird,kprf,idxc,cmix,denn,rh,    &
     &            htrc,tupfxc,tdnflx,supfxc,sdnfxc,                      &
     &            tupfx0,supfx0,sdnfx0,                                  &
     &            sdnfvb,sdnfvd,sdnfnb,sdnfnd, nday, ir,                 &
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
      use machine , only : kind_rad
      implicit none
!
      integer l, lp1, imax, nsrc, nbd, nvb, nae, ndm, nxc, ndn           &  
     &,       icfc, icwp, nday, ir(nday), me
!
      integer     idm (l,nae), idxc(nxc,imax), kprf(imax), isrc(nsrc)
      real (kind=kind_rad) haer(ndm,nae)                                 &
     &,                     dz(imax,l), hz(imax,l+1), taur(l,nbd)        &
     &,                     cmix(nxc,imax), denn(ndn,imax)
! ---  input
      real (kind=kind_rad) s0, co2                                       &
     &, pl (imax,lp1), ta(imax,l),   wa(imax,l),     oa(imax,l)          &
     &, taucl(imax,l), ccly(imax,l), cfac(imax,lp1), cosz(imax)          &
     &, albuvb(imax),  albuvd(imax), albirb(imax),   albird(imax)        &
     &, rh(imax,l),    fice(imax,l)                                      &
     &, cwp(imax,l),   cip(imax,l),  rew(imax,l),    rei(imax,l)

! ---  output
      real (kind=kind_rad)                                               &
     &  tupfxc(imax), supfxc(imax), sdnfxc(imax), tdnflx(imax)           &
     &, tupfx0(imax), supfx0(imax), sdnfx0(imax), htrc(imax,l)           &
     &, sdnfvb(imax), sdnfvd(imax), sdnfnb(imax), sdnfnd(imax)           &
     &, sdn0vb(imax), sdn0vd(imax), sdn0nb(imax), sdn0nd(imax)

! ---  internal array
      real (kind=kind_rad)  htr0 (imax,l)
!
!     locals
!
      real (kind=kind_rad)                                               &
     &  pl1 (nday,lp1), ta1(nday,l),   wa1(nday,l),     oa1(nday,l)      &
     &, taucl1(nday,l), ccly1(nday,l), cfac1(nday,lp1), cosz1(nday)      &
     &, al1uvb(nday),   al1uvd(nday),  al1irb(nday),    al1ird(nday)     &
     &, rh1(nday,l),    fice1(nday,l)                                    &
     &, cwp1(nday,l),   cip1(nday,l), rew1(nday,l),     rei1(nday,l)     

      real (kind=kind_rad)                                               &
     &  tu1fxc(nday), su1fxc(nday), sd1fxc(nday), td1flx(nday)           &
     &, tu1fx0(nday), su1fx0(nday), sd1fx0(nday), htrc1(nday,l)          &
     &, sd1fvb(nday), sd1fvd(nday), sd1fnb(nday), sd1fnd(nday)           &
     &, sd10vb(nday), sd10vd(nday), sd10nb(nday), sd10nd(nday)           &
     &, htr01(nday,l)
      real (kind=kind_rad) cmix1(nxc,nday), denn1(ndn,nday)              &
     &,                    dz1(nday,l),     hz1(nday,l+1)
      integer idxc1(nxc,nday), kprf1(nday)
!
      integer i, ii, k
!     logical lprnt
!
!
!===> ... begin here
!
      do i=1,nday
        ii         = ir(i)
        tdnflx(ii) = s0 * cosz(ii)
        sdn0vb(i)  = 0.0
        sdn0vd(i)  = 0.0
        sdn0nb(i)  = 0.0
        sdn0nd(i)  = 0.0
      enddo
!
!     reduce the vectors
!
      do k=1,l
        do i=1,nday
          ii          = ir(i)
          pl1(i,k)    = pl(ii,k)
          ta1(i,k)    = ta(ii,k)
          wa1(i,k)    = wa(ii,k)
          oa1(i,k)    = oa(ii,k)
          taucl1(i,k) = taucl(ii,k)
          cfac1(i,k)  = cfac(ii,k)
          ccly1(i,k)  = ccly(ii,k)
          fice1(i,k)  = fice(ii,k)
          cwp1(i,k)   = cwp(ii,k)
          cip1(i,k)   = cip(ii,k)
          rew1(i,k)   = rew(ii,k)
          rei1(i,k)   = rei(ii,k)
          rh1(i,k)    = rh(ii,k)
!
          hz1(i,k)    = hz(ii,k)
          dz1(i,k)    = dz(ii,k)
        enddo
      enddo
      do i=1,nday
        ii           = ir(i)
        pl1(i,lp1)   = pl(ii,lp1)
        cfac1(i,lp1) = cfac(ii,lp1)
        al1uvb(i)    = albuvb(ii)
        al1uvd(i)    = albuvd(ii)
        al1irb(i)    = albirb(ii)
        al1ird(i)    = albird(ii)
        cosz1(i)     = cosz(ii)
        td1flx(i)    = tdnflx(ii)
!
        kprf1(i)     = kprf(ii)
        hz1(i,lp1)   = hz(ii,lp1)
      enddo
      do k=1,nxc
        do i=1,nday
          ii         = ir(i)
          idxc1(k,i) = idxc(k,ii)
          cmix1(k,i) = cmix(k,ii)
        enddo
      enddo
      do k=1,ndn
        do i=1,nday
          ii         = ir(i)
          denn1(k,i) = denn(k,ii)
        enddo
      enddo
!
      call swr95a(s0,isrc,pl1,ta1,wa1,oa1,co2,cosz1,taucl1,              &
     &            ccly1,cfac1,icfc,icwp,cwp1,cip1,rew1,rei1,fice1,       &
     &            al1uvb,al1uvd,al1irb,al1ird,kprf1,idxc1,cmix1,denn1,   & 
     &            rh1,                                                   &
     &            htrc1,tu1fxc,td1flx,su1fxc,sd1fxc,                     &
     &            tu1fx0,su1fx0,sd1fx0,                                  &
     &            sd1fvb,sd1fvd,sd1fnb,sd1fnd,                           &
     &            l, lp1, nday, nsrc, nbd, nvb, nae, ndm, nxc, ndn,      &
     &            haer, idm, dz1, hz1, taur, me)
!    &,           lprnt)
!
      do i=1,nday
        ii         = ir(i)
        sdnfnb(ii) = sd1fnb(i)
        sdnfnd(ii) = sd1fnd(i)
        sdnfvb(ii) = sd1fvb(i)
        sdnfvd(ii) = sd1fvd(i)
        tupfx0(ii) = tu1fx0(i)
        tupfxc(ii) = tu1fxc(i)
        supfx0(ii) = su1fx0(i)
        supfxc(ii) = su1fxc(i)
        sdnfx0(ii) = sd1fx0(i)
        sdnfxc(ii) = sd1fxc(i)
      enddo
!
      do k=1,l
        do i=1,nday
          htrc(ir(i),k) = htrc1(i,k)
        enddo
      enddo
!
      return
      end
