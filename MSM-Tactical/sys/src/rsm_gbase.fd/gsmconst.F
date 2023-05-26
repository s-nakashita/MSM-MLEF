!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!------- initialize constants & lookup tables for microphysics ---------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      subroutine gsmconst (dtpg,mype,first)
!
!-------------------------------------------------------------------------------
!---  subprogram documentation block
!   prgrmmr: ferrier         org: w/np22     date: february 2001
!-------------------------------------------------------------------------------
! abstract:
!   * reads various microphysical lookup tables used in column_micro
!   * lookup tables were created "offline" and are read in during execution
!   * creates lookup tables for saturation vapor pressure w/r/t water & ice
!-------------------------------------------------------------------------------
!     
! usage: call gsmconst from subroutine gsmdrive at model start time
!
!   input argument list:
!       dtph - physics time step (s)
!  
!   output argument list: 
!     none
!     
!   output files:
!     none
!     
!   subroutines:
!     my_growth_rates - lookup table for growth of nucleated ice
!
!   unique: none
!  
!   library: none
!  
!   common blocks:
!     cmicro_cons - constants used in gsmcolumn
!     cmy600       - lookup table for growth of ice crystals in 
!                    water saturated conditions (miller & young, 1979)
!     ivent_tables - lookup tables for ventilation effects of ice
!     iaccr_tables - lookup tables for accretion rates of ice
!     imass_tables - lookup tables for mass content of ice
!     irate_tables - lookup tables for precipitation rates of ice
!     irime_tables - lookup tables for increase in fall speed of rimed ice
!     mapot        - need lat/lon grid resolution
!     rvent_tables - lookup tables for ventilation effects of rain
!     raccr_tables - lookup tables for accretion rates of rain
!     rmass_tables - lookup tables for mass content of rain
!     rvelr_tables - lookup tables for fall speeds of rain
!     rrate_tables - lookup tables for precipitation rates of rain
!   
! attributes:
!   language: fortran 90
!   machine : ibm sp
!
      use funcphys , only : fpvsl
      use physcons, t0c => con_t0c
!-----------------------------------------------------------------------
!
!--- include common blocks
!
!     include "parmeta"
!     include "mpp.h"
!     integer, parameter :: lp1=lm+1
!     include "mapot.comm"
!
!------------------------------------------------------------------------- 
!-------------- parameters & arrays for lookup tables -------------------- 
!------------------------------------------------------------------------- 
!
!--- common block of constants used in column microphysics
!
      common /cmicro_cons/ abfr, cbfr, ciacw, ciacr, c_n0r0,            &
     & cn0r0, cn0r_dmrmin, cn0r_dmrmax, cracw, craut, esw0,             &
!    & qaut0, rfmax, rhgrd, rqr_dr1, rqr_dr2, rqr_dr3, rqr_drmin,       &
     & qautx, rfmax,        rqr_dr1, rqr_dr2, rqr_dr3, rqr_drmin,       &
     & rqr_drmax, rr_drmin, rr_dr1, rr_dr2, rr_dr3, rr_drmax
!
      common /cmicro_con2/ mic_step
!hmhj 
     integer mic_step
!
!--- common block for lookup table used in calculating growth rates of
!    nucleated ice crystals growing in water saturated conditions
!
      integer, parameter :: my_t1=1, my_t2=35
      common /cmy600/ my_growth(my_t1:my_t2)
      real my_growth
!
!--- mean ice particle diameters vary from 50 microns to 1000 microns
!
      real, parameter :: dmimin=.05e-3, dmimax=1.e-3, deldmi=1.e-6,     &
     &  xmimin=1.e6*dmimin, xmimax=1.e6*dmimax
      integer, parameter :: mdimin=xmimin, mdimax=xmimax
!
!--- various ice lookup tables
!
      common /iaccr_tables/ accri(mdimin:mdimax)
      common /imass_tables/ massi(mdimin:mdimax)
      common /sdens_tables/ sdens(mdimin:mdimax)
      real massi
      common /irate_tables/ vsnowi(mdimin:mdimax)
      common /ivent_tables/ venti1(mdimin:mdimax), venti2(mdimin:mdimax)
!
!--- common block for riming tables
!
      integer, parameter :: nrime=40
      common /irime_tables/ vel_rf(2:9,0:nrime)
!
!--- mean rain drop diameters vary from 50 microns to 450 microns 
!
      real, parameter :: dmrmin=.05e-3, dmrmax=.45e-3, deldmr=1.e-6,    &
     & xmrmin=1.e6*dmrmin, xmrmax=1.e6*dmrmax
      integer, parameter :: mdrmin=xmrmin, mdrmax=xmrmax
!
!--- various rain lookup tables
!
      common /raccr_tables/ accrr(mdrmin:mdrmax)
      common /rmass_tables/ massr(mdrmin:mdrmax)
      real massr
      common /rrate_tables/ rrate(mdrmin:mdrmax)
      common /rvelr_tables/ vrain(mdrmin:mdrmax)
      common /rvent_tables/ ventr1(mdrmin:mdrmax), ventr2(mdrmin:mdrmax)
!
!--- parameters & data statement for local calculations
!
      real, parameter :: c1=1./3., dmr1=.1e-3, dmr2=.2e-3, dmr3=.32e-3, &
!    & n0r0=8.e6, n0s0=4.e6, rhol=1000., rhos=100., t0c=273.15,         &
     & n0r0=8.e6, n0s0=4.e6, rhol=1000., rhos=100.,                     &
     & xmr1=1.e6*dmr1, xmr2=1.e6*dmr2, xmr3=1.e6*dmr3
      integer, parameter :: mdr1=xmr1, mdr2=xmr2, mdr3=xmr3
!
!--- added on 5/16/01 for moorthi
!
      logical, parameter :: read_lookup=.false., write_lookup=.false.
      logical first
!
!------------------------------------------------------------------------
!--------- constants passed through /cmicro_cons/ common block ----------
!------------------------------------------------------------------------
!  *************  parameters used in eta model -- not used in global model *****
!
!--- dphd, dlmd are delta latitude and longitude at the model (not geodetic) equator
!    => "dx" is the hypotenuse of the model zonal & meridional grid increments.
!
!     dx=111.*(dphd**2+dlmd**2)**.5         ! resolution at model equator (km)
!     dx=min(100., max(5., dx) )
!
!--- assume the following functional relationship for key constants that 
!    depend on grid resolution from dxmin (5 km) to dxmax (100 km) resolution:
!
!     dxmin=5.
!     dxmax=100.
!     dx=min(dxmax, max(dxmin, dx) )
!
!--- extune determines the degree to which the coefficients change with resolution.
!    the larger extune is, the more sensitive the parameter.
!
!     extune=1.
!
!--- fxtune ==> f(dx) is the grid-resolution tuning parameter (from 0 to 1)
!
!     fxtune=((dxmax-dx)/(dxmax-dxmin))**extune
!     fxtune=max(0., min(1., fxtune))
!
!--- calculate grid-averaged rh for the onset of condensation (rhgrd) based on 
!    simple ***assumed*** (user-specified) values at dxmax and at dxmin.  
!
!     rh_dxmax=.90              !-- 90percent rh at dxmax=100 km
!     rh_dxmin=.98              !-- 98percent rh at dxmin=5 km
!
!--- note that rhgrd is right now fixed throughout the domain!!
!
!     rhgrd=rh_dxmax+(rh_dxmin-rh_dxmax)*fxtune
!   ********************************************************************************
!
      pi=acos(-1.)
!
      if (first) then
!
!--- read in various lookup tables
!
      if ( read_lookup ) then
        open (unit=1,file="eta_micro_lookup.dat",form="unformatted")
        read(1) ventr1
        read(1) ventr2
        read(1) accrr
        read(1) massr
        read(1) vrain
        read(1) rrate
        read(1) venti1
        read(1) venti2
        read(1) accri
        read(1) massi
        read(1) vsnowi
        read(1) vel_rf
!       read(1) my_growth    ! applicable only for dtph=180 s for offline testing
        close (1)
      else
        etime1=timef()
        call ice_lookup                   ! lookup tables for ice
        etime2=timef()
        if (mype .eq. 0)                                                &
     &  print *,'cpu time in ice_lookup = ',etime2-etime1
        call rain_lookup                  ! lookup tables for rain
        etime1=timef()
        if (mype .eq. 0)                                                &
     &   print *,'cpu time in rain_lookup = ',etime1-etime2
        if (write_lookup) then
          open(unit=1,file='micro_lookup.dat',form='unformatted')
          write(1) ventr1
          write(1) ventr2
          write(1) accrr
          write(1) massr
          write(1) vrain
          write(1) rrate
          write(1) venti1
          write(1) venti2
          write(1) accri
          write(1) massi
          write(1) vsnowi
          write(1) vel_rf
!         write(1) my_growth    ! applicable only for dtph=180 s ????
          close (1)
        endif
      endif
!
!--- constants associated with biggs (1953) freezing of rain, as parameterized
!    following lin et al. (jcam, 1983) & reisner et al. (1998, qjrms).
!
      abfr=-0.66
      bbfr=100.
      cbfr=20.*pi*pi*bbfr*rhol*1.e-21
!
!--- qaut0 is the threshold cloud content for autoconversion to rain
!      needed for droplets to reach a diameter of 20 microns (following
!      manton and cotton, 1977; banta and hanson, 1987, jcam).  it is
!      **strongly** affected by the assumed droplet number concentrations
!     xncw!  for example, qaut0=1.2567, 0.8378, or 0.4189 g/m**3 for
!     droplet number concentrations of 300, 200, and 100 cm**-3, respectively.
!
!--- calculate grid-averaged xncw based on simple ***assumed*** (user-specified)
!    values at dxmax and at dxmin.
!
!     xncw_dxmax=50.e6          !--  50 /cm**3 at dxmax=100 km
!     xncw_dxmin=200.e6         !-- 200 /cm**3 at dxmin=5 km
!
!--- note that xncw is right now fixed throughout the domain!!
!
!     xncw=xncw_dxmax+(xncw_dxmin-xncw_dxmax)*fxtune
!
!     qaut0=pi*rhol*xncw*(20.e-6)**3/6.
      qautx=pi*rhol*1.0e6*(20.e-6)**3/6.
!
!--- based on rain lookup tables for mean diameters from 0.05 to 0.45 mm
!    * four different functional relationships of mean drop diameter as
!      a function of rain rate (rr), derived based on simple fits to
!      mass-weighted fall speeds of rain as functions of mean diameter
!      from the lookup tables.
!
      rr_drmin=n0r0*rrate(mdrmin)     ! rr for mean drop diameter of .05 mm
      rr_dr1=n0r0*rrate(mdr1)         ! rr for mean drop diameter of .10 mm
      rr_dr2=n0r0*rrate(mdr2)         ! rr for mean drop diameter of .20 mm
      rr_dr3=n0r0*rrate(mdr3)         ! rr for mean drop diameter of .32 mm
      rr_drmax=n0r0*rrate(mdrmax)     ! rr for mean drop diameter of .45 mm
!
      rqr_drmin=n0r0*massr(mdrmin)    ! rain content for mean drop diameter of .05 mm
      rqr_dr1=n0r0*massr(mdr1)        ! rain content for mean drop diameter of .10 mm
      rqr_dr2=n0r0*massr(mdr2)        ! rain content for mean drop diameter of .20 mm
      rqr_dr3=n0r0*massr(mdr3)        ! rain content for mean drop diameter of .32 mm
      rqr_drmax=n0r0*massr(mdrmax)    ! rain content for mean drop diameter of .45 mm
      c_n0r0=pi*rhol*n0r0
      cn0r0=1.e6/c_n0r0**.25
      cn0r_dmrmin=1./(pi*rhol*dmrmin**4)
      cn0r_dmrmax=1./(pi*rhol*dmrmax**4)
!
      endif                     !  if (first) then loop ends here
!
!     find out what microphysics time step should be
!
      mic_step = max(1, int(dtpg/600.0+0.5))
!     mic_step = max(1, int(dtpg/300.0+0.5))
      dtph     = dtpg / float(mic_step)
      if (mype .eq. 0) print *,' dtpg=',dtpg,' mic_step=',mic_step      &
     &,                ' dtph=',dtph
!
!--- calculates coefficients for growth rates of ice nucleated in water
!    saturated conditions, scaled by physics time step (lookup table)
!
      call my_growth_rates (dtph)
!
!--- ciacw is used in calculating riming rates
!      the assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
      ciacw=dtph*0.25*pi*0.5*(1.e5)**c1
!
!--- ciacr is used in calculating freezing of rain colliding with large ice
!      the assumed collection efficiency is 1.0
!
      ciacr=pi*dtph
!
!--- cracw is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
      cracw=dtph*0.25*pi*1.0
!
      esw0=fpvsl(t0c)           ! saturation vapor pressure at 0c
      rfmax=1.1**nrime          ! maximum rime factor allowed
!
!------------------------------------------------------------------------
!--------------- constants passed through argument list -----------------
!------------------------------------------------------------------------
!
!--- important parameters for self collection (autoconversion) of 
!    cloud water to rain. 
!
!--- craut is proportional to the rate that cloud water is converted by
!      self collection to rain (autoconversion rate)
!
      craut=1.-exp(-1.e-3*dtph)
!
!     if (mype .eq. 0)                                                   &
!    & write(6,"(a, a,f6.2,a, a,f5.4, a,f7.3,a, a,f6.2,a, a,f5.3,a)")    &
!    &   'key microphysical parameters for '                             &
!    &  ,'dx=',dx,' km:'                                                 &
!    &  ,'   fxtune=',fxtune                                             &
!    &  ,'   rhgrd=',100.*rhgrd,' percent'                               &
!    &  ,'   ncw=',1.e-6*xncw,' /cm**3'                                  &
!    &  ,'   qaut0=',1.e3*qaut0,' g/kg'
!
!--- for calculating snow optical depths by considering bulk density of
!      snow based on emails from q. fu (6/27-28/01), where optical 
!      depth (t) = 1.5*swp/(reff*dens), swp is snow water path, reff 
!      is effective radius, and dens is the bulk density of snow.
!
!    swp (kg/m**2)=(1.e-3 kg/g)*swprad, swprad in g/m**2 used in radiation
!    t = 1.5*1.e3*swprad/(reff*dens)
!  
!    see derivation for massi(indexs), note equal to rho*qsnow/nsnow
!
!    sdens=1.5e3/dens, dens=massi(indexs)/[pi*(1.e-6*indexs)**3]
!
      do i=mdimin,mdimax
        sdens(i)=pi*1.5e-15*float(i*i*i)/massi(i)
      enddo
!
!-----------------------------------------------------------------------
!
      return
      end
