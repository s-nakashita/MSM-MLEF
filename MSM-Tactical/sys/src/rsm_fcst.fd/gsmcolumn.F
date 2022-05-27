!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! ***** version of microphysics designed for higher resolution meso eta model
!       (1) represents sedimentation by preserving a portion of the precipitation
!           through top-down integration from cloud-top.  modified procedure to
!           zhao and carr (1997).
!       (2) microphysical equations are modified to be less sensitive to time
!           steps by use of clausius-clapeyron equation to account for changes in
!           saturation mixing ratios in response to latent heating/cooling.  
!       (3) prevent spurious temperature oscillations across 0c due to 
!           microphysics.
!       (4) uses lookup tables for: calculating two different ventilation 
!           coefficients in condensation and deposition processes; accretion of
!           cloud water by precipitation; precipitation mass; precipitation rate
!           (and mass-weighted precipitation fall speeds).
!       (5) assumes temperature-dependent variation in mean diameter of large ice
!           (houze et al., 1979; ryan et al., 1996).
!        -> 8/22/01: this relationship has been extended to colder temperatures
!           to parameterize smaller large-ice particles down to mean sizes of mdimin,
!           which is 50 microns reached at -55.9c.
!       (6) attempts to differentiate growth of large and small ice, mainly for
!           improved transition from thin cirrus to thick, precipitating ice
!           anvils.
!        -> 8/22/01: this feature has been diminished by effectively adjusting to
!           ice saturation during depositional growth at temperatures colder than
!           -10c.  ice sublimation is calculated more explicitly.  the logic is
!           that sources of are either poorly understood (e.g., nucleation for nwp) 
!           or are not represented in the eta model (e.g., detrainment of ice from 
!           convection).  otherwise the model is too wet compared to the radiosonde
!           observations based on 1 feb - 18 march 2001 retrospective runs.  
!       (7) top-down integration also attempts to treat mixed-phase processes,
!           allowing a mixture of ice and water.  based on numerous observational
!           studies, ice growth is based on nucleation at cloud top &
!           subsequent growth by vapor deposition and riming as the ice particles 
!           fall through the cloud.  effective nucleation rates are a function
!           of ice supersaturation following meyers et al. (jam, 1992).  
!        -> 8/22/01: the simulated relative humidities were far too moist compared 
!           to the rawinsonde observations.  this feature has been substantially 
!           diminished, limited to a much narrower temperature range of 0 to -10c.  
!       (8) depositional growth of newly nucleated ice is calculated for large time
!           steps using fig. 8 of miller and young (jas, 1979), at 1 deg intervals
!           using their ice crystal masses calculated after 600 s of growth in water
!           saturated conditions.  the growth rates are normalized by time step
!           assuming 3d growth with time**1.5 following eq. (6.3) in young (1993).
!        -> 8/22/01: this feature has been effectively limited to 0 to -10c.  
!       (9) ice precipitation rates can increase due to increase in response to
!           cloud water riming due to (a) increased density & mass of the rimed
!           ice, and (b) increased fall speeds of rimed ice.
!        -> 8/22/01: this feature has been effectively limited to 0 to -10c.  
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      subroutine gsmcolumn ( araing, asnowg, dtpg, i_index, j_index,    &
     & lsfc, p_col, qi_col, qr_col, qv_col, qw_col, rimef_col, t_col,   &
     & thick_col, wc_col, lm, rhc_col, xncw, print_diag)
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!-------------------------------------------------------------------------------
!----- note:  code is currently set up w/o threading!  
!-------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .     
! subprogram:  grid-scale microphysical processes - condensation & precipitation
!   prgrmmr: ferrier         org: w/np22     date: 08-2001
!-------------------------------------------------------------------------------
! abstract:
!   * merges original gscond & precpd subroutines.   
!   * code has been substantially streamlined and restructured.
!   * exchange between water vapor & small cloud condensate is calculated using
!     the original asai (1965, j. japan) algorithm.  see also references to
!     yau and austin (1979, jas), rutledge and hobbs (1983, jas), and tao et al.
!     (1989, mwr).  this algorithm replaces the sundqvist et al. (1989, mwr)
!     parameterization.  
!-------------------------------------------------------------------------------
!     
! usage: 
!   * call gsmcolumn from subroutine gsmdrive
!   * subroutine gsmdrive called from main program ebu
!
! input argument list:
!   dtph       - physics time step (s)
!   i_index    - i index
!   j_index    - j index
!   lsfc       - eta level of level above surface, ground
!   p_col      - vertical column of model pressure (pa)
!   qi_col     - vertical column of model ice mixing ratio (kg/kg)
!   qr_col     - vertical column of model rain ratio (kg/kg)
!   qv_col     - vertical column of model water vapor specific humidity (kg/kg)
!   qw_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   rimef_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   t_col      - vertical column of model temperature (deg k)
!   thick_col  - vertical column of model mass thickness (density*height increment)
!   wc_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   
!
! output argument list: 
!   arain      - accumulated rainfall at the surface (kg)
!   asnow      - accumulated snowfall at the surface (kg)
!   qv_col     - vertical column of model water vapor specific humidity (kg/kg)
!   wc_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   qw_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   qi_col     - vertical column of model ice mixing ratio (kg/kg)
!   qr_col     - vertical column of model rain ratio (kg/kg)
!   rimef_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   t_col      - vertical column of model temperature (deg k)
!     
! output files:
!     none
!     
! subprograms & functions called:
!   * real function condense  - cloud water condensation
!   * real function deposit   - ice deposition (not sublimation)
!
! unique: none
!  
! library: none
!  
! common blocks:  
!     cmicro_cons  - key constants initialized in gsmconst
!     cmicro_stats - accumulated and maximum statistics
!     cmy_growth   - lookup table for growth of ice crystals in 
!                    water saturated conditions (miller & young, 1979)
!     ivent_tables - lookup tables for ventilation effects of ice
!     iaccr_tables - lookup tables for accretion rates of ice
!     imass_tables - lookup tables for mass content of ice
!     irate_tables - lookup tables for precipitation rates of ice
!     irime_tables - lookup tables for increase in fall speed of rimed ice
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
      use funcphys
      use physcons, cp => con_cp, rd => con_rd, rv => con_rv            &
     &,             t0c => con_t0c, hvap => con_hvap, hfus => con_hfus  &
     &,             eps => con_eps, epsm1 => con_epsm1                  &
     &,             eps1 => con_fvirt
!
!------------------------------------------------------------------------- 
!--------------- arrays & constants in argument list --------------------- 
!------------------------------------------------------------------------- 
!
!     include "parmeta"
!     include "mpp.h"
      real araing, asnowg, p_col(lm), qi_col(lm), qr_col(lm), qv_col(lm) &
     &, qw_col(lm), rimef_col(lm), t_col(lm), thick_col(lm),             &
     &  wc_col(lm), rhc_col(lm), xncw(lm), arain, asnow
!
      integer i_index, j_index, lsfc
!
!------------------------------------------------------------------------- 
!-------------- common blocks for microphysical statistics ---------------
!------------------------------------------------------------------------- 
!
!------------------------------------------------------------------------- 
!--------- common blocks for constants initialized in gsmconst ----------
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
!--- the following variables are for microphysical statistics
!
      integer, parameter :: itlo=-60, ithi=40
      common /cmicro_stats/ nstats(itlo:ithi,4), qmax(itlo:ithi,5),     &
     & qtot(itlo:ithi,22)
      integer :: nstats
      real :: qmax, qtot
!
!------------------------------------------------------------------------- 
!--------------- common blocks for various lookup tables -----------------
!
!--- discretized growth rates of small ice crystals after their nucleation 
!     at 1 c intervals from -1 c to -35 c, based on calculations by miller 
!     and young (1979, jas) after 600 s of growth.  resultant growth rates
!     are multiplied by physics time step in gsmconst.
!
      integer, parameter :: my_t1=1, my_t2=35
      common /cmy600/ my_growth(my_t1:my_t2)
      real my_growth
!
!------------------------------------------------------------------------- 
!
!--- mean ice-particle diameters varying from 50 microns to 1000 microns
!      (1 mm), assuming an exponential size distribution.  
!
!---- meaning of the following arrays: 
!        - mdiam - mean diameter (m)
!        - venti1 - integrated quantity associated w/ ventilation effects 
!                   (capacitance only) for calculating vapor deposition onto ice
!        - venti2 - integrated quantity associated w/ ventilation effects 
!                   (with fall speed) for calculating vapor deposition onto ice
!        - accri  - integrated quantity associated w/ cloud water collection by ice
!        - massi  - integrated quantity associated w/ ice mass 
!        - vsnowi - mass-weighted fall speed of snow (large ice), used to calculate 
!                   precipitation rates
!
      real, parameter :: dmimin=.05e-3, dmimax=1.e-3, deldmi=1.e-6,     &
     &  xmimin=1.e6*dmimin, xmimax=1.e6*dmimax
      integer, parameter :: mdimin=xmimin, mdimax=xmimax
!
      common /iaccr_tables/ accri(mdimin:mdimax)
      common /imass_tables/ massi(mdimin:mdimax)
      real massi
      common /irate_tables/ vsnowi(mdimin:mdimax)
      common /ivent_tables/ venti1(mdimin:mdimax), venti2(mdimin:mdimax)
!
!------------------------------------------------------------------------- 
!
!--- vel_rf - velocity increase of rimed particles as functions of crude
!      particle size categories (at 0.1 mm intervals of mean ice particle
!      sizes) and rime factor (different values of rime factor of 1.1**n, 
!      where n=0 to nrime).
!
      integer, parameter :: nrime=40
      common /irime_tables/ vel_rf(2:9,0:nrime)
!
!------------------------------------------------------------------------- 
!
!--- mean rain drop diameters varying from 50 microns (0.05 mm) to 450 microns 
!      (0.45 mm), assuming an exponential size distribution.  
!
      real, parameter :: dmrmin=.05e-3, dmrmax=.45e-3, deldmr=1.e-6,    &
     & xmrmin=1.e6*dmrmin, xmrmax=1.e6*dmrmax
      integer, parameter :: mdrmin=xmrmin, mdrmax=xmrmax
!
      common /raccr_tables/ accrr(mdrmin:mdrmax)
      common /rmass_tables/ massr(mdrmin:mdrmax)
      real massr
      common /rrate_tables/ rrate(mdrmin:mdrmax)
      common /rvelr_tables/ vrain(mdrmin:mdrmax)
      common /rvent_tables/ ventr1(mdrmin:mdrmax), ventr2(mdrmin:mdrmax)
!
!------------------------------------------------------------------------- 
!------- key parameters, local variables, & important comments ---------
!-----------------------------------------------------------------------
!
!--- key parameters:
!
!---- comments on 14 march 2002
!    * set epsq to the universal value of 1.e-12 throughout the code
!      condensate.  the value of epsq will need to be changed in the other 
!      subroutines in order to make it consistent throughout the eta code.  
!    * set climit=10.*epsq as the lower limit for the total mass of 
!      condensate in the current layer and the input flux of condensate
!      from above (tot_ice, tot_icenew, tot_rain, and tot_rainnew).
!
!- nlimax - maximum number concentration of large ice crystals (20,000 /m**3, 20 per liter)
!- nlimin - minimum number concentration of large ice crystals (100 /m**3, 0.1 per liter)
!
!     real, parameter :: cp=1004.6, epsq=1.e-12, rd=287.04,             &
!    & rhol=1000., rv=461.5, t0c=273.15, xls=2.834e6, eps=rd/rv, 
      real, parameter ::  epsq=1.e-12, rhol=1000.,  xls=hvap+hfus,      &
     & nlimax=20.e3, nlimin=100., t_ice=-10., t_ice_init=-5.,           &
     & toler=5.e-7,                                                     &
!    & climit=10.*epsq, eps1=rv/rd-1., rcp=1./cp,                       &
     & climit=10.*epsq, rcp=1./cp,                                      &
     & rcprv=rcp/rv, rrhol=1./rhol, xls1=xls*rcp, xls2=xls*xls*rcprv,   &
     & xls3=xls*xls/rv,                                                 &
     & c1=1./3., c2=1./6., c3=3.31/6.,                                  &
     & dmr1=.1e-3, dmr2=.2e-3, dmr3=.32e-3, n0r0=8.e6, n0rmin=1.e4,     &
     & n0s0=4.e6, rho0=1.194, xmr1=1.e6*dmr1, xmr2=1.e6*dmr2,           &
     & xmr3=1.e6*dmr3, xratio=.025
      integer, parameter :: mdr1=xmr1, mdr2=xmr2, mdr3=xmr3
!
!--- if blend=1:
!      precipitation (large) ice amounts are estimated at each level as a 
!      blend of ice falling from the grid point above and the precip ice
!      present at the start of the time step (see tot_ice below).
!--- if blend=0:
!      precipitation (large) ice amounts are estimated to be the precip
!      ice present at the start of the time step.
!
!--- extended to include sedimentation of rain on 2/5/01 
!
      real, parameter :: blend=1.
!
!--- this variable is for debugging purposes (if .true.)
!
!     logical, parameter :: print_diag=.true.
      logical  print_diag
!
!--- local variables
!
      real emairi, n0r, nlice, nsmice
      logical clear, ice_logical, dbg_logical, rain_logical
!
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@ begin execution @@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      dtph = dtpg / mic_step
      araing=0.           ! total accumulated rainfall into grid box from above (kg/m**2)
      asnowg=0.           ! total accumulated snowfall into grid box from above (kg/m**2)
!
      do ntimes =1,mic_step
!
      arain=0.                ! accumulated rainfall into grid box from above (kg/m**2)
      asnow=0.                ! accumulated snowfall into grid box from above (kg/m**2)
!
!-----------------------------------------------------------------------
!------------ loop from top (l=1) to surface (l=lsfc) ------------------
!-----------------------------------------------------------------------
!
      do 10 l=1,lsfc

!--- skip this level and go to the next lower level if no condensate 
!      and very low specific humidities
!
        if (qv_col(l).le.epsq .and. wc_col(l).le.epsq) go to 10
!
!-----------------------------------------------------------------------
!------------ proceed with cloud microphysics calculations -------------
!-----------------------------------------------------------------------
!
          tk=t_col(l)         ! temperature (deg k)
          tc=tk-t0c           ! temperature (deg c)
          pp=p_col(l)         ! pressure (pa)
          qv=qv_col(l)        ! specific humidity of water vapor (kg/kg)
!         wv=qv/(1.-qv)       ! water vapor mixing ratio (kg/kg)
          wv=qv               ! water vapor mixing ratio (kg/kg)
          wc=wc_col(l)        ! grid-scale mixing ratio of total condensate (water or ice; kg/kg)
!         wc=wc/(1.-wc)
          rhgrd = rhc_col(l)
!
!-----------------------------------------------------------------------
!--- moisture variables below are mixing ratios & not specifc humidities
!-----------------------------------------------------------------------
!
          clear=.true.
!    
!--- this check is to determine grid-scale saturation when no condensate is present
!    
          esw=min(pp, fpvsl(tk))           ! saturation vapor pressure w/r/t water
!         qsw=eps*esw/(pp-esw)             ! saturation mixing ratio w/r/t water
          qsw=eps*esw/(pp+epsm1*esw)       ! saturation specific humidity  w/r/t water
          ws=qsw                           ! general saturation mixing ratio (water/ice)
          if (tc .lt. 0.) then
            esi=min(pp,fpvsi(tk))          ! saturation vapor pressure w/r/t ice
!           qsi=eps*esi/(pp-esi)           ! saturation mixing ratio w/r/t water
            qsi=eps*esi/(pp+epsm1*esi)     ! saturation specific humidity w/r/t water
            ws=qsi                         ! general saturation mixing ratio (water/ice)
            if (pp .le. esi) ws = wv /rhgrd
          endif
!
!--- effective grid-scale saturation mixing ratios
!
          qswgrd=rhgrd*qsw
          qsigrd=rhgrd*qsi
          wsgrd=rhgrd*ws
!
!--- check if air is subsaturated and w/o condensate
!
          if (wv.gt.wsgrd .or. wc.gt.epsq) clear=.false.
!
!--- check if any rain is falling into layer from above
!
          if (arain .gt. climit) then
            clear=.false.
          else
            arain=0.
          endif
!
!--- check if any ice is falling into layer from above
!
!--- note that "snow" in variable names is synonomous with 
!    large, precipitation ice particles
!
          if (asnow .gt. climit) then
            clear=.false.
          else
            asnow=0.
          endif
!
!-----------------------------------------------------------------------
!-- loop to the end if in clear, subsaturated air free of condensate ---
!-----------------------------------------------------------------------
!
          if (clear) go to 10
!
!-----------------------------------------------------------------------
!--------- initialize rho, thick & microphysical processes -------------
!-----------------------------------------------------------------------
!
!
!--- virtual temperature, tv=t*(1./eps-1)*q, q is specific humidity;
!    (see pp. 63-65 in fleagle & businger, 1963)
!
          rho=pp/(rd*tk*(1.+eps1*qv))   ! air density (kg/m**3)
          rrho=1./rho                ! reciprocal of air density
          dtrho=dtph*rho             ! time step * air density
          bldtrh=blend*dtrho         ! blend parameter * time step * air density
          thick=thick_col(l)         ! layer thickness = rho*dz = -dp/g = (psfc-ptop)*d_eta/(g*eta_sfc)
!
          arainnew=0.                ! updated accumulated rainfall
          asnownew=0.                ! updated accumulated snowfall
          qi=qi_col(l)               ! ice mixing ratio
          qinew=0.                   ! updated ice mixing ratio
          qr=qr_col(l)               ! rain mixing ratio
          qrnew=0.                   ! updated rain ratio
          qw=qw_col(l)               ! cloud water mixing ratio
          qwnew=0.                   ! updated cloud water ratio
!
          pcond=0.                   ! condensation (>0) or evaporation (<0) of cloud water (kg/kg)
          pidep=0.                   ! deposition (>0) or sublimation (<0) of ice crystals (kg/kg)
          piacw=0.                   ! cloud water collection (riming) by precipitation ice (kg/kg; >0)
          piacwi=0.                  ! growth of precip ice by riming (kg/kg; >0)
          piacwr=0.                  ! shedding of accreted cloud water to form rain (kg/kg; >0)
          piacr=0.                   ! freezing of rain onto large ice at supercooled temps (kg/kg; >0)
          picnd=0.                   ! condensation (>0) onto wet, melting ice (kg/kg)
          pievp=0.                   ! evaporation (<0) from wet, melting ice (kg/kg)
          pimlt=0.                   ! melting ice (kg/kg; >0)
          praut=0.                   ! cloud water autoconversion to rain (kg/kg; >0)
          pracw=0.                   ! cloud water collection (accretion) by rain (kg/kg; >0)
          prevp=0.                   ! rain evaporation (kg/kg; <0)
!
!--- double check input hydrometeor mixing ratios
!
!          dum=wc-(qi+qw+qr)
!          dum1=abs(dum)
!          dum2=toler*min(wc, qi+qw+qr)
!          if (dum1 .gt. dum2) then
!            write(6,"(/2(a,i4),a,i2)") '{@ i=',i_index,' j=',j_index,   &
!     &                                 ' l=',l
!            write(6,"(4(a12,g11.4,1x))")                                &
!     & '{@ tcold=',tc,'p=',.01*pp,'diff=',dum,'wcold=',wc,              &
!     & '{@ qiold=',qi,'qwold=',qw,'qrold=',qr
!          endif
!
!***********************************************************************
!*********** main microphysics calculations now follow! ****************
!***********************************************************************
!
!--- calculate a few variables, which are used more than once below
!
!--- latent heat of vaporization as a function of temperature from
!      bolton (1980, jas)
!
          xlv=3.148e6-2370*tk        ! latent heat of vaporization (lv)
          xlf=xls-xlv                ! latent heat of fusion (lf)
          xlv1=xlv*rcp               ! lv/cp
          xlf1=xlf*rcp               ! lf/cp
          tk2=1./(tk*tk)             ! 1./tk**2
          xlv2=xlv*xlv*qsw*tk2/rv    ! lv**2*qsw/(rv*tk**2)
          denomw=1.+xlv2*rcp         ! denominator term, clausius-clapeyron correction
!
!--- basic thermodynamic quantities
!      * dynvis - dynamic viscosity  [ kg/(m*s) ]
!      * therm_cond - thermal conductivity  [ j/(m*s*k) ]
!      * diffus - diffusivity of water vapor  [ m**2/s ]
!
          tfactor=tk**1.5/(tk+120.)
          dynvis=1.496e-6*tfactor
          therm_cond=2.116e-3*tfactor
          diffus=8.794e-5*tk**1.81/pp
!
!--- air resistance term for the fall speed of ice following the
!      basic research by heymsfield, kajikawa, others 
!
          gammas=(1.e5/pp)**c1
!
!--- air resistance for rain fall speed (beard, 1985, jaot, p.470)
!
!moorthi  gammar=(rho0/rho)**.4
          gammar=sqrt(rho0/rho)
!
!----------------------------------------------------------------------
!-------------  important microphysics decision tree  -----------------
!----------------------------------------------------------------------
!
!--- determine if conditions supporting ice are present
!
          if (tc.lt.0. .or. qi.gt.epsq .or. asnow.gt.climit) then
            ice_logical=.true.
          else
            ice_logical=.false.
            qlice=0.
            qtice=0.
          endif
!
!--- determine if rain is present
!
          rain_logical=.false.
          if (arain.gt.climit .or. qr.gt.epsq) rain_logical=.true.
!
          if (ice_logical) then
!
!--- important:  estimate time-averaged properties.
!
!---
!  * flarge  - ratio of number of large ice to total (large & small) ice
!  * fsmall  - ratio of number of small ice crystals to large ice particles
!  ->  small ice particles are assumed to have a mean diameter of 50 microns.
!  * xsimass - used for calculating small ice mixing ratio
!---
!  * tot_ice - total mass (small & large) ice before microphysics,
!              which is the sum of the total mass of large ice in the 
!              current layer and the input flux of ice from above
!  * piloss  - greatest loss (<0) of total (small & large) ice by
!              sublimation, removing all of the ice falling from above
!              and the ice within the layer
!  * rimef1  - rime factor, which is the mass ratio of total (unrimed & rimed) 
!              ice mass to the unrimed ice mass (>=1)
!  * vrimef  - the velocity increase due to rime factor or melting (ratio, >=1)
!  * vsnow   - fall speed of rimed snow w/ air resistance correction
!  * emairi  - equivalent mass of air associated layer and with fall of snow into layer
!  * xlimass - used for calculating large ice mixing ratio
!  * flimass - mass fraction of large ice
!  * qtice   - time-averaged mixing ratio of total ice
!  * qlice   - time-averaged mixing ratio of large ice
!  * nlice   - time-averaged number concentration of large ice
!  * nsmice  - number concentration of small ice crystals at current level
!---
!--- assumed number fraction of large ice particles to total (large & small) 
!    ice particles, which is based on a general impression of the literature.
!
            wvqw=wv+qw                ! water vapor & cloud water
!
            if (tc.ge.0. .or. wvqw.lt.qsigrd) then
   !
   !--- eliminate small ice particle contributions for melting & sublimation
   !
              flarge=1.
            else
   !
   !--- enhanced number of small ice particles during depositional growth
   !    (effective only when 0c > t >= t_ice [-10c] )
   !
   !          flarge=.2
              flarge=.15
   !          flarge=.1
   !
   !--- larger number of small ice particles due to rime splintering
   !
              if (tc.ge.-8. .and. tc.le.-3.) flarge=.5*flarge
!
            endif            ! end if (tc.ge.0. .or. wvqw.lt.qsigrd)
            fsmall=(1.-flarge)/flarge
            xsimass=rrho*massi(mdimin)*fsmall
            if (qi.le.epsq .and. asnow.le.climit) then
              indexs=mdimin
              tot_ice=0.
              piloss=0.
              rimef1=1.
              vrimef=1.
              vel_inc=gammas
              vsnow=0.
              emairi=thick
              xlimass=rrho*rimef1*massi(indexs)
              flimass=xlimass/(xlimass+xsimass)
              qlice=0.
              qtice=0.
              nlice=0.
              nsmice=0.
            else
   !
   !--- for t<0c mean particle size follows houze et al. (jas, 1979, p. 160), 
   !    converted from fig. 5 plot of lamdas.  similar set of relationships 
   !    also shown in fig. 8 of ryan (bams, 1996, p. 66).
   !
!moorthi      dum=xmimax*exp(.0536*tc)
              dum=xmimax*exp(.0550*tc)
!             dum=xmimax*exp(.0540*tc)
              indexs=min(mdimax, max(mdimin, int(dum) ) )
              tot_ice=thick*qi+blend*asnow
              piloss=-tot_ice/thick
              lbef=max(1,l-1)
              dum1=rimef_col(lbef)
              dum2=rimef_col(l)
              rimef1=(dum2*thick*qi+dum1*blend*asnow)/tot_ice
              rimef1=min(rimef1, rfmax)
              do ipass=0,1
                if (rimef1 .le. 1.) then
                  rimef1=1.
                  vrimef=1.
                else
                  ixs=max(2, min(indexs/100, 9))
                  xrf=10.492*alog(rimef1)
                  ixrf=max(0, min(int(xrf), nrime))
                  if (ixrf .ge. nrime) then
                    vrimef=vel_rf(ixs,nrime)
                  else
                    vrimef=vel_rf(ixs,ixrf)+(xrf-float(ixrf))*          &
     &                    (vel_rf(ixs,ixrf+1)-vel_rf(ixs,ixrf))
                  endif
                endif            ! end if (rimef1 .le. 1.)
                vel_inc=gammas*vrimef
                vsnow=vel_inc*vsnowi(indexs)
                emairi=thick+bldtrh*vsnow
                xlimass=rrho*rimef1*massi(indexs)
                flimass=xlimass/(xlimass+xsimass)
                qtice=tot_ice/emairi
                qlice=flimass*qtice
                nlice=qlice/xlimass
                nsmice=fsmall*nlice
   !
                if ( (nlice.ge.nlimin .and. nlice.le.nlimax)            &
     &                .or. ipass.eq.1) then
                  exit
                else
        !
        !--- reduce excessive accumulation of ice at upper levels
        !    associated with strong grid-resolved ascent
        !
        !--- force nlice to be between nlimin and nlimax
        !
                  dum=max(nlimin, min(nlimax, nlice) )
                  xli=rho*(qtice/dum-xsimass)/rimef1
                  if (xli .le. massi(mdimin) ) then
                    indexs=mdimin
                  else if (xli .le. massi(450) ) then
                    dli=9.5885e5*xli**.42066         ! dli in microns
                    indexs=min(mdimax, max(mdimin, int(dli) ) )
                  else if (xli .le. massi(mdimax) ) then
                    dli=3.9751e6*xli**.49870         ! dli in microns
                    indexs=min(mdimax, max(mdimin, int(dli) ) )
                  else 
                    indexs=mdimax
           !
           !--- 8/22/01: increase density of large ice if maximum limits 
           !    are reached for number concentration (nlimax) and mean size 
           !    (mdimax).  done to increase fall out of ice.
           !
                    if (dum .ge. nlimax)                                &
     &                rimef1=rho*(qtice/nlimax-xsimass)/massi(indexs)
                  endif             ! end if (xli .le. massi(mdimin) ) 
!            write(6,"(4(a12,g11.4,1x))")                               &
!     & '{$ tc=',tc,'p=',.01*pp,'nlice=',nlice,'dum=',dum,              &
!     & '{$ xli=',xli,'indexs=',float(indexs),'rho=',rho,'qtice=',qtice,&
!     & '{$ xsimass=',xsimass,'rimef1=',rimef1
                endif                  ! end if ( (nlice.ge.nlimin .and. nlice.le.nlimax) ...
              enddo                    ! end do ipass=0,1
            endif                      ! end if (qi.le.epsq .and. asnow.le.climit)
          endif                        ! end if (ice_logical)
!
!----------------------------------------------------------------------
!--------------- calculate individual processes -----------------------
!----------------------------------------------------------------------
!
!--- cloud water autoconversion to rain and collection by rain
!
          if (qw.gt.epsq .and. tc.ge.t_ice) then
   !
   !--- qw0 could be modified based on land/sea properties, 
   !      presence of convection, etc.  this is why qaut0 and craut
   !      are passed into the subroutine as externally determined
   !      parameters.  can be changed in the future if desired.
   !
!           qw0=qaut0*rrho
            qw0=qautx*rrho*xncw(l)
            praut=max(0., qw-qw0)*craut
            if (qlice .gt. epsq) then
      !
      !--- collection of cloud water by large ice particles ("snow")
      !    piacwi=piacw for riming, piacwi=0 for shedding
      !
              fws=min(1., ciacw*vel_inc*nlice*accri(indexs)/pp**c1)
              piacw=fws*qw
              if (tc .lt. 0.) piacwi=piacw    ! large ice riming
            endif           ! end if (qlice .gt. epsq)
          endif             ! end if (qw.gt.epsq .and. tc.ge.t_ice)
!
!----------------------------------------------------------------------
!--- loop around some of the ice-phase processes if no ice should be present
!----------------------------------------------------------------------
!
          if (ice_logical .eqv. .false.) go to 20
!
!--- now the pretzel logic of calculating ice deposition
!
          if (tc.lt.t_ice .and. (wv.gt.qsigrd .or. qw.gt.epsq)) then
   !
   !--- adjust to ice saturation at t<t_ice (-10c) if supersaturated.
   !    sources of ice due to nucleation and convective detrainment are
   !    either poorly understood, poorly resolved at typical nwp 
   !    resolutions, or are not represented (e.g., no detrained 
   !    condensate in bmj cu scheme).
   !    
            pcond=-qw
            dum1=tk+xlv1*pcond                 ! updated (dummy) temperature (deg k)
            dum2=wv+qw                         ! updated (dummy) water vapor mixing ratio
            dum=min(pp,fpvsi(dum1))            ! updated (dummy) saturation vapor pressure w/r/t ice
            dum=rhgrd*eps*dum/(pp+epsm1*dum)   ! updated (dummy) saturation specific humidity w/r/t ice
!           dum=rhgrd*eps*dum/(pp-dum)         ! updated (dummy) saturation mixing ratio w/r/t ice
            if (dum2 .gt. dum) pidep=deposit (pp, rhgrd, dum1, dum2)
            dwvi=0.    ! used only for debugging
   !
          else if (tc .lt. 0.) then
   !
   !--- these quantities are handy for ice deposition/sublimation
   !    pidep_max - max deposition or minimum sublimation to ice saturation
   !
            denomi=1.+xls2*qsi*tk2
            dwvi=min(wvqw,qsw)-qsi
            pidep_max=max(piloss, dwvi/denomi)
            if (qtice .gt. 0.) then
      !
      !--- calculate ice deposition/sublimation
      !      * sfactor - [vel_inc**.5]*[schmidt**(1./3.)]*[(rho/dynvis)**.5],
      !        where schmidt (schmidt number) =dynvis/(rho*diffus)
      !      * units: sfactor - s**.5/m ;  abi - m**2/s ;  nlice - m**-3 ;
      !               ventil, ventis - m**-2 ;  venti1 - m ;  
      !               venti2 - m**2/s**.5 ; didep - unitless
      !
              sfactor=vel_inc**.5*(rho/(diffus*diffus*dynvis))**c2
              abi=1./(rho*xls3*qsi*tk2/therm_cond+1./diffus)
      !
      !--- ventil - number concentration * ventilation factors for large ice
      !--- ventis - number concentration * ventilation factors for small ice
      !
      !--- variation in the number concentration of ice with time is not
      !      accounted for in these calculations (could be in the future).
      !
              ventil=(venti1(indexs)+sfactor*venti2(indexs))*nlice
              ventis=(venti1(mdimin)+sfactor*venti2(mdimin))*nsmice
              didep=abi*(ventil+ventis)*dtph
      !
      !--- account for change in water vapor supply w/ time
      !
              if (didep .ge. xratio)                                    &
     &          didep=(1.-exp(-didep*denomi))/denomi
              if (dwvi .gt. 0.) then
                pidep=min(dwvi*didep, pidep_max)
              else if (dwvi .lt. 0.) then
                pidep=max(dwvi*didep, pidep_max)
              endif
      !
            else if (wvqw.gt.qsi .and. tc.le.t_ice_init) then
      !
      !--- ice nucleation in near water-saturated conditions.  ice crystal
      !    growth during time step calculated using miller & young (1979, jas).
      !--- these deposition rates could drive conditions below water saturation,
      !    which is the basis of these calculations.  intended to approximate
      !    more complex & computationally intensive calculations.
      !
              index_my=max(my_t1, min( int(.5-tc), my_t2 ) )
      !
      !--- dum1 is the supersaturation w/r/t ice at water-saturated conditions
      !
      !--- dum2 is the number of ice crystals nucleated at water-saturated 
      !    conditions based on meyers et al. (jam, 1992).
      !
      !--- prevent unrealistically large ice initiation (limited by pidep_max)
      !      if dum2 values are increased in future experiments
      !
              dum1=qsw/qsi-1.      
              dum2=1.e3*exp(12.96*dum1-0.639)
              pidep=min(pidep_max, dum2*my_growth(index_my)*rrho)
      !
            endif       ! end if (qtice .gt. 0.)
   !
          endif         ! end if (tc.lt.t_ice .and. (wv.gt.qsigrd .or. qw.gt.epsq))
!
!------------------------------------------------------------------------
!
20      continue     ! jump here if conditions for ice are not present
!
!------------------------------------------------------------------------
!
!--- cloud water condensation
!
          if (tc.ge.t_ice .and. (qw.gt.epsq .or. wv.gt.qswgrd)) then
            if (piacwi.eq.0. .and. pidep.eq.0.) then
              pcond=condense (pp, qw, rhgrd, tk, wv)
            else
   !
   !--- modify cloud condensation in response to ice processes
   !
              dum=xlv*qswgrd*rcprv*tk2
              denomwi=1.+xls*dum
              denomf=xlf*dum
              dum=max(0., pidep)
              pcond=(wv-qswgrd-denomwi*dum-denomf*piacwi)/denomw
              dum1=-qw
              dum2=pcond-piacw
              if (dum2 .lt. dum1) then
      !
      !--- limit cloud water sinks
      !
                dum=dum1/dum2
                pcond=dum*pcond
                piacw=dum*piacw
                piacwi=dum*piacwi
              endif        ! end if (dum2 .lt. dum1)
            endif          ! end if (piacwi.eq.0. .and. pidep.eq.0.)
          endif            ! end if (tc.ge.t_ice .and. (qw.gt.epsq .or. wv.gt.qswgrd))
!
!--- limit freezing of accreted rime to prevent temperature oscillations,
!    a crude schumann-ludlam limit (p. 209 of young, 1993). 
!
          tcc=tc+xlv1*pcond+xls1*pidep+xlf1*piacwi
          if (tcc .gt. 0.) then
            piacwi=0.
            tcc=tc+xlv1*pcond+xls1*pidep
          endif
!
          if (tc.gt.0. .and. tcc.gt.0. .and. ice_logical) then
   !
   !--- calculate melting and evaporation/condensation
   !      * units: sfactor - s**.5/m ;  abi - m**2/s ;  nlice - m**-3 ;
   !               ventil - m**-2 ;  venti1 - m ;  
   !               venti2 - m**2/s**.5 ; cievp - /s
   !
            sfactor=vel_inc**.5*(rho/(diffus*diffus*dynvis))**c2
            ventil=nlice*(venti1(indexs)+sfactor*venti2(indexs))
            aievp=ventil*diffus*dtph
            if (aievp .lt. xratio) then
              dievp=aievp
            else
              dievp=1.-exp(-aievp)
            endif
!           qsw0=eps*esw0/(pp-esw0)
!           qsw0=eps*esw0/(pp+epsm1*esw0)
            dum = min(pp, esw0)
            qsw0=eps*dum/(pp+epsm1*dum)
            dwv0=min(wv,qsw)-qsw0
            dum=qw+pcond
            if (wv.lt.qsw .and. dum.le.epsq) then
   !
   !--- evaporation from melting snow (sink of snow) or shedding
   !    of water condensed onto melting snow (source of rain)
   !
              dum=dwv0*dievp
              pievp=max( min(0., dum), piloss)
              picnd=max(0., dum)
            endif            ! end if (wv.lt.qsw .and. dum.le.epsq)
            pimlt=therm_cond*tcc*ventil*rrho*dtph/xlf
   !
   !--- limit melting to prevent temperature oscillations across 0c
   !
            dum1=max( 0., (tcc+xlv1*pievp)/xlf1 )
            pimlt=min(pimlt, dum1)
   !
   !--- limit loss of snow by melting (>0) and evaporation
   !
            dum=pievp-pimlt
            if (dum .lt. piloss) then
              dum1=piloss/dum
              pimlt=pimlt*dum1
              pievp=pievp*dum1
            endif           ! end if (dum .gt. qtice)
          endif             ! end if (tc.gt.0. .and. tcc.gt.0. .and. ice_logical) 
!
!--- important:  estimate time-averaged properties.
!
!  * tot_rain - total mass of rain before microphysics, which is the sum of
!               the total mass of rain in the current layer and the input 
!               flux of rain from above
!  * vrain1   - fall speed of rain into grid from above (with air resistance correction)
!  * qtrain   - time-averaged mixing ratio of rain (kg/kg)
!  * prloss   - greatest loss (<0) of rain, removing all rain falling from
!               above and the rain within the layer
!  * rqr      - rain content (kg/m**3)
!  * indexr   - mean size of rain drops to the nearest 1 micron in size
!  * n0r      - intercept of rain size distribution (typically 10**6 m**-4)
!
          tot_rain=0.
          vrain1=0.
          qtrain=0.
          prloss=0.
          rqr=0.
          n0r=0.
          indexr1=indexr    ! for debugging only
          indexr=mdrmin
          if (rain_logical) then
            if (arain .le. 0.) then
              indexr=mdrmin
              vrain1=0.
            else
   !
   !--- indexr (related to mean diameter) & n0r could be modified 
   !      by land/sea properties, presence of convection, etc.
   !
   !--- rain rate normalized to a density of 1.194 kg/m**3
   !
              rr=arain/(dtph*gammar)
   !
              if (rr .le. rr_drmin) then
        !
        !--- assume fixed mean diameter of rain (0.2 mm) for low rain rates, 
        !      instead vary n0r with rain rate
        !
                indexr=mdrmin
              else if (rr .le. rr_dr1) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.05 and 0.10 mm:
        !      v(dr)=5.6023e4*dr**1.136, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*5.6023e4*dr**(4+1.136) = 1.408e15*dr**5.136,
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.123e-3*rr**.1947 -> dr (microns) = 1.123e3*rr**.1947
        !
                indexr=int( 1.123e3*rr**.1947 + .5 )
                indexr=max( mdrmin, min(indexr, mdr1) )
              else if (rr .le. rr_dr2) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.10 and 0.20 mm:
        !      v(dr)=1.0867e4*dr**.958, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*1.0867e4*dr**(4+.958) = 2.731e14*dr**4.958,
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.225e-3*rr**.2017 -> dr (microns) = 1.225e3*rr**.2017
        !
                indexr=int( 1.225e3*rr**.2017 + .5 )
                indexr=max( mdr1, min(indexr, mdr2) )
              else if (rr .le. rr_dr3) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.20 and 0.32 mm:
        !      v(dr)=2831.*dr**.80, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*2831.*dr**(4+.80) = 7.115e13*dr**4.80, 
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.3006e-3*rr**.2083 -> dr (microns) = 1.3006e3*rr**.2083
        !
                indexr=int( 1.3006e3*rr**.2083 + .5 )
                indexr=max( mdr2, min(indexr, mdr3) )
              else if (rr .le. rr_drmax) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.32 and 0.45 mm:
        !      v(dr)=944.8*dr**.6636, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*944.8*dr**(4+.6636) = 2.3745e13*dr**4.6636,
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.355e-3*rr**.2144 -> dr (microns) = 1.355e3*rr**.2144
        !
                indexr=int( 1.355e3*rr**.2144 + .5 )
                indexr=max( mdr3, min(indexr, mdrmax) )
              else 
        !
        !--- assume fixed mean diameter of rain (0.45 mm) for high rain rates, 
        !      instead vary n0r with rain rate
        !
                indexr=mdrmax
              endif              ! end if (rr .le. rr_drmin) etc. 
              vrain1=gammar*vrain(indexr)
            endif              ! end if (arain .le. 0.)
            indexr1=indexr     ! for debugging only
            tot_rain=thick*qr+blend*arain
            qtrain=tot_rain/(thick+bldtrh*vrain1)
            prloss=-tot_rain/thick
            rqr=rho*qtrain
   !
   !--- rqr - time-averaged rain content (kg/m**3)
   !
            if (rqr .le. rqr_drmin) then
              n0r=max(n0rmin, cn0r_dmrmin*rqr)
              indexr=mdrmin
            else if (rqr .ge. rqr_drmax) then
              n0r=cn0r_dmrmax*rqr
              indexr=mdrmax
            else
              n0r=n0r0
              indexr=max( xmrmin, min(cn0r0*rqr**.25, xmrmax) )
            endif
   !
            if (tc .lt. t_ice) then
              piacr=-prloss
            else
              dwvr=wv-pcond-qsw
              dum=qw+pcond
              if (dwvr.lt.0. .and. dum.le.epsq) then
      !
      !--- rain evaporation
      !
      !    * rfactor - [gammar**.5]*[schmidt**(1./3.)]*[(rho/dynvis)**.5],
      !        where schmidt (schmidt number) =dynvis/(rho*diffus)
      !
      !    * units: rfactor - s**.5/m ;  abw - m**2/s ;  ventr - m**-2 ;  
      !             n0r - m**-4 ;  ventr1 - m**2 ;  ventr2 - m**3/s**.5 ;
      !             crevp - unitless
      !
!               rfactor=gammar**.5*(rho/(diffus*diffus*dynvis))**c2
                rfactor=sqrt(gammar)*(rho/(diffus*diffus*dynvis))**c2
                abw=1./(rho*xlv2/therm_cond+1./diffus)
      !
      !--- note that ventr1, ventr2 lookup tables do not include the 
      !      1/davg multiplier as in the ice tables
      !
                ventr=n0r*(ventr1(indexr)+rfactor*ventr2(indexr))
                crevp=abw*ventr*dtph
                if (crevp .lt. xratio) then
                  dum=dwvr*crevp
                else
                  dum=dwvr*(1.-exp(-crevp*denomw))/denomw
                endif
                prevp=max(dum, prloss)
              else if (qw .gt. epsq) then
                fwr=cracw*gammar*n0r*accrr(indexr)
                pracw=min(1.,fwr)*qw
              endif           ! end if (dwvr.lt.0. .and. dum.le.epsq)
      !
              if (tc.lt.0. .and. tcc.lt.0.) then
         !
         !--- biggs (1953) heteorogeneous freezing (e.g., lin et al., 1983)
         !
                dum=(exp(abfr*tc)-1.)*(.001*float(indexr))**7
                piacr=min(cbfr*n0r*rrho*dum, qtrain)
                if (qlice .gt. epsq) then
            !
            !--- freezing of rain by collisions w/ large ice
            !
                  dum=gammar*vrain(indexr)
                  dum1=dum-vsnow
            !
            !--- dum2 - difference in spectral fall speeds of rain and
            !      large ice, parameterized following eq. (48) on p. 112 of 
            !      murakami (j. meteor. soc. japan, 1990)
            !
                  dum2=(dum1*dum1+.04*dum*vsnow)**.5
                  dum1=5.e-12*indexr*indexr+2.e-12*indexr*indexs        &
     &                 +.5e-12*indexs*indexs
                  fir=min(1., ciacr*nlice*dum1*dum2)
            !
            !--- future?  should collection by small ice should be included???
            !
                  piacr=min(piacr+fir*qtrain, qtrain)
                endif        ! end if (qlice .gt. epsq)
                dum=prevp-piacr
                if (dum .lt. prloss) then
                  dum1=prloss/dum
                  prevp=dum1*prevp
                  piacr=dum1*piacr
                endif        ! end if (dum .lt. prloss)
              endif          ! end if (tc.lt.0. .and. tcc.lt.0.)
            endif            ! end if (tc .lt. t_ice)
          endif              ! end if (rain_logical) 
!
!----------------------------------------------------------------------
!---------------------- main budget equations -------------------------
!----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!--- update fields, determine characteristics for next lower layer ----
!-----------------------------------------------------------------------
!
!--- carefully limit sinks of cloud water
!
          dum1=piacw+praut+pracw-min(0.,pcond)
          if (dum1 .gt. qw) then
            dum=qw/dum1
            piacw=dum*piacw
            piacwi=dum*piacwi
            praut=dum*praut
            pracw=dum*pracw
            if (pcond .lt. 0.) pcond=dum*pcond
          endif
          piacwr=piacw-piacwi          ! tc >= 0c
!
!--- qwnew - updated cloud water mixing ratio
!
          delw=pcond-piacw-praut-pracw
          qwnew=qw+delw
          if (qwnew .le. epsq) qwnew=0.
          if (qw.gt.0. .and. qwnew.ne.0.) then
            dum=qwnew/qw
            if (dum .lt. toler) qwnew=0.
          endif
!
!--- update temperature and water vapor mixing ratios
!
          delt= xlv1*(pcond+pievp+picnd+prevp)                          &
     &         +xls1*pidep+xlf1*(piacwi+piacr-pimlt)
          tnew=tk+delt
!
          delv=-pcond-pidep-pievp-picnd-prevp
          wvnew=wv+delv
!
!--- update ice mixing ratios
!
!---
!  * tot_icenew - total mass (small & large) ice after microphysics,
!                 which is the sum of the total mass of large ice in the 
!                 current layer and the flux of ice out of the grid box below
!  * rimef      - rime factor, which is the mass ratio of total (unrimed & 
!                 rimed) ice mass to the unrimed ice mass (>=1)
!  * qinew      - updated mixing ratio of total (large & small) ice in layer
!      -> tot_icenew=qinew*thick+bldtrh*qlicenew*vsnow
!        -> but qlicenew=qinew*flimass, so
!      -> tot_icenew=qinew*(thick+bldtrh*flimass*vsnow)
!  * asnownew   - updated accumulation of snow at bottom of grid cell
!---
!
          deli=0.
          rimef=1.
          if (ice_logical) then
            deli=pidep+pievp+piacwi+piacr-pimlt
            tot_icenew=tot_ice+thick*deli
            if (tot_ice.gt.0. .and. tot_icenew.ne.0.) then
              dum=tot_icenew/tot_ice
              if (dum .lt. toler) tot_icenew=0.
            endif
            if (tot_icenew .le. climit) then
              tot_icenew=0.
              rimef=1.
              qinew=0.
              asnownew=0.
            else
      !
      !--- update rime factor if appropriate
      !
              dum=piacwi+piacr
              if (dum.le.epsq .and. pidep.le.epsq) then
                rimef=rimef1
              else
         !
         !--- rime factor, rimef = (total ice mass)/(total unrimed ice mass)
         !      dum1 - total ice mass, rimed & unrimed
         !      dum2 - estimated mass of *unrimed* ice
         !
                dum1=tot_ice+thick*(pidep+dum)
                dum2=tot_ice/rimef1+thick*pidep
                if (dum2 .le. 0.) then
                  rimef=rfmax
                else
                  rimef=min(rfmax, max(1., dum1/dum2) )
                endif
              endif       ! end if (dum.le.epsq .and. pidep.le.epsq)
              qinew=tot_icenew/(thick+bldtrh*flimass*vsnow)
              if (qinew .le. epsq) qinew=0.
              if (qi.gt.0. .and. qinew.ne.0.) then
                dum=qinew/qi
                if (dum .lt. toler) qinew=0.
              endif
              asnownew=bldtrh*flimass*vsnow*qinew
              if (asnow.gt.0. .and. asnownew.ne.0.) then
                dum=asnownew/asnow
                if (dum .lt. toler) asnownew=0.
              endif
            endif         ! end if (tot_icenew .le. climit)
          endif           ! end if (ice_logical)
!
!--- update rain mixing ratios
!
!---
! * tot_rainnew - total mass of rain after microphysics
!                 current layer and the input flux of ice from above
! * vrain2      - time-averaged fall speed of rain in grid and below 
!                 (with air resistance correction)
! * qrnew       - updated rain mixing ratio in layer
!      -> tot_rainnew=qrnew*(thick+bldtrh*vrain2)
!  * arainnew  - updated accumulation of rain at bottom of grid cell
!---
!
          delr=praut+pracw+piacwr-piacr+pimlt+prevp+picnd
          tot_rainnew=tot_rain+thick*delr
          if (tot_rain.gt.0. .and. tot_rainnew.ne.0.) then
            dum=tot_rainnew/tot_rain
            if (dum .lt. toler) tot_rainnew=0.
          endif
          if (tot_rainnew .le. climit) then
            tot_rainnew=0.
            vrain2=0.
            qrnew=0.
            arainnew=0.
          else
   !
   !--- 1st guess time-averaged rain rate at bottom of grid box
   !
            rr=tot_rainnew/(dtph*gammar)
   !
   !--- use same algorithm as above for calculating mean drop diameter
   !      (idr, in microns), which is used to estimate the time-averaged
   !      fall speed of rain drops at the bottom of the grid layer.  this
   !      isn't perfect, but the alternative is solving a transcendental 
   !      equation that is numerically inefficient and nasty to program
   !      (coded in earlier versions of gsmcolumn prior to 8-22-01).
   !
            if (rr .le. rr_drmin) then
              idr=mdrmin
            else if (rr .le. rr_dr1) then
              idr=int( 1.123e3*rr**.1947 + .5 )
              idr=max( mdrmin, min(idr, mdr1) )
            else if (rr .le. rr_dr2) then
              idr=int( 1.225e3*rr**.2017 + .5 )
              idr=max( mdr1, min(idr, mdr2) )
            else if (rr .le. rr_dr3) then
              idr=int( 1.3006e3*rr**.2083 + .5 )
              idr=max( mdr2, min(idr, mdr3) )
            else if (rr .le. rr_drmax) then
              idr=int( 1.355e3*rr**.2144 + .5 )
              idr=max( mdr3, min(idr, mdrmax) )
            else 
              idr=mdrmax
            endif              ! end if (rr .le. rr_drmin)
            vrain2=gammar*vrain(idr)
            qrnew=tot_rainnew/(thick+bldtrh*vrain2)
            if (qrnew .le. epsq) qrnew=0.
            if (qr.gt.0. .and. qrnew.ne.0.) then
              dum=qrnew/qr
              if (dum .lt. toler) qrnew=0.
            endif
            arainnew=bldtrh*vrain2*qrnew
            if (arain.gt.0. .and. arainnew.ne.0.) then
              dum=arainnew/arain
              if (dum .lt. toler) arainnew=0.
            endif
          endif                ! end if (tot_rainnew .le. climit)
!
          wcnew=qwnew+qrnew+qinew
!
!----------------------------------------------------------------------
!-------------- begin debugging & verification ------------------------
!----------------------------------------------------------------------
!
!--- qt, qtnew - total water (vapor & condensate) before & after microphysics, resp.
!
!         qt=thick*(qv+wc_col(l))+arain+asnow
!         qtnew=thick*(wvnew/(1.+wvnew)+wcnew/(1.+wcnew))                &
!    &         +arainnew+asnownew
          qt=thick*(wv+wc)+arain+asnow
          qtnew=thick*(wvnew+wcnew)+arainnew+asnownew
          budget=qt-qtnew
!
!--- additional check on budget preservation, accounting for truncation effects
!
          dbg_logical=.false.
          dum=abs(budget)
          if (dum .gt. toler) then
            dum=dum/min(qt, qtnew)
            if (dum .gt. toler) dbg_logical=.true.
          endif
!
!          dum=(rhgrd+.001)*qsinew
!          if ( (qwnew.gt.epsq .or. qrnew.gt.epsq .or. wvnew.gt.dum)     &
!     &        .and. tc.lt.t_ice )  dbg_logical=.true.
!
!          if (tc.gt.5. .and. qinew.gt.epsq) dbg_logical=.true.
!
          if ((wvnew.lt.epsq .or. dbg_logical) .and. print_diag) then
   !
            write(6,"(/2(a,i4),2(a,i2))") '{} i=',i_index,' j=',j_index,&
     &                                    ' l=',l,' lsfc=',lsfc
   !
            esw=min(pp, fpvsl(tnew))
!           qswnew=eps*esw/(pp-esw)
            qswnew=eps*esw/(pp+epsm1*esw)
            if (tc.lt.0. .or. tnew .lt. 0.) then
              esi=min(pp, fpvsi(tnew))
!             qsinew=eps*esi/(pp-esi)
              qsinew=eps*esi/(pp+epsm1*esi)
            else
              qsi=qsw
              qsinew=qswnew
            endif
            wsnew=qsinew
            write(6,"(4(a12,g11.4,1x))")                                &
     & '{} tcold=',tc,'tcnew=',tnew-t0c,'p=',.01*pp,'rho=',rho,         &
     & '{} thick=',thick,'rhold=',wv/ws,'rhnew=',wvnew/wsnew,           &
     &   'rhgrd=',rhgrd,                                                &
     & '{} rhwold=',wv/qsw,'rhwnew=',wvnew/qswnew,'rhiold=',wv/qsi,     &
     &   'rhinew=',wvnew/qsinew,                                        &
     & '{} qswold=',qsw,'qswnew=',qswnew,'qsiold=',qsi,'qsinew=',qsinew,&
     & '{} wsold=',ws,'wsnew=',wsnew,'wvold=',wv,'wvnew=',wvnew,        &
     & '{} wcold=',wc,'wcnew=',wcnew,'qwold=',qw,'qwnew=',qwnew,        &
     & '{} qiold=',qi,'qinew=',qinew,'qrold=',qr,'qrnew=',qrnew,        &
     & '{} arainold=',arain,'arainnew=',arainnew,'asnowold=',asnow,     &
     &   'asnownew=',asnownew,                                          &
     & '{} tot_rain=',tot_rain,'tot_rainnew=',tot_rainnew,              &
     &   'tot_ice=',tot_ice,'tot_icenew=',tot_icenew,                   &
     & '{} budget=',budget,'qtold=',qt,'qtnew=',qtnew
   !
            write(6,"(4(a12,g11.4,1x))")                                &
     & '{} delt=',delt,'delv=',delv,'delw=',delw,'deli=',deli,          &
     & '{} delr=',delr,'pcond=',pcond,'pidep=',pidep,'pievp=',pievp,    &
     & '{} picnd=',picnd,'prevp=',prevp,'praut=',praut,'pracw=',pracw,  &
     & '{} piacw=',piacw,'piacwi=',piacwi,'piacwr=',piacwr,'pimlt=',    &
     &    pimlt,                                                        &
     & '{} piacr=',piacr
   !
            if (ice_logical) write(6,"(4(a12,g11.4,1x))")               &
     & '{} rimef1=',rimef1,'gammas=',gammas,'vrimef=',vrimef,           &
     &   'vsnow=',vsnow,                                                &
     & '{} indexs=',float(indexs),'flarge=',flarge,'fsmall=',fsmall,    &
     &   'flimass=',flimass,                                            &
     & '{} xsimass=',xsimass,'xlimass=',xlimass,'qlice=',qlice,         &
     &   'qtice=',qtice,                                                &
     & '{} nlice=',nlice,'nsmice=',nsmice,'piloss=',piloss,             &
     &   'emairi=',emairi,                                              &
     & '{} rimef=',rimef
   !
            if (tot_rain.gt.0. .or. tot_rainnew.gt.0.)                  &
     &        write(6,"(4(a12,g11.4,1x))")                              &
     & '{} indexr1=',float(indexr1),'indexr=',float(indexr),            &
     &   'gammar=',gammar,'n0r=',n0r,                                   &
     & '{} vrain1=',vrain1,'vrain2=',vrain2,'qtrain=',qtrain,'rqr=',rqr,&
     & '{} prloss=',prloss,'volr1=',thick+bldtrh*vrain1,                &
     &   'volr2=',thick+bldtrh*vrain2
   !
            if (praut .gt. 0.) write(6,"(a12,g11.4,1x)") '{} qw0=',qw0
   !
            if (pracw .gt. 0.) write(6,"(a12,g11.4,1x)") '{} fwr=',fwr
   !
            if (piacr .gt. 0.) write(6,"(a12,g11.4,1x)") '{} fir=',fir
   !
            dum=pimlt+picnd-prevp-pievp
            if (dum.gt.0. .or. dwvi.ne.0.)                              &
     &        write(6,"(4(a12,g11.4,1x))")                              &
     & '{} tfactor=',tfactor,'dynvis=',dynvis,                          &
     &   'therm_con=',therm_cond,'diffus=',diffus
   !
            if (prevp .lt. 0.) write(6,"(4(a12,g11.4,1x))")             &
     & '{} rfactor=',rfactor,'abw=',abw,'ventr=',ventr,'crevp=',crevp,  &
     & '{} dwvr=',dwvr,'denomw=',denomw
   !
            if (pidep.ne.0. .and. dwvi.ne.0.)                           &
     &        write(6,"(4(a12,g11.4,1x))")                              &
     & '{} dwvi=',dwvi,'denomi=',denomi,'pidep_max=',pidep_max,         &
     &   'sfactor=',sfactor,                                            &
     & '{} abi=',abi,'ventil=',ventil,'ventil1=',venti1(indexs),        &
     &   'ventil2=',sfactor*venti2(indexs),                             &
     & '{} ventis=',ventis,'didep=',didep
   !
            if (pidep.gt.0. .and. pcond.ne.0.)                          &
     &        write(6,"(4(a12,g11.4,1x))")                              &
     & '{} denomw=',denomw,'denomwi=',denomwi,'denomf=',denomf,         &
     &    'dum2=',pcond-piacw
   !
            if (fws .gt. 0.) write(6,"(4(a12,g11.4,1x))")               &
     & '{} fws=',fws
   !
            dum=pimlt+picnd-pievp
            if (dum.gt. 0.) write(6,"(4(a12,g11.4,1x))")                &
     & '{} sfactor=',sfactor,'ventil=',ventil,'ventil1=',venti1(indexs),&
     &   'ventil2=',sfactor*venti2(indexs),                             &
     & '{} aievp=',aievp,'dievp=',dievp,'qsw0=',qsw0,'dwv0=',dwv0
   !
          if(lsfc .gt. 0) stop
          endif
!
!----------------------------------------------------------------------
!-------------- water budget statistics & maximum values --------------
!----------------------------------------------------------------------
!
          if (print_diag) then
            itdx=max( itlo, min( int(tnew-t0c), ithi ) )
            if (qinew .gt. epsq) nstats(itdx,1)=nstats(itdx,1)+1
            if (qinew.gt.epsq .and.  qrnew+qwnew.gt.epsq)               &
     &        nstats(itdx,2)=nstats(itdx,2)+1
            if (qwnew .gt. epsq) nstats(itdx,3)=nstats(itdx,3)+1 
            if (qrnew .gt. epsq) nstats(itdx,4)=nstats(itdx,4)+1
  !
            qmax(itdx,1)=max(qmax(itdx,1), qinew)
            qmax(itdx,2)=max(qmax(itdx,2), qwnew)
            qmax(itdx,3)=max(qmax(itdx,3), qrnew)
            qmax(itdx,4)=max(qmax(itdx,4), asnownew)
            qmax(itdx,5)=max(qmax(itdx,5), arainnew)
            qtot(itdx,1)=qtot(itdx,1)+qinew*thick
            qtot(itdx,2)=qtot(itdx,2)+qwnew*thick
            qtot(itdx,3)=qtot(itdx,3)+qrnew*thick
  !
            qtot(itdx,4)=qtot(itdx,4)+pcond*thick
            qtot(itdx,5)=qtot(itdx,5)+picnd*thick
            qtot(itdx,6)=qtot(itdx,6)+pievp*thick
            qtot(itdx,7)=qtot(itdx,7)+pidep*thick
            qtot(itdx,8)=qtot(itdx,8)+prevp*thick
            qtot(itdx,9)=qtot(itdx,9)+praut*thick
            qtot(itdx,10)=qtot(itdx,10)+pracw*thick
            qtot(itdx,11)=qtot(itdx,11)+pimlt*thick
            qtot(itdx,12)=qtot(itdx,12)+piacw*thick
            qtot(itdx,13)=qtot(itdx,13)+piacwi*thick
            qtot(itdx,14)=qtot(itdx,14)+piacwr*thick
            qtot(itdx,15)=qtot(itdx,15)+piacr*thick
  !
            qtot(itdx,16)=qtot(itdx,16)+(wvnew-wv)*thick
            qtot(itdx,17)=qtot(itdx,17)+(qwnew-qw)*thick
            qtot(itdx,18)=qtot(itdx,18)+(qinew-qi)*thick
            qtot(itdx,19)=qtot(itdx,19)+(qrnew-qr)*thick
            qtot(itdx,20)=qtot(itdx,20)+(arainnew-arain)
            qtot(itdx,21)=qtot(itdx,21)+(asnownew-asnow)
            if (qinew .gt. 0.)                                          &
     &        qtot(itdx,22)=qtot(itdx,22)+qinew*thick/rimef
  !
          endif
!
!----------------------------------------------------------------------
!------------------------- update arrays ------------------------------
!----------------------------------------------------------------------
!
          t_col(l)=tnew                           ! updated temperature
!
!         qv_col(l)=max(epsq, wvnew/(1.+wvnew))   ! updated specific humidity
          qv_col(l)=max(epsq, wvnew           )   ! updated specific humidity
          wc_col(l)=max(epsq, wcnew)              ! updated total condensate mixing ratio
          qi_col(l)=max(epsq, qinew)              ! updated ice mixing ratio
          qr_col(l)=max(epsq, qrnew)              ! updated rain mixing ratio
          qw_col(l)=max(epsq, qwnew)              ! updated cloud water mixing ratio
          rimef_col(l)=rimef                      ! updated rime factor
          asnow=asnownew                          ! updated accumulated snow
          arain=arainnew                          ! updated accumulated rain
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
10      continue         ! @@@@@ end "l" loop through model levels @@@@@
!
        araing = araing + arain
        asnowg = asnowg + asnow
      enddo              ! do for ntimes=1,mic_step
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!-----------------------------------------------------------------------
!--------------------------- return to gsmdrive -----------------------
!-----------------------------------------------------------------------
!
      return
      end
