!!!!!  ==========================================================  !!!!!
!!!!!              rrtm radiation package description              !!!!!
!!!!!  =+========================================================  !!!!!
!                                                                      !
!    the rrtm package includes three parts:                            !
!                                                                      !
!       'radlw_rrtm_param.f'                                           !
!       'radlw_rrtm_datatb.f'                                          !
!       'radlw_rrtm_main.f'                                            !
!                                                                      !
!    the 'radlw_rrtm_param.f' contains:                                !
!                                                                      !
!       'module_radlw_parameters'  -- band parameters set up           !
!       'module_radlw_cntr_para'   -- control parameters set up        !
!                                                                      !
!    the 'radlw_rrtm_datatb.f' contains:                               !
!                                                                      !
!       'module_radlw_aerosols'    -- aerosols data tables (not yet!)  !
!       'module_radlw_avplank'     -- plank flux data                  !
!       'module_radlw_cldprlw'     -- cloud property coefficients      !
!       'module_radlw_kgbnn'       -- absorption coeffients for 16     !
!                                     bands, where nn = 01-16          !
!                                                                      !
!    the 'radlw_rrtm_main.f' contains the main module:                 !
!                                                                      !
!       'module_radlw_main'                                            !
!                                                                      !
!    in the main module 'module_radlw_main' there are only two         !
!    externally callable subroutines:                                  !
!                                                                      !
!                                                                      !
!       'lwrad'     -- main rrtm lw radiation routine                  !
!          inputs:                                                     !
!           (pmid,pint,tmid,tint,qnm,o3mmr,co2vmr,                     !
!            ch4vmr,n2ovmr,o2vmr,c11vmr,c12vmr,c22vmr,ccl4vmr,         !
!            cldfrc,cwp,cip,rew,rei,cldtau,iovr,                       !
!!aer        kprf,idxc,cmix,denn,rh,hzaer,dzaer,idmaer,                !
!            ipts, l,lp1, lprnt,                                       !
!          outputs:                                                    !
!            hlwc,tlwup,slwup,slwdn,tlwu0,slwd0,                       !
!          optional outputs:                                           !
!            uplwc,dnlwc,uplw0,dnlw0,hlw0)                             !
!                                                                      !
!       'rlwinit'   -- initialization routine                          !
!          inputs:                                                     !
!           (none)                                                     !
!          outputs:                                                    !
!           (none)                                                     !
!                                                                      !
!    all the lw radiation subprograms become contained subprograms     !
!    in module 'module_radlw_main' and many of them are not directly   !
!    accessable from places outside the module.                        !
!                                                                      !
!    exterior modules referenced:                                      !
!                                                                      !
!       'module machine'                    in 'machine.f'             !
!       'module physcons'                   in 'physcons.f'            !
!aer    'module module_aerosols'            in 'rad_aerosols.f'        !
!                                                                      !
!    compilation sequence is:                                          !
!                                                                      !
!       'radlw_rrtm_param,f'                                           !
!       'radlw_rrtm_datatb,f'                                          !
!       'radlw_rrtm_main.f'                                            !
!                                                                      !
!    and all should be put in front of routines that use lw modules    !
!                                                                      !
!                                                                      !
!    the original program descriptions:                                !
!                                                                      !
!                               rrtm                                   !
!                                                                      !
!                   rapid radiative transfer model                     !
!                                                                      !
!            atmospheric and environmental research, inc.              !
!                        840 memorial drive                            !
!                        cambridge, ma 02139                           !
!                                                                      !
!                           eli j. mlawer                              !
!                         steven j. taubman~                           !
!                         shepard a. clough                            !
!                                                                      !
!                         ~currently at gfdl                           !
!                                                                      !
!                       email:  mlawer@aer.com                         !
!                                                                      !
!        the authors wish to acknowledge the contributions of the      !
!        following people:  patrick d. brown, michael j. iacono,       !
!        ronald e. farren, luke chen, robert bergstrom.                !
!                                                                      !
!                                                                      !
!    modification history log:                                         !
!                                                                      !
!       nov 1999,  ken campana                                         !
!                  updated to link up with ncep mrf model              !
!       jun 2000,  ken campana                                         !
!                  added option to call aer max/ran overlap            !
!           2001,  shrinivas moorthi                                   !
!                  further updates for mrf model                       !
!       may 2001,  yu-tai hou                                          !
!                  updated on trace gases and cloud property based on  !
!                  rrtm_v3.0 codes                                     !
!       dec 2001,  yu-tai hou                                          !
!                  rewritten code into fortran 90                      !
!                                                                      !
!                                                                      !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radlw_main           !
!........................................!
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : nbands, ngpt, n5000, n200
!
      implicit none
!
      private

!  ---  constant values
      real (kind=kind_rad) :: eps, oneminus, bpade, stpfac, wtnum       &
     &,     avgdro, secang, co2avg, zero

      parameter (eps=1.0e-6,  oneminus=1.0-eps)
      parameter (bpade=1.0/0.278)      ! pade approximation constant
      parameter (stpfac=296./1013.)
      parameter (wtnum=0.5)
      parameter (avgdro=6.022e23)      ! avogadro constant  (1/mol)
      parameter (secang=1.66)          ! diffusivity angle
      parameter (co2avg=3.55e-4)       ! average co2 value for cal. of co2mult
      parameter (zero=0.0)

!  ...  atomic weights for conversion from mass to volume mixing ratios
      real (kind=kind_rad) :: amd, amw, amdw, amdo3

      parameter (amd=28.9644,   amw=18.0154)
      parameter (amdw=1.607758, amdo3=0.603428)

!  ...  band indices
      integer, save :: nspa(nbands), nspb(nbands), ngb(ngpt), ifpr

      data nspa / 1, 1,10, 9, 9, 1, 9, 1,11, 1, 1, 9, 9, 1, 9, 9 /
      data nspb / 1, 1, 5, 6, 5, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 /
      data ngb  /  8*1, 14*2, 16*3, 14*4, 16*5,  8*6, 12*7,  8*8,       &
     &            12*9, 6*10, 8*11, 8*12, 4*13, 2*14, 2*15, 2*16 /

!  ...  band wavenumber intervals
!     real (kind=kind_rad), save :: wavenum1(nbands), wavenum2(nbands)
!     data wavenum1/                                                    &
!    &         10.,  250.,  500.,  630.,  700.,  820.,  980., 1080.,    &
!    &       1180., 1390., 1480., 1800., 2080., 2250., 2380., 2600. /
!     data wavenum2/                                                    &
!    &        250.,  500.,  630.,  700.,  820.,  980., 1080., 1180.,    &
!    &       1390., 1480., 1800., 2080., 2250., 2380., 2600., 3000. /
      real (kind=kind_rad), save :: delwave(nbands)
      data delwave / 240., 250., 130.,  70., 120., 160., 100., 100.,    &
     &               210.,  90., 320., 280., 170., 130., 220., 400. /

!  ---  reference pressure and temperature
      real (kind=kind_rad), dimension(59), save :: pref, preflog, tref

!  ...  these pressures are chosen such that the ln of the first one
!       has only a few non-zero digits (i.e. ln(pref(1)) = 6.96000) and
!       each subsequent ln(pref) differs from the previous one by 0.2.
      data pref /                                                       &
     &    1.05363e+03,8.62642e+02,7.06272e+02,5.78246e+02,4.73428e+02,  &
     &    3.87610e+02,3.17348e+02,2.59823e+02,2.12725e+02,1.74164e+02,  &
     &    1.42594e+02,1.16746e+02,9.55835e+01,7.82571e+01,6.40715e+01,  &
     &    5.24573e+01,4.29484e+01,3.51632e+01,2.87892e+01,2.35706e+01,  &
     &    1.92980e+01,1.57998e+01,1.29358e+01,1.05910e+01,8.67114e+00,  &
     &    7.09933e+00,5.81244e+00,4.75882e+00,3.89619e+00,3.18993e+00,  &
     &    2.61170e+00,2.13828e+00,1.75067e+00,1.43333e+00,1.17351e+00,  &
     &    9.60789e-01,7.86628e-01,6.44036e-01,5.27292e-01,4.31710e-01,  &
     &    3.53455e-01,2.89384e-01,2.36928e-01,1.93980e-01,1.58817e-01,  &
     &    1.30029e-01,1.06458e-01,8.71608e-02,7.13612e-02,5.84256e-02,  &
     &    4.78349e-02,3.91639e-02,3.20647e-02,2.62523e-02,2.14936e-02,  &
     &    1.75975e-02,1.44076e-02,1.17959e-02,9.65769e-03 /
      data preflog /                                                    &
     &     6.9600e+00, 6.7600e+00, 6.5600e+00, 6.3600e+00, 6.1600e+00,  &
     &     5.9600e+00, 5.7600e+00, 5.5600e+00, 5.3600e+00, 5.1600e+00,  &
     &     4.9600e+00, 4.7600e+00, 4.5600e+00, 4.3600e+00, 4.1600e+00,  &
     &     3.9600e+00, 3.7600e+00, 3.5600e+00, 3.3600e+00, 3.1600e+00,  &
     &     2.9600e+00, 2.7600e+00, 2.5600e+00, 2.3600e+00, 2.1600e+00,  &
     &     1.9600e+00, 1.7600e+00, 1.5600e+00, 1.3600e+00, 1.1600e+00,  &
     &     9.6000e-01, 7.6000e-01, 5.6000e-01, 3.6000e-01, 1.6000e-01,  &
     &    -4.0000e-02,-2.4000e-01,-4.4000e-01,-6.4000e-01,-8.4000e-01,  &
     &    -1.0400e+00,-1.2400e+00,-1.4400e+00,-1.6400e+00,-1.8400e+00,  &
     &    -2.0400e+00,-2.2400e+00,-2.4400e+00,-2.6400e+00,-2.8400e+00,  &
     &    -3.0400e+00,-3.2400e+00,-3.4400e+00,-3.6400e+00,-3.8400e+00,  &
     &    -4.0400e+00,-4.2400e+00,-4.4400e+00,-4.6400e+00 /
!  ...  these are the temperatures associated with the respective
!       pressures for the mls standard atmosphere.
      data tref /                                                       &
     &     2.9420e+02, 2.8799e+02, 2.7894e+02, 2.6925e+02, 2.5983e+02,  &
     &     2.5017e+02, 2.4077e+02, 2.3179e+02, 2.2306e+02, 2.1578e+02,  &
     &     2.1570e+02, 2.1570e+02, 2.1570e+02, 2.1706e+02, 2.1858e+02,  &
     &     2.2018e+02, 2.2174e+02, 2.2328e+02, 2.2479e+02, 2.2655e+02,  &
     &     2.2834e+02, 2.3113e+02, 2.3401e+02, 2.3703e+02, 2.4022e+02,  &
     &     2.4371e+02, 2.4726e+02, 2.5085e+02, 2.5457e+02, 2.5832e+02,  &
     &     2.6216e+02, 2.6606e+02, 2.6999e+02, 2.7340e+02, 2.7536e+02,  &
     &     2.7568e+02, 2.7372e+02, 2.7163e+02, 2.6955e+02, 2.6593e+02,  &
     &     2.6211e+02, 2.5828e+02, 2.5360e+02, 2.4854e+02, 2.4348e+02,  &
     &     2.3809e+02, 2.3206e+02, 2.2603e+02, 2.2000e+02, 2.1435e+02,  &
     &     2.0887e+02, 2.0340e+02, 1.9792e+02, 1.9290e+02, 1.8809e+02,  &
     &     1.8329e+02, 1.7849e+02, 1.7394e+02, 1.7212e+02 /

      data ifpr / 0 /

!  ---  those data will be set up only once by "rlwinit"

!  ...  fluxfac, heatfac are factors for fluxes (in w/m**2) and heating
!       rates (in k/day, or k/sec set by subroutine 'rlwinit')
!       semiss are surface emissivity for each bands
      real (kind=kind_rad), save :: fluxfac, heatfac, semiss(nbands)

      real (kind=kind_rad), dimension(0:n5000), save :: tau, tf, trans
      real (kind=kind_rad), dimension(0:n200 ), save :: corr1, corr2

      public lwrad, rlwinit


! =================
      contains
! =================

!-----------------------------------
      subroutine lwrad                                                  &
!...................................

!  ---  inputs:
     &     ( pmid,pint,tmid,tint,qnm,o3mmr,co2vmr,ch4vmr,               &
     &       n2ovmr,o2vmr,c11vmr,c12vmr,c22vmr,ccl4vmr,                 &
     &       cldfrc,cwp,cip,rew,rei,cldtau,iovr,icwp,                   &
!aer &       kprf,idxc,cmix,denn,rh,hzaer,dzaer,idmaer,                 &
     &       ipts, l,lp1, lprnt, me,                                    &
!  ---  outputs:
     &       hlwc,tlwup,slwup,slwdn,tlwu0,slwd0                         &
!! ---  optional:
!!   &,      uplwc,dnlwc,uplw0,dnlw0,hlw0                               &
     &     )

!  ====================  defination of variables  ===================  !
!                                                                      !
!  input variables:                                                    !
!     pmid   (ipts,l)       - layer pressures                          !
!     pint   (ipts,lp1)     - interface pressures                      !
!     tmid   (ipts,l)       - layer temperature                        !
!     tint   (ipts,lp1)     - interface temperatures                   !
!     qnm    (ipts,l)       - layer h2o specific humidity (mmr)        !
!     o3mmr  (ipts,l)       - layer o3 mass mixing ratio               !
!     co2vmr (ipts)         - mean atmos co2 volume mixing ratio       !
!     ch4vmr (ipts)         - mean ch4 volume mixing ratio             !
!     n2ovmr (ipts)         - mean n2o volume mixing ratio             !
!     o2vmr  (ipts)         - mean o2 volume mixing ratio              !
!     c11vmr (ipts)         - mean cfc11 volume mixing ratio           !
!     c12vmr (ipts)         - mean cfc12 volume mixing ratio           !
!     c22vmr (ipts)         - mean cfc22 volume mixing ratio           !
!     ccl4vmr(ipts)         - mean ccl4 volume mixing ratio            !
!     cldfrc (ipts,l)       - layer cloud fraction                     !
!     cldtau (ipts,l)       - layer cloud optical depth                !
!     iovr                  - control flag for cloud overlapping       !
!                             =0: random overlapping clouds            !
!                             =1: max/ran overlapping clouds           !
!                             =2: max+ran overlapping clouds (not yet) !
!aer  kprf   (ipts)         - indecies of aerosol prof structures      !
!aer  idxc   (nxc,ipts)     - indecies of aerosol components           !
!aer  cmix   (nxc,ipts)     - mixing ratioes of aerosol components     !
!aer  denn   (ndn,ipts)     - aerosol number densities                 !
!aer  rh     (ipts,l)       - relative humidity                        !
!aer  hzaer  (ipts,lp1)     - level height for aerosols calc in m      !
!aer  dzaer  (ipts,l)       - layer thickness in m                     !
!aer  idmaer (nae,l)        - aerosol domain index array               !
!     cwp    (ipts,l)       - layer cloud liquid water path (g/m**2)   !
!     cip    (ipts,l)       - layer cloud ice water path (g/m**2)      !
!     rew    (ipts,l)       - layer liq-cloud eff radius (micron)      !
!     rei    (ipts,l)       - layer ice-cloud eff radius (micron)      !
!     ipts                  - total number of horizontal points        !
!     l, lp1                - total number of vertical layers, levels  !
!     lprnt                 - cntl flag for diagnostic print out       !
!                                                                      !
!  control parameters in module "module_radlw_cntr_para":              !
!     ilwrate               - heating rate unit selections             !
!                             =1: output in k/day                      !
!                             =2: output in k/second                   !
!aer  iaerlw                - control flag for aerosols (not yet)      !
!                             =0: do not include aerosol effect        !
!                             =1: include aerosol effect               !
!     icfclw                - control flag for cfc gases               !
!                             =0: do not include cfc gases             !
!                             =1: include all cfc gases                !
!     iflagliq              - liq-cloud optical properties contrl flag !
!                             =0: input cld opt dep, ignor iflagice    !
!                             =1: input cwp,cip, (ccm2) ignor iflagice !
!                             =2: input cwp rew, (ccm3 method)         !
!                             =3: input cwp rew, hu and stamnes (1993) !
!     iflagice              - ice-cloud optical properties contrl flag !
!                       * * * if iflagliq .lt. 2, iflafice is ignored  !
!                             =0: input cip rei, (ccm3 method)         !
!                             =1: input cip rei, ebert and curry (1997)!
!                             =2: input cip rei, streamer (1996)       !
!                                                                      !
!  output variables:                                                   !
!     hlwc   (ipts,l)       - total sky heating rate (k/day or k/sec)  !
!     tlwup  (ipts)         - total sky upward flux at top (w/m2)      !
!     slwup  (ipts)         - upward flux at sfc (w/m2)                !
!     slwdn  (ipts)         - total sky downward flux at sfc (w/m2)    !
!     tlwu0  (ipts)         - clear sky upward flux at top (w/m2)      !
!     slwd0  (ipts)         - clear sky downward flux at sfc (w/m2)    !
!                                                                      !
!! optional output variables:                                          !
!!    uplwc  (ipts,lp1)     - total sky upward flux at interface(w/m2) !
!!    dnlwc  (ipts,lp1)     - total sky dnward flux at interface(w/m2) !
!!    uplw0  (ipts,lp1)     - clear sky upward flux at interface(w/m2) !
!!    dnlw0  (ipts,lp1)     - clear sky dnward flux at interface(w/m2) !
!!    hlw0   (ipts,l)       - total sky heating rate (k/day)           !
!                                                                      !
!  module parameters, control and local variables:                     !
!     nbands                - number of longwave spectral bands        !
!     maxgas                - maximum number of absorbing gaseous      !
!     maxxsec               - maximum number of cross-sections         !
!     ngpt                  - total number of g-point subintervals     !
!     ngnn   (nn=1-16)      - number of g-points in band nn            !
!     nspa,nspb(nbands)     - number of lower/upper ref atmos per band !
!     delwave(nbands)       - longwave band width (wavenumbers)        !
!     bpade                 - pade approximation constant (1/0.278)    !
!     pavel  (l)            - layer pressures (mb)                     !
!     delp   (l)            - layer pressure thickness (mb)            !
!     tavel  (l)            - layer temperatures (k)                   !
!     tz     (0:l)          - level (interface) temperatures (k)       !
!     semiss (nbands)       - surface emissivity for each band         !
!     wx     (l,maxxsec)    - cross-section molecules concentration    !
!     coldry (l)            - dry air column amount                    !
!                                   (1.e-20*molecules/cm**2)           !
!     cldfrac(l)            - layer cloud fraction                     !
!     taucloud(nbands,l)    - layer cloud optical depth for each band  !
!     taug   (ngpt,l)       - gaseous optical depths                   !
!     pfrac  (ngpt,l)       - planck fractions                         !
!     itr    (ngpt,l)       - integer look-up table index              !
!     colamt(l,ipts,maxgas) - column amounts of absorbing gases        !
!                             1-maxgas are for watervapor, carbon      !
!                             dioxide, ozone, nitrous oxide, methane,  !
!                             oxigen, respectively (molecules/cm**2)   !
!     co2mult(l)            - the factor used to multiply the ave co2  !
!                             abs coeff to get the added contribution  !
!                             to the optical depth relative to 355 ppm.!
!     facij  (l)            - indicator of interpolation factors       !
!                             =0/1: indicate lower/higher temp & height!
!     selffac(l)            - scale factor for self-continuum, equals  !
!                          (w.v. density)/(atm density at 296k,1013 mb)!
!     selffrac(l)           - factor for temp interpolation of ref     !
!                             self-continuum data                      !
!     indself(l)            - index of the lower two appropriate ref   !
!                             temp for the self-continuum interpolation!
!     laytrop,layswtch,laylow                                          !
!                           - layer at which switch is made from one   !
!                             combination of key species to another    !
!     totuflux(0:l)         - upward longwave flux (w/m2)              !
!     totdflux(0:l)         - downward longwave flux (w/m2)            !
!     totuclfl(0:l)         - clear-sky upward longwave flux (w/m2)    !
!     totdclfl(0:l)         - clear-sky downward longwave flux (w/m2)  !
!     fnet    (0:l)         - net longwave flux (w/m2)                 !
!     fnetc   (0:l)         - clear-sky net longwave flux (w/m2)       !
!                                                                      !
!                                                                      !
!  =====================    end of definitions    ===================  !
!
      use machine,                 only : kind_rad
      use physcons,                only : con_g
      use module_radlw_parameters
      use module_radlw_cntr_para,  only : icfclw, iaerlw
!aer  use module_aerosols,         only : nxc, ndn
!
      implicit none

!  ---  inputs:
      real (kind=kind_rad), dimension(ipts,lp1),intent(in) :: pint,tint
      real (kind=kind_rad), dimension(ipts,l),  intent(in) :: pmid,tmid &
     &,     qnm, o3mmr, cldfrc, cwp, cip, rew, rei, cldtau
      real (kind=kind_rad), dimension(ipts),    intent(in) ::           &
     &      co2vmr,ch4vmr,n2ovmr,o2vmr, c11vmr,c12vmr,c22vmr,ccl4vmr

!aer  real (kind=kind_rad), intent(in) :: hzaer(ipts,lp1),dzaer(ipts,l) &
!aer &,     cmix(nxc,ipts), denn(ndn,ipts), rh(ipts,l)
!aer  integer,  intent(in) :: idxc(nxc,ipts), kprf(ipts), idmaer(nae,l)

      integer,  intent(in) :: ipts, l, lp1, iovr, me, icwp
      logical,  intent(in) :: lprnt

!  ---  outputs:
      real (kind=kind_rad), dimension(ipts,l), intent(out) :: hlwc
      real (kind=kind_rad), dimension(ipts),   intent(out) ::           &
     &      tlwup, slwup, slwdn, tlwu0, slwd0
!! ---  optional outputs:
!!    real (kind=kind_rad), dimension(ipts,l), intent(out) :: hlw0
!!    real (kind=kind_rad), dimension(ipts,lp1),intent(out)::           &
!!   &      uplwc, dnlwc, uplw0, dnlw0

!  ---  locals:
      real (kind=kind_rad), dimension(0:l) :: totuflux, totdflux,       &
     &      totuclfl, totdclfl, htr, htrcl, tz
      real (kind=kind_rad), dimension(l)   :: pavel, tavel, delp,       &
     &      cldfrac, taucl, cwp1, cip1, rew1, rei1, coldry, co2mult,    &
     &      h2ovmr, o3vmr, fac00, fac01, fac10, fac11, forfac, plog,    &
     &      selffac, selffrac
      real (kind=kind_rad) :: colamt(l,maxgas), taucloud(nbands,l),     &
     &      wx(l,maxxsec), pfrac(ngpt,l)
      real (kind=kind_rad) :: fp, ft, ft1, tem0, tem1, tem2

!aer  real (kind=kind_rad) :: rh1(l), cmix1(nxc), denn1(ndn)            &
!aer &,                       hz1(lp1), dz1(l)
!aer  integer              :: idxc1(nxc), kprf1

      integer, dimension(l) :: jp, jt, jt1, indself
      integer :: itr(ngpt,l), laytrop, layswtch, laylow, jp1
      integer :: j, k, iplon
!
!===> ... begin here
!
!===> ... loop over horizontal ipts profiles
!

      lab_do_iplon : do iplon = 1, ipts

!  ---  prepare atmospheric profile for use in rrtm

        tz(0) = tint(iplon,1)
        do k = 1, l
          pavel(k)= pmid(iplon,k)
          delp(k) = pint(iplon,k) - pint(iplon,k+1)
          tavel(k)= tmid(iplon,k)
          tz(k)   = tint(iplon,k+1)
          taucl(k) = cldtau(iplon,k)
          cwp1 (k) = cwp(iplon,k)
          cip1 (k) = cip(iplon,k)
          rew1 (k) = rew(iplon,k)
          rei1 (k) = rei(iplon,k)

!  ---  set absorber amount
          h2ovmr(k) = max(zero, qnm(iplon,k)*amdw/(1.0-qnm(iplon,k)))
          o3vmr (k) = max(zero, o3mmr(iplon,k)*amdo3)
        enddo

        do k = 1, l
          if (cldfrc(iplon,k) .ge. eps) then
            cldfrac(k) = cldfrc(iplon,k)
          else
            cldfrac(k) = zero
          endif
        enddo

!  ---  compute aerosol properties (not yet)

!aer    if (iaerlw .eq. 1) then
!         do k = 1, l
!           rh1(k) = rh(iplon,k)
!           hz1(k) = hzaer(iplon,k)
!           dz1(k) = dzaer(iplon,k)
!         enddo
!         hz1(lp1) = hzaer(iplon,lp1)

!         do k = 1, nxc
!           cmix1(k)  = cmix(k,iplon)
!           idxc1(k)  = idxc(k,iplon)
!         enddo

!         do k = 1, ndn
!           denn1(k)  = denn(k,iplon)
!         enddo
!         kprf1 = kprf(iplon)

!         call aeros                                                    &
!  ---  inputs:
!    &     ( kprf1,idxc1,cmix1,denn1,rh1,hz1,dz1,idmaer                 &
!    &,      l, lp1                                                     &
!  ---  outputs:
!    &,      tauae,ssaae,asyae                                          &
!    &     )
!       else
!         do j = 1, nbands
!           do k = 1, l
!             tauae(k,j) = zero
!             ssaae(k,j) = zero
!             asyae(k,j) = zero
!           enddo
!         enddo
!aer    endif

        tem1 = 100.0 * con_g
        tem2 = 1.0e-20 * 1.0e3 * avgdro
        do k = 1, l
          tem0     = (1-h2ovmr(k))*amd + h2ovmr(k)*amw
          coldry(k)= tem2 * delp(k) / (tem1*tem0*(1 + h2ovmr(k)))
        enddo

!  ---  set up col amount and wx, convert from volume mixing ratio to
!       molec/cm2 based on coldry (scaled to 1.0e-20) for use in rrtm

        do k = 1, l
          tem0 = 1.0e-12 * coldry(k)

          colamt(k,1) =               coldry(k)*h2ovmr(k)      ! h2o
          colamt(k,2) = max(tem0,     coldry(k)*co2vmr(iplon)) ! co2
          colamt(k,3) =               coldry(k)*o3vmr(k)       ! o3
          colamt(k,4) = max(tem0,     coldry(k)*n2ovmr(iplon)) ! n2o
          colamt(k,5) = max(tem0,     coldry(k)*ch4vmr(iplon)) ! ch4
!         colamt(k,6) = max(zero,     coldry(k)*o2vmr(iplon))  ! o2 - not used
        enddo

        if (icfclw .eq. 1) then
          do k = 1, l
            wx (k,1) = max(zero, coldry(k)*ccl4vmr(iplon))
            wx (k,2) = max(zero, coldry(k)*c11vmr(iplon) )
            wx (k,3) = max(zero, coldry(k)*c12vmr(iplon) )
            wx (k,4) = max(zero, coldry(k)*c22vmr(iplon) )
          enddo
        else
          do k = 1, l
            wx (k,1) = zero
            wx (k,2) = zero
            wx (k,3) = zero
            wx (k,4) = zero
          enddo
        endif

        do k = 1, l
!  ...  using e = 1334.2 cm-1.
          tem1 = co2avg * coldry(k)
          co2mult(k) = (colamt(k,2) - tem1) * 272.63                    &
     &               * exp(-1919.4/tavel(k)) / (8.7604e-4*tavel(k))
          forfac(k)  = pavel(k)*stpfac / (tavel(k)*(1.0 + h2ovmr(k)))
        enddo
        
!     if (lprnt) then
!     print *,'  coldry',coldry
!     print *,' wx(*,1) ',(wx(k,1),k=1,l)
!     print *,' wx(*,2) ',(wx(k,2),k=1,l)
!     print *,' wx(*,3) ',(wx(k,3),k=1,l)
!     print *,' wx(*,4) ',(wx(k,4),k=1,l)
!     print *,' iplon ',iplon
!     print *,'  pavel ',pavel
!     print *,'  delp ',delp
!     print *,'  tavel ',tavel
!     print *,'  tz ',tz
!     print *,' h2ovmr ',h2ovmr
!     print *,' o3vmr ',o3vmr
!     print *,' co2vmr',co2vmr
!     print *,' ch4vmr',ch4vmr
!     print *,' n2ovmr',n2ovmr
!     print *,' o2vmr',o2vmr
!     print *,' c11vmr',c11vmr
!     print *,' c12vmr ',c12vmr
!     print *,' c22vmr ',c22vmr
!     print *,' ccl4vmr ',ccl4vmr
!     print *,' cldfrac',cldfrac
!     print *,' taucl',taucl
!     endif

!  ---  calculate cloud optical properties

        call cldprop                                                    &
!  ---  inputs:
     &     ( cldfrac,cwp1,cip1,rew1,rei1,taucl, l, icwp                 &
!  ---  output:
     &,      taucloud                                                   &
     &     )

!     if (lprnt) then
!     print *,' after cldprop'
!     print *,' cwp1',cwp1
!     print *,' cip1',cip1
!     print *,' rew1',rew1
!     print *,' rei1',rei1
!     print *,' taucl',taucl
!     print *,' cldfrac',cldfrac
!     print *,' taucloud',taucloud
!     endif

!  ---  calculate information needed by the radiative transfer routine
!       that is specific to this atmosphere, especially some of the 
!       coefficients and indices needed to compute the optical depths
!       by interpolating data from stored reference atmospheres. 

        laytrop = 0
        layswtch= 0
        laylow  = 0

        do k = 1, l

!  ---  find the two reference pressures on either side of the
!       layer pressure.  store them in jp and jp1.  store in fp the
!       fraction of the difference (in ln(pressure)) between these
!       two values that the layer pressure lies.

          plog(k) = log(pavel(k))
          jp(k)= max(1, min(58, int(36.0 - 5.0*(plog(k)+0.04)) ))
          jp1  = jp(k) + 1
          fp   = 5.0 * (preflog(jp(k)) - plog(k))

!  ---  determine, for each reference pressure (jp and jp1), which
!       reference temperature (these are different for each
!       reference pressure) is nearest the layer temperature but does
!       not exceed it.  store these indices in jt and jt1, resp.
!       store in ft (resp. ft1) the fraction of the way between jt
!       (jt1) and the next highest reference temperature that the
!       layer temperature falls.

          jt(k) = max(1, min(4, int(3.0+(tavel(k)-tref(jp(k)))/15.0)))
          jt1(k)= max(1, min(4, int(3.0+(tavel(k)-tref(jp1)) / 15.0)))
          ft  = ((tavel(k) - tref(jp(k))) / 15.0) - float(jt(k)  - 3)
          ft1 = ((tavel(k) - tref(jp1)  ) / 15.0) - float(jt1(k) - 3)

!  ---  we have now isolated the layer ln pressure and temperature,
!       between two reference pressures and two reference temperatures
!       (for each reference pressure).  we multiply the pressure
!       fraction fp with the appropriate temperature fractions to get
!       the factors that will be needed for the interpolation that yields
!       the optical depths (performed in routines taugbn for band n).

          fac10(k) = (1.0 - fp) * ft
          fac00(k) = (1.0 - fp) * (1.0 - ft)
          fac11(k) = fp * ft1
          fac01(k) = fp * (1.0 - ft1)

        enddo

!  ---  if the pressure is less than ~100mb, perform a different
!       set of species interpolations.

        do k = 1, l
          if (plog(k) .gt. 4.56) then
            laytrop =  laytrop + 1

!  ---  for one band, the "switch" occurs at ~300 mb.
            if (plog(k) .ge. 5.76) layswtch = layswtch + 1
            if (plog(k) .ge. 6.62) laylow   = laylow   + 1

!  ---  set up factors (tem1) needed to separately include the water
!       vapor self-continuum in the calculation of absorption
!       coefficient.

            tem1 = (tavel(k) - 188.0) / 7.2

            selffac(k) = h2ovmr(k) * forfac(k)
            indself(k) = min(9, max(1, int(tem1)-7 ))
            selffrac(k)= tem1 - float(indself(k) + 7)

          else

            selffac(k) = zero
            indself(k) = zero
            selffrac(k)= zero

          endif
        enddo

!  ---  set laylow for profiles with surface pressure less than 750mb.
        if (laylow .eq. 0) laylow = 1

!     if (lprnt) then
!      print *,'laytrop,layswtch,laylow',laytrop,layswtch,laylow
!      print *,'colh2o',(colamt(k,1),k=1,l)
!      print *,'colco2',(colamt(k,2),k=1,l)
!      print *,'colo3', (colamt(k,3),k=1,l)
!      print *,'coln2o',(colamt(k,4),k=1,l)
!      print *,'colch4',(colamt(k,5),k=1,l)
!      print *,'co2mult',co2mult
!      print *,'fac00',fac00
!      print *,'fac01',fac01
!      print *,'fac10',fac10
!      print *,'fac11',fac11
!      print *,'jp',jp
!      print *,'jt',jt
!      print *,'jt1',jt1
!      print *,'selffac',selffac
!      print *,'selffrac',selffrac
!      print *,'indself',indself
!      print *,'forfac',forfac
!     endif

        call taumol                                                     &
!  ---  inputs:
     &     ( laytrop,layswtch,laylow,h2ovmr,colamt,wx,co2mult,          &
     &       fac00,fac01,fac10,fac11,jp,jt,jt1,selffac,selffrac,        &
     &       indself,forfac, l,                                         &
!  ---  outputs:
     &       itr, pfrac                                                 &
     &     )

!     if (lprnt) then
!     print *,' after taumol'
!     do k=1,l
!       write(6,123) k
!123    format(' k =',i3,5x,'pfrac')
!       write(6,122) (pfrac(j,k),j=1,ngpt)
!122    format(10e14.7)
!       write(6,124) k
!124    format(' k =',i3,5x,'itr')
!       write(6,125) (itr(j,k),j=1,ngpt)
!125    format(10i10)
!     enddo
!     endif

!  ---  call the radiative transfer routine.

        if (iovr .eq. 0) then

          call rtrn                                                     &
!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,itr, l,        &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl             &
     &     )

        else

          call rtrnmr                                                   &
!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,itr, l,        &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl             &
     &     )

        endif


!  ---  pass total sky up and down flux profiles to ccm output arrays
!       in mks units. pass total sky heating rate profile to output
!       arrays and convert units to k/sec.

        tlwup(iplon) = totuflux(l)
        slwup(iplon) = totuflux(0)
        slwdn(iplon) = totdflux(0)
        tlwu0(iplon) = totuclfl(l)
        slwd0(iplon) = totdclfl(0)

!! ---  optional fluxes
!!      do k = 0, l
!!        uplwc(iplon,k+1) = totuflux(k)
!!        dnlwc(iplon,k+1) = totdflux(k)
!!        uplw0(iplon,k+1) = totuclfl(k)
!!        dnlw0(iplon,k+1) = totdclfl(k)
!!      enddo

        do k = 0, l-1
          hlwc(iplon,k+1) = htr(k)
!! ---  optional clear sky heating rate
!!        hlw0(iplon,k+1) = htrcl(k)
        enddo

      enddo  lab_do_iplon

!...output print
!     if (lprnt) then
!     print *,' output from subroutine rrtm     '
!     print *,'totuflux',totuflux
!     print *,'totdflux',totdflux
!     print *,'fnet',fnet
!     print *,'htr',htr
!     print *,'totuclfl',totuclfl
!     print *,'totdclfl',totdclfl
!     print *,'fnetc',fnetc
!     print *,'htrcl',htrcl
!     endif

      return
!...................................
      end subroutine lwrad
!-----------------------------------



!-----------------------------------
      subroutine rlwinit
!...................................

!  ---  inputs:  (none)

!  ---  outputs: (none)

!  *******************************************************************  !
!                                                                       !
!  rrtm longwave radiative transfer model                               !
!  atmospheric and environmental research, inc., cambridge, ma          !
!                                                                       !
!  original version:       michael j. iacono; july, 1998                !
!  revision for ncar ccm:  michael j. iacono; september, 1998           !
!                                                                       !
!  this subroutine performs calculations necessary for the initialization
!  of the lw model, rrtm.  lookup tables are computed for use in the lw !
!  radiative transfer, and input absorption coefficient data for each   !
!  spectral band are reduced from 256 g-points to 140 for use in rrtm.  !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
! definitions:                                                          !
!     arrays for 5000-point look-up tables:                             !
!     tau  - clear-sky optical depth (used in cloudy radiative transfer)!
!     tf     tau transition function; i.e. the transition of the planck !
!            function from that for the mean layer temperature to that  !
!            for the layer boundary temperature as a function of optical!
!            depth. the "linear in tau" method is used to make the table!
!     trans- transmittance                                              !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!  inputs:  (none)                                                      !
!                                                                       !
!  outputs: (none)                                                      !
!                                                                       !
!  control flags in module "module_radlw_cntr_para":                    !
!     ilwrate - heating rate unit selections                            !
!               =1: output in k/day                                     !
!               =2: output in k/second                                  !
!                                                                       !
!  *******************************************************************  !
!
      use machine,                  only : kind_rad
      use physcons,                 only : con_g, con_cp
      use module_radlw_parameters,  only : n5000, n200
      use module_radlw_cntr_para,   only : ilwrate
!
      implicit none
!
!  ---  inputs: none

!  ---  outputs: none

!  ---  locals:
      real (kind=kind_rad) :: tfn, fp, rtfp, pival, explimit
      integer              :: i
!
!===> ... begin here
!

!  --- ...  setup surface emissivity for each band here

      semiss(:) = 1.0          ! default value

!  --- ...  setup constant factors for flux and heating rate
!           the 1.0e-2 is to convert pressure from mb to n/m**2

      pival = 2.0*asin(1.0)
      fluxfac = pival * 2.0d4
!     fluxfac = 62831.85307179586                   ! = 2 * pi * 1.0e4

      if (ilwrate .eq. 1) then
!       heatfac = con_g * 86400. * 1.0e-2 / con_cp  !   (in k/day)
        heatfac = con_g * 864.0 / con_cp            !   (in k/day)
      else
        heatfac = con_g * 1.0e-2 / con_cp           !   (in k/second)
      endif

!  --- ...  compute lookup tables for transmittance, tau transition
!           function, and clear sky tau (for the cloudy sky radiative
!           transfer).  tau is computed as a function of the tau 
!           transition function, transmittance is calculated as a 
!           function of tau, and the tau transition function is 
!           calculated using the linear in tau formulation at values of
!           tau above 0.01.  tf is approximated as tau/6 for tau < 0.01.
!           all tables are computed at intervals of 0.001.  the inverse
!           of the constant used in the pade approximation to the tau
!           transition function is set to b.

      tau  (0) = zero
      tf   (0) = zero
      trans(0) = 1.0

      tau  (n5000) = 1.e10
      tf   (n5000) = 1.0
      trans(n5000) = zero

      explimit = aint( -log(tiny(trans(0))) )

      do i = 1, n5000-1
         tfn = real(i, kind_rad) / real(n5000-i, kind_rad)
         tau  (i) = bpade * tfn
         if (tau(i) .ge. explimit) then
           trans(i) = zero
         else
           trans(i) = exp(-tau(i))
         endif

         if (tau(i) .lt. 0.1) then
            tf(i) = tau(i) / 6.0
         else
            tf(i) = 1. - 2.*( (1./tau(i)) - (trans(i)/(1.-trans(i))) )
         endif
      enddo

!  --- ...  calculate lookup tables for functions needed in routine
!           taumol (taugb2)

      corr1(0) = 1.0
      corr2(0) = 1.0

      corr1(n200) = 1.0
      corr2(n200) = 1.0

      do i = 1, n200-1
         fp = 0.005 * float(i)
         rtfp = sqrt(fp)
         corr1(i) = rtfp / fp
         corr2(i) = (1.0 - rtfp) / (1.0 - fp)
      enddo

!...................................
      end subroutine rlwinit
!-----------------------------------



!-----------------------------------
      subroutine cldprop                                                &
!...................................

!  ---  inputs:                                                         
     &     ( cldfrac,cliqp,cicep,reliq,reice,taucl, l, icwp,            &
!  ---  output:
     &       taucloud                                                   &
     &     )

!  *******************************************************************  !
!                                                                       !
!    purpose:  compute the cloud optical depth(s) for each cloudy layer.!
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!    explanation of the method for each value of iflagliq, and iflagice.!
!    set up in module "module_radlw_cntr_para"                          !
!                                                                       !
!     iflagliq=0 and =1 do not distingish being liquid and ice clouds.  !
!     iflagliq=2 and =3 does distinguish between liquid and ice clouds, !
!                  and requires further user input (iflagice) to specify!
!                  the method to be used to compute the aborption due to!
!                  liquid and ice parts.                                !
!  ...................................................................  !
!                                                                       !
!     iflagliq=0:  for each cloudy layer, the cloud fraction and (gray) !
!                  optical depth are input.                             !
!     iflagliq=1:  for each cloudy layer, the cloud fraction and cloud  !
!                  water path (g/m2) are input.  using clp+cip, the     !
!                  (gray) cloud optical depth is computed as in ccm2.   !
!     iflagliq=2:  the optical depths due to water clouds are computed  !
!                  as in ccm3.                                          !
!     iflagliq=3:  the water droplet effective radius (microns) is input!
!                  and the opt depths due to water clouds are computed  !
!                  as in hu and stamnes, j., clim., 6, 728-742, (1993). !
!                  the values for absorption coefficients appropriate for
!                  the spectral bands in rrtm have been obtained for a  !
!                  range of effective radii by an averaging procedure   !
!                  based on the work of j. pinto (private communication).
!                  linear interpolation is used to get the absorption   !
!                  coefficients for the input effective radius.         !
!                                                                       !
!     iflagice=0:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in ccm3.                      !
!     iflagice=1:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in ebert and curry, jgr, 97,  !
!                  3831-3836 (1992).  the spectral regions in this work !
!                  have been matched with the spectral bands in rrtm to !
!                  as great an extent as possible:                      !
!                     e&c 1      ib = 5      rrtm bands 9-16            !
!                     e&c 2      ib = 4      rrtm bands 6-8             !
!                     e&c 3      ib = 3      rrtm bands 3-5             !
!                     e&c 4      ib = 2      rrtm band 2                !
!                     e&c 5      ib = 1      rrtm band 1                !
!     iflagice=2:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in streamer (reference: j. key,
!                  streamer users guide, technical report 96-01,        !
!                  department of geography, boston university, 85 pp.   !
!                  (1996)).  the values of absorption coefficients      !
!                  appropriate for the spectral bands of rrtm were      !
!                  obtained by an averaging procedure based on the work !
!                  of j. pinto (private communication).                 !
!                                                                       !
!  *******************************************************************  !
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : nbands
      use module_radlw_cntr_para,  only : iflagliq, iflagice
      use module_radlw_cldprlw

      implicit none

!  ---  inputs:
      real (kind=kind_rad), dimension(l), intent(in) :: cldfrac,        &
     &      cliqp, cicep, reliq, reice, taucl

      integer, intent(in) :: l, icwp

!  ---  outputs:
      real (kind=kind_rad), intent(out) :: taucloud(nbands,l)

!  ---  locals:
      real (kind=kind_rad) :: cliq, cice, radliq, radice, factor, fint
      integer              :: j, k, index

!
!===> ... begin here
!
      do k = 1, l
        do j = 1, nbands
          taucloud(j,k) = zero
        enddo
      enddo

      lab_do_k : do k = 1, l

        lab_if_cld : if (cldfrac(k) .ge. eps) then

!  ---  ice clouds and water clouds combined.
          lab_if_liq : if (iflagliq .eq. 0 .or. icwp .eq. 0) then

            do j = 1, nbands
              taucloud(j,k) = taucl(k)
            enddo

          elseif (iflagliq .eq. 1) then  lab_if_liq

            taucloud(1,k) = absliq1 * cliqp(k)
!           taucloud(1,k) = absliq1 * (cliqp(k) + cicep(k))
            do j = 2,nbands
              taucloud(j,k) = taucloud(1,k)
            enddo

!  ---  separate treatement of ice clouds and water clouds.
          else  lab_if_liq

            cliq = cliqp(k)
            cice = cicep(k)
            radliq = max(2.5e0, min(60.0e0, real(reliq(k)) ))
            radice = reice(k)
!           radice = max(13.e0, min(130.e0, real(reice(k)) ))

!  ---  calculation of absorption coefficients due to liquid clouds.
            if (cliq .eq. zero) then
              do j = 1, nbands
                abscoliq(j) = zero
              enddo
            elseif (iflagliq .eq. 2) then
              abscoliq(1) = cliq * absliq2
              do j = 2, nbands
                abscoliq(j) = abscoliq(1)
              enddo
            elseif (iflagliq .eq. 3) then
              factor = radliq - 1.5
              index = min(57, int(factor))
              fint = factor - index
              do j = 1, nbands
                abscoliq(j) = cliq * (absliq3(index,j) + fint *         &
     &                    (absliq3(index+1,j) - (absliq3(index,j))))
              enddo
            endif

!  ---  calculation of absorption coefficients due to ice clouds.
            if (cice .eq. zero) then
              do j = 1, nbands
                abscoice(j) = zero
              enddo
            elseif (iflagice .eq. 0) then
              abscoice(1) = cice * (absice0(1) + absice0(2)/radice)
              do j = 2, nbands
                abscoice(j) = abscoice(1)
              enddo
            elseif (iflagice .eq. 1) then
              do j = 1, nbands
                index = ipat(j)
                abscoice(j) = cice * (absice1(1,index)                  &
     &                      + absice1(2,index)/radice)
              enddo
            elseif (iflagice .eq. 2) then
              factor = (radice - 10.0) / 3.0
              index = min(39, int(factor))
              fint = factor - index
              do j = 1, nbands
                abscoice(j) = cice * (absice2(index,j) + fint *         &
     &                    (absice2(index+1,j) - (absice2(index,j))))
              enddo
            endif

            do j = 1, nbands
              taucloud(j,k) = abscoice(j) + abscoliq(j)
            enddo

          endif  lab_if_liq

        endif  lab_if_cld

      enddo  lab_do_k

      return
!...................................
      end subroutine cldprop
!-----------------------------------



!-----------------------------------
      subroutine rtrn                                                   &
!...................................

!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,itr, l,        &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl             &
     &     )

!  *******************************************************************  !
!                                                                       !
!  rrtm longwave radiative transfer model                               !
!  atmospheric and environmental research, inc., cambridge, ma          !
!                                                                       !
!  original version:       e. j. mlawer, et al.                         !
!  revision for ncar ccm:  michael j. iacono; september, 1998           !
!                                                                       !
!  this program calculates the upward fluxes, downward fluxes, and      !
!  heating rates for an arbitrary clear or cloudy atmosphere.  the input!
!  to this program is the atmospheric profile, all planck function      !
!  information, and the cloud fraction by layer.  the diffusivity angle !
!  (secang=1.66) is used for the angle integration for consistency with !
!  the ncar ccm; the gaussian weight appropriate to this angle(wtnum=0.5)
!  is applied here.  note that use of the emissivity angle for the flux !
!  integration can cause errors of 1 to 4 w/m2 within cloudy layers.    !
!                                                                       !
!  *******************************************************************  !
!
      use machine,                  only : kind_rad
      use module_radlw_parameters,  only : nplnk, nbands, ngpt
      use module_radlw_avplank
!
      implicit none

!  ---  inputs:
      real (kind=kind_rad), dimension(0:l),intent(in) :: tz
      real (kind=kind_rad), dimension(l),  intent(in) :: tavel, delp,   &
     &      cldfrac
      real (kind=kind_rad), intent(in) :: taucloud(nbands,l),           &
     &      semiss(nbands), pfrac(ngpt,l)

      integer, intent(in) :: l, itr(ngpt,l)

!  ---  outputs:
      real (kind=kind_rad), dimension(0:l), intent(out) :: htr, htrcl,  &
     &      totuflux, totdflux, totuclfl, totdclfl

!  ---  locals:
      real (kind=kind_rad), dimension(ngpt,l)    :: gassrcu, cldsrcu,   &
     &      trans0
      real (kind=kind_rad), dimension(ngpt,0:l)  :: bglev
      real (kind=kind_rad), dimension(ngpt)      :: radclru, radclrd,   &
     &      radtotu, radtotd
      real (kind=kind_rad), dimension(nbands,0:l):: plvl
      real (kind=kind_rad), dimension(nbands,l)  :: play, odcld,        &
     &      trncld, efcfr1
      real (kind=kind_rad), dimension(0:l)       :: fnet, fnetc

      real (kind=kind_rad) :: totdrad, clrdrad, toturad, clrurad
      real (kind=kind_rad) :: delbgup, delbgdn, bglay, tau0, tauc,      &
     &      transc, cldsrcd, gassrcd, factot, odsm, tem1, tem2

      integer :: j, k, ind, inb, itm1, itm2, jtm1, jtm2

!  ====================  defination of variables  ===================  !
!                                                                      !
!  input variables:                                                    !
!    tavel   (l)         ! layer temperatures (k)                      !
!    tz      (0:l)       ! level (interface) temperature (k)           !
!    delp    (l)         ! layer pressure thickness (mb)               !
!    semiss  (nbands)    ! surface emissivities for each band          !
!    cldfrac (l)         ! layer cloud fraction                        !
!    taucloud(nbands,l)  ! layer cloud optical depth                   !
!    pfrac   (ngpt,l)    ! planck fractions                            !
!    itr     (ngpt,l)    ! integer look-up table index                 !
!    l                   ! number of model layers                      !
!                                                                      !
!  constants or shared variables:                                      !
!    ngpt                ! total number of g-point subintervals        !
!    nbands              ! number of longwave spectral bands           !
!    secang              ! diffusivity angle                           !
!    wtnum               ! weight for radiance to flux conversion      !
!    bpade               ! pade constant                               !
!    tau                 ! clear sky optical depth look-up table       !
!    tf                  ! tau transition function look-up table       !
!    trans               ! clear sky transmittance look-up table       !
!                                                                      !
!  output variables:                                                   !
!    totuflux(0:l)       ! upward longwave flux (w/m2)                 !
!    totdflux(0:l)       ! downward longwave flux (w/m2)               !
!    htr     (0:l)       ! longwave heating rate (k/day)               !
!    totuclfl(0:l)       ! clear sky upward longwave flux (w/m2)       !
!    totdclfl(0:l)       ! clear sky downward longwave flux (w/m2)     !
!    htrcl   (0:l)       ! clear sky longwave heating rate (k/day)     !
!                                                                      !
!  local variables:                                                    !
!    odcld   (nbands,l)  ! cloud optical depth                         !
!    trncld  (nbands,l)  ! cloud transmittance                         !
!    efcfr1  (nbands,l)  ! effective cloudy sky fraction               !
!    radtotu (ngpt)      ! upward radiance                             !
!    radtotd (ngpt)      ! downward radiance                           !
!    radclru (ngpt)      ! clear sky upward radiance                   !
!    radclrd (ngpt)      ! clear sky downward radiance                 !
!    toturad             ! spectrally summed upward radiance           !
!    totdrad             ! spectrally summed downward radiance         !
!    clrurad             ! spectrally summed clear sky upward radiance !
!    clrdrad             ! spectrally summed clear sky downward radiance
!                                                                      !
!    fnet    (0:l)       ! net longwave flux (w/m2)                    !
!    fnetc   (0:l)       ! clear sky net longwave flux (w/m2)          !
!                                                                      !
!  =====================    end of definitions    ===================  !

!
!===> ... begin here
!

!  --- ... calculate the integrated planck functions at the level and
!          layer temperatures.

      itm1 = min(nplnk, max(1, int(tz(0)-159.0) ))
      itm2 = min(nplnk, itm1+1)
      tem1 = tz(0) - int(tz(0))
      do j = 1, nbands
         plvl(j,0) = delwave(j) * ( totplnk(itm1,j)                     &
     &             + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
      enddo

      do k = 1, l
        itm1 = min(nplnk, max(1, int(tz(k)   -159.0) ))
        itm2 = min(nplnk, itm1+1)
        jtm1 = min(nplnk, max(1, int(tavel(k)-159.0) ))
        jtm2 = min(nplnk, jtm1+1)

        tem1 = tz(k)    - int(tz(k))
        tem2 = tavel(k) - int(tavel(k))

        do j = 1, nbands
          plvl(j,k) = delwave(j) * ( totplnk(itm1,j)                    &
     &              + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
          play(j,k) = delwave(j) * ( totplnk(jtm1,j)                    &
     &              + tem2 * (totplnk(jtm2,j) - totplnk(jtm1,j)) )

!  --- ... cloudy sky optical depth and absorptivity.
          odcld(j,k)  = secang * taucloud(j,k)
          trncld(j,k) = exp( -odcld(j,k) )
          efcfr1(j,k) = 1.0 - cldfrac(k) + trncld(j,k)*cldfrac(k)
        enddo

        do j = 1, ngpt
          inb = ngb(j)                 ! band index
          bglev(j,k-1) = pfrac(j,k) * plvl(inb,k-1)
        enddo
      enddo

!  --- ...  initialize for radiative transfer.

      do j = 1, ngpt
         inb = ngb(j)                 ! band index
         radclrd(j) = zero
         radtotd(j) = zero
         bglev(j,l) = pfrac(j,l) * plvl(inb,l)
      enddo

!===> ...  downward radiative transfer
!          totdrad holds summed radiance for total sky stream
!          clrdrad holds summed radiance for clear sky stream

      do k = l, 1, -1

        totdrad = zero
        clrdrad = zero

        if (cldfrac(k) .ge. eps) then
!  --- ... cloudy layer

          do j = 1, ngpt
!  --- ... get lookup table index
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ... get clear sky transmittance from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)
            transc      = trans0(j,k) * trncld(inb,k)

!  --- ... add clear sky and cloud optical depths
            odsm = tau(ind) + odcld(inb,k)
            tauc = odsm / (bpade + odsm)

            bglay = pfrac(j,k) * play(inb,k)
            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            tem2 = bglay + tauc*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
            cldsrcu(j,k) = tem2 - transc     *tem2

            delbgdn = bglev(j,k-1) - bglay
            tem1 = bglay + tau0*delbgdn
            tem2 = bglay + tauc*delbgdn
            gassrcd = tem1 - trans0(j,k)*tem1
            cldsrcd = tem2 - transc     *tem2

!  --- ... total sky radiance
            radtotd(j) = radtotd(j)*trans0(j,k)*efcfr1(inb,k)           &
     &                 + gassrcd + cldfrac(k)*(cldsrcd - gassrcd)
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)
          enddo

        else

!  --- ... clear layer

          do j = 1, ngpt
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ... get clear sky transmittance from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)

            bglay = pfrac(j,k) * play(inb,k)

            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
!           cldsrcu(j,k) = 0.0

            delbgdn  = bglev(j,k-1) - bglay
            tem2 = bglay + tau0*delbgdn
            gassrcd = tem2 - trans0(j,k)*tem2

!  --- ... total sky radiance
            radtotd(j) = radtotd(j)*trans0(j,k) + gassrcd
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)

          enddo

        endif

        totdflux(k-1) = totdrad
        totdclfl(k-1) = clrdrad

      enddo

      totdflux(l) = zero
      totdclfl(l) = zero

!  --- ...  spectral emissivity & reflectance
!           include the contribution of spectrally varying longwave
!           emissivity and reflection from the surface to the upward
!           radiative transfer.
!    note: spectral and lambertian reflection are identical for the one
!           angle flux integration used here.

      toturad = zero
      clrurad = zero

      do j = 1, ngpt
        inb = ngb(j)                 ! band index
        tem1 = 1.0 - semiss(inb)
        tem2 = bglev(j,0) * semiss(inb)

!  --- ... total sky radiance
        radtotu(j) = tem2 + tem1 * radtotd(j)
        toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
        radclru(j) = tem2 + tem1 * radclrd(j)
        clrurad = clrurad + radclru(j)
      enddo

      totuflux(0) = toturad
      totuclfl(0) = clrurad

!     print *,' toturad(0)=',totuflux(0)
!     print *,' clrurad(0)=',totuclfl(0)

!===> ...  upward radiative transfer
!          toturad holds summed radiance for total sky stream
!          clrurad holds summed radiance for clear sky stream

      do k = 1, l

        toturad = zero
        clrurad = zero

!  --- ... check flag for cloud in current layer
        if (cldfrac(k) .ge. eps) then

!  --- ... cloudy layers

          do j = 1, ngpt
            inb   = ngb(j)                 ! band index

!  --- ... total sky radiance
            radtotu(j) = radtotu(j)*trans0(j,k)*efcfr1(inb,k)           &
     &         + gassrcu(j,k) + cldfrac(k)*(cldsrcu(j,k)-gassrcu(j,k))
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)
          enddo

        else

!  --- ... clear layer

          do j = 1, ngpt

!  --- ... total sky radiance
            radtotu(j) = radtotu(j)*trans0(j,k) + gassrcu(j,k)
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)
          enddo

        endif

        totuflux(k) = toturad
        totuclfl(k) = clrurad

      enddo

!===> ...  convert radiances to fluxes and heating rates for total sky.
!          calculates clear sky surface and toa values.  to compute clear
!          sky profiles, uncomment relevant lines below.

      factot = wtnum * fluxfac
      do k = 0, l
        totuflux(k) = totuflux(k) * factot
        totdflux(k) = totdflux(k) * factot

        totuclfl(k) = totuclfl(k) * factot
        totdclfl(k) = totdclfl(k) * factot

        fnet(k) = totuflux(k) - totdflux(k)
        fnetc(k) = totuclfl(k) - totdclfl(k)
      enddo

!  --- ...  calculate heating rates.
      do k = 1, l
         j = k - 1
         htr  (j) = heatfac * (fnet (j) - fnet (k)) / delp(k)
         htrcl(j) = heatfac * (fnetc(j) - fnetc(k)) / delp(k)
      enddo

      htr  (l) = zero
      htrcl(l) = zero

      return
!...................................
      end subroutine rtrn
!-----------------------------------



!-----------------------------------
      subroutine rtrnmr                                                 &
!...................................

!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,itr, l,        &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl             &
     &     )

!  *******************************************************************  !
!                                                                       !
!  rrtm longwave radiative transfer model                               !
!  atmospheric and environmental research, inc., cambridge, ma          !
!                                                                       !
!  original version:       e. j. mlawer, et al.                         !
!  revision for ncar ccm:  michael j. iacono; september, 1998           !
!                                                                       !
!  this program calculates the upward fluxes, downward fluxes, and      !
!  heating rates for an arbitrary clear or cloudy atmosphere.  the input!
!  to this program is the atmospheric profile, all planck function      !
!  information, and the cloud fraction by layer.  the diffusivity angle !
!  (secang=1.66) is used for the angle integration for consistency with !
!  the ncar ccm; the gaussian weight appropriate to this angle(wtnum=0.5)
!  is applied here.  note that use of the emissivity angle for the flux !
!  integration can cause errors of 1 to 4 w/m2 within cloudy layers.    !
!  this routine computes a generalized maximum/random cloud overlap.    !
!  adjacent cloud layers are treated with maximum overlap in which up   !
!  to two previous layers of cloud information is considered.  non-     !
!  adjacent groups of clouds are treated with random overlap.           !
!                                                                       !
!  *******************************************************************  !
!
      use machine,                  only : kind_rad
      use module_radlw_parameters,  only : nplnk, nbands, ngpt
      use module_radlw_avplank
!
      implicit none

!  ---  inputs:
      real (kind=kind_rad), dimension(0:l),intent(in) :: tz
      real (kind=kind_rad), dimension(l),  intent(in) :: tavel, delp,   &
     &      cldfrac
      real (kind=kind_rad), intent(in) :: taucloud(nbands,l),           &
     &      semiss(nbands), pfrac(ngpt,l)

      integer, intent(in) :: l, itr(ngpt,l)

!  ---  outputs:
      real (kind=kind_rad), dimension(0:l), intent(out) :: htr, htrcl,  &
     &      totuflux, totdflux, totuclfl, totdclfl

!  ---  locals:
!  dimensions for radiative transfer
      real (kind=kind_rad), dimension(ngpt,l)    :: gassrcu, cldsrcu,   &
     &      trans0, transc
      real (kind=kind_rad), dimension(ngpt,0:l)  :: bglev
      real (kind=kind_rad), dimension(ngpt)      :: radclru, radclrd,   &
     &      radtotu, radtotd
      real (kind=kind_rad), dimension(nbands,0:l):: plvl
      real (kind=kind_rad), dimension(nbands,l)  :: play, odcld, trncld
      real (kind=kind_rad), dimension(0:l)       :: fnet, fnetc

      real (kind=kind_rad) :: totdrad, clrdrad, toturad, clrurad
      real (kind=kind_rad) :: delbgup, delbgdn, bglay, tau0, tauc,      &
     &      cldsrcd, gassrcd, factot, odsm, tem1, tem2

      integer :: j, k, ind, inb, itm1, itm2, jtm1, jtm2

!  dimensions for cloud overlap adjustment
      real (kind=kind_rad), dimension(ngpt)  ::  clrradu, cldradu,      &
     &      clrradd, cldradd, rad
      real (kind=kind_rad), dimension(l+1)   ::  faccld1u, faccld2u,    &
     &      facclr1u, facclr2u, faccmb1u, faccmb2u
      real (kind=kind_rad), dimension(0:l)   ::  faccld1d, faccld2d,    &
     &      facclr1d, facclr2d, faccmb1d, faccmb2d
      real (kind=kind_rad) :: fmax, fmin, rat1, rat2, radmod, cldsrc

      logical :: istcldu(l), istcldd(l)

!  ====================  defination of variables  ===================  !
!                                                                      !
!  input variables:                                                    !
!    tavel   (l)         ! layer temperatures (k)                      !
!    tz      (0:l)       ! level (interface) temperatures (k)          !
!    delp    (l)         ! layer pressure thickness (mb)               !
!    semiss  (nbands)    ! surface emissivities for each band          !
!    cldfrac (l)         ! layer cloud fraction                        !
!    taucloud(nbands,l)  ! layer cloud optical depth                   !
!    pfrac   (ngpt,l)    ! planck fractions                            !
!    itr     (ngpt,l)    ! integer look-up table index                 !
!    l                   ! number of model layers                      !
!                                                                      !
!  constants or shared variables:                                      !
!    ngpt                ! total number of g-point subintervals        !
!    nbands              ! number of longwave spectral bands           !
!    secang              ! diffusivity angle                           !
!    wtnum               ! weight for radiance to flux conversion      !
!    bpade               ! pade constant                               !
!    tau                 ! clear sky optical depth look-up table       !
!    tf                  ! tau transition function look-up table       !
!    trans               ! clear sky transmittance look-up table       !
!                                                                      !
!  output variables:                                                   !
!    totuflux(0:l)       ! upward longwave flux (w/m2)                 !
!    totdflux(0:l)       ! downward longwave flux (w/m2)               !
!    htr     (0:l)       ! longwave heating rate (k/day)               !
!    totuclfl(0:l)       ! clear sky upward longwave flux (w/m2)       !
!    totdclfl(0:l)       ! clear sky downward longwave flux (w/m2)     !
!    htrcl   (0:l)       ! clear sky longwave heating rate (k/day)     !
!                                                                      !
!  local variables:                                                    !
!    odcld   (nbands,l)  ! cloud optical depth                         !
!    trncld  (nbands,l)  ! cloud transmittance                         !
!    radtotu (ngpt)      ! upward radiance                             !
!    radtotd (ngpt)      ! downward radiance                           !
!    radclru (ngpt)      ! clear sky upward radiance                   !
!    radclrd (ngpt)      ! clear sky downward radiance                 !
!    toturad             ! spectrally summed upward radiance           !
!    totdrad             ! spectrally summed downward radiance         !
!    clrurad             ! spectrally summed clear sky upward radiance !
!    clrdrad             ! spectrally summed clear sky downward radiance
!                                                                      !
!    fnet    (0:l)       ! net longwave flux (w/m2)                    !
!    fnetc   (0:l)       ! clear sky net longwave flux (w/m2)          !
!                                                                      !
!  =====================    end of definitions    ===================  !
!

!
!===> ... begin here
!
      do k = 1, l+1
        faccld1u(k) = zero
        faccld2u(k) = zero
        facclr1u(k) = zero
        facclr2u(k) = zero
        faccmb1u(k) = zero
        faccmb2u(k) = zero
      enddo

      istcldu(1) = cldfrac(1) .ge. eps
      rat1 = zero
      rat2 = zero

      do k = 1, l-1

        istcldu(k+1) = cldfrac(k+1).ge.eps  .and. cldfrac(k).lt.eps

        if (cldfrac(k) .ge. eps) then
!  --- ... maximum/random cloud overlap

          if (cldfrac(k+1) .ge. cldfrac(k)) then
            if (istcldu(k)) then
              if (cldfrac(k) .lt. 1.0) then
                facclr2u(k+1) = (cldfrac(k+1) - cldfrac(k))             &
     &                        / (1.0 - cldfrac(k))
              endif
              facclr2u(k) = zero
              faccld2u(k) = zero
            else
              fmax = max(cldfrac(k), cldfrac(k-1))
              if (cldfrac(k+1) .gt. fmax) then
                facclr1u(k+1) = rat2
                facclr2u(k+1) = (cldfrac(k+1) - fmax)/(1.0 - fmax)
              elseif (cldfrac(k+1) .lt. fmax) then
                facclr1u(k+1) = (cldfrac(k+1) - cldfrac(k))             &
     &                        / (cldfrac(k-1) - cldfrac(k))
              else
                facclr1u(k+1) = rat2
              endif
            endif

            if (facclr1u(k+1).gt.zero .or. facclr2u(k+1).gt.zero) then
              rat1 = 1.0
              rat2 = zero
            else
              rat1 = zero
              rat2 = zero
            endif
          else
            if (istcldu(k)) then
              faccld2u(k+1) = (cldfrac(k) - cldfrac(k+1)) /  cldfrac(k)
              facclr2u(k) = zero
              faccld2u(k) = zero
            else
              fmin = min(cldfrac(k), cldfrac(k-1))
              if (cldfrac(k+1) .le. fmin) then
                faccld1u(k+1) = rat1
                faccld2u(k+1) = (fmin - cldfrac(k+1)) / fmin
              else
                faccld1u(k+1) = (cldfrac(k) - cldfrac(k+1))             &
     &                        / (cldfrac(k) - fmin)
              endif
            endif

            if (faccld1u(k+1).gt.zero .or. faccld2u(k+1).gt.zero) then
              rat1 = zero
              rat2 = 1.0
            else
              rat1 = zero
              rat2 = zero
            endif
          endif

          if (k .eq. 1) then
            faccmb1u(k+1) = facclr1u(k+1) * faccld2u(k)
            faccmb2u(k+1) = faccld1u(k+1) * facclr2u(k)
          else
            faccmb1u(k+1) = facclr1u(k+1) * faccld2u(k) * cldfrac(k-1)
            faccmb2u(k+1) = faccld1u(k+1) * facclr2u(k)                 &
     &                    * (1.0 - cldfrac(k-1))
          endif

        endif

      enddo

      do k = 0, l
        faccld1d(k) = zero
        faccld2d(k) = zero
        facclr1d(k) = zero
        facclr2d(k) = zero
        faccmb1d(k) = zero
        faccmb2d(k) = zero
      enddo

      istcldd(l) = cldfrac(l) .ge. eps
      rat1 = zero
      rat2 = zero

      do k = l, 2, -1

        istcldd(k-1) = cldfrac(k-1).ge.eps .and. cldfrac(k).lt.eps

        if (cldfrac(k) .ge. eps) then

          if (cldfrac(k-1) .ge. cldfrac(k)) then
            if (istcldd(k)) then
              if (cldfrac(k) .lt. 1.0) then
                facclr2d(k-1) = (cldfrac(k-1) - cldfrac(k))             &
     &                        / (1.0 - cldfrac(k))
              endif
              facclr2d(k) = zero
              faccld2d(k) = zero
            else
              fmax = max(cldfrac(k), cldfrac(k+1))

              if (cldfrac(k-1) .gt. fmax) then
                facclr1d(k-1) = rat2
                facclr2d(k-1) = (cldfrac(k-1) - fmax)/(1.0 - fmax)
              elseif (cldfrac(k-1) .lt. fmax) then
                facclr1d(k-1) = (cldfrac(k-1) - cldfrac(k))             &
     &                        / (cldfrac(k+1) - cldfrac(k))
              else
                facclr1d(k-1) = rat2
              endif
            endif

            if (facclr1d(k-1).gt.zero .or. facclr2d(k-1).gt.zero) then
              rat1 = 1.0
              rat2 = zero
            else
              rat1 = zero
              rat2 = zero
            endif
          else
            if (istcldd(k)) then
              faccld2d(k-1) = (cldfrac(k) - cldfrac(k-1)) / cldfrac(k)
              facclr2d(k) = zero
              faccld2d(k) = zero
            else
              fmin = min(cldfrac(k), cldfrac(k+1))

              if (cldfrac(k-1) .le. fmin) then
                faccld1d(k-1) = rat1
                faccld2d(k-1) = (fmin - cldfrac(k-1)) / fmin
              else
                faccld1d(k-1) = (cldfrac(k) - cldfrac(k-1))             &
     &                        / (cldfrac(k) - fmin)
              endif
            endif

            if (faccld1d(k-1).gt.zero .or. faccld2d(k-1).gt.zero) then
              rat1 = zero
              rat2 = 1.0
            else
              rat1 = zero
              rat2 = zero
            endif
          endif

          faccmb1d(k-1) = facclr1d(k-1) * faccld2d(k) * cldfrac(k+1)
          faccmb2d(k-1) = faccld1d(k-1) * facclr2d(k)                   &
     &                  * (1.0 - cldfrac(k+1))

        endif

      enddo

!  --- ... calculate the integrated planck functions at the level and
!          layer temperatures.

      itm1 = min(nplnk, max(1, int(tz(0)-159.0) ))
      itm2 = min(nplnk, itm1+1)
      tem1 = tz(0) - int(tz(0))
      do j = 1, nbands
        plvl(j,0) = delwave(j) * ( totplnk(itm1,j)                      &
     &            + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
      enddo

      do k = 1, l
        itm1 = min(nplnk, max(1, int(tz(k)   -159.0) ))
        itm2 = min(nplnk, itm1+1)
        jtm1 = min(nplnk, max(1, int(tavel(k)-159.0) ))
        jtm2 = min(nplnk, jtm1+1)

        tem1 = tz(k)    - int(tz(k))
        tem2 = tavel(k) - int(tavel(k))

        do j = 1, nbands
          plvl(j,k) = delwave(j) * ( totplnk(itm1,j)                    &
     &              + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
          play(j,k) = delwave(j) * ( totplnk(jtm1,j)                    &
     &              + tem2 * (totplnk(jtm2,j) - totplnk(jtm1,j)) )

!  --- ... cloudy sky optical depth and absorptivity.
          odcld(j,k)  = secang * taucloud(j,k)
          trncld(j,k) = exp( -odcld(j,k) )
        enddo

        do j = 1, ngpt
          inb = ngb(j)                 ! band index
          bglev(j,k-1) = pfrac(j,k) * plvl(inb,k-1)
        enddo
      enddo

!  --- ...  initialize for radiative transfer.

      do j = 1, ngpt
        inb = ngb(j)                 ! band index
        radclrd(j) = zero
        radtotd(j) = zero
        bglev (j,l) = pfrac(j,l) * plvl(inb,l)
      enddo

!===> ...  downward radiative transfer
!          totdrad holds summed radiance for total sky stream
!          clrdrad holds summed radiance for clear sky stream

      do k = l, 1, -1

        totdrad = zero
        clrdrad = zero

        if (istcldd(k)) then
          do j = 1, ngpt
            cldradd(j) = cldfrac(k) * radtotd(j)
            clrradd(j) = radtotd(j) - cldradd(j)
            rad    (j) = zero
          enddo
        endif

        if (cldfrac(k) .ge. eps) then
!  --- ... cloudy layer

          do j = 1, ngpt

!  --- ... get lookup table index
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ...  get tf from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)
            transc(j,k) = trans(ind) * trncld(inb,k)

!  --- ... add clear sky and cloud optical depths
            odsm = tau(ind) + odcld(inb,k)
            tauc = odsm / (bpade + odsm)

            bglay = pfrac(j,k) * play(inb,k)
            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            tem2 = bglay + tauc*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
            cldsrcu(j,k) = tem2 - transc(j,k)*tem2

            delbgdn = bglev(j,k-1) - bglay
            tem1 = bglay + tau0*delbgdn
            tem2 = bglay + tauc*delbgdn
            gassrcd = tem1 - trans0(j,k)*tem1
            cldsrcd = tem2 - transc(j,k)*tem2

            cldradd(j) = cldradd(j)*transc(j,k) + cldfrac(k)*cldsrcd
            clrradd(j) = clrradd(j)*trans0(j,k)                         &
     &                                    + (1.0-cldfrac(k))*gassrcd

!  --- ... total sky radiance
            radtotd(j) = cldradd(j) + clrradd(j)
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)

            radmod = rad(j)                                             &
     &        * (facclr1d(k-1)*trans0(j,k) + faccld1d(k-1)*transc(j,k)) &
     &         - faccmb1d(k-1)*gassrcd + faccmb2d(k-1)*cldsrcd

            rad(j) = -radmod + facclr2d(k-1)*(clrradd(j) + radmod)      &
     &                       - faccld2d(k-1)*(cldradd(j) - radmod)
            cldradd(j) = cldradd(j) + rad(j)
            clrradd(j) = clrradd(j) - rad(j)

          enddo

        else

!  --- ... clear layer

          do j = 1, ngpt
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ... get tf from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)
            transc(j,k) = zero

            bglay = pfrac(j,k) * play(inb,k)

            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
!           cldsrcu(j,k) = 0.0

            delbgdn  = bglev(j,k-1) - bglay
            tem2 = bglay + tau0*delbgdn
            gassrcd = tem2 - trans0(j,k)*tem2

!  --- ... total sky radiance
            radtotd(j) = radtotd(j)*trans0(j,k) + gassrcd
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)

          enddo

        endif

        totdflux(k-1) = totdrad
        totdclfl(k-1) = clrdrad

      enddo

      totdflux(l) = zero
      totdclfl(l) = zero

!  --- ...  spectral emissivity & reflectance
!           include the contribution of spectrally varying longwave
!           emissivity and reflection from the surface to the upward
!           radiative transfer.
!    note: spectral and lambertian reflection are identical for the one
!           angle flux integration used here.

      toturad = zero
      clrurad = zero

      do j = 1, ngpt
        inb = ngb(j)                 ! band index
        tem1 = 1.0 - semiss(inb)
        tem2 = bglev(j,0) * semiss(inb)

!  --- ... total sky radiance
        radtotu(j) = tem2 + tem1 * radtotd(j)
        toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
        radclru(j) = tem2 + tem1 * radclrd(j)
        clrurad = clrurad + radclru(j)
      enddo

      totuflux(0) = toturad
      totuclfl(0) = clrurad

!===> ...   upward radiative transfer
!           toturad holds the summed radiance for total sky stream
!           clrurad holds the summed radiance for clear sky stream

      do k = 1, l
        toturad = zero
        clrurad = zero

        if (istcldu(k)) then
          do j = 1, ngpt
            cldradu(j) = radtotu(j) * cldfrac(k)
            clrradu(j) = radtotu(j) - cldradu(j)
            rad(j)     = zero
          enddo
        endif

!  --- ... check flag for cloud in current layer
        if (cldfrac(k) .ge. eps) then

!  --- ... cloudy layers

          do j = 1, ngpt
            cldradu(j) = cldradu(j)*transc(j,k)+cldfrac(k)*cldsrcu(j,k)
            clrradu(j) = clrradu(j)*trans0(j,k)                         &
     &                                + (1.0 - cldfrac(k))*gassrcu(j,k)

!  --- ... total sky radiance
            radtotu(j) = cldradu(j) + clrradu(j)
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)

            radmod = rad(j)                                             &
     &        * (facclr1u(k+1)*trans0(j,k) + faccld1u(k+1)*transc(j,k)) &
     &        - faccmb1u(k+1)*gassrcu(j,k) + faccmb2u(k+1)*cldsrcu(j,k)

            rad(j) = -radmod + facclr2u(k+1)*(clrradu(j) + radmod)      &
     &                       - faccld2u(k+1)*(cldradu(j) - radmod)
            cldradu(j) = cldradu(j) + rad(j)
            clrradu(j) = clrradu(j) - rad(j)
          enddo

        else

!  --- ... clear layer

          do j = 1, ngpt

!  --- ... total sky radiance
            radtotu(j) = radtotu(j)*trans0(j,k) + gassrcu(j,k)
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
!          upward clear and total sky streams must remain separate
!          because surface reflectance is different for each.
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)
          enddo

        endif

        totuflux(k) = toturad
        totuclfl(k) = clrurad

      enddo


!===> ...  convert radiances to fluxes and heating rates for total sky.
!          calculates clear sky surface and toa values. to compute clear
!          sky profiles, uncomment relevant lines below.

      factot = fluxfac * wtnum
      do k = 0, l
         totuflux(k) = totuflux(k) * factot
         totdflux(k) = totdflux(k) * factot
         totuclfl(k) = totuclfl(k) * factot
         totdclfl(k) = totdclfl(k) * factot

         fnet(k) = totuflux(k) - totdflux(k)
         fnetc(k) = totuclfl(k) - totdclfl(k)

      enddo

!  --- ... calculate heating rates.
      do k = 1, l
         j = k - 1
         htr  (j) = heatfac * (fnet (j) - fnet (k)) / delp(k)
         htrcl(j) = heatfac * (fnetc(j) - fnetc(k)) / delp(k)
      enddo

      htr  (l) = zero
      htrcl(l) = zero

      return
!...................................
      end subroutine rtrnmr
!-----------------------------------




!-----------------------------------
      subroutine taumol                                                 &
!...................................
!  ---  inputs:
     &     ( laytrop,layswtch,laylow,h2ovmr,colamt,wx,co2mult,          &
     &       fac00,fac01,fac10,fac11,jp,jt,jt1,selffac,selffrac,        &
     &       indself,forfac, l,                                         &
!  ---  outputs:
     &       itr, pfrac                                                 &
     &     )

!  ************    original subprogram description    ***************  *
!                                                                      *
!                  optical depths developed for the                    *
!                                                                      *
!                rapid radiative transfer model (rrtm)                 *
!                                                                      *
!            atmospheric and environmental research, inc.              *
!                        840 memorial drive                            *
!                        cambridge, ma 02139                           *
!                                                                      *
!                           eli j. mlawer                              *
!                         steven j. taubman                            *
!                                                                      *
!                       email:  mlawer@aer.com                         *
!                                                                      *
!        the authors wish to acknowledge the contributions of the      *
!        following people:  patrick d. brown, michael j. iacono,       *
!        ronald e. farren, luke chen, robert bergstrom.                *
!                                                                      *
!  revision for ncar ccm:  michael j. iacono; september, 1998          *
!                                                                      *
!     taumol                                                           *
!                                                                      *
!     this file contains the subroutines taugbn (where n goes from     *
!     1 to 16).  taugbn calculates the optical depths and planck       *
!     fractions per g-value and layer for band n.                      *
!                                                                      *
!  output:  transmittance look-up table index (unitless)               *
!           fractions needed to compute planck functions at every layer*
!           and g-value                                                *
!                                                                      !
!  description:                                                        !
!     ngxx        - number of g-values in band xx (xx=01-16)           !
!     nspa(iband) - for the lower atmosphere, the number of reference  !
!                   atmospheres that are stored for band iband per     !
!                   pressure level and temperature.  each of these     !
!                   atmospheres has different relative amounts of the  !
!                   key species for the band (i.e. different binary    !
!                   species parameters).                               !
!     nspb(iband) - same for upper atmosphere                          !
!     oneminus    - since problems are caused in some cases by         !
!                   interpolation parameters equal to or greater than  !
!                   1, for these cases these parameters are set to this!
!                   value, slightly < 1.                               !
!     laytrop, layswtch,laylow                                         !
!                 - layer at which switch is made from one combination !
!                   of key species to another                          !
!     h2ovmr(l)   - layer h2o volume mixing ratio (vmr)                !
!     colamt(l,maxgas)                                                 !
!                 - column amounts of water vapor,carbon dioxide,      !
!                   ozone, nitrous oxide, methane, o2, respectively    !
!                   (molecules/cm**2)                                  !
!     co2mult(l)  - for bands in which carbon dioxide is implemented   !
!                   as a trace species, this is the factor used to     !
!                   multiply the band average co2 absorption           !
!                   coefficient to get the added contribution to the   !
!                   optical depth relative to 355 ppm.                 !
!     facij (l)   - for layer lay, these are factors that are needed to!
!                   compute the interpolation factors that multiply the!
!                   appropriate reference k-values.  a value of 0 (1)  !
!                   for i,j indicates that the corresponding factor    !
!                   multiplies reference k-value for the lower (higher)!
!                   of the two appropriate temperatures, and altitudes,!
!                   respectively.                                      !
!     jp (l)      - the index of the lower (in altitude) of the two    !
!                   appropriate reference pressure levels needed for   !
!                   interpolation.                                     !
!     jt, jt1 (l) - the indices of the lower of the two appropriate    !
!                   reference temperatures needed for interpolation    !
!                   (for pressure levels jp and jp+1, respectively)    !
!     selffac(l)  - scale factor needed to water vapor self-continuum, !
!                   equals (water vapor density)/(atmospheric density  !
!                   at 296k and 1013 mb)                               !
!     selffrac(l) - factor needed for temperature interpolation of     !
!                   reference water vapor self-continuum data          !
!     indself(l)  - index of the lower of the two appropriate reference!
!                   temperatures needed for the self-continuum         !
!                   interpolation                                      !
!                                                                      !
!  data input                                                          !
!     absa(nspa(nn),5,13,ngnn), absb(nspb(nn),5,13:59,ngnn),           !
!     selfref(10,ngnn)                                                 !
!                      (note:  nn is the band number)                  !
!                                                                      !
!     absa    - k-values for low reference atmospheres (no water vapor !
!               self-continuum) (units: cm**2/molecule)                !
!     absb    - k-values for high reference atmospheres (all sources)  !
!               (units: cm**2/molecule)                                !
!     selfref - k-values for water vapor self-continuum for reference  !
!               atmospheres (used below laytrop)                       !
!               (units: cm**2/molecule)                                !
!                                                                      !
!  ******************************************************************  !
!
      use machine,                 only : kind_rad
      use module_radlw_parameters
!
      implicit none
!
!  ---  inputs:
      real (kind=kind_rad), dimension(l), intent(in) ::   h2ovmr,       &
     &      co2mult, fac00, fac01, fac10, fac11, selffac, selffrac,     &
     &      forfac
      real (kind=kind_rad),intent(in) :: colamt(l,maxgas),wx(l,maxxsec)

      integer, dimension(l), intent(in) :: jp, jt, jt1, indself
      integer, intent(in) :: laytrop, layswtch, laylow, l

!  ---  outputs:
      real (kind=kind_rad),dimension(ngpt,l), intent(out) :: pfrac
      integer,             dimension(ngpt,l), intent(out) :: itr

!  ---  locals:
      real (kind=kind_rad) :: taug(ngpt,l), tem1, tem2
      integer :: j, k, ja, jb, kk, id0(l,nbands), id1(l,nbands)
!
!===> ... begin here
!
      do j = 1, nbands
        ja = 13
        jb = 12
        kk = laytrop
        if (j .eq. 8) then
          ja = 7
          jb = 6
          kk = layswtch
        endif

        do k = 1, kk
          id0(k,j) = ((jp(k)-1) *5 + jt (k) - 1) * nspa(j)
          id1(k,j) = ( jp(k)    *5 + jt1(k) - 1) * nspa(j)
        enddo
        do k = kk+1, l
          id0(k,j) = ((jp(k)-ja)*5 + jt (k) - 1) * nspb(j)
          id1(k,j) = ((jp(k)-jb)*5 + jt1(k) - 1) * nspb(j)
        enddo
      enddo

      call taugb01
      call taugb02
      call taugb03
      call taugb04
      call taugb05
      call taugb06
      call taugb07
      call taugb08
      call taugb09
      call taugb10
      call taugb11
      call taugb12
      call taugb13
      call taugb14
      call taugb15
      call taugb16

      do k = 1, l
        do j = 1, ngpt
          tem1 = max(zero, secang*taug(j,k) )
          tem2 = tem1 / (bpade + tem1)
! hmhj    itr(j,k) = max(0, min(N5000, int(5.0e3*tem2+0.5) ))
          itr(j,k) = max(1, min(N5000, int(5.0e3*tem2+0.5) ))
        enddo
      enddo


! =================
      contains
! =================

!-----------------------------------
      subroutine taugb01
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!     revised by michael j.iacono, atmospheric & environmental research!
!                                                                      !
!     band 1:  10-250 cm-1 (low - h2o; high - h2o)                     !
!                                                                      !
!     compute the optical depth by interpolating in ln(pressure) and   !
!     temperature.  below laytrop, the water vapor self-continuum      !
!     is interpolated (in temperature) separately.                     !
!  ------------------------------------------------------------------  !
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng01
      use module_radlw_kgb01
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds

      do k = 1, laytrop
        ind01 = id0(k,1) + 1
        ind02 = ind01 + 1
        ind11 = min(msa01, id1(k,1) + 1 )
        ind12 = min(msa01, ind11 + 1)
        inds = indself(k)

        do j = 1, ng01
          taug(j,k) = colamt(k,1)                                       &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k)*( selfref(inds,j) + selffrac(k) *         &
     &            (selfref(inds+1,j) - selfref(inds,j)) ) +             &
     &             forfac(k)*forref(j) )

          pfrac(j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, l
        ind01 = id0(k,1) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,1) + 1
        ind12 = ind11 + 1

        do j = 1, ng01
          taug(j,k) = colamt(k,1)                                       &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) +    &
     &             forfac(k) * forref(j) )

          pfrac(j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb01
!-----------------------------------


!-----------------------------------
      subroutine taugb02
!...................................

!     band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng02, ns02
      use module_radlw_kgb02
!
      implicit none
!
      integer :: i, j, k, ind01, ind02, ind11, ind12, inds, ifrac, ifp
!
      real (kind=kind_rad) :: fc00, fc01, fc10, fc11, h2oparam,         &
     &      fracint, one
      data one / 1.0 /

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  below laytrop, the water vapor self-continuum is 
!     interpolated (in temperature) separately.

      do k = 1, laytrop
        h2oparam = h2ovmr(k) / (h2ovmr(k) + 0.002)

        ifrac = 13
!!changed b_do_ifrac : do i = 2, 12
        lab_do_ifrac : do i = 2, 13
          if (h2oparam .ge. refparam(i)) then
            ifrac = i
            exit lab_do_ifrac
          endif
        enddo lab_do_ifrac

        fracint = max(-one, min(one, (h2oparam - refparam(ifrac))       &
     &          / (refparam(ifrac-1) - refparam(ifrac)) ))

        ifp = max( 0, int(2.e2*(fac11(k)+fac01(k))+0.5) )
        fc00 = fac00(k) * corr2(ifp)
        fc10 = fac10(k) * corr2(ifp)
        fc01 = fac01(k) * corr1(ifp)
        fc11 = fac11(k) * corr1(ifp)

        ind01 = id0(k,2) + 1
        ind02 = ind01 + 1
        ind11 = min(msa02, id1(k,2) + 1 )
        ind12 = min(msa02, ind11 + 1 )
        inds = indself(k)

        do j = 1, ng02
          taug(ns02+j,k) = colamt(k,1)                                  &
     &         * ( fc00*absa(ind01,j) + fc10*absa(ind02,j) +            &
     &             fc01*absa(ind11,j) + fc11*absa(ind12,j) +            &
     &             selffac(k)*(selfref(inds,j) + selffrac(k) *          &
     &            ( selfref(inds+1,j) - selfref(inds,j)) ) +            &
     &             forfac(k) * forref(j) )

          pfrac(ns02+j,k) = fracrefa(j,ifrac) + fracint                 &
     &         * (fracrefa(j,ifrac-1) - fracrefa(j,ifrac))
        enddo
      enddo

      do k = laytrop+1, l
        ifp = max( 0, int(2.e2*(fac11(k)+fac01(k))+0.5) )
        fc00 = fac00(k) * corr2(ifp)
        fc10 = fac10(k) * corr2(ifp)
        fc01 = fac01(k) * corr1(ifp)
        fc11 = fac11(k) * corr1(ifp)

        ind01 = id0(k,2) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,2) + 1
        ind12 = ind11 + 1

        do j = 1, ng02
          taug(ns02+j,k) = colamt(k,1)                                  &
     &         * ( fc00*absb(ind01,j) + fc10*absb(ind02,j) +            &
     &             fc01*absb(ind11,j) + fc11*absb(ind12,j) +            &
     &             forfac(k) * forref(j) )

          pfrac(ns02+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb02
!-----------------------------------


!-----------------------------------
      subroutine taugb03
!...................................

!     band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng03, ns03
      use module_radlw_kgb03
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns, jp0, jp1
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult,         &
     &      fs, fs1, fp, ratio, n2omult, tem0, tem1, tem2

      strrat = 1.19268

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)
        if (js .eq. 8) then
          if (fs .ge. 0.9) then
            js = 9
            fs = 10.0 * (fs - 0.9)
          else
            fs = fs / 0.9
          endif
        endif

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,3) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 10
        ind04 = ind01 + 11
        ind11 = min(msa03, id1(k,3) + js )
        ind12 = min(msa03, ind11 + 1 )
        ind13 = min(msa03, ind11 + 10)
        ind14 = min(msa03, ind11 + 11)
        inds = indself(k)

        jp0 = jp(k)
        jp1 = jp0 + 1
        ns = js + int(fs + 0.5)
        if (ns .eq. 10) then
          tem1 = n2oref(jp0) / h2oref(jp0)
          tem2 = n2oref(jp1) / h2oref(jp1)
        else
          tem0 = (1.0 - etaref(ns)) / strrat
          tem1 = tem0 * n2oref(jp0) / co2ref(jp0)
          tem2 = tem0 * n2oref(jp1) / co2ref(jp1)
        endif
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - speccomb*ratio

        do j = 1, ng03
          taug(ns03+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * ( selffac(k)*(selfref(inds,j) +          &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) ) +  &
     &             forfac(k) * forref(j) )                              &
     &         + n2omult * absn2oa(j)

          pfrac(ns03+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        speccomb = colamt(k,1) + strrat*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,3) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,3) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6

        jp0 = jp(k)
        jp1 = jp0 + 1
        ns = js + int(fs + 0.5)
        if (ns .eq. 5) then
          tem1 = n2oref(jp0) / h2oref(jp0)
          tem2 = n2oref(jp1) / h2oref(jp1)
        else
          tem0 = (1.0 - etaref(ns)) / strrat
          tem1 = tem0 * n2oref(jp0) / co2ref(jp0)
          tem2 = tem0 * n2oref(jp1) / co2ref(jp1)
        endif
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - speccomb * ratio

        do j = 1, ng03
          taug(ns03+j,k) = speccomb                                     &
     &         * ( fac000*absb(ind01,j) + fac100*absb(ind02,j) +        &
     &             fac010*absb(ind03,j) + fac110*absb(ind04,j) +        &
     &             fac001*absb(ind11,j) + fac101*absb(ind12,j) +        &
     &             fac011*absb(ind13,j) + fac111*absb(ind14,j) )        &
     &         + colamt(k,1) * forfac(k)*forref(j)                      &
     &         + n2omult * absn2ob(j)

          pfrac(ns03+j,k) = fracrefb(j,js)                              &
     &          + fs * (fracrefb(j,js+1) - fracrefb(j,js))
        enddo
      enddo

      return
!...................................
      end subroutine taugb03
!-----------------------------------


!-----------------------------------
      subroutine taugb04
!...................................

!     band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng04, ns04
      use module_radlw_kgb04
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat1, strrat2, speccomb,         &
     &      specmult, fs, fs1

      strrat1 = 850.577
      strrat2 = 35.7416

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat1*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,4) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa04, id1(k,4) + js )
        ind12 = min(msa04, ind11 + 1 )
        ind13 = min(msa04, ind11 + 9 )
        ind14 = min(msa04, ind11 + 10)
        inds = indself(k)

        do j = 1, ng04
          taug(ns04+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(ns04+j,k) = fracrefa(j,js)                              &
     &         + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        speccomb = colamt(k,3) + strrat2*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,3)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)
        if (js .gt. 1) then
          js = js + 1
        elseif (fs .ge. 0.0024) then
          js = 2
          fs = (fs - 0.0024) / 0.9976
        else
          js = 1
          fs = fs / 0.0024
        endif

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,4) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 6
        ind04 = ind01 + 7
        ind11 = id1(k,4) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 6
        ind14 = ind11 + 7

        do j = 1, ng04
          taug(ns04+j,k) = speccomb                                     &
     &         * ( fac000*absb(ind01,j) + fac100*absb(ind02,j) +        &
     &             fac010*absb(ind03,j) + fac110*absb(ind04,j) +        &
     &             fac001*absb(ind11,j) + fac101*absb(ind12,j) +        &
     &             fac011*absb(ind13,j) + fac111*absb(ind14,j) )

          pfrac(ns04+j,k) = fracrefb(j,js)                              &
     &          + fs * (fracrefb(j,js+1) - fracrefb(j,js))
        enddo

!     empirical modification to code to improve stratospheric cooling rates
!     for co2. revised to apply weighting for g-point reduction in this band.
!     from mike iacono, aer, april 01, 2003.

        taug(ns04+8, k) = taug(ns04+8, k) * 0.92
        taug(ns04+9, k) = taug(ns04+9, k) * 0.88
        taug(ns04+10,k) = taug(ns04+10,k) * 1.07
        taug(ns04+11,k) = taug(ns04+11,k) * 1.10
        taug(ns04+12,k) = taug(ns04+12,k) * 0.99
        taug(ns04+13,k) = taug(ns04+13,k) * 0.88
        taug(ns04+14,k) = taug(ns04+14,k) * 0.943

      enddo

      return
!...................................
      end subroutine taugb04
!-----------------------------------


!-----------------------------------
      subroutine taugb05
!...................................

!     band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng05, ns05
      use module_radlw_kgb05
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat1, strrat2, speccomb,         &
     &      specmult, fs, fs1

      strrat1 = 90.4894
      strrat2 = 0.900502

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat1*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,5) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa05, id1(k,5) + js )
        ind12 = min(msa05, ind11 + 1 )
        ind13 = min(msa05, ind11 + 9 )
        ind14 = min(msa05, ind11 + 10)
        inds = indself(k)

        do j = 1, ng05
          taug(ns05+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )    &
     &         + wx(k,1) * ccl4(j)

          pfrac(ns05+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        speccomb = colamt(k,3) + strrat2*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,3)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,5) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,5) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6

        do j = 1, ng05
          taug(ns05+j,k) = speccomb                                     &
     &         * ( fac000*absb(ind01,j) + fac100*absb(ind02,j) +        &
     &             fac010*absb(ind03,j) + fac110*absb(ind04,j) +        &
     &             fac001*absb(ind11,j) + fac101*absb(ind12,j) +        &
     &             fac011*absb(ind13,j) + fac111*absb(ind14,j) )        &
     &         + wx(k,1) * ccl4(j)

          pfrac(ns05+j,k) = fracrefb(j,js)                              &
     &          + fs * (fracrefb(j,js+1) - fracrefb(j,js))
        enddo
      enddo

      return
!...................................
      end subroutine taugb05
!-----------------------------------


!-----------------------------------
      subroutine taugb06
!...................................

!     band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng06, ns06
      use module_radlw_kgb06
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds, js, ns

!     compute the optical depth by interpolating in ln(pressure) and
!     temperature. the water vapor self-continuum is interpolated
!     (in temperature) separately.  

      do k = 1, laytrop
        ind01 = id0(k,6) + 1
        ind02 = ind01 + 1
        ind11 = min(msa06, id1(k,6) + 1 )
        ind12 = min(msa06, ind11 + 1 )
        inds = indself(k)

        do j = 1, ng06
          taug(ns06+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k) *( selfref(inds,j) +                      &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) ) )  &
     &         + wx(k,2) * cfc11adj(j) + wx(k,3) * cfc12(j)             &
     &         + co2mult(k) * absco2(j)

          pfrac(ns06+j,k) = fracrefa(j)
        enddo
      enddo

!     nothing important goes on above laytrop in this band.
      do k = laytrop+1, l
        do j = 1, ng06
          taug(ns06+j,k) = wx(k,2) * cfc11adj(j) + wx(k,3) * cfc12(j)

          pfrac(ns06+j,k) = fracrefa(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb06
!-----------------------------------


!-----------------------------------
      subroutine taugb07
!...................................

!     band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng07, ns07
      use module_radlw_kgb07
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 8.21104e4

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,3)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,7) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa07, id1(k,7) + js )
        ind12 = min(msa07, ind11 + 1 )
        ind13 = min(msa07, ind11 + 9 )
        ind14 = min(msa07, ind11 + 10)
        inds = indself(k)

        do j = 1, ng07
          taug(ns07+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )    &
     &         + co2mult(k) * absco2(j)

          pfrac(ns07+j,k) = fracrefa(j,js)                              &
     &              + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l

        ind01 = id0(k,7) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,7) + 1
        ind12 = ind11 + 1

        do j = 1, ng07
          taug(ns07+j,k) = colamt(k,3)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )    &
     &         + co2mult(k) * absco2(j)

          pfrac(ns07+j,k) = fracrefb(j)
        enddo

!     empirical modification to code to improve stratospheric cooling rates
!     for o3. revised to apply weighting for g-point reduction in this band.
!     from mike iacono, aer, april 01, 2003.

        taug(ns07+6, k) = taug(ns07+6, k) * 0.92
        taug(ns07+7, k) = taug(ns07+7, k) * 0.88
        taug(ns07+8, k) = taug(ns07+8, k) * 1.07
        taug(ns07+9, k) = taug(ns07+9, k) * 1.10
        taug(ns07+10,k) = taug(ns07+10,k) * 0.99
        taug(ns07+11,k) = taug(ns07+11,k) * 0.855

      enddo

      return
!...................................
      end subroutine taugb07
!-----------------------------------


!-----------------------------------
      subroutine taugb08
!...................................

!     band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng08, ns08
      use module_radlw_kgb08
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds, jp0, jp1
!
      real (kind=kind_rad) :: ratio, n2omult, tem1, tem2

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  

      do k = 1, layswtch
        ind01 = id0(k,8) + 1
        ind02 = ind01 + 1
        ind11 = min(msa08, id1(k,8) + 1 )
        ind12 = min(msa08, ind11 + 1 )
        inds = indself(k)

        jp0 = jp(k)
        jp1 = jp0 + 1
        tem1 = n2oref(jp0) / h2oref(jp0)
        tem2 = n2oref(jp1) / h2oref(jp1)
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - colamt(k,1)*ratio

        do j = 1, ng08
          taug(ns08+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k)*( selfref(inds,j) +                       &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) ) )  &
     &         + wx(k,3) * cfc12(j) + wx(k,4) * cfc22adj(j)             &
     &         + co2mult(k) * absco2a(j) + n2omult * absn2oa(j)

          pfrac(ns08+j,k) = fracrefa(j)
        enddo
      enddo

      do k = layswtch+1, l
        ind01 = id0(k,8) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,8) + 1
        ind12 = ind11 + 1

        jp0 = jp(k)
        jp1 = jp0 + 1
        tem1 = n2oref(jp0) / o3ref(jp0)
        tem2 = n2oref(jp1) / o3ref(jp1)
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - colamt(k,3) * ratio

        do j = 1, ng08
          taug(ns08+j,k) = colamt(k,3)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )    &
     &         + wx(k,3) * cfc12(j) + wx(k,4) * cfc22adj(j)             &
     &         + co2mult(k) * absco2b(j) + n2omult * absn2ob(j)

          pfrac(ns08+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb08
!-----------------------------------


!-----------------------------------
      subroutine taugb09
!...................................

!     band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng09, ns09
      use module_radlw_kgb09
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns, jfrac, ioff, jp0, jp1
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1,&
     &      ffrac, ratio, n2omult, tem0, tem1, tem2

      strrat = 21.6282
      ioff = 0

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        if (k .eq. laylow) ioff = ng09
        if (k .eq. layswtch) ioff = 2 * ng09

        speccomb = colamt(k,1) + strrat*colamt(k,5)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        jfrac = js
        fs = specmult - int(specmult)
        ffrac = fs
        if (js .eq. 8) then
          if (fs .le. 0.68) then
            fs = fs / 0.68
          elseif (fs .le. 0.92) then
            js = js + 1
            fs = (fs - 0.68) / 0.24
          else
            js = js + 2
            fs = (fs - 0.92) / 0.08
          endif
        elseif (js .eq.9) then
          js = 10
          fs = 1.0
          jfrac = 8
          ffrac = 1.0
        endif

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,9) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 11
        ind04 = ind01 + 12
        ind11 = min(msa09, id1(k,9) + js )
        ind12 = min(msa09, ind11 + 1 )
        ind13 = min(msa09, ind11 + 11)
        ind14 = min(msa09, ind11 + 12)
        inds = indself(k)

        jp0 = jp(k)
        jp1 = jp0 + 1
        ns = js + int(fs + 0.5)
        tem0 = (1.0 - etaref(ns)) / strrat
        if (ns .eq. 11) then
          tem1 = n2oref(jp0) / h2oref(jp0)
          tem2 = n2oref(jp1) / h2oref(jp1)
        else
          tem1 = tem0 * n2oref(jp0) / ch4ref(jp0)
          tem2 = tem0 * n2oref(jp1) / ch4ref(jp1)
        endif
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - speccomb*ratio

        do j = 1, ng09
          taug(ns09+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )    &
     &         + n2omult * absn2o(j+ioff)

          pfrac(ns09+j,k) = fracrefa(j,jfrac)                           &
     &          + ffrac * (fracrefa(j,jfrac+1)-fracrefa(j,jfrac))
        enddo
      enddo

      do k = laytrop+1, l
        ind01 = id0(k,9) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,9) + 1
        ind12 = ind11 + 1

        do j = 1, ng09
          taug(ns09+j,k) = colamt(k,5)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )

          pfrac(ns09+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb09
!-----------------------------------


!-----------------------------------
      subroutine taugb10
!...................................

!     band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng10, ns10
      use module_radlw_kgb10
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12
!
!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  

      do k = 1, laytrop
        ind01 = id0(k,10) + 1
        ind02 = ind01 + 1
        ind11 = min(msa10, id1(k,10) + 1 )
        ind12 = min(msa10, ind11 + 1 )

        do j = 1, ng10
          taug(ns10+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) ) 

          pfrac(ns10+j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, l
        ind01 = id0(k,10) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,10) + 1
        ind12 = ind11 + 1

        do j = 1, ng10
          taug(ns10+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 

          pfrac(ns10+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb10
!-----------------------------------


!-----------------------------------
      subroutine taugb11
!...................................

!     band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng11, ns11
      use module_radlw_kgb11
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds
!

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  below laytrop, the water vapor self-continuum 
!     is interpolated (in temperature) separately.  

      do k = 1, laytrop
        ind01 = id0(k,11) + 1
        ind02 = ind01 + 1
        ind11 = min(msa11, id1(k,11) + 1 )
        ind12 = min(msa11, ind11 + 1 )
        inds = indself(k)

        do j = 1, ng11
          taug(ns11+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k)*( selfref(inds,j) +  selffrac(k) *        &
     &            (selfref(inds+1,j)-selfref(inds,j)) ) )

          pfrac(ns11+j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, l
        ind01 = id0(k,11) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,11) + 1
        ind12 = ind11 + 1

        do j = 1, ng11
          taug(ns11+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 

          pfrac(ns11+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb11
!-----------------------------------


!-----------------------------------
      subroutine taugb12
!...................................

!     band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng12, ns12
      use module_radlw_kgb12
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 0.009736757

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,12) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa12, id1(k,12) + js )
        ind12 = min(msa12, ind11 + 1 )
        ind13 = min(msa12, ind11 + 9 )
        ind14 = min(msa12, ind11 + 10)
        inds = indself(k)

        do j = 1, ng12
          taug(ns12+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(ns12+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        do j = 1, ng12
          taug(ns12+j,k) = zero
          pfrac(ns12+j,k) = zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb12
!-----------------------------------


!-----------------------------------
      subroutine taugb13
!...................................

!     band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng13, ns13
      use module_radlw_kgb13
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 16658.87

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,4)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,13) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa13, id1(k,13) + js )
        ind12 = min(msa13, ind11 + 1 )
        ind13 = min(msa13, ind11 + 9 )
        ind14 = min(msa13, ind11 + 10)
        inds = indself(k)

        do j = 1, ng13
          taug(ns13+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(ns13+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        do j = 1, ng13
          taug(ns13+j,k) = zero
          pfrac(ns13+j,k) = zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb13
!-----------------------------------


!-----------------------------------
      subroutine taugb14
!...................................

!     band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng14, ns14
      use module_radlw_kgb14
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds
!

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  below laytrop, the water vapor self-continuum 
!     is interpolated (in temperature) separately.  

      do k = 1, laytrop
        ind01 = id0(k,14) + 1
        ind02 = ind01 + 1
        ind11 = min(msa14, id1(k,14) + 1 )
        ind12 = min(msa14, ind11 + 1 )
        inds = indself(k)

        do j = 1, ng14
          taug(ns14+j,k) = colamt(k,2)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) )    &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(ns14+j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, l
        ind01 = id0(k,14) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,14) + 1
        ind12 = ind11 + 1

        do j = 1, ng14
          taug(ns14+j,k) = colamt(k,2)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 

          pfrac(ns14+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb14
!-----------------------------------


!-----------------------------------
      subroutine taugb15
!...................................

!     band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng15, ns15
      use module_radlw_kgb15
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 0.2883201

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,4) + strrat*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,4)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,15) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa15, id1(k,15) + js )
        ind12 = min(msa15, ind11 + 1 )
        ind13 = min(msa15, ind11 + 9 )
        ind14 = min(msa15, ind11 + 10)
        inds = indself(k)

        do j = 1, ng15
          taug(ns15+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(ns15+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        do j = 1, ng15
          taug(ns15+j,k) = zero
          pfrac(ns15+j,k) = zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb15
!-----------------------------------


!-----------------------------------
      subroutine taugb16
!...................................

!     band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
      use machine,                 only : kind_rad
      use module_radlw_parameters, only : ng16, ns16
      use module_radlw_kgb16
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_rad) :: fac000, fac010, fac100, fac110, fac001,   &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 830.411

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,5)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,16) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(msa16, id1(k,16) + js )
        ind12 = min(msa16, ind11 + 1 )
        ind13 = min(msa16, ind11 + 9 )
        ind14 = min(msa16, ind11 + 10)
        inds = indself(k)

        do j = 1, ng16
          taug(ns16+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(ns16+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, l
        do j = 1, ng16
          taug(ns16+j,k) = zero
          pfrac(ns16+j,k) = zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb16
!-----------------------------------


!...................................
      end subroutine taumol
!-----------------------------------


!
!........................................!
      end module module_radlw_main       !
!========================================!

