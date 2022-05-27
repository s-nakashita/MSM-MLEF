      module physcons
      use machine,only:kind_phys
!  physical constants as set in nmc handbook from smithsonian tables.
!  physical constants are given to 5 places.
!  1990/04/30: g and rd are made consistent with nws usage.
!  2001/10/22: g made consistent with si usage.
!  2009/12/28: add some microphysics constants by zyf
!  math constants
      real(kind=kind_phys),parameter:: con_pi      =3.1415926535897931 ! pi
      real(kind=kind_phys),parameter:: con_sqrt2   =1.414214e+0 ! square root of 2
      real(kind=kind_phys),parameter:: con_sqrt3   =1.732051e+0 ! square root of 3
!  primary constants
      real(kind=kind_phys),parameter:: con_rerth   =6.3712e+6 ! radius of earth     (m)
      real(kind=kind_phys),parameter:: con_g       =9.80665e+0! gravity             (m/s2)
      real(kind=kind_phys),parameter:: con_omega   =7.2921e-5 ! ang vel of earth    (1/s)
      real(kind=kind_phys),parameter:: con_rd      =2.8705e+2 ! gas constant air    (j/kg/k)
      real(kind=kind_phys),parameter:: con_rv      =4.6150e+2 ! gas constant h2o    (j/kg/k)
      real(kind=kind_phys),parameter:: con_cp      =1.0046e+3 ! spec heat air @p    (j/kg/k)
      real(kind=kind_phys),parameter:: con_cv      =7.1760e+2 ! spec heat air @v    (j/kg/k)
      real(kind=kind_phys),parameter:: con_cvap    =1.8460e+3 ! spec heat h2o gas   (j/kg/k)
      real(kind=kind_phys),parameter:: con_cliq    =4.1855e+3 ! spec heat h2o liq   (j/kg/k)
      real(kind=kind_phys),parameter:: con_csol    =2.1060e+3 ! spec heat h2o ice   (j/kg/k)
      real(kind=kind_phys),parameter:: con_hvap    =2.5000e+6 ! lat heat h2o cond   (j/kg)
      real(kind=kind_phys),parameter:: con_hfus    =3.3358e+5 ! lat heat h2o fusion (j/kg)
      real(kind=kind_phys),parameter:: con_psat    =6.1078e+2 ! pres at h2o 3pt     (pa)  
      real(kind=kind_phys),parameter:: con_sbc     =5.6730e-8 ! stefan-boltzmann    (w/m2/k4)
      real(kind=kind_phys),parameter:: con_solr    =1.3533e+3 ! solar constant      (w/m2)
      real(kind=kind_phys),parameter:: con_t0c     =2.7315e+2 ! temp at 0c          (k)
      real(kind=kind_phys),parameter:: con_ttp     =2.7316e+2 ! temp at h2o 3pt     (k)
      real(kind=kind_phys),parameter:: con_jcal    =4.1855e+0 ! joules per calorie  ()
! add by zyf
      real(kind=kind_phys),parameter:: con_rhoair0 = 1.28      ! air density        (kg/m3)
      real(kind=kind_phys),parameter:: con_rhoh2o  = 1000.0    ! water density      (kg/m3)
      real(kind=kind_phys),parameter:: con_rhosnow = 100.0     ! snow density       (kg/m3)

!  secondary constants
      real(kind=kind_phys),parameter:: con_rocp    =con_rd/con_cp
      real(kind=kind_phys),parameter:: con_cpor    =con_cp/con_rd
      real(kind=kind_phys),parameter:: con_rog     =con_rd/con_g
      real(kind=kind_phys),parameter:: con_fvirt   =con_rv/con_rd-1.
      real(kind=kind_phys),parameter:: con_eps     =con_rd/con_rv
      real(kind=kind_phys),parameter:: con_epsm1   =con_rd/con_rv-1.
      real(kind=kind_phys),parameter:: con_dldt    =con_cvap-con_cliq
      real(kind=kind_phys),parameter:: con_xpona   =-con_dldt/con_rv
      real(kind=kind_phys),parameter:: con_xponb   =                    &
     &                       -con_dldt/con_rv+con_hvap/(con_rv*con_ttp)

      end module
