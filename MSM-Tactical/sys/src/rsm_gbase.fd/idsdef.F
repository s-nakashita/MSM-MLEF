      subroutine idsdef(iptv,ids)
!$$$  subprogram documentation block
!
! subprogram: idsdef         sets default decimal scalings
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: sets decimal scalings defaults for various parameters.
!   a decimal scaling of -3 means data is packed in kilo-si units.
!
! program history log:
!   92-10-31  iredell
!
! usage:    call idsdef(iptv,ids)
!   input arguments:
!     iptv         paramter table version (only 1 or 2 is recognized)
!   output arguments:
!     ids          integer (255) decimal scalings
!                  (unknown decimal scalings will not be set)
!
! attributes:
!   language: cray fortran
!
!$$$
      dimension ids(255)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(iptv.eq.1.or.iptv.eq.2) then
        ids(001)=-1     ! pressure (pa)
        ids(002)=-1     ! sea-level pressure (pa)
        ids(003)=5      ! pressure tendency (pa/s)
                        !
                        !
        ids(006)=-1     ! geopotential (m2/s2)
        ids(007)=0      ! geopotential height (m)
        ids(008)=0      ! geometric height (m)
        ids(009)=0      ! standard deviation of height (m)
                        !
        ids(011)=1      ! temperature (k)
        ids(012)=1      ! virtual temperature (k)
        ids(013)=1      ! potential temperature (k)
        ids(014)=1      ! pseudo-adiabatic potential temperature (k)
        ids(015)=1      ! maximum temperature (k)
        ids(016)=1      ! minimum temperature (k)
        ids(017)=1      ! dewpoint temperature (k)
        ids(018)=1      ! dewpoint depression (k)
        ids(019)=4      ! temperature lapse rate (k/m)
        ids(020)=0      ! visibility (m)
                        ! radar spectra 1 ()
                        ! radar spectra 2 ()
                        ! radar spectra 3 ()
                        !
        ids(025)=1      ! temperature anomaly (k)
        ids(026)=-1     ! pressure anomaly (pa)
        ids(027)=0      ! geopotential height anomaly (m)
                        ! wave spectra 1 ()
                        ! wave spectra 2 ()
                        ! wave spectra 3 ()
        ids(031)=0      ! wind direction (degrees)
        ids(032)=1      ! wind speed (m/s)
        ids(033)=1      ! zonal wind (m/s)
        ids(034)=1      ! meridional wind (m/s)
        ids(035)=-4     ! streamfunction (m2/s)
        ids(036)=-4     ! velocity potential (m2/s)
        ids(037)=-1     ! montgomery stream function (m2/s2)
        ids(038)=8      ! sigma vertical velocity (1/s)
        ids(039)=3      ! pressure vertical velocity (pa/s)
        ids(040)=4      ! geometric vertical velocity (m/s)
        ids(041)=6      ! absolute vorticity (1/s)
        ids(042)=6      ! absolute divergence (1/s)
        ids(043)=6      ! relative vorticity (1/s)
        ids(044)=6      ! relative divergence (1/s)
        ids(045)=4      ! vertical u shear (1/s)
        ids(046)=4      ! vertical v shear (1/s)
        ids(047)=0      ! direction of current (degrees)
                        ! speed of current (m/s)
                        ! u of current (m/s)
                        ! v of current (m/s)
        ids(051)=5      ! specific humidity (kg/kg)
        ids(052)=0      ! relative humidity (percent)
        ids(053)=5      ! humidity mixing ratio (kg/kg)
        ids(054)=1      ! precipitable water (kg/m2)
        ids(055)=-1     ! vapor pressure (pa)
        ids(056)=-1     ! saturation deficit (pa)
        ids(057)=1      ! evaporation (kg/m2)
        ids(058)=1      ! cloud ice (kg/m2)
        ids(059)=6      ! precipitation rate (kg/m2/s)
        ids(060)=0      ! thunderstorm probability (percent)
        ids(061)=1      ! total precipitation (kg/m2)
        ids(062)=1      ! large-scale precipitation (kg/m2)
        ids(063)=1      ! convective precipitation (kg/m2)
        ids(064)=6      ! water equivalent snowfall rate (kg/m2/s)
        ids(065)=0      ! water equivalent of snow depth (kg/m2)
        ids(066)=2      ! snow depth (m)
                        ! mixed-layer depth (m)
                        ! transient thermocline depth (m)
                        ! main thermocline depth (m)
                        ! main thermocline anomaly (m)
        ids(071)=0      ! total cloud cover (percent)
        ids(072)=0      ! convective cloud cover (percent)
        ids(073)=0      ! low cloud cover (percent)
        ids(074)=0      ! middle cloud cover (percent)
        ids(075)=0      ! high cloud cover (percent)
        ids(076)=1      ! cloud water (kg/m2)
                        !
        ids(078)=1      ! convective snow (kg/m2)
        ids(079)=1      ! large scale snow (kg/m2)
        ids(080)=1      ! water temperature (k)
        ids(081)=0      ! sea-land mask ()
                        ! deviation of sea level from mean (m)
        ids(083)=5      ! roughness (m)
        ids(084)=1      ! albedo (percent)
        ids(085)=1      ! soil temperature (k)
        ids(086)=0      ! soil wetness (kg/m2)
        ids(087)=0      ! vegetation (percent)
                        ! salinity (kg/kg)
        ids(089)=4      ! density (kg/m3)
        ids(090)=1      ! runoff (kg/m2)
        ids(091)=0      ! ice concentration ()
                        ! ice thickness (m)
        ids(093)=0      ! direction of ice drift (degrees)
                        ! speed of ice drift (m/s)
                        ! u of ice drift (m/s)
                        ! v of ice drift (m/s)
                        ! ice growth (m)
                        ! ice divergence (1/s)
        ids(099)=1      ! snow melt (kg/m2)
                        ! sig height of waves and swell (m)
        ids(101)=0      ! direction of wind waves (degrees)
                        ! sig height of wind waves (m)
                        ! mean period of wind waves (s)
        ids(104)=0      ! direction of swell waves (degrees)
                        ! sig height of swell waves (m)
                        ! mean period of swell waves (s)
        ids(107)=0      ! primary wave direction (degrees)
                        ! primary wave mean period (s)
        ids(109)=0      ! secondary wave direction (degrees)
                        ! secondary wave mean period (s)
        ids(111)=0      ! net solar radiative flux at surface (w/m2)
        ids(112)=0      ! net longwave radiative flux at surface (w/m2)
        ids(113)=0      ! net solar radiative flux at top (w/m2)
        ids(114)=0      ! net longwave radiative flux at top (w/m2)
        ids(115)=0      ! net longwave radiative flux (w/m2)
        ids(116)=0      ! net solar radiative flux (w/m2)
        ids(117)=0      ! total radiative flux (w/m2)
        ids(118)=5      ! maximum specific humidity (kg/kg)
        ids(119)=5      ! minimum specific humidity (kg/kg)
                        !
                        !
                        !
        ids(121)=0      ! latent heat flux (w/m2)
        ids(122)=0      ! sensible heat flux (w/m2)
        ids(123)=0      ! boundary layer dissipation (w/m2)
        ids(124)=3      ! u wind stress (n/m2)
        ids(125)=3      ! v wind stress (n/m2)
                        ! wind mixing energy (j)
                        ! image data ()
        ids(128)=-1     ! mean sea-level pressure (stdatm) (pa)
        ids(129)=-1     ! mean sea-level pressure (maps) (pa)
        ids(130)=-1     ! mean sea-level pressure (eta) (pa)
        ids(131)=1      ! surface lifted index (k)
        ids(132)=1      ! best lifted index (k)
        ids(133)=1      ! k index (k)
        ids(134)=1      ! sweat index (k)
        ids(135)=10     ! horizontal moisture divergence (kg/kg/s)
        ids(136)=4      ! speed shear (1/s)
        ids(137)=5      ! 3-hr pressure tendency (pa/s)
        ids(138)=6      ! brunt-vaisala frequency squared (1/s2)
        ids(139)=11     ! potential vorticity (mass-weighted) (1/s/m)
        ids(140)=0      ! rain mask ()
        ids(141)=0      ! freezing rain mask ()
        ids(142)=0      ! ice pellets mask ()
        ids(143)=0      ! snow mask ()
        ids(144)=3      ! volumetric soil moisture content (fraction)
        ids(145)=0      ! potential evaporation rate (w/m2)
        ids(146)=0      ! cloud workfunction (j/kg)
        ids(147)=3      ! u gravity wave stress (n/m2)
        ids(148)=3      ! v gravity wave stress (n/m2)
        ids(149)=10     ! potential vorticity (m2/s/kg)
                        ! covariance between v and u (m2/s2)
                        ! covariance between u and t (k*m/s)
                        ! covariance between v and t (k*m/s)
                        !
                        !
        ids(155)=0      ! ground heat flux (w/m2)
        ids(156)=0      ! convective inhibition (w/m2)
        ids(157)=0      ! convective ape (j/kg)
        ids(158)=0      ! turbulent ke (j/kg)
        ids(159)=-1     ! condensation pressure of lifted parcel (pa)
        ids(160)=0      ! clear sky upward solar flux (w/m2)
        ids(161)=0      ! clear sky downward solar flux (w/m2)
        ids(162)=0      ! clear sky upward longwave flux (w/m2)
        ids(163)=0      ! clear sky downward longwave flux (w/m2)
        ids(164)=0      ! cloud forcing net solar flux (w/m2)
        ids(165)=0      ! cloud forcing net longwave flux (w/m2)
        ids(166)=0      ! visible beam downward solar flux (w/m2)
        ids(167)=0      ! visible diffuse downward solar flux (w/m2)
        ids(168)=0      ! near ir beam downward solar flux (w/m2)
        ids(169)=0      ! near ir diffuse downward solar flux (w/m2)
                        !
                        !
        ids(172)=3      ! momentum flux (n/m2)
        ids(173)=0      ! mass point model surface ()
        ids(174)=0      ! velocity point model surface ()
        ids(175)=0      ! sigma layer number ()
        ids(176)=2      ! latitude (degrees)
        ids(177)=2      ! east longitude (degrees)
                        !
                        !
                        !
        ids(181)=9      ! x-gradient log pressure (1/m)
        ids(182)=9      ! y-gradient log pressure (1/m)
        ids(183)=5      ! x-gradient height (m/m)
        ids(184)=5      ! y-gradient height (m/m)
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
        ids(201)=0      ! ice-free water surcace (percent)
                        !
                        !
        ids(204)=0      ! downward solar radiative flux (w/m2)
        ids(205)=0      ! downward longwave radiative flux (w/m2)
                        !
        ids(207)=0      ! moisture availability (percent)
                        ! exchange coefficient (kg/m2/s)
        ids(209)=0      ! number of mixed layer next to sfc ()
                        !
        ids(211)=0      ! upward solar radiative flux (w/m2)
        ids(212)=0      ! upward longwave radiative flux (w/m2)
        ids(213)=0      ! non-convective cloud cover (percent)
        ids(214)=6      ! convective precipitation rate (kg/m2/s)
        ids(215)=7      ! total diabatic heating rate (k/s)
        ids(216)=7      ! total radiative heating rate (k/s)
        ids(217)=7      ! total diabatic nonradiative heating rate (k/s)
        ids(218)=2      ! precipitation index (fraction)
        ids(219)=1      ! std dev of ir t over 1x1 deg area (k)
        ids(220)=4      ! natural log of surface pressure over 1 kpa ()
        ids(221)=0      ! planetary boundary layer height (m)
        ids(222)=0      ! 5-wave geopotential height (m)
        ids(223)=1      ! plant canopy surface water (kg/m2)
                        !
                        !
                        ! blackadars mixing length (m)
                        ! asymptotic mixing length (m)
        ids(228)=1      ! potential evaporation (kg/m2)
        ids(229)=0      ! snow phase-change heat flux (w/m2)
                        !
        ids(231)=3      ! convective cloud mass flux (pa/s)
        ids(232)=0      ! downward total radiation flux (w/m2)
        ids(233)=0      ! upward total radiation flux (w/m2)
        ids(224)=1      ! baseflow-groundwater runoff (kg/m2)
        ids(225)=1      ! storm surface runoff (kg/m2)
                        !
                        !
        ids(238)=0      ! snow cover (percent)
        ids(239)=1      ! snow temperature (k)
                        !
        ids(241)=7      ! large scale condensation heating rate (k/s)
        ids(242)=7      ! deep convective heating rate (k/s)
        ids(243)=10     ! deep convective moistening rate (kg/kg/s)
        ids(244)=7      ! shallow convective heating rate (k/s)
        ids(245)=10     ! shallow convective moistening rate (kg/kg/s)
        ids(246)=7      ! vertical diffusion heating rate (kg/kg/s)
        ids(247)=7      ! vertical diffusion zonal acceleration (m/s/s)
        ids(248)=7      ! vertical diffusion merid acceleration (m/s/s)
        ids(249)=10     ! vertical diffusion moistening rate (kg/kg/s)
        ids(250)=7      ! solar radiative heating rate (k/s)
        ids(251)=7      ! longwave radiative heating rate (k/s)
                        ! drag coefficient ()
                        ! friction velocity (m/s)
                        ! richardson number ()
                        !
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
