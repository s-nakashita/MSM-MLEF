!-------------------------------------------------------------------------------
! Description:
!
!   Module for declaring derived types.
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2018, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

module radsim_mod_types

use radsim_mod_constants, only : &
  int32, &
  int64, &
  real32, &
  real64

implicit none

!-------------------------------------------------------------------------------
! Derived-type declarations
!-------------------------------------------------------------------------------

!----------------
! Grid definition
!----------------

type grid_type
  integer :: type = -1
  integer :: ncols = 0
  integer :: nrows = 0
  integer :: xdir = 0
  integer :: ydir = 0
  logical :: row_order = .true.
  real :: lon1
  real :: lat1
  real :: dlon
  real :: dlat
  real :: pole_lon
  real :: pole_lat
  real, pointer :: lambda(:) => null() ! Single row of longitude coordinates
  real, pointer :: phi(:)    => null() ! Single column of latitude coordinates
  real(real32), pointer :: lon(:) => null() ! Multiple rows of longitude coordinates
  real(real32), pointer :: lat(:) => null() ! Multiple columns of latitude coordinates
end type grid_type

!-------------
! Model fields
!-------------

type model_type

  ! Metadata
  integer :: nprofs
  integer :: nlevels
  integer(int32), pointer :: validity_time(:,:) => null() ! Data time plus forecast time
  integer(int32), pointer :: data_time(:,:) => null()     ! Analysis time
  integer :: ref_time ! Number of minutes since reference time; used for temporal interpolation
  real(real64) :: rmdi = -2.0_8**31
  integer :: imdi = -2**15

  ! Geolocation
  type(grid_type)       :: grid
  real(real64), pointer :: lsm(:) => null()
  real(real64), pointer :: zsurf(:) => null()
  real(real64), pointer :: seaice(:) => null()

  ! Single-level quantities
  real(real64), pointer :: pstar(:) => null()
  real(real64), pointer :: t2(:) => null()
  real(real64), pointer :: td2(:) => null()
  real(real64), pointer :: q2(:) => null()
  real(real64), pointer :: rh2(:) => null()
  real(real64), pointer :: tskin(:) => null()
  real(real64), pointer :: u10(:) => null()
  real(real64), pointer :: v10(:) => null()
  real(real64), pointer :: total_cc(:) => null()
  real(real64), pointer :: low_cc(:) => null()
  real(real64), pointer :: medium_cc(:) => null()
  real(real64), pointer :: high_cc(:) => null()
  real(real64), pointer :: snow_depth(:) => null()

  ! Multi-level quantities
  real(real64), pointer :: z(:,:) => null()
  real(real64), pointer :: p(:,:) => null()
  real(real64), pointer :: ph(:,:) => null()
  real(real64), pointer :: t(:,:) => null()
  real(real64), pointer :: theta(:,:) => null()
  real(real64), pointer :: q(:,:) => null()
  real(real64), pointer :: rh(:,:) => null()
  real(real64), pointer :: density(:,:) => null()
  real(real64), pointer :: clw(:,:) => null()
  real(real64), pointer :: clw_deff(:,:) => null()
  real(real64), pointer :: ciw(:,:) => null()
  real(real64), pointer :: ciw_deff(:,:) => null()
  real(real64), pointer :: rain(:,:) => null()
  real(real64), pointer :: snow(:,:) => null()
  real(real64), pointer :: cfrac(:,:) => null()
  real(real64), pointer :: cfrac_liq(:,:) => null()
  real(real64), pointer :: cfrac_ice(:,:) => null()
  real(real64), pointer :: cfrac_conv(:,:) => null()
  real(real64), pointer :: conv_cloud(:,:) => null()
  real(real64), pointer :: conv_inc(:,:) => null()
  real(real64), pointer :: o3(:,:) => null()
  real(real64), pointer :: co2(:,:) => null()
  real(real64), pointer :: n2o(:,:) => null()
  real(real64), pointer :: co(:,:) => null()
  real(real64), pointer :: ch4(:,:) => null()
  real(real64), pointer :: so2(:,:) => null()
  real(real64), pointer :: cams_sea_salt1(:,:) => null()
  real(real64), pointer :: cams_sea_salt2(:,:) => null()
  real(real64), pointer :: cams_sea_salt3(:,:) => null()
  real(real64), pointer :: cams_dust1(:,:) => null()
  real(real64), pointer :: cams_dust2(:,:) => null()
  real(real64), pointer :: cams_dust3(:,:) => null()
  real(real64), pointer :: cams_hphil_omat(:,:) => null()
  real(real64), pointer :: cams_hphob_bcar(:,:) => null()
  real(real64), pointer :: cams_sulphate(:,:) => null()

end type model_type

!---------
! Obs data
!---------

type obs_type

  integer :: nobs = 0

  ! Geolocation
  integer(int32), pointer :: year(:) => null()
  integer(int32), pointer :: month(:) => null()
  integer(int32), pointer :: day(:) => null()
  integer(int32), pointer :: hour(:) => null()
  integer(int32), pointer :: minute(:) => null()
  integer(int32), pointer :: ref_time(:) => null()
  real(real32),   pointer :: lat(:) => null()
  real(real32),   pointer :: lon(:) => null()
  real(real32),   pointer :: zsurf(:) => null()
  real(real32),   pointer :: satzen(:) => null()
  real(real32),   pointer :: satazim(:) => null()
  real(real32),   pointer :: solzen(:) => null()
  real(real32),   pointer :: solazim(:) => null()
  real(real32),   pointer :: lsm(:) => null()
  real(real32),   pointer :: footprint_rmajor(:) => null()
  real(real32),   pointer :: footprint_rminor(:) => null()
  integer(int32), pointer :: rtsurf(:) => null()
  integer(int32), pointer :: viewid(:) => null()
  integer(int32), pointer :: scanline(:) => null()
  integer(int32), pointer :: scanpos(:) => null()
  ! QC
  integer(int32), pointer :: qcflags(:) => null()
  integer(int32), pointer :: qcinfo(:) => null()
  integer(int32), pointer :: qcrttov(:) => null()

  ! Radiances
  real(real32), pointer :: bt(:,:) => null()
  real(real32), pointer :: refl(:,:) => null()
  real(real32), pointer :: radiance(:,:) => null()
  real(real32), pointer :: emiss(:,:) => null()
  real(real32), pointer :: brdf(:,:) => null()
  real(real32), pointer :: cads_height_assignment(:,:) => null()
  real(real32), pointer :: geometric_height(:,:) => null()
  real(real32), pointer :: tjac(:,:,:) => null()
  real(real32), pointer :: qjac(:,:,:) => null()
  real(real32), pointer :: o3jac(:,:,:) => null()
  real(real32), pointer :: tskinjac(:,:) => null()
  real(real32), pointer :: wind10mujac(:,:) => null()
  real(real32), pointer :: wind10mvjac(:,:) => null()
  real(real32), pointer :: emissjac(:,:) => null()
  real(real32), pointer :: trans(:,:,:) => null()

  ! Instrument
  integer(int32), pointer :: channels(:) => null()
  real(real32),   pointer :: wavenumbers(:) => null()

  ! Footprint simulations
  integer(int32), pointer :: sim_to_obs(:) => null()
  integer(int32), pointer :: nsim_per_obs(:) => null()

end type obs_type

!----------------------
! Footprint file data
!----------------------

type footprint_data_type
  integer(int32)          :: file_id
  integer(int32)          :: var_id_nsim_per_obs
  integer(int32)          :: var_id_qcinfo
  integer(int32)          :: var_id_indices
  integer(int32)          :: footprint_index_count = 0
end type footprint_data_type

!----------------------
! UM fieldsfile headers
!----------------------

type ff_hd_type
  integer                 :: word_size
  integer(int64)          :: nbytes
  integer(int64)          :: fix_len_header(256)
  integer(int64), pointer :: int_c(:) => null()
  integer(int64), pointer :: lookup_i(:,:) => null()
  real(real64),   pointer :: lookup_r(:,:) => null()
  real(real64),   pointer :: real_c(:) => null()
  real(real64),   pointer :: level_dep_c(:,:) => null()
  real(real64),   pointer :: row_dep_c(:,:) => null()
  real(real64),   pointer :: col_dep_c(:,:) => null()
end type ff_hd_type

!-----------------
! NWP SAF datasets
!-----------------

! See https://www.nwpsaf.eu/site/software/atmospheric-profile-data/
!
! Structure components are reproduced directly from the NWP SAF code supplied
! to read in the dataset, in the order in which they appear in the files.

type ecprof60_type
  integer(int32) :: jdat    ! date (yyyymmddhh)
  real(real32) :: srlon     ! longitude (deg)
  real(real32) :: srlat     ! latitude (deg)
  real(real32) :: srlsm     ! land/see mask (1=land, 0=sea)
  real(real32) :: sst       ! surface skin temperature (K)
  real(real32) :: spsurf    ! surface pressure (hPa)
  real(real32) :: su10      ! 10-meter u wind (m/s)
  real(real32) :: sv10      ! 10-meter v wind (m/s)
  real(real32) :: st2m      ! 2-meter temperature (K)
  real(real32) :: sq2m      ! 2-meter specific humidity (kg/kg)
  real(real32) :: stt(60)   ! temperature (K)
  real(real32) :: swv(60)   ! specific humidity (kg/kg)
  real(real32) :: so3(60)   ! specific ozone (kg/kg)
  real(real32) :: scc(60)   ! cloud cover
  real(real32) :: sclw(60)  ! cloud liquid water content (kg/kg)
  real(real32) :: sciw(60)  ! cloud ice water content (kg/kg)
  real(real32) :: sw(60)    ! vertical velocity (Pa/s)
  real(real32) :: scvl      ! low vegetation cover
  real(real32) :: scvh      ! high vegetation cover
  real(real32) :: stvl      ! type of low vegetation 
  real(real32) :: stvh      ! type of high vegetation 
  real(real32) :: sci       ! ice cover
  real(real32) :: sasn      ! snow albedo (0-1)
  real(real32) :: srsn      ! snow density (kg/m3)
  real(real32) :: stsn      ! snow temperature (K)
  real(real32) :: ssd       ! snow depth (m)
  real(real32) :: ssr       ! surface roughness (m)
  real(real32) :: sal       ! surface albedo (0-1)
  real(real32) :: sistl(4)  ! layer ice temp. (K)  (top to bottom)
  real(real32) :: sstl(4)   ! layer soil temp. (K) (top to bottom)
  real(real32) :: sswvl(4)  ! layer soil volumetric water (m3/m3) (top to bottom)
  real(real32) :: soro      ! surface geometric height (m)
  type(ecprof60_type), pointer :: next => null()
end type ecprof60_type

type ecprof91_type
  real :: temp(91)   !  1) Temperature [K]                          (1-91)
  real :: hum(91)    !  2) Humidity [kg/kg]                         (92-182)
  real :: ozo(91)    !  3) Ozone [kg/kg]                            (183-273)
  real :: cc(91)     !  4) Cloud Cover [0-1]                        (274-364)
  real :: clw(91)    !  5) C Liquid W [kg/kg]                       (365-455)
  real :: ciw(91)    !  6) C Ice W [kg/kg]                          (456-546)
  real :: rain(91)   !  7) Rain [kg/(m2 *s)]                        (547-637)
  real :: snow(91)   !  8) Snow [kg/(m2 *s)]                        (638-728)
  real :: w(91)      !  9) Vertical Velocity [Pa/s]                 (729-819)
  real :: lnpsurf    ! 10) Ln of Surf Pressure in [Pa]              (820)
  real :: z0         ! 11) Surface geopotential [m2/s2]             (821) 
  real :: tsurf      ! 12) Surface Skin Temperature [K]             (822)
  real :: t2m        ! 13) 2m Temperature [K]                       (823)
  real :: td2m       ! 14) 2m Dew point temperature [K]             (824)
  real :: hum2m      ! 15) 2m Specific Humidity [kg/kg]             (825)
  real :: u10        ! 16) 10m wind speed U component [m/s]         (826)
  real :: v10        ! 17) 10m wind speed V component [m/s]         (827)
  real :: stratrsrf  ! 18) Stratiform rain at surface [kg/(m2 *s)]  (828)
  real :: convrsrf   ! 19) Convective rain at surface [kg/(m2 *s)]  (829)
  real :: snowsurf   ! 20) Snow at surface [kg/(m2 *s)]             (830)
  real :: lsm        ! 21) Land/sea Mask [0-1]                      (831)
  real :: lat        ! 22) Latitude [deg]                           (832)
  real :: long       ! 23) Longitude [deg]                          (833)
  integer :: year    ! 24) Year                                     (834)
  integer :: month   ! 25) Month                                    (835)
  integer :: day     ! 26) Day                                      (836)
  integer :: step    ! 27) Step                                     (837)
  integer :: gpoint  ! 28) Grid point [1-843490]                    (838)
  integer :: ind     ! 29) Index (rank-sorted)                      (839) 
  real :: seaice     ! item 7 from the surface file [0-1]
  type(ecprof91_type), pointer :: next => null()
end type ecprof91_type

type ecprof137_type
  real :: temp(137)  !  1) Temperature [K]                          (1-137)
  real :: hum(137)   !  2) Humidity [kg/kg]                         (138-274)
  real :: ozo(137)   !  3) Ozone [kg/kg]                            (275-411)
  real :: cc(137)    !  4) Cloud Cover [0-1]                        (412-548)
  real :: clw(137)   !  5) C Liquid W [kg/kg]                       (549-685)
  real :: ciw(137)   !  6) C Ice W [kg/kg]                          (686-822)
  real :: rain(137)  !  7) Rain [kg/(m2 *s)]                        (823-959)
  real :: snow(137)  !  8) Snow [kg/(m2 *s)]                        (960-1096)
  real :: w(137)     !  9) Vertical Velocity [Pa/s]                 (1097-1233)
  real :: lnpsurf    ! 10) Ln of Surf Pressure in [Pa]              (1234)
  real :: z0         ! 11) Surface geopotential [m2/s2]             (1235) 
  real :: tsurf      ! 12) Surface Skin Temperature [K]             (1236)
  real :: t2m        ! 13) 2m Temperature [K]                       (1237)
  real :: td2m       ! 14) 2m Dew point temperature [K]             (1238)
  real :: u10        ! 15) 10m wind speed U component [m/s]         (1239)
  real :: v10        ! 16) 10m wind speed V component [m/s]         (1240)
  real :: stratrsrf  ! 17) Stratiform precipitation at surface [m]  (1241)
  real :: convrsrf   ! 18) Convective precipitation at surface [m]  (1242)
  real :: snowsurf   ! 19) Snow at surface [kg/(m2 *s)]             (1243)
  real :: lsm        ! 20) Land/sea Mask [0-1]                      (1244)
  real :: lat        ! 21) Latitude [deg]                           (1245)
  real :: long       ! 22) Longitude [deg]                          (1246)
  integer :: year    ! 23) Year                                     (1247)
  integer :: month   ! 24) Month                                    (1248)
  integer :: day     ! 25) Day                                      (1249)
  integer :: step    ! 26) Step                                     (1250)
  integer :: gpoint  ! 27) Grid point [1-843490]                    (1251)
  integer :: ind     ! 28) Index (rank-sorted)                      (1252) 
  real :: seaice     ! item 7 from the surface file [0-1]
  type(ecprof137_type), pointer :: next => null()
end type ecprof137_type

end module radsim_mod_types
