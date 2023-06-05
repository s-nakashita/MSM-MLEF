!-------------------------------------------------------------------------------
! Description:
!
!   Read a GRIB file into a model structure.
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

subroutine radsim_read_grib(file_name, model_ntimes, model, file_name_ancil)

use eccodes

use radsim_mod_cfg, only : &
  output_mode, &
  model_filetype, &
  temporal_data, &
  run_scatt, &
  mmr_snowrain, &
  clw_data, &
  ozone_data, &
  ozone_mgperkg, &
  ir_addaerosols_cams, &
  ir_addclouds, &
  ircloud_clw_scheme, &
  ircloud_ice_scheme, &
  ircloud_use_model_clw_deff, &
  ircloud_use_model_ice_deff

use radsim_mod_constants, only : &
  real64, &
  int32, &
  gravity, &
  status_error, &
  output_verbose, &
  output_default

use radsim_mod_io, only : &
  filetype_grib_ecmwf, &
  filetype_grib_icon, &
  filetype_grib_harmonie, &
  filetype_grib_jma

use radsim_mod_process

use radsim_mod_types, only : &
  model_type

use radsim_mod_functions, only : &
  time_in_minutes, &
  date_time_plus_minutes

use rttov_const, only : &
  clw_scheme_deff, &
  ice_scheme_baum

implicit none

include 'radsim_calc_plevels.interface'
include 'radsim_error_report.interface'
include 'radsim_grib_paramid_name.interface'
include 'radsim_dealloc_model.interface'

! Subroutine args
character(len=400), intent(in)  :: file_name
integer,            intent(out) :: model_ntimes
type(model_type),   intent(out) :: model(:)
character(len=400), intent(in)  :: file_name_ancil

! Local variables
integer :: i, j, imodel
integer(int32) :: file_id, message_id, status
integer :: nmessage
integer :: field_list(nfields_rttov, 5)
character(len=80) :: ltype_list(nfields_rttov, 5)
integer :: iltype_list(nfields_rttov, 5)
integer :: level_list(nfields_rttov, 5)
integer :: remap_list(nfields_rttov, 5)
logical :: got_field(nfields_rttov)
integer :: nprofs, nlevels, model_nlevels
integer :: field_index
integer :: paramId, indicatorOfTypeOfLevel
integer :: startStep, firstStartStep, prevStartStep, stepUnits
integer :: date_time(5)
integer :: numberOfDataPoints
integer :: NV
real(real64), allocatable :: pv(:)
integer :: gridType
integer :: Ni
integer :: Nj
real :: latitudeOfFirstGridPointInDegrees
real :: longitudeOfFirstGridPointInDegrees
real :: latitudeOfLastGridPointInDegrees
real :: longitudeOfLastGridPointInDegrees
real :: jDirectionIncrementInDegrees
real :: iDirectionIncrementInDegrees
integer :: jPointsAreConsecutive
integer :: iScansNegatively
integer :: jScansPositively
character(len=10) :: centre = ''
character(len=10) :: dataDate
character(len=6) :: dataTime
character(len=80) :: typeOfLevel
character(len=80) :: message
integer :: level
real(real64), pointer :: values(:)
real(real64) :: missingvalue
character(len=20) :: param_name
character(len=20) :: model_name
logical :: ecmwf = .false.
logical :: icon = .false.
logical :: harmonie = .false.
logical :: jma = .false.

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Define required field list
!-------------------------------------------------------------------------------

! Each field has a unique ID which is the same in GRIB-1 and 2. These are
! stored in the paramID variable*.

! Some fields may be stored incorrectly, or they may use different level codes
! to that of the UM, hence we allow for primary and secondary requirements
! (secondary used if primary not present). Also, some fields may be derived
! from secondary fields under certain circumstances.

! *HARMONIE is an exception to this: instead it uses the parameter type
! indicator which requires also the level type indicator and level number to
! uniquely identify each field. Once a field is identified the paramIds are
! remapped to ECMWF equivalents to simplify the code.

! *JMA also uses the typeOfLevel to identify fields.

field_list = -1
level_list = -1
ltype_list = ''

! See Annex in RTTOV Users Guide for required fields
! Separate field IDs are specified for each supported source model

if ( model_filetype == filetype_grib_ecmwf ) then

  !---------------
  ! Surface fields
  !---------------

  field_list(field_rttov_tskin, 1) = 235    ! skin temperature
  field_list(field_rttov_t2, 1)    = 167    ! temperature at 2m
  field_list(field_rttov_q2, 1)    = 168    ! dewpoint temperature at 2m
  field_list(field_rttov_pstar, 1) = 134    ! surface pressure
  field_list(field_rttov_pstar, 2) = 152    ! log of surface pressure
  field_list(field_rttov_u10, 1)   = 165    ! 10m wind u component
  field_list(field_rttov_v10, 1)   = 166    ! 10m wind v component

  ! Surface type usually needs to be derived from 2 fields - a land-sea mask and
  ! the seaice fraction. The priority is disabled in this case.

  field_list(field_rttov_surftype, 1) = 172    ! LSM
  field_list(field_rttov_surftype, 2) =  31    ! seaice cover
  field_list(field_rttov_surftype, 3) = 129    ! geopotential

  !---------
  ! Profiles
  !---------

  field_list(field_rttov_p, 1) =  54  ! pressure levels
  field_list(field_rttov_t, 1) = 130  ! T
  field_list(field_rttov_t, 2) =   3  ! theta
  field_list(field_rttov_q, 1) = 133  ! specific humidity
  field_list(field_rttov_q, 2) = 157  ! relative humidity

  if ( ozone_data ) then
    field_list(field_rttov_ozone, 1) = 203  ! ozone
  endif

  ! Profiles for running scattering code

  if ( clw_data .or. run_scatt .or. ir_addclouds ) then
    field_list(field_rttov_qcl, 1) = 246     ! qcl
  end if

  if ( run_scatt .or. ir_addclouds ) then
    field_list(field_rttov_qcf, 1)   = 247    ! qcf
    field_list(field_rttov_cfrac, 1) = 248    ! cloud cover
  end if

  if ( run_scatt ) then
    field_list(field_rttov_rain, 1) = 75     ! rain mixing ratio
    field_list(field_rttov_snow, 1) = 76     ! snow mixing ratio
    mmr_snowrain = .true.                    ! specify mass mixing ratio
  end if

  where (field_list(:,1) > 0)
    got_field = .false.
  elsewhere
    got_field = .true.
  end where

  ! Optional fields must be specified after got_field has been initialised

  ! CAMS aerosol fields are optional if aerosol simulations enabled
  if ( ir_addaerosols_cams ) then
    field_list(field_rttov_cams_salt1, 1)      = 210001 ! sea salt size bin 1
    field_list(field_rttov_cams_salt2, 1)      = 210002 ! sea salt size bin 2
    field_list(field_rttov_cams_salt3, 1)      = 210003 ! sea salt size bin 3
    field_list(field_rttov_cams_dust1, 1)      = 210004 ! dust size bin 1
    field_list(field_rttov_cams_dust2, 1)      = 210005 ! dust size bin 2
    field_list(field_rttov_cams_dust3, 1)      = 210006 ! dust size bin 3
    field_list(field_rttov_cams_hphil_omat, 1) = 210007 ! hydrophilic organic matter
    field_list(field_rttov_cams_hphob_bcar, 1) = 210010 ! hydrophobic black carbon
    field_list(field_rttov_cams_sulph, 1)      = 210011 ! sulphate
  end if

  ! Fields not used by RTTOV, but if present are read in, interpolated, and
  ! output with other fields
  field_list(field_total_cc, 1)  = 164 ! Total cloud cover
  field_list(field_low_cc, 1)    = 186 ! Low cloud cover
  field_list(field_medium_cc, 1) = 187 ! Medium cloud cover
  field_list(field_high_cc, 1)   = 188 ! High cloud cover

  ecmwf = .true.

else if ( model_filetype == filetype_grib_icon ) then

  !---------------
  ! Surface fields
  !---------------

  ! Latitude/longitude
  field_list(field_rttov_lat, 1)   = 502309  ! CLAT (latitude of triangle circumcentre)
  field_list(field_rttov_lat, 2)   = 250003  !
  field_list(field_rttov_lon, 1)   = 502310  ! CLON (longitude of triangle circumcentre)
  field_list(field_rttov_lon, 2)   = 250004  !

  ! Surface variables
  field_list(field_rttov_tskin, 1) = 500010 ! skin temperature
  field_list(field_rttov_t2, 1)    = 167    ! temperature at 2m
  field_list(field_rttov_t2, 2)    = 500011 !
  field_list(field_rttov_q2, 1)    = 168    ! dewpoint temperature at 2m
  field_list(field_rttov_q2, 2)    = 500017 !
  field_list(field_rttov_pstar, 1) = 134    ! surface pressure
  field_list(field_rttov_pstar, 2) = 500000 ! 
  field_list(field_rttov_u10, 1)   = 165    ! 10m wind u component
  field_list(field_rttov_u10, 2)   = 500027 !
  field_list(field_rttov_v10, 1)   = 166    ! 10m wind v component
  field_list(field_rttov_v10, 2)   = 500029 !

  ! Surface type usually needs to be derived from 2 fields - a land-sea mask and
  ! the seaice fraction. The priority is disabled in this case.

  field_list(field_rttov_surftype, 1) = 172    ! LSM
  field_list(field_rttov_surftype, 2) = 500054 ! 
  field_list(field_rttov_surftype, 3) = 31     ! seaice cover
  field_list(field_rttov_surftype, 4) = 500069 ! 
  field_list(field_rttov_surftype, 5) = 500007 ! land surface height

  !---------
  ! Profiles
  !---------

  field_list(field_rttov_p, 1)  = 54     ! pressure levels (full- and half-levels)
  field_list(field_rttov_p, 2)  = 500001 !
  field_list(field_rttov_t, 1)  = 130    ! T
  field_list(field_rttov_t, 2)  = 500014 !
  field_list(field_rttov_q, 1)  = 133    ! specific humidity
  field_list(field_rttov_q, 2)  = 500035 !

  if ( ozone_data ) then
    field_list(field_rttov_ozone, 1) = 203    ! ozone
    field_list(field_rttov_ozone, 2) = 500242
  endif

  ! Profiles for running scattering code

  if ( clw_data .or. run_scatt .or. ir_addclouds ) then
    field_list(field_rttov_qcl, 1) = 503056  ! tot_qc_dia (total liquid cloud inc. sub-grid)
  end if

  if ( run_scatt .or. ir_addclouds ) then
    field_list(field_rttov_qcf, 1)   = 503057 ! tot_qi_dia (total ice cloud inc. sub-grid)
    field_list(field_rttov_cfrac, 1) = 260257 ! cloud cover
    field_list(field_rttov_cfrac, 2) = 500098 ! cloud cover
  end if

  if ( ir_addclouds .and. ircloud_clw_scheme == clw_scheme_deff .and. &
       ircloud_use_model_clw_deff) then
    field_list(field_rttov_clw_deff, 1) = 500779 ! cloud liquid diameter
    field_list(field_rttov_clw_deff, 2) = 503566 !
  end if

  if ( ir_addclouds .and. ircloud_ice_scheme == ice_scheme_baum .and. &
       ircloud_use_model_ice_deff) then
    field_list(field_rttov_ciw_deff, 1) = 500781 ! cloud ice effective diameter
    field_list(field_rttov_ciw_deff, 2) = 503568 !
  end if

  if ( run_scatt ) then
    field_list(field_rttov_rain, 1) = 260020 ! rain mixing ratio
    field_list(field_rttov_rain, 2) = 500102 !
    field_list(field_rttov_snow, 1) = 260021 ! snow mixing ratio
    field_list(field_rttov_snow, 2) = 500103 !
    mmr_snowrain = .true.                    ! specify mass mixing ratio
  end if

  where (field_list(:,1) > 0)
    got_field = .false.
  elsewhere
    got_field = .true.
  end where

  ! Optional fields which are used if read in
  if ( ir_addclouds .and. ircloud_clw_scheme == clw_scheme_deff .and. &
       .not. ircloud_use_model_clw_deff) then
    field_list(field_rttov_density, 1) = 3089   ! density
    field_list(field_rttov_density, 2) = 500545 !
  end if

  icon = .true.

else if ( model_filetype == filetype_grib_harmonie ) then

  ! For each indicatorOfParameter ID (field_list) we must also have a
  ! corresponding indicatorOfTypeOfLevel (iltype_list) and the ECMWF paramId
  ! (see above) in remap_list. For 2D fields, the level is also required
  ! (level_list) for identification.

  !---------------
  ! Surface fields
  !---------------

  field_list(field_rttov_tskin, 1)  = 11     ! skin temperature
  iltype_list(field_rttov_tskin, 1) = 105
  level_list(field_rttov_tskin, 1)  = 0
  remap_list(field_rttov_tskin, 1)  = 235

  field_list(field_rttov_t2, 1)     = 11     ! temperature at 2m
  iltype_list(field_rttov_t2, 1)    = 105
  level_list(field_rttov_t2, 1)     = 2
  remap_list(field_rttov_t2, 1)     = 167

  field_list(field_rttov_q2, 1)     = 17     ! dewpoint temperature at 2m
  iltype_list(field_rttov_q2, 1)    = 105
  level_list(field_rttov_q2, 1)     = 2
  remap_list(field_rttov_q2, 1)     = 168

  field_list(field_rttov_pstar, 1)  = 1      ! surface pressure
  iltype_list(field_rttov_pstar, 1) = 105
  level_list(field_rttov_pstar, 1)  = 0
  remap_list(field_rttov_pstar, 1)  = 134

  field_list(field_rttov_u10, 1)    = 33     ! 10m wind u component
  iltype_list(field_rttov_u10, 1)   = 105
  level_list(field_rttov_u10, 1)    = 10
  remap_list(field_rttov_u10, 1)    = 165

  field_list(field_rttov_v10, 1)    = 34     ! 10m wind v component
  iltype_list(field_rttov_v10, 1)   = 105
  level_list(field_rttov_v10, 1)    = 10
  remap_list(field_rttov_v10, 1)    = 166

  ! Surface type usually needs to be derived from 2 fields - a land-sea mask and
  ! the seaice fraction. The priority is disabled in this case.

  field_list(field_rttov_surftype, 1)  = 81     ! LSM
  iltype_list(field_rttov_surftype, 1) = 105
  level_list(field_rttov_surftype, 1)  = 0
  remap_list(field_rttov_surftype, 1)  = 172

  field_list(field_rttov_surftype, 2)  = 91     ! seaice cover
  iltype_list(field_rttov_surftype, 2) = 102
  level_list(field_rttov_surftype, 2)  = 0
  remap_list(field_rttov_surftype, 2)  = 31

  field_list(field_rttov_surftype, 3)  = 6      ! geopotential
  iltype_list(field_rttov_surftype, 3) = 105
  level_list(field_rttov_surftype, 3)  = 0
  remap_list(field_rttov_surftype, 3)  = 129

  !---------
  ! Profiles
  !---------

  field_list(field_rttov_t, 1)  = 11   ! T
  iltype_list(field_rttov_t, 1) = 109
  remap_list(field_rttov_t, 1)  = 130

  field_list(field_rttov_q, 1)  = 51   ! specific humidity
  iltype_list(field_rttov_q, 1) = 109
  remap_list(field_rttov_q, 1)  = 133

  ! Profiles for running scattering code

  if ( clw_data .or. run_scatt .or. ir_addclouds ) then
    field_list(field_rttov_qcl, 1)  = 76      ! qcl
    iltype_list(field_rttov_qcl, 1) = 109
    remap_list(field_rttov_qcl, 1)  = 246
  end if

  if ( run_scatt .or. ir_addclouds ) then
    field_list(field_rttov_qcf, 1)    = 58     ! qcf
    iltype_list(field_rttov_qcf, 1)   = 109
    remap_list(field_rttov_qcf, 1)    = 247

    field_list(field_rttov_cfrac, 1)  = 71     ! cloud cover
    iltype_list(field_rttov_cfrac, 1) = 109
    remap_list(field_rttov_cfrac, 1)  = 248
  end if

  if ( run_scatt ) then
    field_list(field_rttov_rain, 1)  = 181    ! rain mixing ratio
    iltype_list(field_rttov_rain, 1) = 109
    remap_list(field_rttov_rain, 1)  = 75

    field_list(field_rttov_snow, 1)  = 184    ! snow mixing ratio
    iltype_list(field_rttov_snow, 1) = 109
    remap_list(field_rttov_snow, 1)  = 76
    mmr_snowrain = .true.                    ! specify mass mixing ratio
  end if

  where (field_list(:,1) > 0)
    got_field = .false.
  elsewhere
    got_field = .true.
  end where

  ! Optional fields which are used if present (set got_field before this)
  field_list(field_rttov_snow_depth, 1)   = 66      ! Snow depth
  iltype_list(field_rttov_snow_depth, 1)  = 105
  level_list(field_rttov_snow_depth, 1)   = 0
  remap_list(field_rttov_snow_depth, 1)   = 66      ! No ECMWF equivalent, just needs to be unique

  ! Fields not used by RTTOV, but if present are read in, interpolated, and
  ! output with other fields
  field_list(field_total_cc, 1)   = 71  ! Total cloud cover
  iltype_list(field_total_cc, 1)  = 105
  level_list(field_total_cc, 1)   = 0
  remap_list(field_total_cc, 1)   = 164

  field_list(field_low_cc, 1)     = 73  ! Low cloud cover
  iltype_list(field_low_cc, 1)    = 105
  level_list(field_low_cc, 1)     = 0
  remap_list(field_low_cc, 1)     = 186

  field_list(field_medium_cc, 1)  = 74  ! Medium cloud cover
  iltype_list(field_medium_cc, 1) = 105
  level_list(field_medium_cc, 1)  = 0
  remap_list(field_medium_cc, 1)  = 187

  field_list(field_high_cc, 1)    = 75  ! High cloud cover
  iltype_list(field_high_cc, 1)   = 105
  level_list(field_high_cc, 1)    = 0
  remap_list(field_high_cc, 1)    = 188

  harmonie = .true.

else if ( model_filetype == filetype_grib_jma ) then

  !---------------
  ! Surface fields
  !---------------

  field_list(field_rttov_tskin, 1) = 194    ! skin temperature
  ltype_list(field_rttov_tskin, 1) = 'surface'
  level_list(field_rttov_tskin, 1) = 0
  remap_list(field_rttov_tskin, 1) = 235

  field_list(field_rttov_t2, 1)    = 167    ! temperature at 2m
  ltype_list(field_rttov_t2, 1)    = 'heightAboveGround'
  level_list(field_rttov_t2, 1)    = 2
  remap_list(field_rttov_t2, 1)    = 167
  field_list(field_rttov_t2, 2)    = 130    ! temperature at 2m
  ltype_list(field_rttov_t2, 2)    = 'heightAboveGround'
  level_list(field_rttov_t2, 2)    = 2
  remap_list(field_rttov_t2, 2)    = 167

  field_list(field_rttov_q2, 1)    = 133    ! specific humidity at 2m
  ltype_list(field_rttov_q2, 1)    = 'heightAboveGround'
  level_list(field_rttov_q2, 1)    = 2
  remap_list(field_rttov_q2, 1)    = 133
  field_list(field_rttov_q2, 2)    = 174096
  ltype_list(field_rttov_q2, 2)    = 'heightAboveGround'
  level_list(field_rttov_q2, 2)    = 2
  remap_list(field_rttov_q2, 2)    = 133

  field_list(field_rttov_pstar, 1) = 134    ! surface pressure
  ltype_list(field_rttov_pstar, 1) = 'surface'
  level_list(field_rttov_pstar, 1) = 0
  remap_list(field_rttov_pstar, 1) = 134
  field_list(field_rttov_pstar, 2) = 54    ! surface pressure
  ltype_list(field_rttov_pstar, 2) = 'surface'
  level_list(field_rttov_pstar, 2) = 0
  remap_list(field_rttov_pstar, 2) = 134

  field_list(field_rttov_u10, 1)   = 165    ! 10m wind u component
  ltype_list(field_rttov_u10, 1)   = 'heightAboveGround'
  level_list(field_rttov_u10, 1)   = 10
  remap_list(field_rttov_u10, 1)   = 165
  field_list(field_rttov_u10, 2)   = 131    ! 10m wind u component
  ltype_list(field_rttov_u10, 2)   = 'heightAboveGround'
  level_list(field_rttov_u10, 2)   = 10
  remap_list(field_rttov_u10, 2)   = 165

  field_list(field_rttov_v10, 1)   = 166    ! 10m wind v component
  ltype_list(field_rttov_v10, 1)   = 'heightAboveGround'
  level_list(field_rttov_v10, 1)   = 10
  remap_list(field_rttov_v10, 1)   = 166
  field_list(field_rttov_v10, 2)   = 132    ! 10m wind v component
  ltype_list(field_rttov_v10, 2)   = 'heightAboveGround'
  level_list(field_rttov_v10, 2)   = 10
  remap_list(field_rttov_v10, 2)   = 166

  ! Surface type usually needs to be derived from 2 fields - a land-sea mask and
  ! the seaice fraction. The priority is disabled in this case.

  field_list(field_rttov_surftype, 1) = 172    ! LSM
  ltype_list(field_rttov_surftype, 1) = 'surface'
  level_list(field_rttov_surftype, 1) = 0
  remap_list(field_rttov_surftype, 1) = 172

  field_list(field_rttov_surftype, 2) = 31     ! seaice cover
  ltype_list(field_rttov_surftype, 2) = 'surface'
  level_list(field_rttov_surftype, 2) = 0
  remap_list(field_rttov_surftype, 2) = 31
  field_list(field_rttov_surftype, 3) = 3091   ! seaice cover
  ltype_list(field_rttov_surftype, 3) = 'surface'
  level_list(field_rttov_surftype, 3) = 0
  remap_list(field_rttov_surftype, 3) = 31

  field_list(field_rttov_surftype, 4) = 129    ! geopotential
  ltype_list(field_rttov_surftype, 4) = 'surface'
  level_list(field_rttov_surftype, 4) = 0
  remap_list(field_rttov_surftype, 4) = 129

  !---------
  ! Profiles
  !---------

  field_list(field_rttov_t, 1) = 130  ! T
  ltype_list(field_rttov_t, 1) = 'hybrid'
  remap_list(field_rttov_t, 1) = 130

  field_list(field_rttov_q, 1) = 133  ! specific humidity
  ltype_list(field_rttov_q, 1) = 'hybrid'
  remap_list(field_rttov_q, 1) = 133

  if ( ozone_data ) then
    field_list(field_rttov_ozone, 1) = 203  ! ozone (kg/kg)
    ltype_list(field_rttov_ozone, 1) = 'hybrid'
    remap_list(field_rttov_ozone, 1) = 203

    field_list(field_rttov_ozone, 2) = 260624 ! ozone (mg/kg)
    ltype_list(field_rttov_ozone, 2) = 'hybrid'
    remap_list(field_rttov_ozone, 2) = 260624
  endif

  where (field_list(:,1) > 0)
    got_field = .false.
  elsewhere
    got_field = .true.
  end where

  jma = .true.

end if


!-------------------------------------------------------------------------------
! 2a. Read GRIB
!-------------------------------------------------------------------------------

! Data is stored as a series of messages, with one field per message. In the
! case of multi-level fields, each level is in a separate message.

print '(a)', 'Reading GRIB messages from ' // trim(file_name)

call grib_open_file(file_id, file_name, 'r')

nmessage = 0
firstStartStep = -1
prevStartStep = -1
imodel = 0

do

  !----------------
  ! Get new message
  !----------------

  call grib_new_from_file(file_id, message_id, status)

  ! Exit if no message found

  if ( status == GRIB_END_OF_FILE ) then
    print '(a)', 'Finished reading: EOF'
    exit
  else if ( status /= GRIB_SUCCESS ) then
    message = 'Could not read GRIB message'
    call radsim_error_report(message, status_error)
  end if

  nmessage = nmessage + 1

  !---------------
  ! Identify field
  !---------------

  call grib_get(message_id, 'centre', centre)
  if ( harmonie ) then
    call grib_get(message_id, 'indicatorOfParameter', paramId)
    call grib_get(message_id, 'indicatorOfTypeOfLevel', indicatorOfTypeOfLevel)
  else
    call grib_get(message_id, 'paramId', paramId)
  end if
  call grib_get(message_id, 'dataDate', dataDate)
  call grib_get(message_id, 'dataTime', dataTime)
  call grib_get(message_id, 'startStep', startStep)
  call grib_get(message_id, 'stepUnits', stepUnits)
  call grib_get(message_id, 'level', level)
  call grib_get(message_id, 'typeOfLevel', typeOfLevel)

  if ( output_mode >= output_verbose ) then
    print '(a)',    '---------------'
    print '(2a)',   'centre = ', centre
    print '(a,i0)', 'paramId = ', paramId
    print '(2a)',   'dataDate = ', dataDate
    print '(2a)',   'dataTime = ', dataTime
    print '(a,i3)', 'startStep = ', startStep
    print '(a,i3)', 'stepUnits = ', stepUnits
  end if

  if ( (centre == 'ecmf' .or. centre == 'ecmwf') .and. .not. ecmwf ) then
    print '(a)', 'GRIB "centre" indicates ECMWF data, but model_filetype is not ECMWF GRIB!!'
  end if

  if ( centre == 'rjtd' .and. .not. jma ) then
    print '(a)', 'GRIB "centre" indicates JMA data, but model_filetype is not JMA GRIB!!'
  end if

  ! We use the startStep to determine the time of each field. We assume fields
  ! are stored in monotonically ascending step order, but it is possible that
  ! e.g. all surface fields (for all steps) are stored before all vertical
  ! profile datasets, so we allow for the "step counter" to be reset and
  ! update the imodel index appropriately to ensure each field is stored with
  ! the appropriate model(:) dataset.
  !
  ! Assumptions:
  ! - the same fields are stored for every time step
  ! - the startStep value determines the time of each field
  ! - startStep can be arbitrary (different step sizes are supported)
  ! - fields are stored in monotonically ascending order of startStep; where
  !   this doesn't happen startStep must jump back to the first step contained
  !   in the grib file.

  if ( firstStartStep < 0 ) firstStartStep = startStep

  if ( startStep /= prevStartStep ) then
    if ( startStep == firstStartStep ) then
      imodel = 1
    else
      imodel = imodel + 1
      prevStartStep = startStep
    end if
  end if

  ! Check to see if we need it

  field_index = 0
  do j = 1, size(field_list, dim = 2)
    do i = 1, size(field_list, dim = 1)
      if ( field_list(i,j) == -1 ) cycle
      if ( paramId == field_list(i,j) ) then
        if ( harmonie ) then
          ! For HARMONIE, must also match type of level and, for 2D fields, level
          ! Then change paramId to the ECMWF value to uniquely identify fields
          ! in the rest of the code
          if ( indicatorOfTypeOfLevel == iltype_list(i,j) ) then
            if ( level == level_list(i,j) .or. level_list(i,j) < 0 ) then
              field_index = i
              paramId = remap_list(i,j)
            end if
          end if
        else if ( jma ) then
          ! For JMA, must also match level type and, for 2D fields, level
          ! Then change paramId to the ECMWF value to uniquely identify fields
          ! in the rest of the code
          if ( trim(typeOfLevel) == trim(ltype_list(i,j)) ) then
            if ( level == level_list(i,j) .or. level_list(i,j) < 0 ) then
              field_index = i
              paramId = remap_list(i,j)
            end if
          end if
        else
          field_index = i
        end if
      end if
    end do
  end do

  if ( field_index == 0 ) then
    if ( output_mode >= output_verbose ) print '(a)', 'Skipping this field'
    cycle
  else
    if ( output_mode >= output_verbose ) then
      print '(2a)', 'Getting data for: ', field_names_rttov(field_index)
    end if
  end if

  !---------
  ! Set time
  !---------

  if ( .not. associated(model(imodel) % validity_time)) then
    allocate(model(imodel) % validity_time(1,5), model(imodel) % data_time(1,5))
    read(dataDate,'(i4,2i2)') model(imodel) % data_time(1,1:3)
    read(dataTime,'(2i2)') model(imodel) % data_time(1,4:5)

    model(imodel) % ref_time = time_in_minutes(model(imodel) % data_time(1,:))
    date_time = model(imodel) % data_time(1,:)

    ! See code table 4
    select case ( stepUnits )
    case ( 0 )  ! minutes
      model(imodel) % ref_time = model(imodel) % ref_time + startStep
      call date_time_plus_minutes(startStep, date_time)
    case ( 1 )  ! hours
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 60
      call date_time_plus_minutes(startStep * 60, date_time)
    case ( 2 )  ! days
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 24 * 60
      call date_time_plus_minutes(startStep * 24 * 60, date_time)
    case ( 3 )  ! months
      date_time(1) = date_time(1) + int((date_time(2) - 1 + startStep) / 12)
      date_time(2) = date_time(2) + mod(startStep, 12)
      model(imodel) % ref_time = time_in_minutes(date_time)
    case ( 4 )  ! years
      date_time(1) = date_time(1) + startStep
      model(imodel) % ref_time = time_in_minutes(date_time)
    case ( 5 )  ! 10 years
      date_time(1) = date_time(1) + 10 * startStep
      model(imodel) % ref_time = time_in_minutes(date_time)
    case ( 6 )  ! 30 years
      date_time(1) = date_time(1) + 30 * startStep
      model(imodel) % ref_time = time_in_minutes(date_time)
    case ( 7 )  ! 100 years
      date_time(1) = date_time(1) + 100 * startStep
      model(imodel) % ref_time = time_in_minutes(date_time)
    case ( 10 )  ! 3 hours
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 3 * 60
      call date_time_plus_minutes(startStep * 3 * 60, date_time)
    case ( 11 )  ! 6 hours
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 6 * 60
      call date_time_plus_minutes(startStep * 6 * 60, date_time)
    case ( 12 )  ! 12 hours
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 12 * 60
      call date_time_plus_minutes(startStep * 12 * 60, date_time)
    case ( 13 )  ! seconds
      model(imodel) % ref_time = model(imodel) % ref_time + int(startStep / 60)
      call date_time_plus_minutes(int(startStep / 60), date_time)
    case ( 14 )  ! quarter hour
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 15
      call date_time_plus_minutes(startStep * 15, date_time)
    case ( 15 )  ! half hour
      model(imodel) % ref_time = model(imodel) % ref_time + startStep * 30
      call date_time_plus_minutes(startStep * 30, date_time)
    case default
      write(message,'(a,i0)') 'Unknown stepUnits: ', stepUnits
      call radsim_error_report(message, status_error)
    end select
    model(imodel) % validity_time(1,:) = date_time
  end if

  !----------------------
  ! Number of data values
  !----------------------

  ! For the ECMWF model, NV is the number of vertical coordinate coefficients
  ! that are stored in pv. These can be used to construct pressure-level
  ! profiles (see below). There are 2 coeffs for each half-level pressure. The
  ! number of full-level pressures is one fewer than half-levels.

  ! Similar considerations apply to HARMONIE.

  ! For the ICON model, NV is the number of half-levels which is one more than
  ! full-levels.

  ! For JMA we hard code to JRA-55 (60L) when the files do not contain this
  ! information.

  call grib_get(message_id, 'numberOfDataPoints', numberOfDataPoints)
  call grib_get(message_id, 'numberOfVerticalCoordinateValues', NV)
  call grib_get(message_id, 'missingValue', missingValue)

  if ( jma ) then
    if ( trim(typeOfLevel) == 'heightAboveGround' .or. &
         trim(typeOfLevel) == 'surface' ) then
      level = 0
    else if ( NV == 0 ) then
      ! Only do this for multi-level fields
      NV = 122      ! Hard-coded for 60 levels, if not specified
    end if
  end if

  ! Assume missing data indicator is the same for all fields
  model(imodel) % rmdi = missingValue

  nprofs = numberOfDataPoints
  if ( ecmwf .or. harmonie .or. jma ) then
    nlevels = NV/2 - 1
  else ! ICON
    nlevels = NV - 1
  end if

  ! Store the number of model levels
  if ( nlevels > 1 ) model_nlevels = nlevels

  ! Read pressure coefficients first time we hit a multi-level field
  if ( (ecmwf .or. harmonie .or. jma) .and. &
       nlevels > 1 .and. .not. allocated(pv) ) then
    allocate(pv(NV))
    call grib_get(message_id, 'pv', pv, status)
    if ( status /= GRIB_SUCCESS ) pv = -1
  end if

  call radsim_grib_paramid_name(paramid, param_name, icon, typeOfLevel)

  if ( output_mode >= output_verbose ) then
    print '(a,i0)', 'numberOfDataPoints = ', numberOfDataPoints
    print '(a,i0)', 'nlevels = ', abs(nlevels)
    print '(a,i0)', 'level = ', level
  else if ( output_mode == output_default ) then
    print '(a,i0,a,i0,a)', 'Getting level ', level, ' data with paramId: ', &
      paramId, ' (' // trim(param_name) // ')'
  end if

  !----------------
  ! Grid definition
  !----------------

  ! The grid definition is available for every field. In most cases this will be
  ! the same but some fields may be on a staggered grid.

  ! We read the grid definition for all time steps for safety, but RadSim assumes
  ! the grid is identical for all time steps

  ! Unstructured grids (in particular ICON) - note that the gridType computed
  ! key does not identify unstructured grids so we must check
  ! the gridDefinitionTemplateNumber key first
  call grib_get(message_id, 'gridDefinitionTemplateNumber', gridType)

  if ( gridType == 101 ) then
    ! Unstructured grid: the lats/lons are read in later

    if ( model(imodel) % grid % type < 0 ) then

      model(imodel) % grid % type = gridType

    else
      ! Check to see if grid is different

      if ( model(imodel) % grid % type /= gridType ) then

        print '(a)', 'WARNING: GRID IS DIFFERENT!'

      end if

      if ( imodel > 1 ) then

        if ( model(imodel) % grid % type /= model(1) % grid % type ) then

          print '(a)', 'WARNING: GRID IS DIFFERENT TO FIRST TIME STEP!'

        end if

      end if

    end if

  else

    call grib_get(message_id, 'gridType', gridType)

    if ( gridType > 1 ) then
      write(message,'(a,i0,a)') 'This grid type is not supported: ', gridType, &
      ' (see GRIB-2 code table 3.1 for definitions).'
      call radsim_error_report(message, status_error)
    end if

    ! Regular lat/lon grid
    call grib_get(message_id, 'Ni', Ni)
    call grib_get(message_id, 'Nj', Nj)
    call grib_get(message_id, 'latitudeOfFirstGridPointInDegrees', latitudeOfFirstGridPointInDegrees)
    call grib_get(message_id, 'longitudeOfFirstGridPointInDegrees', longitudeOfFirstGridPointInDegrees)
    call grib_get(message_id, 'latitudeOfLastGridPointInDegrees', latitudeOfLastGridPointInDegrees)
    call grib_get(message_id, 'longitudeOfLastGridPointInDegrees', longitudeOfLastGridPointInDegrees)
    call grib_get(message_id, 'jDirectionIncrementInDegrees', jDirectionIncrementInDegrees)
    call grib_get(message_id, 'iDirectionIncrementInDegrees', iDirectionIncrementInDegrees)
    call grib_get(message_id, 'iScansNegatively', iScansNegatively)
    call grib_get(message_id, 'jScansPositively', jScansPositively)
    call grib_get(message_id, 'jPointsAreConsecutive', jPointsAreConsecutive)

    if ( output_mode >= output_verbose ) then
      print '(a,i0)', 'gridType = ', gridType
      print '(a,i0)', 'Ni = ', Ni
      print '(a,i0)', 'Nj = ', Nj
      print '(a,f8.2)', 'latitudeOfFirstGridPointInDegrees = ', latitudeOfFirstGridPointInDegrees
      print '(a,f8.2)', 'longitudeOfFirstGridPointInDegrees = ', longitudeOfFirstGridPointInDegrees
      print '(a,f8.2)', 'latitudeOfLastGridPointInDegrees = ', latitudeOfLastGridPointInDegrees
      print '(a,f8.2)', 'longitudeOfLastGridPointInDegrees = ', longitudeOfLastGridPointInDegrees
      print '(a,f8.2)', 'jDirectionIncrementInDegrees = ', jDirectionIncrementInDegrees
      print '(a,f8.2)', 'iDirectionIncrementInDegrees = ', iDirectionIncrementInDegrees
      print '(a,i0)', 'iScansNegatively = ', iScansNegatively
      print '(a,i0)', 'jScansPositively = ', jScansPositively
      print '(a,i0)', 'jPointsAreConsecutive = ', jPointsAreConsecutive
    end if

    if ( .not. associated(model(imodel) % grid % lat) ) then

      model(imodel) % grid % type = gridType
      model(imodel) % grid % lon1 = longitudeOfFirstGridPointInDegrees
      model(imodel) % grid % lat1 = latitudeOfFirstGridPointInDegrees
      model(imodel) % grid % dlon = abs(iDirectionIncrementInDegrees)
      model(imodel) % grid % dlat = abs(jDirectionIncrementInDegrees)
      if ( iScansNegatively == 1 ) model(imodel) % grid % dlon = -model(imodel) % grid % dlon
      if ( jScansPositively == 0 ) model(imodel) % grid % dlat = -model(imodel) % grid % dlat
      if ( jPointsAreConsecutive == 1 ) model(imodel) % grid % row_order = .false.
      model(imodel) % grid % ncols = Ni
      model(imodel) % grid % nrows = Nj
      model(imodel) % grid % pole_lon = 0
      model(imodel) % grid % pole_lat = 0

    else

    ! Check to see if grid is different

      if ( model(imodel) % grid % type /= gridType .or. &
           model(imodel) % grid % lon1 /= longitudeOfFirstGridPointInDegrees .or. &
           model(imodel) % grid % lat1 /= latitudeOfFirstGridPointInDegrees .or. &
           abs(model(imodel) % grid % dlon) /= abs(iDirectionIncrementInDegrees) .or. &
           abs(model(imodel) % grid % dlat) /= abs(jDirectionIncrementInDegrees) .or. &
           model(imodel) % grid % ncols /= Ni .or. &
           model(imodel) % grid % nrows /= Nj ) then

        print '(a)', 'WARNING: GRID IS DIFFERENT!'

      end if

      if ( imodel > 1 ) then

        if ( model(imodel) % grid % type /= model(1) % grid % type .or. &
             model(imodel) % grid % lon1 /= model(1) % grid % lon1 .or. &
             model(imodel) % grid % lat1 /= model(1) % grid % lat1 .or. &
             abs(model(imodel) % grid % dlon) /= abs(model(1) % grid % dlon) .or. &
             abs(model(imodel) % grid % dlat) /= abs(model(1) % grid % dlat) .or. &
             model(imodel) % grid % ncols /= model(1) % grid % ncols .or. &
             model(imodel) % grid % nrows /= model(1) % grid % nrows ) then

          print '(a)', 'WARNING: GRID IS DIFFERENT TO FIRST TIME STEP!'

        end if

      end if

    end if

  end if

  !-------------------------------------
  ! Read field into model data structure
  !-------------------------------------

  ! Point to the correct data space, allocating if necessary

  select case(paramId)
    case(3)     ! theta
      if ( .not. associated(model(imodel) % theta) ) allocate(model(imodel) % theta(nprofs,nlevels))
      values => model(imodel) % theta(:,level)
    case(31, 500069)    ! Seaice
      if ( .not. associated(model(imodel) % seaice) ) allocate(model(imodel) % seaice(nprofs))
      values => model(imodel) % seaice
    case(54, 500001)   ! p on theta levels or ph
      if ( ecmwf .or. trim(typeOfLevel) == 'generalVerticalLayer' ) then
        if ( .not. associated(model(imodel) % p) ) allocate(model(imodel) % p(nprofs,nlevels))
        values => model(imodel) % p(:,level)
      else if ( icon .and. trim(typeOfLevel) == 'generalVertical' ) then
        if ( .not. associated(model(imodel) % ph) ) allocate(model(imodel) % ph(nprofs,nlevels+1))
        values => model(imodel) % ph(:,level)
      end if
    case(66)    ! Snow depth
      if ( .not. associated(model(imodel) % snow_depth) ) allocate(model(imodel) % snow_depth(nprofs))
      values => model(imodel) % snow_depth
    case(75, 260020, 500102)    ! Rain
      if ( .not. associated(model(imodel) % rain) ) allocate(model(imodel) % rain(nprofs,nlevels))
      values => model(imodel) % rain(:,level)
    case(76, 260021, 500103)    ! Snow
      if ( .not. associated(model(imodel) % snow) ) allocate(model(imodel) % snow(nprofs,nlevels))
      values => model(imodel) % snow(:,level)
    case(129)    ! Geopotential of surface
      if ( .not. associated(model(imodel) % zsurf) ) allocate(model(imodel) % zsurf(nprofs))
      values => model(imodel) % zsurf
    case(130, 500014)    ! T
      if ( .not. associated(model(imodel) % t) ) allocate(model(imodel) % t(nprofs,nlevels))
      values => model(imodel) % t(:,level)
    case(133, 500035)    ! q, also q2 for JMA
      if ( jma .and. trim(typeOfLevel) == 'heightAboveGround' ) then
        if ( .not. associated(model(imodel) % q2) ) allocate(model(imodel) % q2(nprofs))
        values => model(imodel) % q2
      else
        if ( .not. associated(model(imodel) % q) ) allocate(model(imodel) % q(nprofs,nlevels))
        values => model(imodel) % q(:,level)
      end if
    case(134, 152, 500000)   ! pstar, ln(pstar), pstar
      if ( .not. associated(model(imodel) % pstar) ) allocate(model(imodel) % pstar(nprofs))
      values => model(imodel) % pstar
    case(164)  ! total cloud cover (not used by RTTOV)
      if ( .not. associated(model(imodel) % total_cc) ) allocate(model(imodel) % total_cc(nprofs))
      values => model(imodel) % total_cc
    case(165, 500027)  ! u at 10m
      if ( .not. associated(model(imodel) % u10) ) allocate(model(imodel) % u10(nprofs))
      values => model(imodel) % u10
    case(166, 500029)  ! v at 10m
      if ( .not. associated(model(imodel) % v10) ) allocate(model(imodel) % v10(nprofs))
      values => model(imodel) % v10
    case(167, 500011)  ! T2
      if ( .not. associated(model(imodel) % t2) ) allocate(model(imodel) % t2(nprofs))
      values => model(imodel) % t2
    case(168, 500017)  ! Td2
      if ( .not. associated(model(imodel) % td2) ) allocate(model(imodel) % td2(nprofs))
      values => model(imodel) % td2
    case(172)    ! Land-sea mask
      if ( .not. associated(model(imodel) % lsm) ) allocate(model(imodel) % lsm(nprofs))
      values => model(imodel) % lsm
    case(186)  ! low cloud cover (not used by RTTOV)
      if ( .not. associated(model(imodel) % low_cc) ) allocate(model(imodel) % low_cc(nprofs))
      values => model(imodel) % low_cc
    case(187)  ! medium cloud cover (not used by RTTOV)
      if ( .not. associated(model(imodel) % medium_cc) ) allocate(model(imodel) % medium_cc(nprofs))
      values => model(imodel) % medium_cc
    case(188)  ! high cloud cover (not used by RTTOV)
      if ( .not. associated(model(imodel) % high_cc) ) allocate(model(imodel) % high_cc(nprofs))
      values => model(imodel) % high_cc
    case(203, 500242)    ! o3
      if ( .not. associated(model(imodel) % o3) ) allocate(model(imodel) % o3(nprofs,nlevels))
      values => model(imodel) % o3(:,level)
    case(260624)    ! o3 in mg/kg
      if ( .not. associated(model(imodel) % o3) ) allocate(model(imodel) % o3(nprofs,nlevels))
      values => model(imodel) % o3(:,level)
      ozone_mgperkg = .true.
    case(235, 500010)    ! Tskin
      if ( .not. associated(model(imodel) % tskin) ) allocate(model(imodel) % tskin(nprofs))
      values => model(imodel) % tskin
    case(246, 503056)   ! qcl
      if ( .not. associated(model(imodel) % clw) ) allocate(model(imodel) % clw(nprofs,nlevels))
      values => model(imodel) % clw(:,level)
    case(247, 503057)    ! qcf
      if ( .not. associated(model(imodel) % ciw) ) allocate(model(imodel) % ciw(nprofs,nlevels))
      values => model(imodel) % ciw(:,level)
    case(248, 260257, 500098)   ! Cloud fraction
      if ( .not. associated(model(imodel) % cfrac) ) allocate(model(imodel) % cfrac(nprofs,nlevels))
      values => model(imodel) % cfrac(:,level)
    case(3089, 500545)   ! Density
      if ( .not. associated(model(imodel) % density) ) allocate(model(imodel) % density(nprofs,nlevels))
      values => model(imodel) % density(:,level)
    case(500779, 503566)   ! CLW deff
      if ( .not. associated(model(imodel) % clw_deff) ) allocate(model(imodel) % clw_deff(nprofs,nlevels))
      values => model(imodel) % clw_deff(:,level)
    case(500781, 503568)   ! Ice deff
      if ( .not. associated(model(imodel) % ciw_deff) ) allocate(model(imodel) % ciw_deff(nprofs,nlevels))
      values => model(imodel) % ciw_deff(:,level)
    case(210001)   ! CAMS sea salt bin 1
      if ( .not. associated(model(imodel) % cams_sea_salt1) ) allocate(model(imodel) % cams_sea_salt1(nprofs,nlevels))
      values => model(imodel) % cams_sea_salt1(:,level)
    case(210002)   ! CAMS sea salt bin 2
      if ( .not. associated(model(imodel) % cams_sea_salt2) ) allocate(model(imodel) % cams_sea_salt2(nprofs,nlevels))
      values => model(imodel) % cams_sea_salt2(:,level)
    case(210003)   ! CAMS sea salt bin 3
      if ( .not. associated(model(imodel) % cams_sea_salt3) ) allocate(model(imodel) % cams_sea_salt3(nprofs,nlevels))
      values => model(imodel) % cams_sea_salt3(:,level)
    case(210004)   ! CAMS dust bin 1
      if ( .not. associated(model(imodel) % cams_dust1) ) allocate(model(imodel) % cams_dust1(nprofs,nlevels))
      values => model(imodel) % cams_dust1(:,level)
    case(210005)   ! CAMS dust bin 2
      if ( .not. associated(model(imodel) % cams_dust2) ) allocate(model(imodel) % cams_dust2(nprofs,nlevels))
      values => model(imodel) % cams_dust2(:,level)
    case(210006)   ! CAMS dust bin 3
      if ( .not. associated(model(imodel) % cams_dust3) ) allocate(model(imodel) % cams_dust3(nprofs,nlevels))
      values => model(imodel) % cams_dust3(:,level)
    case(210007)   ! CAMS hydrophilic organic matter
      if ( .not. associated(model(imodel) % cams_hphil_omat) ) allocate(model(imodel) % cams_hphil_omat(nprofs,nlevels))
      values => model(imodel) % cams_hphil_omat(:,level)
    case(210010)   ! CAMS hydrophobic black carbon
      if ( .not. associated(model(imodel) % cams_hphob_bcar) ) allocate(model(imodel) % cams_hphob_bcar(nprofs,nlevels))
      values => model(imodel) % cams_hphob_bcar(:,level)
    case(210011)   ! CAMS sulphate
      if ( .not. associated(model(imodel) % cams_sulphate) ) allocate(model(imodel) % cams_sulphate(nprofs,nlevels))
      values => model(imodel) % cams_sulphate(:,level)
    case default
      print '(a)', 'Warning: Not set up to read this field'
      cycle
  end select

  call grib_get(message_id, 'values', values)
  got_field(field_index) = .true.

  !-----------------
  ! Unit conversions
  !-----------------

  if ( paramId == 129 ) then
    model(imodel) % zsurf = model(imodel) % zsurf / gravity  ! Geopotential -> surface elevation
  else if ( paramId == 152 ) then
    model(imodel) % pstar = exp(model(imodel) % pstar)       ! ln(psurf) -> psurf
  else if ( paramId == 260257 .or. paramId == 500098 ) then
    ! ICON cloud cover is in %
    model(imodel) % cfrac(:,level) = model(imodel) % cfrac(:,level) * 0.01_real64
  else if ( paramId == 500779 .or. paramId == 503566 ) then
    ! Convert ICON cloud liquid particle radius in m to diameter in microns
    model(imodel) % clw_deff(:,level) = model(imodel) % clw_deff(:,level) * 2.e6_real64
  else if ( paramId == 500781 .or. paramId == 503568 ) then
    ! Convert ICON cloud ice particle radius in m to diameter in microns
    model(imodel) % ciw_deff(:,level) = model(imodel) % ciw_deff(:,level) * 2.e6_real64
  end if

  call grib_release(message_id)

end do

if ( temporal_data ) then
  model_ntimes = imodel
else
  ! Keep only data for the first time step
  do i = 2, imodel
    call radsim_dealloc_model(model(i))
  end do
  model_ntimes = 1
end if

call grib_close_file(file_id)

!-------------------------------------------------------------------------------
! 2b. For ICON data, read ancillary GRIB file
!-------------------------------------------------------------------------------

if ( icon ) then

  ! Ancillary file is ICON invar (==invariant) files. These additional
  ! data do not change between time steps and the required data are only stored
  ! on one level. This simplifies the reading of the files, but the fields that
  ! are read in must be replicated throughout the model(:) structure array.

  print '(a)', 'Reading GRIB messages from ' // trim(file_name_ancil)

  call grib_open_file(file_id, trim(file_name_ancil), 'r')

  nmessage = 0

  do

    !----------------
    ! Get new message
    !----------------

    call grib_new_from_file(file_id, message_id, status)

    ! Exit if no message found

    if ( status == GRIB_END_OF_FILE ) then
      print '(a)', 'Finished reading: EOF'
      exit
    else if ( status /= GRIB_SUCCESS ) then
      message = 'Could not read GRIB message'
      call radsim_error_report(message, status_error)
    end if

    nmessage = nmessage + 1

    !---------------
    ! Identify field
    !---------------

    call grib_get(message_id, 'centre', centre)
    call grib_get(message_id, 'paramId', paramId)

    if ( output_mode >= output_verbose ) then
      print '(a)',    '---------------'
      print '(2a)',   'centre = ', centre
      print '(a,i0)', 'paramId = ', paramId
    end if

    ! Check to see if we need it

    field_index = 0
    do j = 1, size(field_list, dim = 2)
      do i = 1, size(field_list, dim = 1)
        if ( field_list(i,j) == -1 ) cycle
        if ( paramId == field_list(i,j) ) then
          field_index = i
        end if
      end do
    end do

    if ( field_index == 0 ) then
      if ( output_mode >= output_verbose ) print '(a)', 'Skipping this field'
      cycle
    else
      if ( output_mode >= output_verbose ) then
        print '(2a)', 'Getting data for: ', field_names_rttov(field_index)
      end if
    end if

    !----------------------
    ! Number of data values
    !----------------------

    call grib_get(message_id, 'numberOfDataPoints', numberOfDataPoints)
    call grib_get(message_id, 'level', level)
    call grib_get(message_id, 'missingValue', missingValue)

    ! Ensure ancillary file is compatible with main file
    if ( numberOfDataPoints /= nprofs ) then
      call radsim_error_report( &
        'Ancillary GRIB file has different numberOfDataPoints to main file', status_error)
    end if

    call radsim_grib_paramid_name(paramid, param_name)

    if ( output_mode >= output_verbose ) then
      print '(a,i0)', 'numberOfDataPoints = ', numberOfDataPoints
      print '(a,i0)', 'level = ', level
    else if ( output_mode == output_default ) then
      print '(a,i0,a,i0,a)', 'Getting level ', level, ' data with paramId: ', &
        paramId, ' (' // trim(param_name) // ')'
    end if

    !-------------------------------------
    ! Read field into model data structure
    !-------------------------------------

    ! In this case we allocate values instead of pointing to the model data
    ! because we need to copy the data into the model(:) structure for all time
    ! steps (and also the grid % lat/lon are real32 while values(:) is real64)

    nullify(values)
    select case(paramId)
      ! 172, 500054 = land-sea mask
      ! 500007 = land surface height
      ! 502309/250003 = latitude of cell centre (CLAT)
      ! 502310/250004 = longitude of cell centre (CLON)
      case(172, 500054, 500007, 502309, 502310, 250003, 250004)
        allocate(values(nprofs))
      case default
        print '(a)', 'Warning: Not set up to read this field'
        cycle
    end select

    call grib_get(message_id, 'values', values)
    got_field(field_index) = .true.

    !--------------------------------------------
    ! Unit conversions and store for nmodel_times
    !--------------------------------------------

    do imodel = 1, model_ntimes
      select case(paramId)
        case(172, 500054)
          if ( .not. associated(model(imodel) % lsm) ) allocate(model(imodel) % lsm(nprofs))
          model(imodel) % lsm = values
        case(500007)
          if ( .not. associated(model(imodel) % zsurf) ) allocate(model(imodel) % zsurf(nprofs))
          model(imodel) % zsurf = values
        case(502309, 250003)
          if ( .not. associated(model(imodel) % grid % lat) ) allocate(model(imodel) % grid % lat(nprofs))
          model(imodel) % grid % lat = values
        case(502310, 250004)
          if ( .not. associated(model(imodel) % grid % lon) ) allocate(model(imodel) % grid % lon(nprofs))
          model(imodel) % grid % lon = values
      end select
    end do
    if ( associated(values) ) deallocate(values)

    call grib_release(message_id)

  end do

  call grib_close_file(file_id)

end if ! ICON ancillary file

!-------------------------------------------------------------------------------
! 3. Compute pressure levels if we don't have them yet
!-------------------------------------------------------------------------------

! In the ECMWF model, half-level pressures are a linear function of surface
! pressure, with the coefficient values stored in the pv key for
! each level of each multi-level field. We already read in the pv key (if
! it exists) from the first multi-level field encountered during GRIB ingest.
! Here we compute the pressure levels using these coefficients and pstar.
! By doing this at the end of the ingest no longer impose restrictions on
! the order of fields in the GRIB file (which was the case in previous RadSim
! versions).

! Similar considerations apply to HARMONIE and JMA.

if ( (ecmwf .or. harmonie .or. jma) .and. &
     .not. associated(model(1) % p) .and. &
     associated(model(1) % pstar) ) then

  do imodel = 1, model_ntimes

    allocate(model(imodel) % p(nprofs,model_nlevels))
    allocate(model(imodel) % ph(nprofs,model_nlevels+1))

    model_name = 'ecmwf'
    if ( harmonie ) then
      model_name = 'harmonie'
    else if ( jma ) then
      model_name = 'jma'
    end if

    if ( allocated(pv) ) then
      call radsim_calc_plevels( &
        trim(model_name),      &
        model(imodel) % pstar, &
        model(imodel) % p,     &
        model(imodel) % ph,    &
        pv(1:model_nlevels+1), &
        pv(model_nlevels+2:)   )
    else
      call radsim_calc_plevels( &
        trim(model_name),      &
        model(imodel) % pstar, &
        model(imodel) % p,     &
        model(imodel) % ph)
    end if

    got_field(field_rttov_p) = .true.

  end do

end if
if ( allocated(pv) ) deallocate(pv)

!-------------------------------------------------------------------------------
! 4. Derivations
!-------------------------------------------------------------------------------

do imodel = 1, model_ntimes
  if ( associated(model(imodel) % p) ) then
    model(imodel) % nprofs = size(model(imodel) % p, dim=1)
    model(imodel) % nlevels = size(model(imodel) % p, dim=2)
  end if

  ! LSM from seaice fraction if not present

  if ( .not. associated(model(imodel) % lsm) .and. &
       associated(model(imodel) % seaice) ) then
    print '(a)', 'No LSM: Deriving LSM from seaice fraction field'
    allocate(model(imodel) % lsm(nprofs))
    where(model(imodel) % seaice == model(imodel) % rmdi)
      model(imodel) % lsm = 1.0
    elsewhere
      model(imodel) % lsm = 0.0
    end where
  end if
end do

!-------------------------------------------------------------------------------
! 5. Check fields
!-------------------------------------------------------------------------------

do i = 1, size(got_field)
  if ( .not. got_field(i) ) then
    call radsim_error_report( &
      'No data in GRIB file for required RTTOV field: ' // field_names_rttov(i), &
      status_error)
  end if
end do

end subroutine radsim_read_grib
