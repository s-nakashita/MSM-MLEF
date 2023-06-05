!-------------------------------------------------------------------------------
! Description:
!
!   Read the configuration namelist file.
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

subroutine radsim_read_cfg(nlfile)

use radsim_mod_cfg

use radsim_mod_io, only : &
  filetype_fieldsfile, &
  filetype_ecprof60, &
  filetype_ecprof91, &
  filetype_ecprof137, &
  filetype_grib_icon, &
  filetype_netcdf_ecmwf, &
  filetype_grib_harmonie, &
  filetype_grib_jma

use radsim_mod_constants, only : &
  status_error, &
  status_warning, &
  output_verbose

use rttov_const, only : &
  platform_name, &
  inst_name, &
  nplatforms, &
  ninst, &
  sensor_id, &
  sensor_id_ir, &
  sensor_id_mw, &
  sensor_id_hi, &
  sensor_id_po, &
  ir_scatt_dom, &
  vis_scatt_mfasis, &
  clw_scheme_opac, &
  clw_scheme_deff, &
  ice_scheme_baum

implicit none

include 'radsim_error_report.interface'

! Subroutine arguments

character(len=*), intent(in)    :: nlfile

! Local variables

integer :: i
integer :: nchans
integer :: platform_id
integer :: inst_id
logical :: exists, do_jac
character(len=160) :: message

!-------------------------------------------------------------------------------

print '(a)', 'Reading namelist configuration file ' // trim(nlfile)

!-------------------------------------------------------------------------------
! 1. Initialise namelist variables to default cfg
!-------------------------------------------------------------------------------

! Files
obs_datafile = ''
read_footprint_file = ''
write_footprint_file = ''
model_datafile = ''
model_ancil_datafile = ''
rttov_coeffs_dir = ''
rttov_hydrotable_dir = ''
rttov_sccld_dir = ''
rttov_mfasis_cld_dir = ''
rttov_scaer_dir = ''
rttov_pccoeffs_dir = ''
rttov_htfrtc_coeffs_dir = ''
rttov_coeff_file = ''
rttov_hydrotable_file = ''
rttov_sccld_file = ''
rttov_mfasis_cld_file = ''
rttov_scaer_file = ''
rttov_pccoeff_file = ''
rttov_htfrtc_static_file = ''
rttov_htfrtc_sensor_file = ''
output_dir = ''
output_file = ''
emiss_atlas_dir = ''
brdf_atlas_dir = ''
rttov_coeffs_type = '.dat'
rttov_coeffs_options = ''
model_filetype = filetype_fieldsfile

! Output options

write_profiles    = .false.
write_latlon      = .false.
write_geom_height = .false.
write_trans       = .false.
write_tjac        = .false.
write_qjac        = .false.
write_o3jac       = .false.
write_tskinjac    = .false.
write_wind10mjac  = .false.
write_emissjac    = .false.
write_radiances   = .false.
write_emiss       = .false.
write_brdf        = .false.

! General run-time options

temporal_data = .false.
enable_footprints = .false.
run_k = .false.
run_scatt = .false.
run_mfasis = .false.
ircloud_use_model_clw_deff = .false.
ircloud_use_model_ice_deff = .false.
output_mode = 1
seaice_threshold = 0.2_real64
coast_threshold = 0.2_real64
calc_geo_sat_angles = .false.
geo_sat_lat = 0.0_real64
geo_sat_lon = 0.0_real64
geo_sat_height = 35800.0_real64
zen_angle = 0.0_real64
azi_angle = 0.0_real64
sun_zen_angle = -1.0_real64
sun_azi_angle = 0.0_real64
co2_max_ppmv = -1._real64
ssu_co2_cell_pressure = -1._real64
cads_height_assign_threshold = -1._real64
default_brdf_land = -1._real64
default_brdf_seaice = -1._real64
use_emiss_atlas = .false.
use_brdf_atlas = .false.
use_all_atlas_months = .false.
emis_atlas_id = 1
max_array_size = 500
max_profs = 500000
nthreads = 1
nprofs_per_call = 5

! Satellite instrument

platform = ''
inst     = ''
satid    = -1
channels = 0

! RTTOV options

mmr_snowrain            = .false.
apply_reg_limits        = .true.
clw_data                = .true.
ozone_data              = .false.
co2_data                = .false.
n2o_data                = .false.
co_data                 = .false.
ch4_data                = .false.
so2_data                = .false.
ir_addaerosols_cams     = .false.
ir_addclouds            = .false.
addrefrac               = .true.
plane_parallel          = .false.
do_lambertian           = .false.
lambertian_fixed_angle  = .false.
do_nlte_correction      = .false.
addsolar                = .false.
rayleigh_single_scatt   = .true.
dom_rayleigh            = .false.
addpc                   = .false.
htfrtc                  = .false.
npcscores               = 300
ipcreg                  = 1
ipcbnd                  = 1
ircloud_icede_param     = 2
ircloud_ice_scheme      = 3
ircloud_clw_scheme      = 1
ir_scatt_model          = 2
vis_scatt_model         = 1
dom_nstreams            = 8
mw_clw_scheme           = 2
fastem_version          = 6
ir_sea_emis_model       = 2
solar_sea_brdf_model    = 2
interp_mode             = -1
mw_clw_cloud_top        = 322._real64
rayleigh_min_pressure   = 0.0_real64
rayleigh_max_wavelength = 2.0_real64
cldcol_threshold        = 1.E-5_real64
dom_accuracy            = 0.0_real64
dom_opdep_threshold     = 0.0_real64
cc_threshold            = 0.001_real64
ice_polarisation        = 1.4_real64

!-------------------------------------------------------------------------------
! 2. Read namelist and validate inputs
!-------------------------------------------------------------------------------

open(unit=11, file=nlfile, form='formatted')
read(11, nml=radsim_nl)
close(11)

! Switch off options incompatible with UM

if ( model_filetype == filetype_fieldsfile ) then
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for UM: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
end if

! Switch off options incompatible with ECMWF netCDF

if ( model_filetype == filetype_netcdf_ecmwf ) then
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for ECMWF netCDF: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
end if

! Switch off options incompatible with ICON

if ( model_filetype == filetype_grib_icon ) then
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for ICON: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
end if

! Switch off options incompatible with HARMONIE

if ( model_filetype == filetype_grib_harmonie ) then
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for HARMONIE: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
  if ( ozone_data ) then
    message = 'Invalid cfg option for HARMONIE: ozone_data = T.' // &
    ' Continuing with ozone_data = F...'
    call radsim_error_report(message, status_warning)
    ozone_data = .false.
  end if
  if ( write_o3jac ) then
    message = 'Invalid cfg option for HARMONIE: write_o3jac = T.' // &
    ' Continuing with write_o3jac = F...'
    call radsim_error_report(message, status_warning)
    write_o3jac = .false.
  end if
end if

! Switch off options incompatible with JMA

if ( model_filetype == filetype_grib_jma ) then
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for JMA: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
  if ( ir_addclouds ) then
    message = 'Invalid cfg option for JMA: ir_addclouds = T.' // &
    ' Continuing with ir_addclouds = F...'
    call radsim_error_report(message, status_warning)
    ir_addclouds = .false.
  end if
  if ( run_mfasis ) then
    message = 'Invalid cfg option for JMA: run_mfasis = T.' // &
    ' Continuing with run_mfasis = F...'
    call radsim_error_report(message, status_warning)
    run_mfasis = .false.
  end if
  if ( clw_data ) then
    message = 'Invalid cfg option for JMA: clw_data = T.' // &
    ' Continuing with clw_data = F...'
    call radsim_error_report(message, status_warning)
    clw_data = .false.
  end if
  if ( run_scatt ) then
    message = 'Invalid cfg option for JMA: run_scatt = T.' // &
    ' Continuing with run_scatt = F...'
    call radsim_error_report(message, status_warning)
    run_scatt = .false.
  end if
end if

! Switch off options incompatible with NWP SAF profile datasets

if ( model_filetype == filetype_ecprof60 .or. &
     model_filetype == filetype_ecprof91 .or. &
     model_filetype == filetype_ecprof137 ) then
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for NWP SAF profiles: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
  if ( temporal_data ) then
    message = 'Invalid cfg option for NWP SAF profiles: temporal_data = T.' // &
    ' Continuing with temporal_data = F...'
    call radsim_error_report(message, status_warning)
    temporal_data = .false.
  end if
  if ( enable_footprints ) then
    message = 'Invalid cfg option for NWP SAF profiles: enable_footprints = T.' // &
    ' Continuing with enable_footprints = F...'
    call radsim_error_report(message, status_warning)
    enable_footprints = .false.
  end if
end if

! Check platform is valid

platform_id = 0
do i = 1, nplatforms
  if ( platform == platform_name(i) ) then
    platform_id = i
    exit
  end if
end do
if ( platform_id == 0 ) then
  print '(a)', 'Available platforms are:'
  print *, (platform_name(i) // ' ', i = 1, nplatforms)
  write(message,'(2a)') 'Invalid platform in cfg namelist: ', platform
  call radsim_error_report( &
    message, &
    status_error)
end if

! Check instrument name is valid

inst_id = -1
do i = 0, ninst-1
  if ( inst == inst_name(i) ) then
    inst_id = i
    exit
  end if
end do
if ( inst_id == -1 ) then
  print '(a)', 'Valid instrument names are:'
  print *, (inst_name(i) // ' ', i = 0, ninst-1)
  write(message,'(2a)') 'Invalid instrument in cfg namelist: ', inst
  call radsim_error_report( &
    message, &
    status_error)
end if

! Check satellite id is valid

if ( satid <= -1 ) then
  write(message,'(a,i0)') 'Invalid satid in cfg namelist: ', satid
  call radsim_error_report( &
    message, &
    status_error)
end if

! Check data files are present

if ( model_datafile == '' ) then
  call radsim_error_report( &
    'No model data file specified in cfg namelist', &
    status_error)
end if
inquire(file=model_datafile, exist=exists)
if ( .not. exists ) then
  call radsim_error_report( &
    'Cannot find model data file ' // model_datafile, &
    status_error)
end if

if ( model_ancil_datafile /= '' ) then
  inquire(file=model_ancil_datafile, exist=exists)
  if ( .not. exists ) then
    call radsim_error_report( &
      'Cannot find ancillary model data file ' // model_ancil_datafile, &
      status_error)
  end if
else
  if ( model_filetype == filetype_grib_icon ) then
    call radsim_error_report( &
      'ICON GRIB input requires model_ancil_datafile', status_error)
  else if ( model_filetype == filetype_netcdf_ecmwf ) then
    call radsim_error_report( &
      'ECMWF netCDF input requires model_ancil_datafile', status_error)
  end if
end if

obs_grid = .false.
if ( obs_datafile /= '' ) then
  inquire(file=obs_datafile, exist=exists)
  if ( exists ) then
    obs_grid = .true.
  else
    call radsim_error_report( &
      'Cannot find obs data file ' // obs_datafile, &
      status_error)
  end if
end if

select case(rttov_coeffs_type)
  case('.dat','.H5')
    print '(a)', 'Coefficient file type selected = ' // trim(rttov_coeffs_type)
  case default
    call radsim_error_report( &
      'Invalid file type for RTTOV coeffcients: ' // trim(rttov_coeffs_type) // &
      ' (should be .dat or .H5)', &
      status_error)
end select

! Check valid obs_datafile is specified if temporal_data flag set

if ( temporal_data .and. .not. obs_grid ) then
  call radsim_error_report( &
    'An obs_datafile must be specified if temporal_data is true', &
    status_error)
end if

! Check valid obs_datafile is specified if enable_footprints flag set

if ( enable_footprints .and. .not. obs_grid ) then
  call radsim_error_report( &
    'An obs_datafile must be specified if enable_footprints is true', &
    status_error)
end if

!-------------------------------------------------------------------------------
! 3. Validate options
!-------------------------------------------------------------------------------

if ( run_mfasis ) then
  vis_scatt_model = vis_scatt_mfasis
else
  run_mfasis = (vis_scatt_model == vis_scatt_mfasis)
end if

do_jac = write_tjac .or. write_qjac .or. write_o3jac .or. &
         write_tskinjac .or. write_wind10mjac .or. write_emissjac

! Check CO2 specified is SSU CO2 cell pressures are set

if ( any(ssu_co2_cell_pressure > 0) .and. co2_max_ppmv < 0 ) then
  call radsim_error_report( &
    'co2_max_ppmv must be specified if ssu_co2_cell_pressure is specified', &
    status_error)
end if

! Switch off incompatible options

select case (sensor_id(inst_id))
  case(sensor_id_ir, sensor_id_hi)
    if ( run_scatt ) then
      message = 'Invalid cfg option for IR instrument: run_scatt = T.' // &
      ' Continuing with run_scatt = F...'
      call radsim_error_report(message, status_warning)
      run_scatt = .false.
    end if
    if ( clw_data ) then
      message = 'Invalid cfg option for IR instrument: clw_data = T.' // &
      ' Continuing with clw_data = F...'
      call radsim_error_report(message, status_warning)
      clw_data = .false.
    end if

  case(sensor_id_mw, sensor_id_po)
    if ( ozone_data ) then
      message = 'Invalid cfg option for MW instrument: ozone_data = T.' // &
      ' Continuing with ozone_data = F...'
      call radsim_error_report(message, status_warning)
      ozone_data = .false.
    end if
    if ( write_o3jac ) then
      message = 'Invalid cfg option for MW instrument: write_o3jac = T.' // &
      ' Continuing with write_o3jac = F...'
      call radsim_error_report(message, status_warning)
      write_o3jac = .false.
    end if
    if ( co2_max_ppmv > 0. ) then
      message = 'Invalid cfg option for MW instrument: co2_max_ppmv > 0.' // &
      ' Switching off co2_max_ppmv...'
      call radsim_error_report(message, status_warning)
      co2_max_ppmv = -1.
    end if
    if ( addsolar ) then
      message = 'Invalid cfg option for MW instrument: addsolar = T.' // &
      ' Continuing with addsolar = F...'
      call radsim_error_report(message, status_warning)
      addsolar = .false.
    end if
    if ( ir_addaerosols_cams ) then
      message = 'Invalid cfg option for MW instrument: ir_addaerosols_cams = T.' // &
      ' Continuing with ir_addaerosols_cams = F...'
      call radsim_error_report(message, status_warning)
      ir_addaerosols_cams = .false.
    end if
    if ( ir_addclouds ) then
      message = 'Invalid cfg option for MW instrument: ir_addclouds = T.' // &
      ' Continuing with ir_addclouds = F...'
      call radsim_error_report(message, status_warning)
      ir_addclouds = .false.
    end if
    if ( addpc ) then
      message = 'Invalid cfg option for MW instrument: addpc = T.' // &
      ' Continuing with addpc = F...'
      call radsim_error_report(message, status_warning)
      addpc = .false.
    end if
    if ( htfrtc ) then
      message = 'Invalid cfg option for MW instrument: htfrtc = T.' // &
      ' Continuing with htfrtc = F...'
      call radsim_error_report(message, status_warning)
      htfrtc = .false.
    end if
    if ( run_mfasis ) then
      message = 'Invalid cfg option for MW instrument: MFASIS enabled.' // &
      ' Continuing without MFASIS...'
      call radsim_error_report(message, status_warning)
      vis_scatt_model = 1
    end if
    if ( cads_height_assign_threshold > 0. ) then
      message = 'Invalid cfg option for MW instrument: CADS height assignment.' // &
      ' Continuing without CADS height assignment...'
      call radsim_error_report(message, status_warning)
      cads_height_assign_threshold = -1.
    end if

  case default
    write(message,'(a,i0)') 'Unknown RTTOV sensor type = ', sensor_id(inst_id)
    call radsim_error_report(message, status_warning)
end select

! If MFASIS selected, adjust other options

if ( run_mfasis ) then
  if ( .not. addsolar ) then
    message = 'MFASIS selected as vis_scatt_model, but addsolar = F.' // &
    ' Continuing with addsolar = T...'
    call radsim_error_report(message, status_warning)
    addsolar = .true.
  end if
  if ( .not. ir_addclouds ) then
    message = 'MFASIS selected as vis_scatt_model, but ir_addclouds = F.' // &
    ' Continuing with ir_addclouds = T...'
    call radsim_error_report(message, status_warning)
    ir_addclouds = .true.
  end if
  if ( ircloud_ice_scheme /= ice_scheme_baum ) then
    message = 'MFASIS selected as vis_scatt_model, but ircloud_ice_scheme /= 1.' // &
    ' Continuing with ircloud_ice_scheme = 1...'
    call radsim_error_report(message, status_warning)
    ircloud_ice_scheme = ice_scheme_baum
  end if
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option for MFASIS: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
  if ( addpc ) then
    message = 'MFASIS selected as vis_scatt_model, but addpc = T.' // &
    ' Continuing with addpc = F...'
    call radsim_error_report(message, status_warning)
    addpc = .false.
  end if
  if ( htfrtc ) then
    message = 'MFASIS selected as vis_scatt_model, but htfrtc = T.' // &
    ' Continuing with htfrtc = F...'
    call radsim_error_report(message, status_warning)
    htfrtc = .false.
  end if
end if

! Check options for PC compatibility

if ( addpc .and. htfrtc ) then
  message = 'Invalid cfg options: addpc = T and htfrtc = T.' // &
  ' Continuing with addpc = F...'
  call radsim_error_report(message, status_warning)
  addpc = .false.
end if
if ( addpc .or. htfrtc ) then
  if ( addsolar ) then
    message = 'Invalid cfg option with PC simulations: addsolar = T.' // &
    ' Continuing with addsolar = F...'
    call radsim_error_report(message, status_warning)
    addsolar = .false.
  end if
  if ( ir_addaerosols_cams ) then
    message = 'Invalid cfg option with PC simulations: ir_addaerosols_cams = T.' // &
    ' Continuing with ir_addaerosols_cams = F...'
    call radsim_error_report(message, status_warning)
    ir_addaerosols_cams = .false.
  end if
  if ( ir_addclouds ) then
    message = 'Invalid cfg option with PC simulations: ir_addclouds = T.' // &
    ' Continuing with ir_addclouds = F...'
    call radsim_error_report(message, status_warning)
    ir_addclouds = .false.
  end if
  if ( write_emissjac ) then
    message = 'Invalid cfg option with PC simulations: write_emissjac = T.' // &
    ' Continuing with write_emissjac = F...'
    call radsim_error_report(message, status_warning)
    write_emissjac = .false.
  end if
  if ( cads_height_assign_threshold > 0. ) then
    message = 'Invalid cfg option for PC simulations: CADS height assignment.' // &
    ' Continuing without CADS height assignment...'
    call radsim_error_report(message, status_warning)
    cads_height_assign_threshold = -1.
  end if
end if
if ( htfrtc ) then
  if ( write_geom_height ) then
    message = 'Invalid cfg option with HTFRTC simulations: write_geom_height = T.' // &
    ' Continuing with write_geom_height = F...'
    call radsim_error_report(message, status_warning)
    write_geom_height = .false.
  end if
end if

! Check use model Deff flags

if ( ir_addclouds ) then
  if ( ircloud_use_model_clw_deff .and. ircloud_clw_scheme /= clw_scheme_deff ) then
    message = 'Use of model cloud liquid Deff, but RTTOV CLW Deff scheme not selected.' // &
    ' Continuing with CLW Deff scheme for water clouds...'
    call radsim_error_report(message, status_warning)
    ircloud_clw_scheme = clw_scheme_deff
  end if
  if ( ircloud_use_model_ice_deff .and. ircloud_ice_scheme /= ice_scheme_baum ) then
    message = 'Use of model cloud ice Deff, but cloud ice Baum scheme not selected.' // &
    ' Continuing with Baum scheme for ice clouds...'
    call radsim_error_report(message, status_warning)
    ircloud_ice_scheme = ice_scheme_baum
  end if
end if


! Switch off BRDF atlas and BRDF output if addsolar is false

if ( use_brdf_atlas .and. .not. addsolar ) then
  message = 'Invalid cfg options: addsolar = F and use_brdf_atlas = T.' // &
  ' Continuing with use_brdf_atlas = F...'
  call radsim_error_report(message, status_warning)
  use_brdf_atlas = .false.
end if
if ( write_brdf .and. .not. addsolar ) then
  message = 'Invalid cfg options: addsolar = F and write_brdf = T.' // &
  ' Continuing with write_brdf = F...'
  call radsim_error_report(message, status_warning)
  write_brdf = .false.
end if

! Check the use_all_atlas_months option

if ( use_all_atlas_months ) then
  if ( .not. (use_emiss_atlas .or. use_brdf_atlas) ) then
    message = 'Invalid cfg options: use_all_atlas_months = T only valid with emissivity/BRDF atlases.' // &
    ' Continuing with use_all_atlas_months = F...'
    call radsim_error_report(message, status_warning)
    use_all_atlas_months = .false.
  end if
  if ( model_filetype < 2 .or. model_filetype > 4 ) then
    message = 'Invalid cfg options: use_all_atlas_months = T only valid with NWP SAF profile datasets.' // &
    ' Continuing with use_all_atlas_months = F...'
    call radsim_error_report(message, status_warning)
    use_all_atlas_months = .false.
  end if
end if

! Switch off transmittance output with RTTOV-SCATT and PC models

if ( run_scatt .and. write_trans ) then
  call radsim_error_report( &
    'Cannot obtain transmittances from RTTOV-SCATT.' // &
    ' Switching off transmittance output...', &
    status_warning)
  write_trans = .false.
end if

if ( (addpc .or. htfrtc) .and. write_trans ) then
  call radsim_error_report( &
    'Cannot obtain transmittances from PC-RTTOV or HTFRTC.' // &
    ' Switching off transmittance output...', &
    status_warning)
  write_trans = .false.
end if

! Switch off emissivity output with PC models

if ( (addpc .or. htfrtc)  .and. write_emiss ) then
  call radsim_error_report( &
    'Cannot obtain emissivities from PC-RTTOV or HTFRTC.' // &
    ' Switching off emissivity output...', &
    status_warning)
  write_emiss = .false.
end if

! Additional checks on CADS height assignment output compatibility

if ( cads_height_assign_threshold > 0. ) then
  if ( (ir_addclouds .or. ir_addaerosols_cams) .and. ir_scatt_model == ir_scatt_dom ) then
    message = 'Invalid cfg option for IR scattering DOM solver: CADS height assignment.' // &
    ' Continuing without CADS height assignment...'
    call radsim_error_report(message, status_warning)
    cads_height_assign_threshold = -1.
  end if

  if ( enable_footprints ) then
    message = 'Invalid cfg option for footprint simulation: CADS height assignment.' // &
    ' Continuing without CADS height assignment...'
    call radsim_error_report(message, status_warning)
    cads_height_assign_threshold = -1.
  end if
end if

! Output option checks

if ( run_scatt .and. do_jac ) then
  call radsim_error_report( &
    'Cannot get Jacobians from MW scattering code.' // &
    ' Switching off Jacobians...', &
    status_warning)
  write_tjac = .false.
  write_qjac = .false.
  write_o3jac = .false.
  write_tskinjac = .false.
  write_wind10mjac = .false.
  write_emissjac = .false.
end if

if ( do_jac ) then
  if ( write_o3jac .and. .not. ozone_data ) then
    call radsim_error_report( &
      'Invalid set up: Ozone Jacobian requested without ozone data.' // &
      ' Switching on ozone data...', &
      status_warning)
    ozone_data = .true.
  end if
  print '(a)', 'Jacobian output requested: switching on K code'
  run_k = .true.
end if

if ( enable_footprints ) then
  if ( do_jac ) then
    call radsim_error_report( &
      'Cannot get Jacobians with footprint simulation.' // &
      ' Switching off Jacobians...', &
      status_warning)
    write_tjac = .false.
    write_qjac = .false.
    write_o3jac = .false.
    write_tskinjac = .false.
    write_wind10mjac = .false.
    write_emissjac = .false.
  end if
  if ( write_profiles ) then
    call radsim_error_report( &
      'Cannot output profiles from footprint simulations.' // &
      ' Switching off profile output...', &
      status_warning)
    write_profiles = .false.
  end if
  if ( write_latlon ) then
    ! Note that lat/lon are always written out with an obs data file, but we
    ! don't want the write_latlon option to be true
    write_latlon = .false.
  end if
  if ( write_geom_height ) then
    call radsim_error_report( &
      'Cannot obtain geometric heights from footprint simulations.' // &
      ' Switching off geometric height output...', &
      status_warning)
    write_geom_height = .false.
  end if
  if ( write_trans ) then
    call radsim_error_report( &
      'Cannot obtain transmittances from footprint simulations.' // &
      ' Switching off transmittance output...', &
      status_warning)
    write_trans = .false.
  end if
  if ( write_emiss ) then
    call radsim_error_report( &
      'Cannot obtain emissivities from footprint simulations.' // &
      ' Switching off emissivity output...', &
      status_warning)
    write_emiss = .false.
  end if
  if ( write_brdf ) then
    call radsim_error_report( &
      'Cannot obtain BRDFs from footprint simulations.' // &
      ' Switching off BRDF output...', &
      status_warning)
    write_brdf = .false.
  end if
  if ( trim(read_footprint_file)  /= '' .and. &
       trim(write_footprint_file) /= '' ) then
    call radsim_error_report( &
      'read_footprint_file and write_footprint_file cannot be enabled together.' // &
      ' Switching off writing to footprint data file...', &
      status_warning)
    write_footprint_file = ''
  end if
else ! not enable_footprints
  if ( trim(read_footprint_file) /= '' ) then
    call radsim_error_report( &
      'read_footprint_file is set, but enable_footprints is false.' // &
      ' Switching off reading of footprint data file...', &
      status_warning)
    read_footprint_file = ''
  end if
  if ( trim(write_footprint_file) /= '' ) then
    call radsim_error_report( &
      'write_footprint_file is set, but enable_footprints is false.' // &
      ' Switching off writing to footprint data file...', &
      status_warning)
    write_footprint_file = ''
  end if
end if

! Check that seaice threshold is in the correct range

if ( seaice_threshold < 0.0 .or. seaice_threshold > 1.0 ) then
  write(message,'(a,f8.3,a)') &
    'Invalid setup: Seaice threshold ', seaice_threshold, &
    ' is outside the range 0-1. Continuing anyway...'
  call radsim_error_report(message, status_warning)
end if

! Check that coastline threshold is in the correct range

if ( coast_threshold < 0.0 .or. coast_threshold > 0.5 ) then
  write(message,'(a,f8.3,a)') &
    'Invalid setup: Coast threshold ', coast_threshold, &
    ' is outside the range 0-0.5. Continuing anyway...'
  call radsim_error_report(message, status_warning)
end if

! GEO satellite parameters

if ( calc_geo_sat_angles ) then
  if ( geo_sat_lat < -90.0 .or. geo_sat_lat > 90.0 ) then
    write(message,'(a)') &
      'Invalid setup: geo_sat_lat should be in the range [-90,+90] degrees'
    call radsim_error_report(message, status_error)
  end if

  geo_sat_lon = modulo(geo_sat_lon, 360._real64)

  if ( geo_sat_height < 10000.0 .or. geo_sat_height > 50000.0 ) then
    write(message,'(a)') &
      'Invalid setup: check geo_sat_height, geostationary altitude in km'
    call radsim_error_report(message, status_error)
  end if
end if

! View and sun angles

if ( zen_angle < 0.0 .or. zen_angle > 90.0 ) then
  write(message,'(a)') &
    'Invalid setup: Zenith angle should be in the range 0-90 degrees'
  call radsim_error_report(message, status_error)
else if ( obs_grid .and. zen_angle /= 0.0 ) then
  write(message,'(a)') &
    'Zenith angle setting will be ignored if an obs datafile is specified'
  call radsim_error_report(message, status_warning)
end if

fixed_sun_angles = (sun_zen_angle >= 0.0)

! Channels

nchans = count(channels > 0)
if ( nchans > 0 ) then
  channels(1:nchans) = pack(channels, channels > 0)
end if

end subroutine radsim_read_cfg
