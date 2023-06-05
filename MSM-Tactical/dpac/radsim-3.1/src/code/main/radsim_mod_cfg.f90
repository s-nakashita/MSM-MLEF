!-------------------------------------------------------------------------------
! Description:
!
!   Module for declaring configuration namelist options.
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

module radsim_mod_cfg

use radsim_mod_constants, only : &
  real64

implicit none

!-------------------------------------------------------------------------------
! Namelist variables
!-------------------------------------------------------------------------------

! Location and type of input and output files
character(len=400) :: obs_datafile
character(len=400) :: read_footprint_file
character(len=400) :: write_footprint_file
character(len=400) :: model_datafile
character(len=400) :: model_ancil_datafile
character(len=400) :: model_ancil2_datafile
character(len=400) :: rttov_coeffs_dir
character(len=400) :: rttov_hydrotable_dir
character(len=400) :: rttov_sccld_dir
character(len=400) :: rttov_mfasis_cld_dir
character(len=400) :: rttov_scaer_dir
character(len=400) :: rttov_pccoeffs_dir
character(len=400) :: rttov_htfrtc_coeffs_dir
character(len=400) :: rttov_coeff_file
character(len=400) :: rttov_hydrotable_file
character(len=400) :: rttov_sccld_file
character(len=400) :: rttov_mfasis_cld_file
character(len=400) :: rttov_scaer_file
character(len=400) :: rttov_pccoeff_file
character(len=400) :: rttov_htfrtc_static_file
character(len=400) :: rttov_htfrtc_sensor_file
character(len=400) :: output_dir
character(len=400) :: output_file
character(len=400) :: emiss_atlas_dir
character(len=400) :: brdf_atlas_dir
character(len=4)   :: rttov_coeffs_type
character(len=64)  :: rttov_coeffs_options
integer :: model_filetype

! Output options
logical      :: write_profiles
logical      :: write_latlon
logical      :: write_geom_height
logical      :: write_trans
logical      :: write_tjac
logical      :: write_qjac
logical      :: write_o3jac
logical      :: write_tskinjac
logical      :: write_wind10mjac
logical      :: write_emissjac
logical      :: write_radiances
logical      :: write_emiss
logical      :: write_brdf

! General run-time options
logical      :: temporal_data
logical      :: enable_footprints
logical      :: run_scatt
logical      :: run_k
logical      :: run_mfasis
logical      :: obs_grid
logical      :: calc_geo_sat_angles
logical      :: fixed_sun_angles
logical      :: use_emiss_atlas
integer      :: emis_atlas_id
logical      :: use_brdf_atlas
logical      :: use_all_atlas_months
logical      :: ircloud_use_model_clw_deff
logical      :: ircloud_use_model_ice_deff
integer      :: output_mode
real(real64) :: seaice_threshold
real(real64) :: coast_threshold
real(real64) :: geo_sat_lat
real(real64) :: geo_sat_lon
real(real64) :: geo_sat_height
real(real64) :: zen_angle
real(real64) :: azi_angle
real(real64) :: sun_zen_angle
real(real64) :: sun_azi_angle
real(real64) :: co2_max_ppmv
real(real64) :: ssu_co2_cell_pressure(3)
real(real64) :: default_brdf_land
real(real64) :: default_brdf_seaice
real(real64) :: cads_height_assign_threshold
integer      :: max_array_size
integer      :: max_profs
integer      :: nthreads
integer      :: nprofs_per_call

! Satellite instrument
character(len=20) :: platform
character(len=20) :: inst
integer           :: satid
integer           :: channels(100000)

! RTTOV options
logical      :: mmr_snowrain
logical      :: apply_reg_limits
logical      :: clw_data
logical      :: ozone_data
logical      :: ozone_mgperkg
logical      :: co2_data
logical      :: n2o_data
logical      :: co_data
logical      :: ch4_data
logical      :: so2_data
logical      :: ir_addclouds        ! Add clouds to VIS/IR calculations
logical      :: ir_addaerosols_cams ! Add CAMS aerosols to VIS/IR calculations
logical      :: addrefrac
logical      :: plane_parallel
logical      :: do_lambertian
logical      :: lambertian_fixed_angle
logical      :: do_nlte_correction
logical      :: addsolar
logical      :: rayleigh_single_scatt
logical      :: dom_rayleigh
logical      :: addpc
logical      :: htfrtc
integer      :: npcscores
integer      :: ipcreg
integer      :: ipcbnd
integer      :: ircloud_icede_param ! Ice water content effective diameter parameterisation
integer      :: ircloud_ice_scheme  ! Cloud ice optical properties
integer      :: ircloud_clw_scheme  ! Cloud liquid optical properties
integer      :: ir_scatt_model
integer      :: vis_scatt_model
integer      :: dom_nstreams
integer      :: mw_clw_scheme       ! MW CLW absorption scheme
integer      :: fastem_version
integer      :: ir_sea_emis_model
integer      :: solar_sea_brdf_model
integer      :: interp_mode
real(real64) :: mw_clw_cloud_top
real(real64) :: rayleigh_min_pressure
real(real64) :: rayleigh_max_wavelength
real(real64) :: cldcol_threshold
real(real64) :: dom_accuracy
real(real64) :: dom_opdep_threshold
real(real64) :: cc_threshold
real(real64) :: ice_polarisation

namelist /radsim_nl/ &
  ! Files
  obs_datafile, read_footprint_file, write_footprint_file, &
  model_datafile, model_ancil_datafile, model_ancil2_datafile, &
  model_filetype, output_dir, output_file, &
  rttov_coeffs_dir, rttov_hydrotable_dir, &
  rttov_sccld_dir, rttov_mfasis_cld_dir, rttov_scaer_dir, &
  rttov_pccoeffs_dir, rttov_htfrtc_coeffs_dir, &
  rttov_coeff_file, rttov_hydrotable_file, &
  rttov_sccld_file, rttov_mfasis_cld_file, rttov_scaer_file, &
  rttov_pccoeff_file, rttov_htfrtc_static_file, rttov_htfrtc_sensor_file, &
  rttov_coeffs_type, rttov_coeffs_options, &
  emiss_atlas_dir, brdf_atlas_dir, &
  ! Output
  write_trans, write_tjac, &
  write_qjac, write_o3jac, &
  write_tskinjac, write_wind10mjac, write_emissjac, &
  write_profiles, write_latlon, write_geom_height, &
  write_radiances, &
  write_emiss, write_brdf, &
  ! General Runtime
  temporal_data, enable_footprints, &
  run_scatt, run_mfasis, &
  ircloud_use_model_clw_deff, ircloud_use_model_ice_deff, &
  output_mode, &
  seaice_threshold, coast_threshold, &
  calc_geo_sat_angles, geo_sat_height, &
  geo_sat_lat, geo_sat_lon, &
  zen_angle, azi_angle, &
  sun_zen_angle, sun_azi_angle, &
  co2_max_ppmv, ssu_co2_cell_pressure, &
  cads_height_assign_threshold, &
  default_brdf_land, default_brdf_seaice, &
  use_emiss_atlas, use_brdf_atlas, &
  emis_atlas_id, &
  use_all_atlas_months, &
  max_array_size, max_profs, &
  nthreads, nprofs_per_call, &
  ! Satellite
  platform, inst, satid, channels, &
  ! RTTOV
  apply_reg_limits, &
  mw_clw_cloud_top, clw_data, ozone_data, &
  addrefrac, plane_parallel, &
  do_lambertian, lambertian_fixed_angle, &
  do_nlte_correction, &
  addsolar, rayleigh_single_scatt, dom_rayleigh, &
  rayleigh_min_pressure, rayleigh_max_wavelength, &
  ir_addaerosols_cams, ir_addclouds, &
  ircloud_icede_param, ircloud_ice_scheme, ircloud_clw_scheme, &
  ir_scatt_model, vis_scatt_model, &
  dom_nstreams, dom_accuracy, dom_opdep_threshold, &
  mw_clw_scheme, fastem_version, ir_sea_emis_model, solar_sea_brdf_model, &
  interp_mode, cldcol_threshold, cc_threshold, ice_polarisation, &
  addpc, htfrtc, npcscores, ipcreg, ipcbnd

end module radsim_mod_cfg
