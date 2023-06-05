!-------------------------------------------------------------------------------
! Description:
!
!   Print the contents of the configuration structure.
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

subroutine radsim_print_cfg

use radsim_mod_cfg

use rttov_const, only : &
  clw_scheme_deff, &
  ice_scheme_baum

implicit none

integer :: nchans
character(len=23) :: header(3)
character(len=23) :: divider

divider   = '-----------------------'
header(1) = '======================='
header(2) = 'radsim configuration'
header(3) = header(1)

print '(a)', header
print '(a)', 'Input, output files:'
print '(a35,a)',  'obs_datafile = ', trim(obs_datafile)
if ( enable_footprints ) then
  if ( trim(read_footprint_file) /= '' ) then
    print '(a35,a)',  'read_footprint_file = ', trim(read_footprint_file)
  end if
  if ( trim(write_footprint_file) /= '' ) then
    print '(a35,a)',  'write_footprint_file = ', trim(write_footprint_file)
  end if
end if
print '(a35,i0)', 'model_filetype = ', model_filetype
print '(a35,a)',  'model_datafile = ', trim(model_datafile)
if ( trim(model_ancil_datafile) /= '' ) then
  print '(a35,a)',  'model_ancil_datafile = ', trim(model_ancil_datafile)
end if
if ( trim(model_ancil2_datafile) /= '' ) then
  print '(a35,a)',  'model_ancil2_datafile = ', trim(model_ancil2_datafile)
end if
if ( .not. htfrtc ) then
  print '(a35,a)',  'rttov_coeffs_dir = ', trim(rttov_coeffs_dir)
  if ( trim(rttov_coeff_file) /= '' ) then
    print '(a35,a)',  'rttov_coeff_file = ', trim(rttov_coeff_file)
  else
    if ( trim(rttov_coeffs_type) /= '' ) &
      print '(a35,a)',  'rttov_coeffs_type = ', trim(rttov_coeffs_type)
    if ( trim(rttov_coeffs_options) /= '' ) &
      print '(a35,a)',  'rttov_coeffs_options = ', trim(rttov_coeffs_options)
  end if
  if ( run_scatt ) then
    print '(a35,a)',  'rttov_hydrotable_dir = ', trim(rttov_hydrotable_dir)
    if ( trim(rttov_hydrotable_file) /= '' ) &
      print '(a35,a)',  'rttov_hydrotable_file = ', trim(rttov_hydrotable_file)
  end if
  if ( ir_addclouds ) then
    print '(a35,a)',  'rttov_sccld_dir = ', trim(rttov_sccld_dir)
    if ( trim(rttov_sccld_file) /= '' ) &
      print '(a35,a)',  'rttov_sccld_file = ', trim(rttov_sccld_file)
    if ( run_mfasis ) then
      print '(a35,a)',  'rttov_mfasis_cld_dir = ', trim(rttov_mfasis_cld_dir)
      if ( trim(rttov_mfasis_cld_file) /= '' ) &
        print '(a35,a)',  'rttov_mfasis_cld_file = ', trim(rttov_mfasis_cld_file)
    end if
  end if
  if ( ir_addaerosols_cams ) then
    print '(a35,a)',  'rttov_scaer_dir = ', trim(rttov_scaer_dir)
    if ( trim(rttov_scaer_file) /= '' ) &
      print '(a35,a)',  'rttov_scaer_file = ', trim(rttov_scaer_file)
  end if
  if ( addpc ) then
    print '(a35,a)',  'rttov_pccoeffs_dir = ', trim(rttov_pccoeffs_dir)
    if ( trim(rttov_pccoeff_file) /= '' ) &
      print '(a35,a)',  'rttov_pccoeff_file = ', trim(rttov_pccoeff_file)
  end if
else
  print '(a35,a)',  'rttov_htfrtc_coeffs_dir = ', trim(rttov_htfrtc_coeffs_dir)
  if ( trim(rttov_htfrtc_static_file) /= '' ) &
    print '(a35,a)',  'rttov_htfrtc_static_file = ', trim(rttov_htfrtc_static_file)
  if ( trim(rttov_htfrtc_sensor_file) /= '' ) &
    print '(a35,a)',  'rttov_htfrtc_sensor_file = ', trim(rttov_htfrtc_sensor_file)
end if
print '(a35,a)',  'output_dir = ', trim(output_dir)
if ( trim(output_file) /= '' ) &
  print '(a35,a)',  'output_file = ', trim(output_file)
print '(a)', divider
print '(a)', 'Satellite instrument:'
print '(a35,a)',  'platform = ', trim(platform)
print '(a35,a)',  'inst = ', trim(inst)
print '(a35,i0)', 'satid = ', satid
nchans = count(channels>0)
print '(a35,l1)', 'user-channels = ', nchans>0
if ( nchans>0 ) then
  print '(16i6)', channels(1:nchans)
end if
print '(a)', divider
print '(a)', 'General runtime options:'
print '(a35,l1)', 'temporal_data = ', temporal_data
print '(a35,l1)', 'enable_footprints = ', enable_footprints
print '(a35,l1)', 'run_scatt = ', run_scatt
print '(a35,l1)', 'run_mfasis = ', run_mfasis
print '(a35,l1)', 'run_k = ', run_k
if ( ir_addclouds .and. ircloud_clw_scheme == clw_scheme_deff ) then
  print '(a35,l1)', 'ircloud_use_model_clw_deff = ', ircloud_use_model_clw_deff
end if
if ( ir_addclouds .and. ircloud_ice_scheme == ice_scheme_baum ) then
  print '(a35,l1)', 'ircloud_use_model_ice_deff = ', ircloud_use_model_ice_deff
end if
print '(a35,f6.2)', 'seaice_threshold = ', seaice_threshold
print '(a35,f6.2)', 'coast_threshold = ', coast_threshold
print '(a35,f8.2)', 'zen_angle = ', zen_angle
print '(a35,f8.2)', 'azi_angle = ', azi_angle
print '(a35,l1)', 'calc_geo_sat_angles = ', calc_geo_sat_angles
if ( calc_geo_sat_angles ) then
  print '(a35,f8.2)', 'geo_sat_lat = ', geo_sat_lat
  print '(a35,f8.2)', 'geo_sat_lon = ', geo_sat_lon
  print '(a35,f8.2)', 'geo_sat_height = ', geo_sat_height
end if
if ( addsolar ) then
  print '(a35,l1)', 'fixed_sun_angles = ', fixed_sun_angles
  if ( fixed_sun_angles ) then
    print '(a35,f8.2)', 'sun_zen_angle = ', sun_zen_angle
    print '(a35,f8.2)', 'sun_azi_angle = ', sun_azi_angle
  end if
end if
print '(a35,f6.2)', 'co2_max_ppmv = ', co2_max_ppmv
if ( any(ssu_co2_cell_pressure > 0.) ) then
  print '(a35,3f7.2)', 'ssu_co2_cell_pressure = ', ssu_co2_cell_pressure
end if
print '(a35,f7.3)', 'cads_height_assign_threshold = ', cads_height_assign_threshold
print '(a35,l1)', 'obs_grid = ', obs_grid
print '(a35,l1)', 'use_emiss_atlas = ', use_emiss_atlas
if ( use_emiss_atlas ) then
  print '(a35,a)', 'emiss_atlas_dir = ', trim(emiss_atlas_dir)
  print '(a35,i0)', 'emis_atlas_id = ', emis_atlas_id
end if
if ( addsolar ) then
  print '(a35,l1)', 'use_brdf_atlas = ', use_brdf_atlas
  if ( use_brdf_atlas ) then
    print '(a35,a)', 'brdf_atlas_dir = ', trim(brdf_atlas_dir)
  end if
  print '(a35,f6.2)', 'default_brdf_land = ', default_brdf_land
  print '(a35,f6.2)', 'default_brdf_seaice = ', default_brdf_seaice
end if
if ( use_emiss_atlas .or. (addsolar .and. use_brdf_atlas) ) then
  print '(a35,l1)', 'use_all_atlas_months = ', use_all_atlas_months
end if
print '(a35,i0)', 'max_array_size = ', max_array_size
print '(a35,i0)', 'max_profs = ', max_profs
print '(a35,i0)', 'nthreads = ', nthreads
print '(a35,i0)', 'nprofs_per_call = ', nprofs_per_call
print '(a)', divider
print '(a)', 'Output options:'
print '(a35,l1)', 'write_profiles = ', write_profiles
print '(a35,l1)', 'write_latlon = ', write_latlon
print '(a35,l1)', 'write_geom_height = ', write_geom_height
print '(a35,l1)', 'write_radiances = ', write_radiances
print '(a35,l1)', 'write_trans = ', write_trans
print '(a35,l1)', 'write_tjac = ', write_tjac
print '(a35,l1)', 'write_qjac = ', write_qjac
print '(a35,l1)', 'write_o3jac = ', write_o3jac
print '(a35,l1)', 'write_tskinjac = ', write_tskinjac
print '(a35,l1)', 'write_wind10mjac = ', write_wind10mjac
print '(a35,l1)', 'write_emissjac = ', write_emissjac
print '(a35,l1)', 'write_emiss = ', write_emiss
if ( addsolar ) then
  print '(a35,l1)', 'write_brdf = ', write_brdf
end if
print '(a)', divider
print '(a)', 'RTTOV options:'
print '(a35,l1)', 'clw_data = ', clw_data
if ( clw_data ) then
  print '(a35,i0)', 'mw_clw_scheme = ', mw_clw_scheme
  print '(a35,f8.4)', 'mw_clw_cloud_top = ', mw_clw_cloud_top
end if
print '(a35,l1)', 'ozone_data = ', ozone_data
print '(a35,l1)', 'co2_data = ', co2_data .or. (co2_max_ppmv > 0.)
print '(a35,l1)', 'n2o_data = ', n2o_data
print '(a35,l1)', 'co_data = ', co_data
print '(a35,l1)', 'ch4_data = ', ch4_data
print '(a35,l1)', 'so2_data = ', so2_data
print '(a35,l1)', 'apply_reg_limits = ', apply_reg_limits
print '(a35,l1)', 'addrefrac = ', addrefrac
print '(a35,l1)', 'plane_parallel = ', plane_parallel
print '(a35,l1)', 'do_nlte_correction = ', do_nlte_correction
print '(a35,i0)', 'interp_mode = ', interp_mode
print '(a35,l1)', 'addsolar = ', addsolar
if ( addsolar ) then
  print '(a35,l1)', 'rayleigh_single_scatt = ', rayleigh_single_scatt
  print '(a35,g12.4)', 'rayleigh_max_wavelength = ', rayleigh_max_wavelength
  print '(a35,g12.4)', 'rayleigh_min_pressure = ', rayleigh_min_pressure
end if
print '(a35,l1)', 'ir_addaerosols_cams = ', ir_addaerosols_cams
print '(a35,l1)', 'ir_addclouds = ', ir_addclouds
if ( ir_addclouds ) then
  print '(a35,i0)', 'ircloud_clw_scheme = ', ircloud_clw_scheme
  print '(a35,i0)', 'ircloud_ice_scheme = ', ircloud_ice_scheme
  if ( ircloud_ice_scheme == ice_scheme_baum ) then
    print '(a35,i0)', 'ircloud_icede_param = ', ircloud_icede_param
  end if
  print '(a35,g12.4)', 'cldcol_threshold = ', cldcol_threshold
end if
if ( ir_addaerosols_cams .or. ir_addclouds ) then
  print '(a35,i0)', 'ir_scatt_model = ', ir_scatt_model
  if ( addsolar ) then
    print '(a35,i0)', 'vis_scatt_model = ', vis_scatt_model
    print '(a35,l1)', 'dom_rayleigh = ', dom_rayleigh
  end if
  print '(a35,i0)', 'dom_nstreams = ', dom_nstreams
  print '(a35,g12.4)', 'dom_accuracy = ', dom_accuracy
  print '(a35,g12.4)', 'dom_opdep_threshold = ', dom_opdep_threshold
end if
if ( run_scatt ) then
  print '(a35,g12.4)', 'cc_threshold = ', cc_threshold
  print '(a35,g12.4)', 'ice_polarisation = ', ice_polarisation
end if
print '(a35,i0)', 'fastem_version = ', fastem_version
print '(a35,i0)', 'ir_sea_emis_model = ', ir_sea_emis_model
if ( addsolar ) then
  print '(a35,i0)', 'solar_sea_brdf_model = ', solar_sea_brdf_model
end if
print '(a35,l1)', 'do_lambertian = ', do_lambertian
if ( do_lambertian ) then
  print '(a35,i0)', 'lambertian_fixed_angle = ', lambertian_fixed_angle
end if
print '(a35,l1)', 'addpc = ', addpc
print '(a35,l1)', 'htfrtc = ', htfrtc
if ( addpc .or. htfrtc ) then
  print '(a35,i0)', 'npcscores = ', npcscores
end if
if ( addpc ) then
  print '(a35,i0)', 'ipcreg = ', ipcreg
  print '(a35,i0)', 'ipcbnd = ', ipcbnd
end if
print '(a)', header(1)

end subroutine radsim_print_cfg
