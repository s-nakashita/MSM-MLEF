!-------------------------------------------------------------------------------
! Description:
!
!   Read RTTOV coefficients and allocate data structures.
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

subroutine radsim_setup_rttov( &
  nlevels,     & ! in
  date,        & ! in
  opts,        & ! out
  opts_scatt,  & ! out
  coeffs,      & ! out
  scat_coeffs, & ! out
  emis_atlas,  & ! out
  brdf_atlas   ) ! out

use parkind1, only : &
  jpim

use radsim_mod_cfg

use radsim_mod_constants, only : &
  status_error, &
  status_warning, &
  output_verbose

use mod_rttov_emis_atlas, only : &
  rttov_emis_atlas_data, &
  atlas_type_mw, &
  atlas_type_ir

use mod_rttov_brdf_atlas, only : &
  rttov_brdf_atlas_data

use rttov_const, only : &
  errorstatus_success, &
  sensor_id_ir, &
  sensor_id_hi, &
  ninterp_modes, &
  clw_scheme_deff

use rttov_types, only : &
  rttov_options,     &
  rttov_options_scatt, &
  rttov_coefs,       &
  rttov_scatt_coef

implicit none

include 'radsim_error_report.interface'
#include "rttov_read_coefs.interface"
#include "rttov_read_coefs_htfrtc.interface"
#include "rttov_read_scattcoeffs.interface"
#include "rttov_setup_emis_atlas.interface"
#include "rttov_setup_brdf_atlas.interface"
#include "rttov_user_options_checkinput.interface"

integer(kind=jpim),              intent(in)    :: nlevels
integer(kind=jpim),              intent(in)    :: date(3)
type(rttov_options),             intent(inout) :: opts
type(rttov_options_scatt),       intent(inout) :: opts_scatt
type(rttov_coefs),               intent(inout) :: coeffs
type(rttov_scatt_coef),          intent(inout) :: scat_coeffs
type(rttov_emis_atlas_data),     intent(inout) :: emis_atlas(12)
type(rttov_brdf_atlas_data),     intent(inout) :: brdf_atlas(12)

character(len=400) :: file_name_clear
character(len=400) :: file_name_aerosol
character(len=400) :: file_name_cloud
character(len=400) :: file_name_mfasis_cld
character(len=400) :: file_name_pc
character(len=400) :: file_name_htfrtc_static
character(len=400) :: file_name_htfrtc_sensor
character(len=400) :: message
character(len=4)   :: clwstr, aerstr
integer(kind=jpim) :: errorstatus
integer(kind=jpim) :: atlas_type
integer(kind=jpim) :: year, month, nchans, i

!-------------------------------------------------------------------------------

print '(a)', 'Setting up RTTOV'
print '(a,i0)', ' Number of profiles per call = ', nprofs_per_call

!-------------------------------------------------------------------------------
! Set options
!-------------------------------------------------------------------------------

! General

opts % config % verbose                 = (output_mode >= 2)
opts_scatt % config % verbose           = (output_mode >= 2)
opts % config % apply_reg_limits        = apply_reg_limits
opts_scatt % config % apply_reg_limits  = apply_reg_limits
opts % config % fix_hgpl                = .true.
opts_scatt % config % fix_hgpl          = .true.
opts % rt_all % addrefrac               = addrefrac .or. addpc
opts_scatt % addrefrac                  = addrefrac
opts % rt_all % plane_parallel          = plane_parallel
opts % rt_all % do_lambertian           = do_lambertian
opts % rt_all % lambertian_fixed_angle  = lambertian_fixed_angle
opts % rt_all % switchrad               = .true.
opts % rt_all % ozone_data              = ozone_data
opts % rt_all % co2_data                = co2_data .or. (co2_max_ppmv > 0.)
opts % rt_all % n2o_data                = n2o_data
opts % rt_all % co_data                 = co_data
opts % rt_all % ch4_data                = ch4_data
opts % rt_all % so2_data                = so2_data

! MW processing options

opts % rt_mw % clw_data                 = clw_data
opts % rt_mw % clw_scheme               = mw_clw_scheme
opts % rt_mw % clw_cloud_top            = mw_clw_cloud_top
opts % rt_mw % fastem_version           = fastem_version
opts_scatt % fastem_version             = fastem_version
opts_scatt % cc_threshold               = cc_threshold
opts_scatt % ice_polarisation           = ice_polarisation

! VIS/IR processing options

opts % rt_ir % addaerosl                = ir_addaerosols_cams
opts % rt_ir % addclouds                = ir_addclouds
opts % rt_ir % grid_box_avg_cloud       = .true.
opts % rt_ir % ir_scatt_model           = ir_scatt_model
opts % rt_ir % vis_scatt_model          = vis_scatt_model
opts % rt_ir % dom_nstreams             = dom_nstreams
opts % rt_ir % dom_accuracy             = dom_accuracy
opts % rt_ir % dom_opdep_threshold      = dom_opdep_threshold
opts % rt_ir % cldcol_threshold         = cldcol_threshold
opts % rt_ir % do_nlte_correction       = do_nlte_correction
opts % rt_ir % addsolar                 = addsolar
opts % rt_ir % rayleigh_single_scatt    = rayleigh_single_scatt
opts % rt_ir % rayleigh_max_wavelength  = rayleigh_max_wavelength
opts % rt_ir % rayleigh_min_pressure    = rayleigh_min_pressure
opts % rt_ir % dom_rayleigh             = dom_rayleigh
opts % rt_ir % solar_sea_brdf_model     = solar_sea_brdf_model
opts % rt_ir % ir_sea_emis_model        = ir_sea_emis_model

! PC processing options

opts % rt_ir % pc % addpc               = addpc
opts % rt_ir % pc % ipcreg              = ipcreg
opts % rt_ir % pc % ipcbnd              = ipcbnd
opts % rt_ir % pc % npcscores           = npcscores
opts % rt_ir % pc % addradrec           = .true.

opts % htfrtc_opts % htfrtc             = htfrtc
opts % htfrtc_opts % n_pc_in            = npcscores
opts % htfrtc_opts % reconstruct        = .true.

! Interpolation

opts % interpolation % addinterp        = .true.
opts % interpolation % reg_limit_extrap = .true.
opts_scatt % reg_limit_extrap           = .true.

!-------------------------------------------------------------------------------
! Read coefficients
!-------------------------------------------------------------------------------

errorstatus = 0

!-------------------
! Standard model (we always need these, unless running HTFRTC)
!-------------------

print '(a)', ' Reading RTTOV coefficient files:'

if ( .not. htfrtc ) then
  if ( trim(rttov_coeff_file) /= '' ) then
    file_name_clear = &
      trim(rttov_coeffs_dir) // '/' // trim(rttov_coeff_file)
  else
    write(unit=file_name_clear, fmt='(a,i0,a)') &
      trim(rttov_coeffs_dir) // '/rtcoef_' // &
      trim(platform) // '_', satid, '_' // trim(inst) // &
      trim(rttov_coeffs_options) // rttov_coeffs_type
  end if

  print '(a)', '  file = ' // trim(file_name_clear)
end if

if ( ir_addaerosols_cams ) then
  if ( trim(rttov_scaer_file) /= '' ) then
    file_name_aerosol = &
      trim(rttov_scaer_dir) // '/' // trim(rttov_scaer_file)
  else
    aerstr = 'cams'
    write(unit=file_name_aerosol, fmt='(a,i0,a)') &
      trim(rttov_scaer_dir) // '/scaercoef_' // &
      trim(platform) // '_', satid, '_' // trim(inst) // &
      '_' // aerstr // rttov_coeffs_type
  endif
  print '(a)', '  file = ' // trim(file_name_aerosol)
else
  file_name_aerosol = ''
end if

if ( ir_addclouds ) then
  if ( trim(rttov_sccld_file) /= '' ) then
    file_name_cloud = &
      trim(rttov_sccld_dir) // '/' // trim(rttov_sccld_file)
  else
    write(unit=file_name_cloud, fmt='(a,i0,a)') &
      trim(rttov_sccld_dir) // '/sccldcoef_' // &
      trim(platform) // '_', satid, '_' // trim(inst) // rttov_coeffs_type
  endif
  print '(a)', '  file = ' // trim(file_name_cloud)

  if ( run_mfasis ) then
    if ( trim(rttov_mfasis_cld_file) /= '' ) then
      file_name_mfasis_cld = &
        trim(rttov_mfasis_cld_dir) // '/' // trim(rttov_mfasis_cld_file)
    else
      clwstr = 'opac'
      if ( ircloud_clw_scheme == clw_scheme_deff ) clwstr = 'deff'
      write(unit=file_name_mfasis_cld, fmt='(a,i0,a)') &
        trim(rttov_mfasis_cld_dir) // '/rttov_mfasis_cld_' // &
        trim(platform) // '_', satid, '_' // trim(inst) // '_' // &
        clwstr // '.H5'
    endif
    print '(a)', '  file = ' // trim(file_name_mfasis_cld)
  end if
else
  file_name_cloud = ''
  file_name_mfasis_cld = ''
end if

if ( addpc ) then
  if ( trim(rttov_pccoeff_file) /= '' ) then
    file_name_pc = &
      trim(rttov_pccoeffs_dir) // '/' // trim(rttov_pccoeff_file)
  else
    write(unit=file_name_pc, fmt='(a,i0,a)') &
      trim(rttov_pccoeffs_dir) // '/pccoef_' // &
      trim(platform) // '_', satid, '_' // trim(inst) // rttov_coeffs_type
  endif
  print '(a)', '  file = ' // trim(file_name_pc)
else
  file_name_pc = ''
end if

if ( htfrtc ) then
  if ( trim(rttov_htfrtc_static_file) /= '' ) then
    file_name_htfrtc_static = &
      trim(rttov_htfrtc_coeffs_dir) // '/' // trim(rttov_htfrtc_static_file)
  else
    file_name_htfrtc_static = &
      trim(rttov_htfrtc_coeffs_dir) // '/' // 'htfrtc_coef_static.nc'
  endif

  if ( trim(rttov_htfrtc_sensor_file) /= '' ) then
    file_name_htfrtc_sensor = &
      trim(rttov_htfrtc_coeffs_dir) // '/' // trim(rttov_htfrtc_sensor_file)
  else
    file_name_htfrtc_sensor = &
      trim(rttov_htfrtc_coeffs_dir) // '/htfrtc_coef_sensor_' // &
      trim(platform) // '_' // trim(inst) // '.nc'
  endif
  print '(a)', '  file = ' // trim(file_name_htfrtc_static)
  print '(a)', '  file = ' // trim(file_name_htfrtc_sensor)
else
  file_name_htfrtc_static = ''
  file_name_htfrtc_sensor = ''
end if

nchans = count(channels > 0)

if ( .not. htfrtc ) then

  if ( nchans > 0 ) then
    call rttov_read_coefs( &
      errorstatus,                            & ! out
      coeffs,                                 & ! out
      opts,                                   & ! in
      channels_rec = channels(1:nchans),      & ! in
      file_coef = file_name_clear,            & ! in
      file_scaer = file_name_aerosol,         & ! in
      file_sccld = file_name_cloud,           & ! in
      file_mfasis_cld = file_name_mfasis_cld, & ! in
      file_pccoef = file_name_pc)               ! in
  else
    call rttov_read_coefs( &
      errorstatus,                            & ! out
      coeffs,                                 & ! out
      opts,                                   & ! in
      file_coef = file_name_clear,            & ! in
      file_scaer = file_name_aerosol,         & ! in
      file_sccld = file_name_cloud,           & ! in
      file_mfasis_cld = file_name_mfasis_cld, & ! in
      file_pccoef = file_name_pc)               ! in
  end if

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_read_coefs = ', errorstatus
    call radsim_error_report(message, status_error)
  end if

  ! Check coef file for variable CO2 if co2_max_ppmv specified
  if ( co2_max_ppmv > 0. .and. coeffs % coef % nco2 == 0 ) then
    message = 'co2_max_ppmv option requires RTTOV coefficients with variable CO2.' // &
    ' Continuing without scaling background CO2 profile...'
    call radsim_error_report(message, status_warning)
    co2_max_ppmv = -1.
    opts % rt_all % co2_data = .false.
  end if

else

  if ( nchans > 0 ) then
    call rttov_read_coefs_htfrtc( &
      errorstatus,             & ! out
      coeffs,                  & ! out
      file_name_htfrtc_static, & ! in
      file_name_htfrtc_sensor, & ! in
      channels(1:nchans))        ! in
  else
    call rttov_read_coefs_htfrtc( &
      errorstatus,             & ! out
      coeffs,                  & ! out
      file_name_htfrtc_static, & ! in
      file_name_htfrtc_sensor)   ! in
  end if

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_read_htfrtc_coefs = ', errorstatus
    call radsim_error_report(message, status_error)
  end if

end if

! Set interpolation mode to avoid "spiky" Jacobians from RTTOV

if ( .not. htfrtc ) then
  if ( interp_mode > 0 .and. interp_mode <= ninterp_modes ) then
    opts % interpolation % interp_mode = interp_mode
    opts_scatt % interp_mode = interp_mode
  else
    if ( nlevels > coeffs % coef % fmv_lvl(1) ) then
      opts % interpolation % interp_mode = 4
      opts_scatt % interp_mode = 4
    else
      opts % interpolation % interp_mode = 1
      opts_scatt % interp_mode = 1
    end if
  end if
end if

!---------------
! RTTOV-SCATT model (if requested)
!---------------

if ( run_scatt ) then

  print '(a)', ' Reading coefficient files for RTTOV-SCATT model'

  if ( trim(rttov_hydrotable_file) /= '' ) then
    file_name_cloud = &
      trim(rttov_hydrotable_dir) // '/' // trim(rttov_hydrotable_file)
  else
    write(unit=file_name_cloud, fmt='(a)') &
      trim(rttov_hydrotable_dir) // '/hydrotable_' // &
      trim(platform) // '_' // trim(inst) // '.dat'
  endif
  print '(a)', '  file = ' // trim(file_name_cloud)

  call rttov_read_scattcoeffs( &
    errorstatus,              & ! out
    opts_scatt,               & ! in
    coeffs,                   & ! in;    RTTOV optical depth coeffs
    scat_coeffs,              & ! inout; scattering coeffs
    file_coef=file_name_cloud ) ! in,    optional; full path of file

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_readscattcoeffs = ', errorstatus
    call radsim_error_report(message, status_error)
  end if

end if

!--------------------------------------------------
! Check consistency of options and RTTOV coeffs
!--------------------------------------------------

if ( .not. htfrtc ) then
  if ( output_mode >= output_verbose ) then
    print '(a)', ' Validating coeffs for requested options'
  end if
  call rttov_user_options_checkinput(errorstatus, opts, coeffs)

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_user_options_checkinput = ', errorstatus
    call radsim_error_report(message, status_error)
  end if
end if

!---------------------
! Emissivity atlas
!---------------------

if ( use_emiss_atlas ) then

  year = date(1)
  month = date(2)

  if ( htfrtc ) then
    atlas_type = atlas_type_ir
  else
    select case(coeffs % coef % id_sensor)
      case(sensor_id_ir, sensor_id_hi)
        atlas_type = atlas_type_ir
      case default
        atlas_type = atlas_type_mw
    end select
  end if

  do i = 1, 12
    ! For NWP SAF profile datasets read emissivity data for all months if requested
    ! Otherwise just read data for the given month
    if ( i == month .or. use_all_atlas_months ) then
      if ( emiss_atlas_dir /= '' ) then
        call rttov_setup_emis_atlas( &
          errorstatus,         & ! out
          opts,                & ! in
          i,                   & ! in
          atlas_type,          & ! in
          emis_atlas(i),       & ! inout
          emis_atlas_id,       & ! in, optional
          coefs=coeffs,        & ! in, optional
          path=emiss_atlas_dir ) ! in, optional
      else
        call rttov_setup_emis_atlas( &
          errorstatus,    & ! out
          opts,           & ! in
          i,              & ! in
          atlas_type,     & ! in
          emis_atlas(i),  & ! inout
          emis_atlas_id,  & ! in, optional
          coefs=coeffs )    ! in, optional
      end if

      if ( errorstatus /= errorstatus_success ) then
        write(message, '(a)') 'Error reading emissivity atlas'
        call radsim_error_report(message, status_error)
      end if
    end if
  end do
end if

!---------------------
! BRDF atlas
!---------------------

if ( use_brdf_atlas ) then

  year = date(1)
  month = date(2)

  do i = 1, 12
    ! For NWP SAF profile datasets read BRDF data for all months if requested
    ! Otherwise just read data for the given month
    if ( i == month .or. use_all_atlas_months ) then
      if ( brdf_atlas_dir /= '' ) then
        call rttov_setup_brdf_atlas( &
          errorstatus,        & ! out
          opts,               & ! in
          i,                  & ! in
          brdf_atlas(i),      & ! inout
          coefs=coeffs,       & ! in, optional
          path=brdf_atlas_dir ) ! in, optional
      else
        call rttov_setup_brdf_atlas( &
          errorstatus,    & ! out
          opts,           & ! in
          i,              & ! in
          brdf_atlas(i),  & ! inout
          coefs=coeffs )    ! in, optional
      end if

      if ( errorstatus /= errorstatus_success ) then
        write(message, '(a)') 'Error reading BRDF atlas'
        call radsim_error_report(message, status_error)
      end if
    end if
  end do
end if

!--------------------------------------------
! For SSU, update cell pressures if specified
!--------------------------------------------

if ( coeffs % coef % pmc_shift ) then
  where ( ssu_co2_cell_pressure > 0 )
    coeffs % coef % pmc_ppmc = ssu_co2_cell_pressure(1:coeffs % coef % fmv_chn)
  else where
    ! In RTTOV v13.0 and earlier pmc_ppmc is uninitialised. I will fix in v13.1
    ! but for v13.0 this ensures it defaults to the nominal cell pressures.
    coeffs % coef % pmc_ppmc = coeffs % coef % pmc_pnominal
  end where
end if

end subroutine radsim_setup_rttov
