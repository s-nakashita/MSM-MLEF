!-------------------------------------------------------------------------------
! Description:
!
!   Check that the right fields are present for running RTTOV.
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

subroutine radsim_check_fields(model)

use radsim_mod_cfg, only : &
  run_scatt, &
  clw_data, &
  ozone_data, &
  ir_addclouds, &
  ircloud_clw_scheme, &
  ircloud_ice_scheme, &
  ircloud_use_model_clw_deff, &
  ircloud_use_model_ice_deff

use radsim_mod_constants, only : &
  status_error, &
  status_warning

use radsim_mod_process

use radsim_mod_types, only : &
  model_type

use rttov_const, only : &
  clw_scheme_deff, &
  ice_scheme_baum

implicit none

include 'radsim_error_report.interface'

type(model_type), intent(in) :: model

integer :: i
integer :: missing(nfields_rttov)
character(len=80) :: message

missing = 0

do i = 1, nfields_rttov

  message = ''

  select case(i)
    ! Compulsory fields
    case(field_rttov_surftype)
      if ( .not. associated(model % lsm) .and. &
           .not. associated(model % zsurf)     ) missing(i) = 1
    case(field_rttov_tskin)
      if ( .not. associated(model % tskin) ) missing(i) = 1
    case(field_rttov_t2)
      if ( .not. associated(model % t2) ) missing(i) = 1
    case(field_rttov_q2)
      if ( .not. associated(model % q2) ) missing(i) = 1
    case(field_rttov_pstar)
      if ( .not. associated(model % pstar) ) missing(i) = 1
    case(field_rttov_u10)
      if ( .not. associated(model % u10) ) missing(i) = 1
    case(field_rttov_v10)
      if ( .not. associated(model % v10) ) missing(i) = 1
    case(field_rttov_p)
      if ( .not. associated(model % p) ) missing(i) = 1
    case(field_rttov_ph)
      if ( run_scatt ) then
        if ( .not. associated(model % ph) ) missing(i) = 1
      end if
    case(field_rttov_t)
      if ( .not. associated(model % t) ) missing(i) = 1
    case(field_rttov_q)
      if ( .not. associated(model % q) ) missing(i) = 1
    ! Optional fields
    case(field_rttov_density)
      ! No need to print out a warning message
      ! if ( ir_addclouds .and. ircloud_clw_scheme == clw_scheme_deff ) then
      !   if ( .not. associated(model % density) ) missing(i) = -1
      ! end if
    case(field_rttov_clw_deff)
      if ( ir_addclouds .and. ircloud_clw_scheme == clw_scheme_deff .and. &
           ircloud_use_model_clw_deff ) then
        if ( .not. associated(model % clw_deff) ) missing(i) = 1
      end if
    case(field_rttov_ciw_deff)
      if ( ir_addclouds .and. ircloud_ice_scheme == ice_scheme_baum .and. &
           ircloud_use_model_ice_deff ) then
        if ( .not. associated(model % ciw_deff) ) missing(i) = 1
      end if
    case(field_rttov_qcl)
      if ( clw_data .or. run_scatt .or. ir_addclouds ) then
        if ( .not. associated(model % clw) ) missing(i) = 1
      end if
    case(field_rttov_qcf)
      if ( run_scatt .or. ir_addclouds ) then
        if ( .not. associated(model % ciw) ) missing(i) = 1
      end if
    case(field_rttov_cloud_strat)
      ! if ( ir_addclouds ) then
      !   if ( .not. associated(model % cfrac_liq) .and. &
      !        .not. associated(model % cfrac_ice)       ) missing(i) = -1
      !   message = 'No liquid/ice cloud fractions present. Will use total cloud fraction instead.'
      ! end if
    case(field_rttov_cloud_conv)
      ! if ( ir_addclouds ) then
      !   if ( .not. associated(model % conv_cloud) ) missing(i) = -1
      !   message = 'No convective cloud, continuing without...'
      ! end if
    case(field_rttov_cfrac)
      if ( run_scatt .or. ir_addclouds ) then
        if ( .not. associated(model % cfrac) ) missing(i) = 1
      end if
    case(field_rttov_rain)
      ! Treat this as optional at the moment
      if ( .not. associated(model % rain) .and. run_scatt ) missing(i) = -1
    case(field_rttov_snow)
      if ( .not. associated(model % snow) .and. run_scatt ) missing(i) = -1
    case(field_rttov_ozone)
      if ( .not. associated(model % o3) .and. ozone_data ) missing(i) = 1
    case(field_total_cc, field_low_cc, field_medium_cc, field_high_cc)
      ! Not mandatory, but if present read in, interpolate and write out
    case(field_rttov_lat, field_rttov_lon)
      if ( model % grid % type == 101 ) then
        if ( .not. associated(model % grid % lat) .or. &
             .not. associated(model % grid % lon)      ) then
          missing(i) = 1
          message = 'Latitude/longitude required with unstructured grid'
        end if
      end if
    case(field_rttov_snow_depth)
      ! optional
    case(field_rttov_cams_salt1, field_rttov_cams_salt2, field_rttov_cams_salt3, &
         field_rttov_cams_dust1, field_rttov_cams_dust2, field_rttov_cams_dust3, &
         field_rttov_cams_hphil_omat, field_rttov_cams_hphob_bcar, field_rttov_cams_sulph)
      ! optional
    case default
      write(message,'(a,i0)') 'Unknown RTTOV field = ', i
      call radsim_error_report(message, status_error)
  end select

  if ( abs(missing(i)) == 1 ) then
    if ( message == '' ) then
      message = 'No ' // trim(field_names_rttov(i)) // ' present'
      if ( missing(i) == 1 ) message = trim(message) // ' (compulsory field)'
    end if
    call radsim_error_report(message, status_warning)
  end if

end do

if ( any(missing == 1) ) then
  message = 'Missing compulsory fields: Cannot run in this configuration'
  call radsim_error_report(message, status_error)
end if

end subroutine radsim_check_fields
