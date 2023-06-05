!-------------------------------------------------------------------------------
! Description:
!
!   Define the fields required for RTTOV simulations.
!    0 => not required
!    1 => compulsory
!   -1 => optional
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

subroutine radsim_set_fields(required)

use radsim_mod_cfg, only : &
  run_scatt, &
  clw_data, &
  ozone_data, &
  ir_addclouds

use radsim_mod_process

implicit none

integer, intent(out) :: required(nfields_rttov)
integer :: i

required = 0

do i = 1, nfields_rttov
  select case(i)
    case(field_rttov_surftype, &
         field_rttov_tskin, &
         field_rttov_t2, &
         field_rttov_q2, &
         field_rttov_pstar, &
         field_rttov_u10, &
         field_rttov_v10, &
         field_rttov_p, &
         field_rttov_t, &
         field_rttov_q)
      required(i) = 1
    case(field_rttov_qcl)
      if ( clw_data .or. run_scatt .or. ir_addclouds ) required(i) = 1
    case(field_rttov_qcf)
      if ( run_scatt .or. ir_addclouds ) required(i) = 1
    case(field_rttov_cloud_strat)
      if ( ir_addclouds ) required(i) = -1
    case(field_rttov_cfrac)
      if ( run_scatt ) required(i) = 1
    case(field_rttov_rain)
      if ( run_scatt ) required(i) = -1
    case(field_rttov_snow)
      if ( run_scatt ) required(i) = -1
    case(field_rttov_ozone)
      if ( ozone_data ) required(i) = 1
  end select
end do

end subroutine radsim_set_fields
