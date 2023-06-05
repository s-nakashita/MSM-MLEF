!-------------------------------------------------------------------------------
! Description:
!
!   Generate a list of required UM STASH fields.
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

subroutine radsim_set_stash(stash_list)

use radsim_mod_cfg, only : &
  run_scatt, &
  ozone_data, &
  ir_addclouds, &
  clw_data

use radsim_mod_process

implicit none

integer, intent(out) :: stash_list(nfields_rttov,2)

!-------------------------------------------------------------------------------

! Multi-level fields assumed to be on model theta levels unless otherwise stated

stash_list = 0

! Compulsory fields

stash_list(field_rttov_surftype, 1) =    33 ! orography (m)
stash_list(field_rttov_surftype, 2) =    31 ! seaice cover (fraction 0-1)
stash_list(field_rttov_tskin,    1) =    24 ! skin temperature (K)
stash_list(field_rttov_t2,       1) =  3236 ! temperature at 1.5m (K)
stash_list(field_rttov_q2,       1) =  3245 ! relative humidity at 1.5m (fraction)
stash_list(field_rttov_pstar,    1) =   409 ! surface pressure (Pa)
stash_list(field_rttov_u10,      1) =  3209 ! 10m wind u component (m/s)
stash_list(field_rttov_v10,      1) =  3210 ! 10m wind v component (m/s)
stash_list(field_rttov_p,        1) =   408 ! pressure levels (Pa)
stash_list(field_rttov_p,        2) =   407 ! pressure at rho levels (Pa)
stash_list(field_rttov_t,        1) = 16004 ! T on levels (K)
stash_list(field_rttov_t,        2) =     4 ! theta (K)
stash_list(field_rttov_q,        1) =    10 ! specific humidity on levels (kg/kg)

! Optional fields

if ( clw_data .or. run_scatt .or. ir_addclouds ) then
  stash_list(field_rttov_qcl, 1)    =   254 ! qcl on levels (kg/kg)
end if

if ( run_scatt .or. ir_addclouds ) then
  stash_list(field_rttov_qcf, 1)    =    12 ! qcf on levels (kg/kg)
  stash_list(field_rttov_cfrac, 1)  =   265 ! area cloud fraction on levels (0-1)
  stash_list(field_rttov_cfrac, 2)  =   266 ! bulk cloud fraction on levels (0-1)
end if

if ( ir_addclouds ) then
  stash_list(field_rttov_cloud_strat, 1)  =   267 ! liquid cloud fraction on levels (0-1)
  stash_list(field_rttov_cloud_strat, 2)  =   268 ! frozen cloud fraction on levels (0-1)
!   stash_list(field_rttov_cloud_conv,  1)  =  5212 ! conv cloud amount on levels (0-1)
!   stash_list(field_rttov_cloud_conv,  2)  =  5213 ! conv cloud water on levels (0-1)
end if

if ( run_scatt ) then
  stash_list(field_rttov_rain,  1)  =   186 ! rain-rate on levels (kg/m^2/s)
!   stash_list(field_rttov_snow,  1)  =   187 ! snow-rate on levels (kg/m^2/s)
end if

if ( ozone_data ) then
  stash_list(field_rttov_ozone, 1)  =    60 ! ozone on levels
end if

end subroutine radsim_set_stash
