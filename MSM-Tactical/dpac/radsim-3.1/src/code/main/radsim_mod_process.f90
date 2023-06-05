!-------------------------------------------------------------------------------
! Description:
!
!   Module for general processing related declarations.
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

module radsim_mod_process

use parkind1, only : &
  jpim

implicit none

! Required RTTOV fields
integer, parameter :: field_rttov_surftype        =  1
integer, parameter :: field_rttov_tskin           =  2
integer, parameter :: field_rttov_t2              =  3
integer, parameter :: field_rttov_q2              =  4
integer, parameter :: field_rttov_pstar           =  5
integer, parameter :: field_rttov_u10             =  6
integer, parameter :: field_rttov_v10             =  7
integer, parameter :: field_rttov_p               =  8
integer, parameter :: field_rttov_t               =  9
integer, parameter :: field_rttov_q               = 10
integer, parameter :: field_rttov_qcl             = 11
integer, parameter :: field_rttov_qcf             = 12
integer, parameter :: field_rttov_cloud_strat     = 13
integer, parameter :: field_rttov_cloud_conv      = 14
integer, parameter :: field_rttov_cfrac           = 15
integer, parameter :: field_rttov_rain            = 16
integer, parameter :: field_rttov_snow            = 17
integer, parameter :: field_rttov_ozone           = 18
integer, parameter :: field_total_cc              = 19 ! Total/low/medium/high cloud cover are not used by RTTOV,
integer, parameter :: field_low_cc                = 20 ! but users have requested they are read in, interpolated,
integer, parameter :: field_medium_cc             = 21 ! and written out if present in ECMWF files
integer, parameter :: field_high_cc               = 22 ! 
integer, parameter :: field_rttov_lat             = 23 ! Latitude/longitude read in for irregular grids
integer, parameter :: field_rttov_lon             = 24 ! 
integer, parameter :: field_rttov_ph              = 25
integer, parameter :: field_rttov_density         = 26
integer, parameter :: field_rttov_snow_depth      = 27
integer, parameter :: field_rttov_clw_deff        = 28
integer, parameter :: field_rttov_ciw_deff        = 29
integer, parameter :: field_rttov_cams_salt1      = 30
integer, parameter :: field_rttov_cams_salt2      = 31
integer, parameter :: field_rttov_cams_salt3      = 32
integer, parameter :: field_rttov_cams_dust1      = 33
integer, parameter :: field_rttov_cams_dust2      = 34
integer, parameter :: field_rttov_cams_dust3      = 35
integer, parameter :: field_rttov_cams_hphil_omat = 36
integer, parameter :: field_rttov_cams_hphob_bcar = 37
integer, parameter :: field_rttov_cams_sulph      = 38
integer, parameter :: nfields_rttov               = 38

character(len=25) :: field_names_rttov(nfields_rttov) = &
  (/ 'surface type             ', &
     'skin temperature         ', &
     '2m temperature           ', &
     '2m specific humidity     ', &
     'surface pressure         ', &
     '10m wind u-component     ', &
     '10m wind v-component     ', &
     'pressure levels          ', &
     'temperature profile      ', &
     'specific humidity profile', &
     'liquid water profile     ', &
     'ice water profile        ', &
     'IR strat cloud profile   ', &
     'IR conv cloud profile    ', &
     'cloud fraction profile   ', &
     'rain profile             ', &
     'snow profile             ', &
     'ozone profile            ', &
     'total cloud cover        ', &
     'low cloud cover          ', &
     'medium cloud cover       ', &
     'high cloud cover         ', &
     'latitude                 ', &
     'longitude                ', &
     'pressure half-levels     ', &
     'density                  ', &
     'surface snow depth       ', &
     'cloud liquid eff diameter', &
     'cloud ice eff diameter   ', &
     'cams sea salt bin 1      ', &
     'cams sea salt bin 2      ', &
     'cams sea salt bin 3      ', &
     'cams dust bin 1          ', &
     'cams dust bin 2          ', &
     'cams dust bin 3          ', &
     'cams h-phil org matter   ', &
     'cams cams h-phob blk carb', &
     'cams suphate             ' /)

real :: z_direction   ! Direction of z-axis for input/output profiles

end module radsim_mod_process
