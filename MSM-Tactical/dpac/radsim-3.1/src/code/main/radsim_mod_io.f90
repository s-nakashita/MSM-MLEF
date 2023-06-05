!-------------------------------------------------------------------------------
! Description:
!
!   Module for I/O related declarations.
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

module radsim_mod_io

use iso_fortran_env, only : &
  error_unit

use netcdf

use radsim_mod_cfg, only : &
  output_mode

use radsim_mod_constants, only : &
  int32, &
  int64, &
  real32, &
  real64, &
  status_warning, &
  status_error, &
  output_verbose, &
  output_debug

! Module procedure for reading attributes and variables from an input netCDF file

interface radsim_nf90_get_att
  module procedure &
    radsim_nf90_get_att_char, &
    radsim_nf90_get_att_int_scalar, &
    radsim_nf90_get_att_real64
end interface radsim_nf90_get_att

interface radsim_nf90_get_var
  module procedure &
    radsim_nf90_get_var_int_1D, &
    radsim_nf90_get_var_real_1D, &
    radsim_nf90_get_var_real64_2D, &
    radsim_nf90_get_var_real64_3D
end interface radsim_nf90_get_var

! Module procedure for writing different fields to the output netCDF file

interface radsim_write_field_nc
  module procedure &
    radsim_write_field_nc_int_1D,  &
    radsim_write_field_nc_int_2D,  &
    radsim_write_field_nc_real_1D, &
    radsim_write_field_nc_real_2D, &
    radsim_write_field_nc_real_3D, &
    radsim_write_field_nc_real64_1D, &
    radsim_write_field_nc_real64_2D, &
    radsim_write_field_nc_real64_3D
end interface radsim_write_field_nc

interface radsim_nf90_put_att
  module procedure &
    radsim_nf90_put_att_char, &
    radsim_nf90_put_att_int_scalar, &
    radsim_nf90_put_att_int64_scalar, &
    radsim_nf90_put_att_real_scalar, &
    radsim_nf90_put_att_real64_scalar, &
    radsim_nf90_put_att_int_1D, &
    radsim_nf90_put_att_int64_1D, &
    radsim_nf90_put_att_real_1D, &
    radsim_nf90_put_att_real64_1D
end interface radsim_nf90_put_att

interface radsim_nf90_put_var
  module procedure &
    radsim_nf90_put_var_int_1D, &
    radsim_nf90_put_var_int64_1D, &
    radsim_nf90_put_var_real_1D, &
    radsim_nf90_put_var_real64_1D, &
    radsim_nf90_put_var_int_2D, &
    radsim_nf90_put_var_int64_2D, &
    radsim_nf90_put_var_real_2D, &
    radsim_nf90_put_var_real64_2D, &
    radsim_nf90_put_var_int_3D, &
    radsim_nf90_put_var_real_3D, &
    radsim_nf90_put_var_real64_3D
end interface radsim_nf90_put_var

! File type IDs

integer, parameter :: filetype_fieldsfile    = 0
integer, parameter :: filetype_grib_ecmwf    = 1
integer, parameter :: filetype_ecprof60      = 2
integer, parameter :: filetype_ecprof91      = 3
integer, parameter :: filetype_ecprof137     = 4
integer, parameter :: filetype_netcdf_ecmwf  = 5
integer, parameter :: filetype_grib_icon     = 6
integer, parameter :: filetype_grib_harmonie = 7
integer, parameter :: filetype_grib_jma      = 8
integer, parameter :: filetype_ncepmsm       = 9

character(len=40) :: written_fields(100) = ''

contains

include 'radsim_write_field_nc.f90'
include 'radsim_readwrite_nf90.f90'

end module radsim_mod_io
