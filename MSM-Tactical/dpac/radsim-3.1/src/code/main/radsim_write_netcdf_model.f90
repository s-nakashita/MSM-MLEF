!-------------------------------------------------------------------------------
! Description:
!
!   Write simulated data to a netCDF file.
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

subroutine radsim_write_netcdf_model(file_id, model, range)

use netcdf

use radsim_mod_cfg, only : &
  output_mode, &
  write_profiles, &
  model_filetype

use radsim_mod_constants, only : &
  int32, &
  output_default

use radsim_mod_io

use radsim_mod_types, only : &
  model_type

implicit none

integer(int32),   intent(in)   :: file_id
type(model_type), intent(inout) :: model
integer, optional, intent(in) :: range(:)

! NetCDF stuff
integer(int32) :: dims(2)
integer(int32) :: dim_obs_id
integer(int32) :: dim_levels_id
integer(int32) :: dim_halflevels_id
integer(int32) :: p1
integer(int32) :: np

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Get dimension information
!-------------------------------------------------------------------------------

call radsim_nf90_inq_dimid(file_id, 'obs', dim_obs_id)
call radsim_nf90_inq_dimid(file_id, 'levels', dim_levels_id)
call radsim_nf90_inq_dimid(file_id, 'halflevels', dim_halflevels_id)

dims(1) = dim_obs_id
dims(2:) = 0

if ( present(range) ) then
  p1 = range(1)
  np = range(2) - range(1) + 1
else
  p1 = 1
  call radsim_nf90_inquire_dimension(file_id, dim_obs_id, len=np)
end if

!-------------------------------------------------------------------------------
! 2. Write global attributes
!-------------------------------------------------------------------------------

if ( output_mode >= output_default ) then
  print '(a)', 'Writing profile data to output file'
end if

! The dataset must be in define mode to write attributes

call radsim_nf90_redef(file_id)

! Write missing data indicators as attributes

call radsim_nf90_put_att(file_id, nf90_global, 'RMDI', model % rmdi)
call radsim_nf90_put_att(file_id, nf90_global, 'IMDI', model % imdi)

!-------------------------------------------------------------------------------
! 3. Write fields
!-------------------------------------------------------------------------------

! Take the file out of define mode. The radsim_write_field_nc routine will
! enter and exit define mode when necessary so it is no longer required here.

call radsim_nf90_enddef(file_id)

! These are mostly 64-bit fields but we write them out as default real

if ( associated(model % grid % lat) ) then
  call radsim_write_field_nc(model % grid % lat(1:np), 'lat', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % grid % lon) ) then
  call radsim_write_field_nc(model % grid % lon(1:np), 'lon', dims(1), file_id, start=(/p1/))
end if

if ( .not. write_profiles ) return

! For the NWP SAF datasets we can write out the profile date and time values:
! in this case there can be no obs file (so no duplication of date/time output)
if ( size(model % validity_time, dim=1) > 1 .and. &
     (model_filetype == filetype_ecprof60 .or. &
      model_filetype == filetype_ecprof91 .or. &
      model_filetype == filetype_ecprof137) ) then
  call radsim_write_field_nc(model % validity_time(1:np,1), 'year', dims(1), file_id, start=(/p1/))
  call radsim_write_field_nc(model % validity_time(1:np,2), 'month', dims(1), file_id, start=(/p1/))
  call radsim_write_field_nc(model % validity_time(1:np,3), 'day', dims(1), file_id, start=(/p1/))
  call radsim_write_field_nc(model % validity_time(1:np,4), 'hour', dims(1), file_id, start=(/p1/))
  call radsim_write_field_nc(model % validity_time(1:np,5), 'minute', dims(1), file_id, start=(/p1/))
end if

if ( associated(model % lsm) ) then
  call radsim_write_field_nc(model % lsm(1:np), 'lsm', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % zsurf) ) then
  call radsim_write_field_nc(model % zsurf(1:np), 'zsurf', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % seaice) ) then
  call radsim_write_field_nc(model % seaice(1:np), 'seaice', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % pstar) ) then
  call radsim_write_field_nc(model % pstar(1:np), 'pstar', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % t2) ) then
  call radsim_write_field_nc(model % t2(1:np), 't2', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % td2) ) then
  call radsim_write_field_nc(model % td2(1:np), 'td2', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % q2) ) then
  call radsim_write_field_nc(model % q2(1:np), 'q2', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % rh2) ) then
  call radsim_write_field_nc(model % rh2(1:np), 'rh2', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % tskin) ) then
  call radsim_write_field_nc(model % tskin(1:np), 'tskin', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % u10) ) then
  call radsim_write_field_nc(model % u10(1:np), 'u10', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % v10) ) then
  call radsim_write_field_nc(model % v10(1:np), 'v10', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % total_cc) ) then
  call radsim_write_field_nc(model % total_cc(1:np), 'total_cc', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % low_cc) ) then
  call radsim_write_field_nc(model % low_cc(1:np), 'low_cc', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % medium_cc) ) then
  call radsim_write_field_nc(model % medium_cc(1:np), 'medium_cc', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % high_cc) ) then
  call radsim_write_field_nc(model % high_cc(1:np), 'high_cc', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % snow_depth) ) then
  call radsim_write_field_nc(model % snow_depth(1:np), 'snow_depth', dims(1), file_id, start=(/p1/))
end if
if ( associated(model % z) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % z(1:np,:), 'z', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % p) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % p(1:np,:), 'p', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % ph) ) then
  dims(2) = dim_halflevels_id
  call radsim_write_field_nc(model % ph(1:np,:), 'ph', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % t) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % t(1:np,:), 't', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % theta) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % theta(1:np,:), 'theta', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % q) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % q(1:np,:), 'q', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % rh) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % rh(1:np,:), 'rh', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % density) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % density(1:np,:), 'density', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % clw_deff) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % clw_deff(1:np,:), 'clw_deff', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % ciw_deff) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % ciw_deff(1:np,:), 'ciw_deff', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % clw) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % clw(1:np,:), 'clw', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % ciw) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % ciw(1:np,:), 'ciw', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % rain) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % rain(1:np,:), 'rain', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % snow) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % snow(1:np,:), 'snow', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cfrac) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cfrac(1:np,:), 'cfrac', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cfrac_liq) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cfrac_liq(1:np,:), 'cfrac_liq', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cfrac_ice) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cfrac_ice(1:np,:), 'cfrac_ice', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % o3) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % o3(1:np,:), 'o3', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % co2) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % co2(1:np,:), 'co2', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % n2o) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % n2o(1:np,:), 'n2o', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % co) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % co(1:np,:), 'co', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % ch4) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % ch4(1:np,:), 'ch4', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % so2) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % so2(1:np,:), 'so2', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_sea_salt1) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_sea_salt1(1:np,:), 'cams_sea_salt1', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_sea_salt2) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_sea_salt2(1:np,:), 'cams_sea_salt2', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_sea_salt3) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_sea_salt3(1:np,:), 'cams_sea_salt3', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_dust1) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_dust1(1:np,:), 'cams_dust1', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_dust2) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_dust2(1:np,:), 'cams_dust2', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_dust3) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_dust3(1:np,:), 'cams_dust3', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_hphil_omat) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_hphil_omat(1:np,:), 'cams_hphil_omat', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_hphob_bcar) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_hphob_bcar(1:np,:), 'cams_hphob_bcar', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(model % cams_sulphate) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(model % cams_sulphate(1:np,:), 'cams_sulphate', dims(1:2), file_id, start=(/p1,1/))
end if

end subroutine radsim_write_netcdf_model
