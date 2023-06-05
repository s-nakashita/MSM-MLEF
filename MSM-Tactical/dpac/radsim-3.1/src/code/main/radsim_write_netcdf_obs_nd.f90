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

subroutine radsim_write_netcdf_obs_nd(file_id, obs, range)

use iso_fortran_env, only : &
  output_unit

use netcdf

use radsim_mod_constants, only : &
  int32

use radsim_mod_io

use radsim_mod_types, only : &
  obs_type

implicit none

integer(int32), intent(in)    :: file_id
type(obs_type), intent(inout) :: obs
integer, optional, intent(in) :: range(:)

! NetCDF stuff
integer(int32) :: dims(3)
integer(int32) :: dim_obs_id
integer(int32) :: dim_levels_id
integer(int32) :: dim_chans_id
integer(int32) :: p1
integer(int32) :: np

!-------------------------------------------------------------------------------

call radsim_nf90_inq_dimid(file_id, 'obs', dim_obs_id)
call radsim_nf90_inq_dimid(file_id, 'levels', dim_levels_id)
call radsim_nf90_inq_dimid(file_id, 'channels', dim_chans_id)

dims(1) = dim_obs_id
dims(2:) = 0

if ( present(range) ) then
  p1 = range(1)
  np = range(2) - range(1) + 1
else
  p1 = 1
  call radsim_nf90_inquire_dimension(file_id, dim_obs_id, len=np)
end if

if ( associated(obs % bt) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%bt(1:np,:), 'bt', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % refl) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%refl(1:np,:), 'refl', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % radiance) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%radiance(1:np,:), 'radiance', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % emiss) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%emiss(1:np,:), 'emiss', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % brdf) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%brdf(1:np,:), 'brdf', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % cads_height_assignment) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%cads_height_assignment(1:np,:), 'cads_height_assignment', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % geometric_height) ) then
  dims(2) = dim_levels_id
  call radsim_write_field_nc(obs%geometric_height(1:np,:), 'geometric_height', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % tjac) ) then
  dims(2:3) = (/ dim_levels_id, dim_chans_id /)
  call radsim_write_field_nc(obs%tjac(1:np,:,:), 'tjac', dims(1:3), file_id, start=(/p1,1,1/))
end if
if ( associated(obs % qjac) ) then
  dims(2:3) = (/ dim_levels_id, dim_chans_id /)
  call radsim_write_field_nc(obs%qjac(1:np,:,:), 'qjac', dims(1:3), file_id, start=(/p1,1,1/))
end if
if ( associated(obs % o3jac) ) then
  dims(2:3) = (/ dim_levels_id, dim_chans_id /)
  call radsim_write_field_nc(obs%o3jac(1:np,:,:), 'o3jac', dims(1:3), file_id, start=(/p1,1,1/))
end if
if ( associated(obs % tskinjac) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%tskinjac(1:np,:), 'tskinjac', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % wind10mujac) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%wind10mujac(1:np,:), 'wind10mujac', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % wind10mvjac) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%wind10mvjac(1:np,:), 'wind10mvjac', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % emissjac) ) then
  dims(2) = dim_chans_id
  call radsim_write_field_nc(obs%emissjac(1:np,:), 'emissjac', dims(1:2), file_id, start=(/p1,1/))
end if
if ( associated(obs % trans) ) then
  dims(2:3) = (/ dim_levels_id, dim_chans_id /)
  call radsim_write_field_nc(obs%trans(1:np,:,:), 'trans', dims(1:3), file_id, start=(/p1,1,1/))
end if

flush(output_unit)

end subroutine radsim_write_netcdf_obs_nd
