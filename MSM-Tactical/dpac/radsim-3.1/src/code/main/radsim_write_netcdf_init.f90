!-------------------------------------------------------------------------------
! Description:
!
!   Initialise netCDF file.
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

subroutine radsim_write_netcdf_init(nprofs, obs, model, file_id)

use netcdf

use radsim_mod_cfg, only : &
  output_dir, &
  output_file, &
  platform, &
  satid, &
  inst

use radsim_mod_constants, only : &
  int32, &
  output_default

use radsim_mod_io

use radsim_mod_types, only : &
  obs_type, &
  model_type

implicit none

integer,           intent(in)    :: nprofs
type(obs_type),    intent(inout) :: obs
type(model_type),  intent(inout) :: model
integer(int32),    intent(out)   :: file_id

! NetCDF stuff
integer(int32) :: nf90_flags
integer(int32) :: dim_obs_id, dim_chans_id
integer(int32) :: dim_levels_id, dim_halflevels_id
character(len=400) :: file_name
integer(int32) :: nchans, nlevels

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Create the new file
!-------------------------------------------------------------------------------

! On creation, the file is in define mode. In this mode we can define
! dimensions and write attributes but we cannot write variables. The other mode
! is data mode for which the opposite is true. When an existing file is opened
! (as opposed to being created) the default mode is data mode so we revert to
! that mode at the end of the routine.

if ( output_dir /= '' ) output_dir = trim(output_dir) // '/'

if ( output_file /= '' ) then

  file_name = trim(output_dir) // trim(output_file)

else
  write(unit=file_name, fmt='(a,i0,a)') trim(output_dir) // 'radsim-' // &
    trim(platform) // '_', satid, '_' // trim(inst)

  if ( size(model % validity_time, dim=1) == 1 ) then
    write(unit=file_name, fmt='(a,i4,4i2.2,a)') &
      trim(file_name) // '-', model % validity_time(1,1:5), '.nc'
  else
    file_name = trim(file_name) // '.nc'
  end if
end if

nf90_flags = nf90_clobber + nf90_netcdf4 + nf90_classic_model
call radsim_nf90_create(file_name, nf90_flags, file_id)

!-------------------------------------------------------------------------------
! 2. Define dimensions
!-------------------------------------------------------------------------------

nchans  = size(obs % channels)
nlevels = model % nlevels

call radsim_nf90_def_dim(file_id, 'obs', nprofs, dim_obs_id)
call radsim_nf90_def_dim(file_id, 'channels', nchans, dim_chans_id)
call radsim_nf90_def_dim(file_id, 'levels', nlevels, dim_levels_id)
call radsim_nf90_def_dim(file_id, 'halflevels', nlevels+1, dim_halflevels_id)

!-------------------------------------------------------------------------------
! 3. Write attributes
!-------------------------------------------------------------------------------

if ( output_mode >= output_default ) then
  print '(a)', 'Writing attributes to output file'
end if

call radsim_nf90_put_att(file_id, nf90_global, 'platform', platform)
call radsim_nf90_put_att(file_id, nf90_global, 'satid', satid)
call radsim_nf90_put_att(file_id, nf90_global, 'instrument', inst)
if ( size(model % validity_time, dim=1) == 1 ) then
  call radsim_nf90_put_att(file_id, nf90_global, 'validity_time', &
                           model % validity_time(1,:))
  call radsim_nf90_put_att(file_id, nf90_global, 'data_time', &
                           model % data_time(1,:))
end if
call radsim_nf90_put_att(file_id, nf90_global, 'channels', obs%channels)
if ( associated(obs % wavenumbers) ) then
  call radsim_nf90_put_att(file_id, nf90_global, 'wavenumbers', obs%wavenumbers)
end if

! Exit define mode. Variables 

call radsim_nf90_enddef(file_id)

end subroutine radsim_write_netcdf_init
