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

subroutine radsim_write_netcdf_obs_1d(file_id, obs, range)

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

integer(int32) :: dim
integer(int32) :: dim_obs_id
integer(int32) :: p1
integer(int32) :: np

!-------------------------------------------------------------------------------

call radsim_nf90_inq_dimid(file_id, 'obs', dim_obs_id)
dim = dim_obs_id

if ( present(range) ) then
  p1 = range(1)
  np = range(2) - range(1) + 1
else
  p1 = 1
  call radsim_nf90_inquire_dimension(file_id, dim_obs_id, len=np)
end if

if ( associated(obs % lat) ) then
  call radsim_write_field_nc(obs % lat(1:np), 'lat', dim, file_id, start=(/p1/))
end if
if ( associated(obs % lon) ) then
  call radsim_write_field_nc(obs % lon(1:np), 'lon', dim, file_id, start=(/p1/))
end if
if ( associated(obs % footprint_rmajor) ) then
  call radsim_write_field_nc(obs % footprint_rmajor(1:np), 'footprint_rmajor', dim, file_id, start=(/p1/))
end if
if ( associated(obs % footprint_rminor) ) then
  call radsim_write_field_nc(obs % footprint_rminor(1:np), 'footprint_rminor', dim, file_id, start=(/p1/))
end if
if ( associated(obs % zsurf) ) then
  call radsim_write_field_nc(obs % zsurf(1:np), 'zsurf', dim, file_id, start=(/p1/))
end if
if ( associated(obs % satzen) ) then
  call radsim_write_field_nc(obs % satzen(1:np), 'satzen', dim, file_id, start=(/p1/))
end if
if ( associated(obs % satazim) ) then
  call radsim_write_field_nc(obs % satazim(1:np), 'satazim', dim, file_id, start=(/p1/))
end if
if ( associated(obs % solzen) ) then
  call radsim_write_field_nc(obs % solzen(1:np), 'solzen', dim, file_id, start=(/p1/))
end if
if ( associated(obs % solazim) ) then
  call radsim_write_field_nc(obs % solazim(1:np), 'solazim', dim, file_id, start=(/p1/))
end if
if ( associated(obs % year) ) then
  call radsim_write_field_nc(obs % year(1:np), 'year', dim, file_id, start=(/p1/))
end if
if ( associated(obs % month) ) then
  call radsim_write_field_nc(obs % month(1:np), 'month', dim, file_id, start=(/p1/))
end if
if ( associated(obs % day) ) then
  call radsim_write_field_nc(obs % day(1:np), 'day', dim, file_id, start=(/p1/))
end if
if ( associated(obs % hour) ) then
  call radsim_write_field_nc(obs % hour(1:np), 'hour', dim, file_id, start=(/p1/))
end if
if ( associated(obs % minute) ) then
  call radsim_write_field_nc(obs % minute(1:np), 'minute', dim, file_id, start=(/p1/))
end if
if ( associated(obs % lsm) ) then
  call radsim_write_field_nc(obs % lsm(1:np), 'lsm', dim, file_id, start=(/p1/))
end if
if ( associated(obs % rtsurf) ) then
  call radsim_write_field_nc(obs % rtsurf(1:np), 'rtsurf', dim, file_id, start=(/p1/))
end if
if ( associated(obs % qcflags) ) then
  call radsim_write_field_nc(obs % qcflags(1:np), 'qcflags', dim, file_id, start=(/p1/))
end if
if ( associated(obs % qcinfo) ) then
  call radsim_write_field_nc(obs % qcinfo(1:np), 'qcinfo', dim, file_id, start=(/p1/))
end if
if ( associated(obs % qcrttov) ) then
  call radsim_write_field_nc(obs % qcrttov(1:np), 'qcrttov', dim, file_id, start=(/p1/))
end if
if ( associated(obs % viewid) ) then
  call radsim_write_field_nc(obs % viewid(1:np), 'viewid', dim, file_id, start=(/p1/))
end if
if ( associated(obs % scanline) ) then
  call radsim_write_field_nc(obs % scanline(1:np), 'scanline', dim, file_id, start=(/p1/))
end if
if ( associated(obs % scanpos) ) then
  call radsim_write_field_nc(obs % scanpos(1:np), 'scanpos', dim, file_id, start=(/p1/))
end if
if ( associated(obs % nsim_per_obs) ) then
  call radsim_write_field_nc(obs % nsim_per_obs(1:np), 'nprof_per_footprint', dim, file_id, start=(/p1/))
end if

end subroutine radsim_write_netcdf_obs_1d
