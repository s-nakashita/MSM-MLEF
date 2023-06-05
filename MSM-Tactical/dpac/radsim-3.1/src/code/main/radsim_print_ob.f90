!-------------------------------------------------------------------------------
! Description:
!
!   Print one simulated observation.
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

subroutine radsim_print_ob(obno,obs)

use radsim_mod_types, only : &
  obs_type

implicit none

integer,        intent(in) :: obno
type(obs_type), intent(in) :: obs

print '(a)',      '-----'
print '(a,i7,a)', 'ob no', obno, ':'
if ( associated(obs % lat) ) then
  print '(a10,f8.2)', 'lat = ', obs % lat(obno)
end if
if ( associated(obs % lon) ) then
  print '(a10,f8.2)', 'lon = ', obs % lon(obno)
end if
if ( associated(obs % zsurf) ) then
  print '(a10,f8.2)', 'zsurf = ', obs % zsurf(obno)
end if
if ( associated(obs % satzen) ) then
  print '(a10,f8.2)', 'satzen = ', obs % satzen(obno)
end if
if ( associated(obs % satazim) ) then
  print '(a10,f8.2)', 'satazim = ', obs % satazim(obno)
end if
if ( associated(obs % solzen) ) then
  print '(a10,f8.2)', 'solzen = ', obs % solzen(obno)
end if
if ( associated(obs % solazim) ) then
  print '(a10,f8.2)', 'solazim = ', obs % solazim(obno)
end if
if ( associated(obs % year) ) then
  print '(a10,i8)', 'year = ', obs % year(obno)
end if
if ( associated(obs % month) ) then
  print '(a10,i8)', 'month = ', obs % month(obno)
end if
if ( associated(obs % day) ) then
  print '(a10,i8)', 'day = ', obs % day(obno)
end if
if ( associated(obs % hour) ) then
  print '(a10,i8)', 'hour = ', obs % hour(obno)
end if
if ( associated(obs % minute) ) then
  print '(a10,i8)', 'minute = ', obs % minute(obno)
end if
if ( associated(obs % lsm) ) then
  print '(a10,f8.2)', 'lsm = ', obs % lsm(obno)
end if
if ( associated(obs % rtsurf) ) then
  print '(a10,i8)', 'rtsurf = ', obs % rtsurf(obno)
end if
if ( associated(obs % qcflags) ) then
  print '(a10,i8)', 'qcflags = ', obs % qcflags(obno)
end if
if ( associated(obs % qcrttov) ) then
  print '(a10,i8)', 'qcrttov = ', obs % qcrttov(obno)
end if
if ( associated(obs % qcinfo) ) then
  print '(a10,i8)', 'qcinfo = ', obs % qcinfo(obno)
end if
if ( associated(obs % channels) ) then
  print '(a)', 'channels:'
  print '(10i8)', obs % channels(:)
end if
if ( associated(obs % wavenumbers) ) then
  print '(a)', 'wavenumbers:'
  print '(10f8.3)', obs % wavenumbers(:)
end if
if ( associated(obs % emiss) ) then
  print '(a)', 'emiss:'
  print '(10f8.3)', obs % emiss(obno,:)
end if
if ( associated(obs % brdf) ) then
  print '(a)', 'brdf:'
  print '(10f8.3)', obs % brdf(obno,:)
end if
if ( associated(obs % bt) ) then
  print '(a)', 'bt:'
  print '(10f8.3)', obs % bt(obno,:)
end if
if ( associated(obs % refl) ) then
  print '(a)', 'refl:'
  print '(10f8.3)', obs % refl(obno,:)
end if
if ( associated(obs % radiance) ) then
  print '(a)', 'radiance:'
  print '(10f8.3)', obs % radiance(obno,:)
end if
if ( associated(obs % cads_height_assignment) ) then
  print '(a)', 'cads_height_assignment:'
  print '(10f8.3)', obs % cads_height_assignment(obno,:)
end if
if ( associated(obs % geometric_height) ) then
  print '(a)', 'geometric_height:'
  print '(10f8.3)', obs % geometric_height(obno,:)
end if
if ( associated(obs % tjac) ) then
  print '(a)', 'tjac:'
  print '(10f8.3)', obs % tjac(obno,:,:)
end if
if ( associated(obs % qjac) ) then
  print '(a)', 'qjac:'
  print '(10f8.3)', obs % qjac(obno,:,:)
end if
if ( associated(obs % o3jac) ) then
  print '(a)', 'o3jac:'
  print '(10f8.3)', obs % o3jac(obno,:,:)
end if
if ( associated(obs % tskinjac) ) then
  print '(a)', 'tskinjac:'
  print '(10f8.3)', obs % tskinjac(obno,:)
end if
if ( associated(obs % wind10mujac) ) then
  print '(a)', 'wind10mujac:'
  print '(10f8.3)', obs % wind10mujac(obno,:)
end if
if ( associated(obs % wind10mvjac) ) then
  print '(a)', 'wind10mvjac:'
  print '(10f8.3)', obs % wind10mvjac(obno,:)
end if
if ( associated(obs % emissjac) ) then
  print '(a)', 'emissjac:'
  print '(10f8.3)', obs % emissjac(obno,:)
end if
if ( associated(obs % trans) ) then
  print '(a)', 'trans:'
  print '(10f8.3)', obs % trans(obno,:,:)
end if

end subroutine radsim_print_ob
