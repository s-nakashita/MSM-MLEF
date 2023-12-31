!
! Automatically generated by ./mod_rw_for_rttov_types.pl
! Do not edit
!
! Copyright:
!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 7 December 2016, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, DWD and MeteoFrance.
!
!    Copyright 2016, EUMETSAT, All Rights Reserved.
!
module rttov_hdf_transmission_io
#include "throw.h"
use parkind1
!use rttov_print_mod
use rttov_hdf_mod
!use rttov_hdf_transmission_def
!use rttov_hdf_transmission_mem
use rttov_types

use hdf5
implicit none
#include "rttov_errorreport.interface"
private
public :: rttov_hdf_transmission_init
public :: rttov_hdf_transmission_rh
public :: rttov_hdf_transmission_wh


contains


subroutine rttov_hdf_transmission_init(x)
type(rttov_transmission),intent(inout)::x

  nullify(x%TAU_TOTAL)
  nullify(x%TAU_LEVELS)
  nullify(x%TAUSUN_TOTAL_PATH2)
  nullify(x%TAUSUN_LEVELS_PATH2)
  nullify(x%TAUSUN_TOTAL_PATH1)
  nullify(x%TAUSUN_LEVELS_PATH1)
  nullify(x%TAU_TOTAL_CLD)
  nullify(x%TAU_LEVELS_CLD)
end subroutine

subroutine rttov_hdf_transmission_rh(x,lun,err)
type(rttov_transmission),intent(out)::x
integer(hid_t),intent(in)::lun
integer(kind=jpim),intent(out)::err
character(len=lensh)::sname
logical :: lext
TRY

call rttov_hdf_transmission_init(x)

sname='TAU_TOTAL'
call read_array_hdf(lun,sname,err,pr1=x%TAU_TOTAL)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAU_LEVELS'
call read_array_hdf(lun,sname,err,pr2=x%TAU_LEVELS)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAUSUN_TOTAL_PATH2'
call read_array_hdf(lun,sname,err,pr1=x%TAUSUN_TOTAL_PATH2)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAUSUN_LEVELS_PATH2'
call read_array_hdf(lun,sname,err,pr2=x%TAUSUN_LEVELS_PATH2)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAUSUN_TOTAL_PATH1'
call read_array_hdf(lun,sname,err,pr1=x%TAUSUN_TOTAL_PATH1)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAUSUN_LEVELS_PATH1'
call read_array_hdf(lun,sname,err,pr2=x%TAUSUN_LEVELS_PATH1)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAU_TOTAL_CLD'
call read_array_hdf(lun,sname,err,pr1=x%TAU_TOTAL_CLD)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='TAU_LEVELS_CLD'
call read_array_hdf(lun,sname,err,pr2=x%TAU_LEVELS_CLD)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

err=0_jpim
CATCH
end subroutine

subroutine rttov_hdf_transmission_wh(x,lun,err,compress,force_double)
type(rttov_transmission),intent(in)::x
integer(hid_t),intent(in)::lun
integer(kind=jpim),intent(out)::err
logical,intent(in),optional::compress
logical,intent(in),optional::force_double
character(len=lensh)::sname

TRY
sname='TAU_TOTAL'
if(associated(x%TAU_TOTAL))then
  call write_array_hdf(lun,sname,&
    & 'Surface-satellite transmittance (channels)',&
    & err,r1=x%TAU_TOTAL   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAU_LEVELS'
if(associated(x%TAU_LEVELS))then
  call write_array_hdf(lun,sname,&
    & 'Level-satellite transmittance (levels,channels)',&
    & err,r2=x%TAU_LEVELS  ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAUSUN_TOTAL_PATH2'
if(associated(x%TAUSUN_TOTAL_PATH2))then
  call write_array_hdf(lun,sname,&
    & 'Sun-surface-satellite solar transmittance',&
    & err,r1=x%TAUSUN_TOTAL_PATH2   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAUSUN_LEVELS_PATH2'
if(associated(x%TAUSUN_LEVELS_PATH2))then
  call write_array_hdf(lun,sname,&
    & 'Sun-level-satellite solar transmittance for each level',&
    & err,r2=x%TAUSUN_LEVELS_PATH2  ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAUSUN_TOTAL_PATH1'
if(associated(x%TAUSUN_TOTAL_PATH1))then
  call write_array_hdf(lun,sname,&
    & 'Surface-satellite solar transmittance',&
    & err,r1=x%TAUSUN_TOTAL_PATH1   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAUSUN_LEVELS_PATH1'
if(associated(x%TAUSUN_LEVELS_PATH1))then
  call write_array_hdf(lun,sname,&
    & 'Level-satellite solar transmittance for each level',&
    & err,r2=x%TAUSUN_LEVELS_PATH1  ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAU_TOTAL_CLD'
if(associated(x%TAU_TOTAL_CLD))then
  call write_array_hdf(lun,sname,&
    & 'Surface-satellite cloud-only transmittance (channels)',&
    & err,r1=x%TAU_TOTAL_CLD   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='TAU_LEVELS_CLD'
if(associated(x%TAU_LEVELS_CLD))then
  call write_array_hdf(lun,sname,&
    & 'Level-satellite cloud-only transmittance (levels,channels)',&
    & err,r2=x%TAU_LEVELS_CLD  ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

err=0_jpim
CATCH
end subroutine

end module
