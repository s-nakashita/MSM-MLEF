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
module rttov_hdf_opt_param_io
#include "throw.h"
use parkind1
!use rttov_print_mod
use rttov_hdf_mod
!use rttov_hdf_opt_param_def
!use rttov_hdf_opt_param_mem
use rttov_types

use hdf5
implicit none
#include "rttov_errorreport.interface"
private
public :: rttov_hdf_opt_param_init
public :: rttov_hdf_opt_param_rh
public :: rttov_hdf_opt_param_wh


contains


subroutine rttov_hdf_opt_param_init(x)
type(rttov_opt_param),intent(inout)::x

  nullify(x%ABS)
  nullify(x%SCA)
  nullify(x%BPR)
  x%NMOM=0_jpim
  nullify(x%LEGCOEF)
  nullify(x%PHANGLE)
  nullify(x%PHA)
end subroutine

subroutine rttov_hdf_opt_param_rh(x,lun,err)
type(rttov_opt_param),intent(out)::x
integer(hid_t),intent(in)::lun
integer(kind=jpim),intent(out)::err
character(len=lensh)::sname
logical :: lext
TRY

call rttov_hdf_opt_param_init(x)

sname='ABS'
call read_array_hdf(lun,sname,err,pr2=x%ABS)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='SCA'
call read_array_hdf(lun,sname,err,pr2=x%SCA)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='BPR'
call read_array_hdf(lun,sname,err,pr2=x%BPR)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='NMOM'
call read_array_hdf(lun,sname,err,i0=x%NMOM)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LEGCOEF'
call read_array_hdf(lun,sname,err,pr3=x%LEGCOEF)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='PHANGLE'
call read_array_hdf(lun,sname,err,pr1=x%PHANGLE)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='PHA'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,pr3=x%PHA)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

err=0_jpim
CATCH
end subroutine

subroutine rttov_hdf_opt_param_wh(x,lun,err,compress,force_double)
type(rttov_opt_param),intent(in)::x
integer(hid_t),intent(in)::lun
integer(kind=jpim),intent(out)::err
logical,intent(in),optional::compress
logical,intent(in),optional::force_double
character(len=lensh)::sname

TRY
sname='ABS'
if(associated(x%ABS))then
  call write_array_hdf(lun,sname,&
    & 'Absorption coef',&
    & err,r2=x%ABS , units = 'km-1' ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='SCA'
if(associated(x%SCA))then
  call write_array_hdf(lun,sname,&
    & 'Scattering coef',&
    & err,r2=x%SCA , units = 'km-1' ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='BPR'
if(associated(x%BPR))then
  call write_array_hdf(lun,sname,&
    & 'Back scattering parameter (0-1)',&
    & err,r2=x%BPR , units = 'n/a' ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='NMOM'
call write_array_hdf(lun,sname,&
  & 'Number of Legendre coefficients in phase function expansions',&
  & err,i0=x%NMOM , units = 'n/a'  )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LEGCOEF'
if(associated(x%LEGCOEF))then
  call write_array_hdf(lun,sname,&
    & 'Phase function Legendre coefficients',&
    & err,r3=x%LEGCOEF , units = 'n/a' ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='PHANGLE'
if(associated(x%PHANGLE))then
  call write_array_hdf(lun,sname,&
    & 'Angles over which phase functions are defined',&
    & err,r1=x%PHANGLE , units = 'degrees'  )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='PHA'
if(associated(x%PHA))then
  call write_array_hdf(lun,sname,&
    & 'Phase functions',&
    & err,r3=x%PHA , units = 'n/a' ,compress=compress ,force_double=force_double)
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

err=0_jpim
CATCH
end subroutine

end module
