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
module rttov_hdf_rttov_coef_pcc_io
#include "throw.h"
use parkind1
!use rttov_print_mod
use rttov_hdf_mod
!use rttov_hdf_rttov_coef_pcc_def
!use rttov_hdf_rttov_coef_pcc_mem
use rttov_types

use hdf5
implicit none
#include "rttov_errorreport.interface"
private
public :: rttov_hdf_rttov_coef_pcc_init
public :: rttov_hdf_rttov_coef_pcc_rh
public :: rttov_hdf_rttov_coef_pcc_wh


contains


subroutine rttov_hdf_rttov_coef_pcc_init(x)
type(rttov_coef_pccomp),intent(inout)::x

  x%FMV_PC_COMP_PC=0_jpim
  x%FMV_PC_CLD=0_jpim
  x%FMV_PC_AER=0_jpim
  x%FMV_PC_NAER_TYPES=0_jpim
  x%FMV_PC_NLTE=0_jpim
  x%FMV_PC_MSETS=0_jpim
  x%FMV_PC_BANDS=0_jpim
  x%FMV_PC_MNUM=0_jpim
  x%FMV_PC_MCHN=0_jpim
  x%FMV_PC_NCHN=0_jpim
  x%FMV_PC_NCHN_NOISE=0_jpim
  x%FMV_PC_NCHE=0_jpim
  x%FMV_PC_GAS=0_jpim
  x%FMV_PC_GAS_LIM=0_jpim
  nullify(x%FMV_PC_SETS)
  nullify(x%EMISS_CHN)
  nullify(x%EMISS_C1)
  nullify(x%EMISS_C2)
  nullify(x%EMISS_C3)
  nullify(x%EMISS_C4)
  nullify(x%EMISS_C5)
  nullify(x%EMISS_C6)
  nullify(x%EMISS_C7)
  nullify(x%EMISS_C8)
  nullify(x%EMISS_C9)
  x%FMV_PC_NLEV=0_jpim
  nullify(x%REF_PC_PRFL_P)
  nullify(x%REF_PC_PRFL_MR)
  nullify(x%LIM_PC_PRFL_TMIN)
  nullify(x%LIM_PC_PRFL_TMAX)
  nullify(x%LIM_PC_PRFL_QMIN)
  nullify(x%LIM_PC_PRFL_QMAX)
  nullify(x%LIM_PC_PRFL_OZMIN)
  nullify(x%LIM_PC_PRFL_OZMAX)
  nullify(x%LIM_PC_PRFL_GASMIN)
  nullify(x%LIM_PC_PRFL_GASMAX)
  nullify(x%LIM_PC_PRFL_AERMIN)
  nullify(x%LIM_PC_PRFL_AERMAX)
  x%LIM_PC_PRFL_PMIN=0._jprb
  x%LIM_PC_PRFL_PMAX=0._jprb
  x%LIM_PC_PRFL_TSMIN=0._jprb
  x%LIM_PC_PRFL_TSMAX=0._jprb
  x%LIM_PC_PRFL_SKMIN=0._jprb
  x%LIM_PC_PRFL_SKMAX=0._jprb
  x%LIM_PC_PRFL_WSMIN=0._jprb
  x%LIM_PC_PRFL_WSMAX=0._jprb
  nullify(x%CO2_PC_REF)
  nullify(x%N2O_PC_REF)
  nullify(x%CO_PC_REF)
  nullify(x%CH4_PC_REF)
  nullify(x%CO2_PC_MIN)
  nullify(x%N2O_PC_MIN)
  nullify(x%CO_PC_MIN)
  nullify(x%CH4_PC_MIN)
  nullify(x%CO2_PC_MAX)
  nullify(x%N2O_PC_MAX)
  nullify(x%CO_PC_MAX)
  nullify(x%CH4_PC_MAX)
  nullify(x%NOISE_IN)
  nullify(x%NOISE)
  nullify(x%NOISE_R)
  nullify(x%FF_ORI_CHN_IN)
  nullify(x%FF_CWN_IN)
  nullify(x%FF_BCO_IN)
  nullify(x%FF_BCS_IN)
  nullify(x%PLANCK1_IN)
  nullify(x%PLANCK2_IN)
  nullify(x%PCREG)
  nullify(x%EIGEN)
end subroutine

subroutine rttov_hdf_rttov_coef_pcc_rh(x,lun,err)
type(rttov_coef_pccomp),intent(out)::x
integer(hid_t),intent(in)::lun
integer(kind=jpim),intent(out)::err
character(len=lensh)::sname
logical :: lext
TRY

call rttov_hdf_rttov_coef_pcc_init(x)

sname='FMV_PC_COMP_PC'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_COMP_PC)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_CLD'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_CLD)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_AER'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_AER)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='FMV_PC_NAER_TYPES'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_NAER_TYPES)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='FMV_PC_NLTE'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_NLTE)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='FMV_PC_MSETS'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_MSETS)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_BANDS'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_BANDS)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_MNUM'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_MNUM)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_MCHN'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_MCHN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_NCHN'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_NCHN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_NCHN_NOISE'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_NCHN_NOISE)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_NCHE'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_NCHE)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_GAS'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_GAS)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='FMV_PC_GAS_LIM'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_GAS_LIM)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='FMV_PC_SETS'
call read_array_hdf(lun,sname,err,pi1=x%FMV_PC_SETS)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_CHN'
call read_array_hdf(lun,sname,err,pi1=x%EMISS_CHN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C1'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C1)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C2'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C2)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C3'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C3)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C4'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C4)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C5'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C5)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C6'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C6)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C7'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C7)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C8'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C8)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='EMISS_C9'
call read_array_hdf(lun,sname,err,pr1=x%EMISS_C9)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FMV_PC_NLEV'
call read_array_hdf(lun,sname,err,i0=x%FMV_PC_NLEV)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='REF_PC_PRFL_P'
call read_array_hdf(lun,sname,err,pr1=x%REF_PC_PRFL_P)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='REF_PC_PRFL_MR'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,pr2=x%REF_PC_PRFL_MR)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='LIM_PC_PRFL_TMIN'
call read_array_hdf(lun,sname,err,pr1=x%LIM_PC_PRFL_TMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_TMAX'
call read_array_hdf(lun,sname,err,pr1=x%LIM_PC_PRFL_TMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_QMIN'
call read_array_hdf(lun,sname,err,pr1=x%LIM_PC_PRFL_QMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_QMAX'
call read_array_hdf(lun,sname,err,pr1=x%LIM_PC_PRFL_QMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_OZMIN'
call read_array_hdf(lun,sname,err,pr1=x%LIM_PC_PRFL_OZMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_OZMAX'
call read_array_hdf(lun,sname,err,pr1=x%LIM_PC_PRFL_OZMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_GASMIN'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,pr2=x%LIM_PC_PRFL_GASMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='LIM_PC_PRFL_GASMAX'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,pr2=x%LIM_PC_PRFL_GASMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='LIM_PC_PRFL_AERMIN'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,pr2=x%LIM_PC_PRFL_AERMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='LIM_PC_PRFL_AERMAX'
call h5lexists_f( lun, sname, lext, err )
if( lext ) then
call read_array_hdf(lun,sname,err,pr2=x%LIM_PC_PRFL_AERMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))
endif

sname='LIM_PC_PRFL_PMIN'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_PMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_PMAX'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_PMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_TSMIN'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_TSMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_TSMAX'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_TSMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_SKMIN'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_SKMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_SKMAX'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_SKMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_WSMIN'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_WSMIN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='LIM_PC_PRFL_WSMAX'
call read_array_hdf(lun,sname,err,r0=x%LIM_PC_PRFL_WSMAX)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))













sname='NOISE_IN'
call read_array_hdf(lun,sname,err,pr1=x%NOISE_IN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='NOISE'
call read_array_hdf(lun,sname,err,pr1=x%NOISE)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))


sname='FF_ORI_CHN_IN'
call read_array_hdf(lun,sname,err,pi1=x%FF_ORI_CHN_IN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FF_CWN_IN'
call read_array_hdf(lun,sname,err,pr1=x%FF_CWN_IN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FF_BCO_IN'
call read_array_hdf(lun,sname,err,pr1=x%FF_BCO_IN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))

sname='FF_BCS_IN'
call read_array_hdf(lun,sname,err,pr1=x%FF_BCS_IN)
THROWM(err.ne.0,"CANNOT READ "//trim(sname))





err=0_jpim
CATCH
end subroutine

subroutine rttov_hdf_rttov_coef_pcc_wh(x,lun,err,compress,force_double)
type(rttov_coef_pccomp),intent(in)::x
integer(hid_t),intent(in)::lun
integer(kind=jpim),intent(out)::err
logical,intent(in),optional::compress
logical,intent(in),optional::force_double
character(len=lensh)::sname

TRY
sname='FMV_PC_COMP_PC'
call write_array_hdf(lun,sname,&
  & 'Principal components coefficient file version number',&
  & err,i0=x%FMV_PC_COMP_PC   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_CLD'
call write_array_hdf(lun,sname,&
  & 'Principal components cloud coefficient flag',&
  & err,i0=x%FMV_PC_CLD   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_AER'
call write_array_hdf(lun,sname,&
  & 'Principal components aerosol coefficient flag',&
  & err,i0=x%FMV_PC_AER   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_NAER_TYPES'
call write_array_hdf(lun,sname,&
  & 'Number of aerosol types',&
  & err,i0=x%FMV_PC_NAER_TYPES   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_NLTE'
call write_array_hdf(lun,sname,&
  & 'Principal components NLTE coefficient flag',&
  & err,i0=x%FMV_PC_NLTE   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_MSETS'
call write_array_hdf(lun,sname,&
  & 'Maximum number of regression sets',&
  & err,i0=x%FMV_PC_MSETS   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_BANDS'
call write_array_hdf(lun,sname,&
  & 'Number of spectral bands',&
  & err,i0=x%FMV_PC_BANDS   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_MNUM'
call write_array_hdf(lun,sname,&
  & 'Maximum number of eigenvectors',&
  & err,i0=x%FMV_PC_MNUM   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_MCHN'
call write_array_hdf(lun,sname,&
  & 'Maximum number of channels',&
  & err,i0=x%FMV_PC_MCHN   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_NCHN'
call write_array_hdf(lun,sname,&
  & 'Number of channels for which eigenvectors are available',&
  & err,i0=x%FMV_PC_NCHN   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_NCHN_NOISE'
call write_array_hdf(lun,sname,&
  & 'Number of channels for which instrument noise is available',&
  & err,i0=x%FMV_PC_NCHN_NOISE   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_NCHE'
call write_array_hdf(lun,sname,&
  & 'Number of channels for which emissisity coefs are available',&
  & err,i0=x%FMV_PC_NCHE   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_GAS'
call write_array_hdf(lun,sname,&
  & 'Number of gases for which a reference profile is given',&
  & err,i0=x%FMV_PC_GAS   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_GAS_LIM'
call write_array_hdf(lun,sname,&
  & 'Number of gases for which min/max limit profiles are given',&
  & err,i0=x%FMV_PC_GAS_LIM   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='FMV_PC_SETS'
if(associated(x%FMV_PC_SETS))then
  call write_array_hdf(lun,sname,&
    & 'Number of regression sets in each band',&
    & err,i1=x%FMV_PC_SETS   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_CHN'
if(associated(x%EMISS_CHN))then
  call write_array_hdf(lun,sname,&
    & 'Number of channels for which emissivity coefficients are',&
    & err,i1=x%EMISS_CHN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C1'
if(associated(x%EMISS_C1))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C1   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C2'
if(associated(x%EMISS_C2))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C2   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C3'
if(associated(x%EMISS_C3))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C3   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C4'
if(associated(x%EMISS_C4))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C4   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C5'
if(associated(x%EMISS_C5))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C5   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C6'
if(associated(x%EMISS_C6))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C6   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C7'
if(associated(x%EMISS_C7))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C7   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C8'
if(associated(x%EMISS_C8))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C8   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='EMISS_C9'
if(associated(x%EMISS_C9))then
  call write_array_hdf(lun,sname,&
    & 'Emissivity coefficient',&
    & err,r1=x%EMISS_C9   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='FMV_PC_NLEV'
call write_array_hdf(lun,sname,&
  & 'Number of reference profile levels',&
  & err,i0=x%FMV_PC_NLEV   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='REF_PC_PRFL_P'
if(associated(x%REF_PC_PRFL_P))then
  call write_array_hdf(lun,sname,&
    & 'pressure  (hPa)       (levels)',&
    & err,r1=x%REF_PC_PRFL_P   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='REF_PC_PRFL_MR'
if(associated(x%REF_PC_PRFL_MR))then
  call write_array_hdf(lun,sname,&
    & 'mixing ratio (ppmv)   (levels)',&
    & err,r2=x%REF_PC_PRFL_MR  ,compress=compress )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_TMIN'
if(associated(x%LIM_PC_PRFL_TMIN))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: temperature',&
    & err,r1=x%LIM_PC_PRFL_TMIN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_TMAX'
if(associated(x%LIM_PC_PRFL_TMAX))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: temperature',&
    & err,r1=x%LIM_PC_PRFL_TMAX   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_QMIN'
if(associated(x%LIM_PC_PRFL_QMIN))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: water vapour',&
    & err,r1=x%LIM_PC_PRFL_QMIN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_QMAX'
if(associated(x%LIM_PC_PRFL_QMAX))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: water vapour',&
    & err,r1=x%LIM_PC_PRFL_QMAX   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_OZMIN'
if(associated(x%LIM_PC_PRFL_OZMIN))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: ozone',&
    & err,r1=x%LIM_PC_PRFL_OZMIN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_OZMAX'
if(associated(x%LIM_PC_PRFL_OZMAX))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: ozone',&
    & err,r1=x%LIM_PC_PRFL_OZMAX   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_GASMIN'
if(associated(x%LIM_PC_PRFL_GASMIN))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: additional gases',&
    & err,r2=x%LIM_PC_PRFL_GASMIN  ,compress=compress )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_GASMAX'
if(associated(x%LIM_PC_PRFL_GASMAX))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: additional gases',&
    & err,r2=x%LIM_PC_PRFL_GASMAX  ,compress=compress )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_AERMIN'
if(associated(x%LIM_PC_PRFL_AERMIN))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: aerosols',&
    & err,r2=x%LIM_PC_PRFL_AERMIN  ,compress=compress )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_AERMAX'
if(associated(x%LIM_PC_PRFL_AERMAX))then
  call write_array_hdf(lun,sname,&
    & 'Profile limit: aerosols',&
    & err,r2=x%LIM_PC_PRFL_AERMAX  ,compress=compress )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='LIM_PC_PRFL_PMIN'
call write_array_hdf(lun,sname,&
  & 'Profile limit: Surface pressure',&
  & err,r0=x%LIM_PC_PRFL_PMIN   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_PMAX'
call write_array_hdf(lun,sname,&
  & 'Profile limit: Surface pressure',&
  & err,r0=x%LIM_PC_PRFL_PMAX   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_TSMIN'
call write_array_hdf(lun,sname,&
  & 'Profile limit: Surface temperature',&
  & err,r0=x%LIM_PC_PRFL_TSMIN   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_TSMAX'
call write_array_hdf(lun,sname,&
  & 'Profile limit: Surface temperature',&
  & err,r0=x%LIM_PC_PRFL_TSMAX   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_SKMIN'
call write_array_hdf(lun,sname,&
  & 'Profile limit: Skin temperature',&
  & err,r0=x%LIM_PC_PRFL_SKMIN   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_SKMAX'
call write_array_hdf(lun,sname,&
  & 'Profile limit: Skin temperature',&
  & err,r0=x%LIM_PC_PRFL_SKMAX   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_WSMIN'
call write_array_hdf(lun,sname,&
  & 'Profile limit: 10m wind speed',&
  & err,r0=x%LIM_PC_PRFL_WSMIN   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))

sname='LIM_PC_PRFL_WSMAX'
call write_array_hdf(lun,sname,&
  & 'Profile limit: 10m wind speed',&
  & err,r0=x%LIM_PC_PRFL_WSMAX   )
THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))













sname='NOISE_IN'
if(associated(x%NOISE_IN))then
  call write_array_hdf(lun,sname,&
    & 'Noise values for the channels whose radiances are reconstructed using principal components',&
    & err,r1=x%NOISE_IN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='NOISE'
if(associated(x%NOISE))then
  call write_array_hdf(lun,sname,&
    & 'Noise values for the channels whose radiances are used as predictors in the computation of principal components',&
    & err,r1=x%NOISE   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif


sname='FF_ORI_CHN_IN'
if(associated(x%FF_ORI_CHN_IN))then
  call write_array_hdf(lun,sname,&
    & 'Instrument Noise, Original channel number',&
    & err,i1=x%FF_ORI_CHN_IN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='FF_CWN_IN'
if(associated(x%FF_CWN_IN))then
  call write_array_hdf(lun,sname,&
    & 'Instrument Noise, central wave number of reconstructed radiances',&
    & err,r1=x%FF_CWN_IN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='FF_BCO_IN'
if(associated(x%FF_BCO_IN))then
  call write_array_hdf(lun,sname,&
    & 'Instrument Noise, band correction offset (K)',&
    & err,r1=x%FF_BCO_IN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif

sname='FF_BCS_IN'
if(associated(x%FF_BCS_IN))then
  call write_array_hdf(lun,sname,&
    & 'Instrument Noise, band correction slope (K/K)',&
    & err,r1=x%FF_BCS_IN   )
  THROWM(err.ne.0,"CANNOT WRITE "//trim(sname))
endif





err=0_jpim
CATCH
end subroutine

end module
