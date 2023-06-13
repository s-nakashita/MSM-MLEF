!-------------------------------------------------------------------------------
! Description:
!
!   Read a NCEP MSM binary file into a model structure.
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2019, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_read_msm(file_name, model_ntimes, model, file_name_ancil, file_name_ancil2)

use read_module, only: &
  read_header, &
  read_sig, &
  read_sfc, &
  read_flx, &
  levmax, &
  nwext, &
  lsoil, &
  nfldsfc, &
  nfldflx

use func_module, only: &
  calc_qs, &
  calc_rh

use radsim_mod_io

use radsim_mod_cfg, only : &
  output_mode, &
  temporal_data, &
  run_scatt, &
  mmr_snowrain, &
  clw_data, &
  ozone_data, &
  ir_addclouds

use radsim_mod_constants, only : &
  real64, &
  real32, &
  int32, &
  gravity, &
  R_air, &
  status_error, &
  output_verbose, &
  output_default

use radsim_mod_process

use radsim_mod_types, only : &
  model_type

use radsim_mod_functions, only : &
  time_in_minutes, &
  date_time_plus_minutes

implicit none

include 'radsim_error_report.interface'

! Subroutine args
character(len=400), intent(in)  :: file_name
integer,            intent(out) :: model_ntimes
type(model_type),   intent(out) :: model(:)
character(len=400), intent(in)  :: file_name_ancil
character(len=400), intent(in)  :: file_name_ancil2

! Local variables
integer :: i, j, ij, imodel, stat
integer(int32) :: file_id, dim_n_level, dim_n_lon, dim_n_lat, dim_n_time
integer, allocatable :: time(:)
character(len=400) :: time_units
integer :: ref_date_time(5)

integer :: field_list(nfields_rttov, 3)
! variables except for surftype
! field_list(:,1) : index
logical :: got_field(nfields_rttov)
integer :: nprofs, nlevels
integer :: iunit, icld
character(len=8) :: label(4)
integer :: idate(4), nflds
real(real64) :: si(levmax+1), sl(levmax)
real(real32) :: fhour, zhour, ext(nwext)
integer :: ids(255), iparam(nfldflx)
integer :: igrd1, jgrd1, levs, nonhyd
real(real64), allocatable :: lat(:), lon(:)
real(real64), allocatable :: mapf(:,:,:)
real(real64), allocatable :: dfld(:,:,:)
! arrays for getting cloud related variables
integer :: istrat, ier, me
integer, parameter :: ncld=1,nbin_rhcl=100,nlon_rhcl=2,nlat_rhcl=4,mcld_rhcl=4,nseal_rhcl=2
real(real64) :: rhcl(nbin_rhcl,nlon_rhcl,nlat_rhcl,mcld_rhcl,nseal_rhcl) !cl-rh relation
real(real64), allocatable :: f_ice(:,:)
real(real64), allocatable :: w(:,:),vvel(:,:)
real(real64), allocatable :: qs(:,:)
real(real64), allocatable :: prsl(:,:), prsi(:,:) ! in kPa
real(real64), allocatable :: cv(:), cvt(:), cvb(:)
real(real64), allocatable :: cldtot(:,:), cldcnv(:,:), cldsa(:,:)
integer, allocatable :: mbota(:,:), mtopa(:,:)

real(real64), pointer :: values1D(:), values2D(:,:)
real(real64), allocatable :: values_in2D(:,:), values_in3D(:,:,:)
real(real64) :: missingvalue, sfac, aoff
logical :: sfac_exists, aoff_exists, zero_flag
character(len=80) :: var_name
character(len=80) :: message
real(real64), parameter :: tol = 1.e-10_real64

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Define required field list
!-------------------------------------------------------------------------------

! See Annex in RTTOV Users Guide for required fields
field_list = -1

!---------------
! Surface fields
!---------------

field_list(field_rttov_tskin, 1) = 5   ! skin temperature
field_list(field_rttov_t2, 1)    = 36  ! temperature at 2m
field_list(field_rttov_q2, 1)    = 37  ! specific humidity at 2m
field_list(field_rttov_pstar, 1) = 38  ! surface pressure
! field_list(field_rttov_pstar, 2) = 'lnsp'  ! log of surface pressure
field_list(field_rttov_u10, 1)   = 34  ! 10m wind u component
field_list(field_rttov_v10, 1)   = 35  ! 10m wind v component

! Surface type usually needs to be derived from 2 fields - a land-sea mask and
! the seaice fraction. The priority is disabled in this case.

field_list(field_rttov_surftype, 1) = 32    ! LSM
field_list(field_rttov_surftype, 2) = 33    ! seaice cover
field_list(field_rttov_surftype, 3) = 1     ! surface geopotential

!---------
! Profiles
!---------

field_list(field_rttov_p, 1) = 7      ! pressure levels
field_list(field_rttov_t, 1) = 8     ! T
! field_list(field_rttov_t, 2) = 'theta'  ! theta
field_list(field_rttov_q, 1) = 4      ! specific humidity
! field_list(field_rttov_q, 2) = 'rh'     ! relative humidity

if ( ozone_data ) then
  field_list(field_rttov_ozone, 1) = 5  ! ozone
endif

! Profiles for running scattering code

if ( clw_data .or. run_scatt .or. ir_addclouds ) then
  field_list(field_rttov_qcl, 1) = 6   ! qcl (total cloud water, later converted into liquid water)
end if

if ( run_scatt .or. ir_addclouds ) then
  field_list(field_rttov_qcf, 1)   = 6 ! qcf (total cloud water, later converted into ice)
  field_list(field_rttov_cfrac, 1) = 6   ! cloud cover (total cloud water, later converted into fraction)
end if

if ( run_scatt ) then
  write(message,'(a,i0)') &
    'Currently not supported scatter simulation for NCEP MSM'
  call radsim_error_report(message, status_error)
  !field_list(field_rttov_rain, 1) = 'crwc' ! Rain mixing ratio
  !field_list(field_rttov_snow, 1) = 'cswc' ! Snow mixing ratio
  !mmr_snowrain = .true.                    ! Specify mass mixing ratio
end if

where (field_list(:,1) > 0)
  got_field = .false.
elsewhere
  got_field = .true.
end where

!! ------------------------------------------------------------------------
!! Fields not used by RTTOV, but if present are read in, interpolated, and
!! output with other fields. It is important to set "got_field" before this
!! to make these optional
!! ------------------------------------------------------------------------
!
!field_list(field_total_cc, 1)  = 'tcc' ! Total cloud cover
!field_list(field_low_cc, 1)    = 'lcc' ! Low cloud cover
!field_list(field_medium_cc, 1) = 'mcc' ! Medium cloud cover
!field_list(field_high_cc, 1)   = 'hcc' ! High cloud cover
!
!-------------------------------------------------------------------------------
! 2a. Read atmospheric data from primary netCDF file 
!-------------------------------------------------------------------------------

print '(a)', 'Reading ' // trim(file_name)

iunit = 11
open(iunit,file=trim(file_name),access='sequential',&
& form='unformatted', action='read')
icld = 1
call read_header(iunit, icld, label, idate, fhour, si, sl, ext, nflds)
igrd1 = int(ext(3))
jgrd1 = int(ext(4))
levs = int(ext(5))
nonhyd = int(ext(16))
allocate( lon(igrd1),lat(jgrd1) )
allocate( mapf(igrd1,jgrd1,3) )
allocate( dfld(igrd1,jgrd1,nflds) )

call read_sig(iunit,igrd1,jgrd1,levs,nflds,nonhyd,icld,&
& fhour, sl(1:levs), dfld, mapf, lat, lon)

close(iunit)

model_ntimes = 1
dim_n_lon = igrd1
dim_n_lat = jgrd1
nlevels = levs
nprofs = dim_n_lat * dim_n_lon

!----------------
! Grid definition
!----------------

imodel=1
model(imodel) % grid % type = 10 ! Mercator projection (See GRIB2 - GRID DEFINITION TEMPLATE 3.10)
if( .not. associated(model(imodel) % grid % phi) ) allocate(model(imodel) % grid % phi(dim_n_lat))
if( .not. associated(model(imodel) % grid % lambda) ) allocate(model(imodel) % grid % lambda(dim_n_lon))

model(imodel) % grid % nrows = dim_n_lat
model(imodel) % grid % ncols = dim_n_lon
model(imodel) % grid % phi(:) = lat(:)
model(imodel) % grid % lambda(:) = lon(:)
deallocate(lat, lon)

if ( output_mode >= output_verbose ) then
  print '(a,i0)', 'nprofiles = ', nprofs
  print '(a,i0)', 'nlevels = ', nlevels
  print '(a,i0)', 'nlatitudes = ', dim_n_lat
  print '(a,i0)', 'nlongitudes = ', dim_n_lon
  print '(a,f8.2)', 'latitude of first point (degrees) = ', model(1) % grid % phi(1)
  print '(a,f8.2)', 'longitude of first point (degrees) = ', model(1) % grid % lambda(1)
end if

!---------
! Set time
!---------

allocate( model(imodel) % validity_time(1,5) )
allocate( model(imodel) % data_time(1,5) )
model(imodel) % data_time(1,1) = idate(4)
model(imodel) % data_time(1,2) = idate(2)
model(imodel) % data_time(1,3) = idate(3)
model(imodel) % data_time(1,4) = idate(1)
model(imodel) % data_time(1,5) = 0
! Compute validity time 
model(imodel) % validity_time(1,:) = model(imodel) % data_time(1,:)
call date_time_plus_minutes(nint(fhour * 60), model(imodel) % validity_time(1,:))
model(imodel) % ref_time = time_in_minutes(model(imodel) % validity_time(1,:))

!------------
! Read fields
!------------

! All datasets in the atmospheric file have the same size
allocate(values_in3D(dim_n_lon, dim_n_lat, nlevels))
allocate(values_in2D(dim_n_lon, dim_n_lat))

! Search through the field_list created above and read first dataset found
! for each RTTOV field
do i = 1, nfields_rttov

  zero_flag = .true.
  select case(i)
    case(field_rttov_surftype)
      if (.not. associated(model(imodel) % zsurf)) allocate(model(imodel) % zsurf(nprofs))
      values1D => model(imodel) % zsurf
    case(field_rttov_p)
      if (.not. associated(model(imodel) % p)) allocate(model(imodel) % p(nprofs, nlevels))
      values2D => model(imodel) % p
      zero_flag = .false.
    case(field_rttov_t)
      if (.not. associated(model(imodel) % t)) allocate(model(imodel) % t(nprofs, nlevels))
      values2D => model(imodel) % t
      zero_flag = .false.
    case(field_rttov_q)
      if (.not. associated(model(imodel) % q)) allocate(model(imodel) % q(nprofs, nlevels))
      values2D => model(imodel) % q
      zero_flag = .false.
    case(field_rttov_ozone)
      if (.not. associated(model(imodel) % o3)) allocate(model(imodel) % o3(nprofs, nlevels))
      values2D => model(imodel) % o3
      zero_flag = .false.
    case(field_rttov_cfrac)
      if (.not. associated(model(imodel) % cfrac)) allocate(model(imodel) % cfrac(nprofs, nlevels))
      values2D => model(imodel) % cfrac
    case(field_rttov_qcl)
      if (.not. associated(model(imodel) % clw)) allocate(model(imodel) % clw(nprofs, nlevels))
      values2D => model(imodel) % clw
    case(field_rttov_qcf)
      if (.not. associated(model(imodel) % ciw)) allocate(model(imodel) % ciw(nprofs, nlevels))
      values2D => model(imodel) % ciw
    case default
      cycle
  end select

  if ( output_mode >= output_default ) then
    print '(a,i0,a,i0)', 'Reading ', nlevels, ' levels of data for variable ',i
  end if

  if(i.eq.field_rttov_surftype) then
    values_in2D = dfld(:,:,field_list(i,3))
    ! Copy data to model field
    values1D = reshape(values_in2D, (/ nprofs /))
  else
    do j=1,nlevels
      values_in3D(:,:,j) = dfld(:,:,2+(field_list(i,1)-1)*nlevels+j)
    end do
    ! Copy data to model field
    values2D = reshape(values_in3D, (/ nprofs, nlevels /))
  end if
  got_field(i) = .true.

end do

deallocate(values_in3D)
!
! Save required fields for getting clouds
!
allocate( w(nprofs,nlevels+1) )
allocate( f_ice(nprofs,nlevels) )
i=8*nlevels+2
do j=1,nlevels+1
  w(:,j) = reshape(dfld(:,:,i+j), (/nprofs/))
end do
i=9*nlevels+3
do j=1,nlevels
  f_ice(:,j) = reshape(dfld(:,:,i+j), (/nprofs/))
end do
deallocate(dfld)
!-------------------------------------------------------------------------------
! 2b. Read surface data from second (ancillary) file
!-------------------------------------------------------------------------------

print '(a)', 'Reading ' // trim(file_name_ancil)
iunit = 11
open(iunit,file=trim(file_name_ancil),access='sequential',&
& form='unformatted', action='read')
allocate( dfld(dim_n_lon,dim_n_lat,nfldflx) )
call read_flx(iunit,igrd1,jgrd1,dfld,ids,iparam,fhour,zhour)
close(iunit)

!------------
! Read fields
!------------

! Search through the field_list created above and read first dataset found
! for each RTTOV field
do i = 1, nfields_rttov

  zero_flag = .true.
  select case(i)
  case(field_rttov_tskin)
    if (.not. associated(model(imodel) % tskin)) allocate(model(imodel) % tskin(nprofs))
    values1D => model(imodel) % tskin
  case(field_rttov_t2)
    if (.not. associated(model(imodel) % t2)) allocate(model(imodel) % t2(nprofs))
    values1D => model(imodel) % t2
  case(field_rttov_q2)
    if (.not. associated(model(imodel) % q2)) allocate(model(imodel) % q2(nprofs))
    values1D => model(imodel) % q2
  case(field_rttov_pstar)
    if (.not. associated(model(imodel) % pstar)) allocate(model(imodel) % pstar(nprofs))
    values1D => model(imodel) % pstar
  case(field_rttov_u10)
    if (.not. associated(model(imodel) % u10)) allocate(model(imodel) % u10(nprofs))
    values1D => model(imodel) % u10
    zero_flag = .false.
  case(field_rttov_v10)
    if (.not. associated(model(imodel) % v10)) allocate(model(imodel) % v10(nprofs))
    values1D => model(imodel) % v10
    zero_flag = .false.
  case(field_rttov_surftype)
    if (.not. associated(model(imodel) % lsm)) allocate(model(imodel) % lsm(nprofs))
  !  values1D => model(imodel) % lsm
    if (.not. associated(model(imodel) % seaice)) allocate(model(imodel) % seaice(nprofs))
  !  values1D => model(imodel) % seaice
  !case('tcc')
  !  if (.not. associated(model(imodel) % total_cc)) allocate(model(imodel) % total_cc(nprofs))
  !  values1D => model(imodel) % total_cc
  !case('lcc')
  !  if (.not. associated(model(imodel) % low_cc)) allocate(model(imodel) % low_cc(nprofs))
  !  values1D => model(imodel) % low_cc
  !case('mcc')
  !  if (.not. associated(model(imodel) % medium_cc)) allocate(model(imodel) % medium_cc(nprofs))
  !  values1D => model(imodel) % medium_cc
  !case('hcc')
  !  if (.not. associated(model(imodel) % high_cc)) allocate(model(imodel) % high_cc(nprofs))
  !  values1D => model(imodel) % high_cc
  case default
    cycle
  end select

  if ( output_mode >= output_default ) then
    print '(a,i0)', 'Reading 1 level of data for variable ',i
  end if

  if(i.eq.field_rttov_surftype) then
    model(imodel) % lsm = reshape(dfld(:,:,field_list(i,1)), (/nprofs/))
    model(imodel) % seaice = reshape(dfld(:,:,field_list(i,2)), (/nprofs/))
  else
    values_in2d = dfld(:,:,field_list(i,1))
    ! Copy data to model field
    values1D = reshape(values_in2D, (/ nprofs /))
  end if
  got_field(i) = .true.
end do

deallocate(values_in2D)
deallocate(dfld)

!-----------------------------------
! Calculate cloud related variables
!-----------------------------------

! require cv, cvt, cvb from surface file
print '(a)', 'Reading ' // trim(file_name_ancil2)
iunit = 11
open(iunit,file=trim(file_name_ancil2),access='sequential',&
& form='unformatted', action='read')
allocate( dfld(dim_n_lon,dim_n_lat,nfldsfc) )
call read_sfc(iunit,igrd1,jgrd1,dfld)
close(iunit)
allocate( prsl(nprofs,nlevels), prsi(nprofs,nlevels+1) )
allocate( cv(nprofs), cvt(nprofs), cvb(nprofs) )
cv(:) = reshape(dfld(:,:,9), (/nprofs/))
cvb(:) = reshape(dfld(:,:,10), (/nprofs/))
cvt(:) = reshape(dfld(:,:,11), (/nprofs/))
prsl(:,:) = model(imodel) % p(:,:) * 1.0e-3
prsi(:,1) = model(imodel) % pstar(:) * 1.0e-3
do j=2,nlevels+1
  prsi(:,j) = 0.5*( prsl(:,j) + prsl(:,j-1) )
end do
allocate( vvel(nprofs,nlevels) )
allocate( model(imodel) % rh(nprofs,nlevels) )
allocate( qs(nprofs,nlevels) )
do j=1,nlevels
  do ij=1,nprofs
    call calc_qs(&
    model(imodel) % t(ij,j),&
    model(imodel) % p(ij,j),&
    qs(ij,j))
    call calc_rh(&
    model(imodel) % t(ij,j),&
    model(imodel) % q(ij,j),&
    model(imodel) % p(ij,j),&
    model(imodel) % rh(ij,j))
    vvel(ij,j) = -0.5*(w(ij,j)+w(ij,j+1)) * &
    & prsl(ij,j)*gravity/(R_air*model(imodel) % t(ij,j))
  end do
end do
allocate( cldtot(nprofs,nlevels) )
allocate( cldcnv(nprofs,nlevels) )
allocate( cldsa(nprofs,5) )
allocate( mbota(nprofs,3), mtopa(nprofs,3) )
!! get cl-rh relation from table
istrat = 1
me = 0
call crhtab(rhcl,ier,me)

allocate(lat(nprofs), lon(nprofs))
ij=1
do j=1,dim_n_lat
  do i=1,dim_n_lon
    lat(ij) = model(imodel) % grid % phi(j)
    lon(ij) = model(imodel) % grid % lambda(i)
    ij = ij + 1
  end do
end do
call getclds(nprofs,nprofs,nlevels,&
& model(imodel) % t, model(imodel) % q, &
& vvel, model(imodel) % rh, qs, model(imodel) % lsm, &
& lon, lat, &
& cv, cvt, cvb, rhcl, istrat, &
& prsi, prsl, cldtot, cldcnv, cldsa, &
& mbota, mtopa, model(imodel) % ciw, ncld &
&)
cldtot = cldtot + cldcnv
model(imodel) % cfrac(:,:) = cldtot
where(cldtot .le. 0.0)
  model(imodel) % cfrac = 0.0
elsewhere(cldtot .ge. 1.0)
  model(imodel) % cfrac = 1.0
end where
deallocate(lat, lon)

! temporary arrays
allocate( values_in2D(nprofs, nlevels) )
values_in2D = model(imodel) % ciw
model(imodel) % ciw = values_in2D * f_ice
model(imodel) % clw = values_in2D - model(imodel) % ciw

!-------------------------------------------------------------------------------
! 3. Derivations
!-------------------------------------------------------------------------------

do imodel = 1, model_ntimes
  if ( associated(model(imodel) % p) ) then
    model(imodel) % nprofs = size(model(imodel) % p, dim=1)
    model(imodel) % nlevels = size(model(imodel) % p, dim=2)
  end if

  ! LSM from seaice fraction if not present

  if ( .not. associated(model(imodel) % lsm) .and. &
       associated(model(imodel) % seaice) ) then
    print '(a)', 'No LSM: Deriving LSM from seaice fraction field'
    allocate(model(imodel) % lsm(nprofs))
    where(model(imodel) % seaice == model(imodel) % rmdi)
      model(imodel) % lsm = 1.0
    elsewhere
      model(imodel) % lsm = 0.0
    end where
  end if
end do

!-------------------------------------------------------------------------------
! 4. Check fields
!-------------------------------------------------------------------------------

do i = 1, size(got_field)
  if ( .not. got_field(i) ) then
    call radsim_error_report( &
      'No data in files for required RTTOV field: ' // field_names_rttov(i), &
      status_error)
  end if
end do

end subroutine radsim_read_msm
