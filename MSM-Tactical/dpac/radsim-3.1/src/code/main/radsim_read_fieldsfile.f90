!-------------------------------------------------------------------------------
! Description:
!
!   Read a Met Office UM fieldsfile into a model structure.
!
! Reference:
!
!   Unified Model Documentation Paper F3, Met Office internal publication.
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

subroutine radsim_read_fieldsfile( &
  file_name,    & ! in
  model_ntimes, & ! out
  model         ) ! out

use radsim_mod_cfg, only : &
  output_mode, &
  temporal_data

use radsim_mod_constants, only : &
  real32, &
  real64, &
  int64, &
  status_warning

use radsim_mod_process, only : &
  nfields_rttov, &
  field_names_rttov

use radsim_mod_types, only : &
  model_type, &
  ff_hd_type

use radsim_mod_functions, only : &
  time_in_minutes

implicit none

include 'radsim_calc_meto_plevels.interface'
include 'radsim_check_ff_packing.interface'
include 'radsim_dealloc_ff_hd.interface'
include 'radsim_error_report.interface'
include 'radsim_read_ff_headers.interface'
include 'radsim_set_stash.interface'
include 'radsim_store_stash.interface'

! Subroutine args
character(len=400), intent(in)  :: file_name
integer,            intent(out) :: model_ntimes
type(model_type),   intent(out) :: model(:)

integer :: stash_list(nfields_rttov,2)
logical, allocatable :: gotstash(:,:,:)

! Local variables
real(real32), allocatable, target :: field_store32(:)
real(real64), allocatable, target :: field_store(:,:)
real(real64), allocatable, target :: field_store_tmp(:,:)
integer :: level, i, j, imodel, nfields, field
integer :: stashcode
integer :: fctime, prev_fctime
integer :: row, col
integer :: nrows, ncols, nprofs, nlevels
integer :: nrows_field, ncols_field
integer :: nlevels_rho
integer, allocatable :: fields_wanted(:,:,:)
integer :: next_field
integer :: lbpack

! Arrays for UM headers
type(ff_hd_type) :: hd
integer :: file_unit
integer(int64) :: nbytes
integer :: nlevels_field, nfields_wanted
character(len=160) :: message
integer :: nwords, npoints, maxwords

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Read headers
!-------------------------------------------------------------------------------

print '(a)', 'Reading fieldsfile headers'

file_unit = 10

open(unit=file_unit, file=file_name, access='stream', form='unformatted')

call radsim_read_ff_headers(file_unit, hd)

! Determine number of fields (there may be more lookup entries than fields)
! and number of time steps in file

nfields = 0
prev_fctime = -1
imodel = 0
do i = 1, size(hd%lookup_i, dim=2)
  if ( hd%lookup_i(30,i) < 0 ) then
    exit
  end if
  nfields = nfields + 1

  fctime = hd % lookup_i(14,i)
  if ( fctime /= prev_fctime .and. (temporal_data .or. prev_fctime < 0) ) then
    imodel = imodel + 1
    allocate(model(imodel) % validity_time(1,5), model(imodel) % data_time(1,5))
    model(imodel) % validity_time(1,:) = hd%lookup_i(1:5,i)
    model(imodel) % data_time(1,:)     = hd%lookup_i(7:11,i)
    model(imodel) % ref_time = time_in_minutes(model(imodel) % validity_time(1,:))
    prev_fctime = fctime
  end if
end do
model_ntimes = imodel

allocate(gotstash(nfields_rttov,2,model_ntimes), &
         fields_wanted(30,2,model_ntimes))

print '(a)', 'File info:'
print '(a,5(1x,i0))', ' Data time     = ', model(1) % data_time
print '(a,5(1x,i0))', ' Validity time = ', model(1) % validity_time
print '(a,i0)', ' Number of time steps = ', model_ntimes

ncols   = hd%int_c(6)
nrows   = hd%int_c(7)
nlevels = hd%int_c(8)
nprofs  = ncols*nrows
model(1:model_ntimes) % nprofs = nprofs
model(1:model_ntimes) % nlevels = nlevels
model(1:model_ntimes) % imdi = hd%int_c(21)
model(1:model_ntimes) % rmdi = hd%real_c(29)

print '(1x,i0,a)', nfields, ' fields'
print '(a,i0,1x,i0)', ' Number of rows, columns = ', nrows, ncols
print '(a,i0,1x,i0)', ' Number of profiles = ', nprofs
print '(a,i0,1x,i0)', ' Number of model levels = ', nlevels

nbytes = hd % nbytes

!-------------------------------------------------------------------------------
! 2. Set model grid
!-------------------------------------------------------------------------------

! Coordinates will be calculated later outside of this routine.

do imodel = 1, model_ntimes
  if ( associated(hd % row_dep_c) ) then

    if ( imodel == 1 ) then
      print '(a)', ' Variable grid'
    end if

    model(imodel) % grid % ncols = ncols
    model(imodel) % grid % nrows = nrows
    allocate(model(imodel) % grid % phi(nrows))
    allocate(model(imodel) % grid % lambda(ncols))
    model(imodel) % grid % phi = hd % row_dep_c(1:nrows,1)    ! Latitude
    model(imodel) % grid % lambda = hd % col_dep_c(1:ncols,1) ! Longitude

    if ( hd % fix_len_header(4) > 100 ) then
      model(imodel) % grid % type = 3  ! Stretched grid with rotated pole
      model(imodel) % grid % pole_lat = hd % real_c(5)
      model(imodel) % grid % pole_lon = hd % real_c(6)
    else
      model(imodel) % grid % type = 2  ! Stretched grid
    end if

  else

    if ( imodel == 1 ) then
      print '(a)', ' Uniform grid:'
      print '(a,f8.3)', '  x grid spacing = ', hd % real_c(1)
      print '(a,f8.3)', '  y grid spacing = ', hd % real_c(2)
      print '(a,f8.3)', '  first point lat  = ', hd % real_c(3)
      print '(a,f8.3)', '  first point lon  = ', hd % real_c(4)
    end if

    model(imodel) % grid % dlon = hd % real_c(1)
    model(imodel) % grid % dlat = hd % real_c(2)
    model(imodel) % grid % lat1 = hd % real_c(3)
    model(imodel) % grid % lon1 = hd % real_c(4)
    model(imodel) % grid % ncols = ncols
    model(imodel) % grid % nrows = nrows

    if ( hd % fix_len_header(4) > 100 ) then
      model(imodel) % grid % type = 1
      model(imodel) % grid % pole_lat = hd % real_c(5)
      model(imodel) % grid % pole_lon = hd % real_c(6)
    else
      model(imodel) % grid % type = 0
    end if

  end if
end do

!-------------------------------------------------------------------------------
! 3. Check STASH
!-------------------------------------------------------------------------------

!----------------------------
! 3.1 Set required STASH list
!----------------------------

call radsim_set_stash(stash_list)

gotstash = .true.
do imodel = 1, model_ntimes
  where( stash_list > 0 )
    gotstash(:,:,imodel) = .false.
  end where
end do

field = 0
fields_wanted = 0
nfields_wanted = 0

!-------------------------------
! 3.2 Check contents of the file
!-------------------------------

print '(a)', 'Checking STASH'

imodel = 0
prev_fctime = -1
do

  field = field + 1
  if ( field > nfields ) exit

  stashcode = hd % lookup_i(42,field)
  fctime = hd % lookup_i(14,field)

  if ( fctime /= prev_fctime ) then
    prev_fctime = fctime
    imodel = imodel + 1
    nfields_wanted = 0
    if ( imodel > model_ntimes ) exit
  endif

  if ( stashcode < 0 ) cycle

  nlevels_field = 1

  do j = 1, nlevels
    if ( field+j > nfields ) exit
    if ( hd % lookup_i(42,field+j) == stashcode ) then
      nlevels_field = nlevels_field + 1
    else
      exit
    end if
  end do

  if ( output_mode > 1 ) then
    print '(a,i2,a,i5,a,i0)', ' Found ', &
      nlevels_field, ' levels of stash item ', &
      stashcode, ' for T+', fctime
  end if

  if ( any(stash_list == stashcode .and. .not. gotstash(:,:,imodel)) ) then

    lbpack = hd % lookup_i(21,field)
    call radsim_check_ff_packing(lbpack)

    where(stash_list == stashcode)
      gotstash(:,:,imodel) = .true.
    end where

    nfields_wanted = nfields_wanted + 1
    fields_wanted(nfields_wanted,1,imodel) = field
    fields_wanted(nfields_wanted,2,imodel) = nlevels_field

  end if

  field = field + nlevels_field - 1

end do

do imodel = 1, model_ntimes
  do i = 1, size(stash_list, dim=1)
    if ( all(stash_list(i,:) > 0 .and. .not. gotstash(i,:,imodel)) ) then
      message = 'Missing fields in fieldsfile for ' // &
        trim(field_names_rttov(i)) // ': Possible STASH = '
      do j = 1, size(stash_list, dim=2)
        if ( stash_list(i,j) > 0 ) then
          write(message,'(a,2i6)') trim(message), stash_list(i,j)
        end if
      end do
      call radsim_error_report(message, status_warning)
    end if
  end do
end do

!-------------------------------------------------------------------------------
! 4. Read fields
!-------------------------------------------------------------------------------

print '(a)', 'Reading fields'

maxwords = maxval(hd % lookup_i(30,:))

if ( hd % word_size == 4 ) then
  allocate(field_store32(maxwords))
end if

allocate(field_store(maxwords, maxval(fields_wanted(:,2,:))))
field_store = model(1) % rmdi

field = 1

do imodel = 1, model_ntimes

  ! Loop over fields

  do j = 1, size(fields_wanted, dim = 1)

    next_field = fields_wanted(j,1,imodel)
    nlevels_field = fields_wanted(j,2,imodel)

    if ( next_field == 0 ) cycle

    if ( next_field > field ) then
      if ( output_mode > 1 ) then
        print '(a,i0,a)', ' Skipping the next ', next_field-field, ' fields'
      end if
      do i = field, next_field-1
        nwords = hd % lookup_i(30,i)
        if ( hd % word_size == 4 ) then
          read(file_unit) field_store32(1:nwords)
        else
          read(file_unit) field_store(1:nwords,1)
        end if
        nbytes = nbytes + nwords * hd % word_size
      end do
    end if
    field = next_field

    fctime = hd % lookup_i(14,field)
    stashcode = hd % lookup_i(42,field)

    print '(a,i2,a,i5,a,i0)', ' Reading ', &
      nlevels_field, ' levels of stash item ', &
      stashcode, ' at T+', fctime

    nrows_field = hd % lookup_i(18,field)
    ncols_field = hd % lookup_i(19,field)
    npoints     = hd % lookup_i(15,field)
    nwords      = hd % lookup_i(30,field)

    if ( npoints /= nprofs .and. output_mode >= 2 ) then
      write(message, '(a,i0,1x,i0)') &
        'field is not the same size as nprofs: nrows, ncols = ', &
          nrows_field, ncols_field
      call radsim_error_report(message, status_warning)
    end if

    do level = 1, nlevels_field

      if ( hd % word_size == 4 ) then
        read(file_unit) field_store32(1:nwords)
        field_store(1:nwords,level) = field_store32(1:nwords)
      else
        read(file_unit) field_store(1:nwords,level)
      end if

      field = field + 1
      nbytes = nbytes + ncols_field * nrows_field * hd%word_size

      ! Fill missing points if a wind field

      select case(stashcode)
        case(3209,3210)
          if ( npoints /= nprofs ) then
            allocate(field_store_tmp(ncols, nrows))
            do row = 1, min(nrows, nrows_field)
              do col = 1, min(ncols, ncols_field)
                field_store_tmp(col,row) = field_store(col+(row-1)*ncols,level)
              end do
            end do
            if ( nrows_field == nrows-1 ) then
              field_store_tmp(:,nrows) = field_store_tmp(:,nrows-1)
            end if
            if ( ncols_field == ncols-1 ) then
              field_store_tmp(ncols,:) = field_store_tmp(ncols-1,:)
            end if
            field_store(1:nprofs,level) = reshape(field_store_tmp, (/ nprofs /))
            deallocate(field_store_tmp)
          end if
        case default
      end select

    end do

    !---------------------------------------
    ! Transfer STASH item to model variables
    !---------------------------------------

    call radsim_store_stash(stashcode, field_store(1:nprofs, 1:nlevels_field), model(imodel))

  end do
end do

deallocate(field_store, gotstash, fields_wanted)
if ( hd % word_size == 4 ) then
  deallocate(field_store32)
end if

!-------------------------------------------------------------------------------
! 5. Pressure level conversion
!-------------------------------------------------------------------------------

! Calculate theta-level pressures from rho-levels

do imodel = 1, model_ntimes
  if ( .not. associated(model(imodel) % p) .and. associated(model(imodel) % ph) ) then

    nlevels_rho = size(model(imodel) % ph, dim=2)

    call radsim_calc_meto_plevels( &
      hd % level_dep_c(2:nlevels_rho,5), & ! in
      hd % level_dep_c(:,7),             & ! in
      hd % level_dep_c(2:nlevels_rho,6), & ! in
      hd % level_dep_c(:,8),             & ! in
      model(imodel)                      ) ! inout

  end if
end do

call radsim_dealloc_ff_hd(hd)

end subroutine radsim_read_fieldsfile
