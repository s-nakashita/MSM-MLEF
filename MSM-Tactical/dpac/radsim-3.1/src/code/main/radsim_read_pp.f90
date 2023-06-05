!-------------------------------------------------------------------------------
! Description:
!
!   Read a Met Office UM PP file in to a model structure. Both 32-bit and
!   64-bit formats are supported, although the latter is deprecated.
!
! Reference:
!
!   http://research.metoffice.gov.uk/research/interproj/nwpsaf/rtm/profile_datasets.html
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

subroutine radsim_read_pp( &
  file_name, & ! in
  model      ) ! out

use radsim_mod_cfg, only : &
  output_mode

use radsim_mod_constants, only : &
  real64, &
  int64, &
  int32, &
  real32, &
  status_warning, &
  status_error, &
  output_verbose

use radsim_mod_process

use radsim_mod_types, only : &
  model_type

implicit none

include 'radsim_calc_meto_plevels.interface'
include 'radsim_check_ff_packing.interface'
include 'radsim_error_report.interface'
include 'radsim_store_stash.interface'

! Subroutine args
character(len=400), intent(in)  :: file_name
type(model_type),   intent(out) :: model

integer :: stashlist(nfields_rttov,2)

! Local variables
real(real64), allocatable :: field_store(:,:)
integer :: level, i, j, nfields
integer :: stashcode
integer :: fctime
integer :: row, col
integer :: nrows, ncols, nlevels
integer :: nrows_field, ncols_field, nprofs_field

! For pressure calculation
real(real64), allocatable :: zsea_theta(:)
real(real64), allocatable :: zsea_rho(:)
real(real64), allocatable :: c_theta(:)
real(real64), allocatable :: c_rho(:)

integer :: file_unit
integer :: nwords, npoints
character, allocatable :: dummy(:)
character(len=160) :: message

type field_type
  integer(int32)            :: stashcode = 0
  integer(int32)            :: lookup_i(45)
  real(real32)              :: lookup_r(46:64)
  real(real64),     pointer :: values(:,:)
  type(field_type), pointer :: next => null()
end type field_type

type(field_type), pointer :: field, field1

integer :: field_index(0:30)
integer :: nindex
logical :: got_grid, got_plevels
integer :: p1, p2
integer :: fctime1
integer(int32) :: status
integer :: word_size
integer(int64)        :: lookup_i(45)
real(real64)          :: lookup_r(46:64)
real(real32), pointer :: values(:,:)
integer :: lbpack

!-------------------------------------------------------------------------------

file_unit = 10
open(unit=file_unit, file=file_name, form='unformatted')

call radsim_set_stash(stashlist)

!-------------------------------------------------------------------------------
! 1. Read fields
!-------------------------------------------------------------------------------

! Read in a single 4-byte value to check the word size. The first item should
! be a year value so it will be 0 if using 8-byte big-endian words.

read(file_unit) status
if ( status == 0 ) then
  word_size = 8
else
  word_size = 4
end if

rewind(file_unit)

if ( output_mode >= output_verbose ) then
  print '(a)', 'Checking word size:'
  print '(1x,i0,a)', word_size, ' bytes per word'
  print '(a)', 'Reading fields'
end if

! Loop over fields

allocate(field1)
field => field1
nfields = 0
fctime1 = 1000

do

  if ( word_size == 4 ) then
    read(file_unit, iostat=status) field % lookup_i, field % lookup_r
  else
    read(file_unit, iostat=status) lookup_i, lookup_r
    field % lookup_i = lookup_i
    field % lookup_r = lookup_r
  end if

  if ( status < 0 ) then
    print '(a)', 'End-of-file reached'
    exit
  else if ( status > 0 ) then
    call radsim_error_report('Error reading PP file', status_error)
  end if

  fctime      = field % lookup_i(14)
  npoints     = field % lookup_i(15)
  nrows_field = field % lookup_i(18)
  ncols_field = field % lookup_i(19)
  nwords      = field % lookup_i(30)
  stashcode   = field % lookup_i(42)

  if ( output_mode > output_verbose ) then
    print '(a,i5,a,i0)', ' Found stash item ', stashcode, ' for T+', fctime
  end if

  if ( fctime > fctime1 ) then
    print '(a)', 'New forecast time. Ignoring the rest.'
    exit
  end if

  if ( any(stashlist == stashcode) ) then

    if ( stashcode /= field % stashcode ) then
      where ( stashlist == field % stashcode )
        stashlist = 0
      end where
    end if

    nfields = nfields + 1
    if ( nfields == 1 ) then
      fctime1 = fctime
      lbpack = field % lookup_i(21)
      call radsim_check_ff_packing(lbpack)
    end if

    field % stashcode = stashcode
    allocate(field % values(ncols_field, nrows_field))
    if ( word_size == 8 ) then
      read(file_unit) field % values
    else
      allocate(values(ncols_field, nrows_field))
      read(file_unit) values
      field % values = values
      deallocate(values)
    end if

    if ( npoints < nwords ) then
      allocate(dummy((nwords-npoints)*word_size))
      read(file_unit) dummy
      deallocate(dummy)
    end if

    allocate(field % next)
    field => field % next

  else

    allocate(dummy(nwords*word_size))
    read(file_unit) dummy
    deallocate(dummy)

  end if

  if ( all(stashlist == 0) ) exit

end do

! Deallocate the last field as it is unused
deallocate(field)
nullify(field)

!-------------------------------------------------------------------------------
! 2. Store fields
!-------------------------------------------------------------------------------

allocate(model % validity_time(1,5), model % data_time(1,5))
model % validity_time(1,:) = field1 % lookup_i(1:5)
model % data_time(1,:)     = field1 % lookup_i(7:11)
print '(a,5(1x,i0))', ' Data time     = ', model % data_time
print '(a,5(1x,i0))', ' Validity time = ', model % validity_time

!--------------------------------------------------------------
! 2.1 Filter stash to get number of levels of each and set grid
!--------------------------------------------------------------

nindex = 0
field_index(0) = 0
field => field1
got_grid = .false.

do i = 1, nfields

  if ( field % stashcode /= field % next % stashcode ) then
    nindex = nindex + 1
    field_index(nindex) = i
  end if

  ! Set grid details if not done already
  if ( .not. got_grid .and. field % stashcode /= 3209 .and. &
       field % stashcode /= 3210                            ) then

    print '(a)', 'Setting grid info'

    if ( field % lookup_i(16) == 1 ) then
      print '(a)', ' Regular lat/lon grid:'
      model % grid % type = 0
    else if ( field % lookup_i(16) == 101 ) then
      print '(a)', ' Rotated lat/lon grid:'
      model % grid % type = 1
    else
      write(message, '(a,i0)') 'unknown grid type: ', field % lookup_i(16)
      call radsim_error_report(message, status_error)
    end if

    nrows = field % lookup_i(18)
    ncols = field % lookup_i(19)
    model % nprofs = nrows*ncols
    model % grid % nrows    = nrows
    model % grid % ncols    = ncols
    model % grid % dlat     = field % lookup_r(60)
    model % grid % dlon     = field % lookup_r(62)
    model % grid % lat1     = field % lookup_r(59) + field % lookup_r(60)
    model % grid % lon1     = field % lookup_r(61) + field % lookup_r(62)
    model % grid % pole_lat = field % lookup_r(56)
    model % grid % pole_lon = field % lookup_r(57)

    print '(a,i8)',    '  number of points  = ', model % nprofs
    print '(a,i8)',    '  number of rows    = ', model % grid % nrows
    print '(a,i8)',    '  number of columns = ', model % grid % ncols
    print '(a,f12.6)', '  first point lat   = ', model % grid % lat1
    print '(a,f12.6)', '  first point lon   = ', model % grid % lon1
    print '(a,f12.6)', '  lat grid spacing  = ', model % grid % dlat
    print '(a,f12.6)', '  lon grid spacing  = ', model % grid % dlon

    got_grid = .true.

  end if

  field => field % next

end do

!-------------------------------
! 2.2 Transfer to storage arrays
!-------------------------------

print '(a)', 'Storing required field data'
got_plevels = .false.


do i = 1, nindex

  field => field1
  nlevels = field_index(i) - field_index(i-1)
  nrows_field = size(field % values, dim=2)
  ncols_field = size(field % values, dim=1)
  nprofs_field = nrows_field*ncols_field
  stashcode = field % stashcode

  ! Store grid details

  if ( nprofs_field /= model % nprofs .and. output_mode >= output_verbose ) then
    write(message, '(a,i0)') &
      'field is not the same size as nprofs: ', model % nprofs
    call radsim_error_report(message, status_warning)
  end if

  ! Store field data

  if ( output_mode >= output_verbose ) then
    print '(a,i2,a,i5)', ' Got ', nlevels, ' levels of stash item ', stashcode
  end if

  allocate(field_store(model % nprofs, nlevels))

  do level = 1, nlevels

    field => field1

    do row = 1, min(nrows, nrows_field)
      do col = 1, min(ncols, ncols_field)
        field_store(col+(row-1)*ncols,level) = field % values(col,row)
      end do
    end do

    select case(stashcode)
      case(3209,3210) ! 10m wind
        if ( nrows_field == nrows-1 ) then
          p1 = (nrows-1)*ncols+1
          p2 = p1 + ncols_field-1
          field_store(p1:p2,level) = field % values(:,nrows_field)
        end if
        if ( ncols_field == ncols-1 ) then
          do j = 1, nrows
            field_store(j*ncols,level) = field % values(ncols_field,j)
          end do
        end if
      case(408)
        got_plevels = .true.
        model % nlevels = nlevels
      case(4,16004) ! Theta or T
        if ( .not. got_plevels ) then
          model % nlevels = nlevels
          if ( level == 1 ) then
            allocate(zsea_theta(nlevels))
            allocate(zsea_rho(nlevels+1))
            allocate(c_theta(nlevels))
            allocate(c_rho(nlevels+1))
          else if ( level == nlevels ) then
            got_plevels = .true.
          end if
          zsea_theta(level) = field % lookup_r(52)
          zsea_rho(level)   = field % lookup_r(53)
          c_theta(level)    = field % lookup_r(54)
          c_rho(level)      = field % lookup_r(55)
          if ( level == model % nlevels ) then
            zsea_rho(level+1) = field % lookup_r(46)
            c_rho(level+1)    = field % lookup_r(47)
          end if
        end if
      case default
    end select

    deallocate(field % values)
    field1 => field % next
    deallocate(field)

  end do

  call radsim_store_stash( &
    stashcode,   & ! in
    field_store, & ! in
    model        ) ! inout

  deallocate(field_store)

end do

!-------------------------------------------------------------------------------
! 3. Calculate pressure levels
!-------------------------------------------------------------------------------

if ( .not. associated(model % p) .and. associated(model % ph) ) then

  ! There appears to be a bug in FF -> PP conversion whereby the first zsea
  ! coefficient is incorrect for rho levels

  if ( model % nlevels == 70 .and. zsea_rho(1) <= 0.0 ) then
    print '(a)', 'Correcting error in first rho-level z coefficient:'
    print '(2(a,f12.8))', ' Changing from ', zsea_rho(1), ' -> ', 10.0_real64
    zsea_rho(1) = 10.0_real64
  end if

  call radsim_calc_meto_plevels( &
    zsea_theta, & ! in
    zsea_rho,   & ! in
    c_theta,    & ! in
    c_rho,      & ! in
    model       ) ! inout

end if

end subroutine radsim_read_pp
