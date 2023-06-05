!-------------------------------------------------------------------------------
! Description:
!
!   Read headers from a Met Office UM fieldsfile and update model grid info.
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

subroutine radsim_read_ff_headers( &
  file_unit, & ! in
  hd         ) ! out

use radsim_mod_cfg, only : &
  output_mode

use radsim_mod_constants, only : &
  int32, &
  int64, &
  real32, &
  output_verbose, &
  output_debug

use radsim_mod_types, only : &
  ff_hd_type

implicit none

! Subroutine arguments
integer,          intent(in)  :: file_unit
type(ff_hd_type), intent(out) :: hd

! Local variables
integer :: i
integer :: head_size(2)
integer(int64) :: nvalues, nbytes
character, allocatable :: dummy(:)
integer(int32)          :: fix_len_header(256)
integer(int32), allocatable :: int_c(:)
integer(int32), allocatable :: lookup_i(:,:)
real(real32),   allocatable :: lookup_r(:,:)
real(real32),   allocatable :: real_c(:)
real(real32),   allocatable :: level_dep_c(:,:)
real(real32),   allocatable :: row_dep_c(:,:)
real(real32),   allocatable :: col_dep_c(:,:)

!-------------------------------------------------------------------------------------

print '(a)', 'Checking word size'

read(file_unit) nvalues
if ( nvalues == 20 ) then
  hd % word_size = 8
else
  hd % word_size = 4
end if
print '(1x,i0,a)', hd % word_size, ' bytes per word'

rewind(file_unit)

print '(a)', 'Reading fieldsfile headers'

!-----------------------------
! 1.1 Read fixed length header
!-----------------------------

if ( hd % word_size == 4 ) then
  read(file_unit) fix_len_header
  hd % fix_len_header = fix_len_header
else
  read(file_unit) hd % fix_len_header
end if

nvalues = 256

if ( output_mode >= output_verbose ) then
  print '(a,i0)', ' Read fixed length header: length = ', nvalues
  if ( output_mode >= output_debug ) then
    print *, hd % fix_len_header
  end if
end if

!---------------------------------
! 1.2 Read integer constants array
!---------------------------------

allocate( hd % int_c(hd % fix_len_header(101)) )

if ( hd % word_size == 4 ) then
  allocate( int_c(hd % fix_len_header(101)) )
  read(file_unit) int_c
  hd % int_c = int_c
  deallocate(int_c)
else
  read(file_unit) hd % int_c
end if

nvalues = nvalues + hd % fix_len_header(101)

if ( output_mode >= output_verbose ) then
  print '(a,i0)', ' Read integer constants array: length = ', size(hd % int_c)
  if ( output_mode >= output_debug ) then
    print *, hd % int_c
  end if
end if

!------------------------------
! 1.3 Read real constants array
!------------------------------

allocate( hd % real_c(hd % fix_len_header(106)) )

if ( hd % word_size == 4 ) then
  allocate( real_c(hd % fix_len_header(106)) )
  read(file_unit) real_c
  hd % real_c = real_c
  deallocate(real_c)
else
  read(file_unit) hd % real_c
end if

nvalues = nvalues + hd % fix_len_header(106)

if ( output_mode >= output_verbose ) then
  print '(a,i0)', ' Read real constants array: length = ', size(hd % real_c)
  if ( output_mode >= output_debug ) then
    print *, hd % real_c
  end if
end if


!----------------------------------------------
! 1.4 Read level-dependent constants if present
!----------------------------------------------

head_size = hd % fix_len_header(111:112)

if ( all(head_size > 0) ) then

  allocate( hd % level_dep_c(head_size(1), head_size(2)) )

  if ( hd % word_size == 4 ) then
    allocate( level_dep_c(head_size(1), head_size(2)) )
    read(file_unit) level_dep_c
    hd % level_dep_c = level_dep_c
    deallocate(level_dep_c)
  else
    read(file_unit) hd % level_dep_c
  end if

  nvalues = nvalues + head_size(1)*head_size(2)

  if ( output_mode >= output_verbose ) then
    print '(a,i0)', ' Read level-dependent constants array: length = ', &
      head_size(1) * head_size(2)
    if ( output_mode >= output_debug ) then
      print *, hd % level_dep_c
    end if
  end if

end if

!--------------------------------------------
! 1.5 Read row-dependent constants if present
!--------------------------------------------

head_size = hd % fix_len_header(116:117)

if ( any(head_size > 1) ) then

  allocate( hd % row_dep_c(head_size(1), head_size(2)) )

  if ( hd % word_size == 4 ) then
    allocate( row_dep_c(head_size(1), head_size(2)) )
    read(file_unit) row_dep_c
    hd % row_dep_c = row_dep_c
    deallocate(row_dep_c)
  else
    read(file_unit) hd % row_dep_c
  end if

  nvalues = nvalues + head_size(1)*head_size(2)

  if ( output_mode >= output_verbose ) then
    print '(a,i0)', ' Read row-dependent constants array: length = ', &
      head_size(1) * head_size(2)
    if ( output_mode >= output_debug ) then
      print *, hd % row_dep_c
    end if
  end if

end if

!-----------------------------------------------
! 1.6 Read column-dependent constants if present
!-----------------------------------------------

head_size = hd % fix_len_header(121:122)

if ( any(head_size > 1) ) then

  allocate( hd % col_dep_c(head_size(1), head_size(2)) )

  if ( hd % word_size == 4 ) then
    allocate( col_dep_c(head_size(1), head_size(2)) )
    read(file_unit) col_dep_c
    hd % col_dep_c = col_dep_c
    deallocate(col_dep_c)
  else
    read(file_unit) hd % col_dep_c
  end if

  nvalues = nvalues + head_size(1)*head_size(2)

  if ( output_mode >= output_verbose ) then
    print '(a,i0)', ' Read column-dependent constants array: length = ', &
      head_size(1) * head_size(2)
    if ( output_mode >= output_debug ) then
      print *, hd % col_dep_c
    end if
  end if

end if

!-------------------------------------------------------
! 1.7 Read any additional bytes before the lookup tables
!-------------------------------------------------------

if ( nvalues < hd % fix_len_header(150)-1 ) then

  nbytes = (hd % fix_len_header(150)-1-nvalues) * hd % word_size
  allocate(dummy(nbytes))

  if ( output_mode >= output_verbose ) then
    print '(a,i0,a)', ' Skipping remaining headers before lookup: length = ', &
      nbytes/hd % word_size
  end if

  read(file_unit) dummy
  nvalues = nvalues + nbytes/hd % word_size
  deallocate(dummy)

end if

!------------------------------------------------------------------------------
! 1.8 Read lookup tables (each table consists of 45 integer and 19 real values)
!------------------------------------------------------------------------------

head_size = hd % fix_len_header(151:152)
allocate( hd % lookup_i(45,head_size(2)) )
allocate( hd % lookup_r(46:64,head_size(2)) )

if ( hd % word_size == 4 ) then
  allocate( lookup_i(45,head_size(2)) )
  allocate( lookup_r(46:64,head_size(2)) )
  read(file_unit) ( lookup_i(:,i), lookup_r(:,i), i=1,head_size(2) )
  hd % lookup_i = lookup_i
  hd % lookup_r = lookup_r
  deallocate(lookup_i)
  deallocate(lookup_r)
else
  read(file_unit) ( hd % lookup_i(:,i), hd % lookup_r(:,i), i=1,head_size(2) )
end if

nvalues = nvalues + 64*head_size(2)

if ( output_mode >= output_verbose ) then
  print '(a,i0)', ' Read lookup tables: length = ', nvalues + 64*head_size(2)
end if

!------------------------------------------------------
! 1.9 Read any additional bytes before the data section
!------------------------------------------------------

if ( nvalues < hd % fix_len_header(160)-1 ) then

  nbytes = (hd % fix_len_header(160)-1-nvalues) * hd % word_size
  allocate(dummy(nbytes))

  if ( output_mode >= output_verbose ) then
    print '(a,i0,a)', ' Skipping header padding: length = ', nbytes/hd % word_size
  end if

  read(file_unit) dummy(1:nbytes)
  nvalues = nvalues + nbytes/hd % word_size
  deallocate(dummy)

end if

hd % nbytes = nvalues * hd % word_size

if ( output_mode >= output_debug ) then
  print '(a,i0)', 'Total number of header values read = ', nvalues
  print '(a,i0)', 'Total number of bytes read = ', hd % nbytes
end if

end subroutine radsim_read_ff_headers
