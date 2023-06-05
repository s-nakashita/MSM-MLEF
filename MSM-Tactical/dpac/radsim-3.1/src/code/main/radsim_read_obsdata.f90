!-------------------------------------------------------------------------------
! Description:
!
!   Read observation data from a file into an obs structure.
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

subroutine radsim_read_obsdata(file_name, nprofs, obs, obstotal, startob)

use radsim_mod_cfg, only : &
  output_mode,       &
  temporal_data,     &
  enable_footprints, &
  addsolar,          &
  fixed_sun_angles,  &
  max_profs,         &
  calc_geo_sat_angles

use radsim_mod_constants, only : &
  output_debug,   &
  status_error

use radsim_mod_types, only : &
  obs_type

implicit none

include 'radsim_error_report.interface'

character(len=200), intent(in)    :: file_name
integer,            intent(in)    :: nprofs
type(obs_type),     intent(inout) :: obs
integer,            intent(out)   :: obstotal
integer, optional,  intent(in)    :: startob

integer, parameter :: maxcols = 18
character(len=9), parameter :: col_text(maxcols) = &
  (/ 'lon      ', 'lat      ', 'h        ', 'lsm      ', &
     'satzen   ', 'viewid   ', 'scanline ', 'scanpos  ', &
     'satazim  ', 'year     ', 'month    ', 'day      ', &
     'hour     ', 'minute   ', 'solzen   ', 'solazim  ', &
     'rmajor   ', 'rminor   ' /)

integer :: i, j
integer, save :: file_version
integer :: nobs
integer, save :: nobs_total
integer :: start
integer, save :: ncols
integer, save :: columns(maxcols)
real :: coldata(maxcols)
character(len=80) :: line
integer :: nobs_read = 0
logical :: check_datetime(5), check_solar_angles(2)

!-------------------------------------------------------------------------------

if ( nobs_read == 0 ) then

  print '(2a)', 'Reading observation data from ', trim(file_name)

  open(unit=20, file=file_name, form='formatted')

  do
    read(20,'(a)') line
    line = adjustl(line)
    if ( line(1:1) == '!' .or. line == '' ) cycle
    backspace(20)
    exit
  end do

  read(20,*) file_version
  read(20,*) nobs_total

  ncols = 5
  columns = 0
  columns(1:5) = (/ 1,2,3,4,5 /)

  if ( file_version == 2 ) then
    read(20,*) ncols
    read(20,*) columns(1:ncols)
  end if

  if ( enable_footprints ) then
    if ( (.not. any(columns(1:ncols) == 9) .and. &
          .not. calc_geo_sat_angles) .or. &
         .not. any(columns(1:ncols) == 17) .or. &
         .not. any(columns(1:ncols) == 18) ) then
      call radsim_error_report( &
        'The enable_footprints option requires satellite azimuth and '// &
        'footprint semi-major/minor axis column in obs_datafile', &
        status_error)
    end if
  end if

  if ( temporal_data .or. (addsolar .and. .not. fixed_sun_angles) ) then
    check_datetime = .false.
    do i = 1, ncols
      if ( columns(i) >= 10 .and. columns(i) <= 14 ) then
        check_datetime(columns(i) - 9) = .true.
      end if
    end do

    check_solar_angles = .false.
    do i = 1, ncols
      if ( columns(i) >= 15 .and. columns(i) <= 16 ) then
        check_solar_angles(columns(i) - 14) = .true.
      end if
    end do

    if ( addsolar ) then
      if ( any(check_solar_angles) .and. .not. all(check_solar_angles) ) then
        call radsim_error_report( &
          'Both solar zenith and azimuth fields must be specified together in obs_datafile', &
          status_error)
      end if
    end if

    if ( any(check_datetime) .and. .not. all(check_datetime) ) then
      call radsim_error_report( &
        'All date/time fields must be specified together in obs_datafile (year/month/day/hour/minute)', &
        status_error)
    end if

    if ( temporal_data ) then
      ! Ensure obs file includes year, month, day, hour and minute
      if ( .not. all(check_datetime) ) then
        call radsim_error_report( &
          'If temporal_data is true year/month/day/hour/minute are required in obs_datafile', &
          status_error)
      end if
    end if

    if ( addsolar ) then
      ! Ensure obs file includes solzen and solazim, or year, month, day, hour and minute
      if ( .not. (all(check_datetime) .or. all(check_solar_angles)) ) then
        call radsim_error_report( &
          'If addsolar is true solzen/solazim or year/month/day/hour/minute are required in obs_datafile', &
          status_error)
      end if
    end if
  end if

  print '(a,i0)', ' File version = ', file_version
  print '(a,i0)', ' Total number of obs = ', nobs_total
  print '(a,8a10)', ' Columns:', col_text(columns(1:ncols))

  ! There is no point in using more than 1 batch if the number of obs is
  ! less than the reference number (this is the value at which batching should
  ! occur).

  ! This does not apply for footprint simulations though because footprints
  ! could generate more profiles to simulate than grid points so we should let
  ! the user specify max_profs as required.

  print '(a,i0)', ' Defined batch size = ', max_profs
  print '(a,i0)', ' Reference batch size = ', nprofs

  if ( .not. enable_footprints ) then
    if ( max_profs < nobs_total .and. nobs_total <= nprofs ) then
      max_profs = nobs_total
      print '(a,i0)', ' Batch size adjusted to reference'
    end if
  end if

  print '(a,i0,a)', ' Data will be processed in ', &
    nobs_total/max_profs + min(1, mod(nobs_total, max_profs)), ' batches'

end if

obstotal = nobs_total

start = 1
if ( present(startob) ) then
  if ( startob > 1 ) start = startob
end if

nobs = min(max_profs, obstotal-start+1)

! If components are allocated already (unlikely), remove if they are not the
! correct size.

if ( nobs /= obs % nobs ) then
  if ( associated(obs % lon)              ) deallocate(obs % lon)
  if ( associated(obs % lat)              ) deallocate(obs % lat)
  if ( associated(obs % zsurf)            ) deallocate(obs % zsurf)
  if ( associated(obs % lsm)              ) deallocate(obs % lsm)
  if ( associated(obs % satzen)           ) deallocate(obs % satzen)
  if ( associated(obs % qcflags)          ) deallocate(obs % qcflags)
  if ( associated(obs % qcinfo)           ) deallocate(obs % qcinfo)
  if ( associated(obs % satazim)          ) deallocate(obs % satazim)
  if ( associated(obs % year)             ) deallocate(obs % year)
  if ( associated(obs % month)            ) deallocate(obs % month)
  if ( associated(obs % day)              ) deallocate(obs % day)
  if ( associated(obs % hour)             ) deallocate(obs % hour)
  if ( associated(obs % minute)           ) deallocate(obs % minute)
  if ( associated(obs % solzen)           ) deallocate(obs % solzen)
  if ( associated(obs % solazim)          ) deallocate(obs % solazim)
  if ( associated(obs % footprint_rmajor) ) deallocate(obs % footprint_rmajor)
  if ( associated(obs % footprint_rminor) ) deallocate(obs % footprint_rminor)
end if

obs % nobs = nobs

! Allocate space for compulsory fields

if ( .not. associated(obs % lon) ) then
  allocate(obs % lon(nobs))
end if
if ( .not. associated(obs % lat) ) then
  allocate(obs % lat(nobs))
end if
if ( .not. associated(obs % qcflags) ) then
  allocate(obs % qcflags(nobs))
end if
if ( .not. associated(obs % qcinfo) ) then
  allocate(obs % qcinfo(nobs))
end if
obs % qcflags = 0
obs % qcinfo = 0

! Allocate space for optional fields

do i = 1, ncols

  select case(columns(i))
    case(3)
      if ( .not. associated(obs % zsurf) ) then
        allocate(obs % zsurf(nobs))
      end if
    case(4)
      if ( .not. associated(obs % lsm) ) then
        allocate(obs % lsm(nobs))
      end if
    case(5)
      if ( .not. associated(obs % satzen) ) then
        allocate(obs % satzen(nobs))
      end if
    case(6)
      if ( .not. associated(obs % viewid) ) then
        allocate(obs % viewid(nobs))
      end if
    case(7)
      if ( .not. associated(obs % scanline) ) then
        allocate(obs % scanline(nobs))
      end if
    case(8)
      if ( .not. associated(obs % scanpos) ) then
        allocate(obs % scanpos(nobs))
      end if
    case(9)
      if ( .not. associated(obs % satazim) ) then
        allocate(obs % satazim(nobs))
      end if
    case(10)
      if ( .not. associated(obs % year) ) then
        allocate(obs % year(nobs))
      end if
    case(11)
      if ( .not. associated(obs % month) ) then
        allocate(obs % month(nobs))
      end if
    case(12)
      if ( .not. associated(obs % day) ) then
        allocate(obs % day(nobs))
      end if
    case(13)
      if ( .not. associated(obs % hour) ) then
        allocate(obs % hour(nobs))
      end if
    case(14)
      if ( .not. associated(obs % minute) ) then
        allocate(obs % minute(nobs))
      end if
    case(15)
      if ( .not. associated(obs % solzen) ) then
        allocate(obs % solzen(nobs))
      end if
    case(16)
      if ( .not. associated(obs % solazim) ) then
        allocate(obs % solazim(nobs))
      end if
    case(17)
      if ( .not. associated(obs % footprint_rmajor) ) then
        allocate(obs % footprint_rmajor(nobs))
      end if
    case(18)
      if ( .not. associated(obs % footprint_rminor) ) then
        allocate(obs % footprint_rminor(nobs))
      end if
    case default
      cycle
  end select

end do

! Skip lines until the first ob is reached

if ( start > nobs_read+1 ) then
  if ( output_mode >= output_debug ) then
    print '(a,i0)', 'Skipping to ob number ', start
  end if
  do i = 1, start-(nobs_read+1)
    read(20,*)
  end do
  nobs_read = start-1
end if

! Read data one line at a time and assign to the appropriate fields

do i = 1, obs % nobs
  read(20,*) coldata(1:ncols)
  do j = 1, ncols
    select case(columns(j))
      case(1)
        obs % lon(i) = coldata(j)
      case(2)
        obs % lat(i) = coldata(j)
      case(3)
        obs % zsurf(i) = coldata(j)
      case(4)
        obs % lsm(i) = coldata(j)
      case(5)
        obs % satzen(i) = coldata(j)
      case(6)
        obs % viewid(i) = nint(coldata(j))
      case(7)
        obs % scanline(i) = nint(coldata(j))
      case(8)
        obs % scanpos(i) = nint(coldata(j))
      case(9)
        obs % satazim(i) = coldata(j)
      case(10)
        obs % year(i) = nint(coldata(j))
      case(11)
        obs % month(i) = nint(coldata(j))
      case(12)
        obs % day(i) = nint(coldata(j))
      case(13)
        obs % hour(i) = nint(coldata(j))
      case(14)
        obs % minute(i) = nint(coldata(j))
      case(15)
        obs % solzen(i) = coldata(j)
      case(16)
        obs % solazim(i) = coldata(j)
      case(17)
        obs % footprint_rmajor(i) = coldata(j)
      case(18)
        obs % footprint_rminor(i) = coldata(j)
      case default
        cycle
    end select
  end do
end do

nobs_read = nobs_read + obs % nobs

if ( nobs_read == obstotal ) close(20)

end subroutine radsim_read_obsdata
