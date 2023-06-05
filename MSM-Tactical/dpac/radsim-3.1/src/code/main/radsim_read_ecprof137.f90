!-------------------------------------------------------------------------------
! Description:
!
!   Read an NWP-SAF 137-level dataset file.
!
! Reference:
!
!   https://www.nwpsaf.eu/site/software/atmospheric-profile-data/
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

subroutine radsim_read_ecprof137( &
  file_name, & ! in
  model      ) ! out

use radsim_mod_constants, only : &
  gravity, &
  status_error

use radsim_mod_types, only : &
  model_type, &
  ecprof137_type

use radsim_mod_cfg, only : &
  mmr_snowrain

implicit none

include 'radsim_calc_plevels.interface'
include 'radsim_error_report.interface'

integer, parameter :: nlevels = 137

! Subroutine args

character(len=400), intent(in)  :: file_name
type(model_type),   intent(out) :: model

! Local variables

integer :: i
integer :: status
integer :: nprofs
real :: dummy_real(19)
integer :: dummy_int(6)
character(len=400) :: sfc_file_name
type(ecprof137_type), pointer :: prof1, prof

!-------------------------------------------------------------------------------
! 1. Open files
!-------------------------------------------------------------------------------

! Only the atmosphere file is specified as input. The surface file is also used
! to get the seaice field but we need to construct the file name by replacing
! 'atm' with 'sfc'.

sfc_file_name = file_name
i = len_trim(sfc_file_name)
sfc_file_name(i-2:i) = 'sfc'

write(*,'(a)') 'Reading data from', trim(file_name)
write(*,'(a)') '...and', trim(sfc_file_name)

open(unit=10, file=file_name, form='formatted', iostat=status)
if ( status /= 0 ) then
  call radsim_error_report('Failed to open ' // file_name, status_error)
end if
open(unit=11, file=sfc_file_name, form='formatted', iostat=status)
if ( status /= 0 ) then
  call radsim_error_report('Failed to open ' // sfc_file_name, status_error)
end if

!-------------------------------------------------------------------------------
! 2. Read in the data
!-------------------------------------------------------------------------------

! We don't know how many profiles are in the file so the best method is to use a
! linked list approach, only allocating profiles as we need them. These will be
! transferred to the working data structures afterwards.

nprofs = 0
allocate(prof1)
prof => prof1

do

  ! Read the next profile into the prof structure. Each profile is stored as a
  ! single record. Note that this fills all elements of prof except for seaice
  ! which is read from the sfc file.

  read(10, fmt='(1246(e13.6,1x),i5,3i3,i7,i5)', iostat=status) &
    prof % temp, &       !  1) Temperature [K]                          (1-137)
    prof % hum, &        !  2) Humidity [kg/kg]                         (138-274)
    prof % ozo, &        !  3) Ozone [kg/kg]                            (275-411)
    prof % cc, &         !  4) Cloud Cover [0-1]                        (412-548)
    prof % clw, &        !  5) C Liquid W [kg/kg]                       (549-685)
    prof % ciw, &        !  6) C Ice W [kg/kg]                          (686-822)
    prof % rain, &       !  7) Rain [kg/(m2 *s)]                        (823-959)
    prof % snow, &       !  8) Snow [kg/(m2 *s)]                        (960-1096)
    prof % w, &          !  9) Vertical Velocity [Pa/s]                 (1097-1233)
    prof % lnpsurf, &    ! 10) Ln of Surf Pressure in [Pa]              (1234)
    prof % z0, &         ! 11) Surface geopotential [m2/s2]             (1235) 
    prof % tsurf, &      ! 12) Surface Skin Temperature [K]             (1236)
    prof % t2m, &        ! 13) 2m Temperature [K]                       (1237)
    prof % td2m, &       ! 14) 2m Dew point temperature [K]             (1238)
    prof % u10, &        ! 15) 10m wind speed U component [m/s]         (1239)
    prof % v10, &        ! 16) 10m wind speed V component [m/s]         (1240)
    prof % stratrsrf, &  ! 17) Stratiform precipitation at surface [m]  (1241)
    prof % convrsrf, &   ! 18) Convective precipitation at surface [m]  (1242)
    prof % snowsurf, &   ! 19) Snow at surface [kg/(m2 *s)]             (1243)
    prof % lsm, &        ! 20) Land/sea Mask [0-1]                      (1244)
    prof % lat, &        ! 21) Latitude [deg]                           (1245)
    prof % long, &       ! 22) Longitude [deg]                          (1246)
    prof % year, &       ! 23) Year                                     (1247)
    prof % month, &      ! 24) Month                                    (1248)
    prof % day, &        ! 25) Day                                      (1249)
    prof % step, &       ! 26) Step                                     (1250)
    prof % gpoint, &     ! 27) Grid point [1-843490]                    (1251)
    prof % ind           ! 28) Index (rank-sorted)                      (1252) 

  ! Exit if the end-of-file was reached.

  if ( status < 0 ) then
    print '(a)', 'End-of-file reached'
    print '(i0,a)', nprofs, ' profiles read in'
    exit
  else if ( status > 0 ) then
    print '(i0,a)', nprofs, ' profiles read in'
    call radsim_error_report('Error reading 137-level profile', status_error)
  end if

  ! Read surface profile but only keep seaice, the rest go into dummy variables.

  read(11, fmt='(26(e13.6,1x),i5,3i3,i8,i5)', iostat=status) &
    dummy_real(1:6), &
    prof % seaice, &
    dummy_real(1:19), &
    dummy_int(1:6)

  ! Exit if the end-of-file was reached.

  if ( status < 0 ) then
    print '(a)', 'End-of-file reached'
    print '(i0,a)', nprofs, ' profiles read in'
    exit
  else if ( status > 0 ) then
    print '(i0,a)', nprofs, ' profiles read in'
    call radsim_error_report('Error reading 137-level profile', status_error)
  end if

  nprofs = nprofs + 1

  ! Add a new profile to the end of the list and point to it.

  allocate(prof % next)
  prof => prof % next

end do

! Deallocate the last profile as it is unused
deallocate(prof)
nullify(prof)

close(10)
close(11)

!-------------------------------------------------------------------------------
! 3. Transfer to data structures
!-------------------------------------------------------------------------------

model % nprofs = nprofs
model % nlevels = nlevels

allocate(model % validity_time(nprofs,5), model % data_time(nprofs,5))
allocate(model % grid % lat(nprofs))
allocate(model % grid % lon(nprofs))
allocate(model % zsurf(nprofs))
allocate(model % seaice(nprofs))
allocate(model % lsm(nprofs))
allocate(model % pstar(nprofs))
allocate(model % t2(nprofs))
allocate(model % td2(nprofs))
allocate(model % tskin(nprofs))
allocate(model % u10(nprofs))
allocate(model % v10(nprofs))
allocate(model % p(nprofs, nlevels))
allocate(model % ph(nprofs, nlevels+1))
allocate(model % t(nprofs, nlevels))
allocate(model % q(nprofs, nlevels))
allocate(model % clw(nprofs, nlevels))
allocate(model % ciw(nprofs, nlevels))
allocate(model % cfrac(nprofs, nlevels))
allocate(model % o3(nprofs, nlevels))
allocate(model % rain(nprofs, nlevels))
allocate(model % snow(nprofs, nlevels))

! Loop over profiles. At each iteration, values from the first profile in the
! list are assigned to the model data structure then the profile is removed from
! the list.

do i = 1, nprofs
  prof => prof1
  model % validity_time(i,1)   = prof % year
  model % validity_time(i,2)   = prof % month
  model % validity_time(i,3)   = prof % day
  model % validity_time(i,4:5) = 0
  model % data_time(i,:)       = model % validity_time(i,:)
  model % grid % lat(i)        = prof % lat
  model % grid % lon(i)        = prof % long
  model % zsurf(i)             = prof % z0 / gravity
  model % seaice(i)            = prof % seaice
  model % lsm(i)               = prof % lsm
  model % pstar(i)             = exp(prof % lnpsurf)
  model % t2(i)                = prof % t2m
  model % td2(i)               = prof % td2m
  model % tskin(i)             = prof % tsurf
  model % u10(i)               = prof % u10
  model % v10(i)               = prof % v10
  model % t(i,:)               = prof % temp
  model % q(i,:)               = prof % hum
  model % clw(i,:)             = prof % clw
  model % ciw(i,:)             = prof % ciw
  model % cfrac(i,:)           = prof % cc
  model % o3(i,:)              = prof % ozo
  model % rain(i,:)            = prof % rain
  model % snow(i,:)            = prof % snow
  prof1                        => prof % next
  deallocate(prof)
end do

! Rain/snow units for RTTOV-SCATT
mmr_snowrain = .false.

! Calculate pressure levels. Note that half-level pressures have 1 more value
! than full-level pressures.

call radsim_calc_plevels( &
  'ecmwf',       &
  model % pstar, &
  model % p,     &
  model % ph     )

end subroutine radsim_read_ecprof137
