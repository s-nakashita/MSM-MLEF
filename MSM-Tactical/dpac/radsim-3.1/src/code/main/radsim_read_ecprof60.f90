!-------------------------------------------------------------------------------
! Description:
!
!   Read an NWP-SAF 60-level dataset file.
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

subroutine radsim_read_ecprof60( &
  file_name, & ! in
  model      ) ! out

use radsim_mod_constants, only : &
  status_error

use radsim_mod_types, only : &
  model_type, &
  ecprof60_type

use rttov_const, only : &
  qmin

implicit none

include 'radsim_calc_plevels.interface'
include 'radsim_error_report.interface'

integer, parameter :: nlevels = 60

! Subroutine args

character(len=400), intent(in)  :: file_name
type(model_type),   intent(out) :: model

! Local variables

integer :: i
integer :: status
integer :: nprofs
type(ecprof60_type), pointer :: prof1, prof

!-------------------------------------------------------------------------------
! 1. Open file
!-------------------------------------------------------------------------------

print '(a)', 'Reading data from', trim(file_name)

open(unit=10, file=file_name, form='unformatted')

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
  ! single record.

  read(10,iostat=status) &
    prof % jdat, &      ! date (yyyymmddhh)
    prof % srlon, &     ! longitude (deg)
    prof % srlat, &     ! latitude (deg)
    prof % srlsm, &     ! land/see mask (1=land, 0=sea)
    prof % sst, &       ! surface skin temperature (K)
    prof % spsurf, &    ! surface pressure (hPa)
    prof % su10, &      ! 10-meter u wind (m/s)
    prof % sv10, &      ! 10-meter v wind (m/s)
    prof % st2m, &      ! 2-meter temperature (K)
    prof % sq2m, &      ! 2-meter specific humidity (kg/kg)
    prof % stt(:), &    ! temperature (K)
    prof % swv(:), &    ! specific humidity (kg/kg)
    prof % so3(:), &    ! specific ozone (kg/kg)
    prof % scc(:), &    ! cloud cover
    prof % sclw(:), &   ! cloud liquid water content (kg/kg)
    prof % sciw(:), &   ! cloud ice water content (kg/kg)
    prof % sw(:), &     ! vertical velocity (Pa/s)
    prof % scvl, &      ! low vegetation cover
    prof % scvh, &      ! high vegetation cover
    prof % stvl, &      ! type of low vegetation
    prof % stvh, &      ! type of high vegetation
    prof % sci, &       ! ice cover
    prof % sasn, &      ! snow albedo (0-1)
    prof % srsn, &      ! snow density (kg/m3)
    prof % stsn, &      ! snow temperature (K)
    prof % ssd, &       ! snow depth (m)
    prof % ssr, &       ! surface roughness (m)
    prof % sal, &       ! surface albedo (0-1)
    prof % sistl(:), &  ! layer ice temp. (K)  (top to bottom)
    prof % sstl(:), &   ! layer soil temp. (K) (top to bottom)
    prof % sswvl(:), &  ! layer soil volumetric water (m3/m3) (top to bottom)
    prof % soro         ! surface geometric height (m)                                          

  ! Exit if the end-of-file was reached.

  if ( status < 0 ) then
    print '(a)', 'End-of-file reached'
    print '(i0,a)', nprofs, ' profiles read in'
    exit
  else if ( status > 0 ) then
    print '(i0,a)', nprofs, ' profiles read in'
    call radsim_error_report('Error reading 60-level profile', status_error)
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
allocate(model % q2(nprofs))
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

! Loop over profiles. At each iteration, values from the first profile in the
! list are assigned to the model data structure then the profile is removed from
! the list.

do i = 1, nprofs
  prof => prof1
  model % validity_time(i,1) = prof % jdat / 1000000
  model % validity_time(i,2) = mod(prof % jdat / 10000, 100)
  model % validity_time(i,3) = mod(prof % jdat / 100, 100)
  model % validity_time(i,4) = mod(prof % jdat, 100)
  model % validity_time(i,5) = 0
  model % data_time(i,:)     = model % validity_time(i,:)
  model % grid % lat(i)      = prof % srlat
  model % grid % lon(i)      = prof % srlon
  model % zsurf(i)           = prof % soro
  model % seaice(i)          = prof % sci                    
  model % lsm(i)             = prof % srlsm            
  model % pstar(i)           = prof % spsurf * 100.0         
  model % t2(i)              = prof % st2m                   
  model % q2(i)              = prof % sq2m                   
  model % tskin(i)           = prof % sst                    
  model % u10(i)             = prof % su10                   
  model % v10(i)             = prof % sv10                   
  model % t(i,:)             = prof % stt                    
  model % q(i,:)             = prof % swv                    
  model % clw(i,:)           = prof % sclw                   
  model % ciw(i,:)           = prof % sciw                   
  model % cfrac(i,:)         = prof % scc                    
  model % o3(i,:)            = prof % so3
  prof1                      => prof % next
  deallocate(prof)
end do

! Fix some anomalous input values (RTTOV won't accept zero q values, and
! the min/max/mean profiles in the reduced 80 profile set have a missing
! data indicator for some variables)
where (model % q < qmin)             model % q = qmin
where (model % zsurf == -1000.)      model % zsurf = 0.
where (model % seaice == -1000.)     model % seaice = 0.
where (model % lsm == -1000.)        model % lsm = 0.
where (model % grid % lon == -1000.) model % grid % lon = 0.
where (model % grid % lat < -90.)    model % grid % lat = 0.
where (model % grid % lat >  90.)    model % grid % lat = 0.

! Calculate pressure levels. Note that half-level pressures have 1 more value
! than full-level pressures.

call radsim_calc_plevels( &
  'ecmwf',       &
  model % pstar, &
  model % p,     &
  model % ph     )

end subroutine radsim_read_ecprof60
