!-------------------------------------------------------------------------------
! Description:
!
!   Interpolate all model fields to observation positions.
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

subroutine radsim_model_to_obs(obs, model_in, model_obs, footprint_data, &
                               obstotal, startob, keep_input)

use netcdf

use radsim_mod_io

use radsim_mod_cfg

use radsim_mod_constants, only : &
  qcflag_domain, &
  qcflag_time_range, &
  qcinfo_empty_footprint, &
  int32,  &
  real32, &
  real64, &
  deg2rad, &
  realtol, &
  output_verbose, &
  output_default

use radsim_mod_types, only : &
  model_type, &
  obs_type, &
  footprint_data_type

use radsim_mod_functions, only : &
  time_in_minutes

use rttov_const, only : &
  earthradius

implicit none

include 'radsim_grid_rotate.interface'
include 'radsim_interp.interface'

! Subroutine arguments:
type(obs_type),             intent(inout) :: obs            ! Observation data
type(model_type), target,   intent(inout) :: model_in(:)    ! Input model data
type(model_type), pointer,  intent(inout) :: model_obs      ! Simulation model data
type(footprint_data_type),  intent(inout) :: footprint_data ! Data for read/write of footprints
integer,                    intent(in)    :: obstotal       ! Total number of obs in all batches
integer,                    intent(in)    :: startob        ! First obs number in this batch
logical,          optional, intent(in)    :: keep_input     ! Keep input model data

! local variables
real(real64),   allocatable :: obs_lat(:), obs_lon(:)
real(real64),   allocatable :: cosgridlat(:), singridlat(:), cosalpha(:)
real(real32),   pointer     :: lat_sim(:), lon_sim(:)
integer,        allocatable :: footprint_grid_indices(:,:)
integer                     :: nprofs, nprofs_sim, ngrid, i, j, k, mthreads
real(real64)                :: fp_cosalpha, sinalpha2, sinalpha, alpha
real(real64)                :: cosobslat, sinobslat, cosobsazi, sinobsazi
real(real64)                :: cosgridazi, singridazi, dlon
real(real64)                :: rmajor2, rminor2, rmajor2rminor2, dx, dy
real                        :: model_lon_1, model_lon_n, model_lon_range(2)
logical                     :: keep

!-------------------------------------------------------------------------------

if ( present(keep_input) ) then
  keep = keep_input
else
  keep = .true.
end if

mthreads = 1
if ( nthreads > 1 ) mthreads = nthreads

nprofs = size(obs % lat)
ngrid  = size(model_in(1) % grid % lat)

! For unstructured grids we cannot flag data as being out of bounds

if ( model_in(1) % grid % type /= 101 ) then

  ! If the model is on a rotated grid we need the observations on the rotated
  ! grid for domain checking and interpolation.

  if ( model_in(1) % grid % type == 1 .or. model_in(1) % grid % type == 3 ) then

    if ( output_mode >= output_default ) then
      print '(a)', 'Model grid has a rotated pole. Rotating obs grid for interpolation.'
    end if

    allocate(obs_lat(nprofs))
    allocate(obs_lon(nprofs))
    obs_lat = obs % lat
    obs_lon = obs % lon

    call radsim_grid_rotate( &
      model_in(1) % grid % pole_lat, &
      model_in(1) % grid % pole_lon, &
      obs % lat, &
      obs % lon, &
      back = .false.)

  end if

  model_lon_1 = model_in(1) % grid % lon(1)
  model_lon_n = model_in(1) % grid % lon(model_in(1) % nprofs)
  model_lon_range = (/ model_lon_1, model_lon_n /)

  ! Check for wraparound and adjust domain to global if so. Do this by checking
  ! the separation between adjusted longitude values. If the separation is less
  ! than or equal to the grid spacing (allowing for 1% tolerance) then it is a
  ! global model.

  if ( output_mode >= output_verbose ) then
    print '(a)', ' Checking domain'
  end if

  if ( abs(model_lon_n - model_lon_1) + &
       abs(model_in(1) % grid % dlon)*1.01 >= 360.0 ) then
    if ( output_mode >= output_verbose ) print '(a)', ' Global domain'
    model_lon_range = (/ 0.0, 360.0 /)
  else if ( output_mode >= output_verbose ) then
    print '(a)', ' Not global domain'
    print '(a,2f12.6)', ' Longitude range = ', model_lon_1, model_lon_n
  end if

  ! Model longitude is nominally in the range 0 -> 360. If this is not a global
  ! model then we need the range to be -180 -> 180 if the domain spans the
  ! 0 degree meridian. (For domain-checking and interpolation, the grid longitude
  ! should be monotonically increasing from the first point).

  if ( model_lon_n < model_lon_1 ) then

    if ( output_mode >= output_default ) then
      print '(a)', ' Grid spans 0 degree meridian. Adjusting coordinates.'
    end if

    where ( model_in(1) % grid % lon > 180.0 )
      model_in(1) % grid % lon = model_in(1) % grid % lon - 360.0
    end where

    if ( associated(model_in(1) % grid % lambda) ) then
      where ( model_in(1) % grid % lambda > 180.0 )
        model_in(1) % grid % lambda = model_in(1) % grid % lambda - 360.0
      end where
    end if

    model_lon_range(1) = model_in(1) % grid % lon(1)

    if ( output_mode >= output_default ) then
      print '(a,2f12.6)', ' New longitude range = ', model_lon_range
    end if

    where ( obs % lon > 180.0 )
      obs % lon = obs % lon - 360.0
    end where

  else

    if ( model_lon_1 >= 0.0 .and. model_lon_n >= 180.0 ) then
      where ( obs % lon < 0.0 )
        obs % lon = obs % lon + 360.0
      end where
    else if ( model_lon_1 < 0.0 ) then
      where ( obs % lon > 180.0 )
        obs % lon = obs % lon - 360.0
      end where
    end if

  end if

  ! Flag any that are outside the domain

  where ( obs % lon < model_lon_range(1)               .or. &
          obs % lon > model_lon_range(2)               .or. &
          obs % lat < minval(model_in(1) % grid % lat) .or. &
          obs % lat > maxval(model_in(1) % grid % lat)      )
    obs % qcflags = ibset(obs % qcflags, qcflag_domain)
  end where

end if ! unstructured grid (grid type == 101)


! For footprint simulations identify all grid points within each footprint

if ( enable_footprints ) then

  if ( trim(read_footprint_file) /= '' ) then

    if ( output_mode >= output_default .and. startob == 1 ) then
      print '(a)', 'Reading footprint data from ' // trim(read_footprint_file)
    end if
    call read_footprint_data()

  else

    if ( output_mode >= output_default .and. startob == 1 ) then
      print '(a)', 'Mapping grid points to footprints'
    end if

    ! We calculate the distance between the obs and each grid point assuming a
    ! spherical Earth of radius r. Let alpha be the central angle between
    ! two points at longitude and latitude lon1, lat1 and lon2, lat2, then the
    ! great-circle distance d between them is given by:

    ! d = r.alpha
    ! cos(alpha) = sin(lat1).sin(lat2) + cos(lat1).cos(lat2).cos(lon1-lon2)

    ! The procedure to identify grid points within a given footprint is:
    ! - compute distance from obs to grid point (central angle alpha), as above
    ! - exclude grid point if this exceeds the footprint semi-major axis
    ! - compute azimuth angle (clockwise from N) of grid point relative to obs
    !   using cosine rule for spherical geometry
    ! - subtract obs azimuth from this to obtain azimuth angle of grid point
    !   in rotated frame with footprint semi-major axis aligned with y-axis (this
    !   is done in the next step)
    ! - compute x and y distance from obs to grid point using Euclidean
    !   trigonometry on the spherical distances in this rotated frame
    ! - use standard equation of ellipse to determine if this point lies
    !   within footprint

    ! Calculate sin/cos of grid latitudes outside the obs loop
    allocate(cosgridlat(ngrid), singridlat(ngrid), cosalpha(ngrid))
    cosgridlat = cos(model_in(1) % grid % lat * deg2rad)
    singridlat = sin(model_in(1) % grid % lat * deg2rad)

    ! Upper limit on grid points per footprint is model_in(1) % nprofs
    allocate(footprint_grid_indices(model_in(1) % nprofs, nprofs))
    footprint_grid_indices(:,:) = 0

    if ( associated(obs % nsim_per_obs) ) deallocate(obs % nsim_per_obs)
    allocate(obs % nsim_per_obs(nprofs))
    obs % nsim_per_obs(:) = 0

    ! Find indices of grid points in each obs footprint
!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(nprofs, obs, model_in, footprint_grid_indices, &
!$OMP&  cosgridlat, singridlat) NUM_THREADS(mthreads) SCHEDULE(DYNAMIC)
    do i = 1, nprofs

      ! Compute some obs-related quantities
      cosobslat = cos(obs % lat(i) * deg2rad)
      sinobslat = sin(obs % lat(i) * deg2rad)
      cosobsazi = cos(obs % satazim(i) * deg2rad)
      sinobsazi = sin(obs % satazim(i) * deg2rad)

      ! Scale semi-major/minor axes by earthradius (to get radii in radians)
      rmajor2 = (obs % footprint_rmajor(i) / earthradius)**2
      rminor2 = (obs % footprint_rminor(i) / earthradius)**2
      rmajor2rminor2 = rmajor2 * rminor2

      ! Calculate cosine of angle subtended by the semi-major axis
      fp_cosalpha = cos(obs % footprint_rmajor(i) / earthradius)
      
      ! Compute distance from grid points to this obs
      cosalpha = sinobslat * singridlat + cosobslat * cosgridlat * &
                 cos((obs % lon(i) - model_in(1) % grid % lon) * deg2rad)

      do j = 1, model_in(1) % nprofs
        ! Skip grid points definitely outside the footprint
        if ( cosalpha(j) < fp_cosalpha ) cycle

        sinalpha2 = max(1._real64 - cosalpha(j)**2, 0._real64)
        sinalpha = sqrt(sinalpha2)
        dlon = modulo(model_in(1) % grid % lon(j) - obs % lon(i), 360._real32)

        ! Handle special cases to avoid singularities
        if ( 1._real64 - cosalpha(j) < realtol ) then
          ! Grid point at obs location
          dx = 0._real64
          dy = 0._real64
        else
          ! Compute azimuth angle of grid point relative to obs (clockwise from N)
          if ( abs(cosobslat) < realtol ) then
            ! Obs lies on N or S pole, the grid azimuth is taken as the difference
            ! in longitudes (S pole) or 180 deg minus this (N pole)
            cosgridazi = cos(dlon * deg2rad)
            if ( obs % lat(i) > 0._real64 ) cosgridazi = -cosgridazi
          else
            ! All other cases - use cosine rule for spherical geometry
            cosgridazi = (singridlat(j) - sinobslat * cosalpha(j)) / &
                        (cosobslat * sinalpha)
          end if

          cosgridazi = min(max(cosgridazi, -1._real64), 1._real64)
          singridazi = sqrt(1._real64 - cosgridazi**2)
          if ( dlon > 180._real64 ) singridazi = -singridazi

          ! Compute angles subtended in x/y directions on axes aligned with
          ! ellipse (semi-major axis aligned with y axis). This uses Euclidean
          ! trigonometry. Here alpha is the angular distance subtended by the arc
          ! between the obs and the grid point, and the grid point is at angle
          ! "gridazi - obsazi" measured clockwise from the semi-major axis.
          alpha = acos(cosalpha(j))
          dx = alpha * (singridazi * cosobsazi - cosgridazi * sinobsazi)
          dy = alpha * (cosgridazi * cosobsazi + singridazi * sinobsazi)
        end if

        ! Determine if the point lies within the footprint ellipse
        if ( dx**2 * rmajor2 + dy**2 * rminor2 <= rmajor2rminor2 ) then
!$OMP CRITICAL
          obs % nsim_per_obs(i) = obs % nsim_per_obs(i) + 1
          footprint_grid_indices(obs % nsim_per_obs(i),i) = j
!$OMP END CRITICAL
        end if
      end do

      ! If no grid points fall within the footprint set flag
      if ( obs % nsim_per_obs(i) == 0 ) then
        obs % nsim_per_obs(i) = 1
        obs % qcinfo(i) = ibset(obs % qcinfo(i), qcinfo_empty_footprint)
      end if

    end do
!$OMP END PARALLEL DO

    deallocate(cosgridlat, singridlat, cosalpha)

  end if ! read_footprint_file

  ! Common code whether footprints were calculated or read in

  if ( any(btest(obs % qcinfo, qcinfo_empty_footprint)) ) then
    print '(a)', 'WARNING: some empty footprint(s)'
  end if

  ! Allocate footprint arrays
  nprofs_sim = sum(obs % nsim_per_obs)
  allocate(lon_sim(nprofs_sim), lat_sim(nprofs_sim))
  if ( associated(obs % sim_to_obs) ) deallocate(obs % sim_to_obs)
  allocate(obs % sim_to_obs(nprofs_sim))

  ! Populate footprint arrays
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i, j, k) NUM_THREADS(mthreads) SCHEDULE(DYNAMIC)
  do i = 1, nprofs
    k = sum(obs % nsim_per_obs(1:i-1))

    if ( btest(obs % qcinfo(i), qcinfo_empty_footprint) ) then
      ! Empty footprint: set lat/lon to obs for normal interpolation and set flag
      obs % sim_to_obs(k + 1) = i
      lon_sim(k + 1) = obs % lon(i)
      lat_sim(k + 1) = obs % lat(i)
    else
      ! Otherwise set relevant grid lat/lon values
      do j = 1, obs % nsim_per_obs(i)
        obs % sim_to_obs(k + j) = i
        lon_sim(k + j) = model_in(1) % grid % lon(footprint_grid_indices(j,i))
        lat_sim(k + j) = model_in(1) % grid % lat(footprint_grid_indices(j,i))
      end do
    end if
  end do
!$OMP END PARALLEL DO

  ! Store footprint data if output requested
  if ( trim(write_footprint_file) /= '' ) then
    if ( output_mode >= output_default .and. startob == 1 ) then
      print '(a)', 'Writing footprint data to ' // trim(write_footprint_file)
    end if
    call write_footprint_data()
  end if 

  ! Keep track of number of footprint indices for reading/writing
  ! footprint data file
  footprint_data % footprint_index_count = &
      footprint_data % footprint_index_count + &
      maxval(obs % nsim_per_obs(:)) * nprofs

  deallocate(footprint_grid_indices)

else

  ! Allocate and populate the sim-to-obs mapping for non-footprint case

  lon_sim => obs % lon
  lat_sim => obs % lat
  if ( associated(obs % sim_to_obs) ) deallocate(obs % sim_to_obs)
  allocate(obs % sim_to_obs(nprofs))
  do i = 1, nprofs
    obs % sim_to_obs(i) = i
  end do
  nprofs_sim = nprofs

end if ! enable_footprints


! Assign/check obs validity and data times

if ( .not. associated(model_obs) ) allocate(model_obs)

if ( temporal_data .or. &
     (addsolar .and. .not. fixed_sun_angles .and. .not. associated(obs % solzen)) ) then
  ! radsim_read_obsdata enforces obs date/time present for temporal_data
  ! and either obs solzen/solazi or date/time present for addsolar
  ! so we know that the date/time fields are present in obs data

  ! Populate validity and data times from obs data
  allocate(model_obs % validity_time(nprofs_sim,5), &
           model_obs % data_time(nprofs_sim,5))
  do i = 1, nprofs_sim
    j = obs % sim_to_obs(i)
    model_obs % validity_time(i,:) = (/ obs % year(j),  &
                                        obs % month(j), &
                                        obs % day(j),   &
                                        obs % hour(j),  &
                                        obs % minute(j) /)
    model_obs % data_time(i,:) = model_obs % validity_time(i,:)
  end do

  if ( temporal_data ) then
    ! Calculate obs ref_time for temporal interpolation and flag
    ! obs which lie outside the time range of the model data
    allocate(obs % ref_time(nprofs))
    do i = 1, nprofs
      if ( enable_footprints ) then
        k = sum(obs % nsim_per_obs(1:i-1)) + 1 ! First sim prof for this ob
      else
        k = i
      end if
      obs % ref_time(i) = time_in_minutes(model_obs % validity_time(k,:))

      if (obs % ref_time(i) < model_in(1) % ref_time .or. &
          obs % ref_time(i) > model_in(size(model_in)) % ref_time) then
        obs % qcflags(i) = ibset(obs % qcflags(i), qcflag_time_range)
      end if
    end do
  end if
else
  ! Validity and data times taken from the model data
  allocate(model_obs % validity_time(1,5), model_obs % data_time(1,5))
  model_obs % validity_time = model_in(1) % validity_time
  model_obs % data_time = model_in(1) % data_time
end if

! Horizontal/temporal interpolation

if ( output_mode >= output_default ) then
  print '(a)', 'Interpolating model fields to obs positions.'
end if

call radsim_interp(obs, lat_sim, lon_sim, model_in, model_obs, &
                   keep_input=keep_input)

! Deallocate lat_sim/lon_sim

if ( enable_footprints ) deallocate(lat_sim, lon_sim)

! Reset obs positions to original values

if ( model_in(1) % grid % type == 1 .or. model_in(1) % grid % type == 3 ) then
  obs % lat = obs_lat
  obs % lon = obs_lon
  deallocate(obs_lat)
  deallocate(obs_lon)
end if

! Reset model grid to original range if necessary

if ( model_in(1) % grid % type /= 101 ) then
  if ( model_lon_n < model_lon_1 ) then
    where ( model_in(1) % grid % lon < 0.0 )
      model_in(1) % grid % lon = model_in(1) % grid % lon + 360.0
    end where
    if ( associated(model_in(1) % grid % lambda) ) then
      where ( model_in(1) % grid % lambda < 0.0 )
        model_in(1) % grid % lambda = model_in(1) % grid % lambda + 360.0
      end where
    end if
  end if
end if

contains

! Subroutines for writing/reading footprint data
!
!   Footprint simulations usually involve obs batching which means
!   we do not know a priori what the max number of grid points per
!   footprint is across all batches. Therefore we cannot predict
!   the size of the dimension required for the grid indices associated
!   with the footprints when writing the file.
!
!   The footprint grid indices are therefore written as a 1D array
!   with unlimited dimension to the footprint data file.
!
!   The limitation is that the batching (max_profs) must be the same
!   for reading and writing a given footprint data file.
!
!   For efficiency, the read routine gets the variable IDs on the first
!   batch so that each subsequent batch needs only to read in the next
!   set of data.

subroutine write_footprint_data()

  character(len=400) :: footprint_file
  integer(int32)     :: nf90_flags, dim_nprofs_id, dim_indices_id
  integer(int32)     :: this_footprint_index_count, max_nsim_per_obs

  if ( startob == 1 ) then
    ! output_dir has already been set up ready for use
    footprint_file = trim(output_dir) // trim(write_footprint_file)

    ! First batch - create file
    nf90_flags = nf90_clobber + nf90_netcdf4 + nf90_classic_model
    call radsim_nf90_create(trim(footprint_file), nf90_flags, &
                                 footprint_data % file_id)
 
    ! Create dimensions for nprofs and indices
    call radsim_nf90_def_dim(footprint_data % file_id, 'nprofs', &
                             obstotal, dim_nprofs_id)
    call radsim_nf90_def_dim(footprint_data % file_id, 'indices', &
                             nf90_unlimited, dim_indices_id)

    ! Write attributes defining this run (checked by the read routine)
    call radsim_nf90_put_att(footprint_data % file_id, nf90_global, &
                             'ngrid', ngrid)
    call radsim_nf90_put_att(footprint_data % file_id, nf90_global, &
                             'nobs_total', obstotal)
    call radsim_nf90_put_att(footprint_data % file_id, nf90_global, &
                             'max_profs', max_profs)

    ! Exit def mode
    call radsim_nf90_enddef(footprint_data % file_id)
  endif

  ! Write the data
  call radsim_write_field_nc(obs % nsim_per_obs(:), 'nsim_per_obs', &
                             dim_nprofs_id, footprint_data % file_id, start=(/startob/))

  call radsim_write_field_nc(obs % qcinfo(:), 'qcinfo_footprint', &
                             dim_nprofs_id, footprint_data % file_id, start=(/startob/))

  max_nsim_per_obs = maxval(obs % nsim_per_obs(:))
  this_footprint_index_count = max_nsim_per_obs * nprofs
  call radsim_write_field_nc(reshape(footprint_grid_indices(1:max_nsim_per_obs,:), &
                                     (/this_footprint_index_count/)), &
                             'footprint_grid_indices', dim_indices_id, footprint_data % file_id, &
                             start=(/footprint_data % footprint_index_count + 1_int32/))

  ! Last batch - close the file
  if ( startob + nprofs - 1 == obstotal ) then
    call radsim_nf90_close(footprint_data % file_id)
  end if

end subroutine write_footprint_data

subroutine check_nc_error(status, msg)

  integer(int32),   intent(in) :: status
  character(len=*), intent(in) :: msg

  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report(trim(msg), status_error)
  end if

end subroutine check_nc_error

subroutine read_footprint_data()

  integer(int32)              :: status, start_index
  integer(int32)              :: this_footprint_index_count, max_nsim_per_obs
  integer(int32), allocatable :: indices(:), qcinfo(:)
  integer                     :: ngrid_file, nobs_file, max_profs_file
  character(len=256)          :: msg

  if ( startob == 1 ) then
    ! First batch - open the file
    call radsim_nf90_open(trim(read_footprint_file), footprint_data % file_id)

    ! Basic checks to see if this run is consistent with that used to create
    ! the footprint data file
    call radsim_nf90_get_att(footprint_data % file_id, nf90_global, &
                             'ngrid', ngrid_file)
    call radsim_nf90_get_att(footprint_data % file_id, nf90_global, &
                             'nobs_total', nobs_file)
    call radsim_nf90_get_att(footprint_data % file_id, nf90_global, &
                             'max_profs', max_profs_file)

    msg = ''
    if ( ngrid_file /= ngrid ) &
      msg = 'read_footprint_file is not consistent with model grid'
    if ( nobs_file /= obstotal ) &
      write (msg,'(a,i0)') 'read_footprint_file is not consistent with '// &
                           'total number of obs. Nobs in file: ', nobs_file
    if ( max_profs_file /= max_profs ) &
      write (msg,'(a,i0)') 'read_footprint_file is not consistent with '// &
                           'max_profs. Max_profs used in file: ', max_profs_file
    if ( trim(msg) /= '' ) call radsim_error_report(trim(msg), status_error)

    ! Get the variable dataset IDs - do this once for efficiency
    status = nf90_inq_varid(footprint_data % file_id, 'nsim_per_obs', &
                            footprint_data % var_id_nsim_per_obs)
    call check_nc_error(status, 'Error reading footprint data variable ID nsim_per_obs')
    status = nf90_inq_varid(footprint_data % file_id, 'qcinfo_footprint', &
                            footprint_data % var_id_qcinfo)
    call check_nc_error(status, 'Error reading footprint data variable ID qcinfo')
    status = nf90_inq_varid(footprint_data % file_id, 'footprint_grid_indices', &
                            footprint_data % var_id_indices)
    call check_nc_error(status, 'Error reading footprint data variable ID indices')
  end if

  ! Read the number of simulations per footprint (obs)
  if ( associated(obs % nsim_per_obs) ) deallocate(obs % nsim_per_obs)
  allocate(obs % nsim_per_obs(nprofs))
  status = nf90_get_var(footprint_data % file_id, &
                        footprint_data % var_id_nsim_per_obs, &
                        obs % nsim_per_obs, start=(/startob/))
  call check_nc_error(status, 'Error reading footprint data variable nsim_per_obs')

  ! Read the model grid indices for each footprint
  max_nsim_per_obs = maxval(obs % nsim_per_obs(:))
  this_footprint_index_count = max_nsim_per_obs * nprofs
  allocate(footprint_grid_indices(max_nsim_per_obs,nprofs), &
           indices(this_footprint_index_count))

  start_index = footprint_data % footprint_index_count + 1
  status = nf90_get_var(footprint_data % file_id, &
                        footprint_data % var_id_indices, &
                        indices, start=(/start_index/))
  call check_nc_error(status, 'Error reading footprint data variable indices')

  footprint_grid_indices(:,:) = reshape(indices(:), (/max_nsim_per_obs, nprofs/))
  deallocate(indices)

  ! Read the qcinfo and flag any empty footprints for the current obs batch
  allocate(qcinfo(nprofs))
  status = nf90_get_var(footprint_data % file_id, &
                        footprint_data % var_id_qcinfo, &
                        qcinfo, start=(/startob/))
  call check_nc_error(status, 'Error reading footprint data variable qcinfo')

  where (btest(qcinfo(:), qcinfo_empty_footprint))
    obs % qcinfo(:) = ibset(obs % qcinfo(:), qcinfo_empty_footprint)
  end where
  deallocate(qcinfo)

  ! Last batch - close the file
  if ( startob + nprofs - 1 == obstotal ) then
    call radsim_nf90_close(footprint_data % file_id)
  end if

end subroutine read_footprint_data

end subroutine radsim_model_to_obs
