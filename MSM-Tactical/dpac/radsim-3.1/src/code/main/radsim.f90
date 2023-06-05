!-------------------------------------------------------------------------------
! Synopsis:
!
!   radsim.exe [RADSIM_NL]
!
! Description:
!
!   Run the Radiance Simulator for the configuration defined in the RADSIM_NL
!   namelist file.
!
!   RADSIM_NL can be supplied as a command line argument or else by setting
!   the RADSIM_NL environment variable. No run will be performed without the
!   namelist file.
!
!   See the Radiance Simulator User Guide for more information.
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

program radsim

use iso_fortran_env, only : &
  output_unit

use netcdf

use radsim_mod_cfg

use radsim_mod_constants, only : &
  status_error, &
  status_warning, &
  qcflag_text, &
  qcinfo_text, &
  int32, &
  output_debug, &
  output_verbose, &
  output_default

use radsim_mod_io, only : &
  radsim_nf90_close

use radsim_mod_types, only : &
  model_type, &
  obs_type, &
  footprint_data_type

use mod_rttov_emis_atlas, only : &
  rttov_emis_atlas_data

use mod_rttov_brdf_atlas, only : &
  rttov_brdf_atlas_data

use rttov_types, only : &
  rttov_options, &
  rttov_options_scatt, &
  rttov_coefs, &
  rttov_scatt_coef

use parkind1, only : jpim

implicit none

! radsim routine interfaces

include 'radsim_calc_geo_sat_angles.interface'
include 'radsim_calc_solar_angles.interface'
include 'radsim_dealloc_obs.interface'
include 'radsim_dealloc_model.interface'
include 'radsim_error_report.interface'
include 'radsim_grid_rotate.interface'
include 'radsim_model_to_obs.interface'
include 'radsim_print_cfg.interface'
include 'radsim_read_cfg.interface'
include 'radsim_read_obsdata.interface'
include 'radsim_read_model.interface'
include 'radsim_run_batch.interface'
include 'radsim_setup_rttov.interface'
include 'radsim_write_netcdf_init.interface'
include 'radsim_write_netcdf_model.interface'

! rttov routine interfaces

! #include "rttov_dealloc_coefs.interface"
! #include "rttov_dealloc_emis_atlas.interface"
! #include "rttov_dealloc_brdf_atlas.interface"
! #include "rttov_dealloc_scattcoeffs.interface"

! RTTOV setup variables

type(rttov_options)            :: opts         ! Options structure
type(rttov_options_scatt)      :: opts_scatt   ! Options structure for scattering code
type(rttov_coefs)              :: coeffs       ! Coefficients structure
type(rttov_scatt_coef)         :: coeffs_scatt ! Coefficients structure for scattering code
type(rttov_emis_atlas_data)    :: emis_atlas(12)
type(rttov_brdf_atlas_data)    :: brdf_atlas(12)

! RTTOV variables

integer :: nchans
integer(kind=jpim) :: nlevels
integer(kind=jpim) :: month

! Data structures

type(model_type), target  :: model_in(1000)       ! Input model data
type(model_type), pointer :: model_obs => null()  ! Simulation model data
type(obs_type) :: obs ! Observation data
type(footprint_data_type) :: footprint_data

! General

integer :: i
integer :: nargs
logical :: exists
real :: cpu(2)
real :: cpu_elapsed(3)
character(len=80) :: message
character(len=400) :: nlfile
integer(int32) :: file_id
integer :: model_ntimes
integer :: obs_done
integer :: obstotal
integer :: batch
integer :: nqcflags(0:31)
integer :: nqcinfo(0:31)
logical :: keep_input
integer :: nflags(1)
integer :: nvalid(3)
integer(jpim) :: err

!-------------------------------------------------------------------------------

call cpu_time(cpu(1))
cpu_elapsed = 0.0

!-------------------------------------------------------------------------------
! 1. Set options
!-------------------------------------------------------------------------------

! Read from the namelist file. Abort if not supplied. It can be supplied as a
! command-line argument (priority) or in the RADSIM_NL environment variable.

nlfile = ''
nargs = command_argument_count()

if ( nargs > 0 ) then
  call get_command_argument(1, value=nlfile)
else
  print '(a)', 'Argument not supplied. Checking ENV variable RADSIM_NL.'
  call get_environment_variable('RADSIM_NL', nlfile)
end if

inquire(file=nlfile, exist=exists)

if ( exists ) then
  call radsim_read_cfg(nlfile)
else
  call radsim_error_report('Cannot find cfg file ' // nlfile, status_error)
end if

call radsim_print_cfg()

!-------------------------------------------------------------------------------
! 2. Read/write unbatched data
!-------------------------------------------------------------------------------

!---------------
! 2.1 Model data
!---------------

! Read all model fields. Data are at model grid-point positions if using regular
! NWP input files.

call radsim_read_model(model_ntimes, model_in)
if ( model_ntimes == 1 .and. temporal_data ) then
  write(message, '(a)') 'Have model fields for one time only, ' // &
    'setting temporal_data to false'
  call radsim_error_report(message, status_warning)
  temporal_data = .false.
end if

!-----------------------
! 2.2 RTTOV coefficients
!-----------------------

! Can't use emissivity/BRDF atlas if the data time is not defined

nlevels = size(model_in(1) % p, dim=2)
month = model_in(1) % validity_time(1,2)
if ( (month <= 0 .or. month > 12) .and. &
     (use_emiss_atlas .or. use_brdf_atlas) ) then
  use_emiss_atlas = .false.
  use_brdf_atlas = .false.
  write(message, '(a,i0)') &
    'Bad month for emissivity/BRDF atlas: ', month
  call radsim_error_report(message, status_error)
end if

! Read in coefficient data. Not all structures will be filled, it depends on
! the options chosen.

call radsim_setup_rttov( &
  nlevels,      & ! in
  model_in(1) % validity_time(1,1:3), & ! in
  opts,         & ! inout
  opts_scatt,   & ! inout
  coeffs,       & ! inout
  coeffs_scatt, & ! inout
  emis_atlas,   & ! inout
  brdf_atlas    ) ! inout

!------------------------
! 2.3 Set channels to use
!------------------------

! These can be specified via namelist, otherwise use all channels defined in
! the RTTOV coefficient file.

nchans = count(channels > 0)

if ( nchans == 0 ) then
  ! Set number of channels to be the same as the coefficient file
  if ( htfrtc ) then
    nchans = coeffs % coef_htfrtc % n_ch
  else
    nchans = coeffs % coef % fmv_chn
  end if
  channels(1:nchans) = (/ (i, i=1,nchans) /)
else if ( any(channels > coeffs % coef % fmv_chn) .and. .not. htfrtc ) then
  nchans = count(channels > coeffs % coef % fmv_chn)
  write(message, '(a,i0,a,i0)') &
    'Invalid channels requested:', nchans, &
    ' channels are over the maximum channel number of ', &
    coeffs % coef % fmv_chn
  call radsim_error_report(message, status_error)
end if
channels(1:nchans) = pack(channels, channels>0)
channels(nchans+1:) = 0

print '(a,i0)', ' Number of channels = ', nchans
if ( output_mode >= output_verbose ) then
  print '(a)', ' List of channels:'
  print '(16(1x,i0))', channels(1:nchans)
end if

call cpu_time(cpu(2))
cpu_elapsed(1) = cpu(2) - cpu(1)

!-------------------------------------------------------------------------------
! Run simulations in batches
!-------------------------------------------------------------------------------

! Very large numbers of profiles can cause the program to run out of memory so
! the simulations are run in batches. In many cases there will be only 1 batch.
! The maximum number of profiles that can be run per batch is defined in the
! maxprofs variable. Note that if simulations are at obs positions, the total
! number of batches required is unknown until the obs data file is read in
! (up to a limit of maxprofs observations) during the first batch. Subsequent
! iterations of the batch loop will read and write data where the previous
! left off.

! With footprint simulations, usually more profiles are simulated than are
! specified in the obs file. The user can prevent memory issues via the
! max_profs config namelist variable.

obs_done = 0
obstotal = 1
batch = 0
nqcflags = 0
nqcinfo = 0
nvalid = 0

do

  if ( obs_done >= obstotal ) exit

  batch = batch + 1

  if ( output_mode >= output_verbose ) then
    print '(a,i0)', 'Running obs batch no. ', batch
  end if

  ! Initialise obs output structure. These variables are the same every
  ! iteration but we initialise here because the information is lost when the
  ! obs structure components are deallocated later.

  allocate(obs % channels(nchans))
  obs % channels = channels(1:nchans)

  allocate(obs % wavenumbers(nchans))
  if ( htfrtc ) then
    ! Only the requested channels were read from the HTFRTC file
    obs % wavenumbers = coeffs % coef_htfrtc % sensor_freq(:)
  else
    obs % wavenumbers = coeffs % coef % ff_cwn(channels(1:nchans))
  end if

  ! Get observation data if available

  if ( obs_grid ) then
    call radsim_read_obsdata(obs_datafile, model_in(1) % nprofs, obs, obstotal, &
      startob=obs_done+1)
  else
    obstotal = model_in(1) % nprofs
    obs % nobs = model_in(1) % nprofs
  end if

  ! Compute GEO satellite zenith and azimuth angles if required
  ! Must be done before radsim_model_to_obs to ensure we have satazim for
  ! footprint simulations

  if ( calc_geo_sat_angles ) then
    call radsim_calc_geo_sat_angles(coeffs, model_in(1), obs)
  end if

  ! Create output file, define all the dimensions, attributes. We can't do this
  ! before the iterations start because the dimensions of the output arrays can
  ! only be defined when the obs data file has been read in.

  if ( batch == 1 ) then
    call radsim_write_netcdf_init(obstotal, obs, model_in(1), file_id)
  end if

  ! Model data at obs locations. For the simulations, model data is stored in
  ! model_obs. If using obs, the fields are interpolated to obs positions. If
  ! not, then model_obs is model_in. Note this means that model-grid
  ! simulations can only run in 1 batch because all the data are in memory
  ! already.

  if ( obs_grid ) then
    if ( batch == 1 ) allocate(model_obs)
    if ( obstotal == obs % nobs ) then
      keep_input = .false.
    else
      keep_input = .true.
    end if
    call radsim_model_to_obs(obs, model_in(1:model_ntimes), model_obs, &
                             footprint_data, obstotal, obs_done+1, &
                             keep_input=keep_input)
  else
    model_obs => model_in(1)
  end if

  ! Allocate array for CO2 if requested: more efficient to do this only for model_obs
  if ( co2_max_ppmv > 0. ) then
    if ( associated(model_obs % co2) ) deallocate(model_obs % co2)
    allocate(model_obs % co2(size(model_obs % q, dim=1), size(model_obs % q, dim=2)))
    model_obs % co2 = model_obs % rmdi
  end if

  ! Calculate standard lat,lon coordinates for a rotated model grid. If
  ! simulating on the model grid then this actually changes the input model
  ! coordinates because model_obs is a pointer to model_in. That's ok because
  ! we only run multiple batches for obs grid simulations.

  if ( model_in(1) % grid % type == 1 .or. model_in(1) % grid % type == 3 ) then

    if ( output_mode >= output_default ) then
      print '(a)', 'Grid has a rotated pole. Rotating to standard lat,lon grid.'
    end if

    call radsim_grid_rotate( &
      model_in(1) % grid % pole_lat, &
      model_in(1) % grid % pole_lon, &
      model_obs % grid % lat, &
      model_obs % grid % lon)

  end if

  ! Compute solar zenith and azimuth angles if required

  if ( addsolar .and. .not. fixed_sun_angles ) then
    call radsim_calc_solar_angles(model_obs, obs)
  end if

  ! Run a batch of simulations

  call radsim_run_batch( &
    model_obs,         & ! inout
    obs,               & ! inout
    opts,              & ! in
    opts_scatt,        & ! in
    coeffs,            & ! in
    coeffs_scatt,      & ! in
    emis_atlas,        & ! in
    brdf_atlas,        & ! in
    file_id,           & ! in
    startob=obs_done+1 ) ! in

  ! Write model data to output file if requested

  if ( write_latlon .or. write_profiles ) then
    call radsim_write_netcdf_model(file_id, model_obs, &
                                   (/obs_done+1, obs_done + model_obs % nprofs/))
  end if

  obs_done = obs_done + obs % nobs

  ! Keep track of numbers of flags
  if ( .not. associated(obs % rtsurf) ) then
    ! For footprint simulations rtsurf may not exist
    nvalid(1) = nvalid(1) + count(obs % qcflags == 0)
  else
    do i = 1, 3
      nvalid(i) = nvalid(i) + count(obs % qcflags == 0 .and. obs % rtsurf==(i-1))
    end do
  end if
  do i = 0, 31
    nqcflags(i) = nqcflags(i) + count(btest(obs % qcflags, i))
    nqcinfo(i) = nqcinfo(i) + count(btest(obs % qcinfo, i))
  end do

  print '(i0,a)', obs_done, ' obs processed'
  flush(output_unit)

  ! Deallocate obs arrays. It is simpler to do this after every iteration
  ! rather than have all code check every time.

  if ( obs_grid ) then
    call radsim_dealloc_model(model_obs)
  end if
  call radsim_dealloc_obs(obs)

  if ( (batch == 1) .and. (obs_done < obstotal) .and. &
       (output_mode < output_debug)                  ) then
    print '(a)', 'Suppressing output for subsequent batches...'
    output_mode = output_mode - 2
  end if

end do

!-------------------------------------------------------------------------------
! 9. Report and tidy up
!-------------------------------------------------------------------------------

print '(a)', 'Finished simulations'
print '(a)', 'Checking qc error flags. Number with'
nflags = ubound(qcflag_text)
do i = 0, nflags(1)
  print '(1x,a,i0,a,i0)', 'flag ', i, &
    ': ' // trim(qcflag_text(i)) // ' = ', nqcflags(i)
end do
print '(a,i0)', 'Number of valid obs = ', sum(nvalid)
if ( .not. enable_footprints ) then
  print '(a,i0)', 'Of these:'
  print '(a,i0)', ' Number of land   points = ', nvalid(1)
  print '(a,i0)', ' Number of sea    points = ', nvalid(2)
  print '(a,i0)', ' Number of seaice points = ', nvalid(3)
end if
print '(a)', 'Checking qc info flags. Number with'
nflags = ubound(qcinfo_text)
do i = 0, nflags(1)
  print '(1x,a,i0,a,i0)', 'flag ', i, &
    ': ' // trim(qcinfo_text(i)) // ' = ', nqcinfo(i)
end do

call radsim_nf90_close(file_id)
do i = 1, model_ntimes
  call radsim_dealloc_model(model_in(i))
end do

if ( obs_grid ) deallocate(model_obs)

if ( use_emiss_atlas ) then
  do i = 1, 12
    call rttov_deallocate_emis_atlas(emis_atlas(i))
  end do
end if
if ( use_brdf_atlas ) then
  do i = 1, 12
    call rttov_deallocate_brdf_atlas(brdf_atlas(i))
  end do
end if
if ( run_scatt ) call rttov_dealloc_scattcoeffs(coeffs_scatt)
call rttov_dealloc_coefs(err, coeffs)

call cpu_time(cpu(1))
cpu_elapsed(2) = cpu(1) - cpu(2)

print '(a)', 'Timing summary (seconds):'
print '(a,f8.2)', 'Input & setup = ', cpu_elapsed(1)
print '(a,f8.2)', 'Simulations   = ', cpu_elapsed(2)
print '(a,f8.2)', 'Total         = ', sum(cpu_elapsed)

end program radsim
