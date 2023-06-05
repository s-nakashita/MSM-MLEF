!-------------------------------------------------------------------------------
! Description:
!
!   Simulate a batch of observations.
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

subroutine radsim_run_batch( &
  model, &
  obs, &
  opts, &
  opts_scatt, &
  coeffs, &
  coeffs_scatt, &
  emis_atlas, &
  brdf_atlas, &
  nc_file_id, &
  startob)

use iso_fortran_env, only : &
  output_unit

use radsim_mod_cfg

use radsim_mod_constants, only : &
  status_warning, &
  qcflag_rttov, &
  qcflag_emiss, &
  qcflag_brdf, &
  qcflag_mfasis_geometry, &
  qcflag_mfasis_opdpedia, &
  int64, &
  int32, &
  output_debug, &
  output_verbose, &
  output_default

use radsim_mod_process, only : &
  z_direction

use radsim_mod_types, only : &
  model_type, &
  obs_type

use mod_rttov_emis_atlas, only : &
  rttov_emis_atlas_data

use mod_rttov_brdf_atlas, only : &
  rttov_brdf_atlas_data

use rttov_const, only : &
  errorstatus_success, &
  surftype_land, &
  surftype_sea, &
  surftype_seaice, &
  sensor_id_mw, &
  max_sol_zen, &
  qflag_mfasis_zenangle, &
  qflag_mfasis_sumzenangle, &
  qflag_mfasis_geometry_bounds, &
  qflag_mfasis_opdpedia_bounds

use rttov_types, only : &
  rttov_options, &
  rttov_options_scatt, &
  rttov_coefs, &
  rttov_scatt_coef, &
  rttov_profile, &
  rttov_profile_cloud, &
  rttov_transmission, &
  rttov_radiance, &
  rttov_pccomp, &
  rttov_chanprof, &
  rttov_emissivity, &
  rttov_reflectance

use parkind1, only : jpim, jprb, jplm

use rttov_math_mod, only : inv_planck

implicit none

! radsim routine interfaces

include 'radsim_error_report.interface'
include 'radsim_init_obs_out.interface'
include 'radsim_init_rttov_data.interface'
include 'radsim_model_to_rttov.interface'
include 'radsim_print_ob.interface'
include 'radsim_write_netcdf_obs_1d.interface'
include 'radsim_write_netcdf_obs_nd.interface'

! RTTOV routine interfaces

include 'rttov_direct.interface'
include 'rttov_get_emis.interface'
include 'rttov_get_brdf.interface'
include 'rttov_init_emis_refl.interface'
include 'rttov_init_prof.interface'
include 'rttov_init_rad.interface'
include 'rttov_init_transmission.interface'
include 'rttov_k.interface'
include 'rttov_parallel_direct.interface'
include 'rttov_parallel_k.interface'
include 'rttov_parallel_scatt.interface'
include 'rttov_print_cld_profile.interface'
include 'rttov_print_info.interface'
include 'rttov_print_opts.interface'
include 'rttov_print_opts_scatt.interface'
include 'rttov_print_profile.interface'
include 'rttov_scatt.interface'

type(model_type),            intent(inout) :: model
type(obs_type),              intent(inout) :: obs
type(rttov_options),         intent(in) :: opts         ! Options structure
type(rttov_options_scatt),   intent(in) :: opts_scatt   ! Options structure for RTTOV-SCATT
type(rttov_coefs),           intent(in) :: coeffs       ! Coefficients structure
type(rttov_scatt_coef),      intent(in) :: coeffs_scatt ! Coefficients structure for RTTOV-SCATT
type(rttov_emis_atlas_data), intent(in) :: emis_atlas(12)
type(rttov_brdf_atlas_data), intent(in) :: brdf_atlas(12)
integer(int32),              intent(in) :: nc_file_id
integer, optional,           intent(in) :: startob

! RTTOV variables
integer(kind=jpim) :: nchans, nchansout
integer(kind=jpim) :: nlevels, nprofs_sim, nprofs_out
integer(kind=jpim) :: errorstatus
type(rttov_chanprof),      allocatable         :: chanprof(:), chanprof1(:)
type(rttov_profile),       allocatable         :: profiles(:)
type(rttov_profile),       allocatable, target :: profiles_k(:)
type(rttov_profile),       allocatable         :: profiles_k_pc(:)
type(rttov_profile),       allocatable, target :: profiles_k_rec(:)
type(rttov_profile),       pointer             :: prof_k(:)
type(rttov_profile_cloud), allocatable         :: cld_profiles(:)
type(rttov_emissivity),    allocatable         :: emissivity(:)
type(rttov_emissivity),    allocatable         :: emissivity_k(:)
logical(kind=jplm),        allocatable         :: calcemis(:)
type(rttov_reflectance),   allocatable         :: reflectance(:)
type(rttov_reflectance),   allocatable         :: reflectance_k(:)
logical(kind=jplm),        allocatable         :: calcrefl(:)
integer(kind=jpim),        allocatable         :: frequencies(:)
real(kind=jprb),           allocatable         :: emiss(:)
real(kind=jprb),           allocatable         :: brdf(:)
integer(kind=jpim),        allocatable         :: channels_rec(:)
logical(kind=jplm),        allocatable         :: chan_flag(:)
type(rttov_transmission) :: transmission
type(rttov_transmission) :: transmission_k
type(rttov_radiance)     :: radiance
type(rttov_radiance)     :: radiance_k
type(rttov_pccomp)       :: pccomp
type(rttov_pccomp)       :: pccomp_k

integer :: i, j, k, lev
integer :: p1, p2, np, prof, profmap
integer :: c1, c2, chan
integer :: ncalls
integer :: nprofchans
integer :: prof_list(nprofs_per_call)
integer :: batchset
integer(int64) :: array_size
integer(int64) :: array_subdims
integer(int64) :: array_size_max
integer :: nprofs_per_batchset, nprofs_per_batchset_out
integer :: level1
integer :: level2
integer :: levelinc
character(len=80) :: message
integer :: start
integer :: obrange(2)
integer :: r1, r2
integer :: callno = 0
integer :: month
real    :: cads_exp(2:model % nlevels)
real(jprb) :: rad, bt
integer, allocatable :: pre_qcflags(:)

!-------------------------------------------------------------------------------

callno = callno + 1

!-------------------------------------------------------------------------------
! 1. Set up RTTOV data structures
!-------------------------------------------------------------------------------

nlevels = model % nlevels

if ( addpc ) then
  nchans = coeffs % coef_pccomp % pcreg(ipcbnd,ipcreg) % fmv_pc_npred
else if ( htfrtc ) then
  nchans = coeffs % coef_htfrtc % n_f
else
  nchans = size(obs % channels)
end if
nchansout = size(obs % channels)

allocate(channels_rec(nchansout))
channels_rec = (/ (i, i = 1, nchansout) /)

call radsim_init_rttov_data( &
  1_jpim,         & ! in
  nchans,         & ! in
  nchansout,      & ! in
  nlevels,        & ! in
  opts,           & ! in
  coeffs,         & ! in
  coeffs_scatt,   & ! in 
  chanprof,       & ! inout
  radiance,       & ! inout
  radiance_k,     & ! inout
  transmission,   & ! inout
  transmission_k, & ! inout
  pccomp,         & ! inout
  pccomp_k,       & ! inout
  emissivity,     & ! inout
  emissivity_k,   & ! inout
  reflectance,    & ! inout
  reflectance_k,  & ! inout
  profiles,       & ! inout
  profiles_k,     & ! inout
  profiles_k_pc,  & ! inout
  profiles_k_rec, & ! inout
  cld_profiles,   & ! inout
  frequencies     ) ! inout

allocate(calcemis(size(emissivity)), &
         calcrefl(size(reflectance)), &
         chan_flag(nchans))
calcemis = .true.
calcrefl = .true.

!-------------------------------------------------------------------------------
! 2. Prepare output storage
!-------------------------------------------------------------------------------

! Work out the number of calls required (more than 1 profile may be processed
! in each call).

nprofs_sim = size(model % p, dim=1)
nprofs_out = obs % nobs

ncalls = nprofs_sim / nprofs_per_call
if ( ncalls * nprofs_per_call /= nprofs_sim ) ncalls = ncalls + 1

! Set instrument details

nprofchans = size(chanprof)
nchans = nprofchans / nprofs_per_call

if ( use_emiss_atlas ) then
  allocate(emiss(nprofchans))
end if
if ( use_brdf_atlas ) then
  allocate(brdf(nprofchans))
end if

! If the simulation to observation mapping has not been set up do it here:
! in this case it is a one-to-one mapping
if ( .not. associated(obs % sim_to_obs) ) then
  allocate(obs % sim_to_obs(nprofs_out))
  obs % sim_to_obs = (/ (i, i = 1, nprofs_out) /)
end if

! QC arrays and brightness temperatures or radiances are always required but
! the other fields are optional. If transmittances or Jacobians are requested
! then the output comes from the K code.

if ( .not. associated(obs % qcflags) ) then
  allocate(obs % qcflags(nprofs_out))
  obs % qcflags = 0
end if

if ( .not. associated(obs % qcrttov) ) then
  allocate(obs % qcrttov(nprofs_out))
  obs % qcrttov = 0
end if

if ( .not. associated(obs % qcinfo) ) then
  allocate(obs % qcinfo(nprofs_out))
  obs % qcinfo = 0
end if

if ( .not. enable_footprints ) then
  allocate(obs % rtsurf(nprofs_out))
  obs % rtsurf = -1
end if

if ( enable_footprints ) then

  ! For footprint simulations we must simulate all profiles for each footprint
  ! together. In order to keep the code simple, we simulate all profiles
  ! together and the user controls the batching at the obs level via max_profs.

  nprofs_per_batchset = nprofs_sim
  nprofs_per_batchset_out = nprofs_out

else

  ! We need to limit the size of the channel-dependent output arrays to avoid
  ! running out of virtual memory (size is configurable). This means that
  ! multi-dimensional arrays in the obs structure have to be written out
  ! in further batches (batchsets).

  array_subdims = nchansout
  if ( write_trans .or. write_tjac .or. write_qjac .or. write_o3jac ) then
    array_subdims = array_subdims * nlevels
  end if

  array_size_max = max_array_size * 1000 * 1000
  array_size = min(array_size_max, array_subdims * nprofs_sim * 4)
  nprofs_per_batchset = array_size / (array_subdims * 4)
  nprofs_per_batchset_out = nprofs_per_batchset

  if ( output_mode >= output_default ) then
    print '(a,i0,a)', 'Max output array size = ', array_size, ' bytes'
    print '(a,i0)', 'Number of profs per batchset = ', nprofs_per_batchset
  end if

end if

if ( write_radiances ) then
  allocate(obs % radiance(nprofs_per_batchset_out,nchansout))
else
  allocate(obs % bt(nprofs_per_batchset_out,nchansout))
  if ( addsolar ) then
    allocate(obs % refl(nprofs_per_batchset_out,nchansout))
  end if
end if

if ( write_emiss ) then
  allocate(obs % emiss(nprofs_per_batchset_out,nchansout))
end if
if ( write_brdf ) then
  allocate(obs % brdf(nprofs_per_batchset_out,nchansout))
end if

if ( cads_height_assign_threshold > 0. ) then
  allocate(obs % cads_height_assignment(nprofs_per_batchset_out,nchansout))
end if
if ( write_geom_height ) then
  allocate(obs % geometric_height(nprofs_per_batchset_out,nlevels))
end if

if ( write_trans ) then
  allocate(obs % trans(nprofs_per_batchset_out,nlevels,nchansout))
end if
if ( write_tjac ) then
  allocate(obs % tjac(nprofs_per_batchset_out,nlevels,nchansout))
end if
if ( write_qjac ) then
  allocate(obs % qjac(nprofs_per_batchset_out,nlevels,nchansout))
end if
if ( write_o3jac ) then
  allocate(obs % o3jac(nprofs_per_batchset_out,nlevels,nchansout))
end if
if ( write_tskinjac ) then
  allocate(obs % tskinjac(nprofs_per_batchset_out,nchansout))
end if
if ( write_wind10mjac ) then
  allocate(obs % wind10mujac(nprofs_per_batchset_out,nchansout))
  allocate(obs % wind10mvjac(nprofs_per_batchset_out,nchansout))
end if
if ( write_emissjac ) then
  allocate(obs % emissjac(nprofs_per_batchset_out,nchansout))
end if

! Set level-order parameters for Jacobian output

! z_direction is set in radsim_model_to_rttov which is only called below.
! Currently therefore z_direction is uninitialised at this point: for most
! compilers it is zero and the Jacobians and transmittances are always output
! from TOA down to surface. I have not changed this behaviour for RadSim2.1
! except to explicitly initialise z_direction here to avoid ambiguity.

z_direction = 0.
if ( z_direction > 0.0 ) then
  level1 = nlevels
  level2 = 1
  levelinc = -1
else
  level1 = 1
  level2 = nlevels
  levelinc = 1
end if

! Initialise multi-dimensional arrays in the obs structure.

call radsim_init_obs_out(obs)

! Store qcflags: for footprint simulations, the qcflags may be set for an obs
! during the simulation loop below, but we would still want the simulations to
! continue for other profiles in the footprint. However if the flags have
! already been set (e.g. obs outside domain) then we want to exclude all grid
! points for that obs. This also works for non-footprint simulations because
! there is just one profile per obs.

allocate(pre_qcflags(nprofs_out))
pre_qcflags = obs % qcflags

!-----------------------------------------------------------------------------
! 3. Run simulations
!-----------------------------------------------------------------------------

if ( output_mode >= output_verbose ) then
  print '(a,i0,a)', 'Running rttov with ', nprofs_per_call, ' profiles per call'
end if

p2 = 0
np = 1
prof_list = 0
batchset = 1
if ( present(startob) ) then
  start = startob
else
  start = 1
end if

if ( use_all_atlas_months ) then
  allocate(chanprof1(nchans))
  chanprof1(:) % prof = 1
  chanprof1(:) % chan = (/ (j, j = 1, nchans) /)
end if

do i = 1, ncalls

  ! Set up iteration variables. We need the start (p1) and end (p2) positions
  ! within the model field arrays, also the end channel position (c2) within
  ! the RTTOV arrays (=number of channels*number of profiles).

  p1 = p2 + 1
  np = 0
  prof_list = 0

  do j = p1, nprofs_sim
    if ( pre_qcflags(obs % sim_to_obs(j)) == 0 ) then
      np = np + 1
      prof_list(np) = j
      if ( np == nprofs_per_call ) exit
    end if
  end do

  if ( np == 0 ) exit

  p2 = prof_list(np)

  c2 = nchans*np

  !------------------------------------------
  ! 3.1 Transfer model data to RTTOV profiles
  !------------------------------------------

  call radsim_model_to_rttov( &
    coeffs,          & ! in
    prof_list(1:np), & ! in
    model,           & ! inout
    obs,             & ! inout
    profiles(1:np),  & ! inout
    cld_profiles     ) ! inout

  if ( output_mode >= output_debug ) then
    call rttov_print_info(coeffs, lu=output_unit)
    if ( run_scatt ) then
      call rttov_print_opts_scatt(opts_scatt, lu=output_unit)
    else
      call rttov_print_opts(opts, lu=output_unit)
    end if
    do j = 1, np
      print '(a,i0)', 'Profile number ', p1+j-1
      call rttov_print_profile(profiles(j), lu=output_unit)
      if ( run_scatt ) then
        call rttov_print_cld_profile(cld_profiles(j), lu=output_unit)
      end if
    end do
  end if

  if ( .not. enable_footprints ) then
    ! Assign RT surface type to output structure
    obs % rtsurf(prof_list(1:np)) = profiles(1:np) % skin % surftype
  end if

  !--------------------------------------
  ! 3.2 Set emissivity and BRDF variables
  !--------------------------------------

  ! Calculate emissivity by default (this should always be the case over sea).

  calcemis = .true.
  emissivity(:) % emis_in = 0.0

  ! Set fixed values for MW over land/seaice if no atlas being used (or if the
  ! atlas returns a bad value). Note these are not likely to be accurate.

  if ( .not. htfrtc ) then
    if ( coeffs % coef % id_sensor == sensor_id_mw ) then

      do j = 1, np
        if ( profiles(j) % skin % surftype == surftype_land ) then
          emissivity((j-1)*nchans+1:j*nchans) % emis_in = 0.95
          calcemis((j-1)*nchans+1:j*nchans) = .false.
        else if ( profiles(j) % skin % surftype == surftype_seaice ) then
          emissivity((j-1)*nchans+1:j*nchans) % emis_in = 0.92
          calcemis((j-1)*nchans+1:j*nchans) = .false.
        end if
      end do

    end if
  end if

  ! Set input values for land/seaice.

  if ( use_emiss_atlas ) then

    ! Atlas values. It's easier just to retrieve for all profiles but only
    ! assign for land/seaice.

    ! If we're using all atlas months for NWP SAF profile datasets, do this
    ! profile-by-profile since the month is different for each one

    if ( use_all_atlas_months ) then
      do j = 1, np
        c1 = (j - 1) * nchans + 1
        c2 = j * nchans
        month = profiles(j) % date(2)
        call rttov_get_emis(  &
          errorstatus,       & ! out
          opts,              & ! in
          chanprof1(:),      & ! in
          profiles(j:j),     & ! in
          coeffs,            & ! in
          emis_atlas(month), & ! in
          emiss(c1:c2)       ) ! out
      end do
    else
      month = model % validity_time(1,2)
      call rttov_get_emis(  &
        errorstatus,       & ! out
        opts,              & ! in
        chanprof(1:c2),    & ! in
        profiles(1:np),    & ! in
        coeffs,            & ! in
        emis_atlas(month), & ! in
        emiss(1:c2)        ) ! out
    end if

    c2 = 0

    do j = 1, np

      ! Do not assign for sea

      if ( profiles(j) % skin % surftype == surftype_sea ) then
        c2 = c2 + nchans
        cycle
      end if

      ! Flag if any are bad, otherwise assign to the RTTOV input arrays

      if ( any(emiss((j-1)*nchans+1:j*nchans) <= 0.0) .or. &
           any(emiss((j-1)*nchans+1:j*nchans) >  1.0)      ) then

        obs % qcflags(obs % sim_to_obs(prof_list(j))) = &
          ibset(obs % qcflags(obs % sim_to_obs(prof_list(j))), qcflag_emiss)

      else

        emissivity(c2+1:c2+nchans) % emis_in = &
          emiss((j-1)*nchans+1:j*nchans)
        calcemis(c2+1:c2+nchans) = .false.

      end if

      c2 = c2 + nchans

    end do

  end if

  if ( addsolar ) then

    ! Calculate BRDF by default (this should always be the case over sea).

    calcrefl = .true.
    reflectance(:) % refl_in = 0.0
    reflectance(:) % refl_out = 0.0

    ! Set input values for land/seaice.

    if ( use_brdf_atlas ) then

      ! Atlas values. It's easier just to retrieve for all profiles but only
      ! assign for land/seaice.

      ! If we're using all atlas months for NWP SAF profile datasets, do this
      ! profile-by-profile since the month is different for each one

      if ( use_all_atlas_months ) then
        do j = 1, np
          c1 = (j - 1) * nchans + 1
          c2 = j * nchans
          month = profiles(j) % date(2)
          call rttov_get_brdf(  &
            errorstatus,       & ! out
            opts,              & ! in
            chanprof1(:),      & ! in
            profiles(j:j),     & ! in
            coeffs,            & ! in
            brdf_atlas(month), & ! in
            brdf(c1:c2)        ) ! out
        end do
      else
        month = model % validity_time(1,2)
        call rttov_get_brdf(  &
          errorstatus,        & ! out
          opts,               & ! in
          chanprof(1:c2),     & ! in
          profiles(1:np),     & ! in
          coeffs,             & ! in
          brdf_atlas(month),  & ! in
          brdf(1:c2)          ) ! out
      end if

      c2 = 0
      chan_flag = coeffs % coef % ss_val_chn == 2 ! Flag visible/near-IR channels

      do j = 1, np

        ! Do not assign for sea; also the atlas doesn't return values for sea-ice
        ! so skip these instead of flagging, but specify alternative input BRDF
        ! if specified

        if ( profiles(j) % skin % surftype == surftype_sea ) then
          c2 = c2 + nchans
          cycle
        else if ( profiles(j) % skin % surftype == surftype_seaice ) then
          if ( default_brdf_seaice > 0. ) then
            reflectance(c2+1:c2+nchans) % refl_in = default_brdf_seaice
            calcrefl(c2+1:c2+nchans) = .false.
          end if
          c2 = c2 + nchans
          cycle
        end if

        ! Flag if any are bad; assign valid values to the RTTOV input arrays
        ! but don't flag if the solar zenith angle exceeds RTTOV's maximum

        if ( profiles(j) % sunzenangle < max_sol_zen ) then
          if ( any(brdf((j-1)*nchans+1:j*nchans) <= 0.0 .and. chan_flag) ) then

            obs % qcflags(obs % sim_to_obs(prof_list(j))) = &
              ibset(obs % qcflags(obs % sim_to_obs(prof_list(j))), qcflag_brdf)

          end if
        end if

        where ( brdf((j-1)*nchans+1:j*nchans) > 0.0 )
          reflectance(c2+1:c2+nchans) % refl_in = &
            brdf((j-1)*nchans+1:j*nchans)
          calcrefl(c2+1:c2+nchans) = .false.
        end where
        if ( default_brdf_land > 0. ) then
          where ( brdf((j-1)*nchans+1:j*nchans) <= 0.0 )
            reflectance(c2+1:c2+nchans) % refl_in = default_brdf_land
            calcrefl(c2+1:c2+nchans) = .false.
          end where
        end if
        c2 = c2 + nchans

      end do

    else

      ! Not using BRDF atlas - use default land/sea-ice values if supplied

      c2 = 0

      do j = 1, np

        if ( profiles(j) % skin % surftype == surftype_land ) then
          if ( default_brdf_land > 0. ) then
            reflectance(c2+1:c2+nchans) % refl_in = default_brdf_land
            calcrefl(c2+1:c2+nchans) = .false.
          end if
        else if ( profiles(j) % skin % surftype == surftype_seaice ) then
          if ( default_brdf_seaice > 0. ) then
            reflectance(c2+1:c2+nchans) % refl_in = default_brdf_seaice
            calcrefl(c2+1:c2+nchans) = .false.
          end if
        end if
        c2 = c2 + nchans

      end do

    end if

  end if ! addsolar

  ! Initialise radiances

  if ( .not. htfrtc ) then
    call rttov_init_rad(radiance)
  end if

  ! Ensure K variables are initialised before each call

  if ( run_k ) then

    if ( .not. htfrtc ) call rttov_init_prof(profiles_k)
    if ( addpc .or. htfrtc ) call rttov_init_prof(profiles_k_rec)
    if ( htfrtc ) call rttov_init_prof(profiles_k_pc)

    call rttov_init_emis_refl(emis=emissivity_k)
    if ( addsolar ) call rttov_init_emis_refl(refl=reflectance_k)

    if ( .not. htfrtc ) then
      call rttov_init_transmission(transmission_k)
      call rttov_init_rad(radiance_k)

      if ( addpc ) then
        call rttov_init_pccomp(pccomp_k)
        pccomp_k % bt_pccomp = 1.
      else
        radiance_k % bt = 1.
        if ( addsolar ) then
          ! For VIS/NIR channels increment is in radiance (total)
          ! RTTOV handles this correctly for IR channels
          radiance_k % total = 1.
        end if
      end if
    end if

  end if


  !-----------------
  ! 3.3 Run the code
  !-----------------

  if ( np == 0 ) cycle

  if ( .not. run_scatt ) then

    !--------------------
    ! 3.3.1 Standard code
    !--------------------

    if ( .not. run_k ) then

      ! Direct code

      if ( nthreads <= 1 ) then

        call rttov_direct( &
          errorstatus,                   & ! out   error flag
          chanprof(1:c2),                & ! in    channel and profile index structure
          opts,                          & ! in    options structure
          profiles(1:np),                & ! in    profile array
          coeffs,                        & ! in    coefficients strucutre
          transmission,                  & ! inout computed transmittances
          radiance,                      & ! inout
          calcemis=calcemis(1:c2),       & ! in
          emissivity=emissivity(1:c2),   & ! inout emissivities
          calcrefl=calcrefl(1:c2),       & ! in
          reflectance=reflectance(1:c2), & ! inout reflectances
          pccomp=pccomp,                 & ! inout computed PCs and rec rads
          channels_rec=channels_rec)       ! in reconstructed channel list

      else

        call rttov_parallel_direct( &
          errorstatus,                   & ! out   error flag
          chanprof(1:c2),                & ! in    channel and profile index structure
          opts,                          & ! in    options structure
          profiles(1:np),                & ! in    profile array
          coeffs,                        & ! in    coefficients strucutre
          transmission,                  & ! inout computed transmittances
          radiance,                      & ! inout
          calcemis=calcemis(1:c2),       & ! in
          emissivity=emissivity(1:c2),   & ! inout emissivities
          calcrefl=calcrefl(1:c2),       & ! in
          reflectance=reflectance(1:c2), & ! inout reflectances
          pccomp=pccomp,                 & ! inout computed PCs and rec rads
          channels_rec=channels_rec,     & ! in reconstructed channel list
          nthreads=nthreads)               ! in number of threads

      end if

    else

      ! K code

      if ( nthreads <= 1 ) then

        call rttov_k( &
          errorstatus,                       & ! out   error flag
          chanprof(1:c2),                    & ! in    channel and profile index structure
          opts,                              & ! in    options structure
          profiles(1:np),                    & ! in    profile array
          profiles_k(1:c2),                  & ! inout
          coeffs,                            & ! in    coefficients strucutre
          transmission,                      & ! inout computed transmittances
          transmission_k,                    & ! inout
          radiance,                          & ! inout
          radiance_k,                        & ! inout
          calcemis=calcemis(1:c2),           & ! in
          emissivity=emissivity(1:c2),       & ! inout emissivities
          emissivity_k=emissivity_k(1:c2),   & ! inout
          calcrefl=calcrefl(1:c2),           & ! in
          reflectance=reflectance(1:c2),     & ! inout reflectances
          reflectance_k=reflectance_k(1:c2), & ! inout
          pccomp=pccomp,                     & ! inout computed PCs and rec rads
          pccomp_k=pccomp_k,                 & ! inout input BT gradient for PC-RTTOV
          profiles_k_pc=profiles_k_pc,       & ! inout output PC score Jacobians
          profiles_k_rec=profiles_k_rec,     & ! inout output reconstructed radiance Jacobians
          channels_rec=channels_rec)           ! in reconstructed channel list

      else

        call rttov_parallel_k( &
          errorstatus,                       & ! out   error flag
          chanprof(1:c2),                    & ! in    channel and profile index structure
          opts,                              & ! in    options structure
          profiles(1:np),                    & ! in    profile array
          profiles_k(1:c2),                  & ! inout
          coeffs,                            & ! in    coefficients strucutre
          transmission,                      & ! inout computed transmittances
          transmission_k,                    & ! inout
          radiance,                          & ! inout
          radiance_k,                        & ! inout
          calcemis=calcemis(1:c2),           & ! in
          emissivity=emissivity(1:c2),       & ! inout emissivities
          emissivity_k=emissivity_k(1:c2),   & ! inout
          calcrefl=calcrefl(1:c2),           & ! in
          reflectance=reflectance(1:c2),     & ! inout reflectances
          reflectance_k=reflectance_k(1:c2), & ! inout
          pccomp=pccomp,                     & ! inout computed PCs and rec rads
          pccomp_k=pccomp_k,                 & ! inout input BT gradient for PC-RTTOV
          profiles_k_pc=profiles_k_pc,       & ! inout output PC score Jacobians
          profiles_k_rec=profiles_k_rec,     & ! inout output reconstructed radiance Jacobians
          channels_rec=channels_rec,         & ! in reconstructed channel list
          nthreads=nthreads )                  ! in

      end if

    end if

  else

    !--------------------------------
    ! 3.3.2 Microwave Scattering Code
    !--------------------------------

    if ( nthreads <= 1 ) then

      call rttov_scatt ( &
        errorstatus,        & ! out
        opts_scatt,         & ! in
        nlevels,            & ! in
        chanprof(1:c2),     & ! in
        frequencies(1:c2),  & ! in
        profiles(1:np),     & ! in
        cld_profiles(1:np), & ! in
        coeffs,             & ! in
        coeffs_scatt,       & ! in
        calcemis(1:c2),     & ! in
        emissivity(1:c2),   & ! inout
        radiance            ) ! out

    else

      call rttov_parallel_scatt ( &
        errorstatus,        & ! out
        opts_scatt,         & ! in
        nlevels,            & ! in
        chanprof(1:c2),     & ! in
        frequencies(1:c2),  & ! in
        profiles(1:np),     & ! in
        cld_profiles(1:np), & ! in
        coeffs,             & ! in
        coeffs_scatt,       & ! in
        calcemis(1:c2),     & ! in
        emissivity(1:c2),   & ! inout
        radiance,           & ! out
        nthreads=nthreads   ) ! in

    end if

  end if

  !---------------------------------------------------
  ! 3.4 Transfer outputs to observation data structure
  !---------------------------------------------------

  ! Check error status

  if ( errorstatus /= errorstatus_success ) then
    write(message,'(a,i0)') 'Error in rttov = ', errorstatus
    call radsim_error_report(message, status_warning)
    obs % qcflags(obs % sim_to_obs(prof_list(1:np))) = &
      ibset(obs % qcflags(obs % sim_to_obs(prof_list(1:np))), qcflag_rttov)
    obs % qcrttov(obs % sim_to_obs(prof_list(1:np))) = errorstatus
  end if

  ! Fill output arrays. These are batched by profile so we have to offset the
  ! profile number in assignment.

  do j = 1, np

    prof = prof_list(j) - (batchset - 1) * nprofs_per_batchset
    if ( prof > nprofs_per_batchset ) then
      ! Note that this code never runs for footprint simulations because we
      ! don't do batching of output obs
      if ( output_mode >= output_verbose ) then
        print '(a,i0)', 'Writing obs output intermediate batch no. ', batchset
      end if
      r1 = 1 + (batchset - 1) * nprofs_per_batchset
      r2 = batchset * nprofs_per_batchset
      obrange = (/r1, r2/) + start - 1
      call radsim_write_netcdf_obs_nd(nc_file_id, obs, range=obrange)
      call radsim_init_obs_out(obs)
      prof = prof - nprofs_per_batchset
      batchset = batchset + 1
    end if

    c1 = (j - 1) * nchansout + 1
    c2 = j * nchansout

    if ( enable_footprints ) then

      ! Accumulate radiances in relevant output array
      profmap = obs % sim_to_obs(prof_list(j))

      if ( write_radiances ) then
        if ( addpc .or. htfrtc ) then
          obs % radiance(profmap,:) = obs % radiance(profmap,:) + &
                                      pccomp % total_pccomp(c1:c2)
        else
          obs % radiance(profmap,:) = obs % radiance(profmap,:) + &
                                      radiance % total(c1:c2)
        end if
      else
        if ( addpc .or. htfrtc ) then
          obs % bt(profmap,:) = obs % bt(profmap,:) + &
                                pccomp % total_pccomp(c1:c2)
        else
          obs % bt(profmap,:) = obs % bt(profmap,:) + &
                                radiance % total(c1:c2)
          if ( addsolar ) then
            ! Radiance->reflectance conversion is linear so accumulate
            ! reflectances directly
            obs % refl(profmap,:) = obs % refl(profmap,:) + &
                                    radiance % refl(c1:c2)
          end if
        end if
      end if

    else

      ! Non-footprint case: copy outputs directly into output arrays

      if ( write_radiances ) then
        if ( addpc .or. htfrtc ) then
          obs % radiance(prof,:) = pccomp % total_pccomp(c1:c2)
        else
          obs % radiance(prof,:) = radiance % total(c1:c2)
        end if
      else
        if ( addpc .or. htfrtc ) then
          obs % bt(prof,:) = pccomp % bt_pccomp(c1:c2)
        else
          obs % bt(prof,:) = radiance % bt(c1:c2)
          if ( addsolar ) then
            obs % refl(prof,:) = radiance % refl(c1:c2)
          end if
        end if
      end if

    end if

    if ( write_geom_height ) then
      ! The RTTOV output array has dimensions (nlevels,nchanprof) but the
      ! heights are the same for all channels associated with a given profile
      ! so we just copy the data corresponding to the first channel (c1)
      obs % geometric_height(prof,:) = radiance % geometric_height(:,c1)
    end if

    if ( cads_height_assign_threshold > 0. ) then
      ! CADS height assignment
      ! Return the first level above the surface at which the following is satisfied:
      !       |overcast-clear| / clear == threshold
      !   where overcast and clear are the RTTOV radiance outputs, and
      !   threshold is usually 0.01 (1%).
      ! The returned level is a real number, interpolated between the
      ! first two levels where |o-c|/c < threshold and |o-c|/c > threshold.

      ! There are nlayers = nlevels-1 overcast radiances, representing
      ! an opaque cloud at the level bounding the bottom of each layer.
      ! We return the (interpolated) "level" corresponding to where the opaque
      ! cloud would be located that results in the above equality being satisfied.

      ! The returned levels lie within the interval [2,nlevels].

      do k = c1, c2
        ! Only return height assignments for IR channels (at wavelengths above 3 microns)
        if ( coeffs % coef % ss_val_chn(chanprof(k)%chan) == 2 ) cycle

        ! Calculate the CADS expression for this channel
        cads_exp = abs(radiance % clear(k) - radiance % overcast(:,k)) / radiance % clear(k)

        if ( cads_exp(nlevels) >= cads_height_assign_threshold ) then

          ! Bottom level already exceeds threshold
          obs % cads_height_assignment(prof,k-c1+1) = real(nlevels)

        else if ( .not. any(cads_exp >= cads_height_assign_threshold) ) then

          ! All levels are within threshold (unlikely)
          obs % cads_height_assignment(prof,k-c1+1) = 2.

        else

          ! Interpolate to obtain the height assignment level
          do lev = nlevels, 3, -1
            if ( cads_exp(lev) < cads_height_assign_threshold .and. &
                 cads_exp(lev-1) >= cads_height_assign_threshold ) then

              obs % cads_height_assignment(prof,k-c1+1) = &
                (lev * (cads_exp(lev-1) - cads_height_assign_threshold) + &
                 (lev-1) * (cads_height_assign_threshold - cads_exp(lev))) / &
                (cads_exp(lev-1) - cads_exp(lev))
              exit

            end if
          end do

        end if
      end do ! channel loop
    end if

    if ( write_emiss ) then
      obs % emiss(prof,:) = emissivity(c1:c2) % emis_out
    end if
    if ( write_brdf ) then
      obs % brdf(prof,:) = reflectance(c1:c2) % refl_out
    end if

    if ( associated(obs % trans) ) then
      obs % trans(prof,:,:) = &
        transmission % tau_levels(level1:level2:levelinc, c1:c2)
    end if

    if ( addpc .or. htfrtc ) then
      prof_k => profiles_k_rec
    else
      prof_k => profiles_k
    end if
    if ( associated(obs % tjac) ) then
      do k = 1, nchansout
        obs % tjac(prof,:,k) = prof_k(k+c1-1) % t(level1:level2:levelinc)
      end do
    end if
    if ( associated(obs % qjac) ) then
      do k = 1, nchansout
        obs % qjac(prof,:,k) = prof_k(k+c1-1) % q(level1:level2:levelinc)
      end do
    end if
    if ( associated(obs % o3jac) ) then
      do k = 1, nchansout
        obs % o3jac(prof,:,k) = prof_k(k+c1-1) % o3(level1:level2:levelinc)
      end do
    end if
    if ( associated(obs % tskinjac) ) then
      do k = 1, nchansout
        obs % tskinjac(prof,k) = prof_k(k+c1-1) % skin % t
      end do
    end if
    if ( associated(obs % wind10mujac) ) then
      do k = 1, nchansout
        obs % wind10mujac(prof,k) = prof_k(k+c1-1) % s2m % u
      end do
    end if
    if ( associated(obs % wind10mvjac) ) then
      do k = 1, nchansout
        obs % wind10mvjac(prof,k) = prof_k(k+c1-1) % s2m % v
      end do
    end if

    if ( associated(obs % emissjac) ) then
      do k = 1, nchansout
        obs % emissjac(prof,k) = emissivity_k(k+c1-1) % emis_in
      end do
    end if

    ! Set MFASIS quality flags as required
    if ( run_mfasis ) then
      profmap = obs % sim_to_obs(prof_list(j))
      do k = 1, nchansout
        if ( btest(radiance % quality(k+c1-1), qflag_mfasis_zenangle)    .or. &
             btest(radiance % quality(k+c1-1), qflag_mfasis_sumzenangle) .or. &
             btest(radiance % quality(k+c1-1), qflag_mfasis_geometry_bounds)  ) then
          obs % qcflags(profmap) = &
            ibset(obs % qcflags(profmap), qcflag_mfasis_geometry)
        end if
        if ( btest(radiance % quality(k+c1-1), qflag_mfasis_opdpedia_bounds) ) then
          obs % qcflags(profmap) = &
            ibset(obs % qcflags(profmap), qcflag_mfasis_opdpedia)
        end if
      end do
    end if

    if ( output_mode >= output_debug ) then
      call radsim_print_ob(prof, obs)
    end if

  end do

end do

!---------------------------------------
! 3.5 Compute footprint-averaged outputs
!---------------------------------------

if ( enable_footprints ) then

  ! Compute footprint-averaged outputs

  do i = 1, nprofs_out

    if ( write_radiances ) then

      obs % radiance(i,:) = obs % radiance(i,:) / obs % nsim_per_obs(i)

    else

      obs % bt(i,:) = obs % bt(i,:) / obs % nsim_per_obs(i)

      ! Compute BTs - see rttov_calcbt

      do j = 1, nchansout
        chan = obs % channels(j)
        if ( coeffs % coef % ss_val_chn(chan) < 2 ) then
          rad = obs % bt(i,j)
          call inv_planck(coeffs % coef % planck1(chan), &
                          coeffs % coef % planck2(chan), rad, bt)
          obs % bt(i,j) = (bt - coeffs % coef % ff_bco(chan)) / &
                          coeffs % coef % ff_bcs(chan)
        else
          obs % bt(i,j) = 0.
        end if
      end do

      if ( addsolar ) then

        obs % refl(i,:) = obs % refl(i,:) / obs % nsim_per_obs(i)

      end if

    end if ! write_radiances

  end do ! output obs loop

end if ! enable_footprints

!-------------------------------------
! 3.6 Write the last batch of obs data
!-------------------------------------

r1 = 1 + (batchset - 1) * nprofs_per_batchset
r2 = nprofs_out
obrange = (/r1, r2/) + start - 1

call radsim_write_netcdf_obs_nd(nc_file_id, obs, range=obrange)
call radsim_write_netcdf_obs_1d(nc_file_id, obs, range=(/start, start + nprofs_out - 1/))

! Tidy up

deallocate(pre_qcflags)
if ( use_emiss_atlas ) deallocate(emiss)
if ( use_brdf_atlas )  deallocate(brdf)
deallocate(calcemis, calcrefl, chan_flag)
deallocate(channels_rec)
if ( use_all_atlas_months ) deallocate(chanprof1)

call radsim_init_rttov_data( &
  0_jpim,         & ! in
  nchans,         & ! in
  nchansout,      & ! in
  nlevels,        & ! in
  opts,           & ! in
  coeffs,         & ! in
  coeffs_scatt,   & ! in 
  chanprof,       & ! inout
  radiance,       & ! inout
  radiance_k,     & ! inout
  transmission,   & ! inout
  transmission_k, & ! inout
  pccomp,         & ! inout
  pccomp_k,       & ! inout
  emissivity,     & ! inout
  emissivity_k,   & ! inout
  reflectance,    & ! inout
  reflectance_k,  & ! inout
  profiles,       & ! inout
  profiles_k,     & ! inout
  profiles_k_pc,  & ! inout
  profiles_k_rec, & ! inout
  cld_profiles,   & ! inout
  frequencies     ) ! inout

end subroutine radsim_run_batch
