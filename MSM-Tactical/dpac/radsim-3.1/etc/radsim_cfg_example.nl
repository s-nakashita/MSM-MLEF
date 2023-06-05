!-------------------------------------------------------------------------------
! Example namelist file for the Radiance Simulator.
!-------------------------------------------------------------------------------
! Note that not all variables need to be included, most can be removed from
! this file without affecting the running of the code. Mandatory variables are
! indicated in the comments. See the User Guide for further details.

 &radsim_nl

!-----------
! Input data
!-----------
! model_datafile, model_filetype, rttov_coeffs_dir MUST be provided. Others
! will depend on the options chosen.

  obs_datafile = 'obs_data.txt'           ! Path to obs data file (optional)
  model_datafile = 'model_data.grb'       ! Path to model profiles data file
  model_filetype = 1                      ! (1=ECMWF GRIB, see User Guide for others)

  rttov_coeffs_dir     = 'rttov13pred54L' ! Path to RTTOV optical depth
                                          ! coefficients directory
  rttov_hydrotable_dir = 'hydrotable'     ! Path to hydrotable file directory
                                          ! (for MW scattering code)
  rttov_sccld_dir      = 'cldaer_visir'   ! Path to VIS/IR cloud scattering
                                          ! coefficients directory

  rttov_coeffs_type    = '.dat'           ! Specify file suffix for ASCII or HDF5
                                          ! coefficient files
  rttov_coeffs_options = '_o3co2'         ! Specify any additional text in rtcoef
                                          ! file name before the suffix

  emiss_atlas_dir      = ''               ! Path to emissivity atlas directory
  brdf_atlas_dir       = ''               ! Path to BRDF atlas directory

!---------------------
! Satellite instrument
!---------------------
! These (platform, inst, satid) are the identifiers defined in RTTOV and used
! in the coefficient file names. They must be provided. The channels variable
! is optional.

  platform = 'msg'
  inst = 'seviri'
  satid = 4
  channels = 4,5,6,7,8,9,10,11  ! Remove or set to zero for all channels

!-------------------------
! General run-time options
!-------------------------

  nthreads          = 1         ! Number of threads to use: RadSim and RTTOV must be
                                ! compiled with OpenMP enabled to use multiple threads.
  temporal_data     = .false.   ! Enable ingest and temporal interpolation of model
                                ! data at multiple forecast times
  enable_footprints = .false.   ! Enable footprint simulations
  run_scatt         = .false.   ! Run microwave scattering code (RTTOV-SCATT)
  output_mode       = 1         ! Diagnostic output level (1=normal, 2=verbose, 3=debug)
  seaice_threshold  = 0.2       ! Seaice threshold (partitions sea,seaice points)

!--------------
! RTTOV options (see RTTOV User's Guide for more details)
!--------------

  ozone_data = .false.          ! Use ozone profiles in simulations
  clw_data = .false.            ! Use CLW profiles in MW "clear-sky" simulations
  mw_clw_scheme = 2             ! MW cloud liquid water scheme
  addsolar = .false.            ! Enable solar radiation for affected channels
  ir_addclouds = .false.        ! Run VIS/IR scattering simulations
  ircloud_ice_scheme = 1        ! IR ice cloud optical property scheme
  fastem_version = 6            ! Fastem version (MW sea surface emissivity model)
  do_lambertian = .false.       ! Treat surface as Lambertian reflector
  do_nlte_correction = .false.  ! Enable non-LTE bias correction

!---------------
! Output options
!---------------
! - Setting output_dir is recommended.
! - It is advisable not to write out transmittances or T, q or o3 Jacobians
!   if running a lot of simulations for instruments with a lot of channels.

  output_dir  = ''              ! Path to output directory
                                !   (default is current directory)
  output_file = ''              ! Output file name (default is constructed
                                !   from sensor and model data date/time)
  write_radiances  = .false.    ! Write radiances instead of brightness temps
  write_profiles   = .false.    ! Write atmospheric profile data
  write_latlon     = .false.    ! Write profile lat/lon data
  write_emiss      = .false.    ! Write surface emissivities
  write_brdf       = .false.    ! Write surface reflectances
  write_trans      = .false.    ! Write layer transmittances
  write_tjac       = .false.    ! Write T Jacobians
  write_qjac       = .false.    ! Write q Jacobians
  write_o3jac      = .false.    ! Write O3 Jacobians
  write_tskinjac   = .false.    ! Write Tskin Jacobians
  write_wind10mjac = .false.    ! Write 10m wind Jacobians
  write_emissjac   = .false.    ! Write surface emissivity Jacobians

 /
