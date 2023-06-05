!-------------------------------------------------------------------------------
! Description:
!
!   Read a netCDF file into a model structure.
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2019, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_read_netcdf(file_name, model_ntimes, model, file_name_ancil)

use radsim_mod_io

use radsim_mod_cfg, only : &
  output_mode, &
  temporal_data, &
  run_scatt, &
  mmr_snowrain, &
  clw_data, &
  ozone_data, &
  ir_addclouds

use radsim_mod_constants, only : &
  real64, &
  int32, &
  gravity, &
  status_error, &
  output_verbose, &
  output_default

use radsim_mod_process

use radsim_mod_types, only : &
  model_type

use radsim_mod_functions, only : &
  time_in_minutes, &
  date_time_plus_minutes

implicit none

include 'radsim_calc_plevels.interface'
include 'radsim_error_report.interface'

! Subroutine args
character(len=400), intent(in)  :: file_name
integer,            intent(out) :: model_ntimes
type(model_type),   intent(out) :: model(:)
character(len=400), intent(in)  :: file_name_ancil

! Local variables
integer :: i, j, imodel, stat
integer(int32) :: file_id, dim_n_level, dim_n_lon, dim_n_lat, dim_n_time
integer, allocatable :: time(:)
character(len=400) :: time_units
integer :: ref_date_time(5)
real, allocatable :: lat(:), lon(:)
character(len=80) :: field_list(nfields_rttov, 3)
logical :: got_field(nfields_rttov)
integer :: nprofs, nlevels
real(real64), pointer :: values1D(:), values2D(:,:)
real(real64), allocatable :: values_in2D(:,:), values_in3D(:,:,:)
real(real64) :: missingvalue, sfac, aoff
logical :: sfac_exists, aoff_exists, zero_flag
character(len=80) :: var_name
character(len=80) :: message
real(real64), parameter :: tol = 1.e-10_real64

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Define required field list
!-------------------------------------------------------------------------------

! NetCDF files are assumed CF-compliant and standard names are associated with
! each field required by RTTOV.

! The fields (and alternatives where they exist) mostly mirror those read in
! radsim_read_grib for ECMWF GRIB data.

field_list = ''

! See Annex in RTTOV Users Guide for required fields

!---------------
! Surface fields
!---------------

field_list(field_rttov_tskin, 1) = 'skt'   ! skin temperature
field_list(field_rttov_t2, 1)    = 't2m'   ! temperature at 2m
field_list(field_rttov_q2, 1)    = 'd2m'   ! dewpoint temperature at 2m
field_list(field_rttov_pstar, 1) = 'sp'    ! surface pressure
! field_list(field_rttov_pstar, 2) = 'lnsp'  ! log of surface pressure
field_list(field_rttov_u10, 1)   = 'u10'   ! 10m wind u component
field_list(field_rttov_v10, 1)   = 'v10'   ! 10m wind v component

! Surface type usually needs to be derived from 2 fields - a land-sea mask and
! the seaice fraction. The priority is disabled in this case.

field_list(field_rttov_surftype, 1) = 'lsm'    ! LSM
field_list(field_rttov_surftype, 2) = 'siconc' ! seaice cover
field_list(field_rttov_surftype, 3) = 'z'      ! geopotential

!---------
! Profiles
!---------

field_list(field_rttov_p, 1) = 'p'      ! pressure levels
field_list(field_rttov_t, 1) = 't'      ! T
! field_list(field_rttov_t, 2) = 'theta'  ! theta
field_list(field_rttov_q, 1) = 'q'      ! specific humidity
! field_list(field_rttov_q, 2) = 'rh'     ! relative humidity

if ( ozone_data ) then
  field_list(field_rttov_ozone, 1) = 'o3'  ! ozone
endif

! Profiles for running scattering code

if ( clw_data .or. run_scatt .or. ir_addclouds ) then
  field_list(field_rttov_qcl, 1) = 'clwc'   ! qcl
end if

if ( run_scatt .or. ir_addclouds ) then
  field_list(field_rttov_qcf, 1)   = 'ciwc' ! qcf
  field_list(field_rttov_cfrac, 1) = 'cc'   ! cloud cover
end if

if ( run_scatt ) then
  field_list(field_rttov_rain, 1) = 'crwc' ! Rain mixing ratio
  field_list(field_rttov_snow, 1) = 'cswc' ! Snow mixing ratio
  mmr_snowrain = .true.                    ! Specify mass mixing ratio
end if

where (field_list(:,1) /= '')
  got_field = .false.
elsewhere
  got_field = .true.
end where

! ------------------------------------------------------------------------
! Fields not used by RTTOV, but if present are read in, interpolated, and
! output with other fields. It is important to set "got_field" before this
! to make these optional
! ------------------------------------------------------------------------

field_list(field_total_cc, 1)  = 'tcc' ! Total cloud cover
field_list(field_low_cc, 1)    = 'lcc' ! Low cloud cover
field_list(field_medium_cc, 1) = 'mcc' ! Medium cloud cover
field_list(field_high_cc, 1)   = 'hcc' ! High cloud cover

!-------------------------------------------------------------------------------
! 2a. Read atmospheric data from primary netCDF file 
!-------------------------------------------------------------------------------

print '(a)', 'Reading netCDF file ' // trim(file_name)

call radsim_nf90_open(file_name, file_id)

! --------------
! Get dimensions
! --------------

! Mandatory dimensions: level, latitude, longitude, time

call radsim_nf90_inquire_dimension_length(file_id, 'level', dim_n_level)
call radsim_nf90_inquire_dimension_length(file_id, 'longitude', dim_n_lon)
call radsim_nf90_inquire_dimension_length(file_id, 'latitude', dim_n_lat)
call radsim_nf90_inquire_dimension_length(file_id, 'time', dim_n_time)

if ( temporal_data ) then
  model_ntimes = dim_n_time
else
  model_ntimes = 1
end if
nlevels = dim_n_level
nprofs = dim_n_lat * dim_n_lon

!----------------
! Grid definition
!----------------

allocate(lon(dim_n_lon), lat(dim_n_lat))
call radsim_nf90_get_var(file_id, 'longitude', lon)
call radsim_nf90_get_var(file_id, 'latitude', lat)

! Ensure dlat and dlon are the same everywhere
if ( any(abs((lon(2:dim_n_lon) - lon(1:dim_n_lon-1)) - (lon(2) - lon(1))) > 1e-6)  .or. &
     any(abs((lat(2:dim_n_lat) - lat(1:dim_n_lat-1)) - (lat(2) - lat(1))) > 1e-6) ) then
  write(message,'(a)') 'Only regular lat/lon grids are supported'
  call radsim_error_report(message, status_error)
end if

do imodel = 1, model_ntimes
  model(imodel) % grid % type = 0
  model(imodel) % grid % lon1 = lon(1)
  model(imodel) % grid % lat1 = lat(1)
  model(imodel) % grid % dlon = lon(2) - lon(1)
  model(imodel) % grid % dlat = lat(2) - lat(1)
  model(imodel) % grid % row_order = .true.
  model(imodel) % grid % ncols = dim_n_lon
  model(imodel) % grid % nrows = dim_n_lat
  model(imodel) % grid % pole_lon = 0
  model(imodel) % grid % pole_lat = 0
end do
deallocate(lat, lon)

if ( output_mode >= output_verbose ) then
  print '(a,i0)', 'nprofiles = ', nprofs
  print '(a,i0)', 'nlevels = ', nlevels
  print '(a,i0)', 'nlatitudes = ', dim_n_lat
  print '(a,i0)', 'nlongitudes = ', dim_n_lon
  print '(a,f8.2)', 'latitude of first point (degrees) = ', model(1) % grid % lat1
  print '(a,f8.2)', 'longitude of first point (degrees) = ', model(1) % grid % lon1
  print '(a,f8.2)', 'delta-latitude (degrees) = ', model(1) % grid % dlat
  print '(a,f8.2)', 'delta-longitude (degrees) = ', model(1) % grid % dlon
end if

!---------
! Set time
!---------

! Extract date/time from units attribute of time dimension and specify validity
! and data times. Validity time is set to data time of first model dataset.

allocate(time(model_ntimes))
call radsim_nf90_get_var(file_id, 'time', time)
call radsim_nf90_get_att(file_id, 'time', 'units', time_units)

if ( any(time < 0) ) then
  write(message,'(a)') 'Error: time dataset contains negative values'
  call radsim_error_report(message, status_error)
end if

if ( time_units(1:12) /= 'hours since ' ) then
  write(message,'(a)') 'Error: time dataset units attribute must specify '// &
                       '"hours since" a particular date'
  call radsim_error_report(message, status_error)
end if

write(message,'(a)') 'Error reading reference time from time dataset units attribute'
read (time_units(13:28),'(i4,4(1x,i2))', iostat=stat) ref_date_time(1:5)
if ( stat /= 0 ) call radsim_error_report(message, status_error)

do imodel = 1, model_ntimes
  allocate(model(imodel) % validity_time(1,5), model(imodel) % data_time(1,5))

  ! Compute validity time for each imodel
  model(imodel) % validity_time(1,:) = ref_date_time
  call date_time_plus_minutes(time(imodel) * 60, model(imodel) % validity_time(1,:))
  model(imodel) % ref_time = time_in_minutes(model(imodel) % validity_time(1,:))

  ! Set the data time for each imodel to the validity time of imodel=1
  model(imodel) % data_time(1,:) = model(1) % validity_time(1,:)
end do

deallocate(time)


!------------
! Read fields
!------------

! All datasets in the atmospheric file have the same size
allocate(values_in3D(dim_n_lon, dim_n_lat, nlevels))

do imodel = 1, model_ntimes

  ! Search through the field_list created above and read first dataset found
  ! for each RTTOV field
  do i = 1, nfields_rttov
    do j = 1, 3
      if ( trim(field_list(i,j)) == '' ) cycle

      var_name = trim(field_list(i,j))

      if ( radsim_var_exists(file_id, trim(var_name)) ) then
        zero_flag = .true.
        select case(trim(var_name))
          case('p')
            if (.not. associated(model(imodel) % p)) allocate(model(imodel) % p(nprofs, nlevels))
            values2D => model(imodel) % p
            zero_flag = .false.
          case('t')
            if (.not. associated(model(imodel) % t)) allocate(model(imodel) % t(nprofs, nlevels))
            values2D => model(imodel) % t
            zero_flag = .false.
          case('theta')
            if (.not. associated(model(imodel) % theta)) allocate(model(imodel) % theta(nprofs, nlevels))
            values2D => model(imodel) % theta
            zero_flag = .false.
          case('q')
            if (.not. associated(model(imodel) % q)) allocate(model(imodel) % q(nprofs, nlevels))
            values2D => model(imodel) % q
            zero_flag = .false.
          case('rh')
            if (.not. associated(model(imodel) % rh)) allocate(model(imodel) % rh(nprofs, nlevels))
            values2D => model(imodel) % rh
            zero_flag = .false.
          case('o3')
            if (.not. associated(model(imodel) % o3)) allocate(model(imodel) % o3(nprofs, nlevels))
            values2D => model(imodel) % o3
            zero_flag = .false.
          case('cc')
            if (.not. associated(model(imodel) % cfrac)) allocate(model(imodel) % cfrac(nprofs, nlevels))
            values2D => model(imodel) % cfrac
          case('clwc')
            if (.not. associated(model(imodel) % clw)) allocate(model(imodel) % clw(nprofs, nlevels))
            values2D => model(imodel) % clw
          case('ciwc')
            if (.not. associated(model(imodel) % ciw)) allocate(model(imodel) % ciw(nprofs, nlevels))
            values2D => model(imodel) % ciw
          case('crwc')
            if (.not. associated(model(imodel) % rain)) allocate(model(imodel) % rain(nprofs, nlevels))
            values2D => model(imodel) % rain
          case('cswc')
            if (.not. associated(model(imodel) % snow)) allocate(model(imodel) % snow(nprofs, nlevels))
            values2D => model(imodel) % snow
          case default
            cycle
        end select

        if ( output_mode >= output_default ) then
          print '(a,i0,a)', 'Reading ', nlevels, ' levels of data for variable '//trim(var_name)
        end if

        ! Read dataset attributes
        call radsim_nf90_get_att(file_id, trim(var_name), 'missing_value', missingvalue)
        call radsim_nf90_get_att(file_id, trim(var_name), 'scale_factor', sfac, sfac_exists)
        call radsim_nf90_get_att(file_id, trim(var_name), 'add_offset', aoff, aoff_exists)

        ! Read dataset
        call radsim_nf90_get_var(file_id, trim(var_name), values_in3D, imodel)

        ! Apply scale factor and offset if specified
        if ( sfac_exists ) then
          where ( values_in3D /= missingvalue ) values_in3D = values_in3D * sfac
        end if
        if ( aoff_exists ) then
          where ( values_in3D /= missingvalue ) values_in3D = values_in3D + aoff
        end if

        ! Rounding/precision issues related to scaling/offset can result in very
        ! small negative values in fields which must be non-negative so reset
        ! any values below a given tolerance to zero.
        if ( (sfac_exists .or. aoff_exists) .and. zero_flag ) then
          where ( values_in3D /= missingvalue .and. &
                  abs(values_in3D) < tol ) values_in3D = 0.
        end if

        ! Use RadSim default model RMDI
        where ( values_in3D == missingvalue ) values_in3D = model(imodel) % rmdi

        ! Copy data to model field
        values2D = reshape(values_in3D, (/ nprofs, nlevels /))

        got_field(i) = .true.

      end if ! dataset exists

    end do
  end do

end do ! imodel

deallocate(values_in3D)
call radsim_nf90_close(file_id)

!-------------------------------------------------------------------------------
! 2b. Read surface data from second (ancillary) netCDF file
!-------------------------------------------------------------------------------

print '(a)', 'Reading netCDF file ' // trim(file_name_ancil)

call radsim_nf90_open(trim(file_name_ancil), file_id)

! ----------------
! Check dimensions
! ----------------

call radsim_nf90_inquire_dimension_length(file_id, 'longitude', dim_n_lon)
call radsim_nf90_inquire_dimension_length(file_id, 'latitude', dim_n_lat)
call radsim_nf90_inquire_dimension_length(file_id, 'time', dim_n_time)

if ( (temporal_data .and. dim_n_time /= model_ntimes) .or. &
     dim_n_lat * dim_n_lon /= nprofs ) then
  write(message,'(a)') 'Dimensions of surface file do match those of atmosphere file'
  call radsim_error_report(message, status_error)
end if

!------------
! Read fields
!------------

! All datasets in the surface file have the same size
allocate(values_in2D(dim_n_lon, dim_n_lat))

do imodel = 1, model_ntimes

  ! Search through the field_list created above and read first dataset found
  ! for each RTTOV field
  do i = 1, nfields_rttov
    do j = 1, 3
      if ( trim(field_list(i,j)) == '' ) cycle

      var_name = trim(field_list(i,j))

      if ( radsim_var_exists(file_id, trim(var_name)) ) then
        zero_flag = .true.
        select case(trim(var_name))
          case('skt')
            if (.not. associated(model(imodel) % tskin)) allocate(model(imodel) % tskin(nprofs))
            values1D => model(imodel) % tskin
          case('t2m')
            if (.not. associated(model(imodel) % t2)) allocate(model(imodel) % t2(nprofs))
            values1D => model(imodel) % t2
          case('d2m')
            if (.not. associated(model(imodel) % td2)) allocate(model(imodel) % td2(nprofs))
            values1D => model(imodel) % td2
          case('sp')
            if (.not. associated(model(imodel) % pstar)) allocate(model(imodel) % pstar(nprofs))
            values1D => model(imodel) % pstar
          case('lnsp')
            if (.not. associated(model(imodel) % pstar)) allocate(model(imodel) % pstar(nprofs))
            values1D => model(imodel) % pstar
            zero_flag = .false.
          case('u10')
            if (.not. associated(model(imodel) % u10)) allocate(model(imodel) % u10(nprofs))
            values1D => model(imodel) % u10
            zero_flag = .false.
          case('v10')
            if (.not. associated(model(imodel) % v10)) allocate(model(imodel) % v10(nprofs))
            values1D => model(imodel) % v10
            zero_flag = .false.
          case('lsm')
            if (.not. associated(model(imodel) % lsm)) allocate(model(imodel) % lsm(nprofs))
            values1D => model(imodel) % lsm
          case('siconc')
            if (.not. associated(model(imodel) % seaice)) allocate(model(imodel) % seaice(nprofs))
            values1D => model(imodel) % seaice
          case('z')
            if (.not. associated(model(imodel) % zsurf)) allocate(model(imodel) % zsurf(nprofs))
            values1D => model(imodel) % zsurf
            zero_flag = .false.
          case('tcc')
            if (.not. associated(model(imodel) % total_cc)) allocate(model(imodel) % total_cc(nprofs))
            values1D => model(imodel) % total_cc
          case('lcc')
            if (.not. associated(model(imodel) % low_cc)) allocate(model(imodel) % low_cc(nprofs))
            values1D => model(imodel) % low_cc
          case('mcc')
            if (.not. associated(model(imodel) % medium_cc)) allocate(model(imodel) % medium_cc(nprofs))
            values1D => model(imodel) % medium_cc
          case('hcc')
            if (.not. associated(model(imodel) % high_cc)) allocate(model(imodel) % high_cc(nprofs))
            values1D => model(imodel) % high_cc
          case default
            cycle
        end select

        if ( output_mode >= output_default ) then
          print '(a)', 'Reading 1 level of data for variable '//trim(var_name)
        end if

        ! Read dataset attributes
        call radsim_nf90_get_att(file_id, trim(var_name), 'missing_value', missingvalue)
        call radsim_nf90_get_att(file_id, trim(var_name), 'scale_factor', sfac, sfac_exists)
        call radsim_nf90_get_att(file_id, trim(var_name), 'add_offset', aoff, aoff_exists)

        ! Read dataset
        call radsim_nf90_get_var(file_id, trim(var_name), values_in2D, imodel)

        ! Apply scale factor and offset if specified
        if ( sfac_exists ) then
          where ( values_in2D /= missingvalue ) values_in2D = values_in2D * sfac
        end if
        if ( aoff_exists ) then
          where ( values_in2D /= missingvalue ) values_in2D = values_in2D + aoff
        end if

        ! Rounding/precision issues related to scaling/offset can result in very
        ! small negative values in fields which must be non-negative so reset
        ! any values below a given tolerance to zero.
        if ( (sfac_exists .or. aoff_exists) .and. zero_flag ) then
          where ( values_in2D /= missingvalue .and. &
                  abs(values_in2D) < tol ) values_in2D = 0.
        end if

        ! Use RadSim default model RMDI
        where ( values_in2D == missingvalue ) values_in2D = model(imodel) % rmdi

        ! Copy data to model field
        values1D = reshape(values_in2D, (/ nprofs /))

        got_field(i) = .true.

        !-----------------
        ! Unit conversions
        !-----------------

        if ( trim(var_name) == 'z' ) then
          model(imodel) % zsurf = model(imodel) % zsurf / gravity  ! Geopotential -> surface elevation
        else if ( trim(var_name) == 'lnsp' ) then
          model(imodel) % pstar = exp(model(imodel) % pstar)       ! ln(psurf) -> psurf
        end if

      end if ! dataset exists

    end do
  end do

end do ! imodel

deallocate(values_in2D)
call radsim_nf90_close(file_id)

!--------------------------
! Set pressure level values
!--------------------------

! In the ECMWF model, half-level pressures are a linear function of surface
! pressure. The netCDF files do not include the coefficients, so RadSim relies
! on the coefficients stored in the code for standard sets of levels.

do imodel = 1, model_ntimes

  if ( associated(model(imodel) % pstar) ) then

    if ( output_mode >= output_verbose ) print '(a)', 'Calculating pressure levels'

    ! Always allocate and calculate half-level pressures
    allocate(model(imodel) % ph(nprofs,nlevels+1))

    if ( .not. associated(model(imodel) % p) ) then
      ! if pressure not read in then allocate and calculate here
      allocate(model(imodel) % p(nprofs,nlevels))
      call radsim_calc_plevels( &
        'ecmwf',               &
        model(imodel) % pstar, &
        model(imodel) % p,     &
        model(imodel) % ph     )
    else
      ! if we read pressure in from file, calculate only half-level pressures
      call radsim_calc_plevels( &
        'ecmwf',                &
        model(imodel) % pstar,  & 
        ph = model(imodel) % ph )
    end if

    got_field(field_rttov_p) = .true.

  end if

end do

!-------------------------------------------------------------------------------
! 3. Derivations
!-------------------------------------------------------------------------------

do imodel = 1, model_ntimes
  if ( associated(model(imodel) % p) ) then
    model(imodel) % nprofs = size(model(imodel) % p, dim=1)
    model(imodel) % nlevels = size(model(imodel) % p, dim=2)
  end if

  ! LSM from seaice fraction if not present

  if ( .not. associated(model(imodel) % lsm) .and. &
       associated(model(imodel) % seaice) ) then
    print '(a)', 'No LSM: Deriving LSM from seaice fraction field'
    allocate(model(imodel) % lsm(nprofs))
    where(model(imodel) % seaice == model(imodel) % rmdi)
      model(imodel) % lsm = 1.0
    elsewhere
      model(imodel) % lsm = 0.0
    end where
  end if
end do

!-------------------------------------------------------------------------------
! 4. Check fields
!-------------------------------------------------------------------------------

do i = 1, size(got_field)
  if ( .not. got_field(i) ) then
    call radsim_error_report( &
      'No data in netCDF file for required RTTOV field: ' // field_names_rttov(i), &
      status_error)
  end if
end do

end subroutine radsim_read_netcdf
