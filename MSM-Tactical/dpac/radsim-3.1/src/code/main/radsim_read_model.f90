!-------------------------------------------------------------------------------
! Description:
!
!   Read atmospheric profile data
!
!   Currently the following formats are supported: 
!     UM fieldsfiles and PP files
!     GRIB files containing ECMWF or ICON data
!     NetCDF files containing ECMWF data
!     NWP SAF 60-, 91- or 137-level profile datasets
!   Users must supply the correct file type in the configuration namelist.
!
!   All data is read into the model data structure
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

subroutine radsim_read_model(model_ntimes, model)

use radsim_mod_cfg

use radsim_mod_constants, only : &
  status_error, &
  status_warning, &
  output_default

use radsim_mod_io, only : &
  filetype_fieldsfile, &
  filetype_grib_ecmwf, &
  filetype_grib_icon, &
  filetype_grib_harmonie, &
  filetype_grib_jma, &
  filetype_netcdf_ecmwf, &
  filetype_ecprof60, &
  filetype_ecprof91, &
  filetype_ecprof137, &
  filetype_ncepmsm

use radsim_mod_types, only : &
  model_type

implicit none

include 'radsim_check_fields.interface'
include 'radsim_convert_fields.interface'
include 'radsim_error_report.interface'
include 'radsim_grid_calc.interface'
include 'radsim_print_grid.interface'
include 'radsim_read_ecprof60.interface'
include 'radsim_read_ecprof91.interface'
include 'radsim_read_ecprof137.interface'
include 'radsim_read_fieldsfile.interface'
include 'radsim_read_grib.interface'
include 'radsim_read_netcdf.interface'
include 'radsim_read_pp.interface'
include 'radsim_read_msm.interface'

! Subroutine args
integer,            intent(out) :: model_ntimes
type(model_type),   intent(out) :: model(:)

! Local variables
integer :: p1, i
character(len=80) :: message

!-------------------------------------------------------------------------------

! radsim_read_cfg already checked for the existence of input model file(s)
if ( trim(model_ancil_datafile) /= '' ) then
  print '(2a)', 'Reading from model data files: '
  print '(2a)', '  ', trim(model_datafile)
  print '(2a)', '  ', trim(model_ancil_datafile)
else if ( trim(model_ancil2_datafile) /= '' ) then
  print '(2a)', 'Reading from model data files: '
  print '(2a)', '  ', trim(model_datafile)
  print '(2a)', '  ', trim(model_ancil_datafile)
  print '(2a)', '  ', trim(model_ancil2_datafile)
else
  print '(2a)', 'Reading from model data file ', trim(model_datafile)
end if

!------------------------------
! 1. UM fieldsfile (or PP file)
!------------------------------

if ( model_filetype == filetype_fieldsfile ) then

  p1 = len_trim(model_datafile) - 2

  if ( model_datafile(p1:p1+2) == '.pp' ) then

    call radsim_read_pp( &
      model_datafile, & ! in
      model(1)        ) ! out

    ! Temporal interpolation not yet implemented for UM PP files
    model_ntimes = 1
    if ( temporal_data ) then
      call radsim_error_report( &
        'Temporal interpolation not yet implemented for UM PP files', &
        status_warning)
    end if
    temporal_data = .false.

  else

    call radsim_read_fieldsfile( &
      model_datafile, & ! in
      model_ntimes,   & ! out
      model           ) ! out

  end if

!-------------
! 2. GRIB file
!-------------

else if ( model_filetype == filetype_grib_ecmwf    .or. &
          model_filetype == filetype_grib_icon     .or. &
          model_filetype == filetype_grib_harmonie .or. &
          model_filetype == filetype_grib_jma ) then

  call radsim_read_grib( &
    model_datafile,     & ! in
    model_ntimes,       & ! out
    model,              & ! out
    model_ancil_datafile) ! in

!-------------------------------------
! 3. ECMWF 60L profile set for NWP SAF
!-------------------------------------

else if ( model_filetype == filetype_ecprof60 ) then

  model_ntimes = 1

  call radsim_read_ecprof60( &
    model_datafile, & ! in
    model(1)        ) ! out

!-------------------------------------
! 4. ECMWF 91L profile set for NWP SAF
!-------------------------------------

else if ( model_filetype == filetype_ecprof91 ) then

  model_ntimes = 1

  call radsim_read_ecprof91( &
    model_datafile, & ! in
    model(1)        ) ! out

!--------------------------------------
! 5. ECMWF 137L profile set for NWP SAF
!--------------------------------------

else if ( model_filetype == filetype_ecprof137 ) then

  model_ntimes = 1

  call radsim_read_ecprof137( &
    model_datafile, & ! in
    model(1)        ) ! out

!---------------
! 6. NetCDF file
!---------------

else if ( model_filetype == filetype_netcdf_ecmwf ) then

  call radsim_read_netcdf( &
    model_datafile,     & ! in
    model_ntimes,       & ! out
    model,              & ! out
    model_ancil_datafile) ! in

!---------------
! 7. NCEP MSM
!---------------

else if ( model_filetype == filetype_ncepmsm ) then

  call radsim_read_msm( &
    model_datafile,       & ! in
    model_ntimes,         & ! out
    model,                & ! out
    model_ancil_datafile, & ! in
    model_ancil2_datafile ) ! in

else

  write(message,'(a,i0)') &
    'Invalid file type for model profile dataset: ', model_filetype
  call radsim_error_report(message, status_error)

end if

!------------------
! 5. Convert fields
!------------------

do i = 1, model_ntimes
  call radsim_convert_fields(model(i))

  ! Check to see if required fields are present
  call radsim_check_fields(model(i))
end do


!------------------------------------
! 6. Calculate model grid coordinates
!------------------------------------

select case(model(1) % grid % type)

  case(0,1,2,3,10)

    print '(a)', 'Calculating coordinates for lat,lon grid'

    call radsim_grid_calc(model(1) % grid)

    print '(a,2f8.2)', ' Latitude range = ', &
      model(1) % grid % lat(1), model(1) % grid % lat(model(1) % nprofs)
    print '(a,2f8.2)', ' Longitude range = ', &
      model(1) % grid % lon(1), model(1) % grid % lon(model(1) % nprofs)

    if ( output_mode >= output_default ) then
      call radsim_print_grid(model(1) % grid)
    end if

  case(101)

    print '(a)', 'Unstructured grid, using lat,lon coordinates provided.'

  case default

    print '(a)', 'Not a regular grid. Assuming lat,lon coordinates provided.'

    ! Switch off obs interpolation if not a regular grid

    if ( obs_grid ) then
      call radsim_error_report( &
        'Cannot interpolate to obs positions ', status_warning)
      print '(a)', '...Continuing on model grid'
      obs_grid = .false.
    end if

end select

!-----------
! 7. Tidy up
!-----------

! Don't need half-levels if not running scattering code

if ( .not. run_scatt ) then
  do i = 1, model_ntimes
    if ( associated(model(i) % ph) ) deallocate(model(i) % ph)
  end do
end if

end subroutine radsim_read_model
