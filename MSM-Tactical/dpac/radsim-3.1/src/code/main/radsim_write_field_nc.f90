!-------------------------------------------------------------------------------
! Description:
!
!   Utility routines for output to a netCDF file. Note that all routines assume
!   4-byte output values, ioncluding when the input data are 8-byte values.
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

subroutine write_field_check(name, status)

implicit none

character(len=*), intent(in)  :: name
integer(int32),   intent(out) :: status
integer :: i

do i = 1, size(written_fields)
  if ( written_fields(i) == '' ) then
    written_fields(i) = name
    status = 0
    exit
  else if ( written_fields(i) == name ) then
    if ( output_mode >= output_debug ) then
      print '(a)', ' ' // trim(name) // ' field already exists.'
    end if
    status = 1
    exit
  end if
end do

end subroutine write_field_check

subroutine radsim_write_field_nc_int_1D(field, name, dim, dataset_id, start)

implicit none

integer(int32),   intent(in) :: field(:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dim
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(1)

integer(int32) :: var_id, status, n(1)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_int, (/dim/), var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_int_1D

subroutine radsim_write_field_nc_int64_1D(field, name, dim, dataset_id, start)

implicit none

integer(int64),   intent(in) :: field(:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dim
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(1)

integer(int32) :: var_id, status, n(1)

call write_field_check(name, status)

if ( .not. status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_int, (/dim/), var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_int64_1D

subroutine radsim_write_field_nc_real_1D(field, name, dim, dataset_id, start)

implicit none

real(real32),     intent(in) :: field(:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dim
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(1)

integer(int32) :: var_id, status, n(1)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_float, (/dim/), var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_real_1D

subroutine radsim_write_field_nc_real64_1D(field, name, dim, dataset_id, start)

implicit none

real(real64),     intent(in) :: field(:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dim
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(1)

integer(int32) :: var_id, status, n(1)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_float, (/dim/), var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_real64_1D

subroutine radsim_write_field_nc_int_2D(field, name, dims, dataset_id, start)

implicit none

integer(int32),   intent(in) :: field(:,:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dims(2)
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(2)

integer(int32) :: var_id, status, n(2)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_int, dims, var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_int_2D

subroutine radsim_write_field_nc_int64_2D(field, name, dims, dataset_id, start)

implicit none

integer(int64),   intent(in) :: field(:,:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dims(2)
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(2)

integer(int32) :: var_id, status, n(2)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_int, dims, var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_int64_2D

subroutine radsim_write_field_nc_real_2D(field, name, dims, dataset_id, start)

implicit none

real(real32),     intent(in) :: field(:,:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dims(2)
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(2)

integer(int32) :: var_id, status, n(2)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_float, dims, var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_real_2D

subroutine radsim_write_field_nc_real64_2D(field, name, dims, dataset_id, start)

implicit none

real(real64),     intent(in) :: field(:,:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dims(2)
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(2)

integer(int32) :: var_id, status, n(2)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_float, dims, var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_real64_2D

subroutine radsim_write_field_nc_real_3D(field, name, dims, dataset_id, start)

implicit none

real(real32),     intent(in) :: field(:,:,:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dims(3)
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(3)

integer(int32) :: var_id, status, n(3)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_float, dims, var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_real_3D

subroutine radsim_write_field_nc_real64_3D(field, name, dims, dataset_id, start)

implicit none

real(real64),     intent(in) :: field(:,:,:)
character(len=*), intent(in) :: name
integer(int32),   intent(in) :: dims(3)
integer(int32),   intent(in) :: dataset_id
integer(int32), optional, intent(in) :: start(3)

integer(int32) :: var_id, status, n(3)

call write_field_check(name, status)

if ( status == 0 ) then

  call radsim_nf90_redef(dataset_id)
  call radsim_nf90_def_var(dataset_id, name, nf90_float, dims, var_id)
  call radsim_nf90_enddef(dataset_id)

else

  call radsim_nf90_inq_varid(dataset_id, name, var_id)

end if

if ( .not. present(start) ) then

  call radsim_nf90_put_var(dataset_id, var_id, field)

else

  n = shape(field)
  call radsim_nf90_put_var(dataset_id, var_id, field, start=start, count=n)

end if

if ( output_mode >= output_verbose ) then
  print '(a)', 'Written variable: ' // trim(name)
end if

end subroutine radsim_write_field_nc_real64_3D
