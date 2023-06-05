!-------------------------------------------------------------------------------
! General functions
!-------------------------------------------------------------------------------

! Open a file for read-only access

subroutine radsim_nf90_open(file_name, file_id)
  character(len=*), intent(in)  :: file_name
  integer(int32),   intent(out) :: file_id
  integer(int32) :: status
  status = nf90_open(file_name, nf90_nowrite, file_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error opening netcdf file ' // file_name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a)', 'Opened netCDF file: ' // trim(file_name)
  end if
end subroutine radsim_nf90_open

! Create a file

subroutine radsim_nf90_create(file_name, cmode, file_id)
  character(len=*), intent(in)  :: file_name
  integer(int32),   intent(in)  :: cmode
  integer(int32),   intent(out) :: file_id
  integer(int32) :: status
  status = nf90_create(file_name, cmode, file_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error creating netcdf file ' // file_name, status_error)
  else if ( output_mode >= 1 ) then
    print '(a)', 'Created new netCDF file: ' // trim(file_name)
  end if
end subroutine radsim_nf90_create

! Close a file

subroutine radsim_nf90_close(file_id)
  integer(int32), intent(in) :: file_id
  integer(int32) :: status
  status = nf90_close(file_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error closing netcdf file', status_error)
  end if
end subroutine radsim_nf90_close

! Exit define mode

subroutine radsim_nf90_enddef(file_id)
  integer(int32),   intent(in)  :: file_id
  integer(int32) :: status
  status = nf90_enddef(file_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error exiting netcdf define mode', status_error)
  end if
end subroutine radsim_nf90_enddef

! Enter define mode

subroutine radsim_nf90_redef(file_id)
  integer(int32),   intent(in)  :: file_id
  integer(int32) :: status
  status = nf90_redef(file_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error entering netcdf define mode', status_error)
  end if
end subroutine radsim_nf90_redef

! Define a dimension

subroutine radsim_nf90_def_dim(file_id, name, nvalues, dim_id)
  integer(int32),   intent(in)  :: file_id
  character(len=*), intent(in)  :: name
  integer(int32),   intent(in)  :: nvalues
  integer(int32),   intent(out) :: dim_id
  integer(int32) :: status
  status = nf90_def_dim(file_id, name, nvalues, dim_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error defining netcdf dimension ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a,i0)', 'Defined new dimension: ' // trim(name) // ' = ', nvalues
  end if
end subroutine radsim_nf90_def_dim

! Inquire a dimension id

subroutine radsim_nf90_inq_dimid(file_id, name, dim_id)
  integer(int32),   intent(in)  :: file_id
  character(len=*), intent(in)  :: name
  integer(int32),   intent(out) :: dim_id
  integer(int32) :: status
  status = nf90_inq_dimid(file_id, name, dim_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error inquiring for netcdf dimension ' // name, status_error)
  end if
end subroutine radsim_nf90_inq_dimid

! Inquire a dimension

subroutine radsim_nf90_inquire_dimension(file_id, dim_id, name, len)
  integer(int32),             intent(in)  :: file_id
  integer(int32),             intent(in)  :: dim_id
  character(len=*), optional, intent(out) :: name
  integer(int32), optional,   intent(out) :: len
  integer(int32) :: status
  status = nf90_noerr
  if ( present(len) .and. present(name) ) then
    status = nf90_inquire_dimension(file_id, dim_id, name=name, len=len)
  else if ( present(len) ) then
    status = nf90_inquire_dimension(file_id, dim_id, len=len)
  else if ( present(name) ) then
    status = nf90_inquire_dimension(file_id, dim_id, name=name)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error inquiring for netcdf dimension', status_error)
  end if
end subroutine radsim_nf90_inquire_dimension

! Inquire a dimension length directly from the name

subroutine radsim_nf90_inquire_dimension_length(file_id, name, len)
  integer(int32),   intent(in)  :: file_id
  character(len=*), intent(in)  :: name
  integer(int32),   intent(out) :: len
  integer(int32) :: status, dim_id
  status = nf90_inq_dimid(file_id, name, dim_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error inquiring for netcdf dimension', status_error)
  end if
  status = nf90_inquire_dimension(file_id, dim_id, len=len)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error inquiring for netcdf dimension', status_error)
  end if
end subroutine radsim_nf90_inquire_dimension_length

! Check whether a variable exists

logical function radsim_var_exists(dataset_id, name)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, name, var_id)
  radsim_var_exists = status == nf90_noerr
end function radsim_var_exists

!-------------------------------------------------------------------------------
! Variable functions
!-------------------------------------------------------------------------------

! Read variable attribute

subroutine radsim_nf90_get_att_char(dataset_id, dname, aname, value)
  integer(int32),   intent(in)    :: dataset_id
  character(len=*), intent(in)    :: dname, aname
  character(len=*), intent(inout) :: value
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, dname, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf attribute', status_error)
  end if
  status = nf90_get_att(dataset_id, var_id, aname, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf attribute', status_error)
  end if
end subroutine radsim_nf90_get_att_char

subroutine radsim_nf90_get_att_int_scalar(dataset_id, var_id, aname, value)
  integer(int32),   intent(in)    :: dataset_id
  integer(int32),   intent(in)    :: var_id
  character(len=*), intent(in)    :: aname
  integer,          intent(inout) :: value
  integer(int32) :: status
  status = nf90_get_att(dataset_id, var_id, aname, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf attribute', status_error)
  end if
end subroutine radsim_nf90_get_att_int_scalar

subroutine radsim_nf90_get_att_real64(dataset_id, dname, aname, value, exists)
  integer(int32),   intent(in)    :: dataset_id
  character(len=*), intent(in)    :: dname, aname
  real(real64),     intent(inout) :: value
  logical,          optional, intent(out) :: exists
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, dname, var_id)
  if ( present(exists) ) then
    exists = status == nf90_noerr
    if ( exists ) then
      status = nf90_get_att(dataset_id, var_id, aname, value)
      exists = status == nf90_noerr
    end if
  else
    if ( status /= nf90_noerr ) then
      write(error_unit,'(a)') trim(nf90_strerror(status))
      call radsim_error_report('Error reading netcdf attribute', status_error)
    end if
    status = nf90_get_att(dataset_id, var_id, aname, value)
    if ( status /= nf90_noerr ) then
      write(error_unit,'(a)') trim(nf90_strerror(status))
      call radsim_error_report('Error reading netcdf attribute', status_error)
    end if
  end if
end subroutine radsim_nf90_get_att_real64

! Read variable data

subroutine radsim_nf90_get_var_int_1D(dataset_id, name, values)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  integer,          intent(out) :: values(:)
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, name, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
  status = nf90_get_var(dataset_id, var_id, values)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
end subroutine radsim_nf90_get_var_int_1D

subroutine radsim_nf90_get_var_real_1D(dataset_id, name, values)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  real,             intent(out) :: values(:)
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, name, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
  status = nf90_get_var(dataset_id, var_id, values)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
end subroutine radsim_nf90_get_var_real_1D

subroutine radsim_nf90_get_var_real64_2D(dataset_id, name, values, itime)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  real(real64),     intent(out) :: values(:,:)
  integer,          optional, intent(in) :: itime
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, name, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
  if ( present(itime) ) then
    status = nf90_get_var(dataset_id, var_id, values, start=(/1, 1, itime/))
  else
    status = nf90_get_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
end subroutine radsim_nf90_get_var_real64_2D

subroutine radsim_nf90_get_var_real64_3D(dataset_id, name, values, itime)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  real(real64),     intent(out) :: values(:,:,:)
  integer,          optional, intent(in) :: itime
  integer(int32) :: status, var_id
  status = nf90_inq_varid(dataset_id, name, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
  if ( present(itime) ) then
    status = nf90_get_var(dataset_id, var_id, values, start=(/1, 1, 1, itime/))
  else
    status = nf90_get_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error reading netcdf variable', status_error)
  end if
end subroutine radsim_nf90_get_var_real64_3D

! Define a variable

subroutine radsim_nf90_def_var(dataset_id, name, xtype, dims, var_id)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  integer(int32),   intent(in)  :: xtype
  integer(int32),   intent(in)  :: dims(:)
  integer(int32),   intent(out) :: var_id
  integer(int32) :: status
  integer(int32) :: shuffle, deflate, deflate_level
  status = nf90_def_var(dataset_id, name, xtype, dims, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error defining netcdf variable ' // name, status_error)
  else if ( output_mode >= 3 ) then
    print '(a)', 'Defined netCDF variable: ' // trim(name)
  end if

  shuffle = 1
  deflate = 1
  deflate_level = 2  ! Recommended here:
  ! https://www.unidata.ucar.edu/software/netcdf/workshops/2011/nc4chunking/CompressionAdvice.html
  status = nf90_def_var_deflate(dataset_id, var_id, shuffle, deflate, deflate_level)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error defining netcdf variable deflate parameters ' // name, status_error)
  else if ( output_mode >= 3 ) then
    print '(a)', 'Defined deflate parameters for netCDF variable: ' // trim(name)
  end if
end subroutine radsim_nf90_def_var

! Get variable ID

subroutine radsim_nf90_inq_varid(dataset_id, name, var_id)
  integer(int32),   intent(in)  :: dataset_id
  character(len=*), intent(in)  :: name
  integer(int32),   intent(out) :: var_id
  integer(int32) :: status
  status = nf90_inq_varid(dataset_id, name, var_id)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error inquiring for netcdf variable ' // name, status_error)
  end if
end subroutine radsim_nf90_inq_varid

! Write variable data

subroutine radsim_nf90_put_var_int_1D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  integer,        intent(in) :: values(:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_int_1D

subroutine radsim_nf90_put_var_int64_1D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  integer(int64), intent(in) :: values(:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_int64_1D

subroutine radsim_nf90_put_var_real_1D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  real,           intent(in) :: values(:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_real_1D

subroutine radsim_nf90_put_var_real64_1D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  real(real64),   intent(in) :: values(:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_real64_1D

subroutine radsim_nf90_put_var_int_2D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  integer,        intent(in) :: values(:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_int_2D

subroutine radsim_nf90_put_var_int64_2D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  integer(int64), intent(in) :: values(:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_int64_2D

subroutine radsim_nf90_put_var_real_2D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  real,           intent(in) :: values(:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_real_2D

subroutine radsim_nf90_put_var_real64_2D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  real(real64),   intent(in) :: values(:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_real64_2D

subroutine radsim_nf90_put_var_int_3D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  integer,        intent(in) :: values(:,:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_int_3D

subroutine radsim_nf90_put_var_real_3D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  real,           intent(in) :: values(:,:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_real_3D

subroutine radsim_nf90_put_var_real64_3D(dataset_id, var_id, values, start, count)
  integer(int32), intent(in) :: dataset_id
  integer(int32), intent(in) :: var_id
  real(real64),   intent(in) :: values(:,:,:)
  integer(int32), optional, intent(in) :: start(:)
  integer(int32), optional, intent(in) :: count(:)
  integer(int32) :: status
  if ( present(start) .and. present(count) ) then
    status = nf90_put_var(dataset_id, var_id, values, start=start, count=count)
  else
    status = nf90_put_var(dataset_id, var_id, values)
  end if
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error writing netcdf variable', status_error)
  end if
end subroutine radsim_nf90_put_var_real64_3D

!-------------------------------------------------------------------------------
! Attribute functions
!-------------------------------------------------------------------------------

! Write an attribute

subroutine radsim_nf90_put_att_char(file_id, var_id, name, value)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  character(len=*), intent(in) :: value
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a)', 'Written attribute: ' // trim(name) // ' = ' // trim(value)
  end if
end subroutine radsim_nf90_put_att_char

subroutine radsim_nf90_put_att_int_scalar(file_id, var_id, name, value)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  integer,          intent(in) :: value
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a,i0)', 'Written attribute: ' // trim(name) // ' = ', value
  end if
end subroutine radsim_nf90_put_att_int_scalar

subroutine radsim_nf90_put_att_int64_scalar(file_id, var_id, name, value)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  integer(int64),  intent(in) :: value
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a,i0)', 'Written attribute: ' // trim(name) // ' = ', value
  end if
end subroutine radsim_nf90_put_att_int64_scalar

subroutine radsim_nf90_put_att_real_scalar(file_id, var_id, name, value)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  real,             intent(in) :: value
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a,e12.4)', 'Written attribute: ' // trim(name) // ' = ', value
  end if
end subroutine radsim_nf90_put_att_real_scalar

subroutine radsim_nf90_put_att_real64_scalar(file_id, var_id, name, value)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  real(real64),     intent(in) :: value
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, value)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a,e16.6)', 'Written attribute: ' // trim(name) // ' = ', value
  end if
end subroutine radsim_nf90_put_att_real64_scalar

subroutine radsim_nf90_put_att_int_1D(file_id, var_id, name, values)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  integer,          intent(in) :: values(:)
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, values)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a)', 'Written attribute: ' // trim(name) // ' = '
    print '(10(1x,i0))', values
  end if
end subroutine radsim_nf90_put_att_int_1D

subroutine radsim_nf90_put_att_int64_1D(file_id, var_id, name, values)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  integer(int64),   intent(in) :: values(:)
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, values)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a)', 'Written attribute: ' // trim(name) // ' = '
    print '(10(1x,i0))', values
  end if
end subroutine radsim_nf90_put_att_int64_1D

subroutine radsim_nf90_put_att_real_1D(file_id, var_id, name, values)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  real,             intent(in) :: values(:)
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, values)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a)', 'Written attribute: ' // trim(name) // ' = '
    print '(10(1x,e16.6))', values
  end if
end subroutine radsim_nf90_put_att_real_1D

subroutine radsim_nf90_put_att_real64_1D(file_id, var_id, name, values)
  integer(int32),   intent(in) :: file_id
  integer(int32),   intent(in) :: var_id
  character(len=*), intent(in) :: name
  real(real64),     intent(in) :: values(:)
  integer(int32) :: status
  status = nf90_put_att(file_id, var_id, name, values)
  if ( status /= nf90_noerr ) then
    write(error_unit,'(a)') trim(nf90_strerror(status))
    call radsim_error_report('Error putting netcdf attribute ' // name, status_error)
  else if ( output_mode >= 2 ) then
    print '(a)', 'Written attribute: ' // trim(name) // ' = '
    print '(10(1x,e16.6))', values
  end if
end subroutine radsim_nf90_put_att_real64_1D

