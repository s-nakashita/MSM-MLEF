!-------------------------------------------------------------------------------
! Namelist file for checking Radiance Simulator installation.
!-------------------------------------------------------------------------------

&radsim_nl

! Input data

model_datafile     = 'etc/nwp_saf_t_test.atm'
model_filetype     = 3
rttov_coeffs_dir   = 'etc'
platform           = 'metop'
inst               = 'amsua'
satid              = 2
clw_data           = .true.

/
