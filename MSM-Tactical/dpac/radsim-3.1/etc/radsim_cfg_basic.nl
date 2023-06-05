!-------------------------------------------------------------------------------
! Example namelist file for the Radiance Simulator.
!-------------------------------------------------------------------------------

! These are the minimum that should be set. See the User Guide for further
! options.

&radsim_nl

! Input data

model_datafile   = ''
model_filetype   =     ! integer (see user guide for valid values)
rttov_coeffs_dir = ''

! Satellite and instrument (as in rttov coefficient file names)

platform = ''    ! e.g., 'metop'
inst     = ''    ! e.g., 'iasi'
satid    =       ! integer e.g., satid=1 if metop-1 wanted

! Output

output_dir  = ''
output_file = ''

/
