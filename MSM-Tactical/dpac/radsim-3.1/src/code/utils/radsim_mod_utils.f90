module radsim_mod_utils

!-------------------------------------------------------------------------------
! 1. Physical constsnts
!-------------------------------------------------------------------------------

real, parameter :: g = 9.80665 ! Earth-surface gravitational acceleration (m/s2)
real, parameter :: M = 0.0289644 ! molar mass of dry air (kg/mol)
real, parameter :: R = 8.31447 ! universal gas constant (J/(mol.K)
real, parameter :: k = g*M/R



contains

include 'pzconv.f90'

end module radsim_mod_utils
