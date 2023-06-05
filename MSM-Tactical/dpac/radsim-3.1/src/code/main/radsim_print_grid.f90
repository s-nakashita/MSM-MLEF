!-------------------------------------------------------------------------------
! Description:
!
!   Print one simulated observation.
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

subroutine radsim_print_grid(grid)

use radsim_mod_types, only : &
  grid_type

implicit none

type(grid_type), intent(in) :: grid

print '(a)', 'Model grid:'
print '(a18,i0)', ' type = ', grid % type
print '(a18,i0)', ' ncols = ', grid % ncols
print '(a18,i0)', ' nrows = ', grid % nrows
print '(a18,i0)', ' xdir = ', grid % xdir
print '(a18,i0)', ' ydir = ', grid % ydir
print '(a18,l1)', ' row_order = ', grid % row_order

print '(a18,f12.6)', ' lon1 = ', grid % lon1
print '(a18,f12.6)', ' lat1 = ', grid % lat1
print '(a18,f12.6)', ' dlon = ', grid % dlon
print '(a18,f12.6)', ' dlat = ', grid % dlat
print '(a18,f12.6)', ' pole_lon = ', grid % pole_lon
print '(a18,f12.6)', ' pole_lat = ', grid % pole_lat
print '(a18,l1)', ' lambda present = ', associated(grid % lambda)
print '(a18,l1)', ' phi present = ', associated(grid % phi)

end subroutine radsim_print_grid
