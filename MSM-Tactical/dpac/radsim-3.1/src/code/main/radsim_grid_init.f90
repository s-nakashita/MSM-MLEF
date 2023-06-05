!-------------------------------------------------------------------------------
! Description:
!
!   Initialise a grid structure.
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

subroutine radsim_grid_init(grid)

use radsim_mod_types, only : &
  grid_type

implicit none

type(grid_type), intent(inout) :: grid

grid % type = -1
grid % ncols = 0
grid % nrows = 0
grid % row_order = .true.
grid % lon1 = 0.0
grid % lat1 = -90.0
grid % dlon = 0.0
grid % dlat = 0.0
grid % pole_lon = 0.0
grid % pole_lat = 90.0
if ( associated(grid % lambda) ) deallocate(grid % lambda)
if ( associated(grid % phi)    ) deallocate(grid % phi)
if ( associated(grid % lon)    ) deallocate(grid % lon)
if ( associated(grid % lat)    ) deallocate(grid % lat)

end subroutine radsim_grid_init
