!-------------------------------------------------------------------------------
! Description:
!
!   Initialise selected obs output arrays.
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

subroutine radsim_init_obs_out(obs)

use radsim_mod_types, only : &
  obs_type

implicit none

type(obs_type), intent(inout) :: obs

!-------------------------------------------------------------------------------

if ( associated(obs % radiance)               ) obs % radiance = 0.0
if ( associated(obs % bt)                     ) obs % bt = 0.0
if ( associated(obs % refl)                   ) obs % refl = 0.0
if ( associated(obs % emiss)                  ) obs % emiss = 0.0
if ( associated(obs % brdf)                   ) obs % brdf = 0.0
if ( associated(obs % trans)                  ) obs % trans = 0.0
if ( associated(obs % cads_height_assignment) ) obs % cads_height_assignment = 0.0
if ( associated(obs % geometric_height)       ) obs % geometric_height = 0.0
if ( associated(obs % tjac)                   ) obs % tjac = 0.0
if ( associated(obs % qjac)                   ) obs % qjac = 0.0
if ( associated(obs % o3jac)                  ) obs % o3jac = 0.0
if ( associated(obs % tskinjac)               ) obs % tskinjac = 0.0
if ( associated(obs % wind10mujac)            ) obs % wind10mujac = 0.0
if ( associated(obs % wind10mvjac)            ) obs % wind10mvjac = 0.0
if ( associated(obs % emissjac)               ) obs % emissjac = 0.0

end subroutine radsim_init_obs_out
