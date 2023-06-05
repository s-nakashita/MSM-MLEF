!-------------------------------------------------------------------------------
! Description:
!
!   Deallocate components of an obs_type structure.
!
! Copyright:
!
!    This software was developed within the context of the EUMETSAT Satellite
!    Application Facility on Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!    Met Office, UK, by one or more partners within the NWP SAF. The partners
!    in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!    Copyright 2018, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_dealloc_obs(obs)

use radsim_mod_types, only : &
  obs_type

implicit none

type(obs_type), intent(inout) :: obs

obs % nobs = 0

if ( associated(obs % lat)                    ) deallocate(obs % lat)
if ( associated(obs % lon)                    ) deallocate(obs % lon)
if ( associated(obs % zsurf)                  ) deallocate(obs % zsurf)
if ( associated(obs % satzen)                 ) deallocate(obs % satzen)
if ( associated(obs % satazim)                ) deallocate(obs % satazim)
if ( associated(obs % solzen)                 ) deallocate(obs % solzen)
if ( associated(obs % solazim)                ) deallocate(obs % solazim)
if ( associated(obs % footprint_rmajor)       ) deallocate(obs % footprint_rmajor)
if ( associated(obs % footprint_rminor)       ) deallocate(obs % footprint_rminor)
if ( associated(obs % year)                   ) deallocate(obs % year)
if ( associated(obs % month)                  ) deallocate(obs % month)
if ( associated(obs % day)                    ) deallocate(obs % day)
if ( associated(obs % hour)                   ) deallocate(obs % hour)
if ( associated(obs % minute)                 ) deallocate(obs % minute)
if ( associated(obs % ref_time)               ) deallocate(obs % ref_time)
if ( associated(obs % lsm)                    ) deallocate(obs % lsm)
if ( associated(obs % rtsurf)                 ) deallocate(obs % rtsurf)
if ( associated(obs % qcflags)                ) deallocate(obs % qcflags)
if ( associated(obs % qcrttov)                ) deallocate(obs % qcrttov)
if ( associated(obs % qcinfo)                 ) deallocate(obs % qcinfo)
if ( associated(obs % bt)                     ) deallocate(obs % bt)
if ( associated(obs % refl)                   ) deallocate(obs % refl)
if ( associated(obs % radiance)               ) deallocate(obs % radiance)
if ( associated(obs % emiss)                  ) deallocate(obs % emiss)
if ( associated(obs % brdf)                   ) deallocate(obs % brdf)
if ( associated(obs % cads_height_assignment) ) deallocate(obs % cads_height_assignment)
if ( associated(obs % geometric_height)       ) deallocate(obs % geometric_height)
if ( associated(obs % tjac)                   ) deallocate(obs % tjac)
if ( associated(obs % qjac)                   ) deallocate(obs % qjac)
if ( associated(obs % o3jac)                  ) deallocate(obs % o3jac)
if ( associated(obs % tskinjac)               ) deallocate(obs % tskinjac)
if ( associated(obs % wind10mujac)            ) deallocate(obs % wind10mujac)
if ( associated(obs % wind10mvjac)            ) deallocate(obs % wind10mvjac)
if ( associated(obs % emissjac)               ) deallocate(obs % emissjac)
if ( associated(obs % trans)                  ) deallocate(obs % trans)
if ( associated(obs % channels)               ) deallocate(obs % channels)
if ( associated(obs % wavenumbers)            ) deallocate(obs % wavenumbers)
if ( associated(obs % nsim_per_obs)           ) deallocate(obs % nsim_per_obs)
if ( associated(obs % sim_to_obs)             ) deallocate(obs % sim_to_obs)

end subroutine radsim_dealloc_obs
