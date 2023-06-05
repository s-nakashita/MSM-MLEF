!-------------------------------------------------------------------------------
! Description:
!
!   Deallocate components of a fieldsfile header structure.
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

subroutine radsim_dealloc_ff_hd(ff_hd)

use radsim_mod_types, only : &
  ff_hd_type

implicit none

type(ff_hd_type), intent(inout) :: ff_hd

if ( associated(ff_hd % int_c)       ) deallocate(ff_hd % int_c)
if ( associated(ff_hd % real_c)      ) deallocate(ff_hd % real_c)
if ( associated(ff_hd % level_dep_c) ) deallocate(ff_hd % level_dep_c)
if ( associated(ff_hd % row_dep_c)   ) deallocate(ff_hd % row_dep_c)
if ( associated(ff_hd % col_dep_c)   ) deallocate(ff_hd % col_dep_c)
if ( associated(ff_hd % level_dep_c) ) deallocate(ff_hd % level_dep_c)
if ( associated(ff_hd % lookup_i)    ) deallocate(ff_hd % lookup_i)
if ( associated(ff_hd % lookup_r)    ) deallocate(ff_hd % lookup_r)

end subroutine radsim_dealloc_ff_hd
