!-------------------------------------------------------------------------------
! Description:
!
!   Deallocate components of a model_type structure.
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

subroutine radsim_dealloc_model(model)

use radsim_mod_types, only : &
  model_type

implicit none

type(model_type), intent(inout) :: model

model % nprofs = 0
model % nlevels = 0
if ( associated(model % validity_time)   ) deallocate(model % validity_time)
if ( associated(model % data_time)       ) deallocate(model % data_time)
if ( associated(model % lsm)             ) deallocate(model % lsm)
if ( associated(model % zsurf)           ) deallocate(model % zsurf)
if ( associated(model % seaice)          ) deallocate(model % seaice)
if ( associated(model % pstar)           ) deallocate(model % pstar)
if ( associated(model % t2)              ) deallocate(model % t2)
if ( associated(model % td2)             ) deallocate(model % td2)
if ( associated(model % q2)              ) deallocate(model % q2)
if ( associated(model % rh2)             ) deallocate(model % rh2)
if ( associated(model % tskin)           ) deallocate(model % tskin)
if ( associated(model % u10)             ) deallocate(model % u10)
if ( associated(model % v10)             ) deallocate(model % v10)
if ( associated(model % total_cc)        ) deallocate(model % total_cc)
if ( associated(model % low_cc)          ) deallocate(model % low_cc)
if ( associated(model % medium_cc)       ) deallocate(model % medium_cc)
if ( associated(model % high_cc)         ) deallocate(model % high_cc)
if ( associated(model % snow_depth)      ) deallocate(model % snow_depth)
if ( associated(model % z)               ) deallocate(model % z)
if ( associated(model % p)               ) deallocate(model % p)
if ( associated(model % ph)              ) deallocate(model % ph)
if ( associated(model % t)               ) deallocate(model % t)
if ( associated(model % theta)           ) deallocate(model % theta)
if ( associated(model % q)               ) deallocate(model % q)
if ( associated(model % rh)              ) deallocate(model % rh)
if ( associated(model % density)         ) deallocate(model % density)
if ( associated(model % clw)             ) deallocate(model % clw)
if ( associated(model % clw_deff)        ) deallocate(model % clw_deff)
if ( associated(model % ciw)             ) deallocate(model % ciw)
if ( associated(model % ciw_deff)        ) deallocate(model % ciw_deff)
if ( associated(model % rain)            ) deallocate(model % rain)
if ( associated(model % snow)            ) deallocate(model % snow)
if ( associated(model % cfrac)           ) deallocate(model % cfrac)
if ( associated(model % cfrac_liq)       ) deallocate(model % cfrac_liq)
if ( associated(model % cfrac_ice)       ) deallocate(model % cfrac_ice)
if ( associated(model % cfrac_conv)      ) deallocate(model % cfrac_conv)
if ( associated(model % conv_cloud)      ) deallocate(model % conv_cloud)
if ( associated(model % conv_inc)        ) deallocate(model % conv_inc)
if ( associated(model % o3)              ) deallocate(model % o3)
if ( associated(model % co2)             ) deallocate(model % co2)
if ( associated(model % n2o)             ) deallocate(model % n2o)
if ( associated(model % co)              ) deallocate(model % co)
if ( associated(model % ch4)             ) deallocate(model % ch4)
if ( associated(model % so2)             ) deallocate(model % so2)

if ( associated(model % cams_sea_salt1)  ) deallocate(model % cams_sea_salt1)
if ( associated(model % cams_sea_salt2)  ) deallocate(model % cams_sea_salt2)
if ( associated(model % cams_sea_salt3)  ) deallocate(model % cams_sea_salt3)
if ( associated(model % cams_dust1)      ) deallocate(model % cams_dust1)
if ( associated(model % cams_dust2)      ) deallocate(model % cams_dust2)
if ( associated(model % cams_dust3)      ) deallocate(model % cams_dust3)
if ( associated(model % cams_hphil_omat) ) deallocate(model % cams_hphil_omat)
if ( associated(model % cams_hphob_bcar) ) deallocate(model % cams_hphob_bcar)
if ( associated(model % cams_sulphate)   ) deallocate(model % cams_sulphate)

if ( associated(model % grid % lat)      ) deallocate(model % grid % lat)
if ( associated(model % grid % lon)      ) deallocate(model % grid % lon)
if ( associated(model % grid % lambda)   ) deallocate(model % grid % lambda)
if ( associated(model % grid % phi)      ) deallocate(model % grid % phi)

end subroutine radsim_dealloc_model
