! Description:
!> @file
!!   Returns sea surface emissivities from the internal RTTOV emissivity models.
!
!> @brief
!!   Returns sea surface emissivities from the internal RTTOV emissivity models.
!!
!! @details
!!   This returns sea surface emissivities from the relevant RTTOV sea
!!   surface emissivity model selected in the opts structure for the type of
!!   instrument specified in the coefs structure.
!!
!!   The profiles(:) argument must be populated with the necessary variables
!!   used by the requested emissivity model:
!!      ISEM    - zenith angle
!!      IREMIS  - zenith angle, 10m wind u/v, T skin
!!      FASTEM  - zenith and azimuth angles, 10m wind u/v, T skin, salinity,
!!                  foam_fraction (if supply_foam_fraction option is true),
!!                  fastem parameters (for land/sea-ice surface types)
!!      TESSEM2 - zenith angle, 10m wind u/v, T skin, salinity
!!
!!   This subroutine calls the internal emissivity subroutines within RTTOV.
!!   This means that if the surface type is set to land or sea-ice, then
!!   the relevant IR/MW land/sea-ice emissivity will be returned for those
!!   profiles. For MW this requires that the fastem(:) parameters in the
!!   profiles(:) structure have been assigned suitable values.
!!
!!   Emissivity values are calculated for every channel/profile where calcemis
!!   is true. Values in the emissivity array argument are untouched in where
!!   the corresponding elements of calcemis are false.
!!
!!   Emissivities are only returned for channels at wavelengths above 3 microns.
!!
!! @param[out]    err               status on exit
!! @param[in]     opts              options to configure the simulations
!! @param[in]     chanprof          specifies channels and profiles to simulate
!! @param[in]     profiles          input atmospheric profiles and surface variables
!! @param[in]     coefs             coefficients structure for instrument to simulate
!! @param[in]     calcemis          logical flags indicating which emissivity values to calculate
!! @param[in,out] emissivity        emissivity values
!
! Copyright:
!    This software was developed within the context of
!    the EUMETSAT Satellite Application Facility on
!    Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 7 December 2016, between
!    EUMETSAT and the Met Office, UK, by one or more partners
!    within the NWP SAF. The partners in the NWP SAF are
!    the Met Office, ECMWF, DWD and MeteoFrance.
!
!    Copyright 2019, EUMETSAT, All Rights Reserved.
!
SUBROUTINE rttov_get_sea_emis( &
                err,            &! out
                opts,           &! in
                chanprof,       &! in
                profiles,       &! in
                coefs,          &! in
                calcemis,       &! in
                emissivity)      ! inout
!INTF_OFF
#include "throw.h"
!INTF_ON
  USE parkind1, ONLY : jpim, jprb, jplm
  USE rttov_types, ONLY : &
          rttov_chanprof, &
          rttov_profile,  &
          rttov_coefs,    &
          rttov_options
!INTF_OFF
  USE rttov_const, ONLY : sensor_id_mw, sensor_id_po
  USE rttov_types, ONLY : rttov_geometry, rttov_transmission_aux
  USE yomhook, ONLY : lhook, dr_hook, jphook
!INTF_ON
  IMPLICIT NONE

  INTEGER(jpim),        INTENT(OUT)   :: err
  TYPE(rttov_options),  INTENT(IN)    :: opts
  TYPE(rttov_chanprof), INTENT(IN)    :: chanprof(:)
  TYPE(rttov_profile),  INTENT(IN)    :: profiles(:)
  TYPE(rttov_coefs),    INTENT(IN)    :: coefs
  LOGICAL(jplm),        INTENT(IN)    :: calcemis(SIZE(chanprof))
  REAL(jprb),           INTENT(INOUT) :: emissivity(SIZE(chanprof))
!INTF_END

#include "rttov_errorreport.interface"
#include "rttov_calcemis_ir.interface"
#include "rttov_calcemis_mw.interface"
#include "rttov_setgeometry.interface"

  TYPE(rttov_transmission_aux) :: transmission_aux
  TYPE(rttov_geometry)         :: geometry(SIZE(profiles))
  REAL(jprb)                   :: reflectivity(SIZE(chanprof))
  REAL(jprb)                   :: tskin(SIZE(chanprof))
  LOGICAL(jplm)                :: thermal(SIZE(chanprof))
  INTEGER(jpim)                :: nchanprof, i
  LOGICAL(jplm)                :: sensor_mw
  REAL(jphook) :: zhook_handle
!----------------------------------------------------------------------------
  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_GET_SEA_EMIS', 0_jpim, ZHOOK_HANDLE)

  !-----------------------------
  ! Initialisation
  !-----------------------------

  ! This routine does not work for PC simulations
  IF (ASSOCIATED(coefs%coef_htfrtc%sensor_freq) .OR. opts%htfrtc_opts%htfrtc) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, 'rttov_get_sea_emis does not work for HTFRTC coefficients')
  ENDIF
  IF (opts%rt_ir%pc%addpc) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, 'rttov_get_sea_emis does not work for PC-RTTOV')
  ENDIF

  nchanprof = SIZE(chanprof)
  sensor_mw = coefs%coef%id_sensor == sensor_id_mw .OR. &
              coefs%coef%id_sensor == sensor_id_po

  ! Init common IR/MW data
  CALL rttov_setgeometry(opts%rt_all%plane_parallel, &
                         profiles, coefs%coef, geometry)

  DO i = 1, nchanprof
    thermal(i) = coefs%coef%ss_val_chn(chanprof(i)%chan) < 2 .AND. &
                 (.NOT. coefs%coef%ff_val_chn(chanprof(i)%chan) == 0)
    tskin(i) = profiles(chanprof(i)%prof)%skin%t
  ENDDO

  !-------------------------------------------
  ! Compute emissivities
  !-------------------------------------------

  IF (sensor_mw) THEN
    ! MW sensor

    ! Allocate/init MW-specific data

    ALLOCATE(transmission_aux%thermal_path1, stat=err)
    THROWM(err.NE.0, 'Error allocating data')
    ALLOCATE(transmission_aux%thermal_path1%tau_surf(0:0,nchanprof), stat=err)
    THROWM(err.NE.0, 'Error allocating data')

    ! The total atmospheric transmittance is only used for calculation of
    ! reflectivity which is not returned by rttov_get_sea_emis
    transmission_aux%thermal_path1%tau_surf = 1._jprb

    ! Compute emissivities

    CALL rttov_calcemis_mw( &
          opts,             &
          profiles,         &
          geometry,         &
          coefs%coef,       &
          chanprof,         &
          thermal,          &
          transmission_aux, &
          calcemis,         &
          tskin,            &
          emissivity,       &
          reflectivity,     &
          err)
    THROW(err.NE.0)

    ! Deallocate MW-specific data
    DEALLOCATE(transmission_aux%thermal_path1%tau_surf, stat=err)
    THROWM(err.NE.0, 'Error deallocating data')
    DEALLOCATE(transmission_aux%thermal_path1, stat=err)
    THROWM(err.NE.0, 'Error deallocating data')

  ELSE
    ! IR sensor

    ! Compute emissivities

    CALL rttov_calcemis_ir( &
          err,              &
          opts,             &
          chanprof,         &
          profiles,         &
          geometry,         &
          coefs,            &
          thermal,          &
          calcemis,         &
          tskin,            &
          emissivity)
    THROW(err.NE.0)

  ENDIF

  IF (LHOOK) CALL DR_HOOK('RTTOV_GET_SEA_EMIS',1_jpim,ZHOOK_HANDLE)
  CATCH
  IF (LHOOK) CALL DR_HOOK('RTTOV_GET_SEA_EMIS',1_jpim,ZHOOK_HANDLE)
END SUBROUTINE rttov_get_sea_emis
