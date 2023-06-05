! Description:
!> @file
!!   Check consistency of selected options and coefficient file.
!
!> @brief
!!   Check consistency of selected options and coefficient file,
!!   useful for "debugging" simulations.
!!
!!   This subroutine reports illegal option settings, and also dubious
!!   options settings which will not result in bad simulations, but may
!!   indicate that the simulation has not been configured as intended.
!!
!!   For the strictly illegal checks this routine calls rttov_check_options.
!!   All other checks (for legal-but-dubious configurations) are done here.
!!
!! @param[out]    err                  status on exit
!! @param[in]     opts                 options to configure the simulations
!! @param[in]     coefs                coefficients structure for instrument to simulate
!! @param[in]     strictly_illegal     if true report only strictly illegal settings, optional
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
!    Copyright 2015, EUMETSAT, All Rights Reserved.
!
SUBROUTINE rttov_user_options_checkinput(err, opts, coefs, strictly_illegal)

!INTF_OFF
#include "throw.h"
!INTF_ON
  USE rttov_types, ONLY : rttov_coefs, rttov_options
  USE parkind1, ONLY : jpim, jplm
!INTF_OFF
  USE yomhook, ONLY : lhook, dr_hook, jphook
  USE rttov_const, ONLY : &
      sensor_id_mw,       &
      sensor_id_po,       &
      vis_scatt_mfasis,   &
      vis_scatt_mfasis_nn
!INTF_ON

  IMPLICIT NONE

  INTEGER(jpim),       INTENT(OUT)          :: err
  TYPE(rttov_options), INTENT(IN)           :: opts
  TYPE(rttov_coefs),   INTENT(IN)           :: coefs
  LOGICAL(jplm),       INTENT(IN), OPTIONAL :: strictly_illegal
!INTF_END

#include "rttov_errorreport.interface"
#include "rttov_check_options.interface"

  CHARACTER(LEN=256) :: msg
  REAL(jphook) :: zhook_handle

!- End of header --------------------------------------------------------

  TRY

  IF (LHOOK) CALL DR_HOOK('RTTOV_USER_OPTIONS_CHECKINPUT',0_jpim,ZHOOK_HANDLE)

  ! Check for strictly illegal options first

  CALL rttov_check_options(err, opts, coefs)
  THROW(err.NE.0)

  ! Return if user requests only strictly illegal checks

  IF (PRESENT(strictly_illegal)) THEN
    IF (strictly_illegal) RETURN
  ENDIF

  ! Checks for dubious option settings

  ! Optional gases
  IF (opts%rt_all%ozone_data .AND. coefs%coef%nozone == 0_jpim) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not allow variable O3'
    THROWM(err.NE.0, msg)
  ENDIF

  IF (opts%rt_all%co2_data .AND. coefs%coef%nco2 == 0_jpim) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not allow variable CO2'
    THROWM(err.NE.0, msg)
  ENDIF

  IF (opts%rt_all%co_data .AND. coefs%coef%nco == 0_jpim) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not allow variable CO'
    THROWM(err.NE.0, msg)
  ENDIF

  IF (opts%rt_all%n2o_data .AND. coefs%coef%nn2o == 0_jpim) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not allow variable N2O'
    THROWM(err.NE.0, msg)
  ENDIF

  IF (opts%rt_all%ch4_data .AND. coefs%coef%nch4 == 0_jpim) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not allow variable CH4'
    THROWM(err.NE.0, msg)
  ENDIF

  IF (opts%rt_all%so2_data .AND. coefs%coef%nso2 == 0_jpim) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not allow variable SO2'
    THROWM(err.NE.0, msg)
  ENDIF


  ! Solar radiation
  IF (opts%rt_ir%addsolar .AND. coefs%coef%fmv_model_ver < 9) THEN
    err = errorstatus_fatal
    msg = 'Coefficient file does not support solar calculations'
    THROWM(err.NE.0, msg)
  ENDIF


  ! lgradp / interpolation
  IF (opts%interpolation%lgradp .AND. .NOT. opts%interpolation%addinterp) THEN
    err = errorstatus_fatal
    msg = 'Interpolation should be enabled if lgradp is set to TRUE'
    THROWM(err.NE.0, msg)
  ENDIF


  ! MW sensors
  IF (coefs%coef%id_sensor == sensor_id_mw .OR. coefs%coef%id_sensor == sensor_id_po) THEN
    IF (opts%rt_ir%addsolar) THEN
      err = errorstatus_fatal
      msg = 'Solar calculations not applicable to MW instruments'
      THROWM(err.NE.0, msg)
    ENDIF
  ENDIF ! MW sensors


  ! Check scattering solver
  IF (opts%rt_ir%addclouds .OR. opts%rt_ir%addaerosl) THEN

    ! MFASIS requires addsolar to be true
    IF (.NOT. opts%rt_ir%addsolar .AND. &
        (opts%rt_ir%vis_scatt_model == vis_scatt_mfasis .OR. &
         opts%rt_ir%vis_scatt_model == vis_scatt_mfasis_nn)) THEN
      err = errorstatus_fatal
      msg = 'MFASIS requires addsolar to be true'
      THROWM(err.NE.0, msg)
    ENDIF

  ENDIF ! clouds or aerosol


  ! NLTE
  IF (opts%rt_ir%do_nlte_correction) THEN
    IF (.NOT. coefs%coef%nltecoef) THEN
      err = errorstatus_fatal
      msg = 'Coefficient file does not support NLTE correction or channel selection does not include any NLTE-affected channels'
      THROWM(err.NE.0, msg)
    ENDIF
  ENDIF

  IF (LHOOK) CALL DR_HOOK('RTTOV_USER_OPTIONS_CHECKINPUT',1_jpim,ZHOOK_HANDLE)

  CATCH

  IF (LHOOK) CALL DR_HOOK('RTTOV_USER_OPTIONS_CHECKINPUT',1_jpim,ZHOOK_HANDLE)
END SUBROUTINE rttov_user_options_checkinput
