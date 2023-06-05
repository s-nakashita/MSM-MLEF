! Description:
!> @file
!!   Nullify/zero an MFASIS NN structure.
!
!> @brief
!!   Nullify/zero an MFASIS NN structure.
!!
!!
!! @param[in,out]  coef_mfasis_nn     the MFASIS NN structure to nullify/zero
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
!    Copyright 2022, EUMETSAT, All Rights Reserved.
!
SUBROUTINE rttov_nullify_coef_mfasis_nn(coef_mfasis_nn)
!INTF_OFF
  USE parkind1, ONLY : jpim
!INTF_ON
  USE rttov_types, ONLY : rttov_coef_mfasis_nn
  IMPLICIT NONE
  TYPE(rttov_coef_mfasis_nn), INTENT(INOUT) :: coef_mfasis_nn
!INTF_END

  coef_mfasis_nn%file_type        = 0_jpim
  coef_mfasis_nn%version          = 0_jpim
  coef_mfasis_nn%creation_date(:) = 0_jpim
  coef_mfasis_nn%readme_nn(:)     = 'xxxx'
  coef_mfasis_nn%nparticles       = 0_jpim
  coef_mfasis_nn%nchannels        = 0_jpim
  coef_mfasis_nn%nchannels_coef   = 0_jpim

  NULLIFY(coef_mfasis_nn%channel_list)
  NULLIFY(coef_mfasis_nn%channel_nn_index)
  NULLIFY(coef_mfasis_nn%nn)

END SUBROUTINE rttov_nullify_coef_mfasis_nn
