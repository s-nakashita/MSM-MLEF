
! Description:
!> @file
!!   Deallocate an MFASIS NN structure.
!!   This should usually be called via rttov_dealloc_coefs.
!
!> @brief
!!   Deallocate an MFASIS NN structure.
!!   This should usually be called via rttov_dealloc_coefs.
!!
!! @param[out]     err             status on exit
!! @param[in,out]  coef_mfasis_nn  the MFASIS NN structure to deallocate
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
SUBROUTINE rttov_dealloc_coef_mfasis_nn(err, coef_mfasis_nn)
!INTF_OFF
#include "throw.h"
!INTF_ON
  USE rttov_types, ONLY : rttov_coef_mfasis_nn
  USE parkind1, ONLY : jpim
  IMPLICIT NONE
  INTEGER(KIND=jpim),         INTENT(OUT)   :: err
  TYPE(rttov_coef_mfasis_nn), INTENT(INOUT) :: coef_mfasis_nn
!INTF_END
#include "rttov_errorreport.interface"
#include "rttov_nullify_coef_mfasis_nn.interface"
  INTEGER(KIND=jpim) :: i, j
!- End of header --------------------------------------------------------
  TRY
  
  IF (ASSOCIATED(coef_mfasis_nn%channel_list)) &
      DEALLOCATE(coef_mfasis_nn%channel_list, STAT = err)
  THROW(err.NE.0)

  IF (ASSOCIATED(coef_mfasis_nn%channel_nn_index)) &
      DEALLOCATE(coef_mfasis_nn%channel_nn_index, STAT = err)
  THROW(err.NE.0)

  IF (ASSOCIATED(coef_mfasis_nn%nn)) THEN
    DO i = 1, SIZE(coef_mfasis_nn%nn)
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%in)) THEN
        DO j = 1, SIZE(coef_mfasis_nn%nn(i)%in)
          IF (ASSOCIATED(coef_mfasis_nn%nn(i)%in(j)%auxparams)) THEN
            DEALLOCATE(coef_mfasis_nn%nn(i)%in(j)%auxparams, STAT = err)
            THROW(err.NE.0)
          ENDIF
        ENDDO
        DEALLOCATE(coef_mfasis_nn%nn(i)%in, STAT = err)
        THROW(err.NE.0)
      ENDIF
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%out)) THEN
        DO j = 1, SIZE(coef_mfasis_nn%nn(i)%out)
          IF (ASSOCIATED(coef_mfasis_nn%nn(i)%out(j)%auxparams)) THEN
            DEALLOCATE(coef_mfasis_nn%nn(i)%out(j)%auxparams, STAT = err)
            THROW(err.NE.0)
          ENDIF
        ENDDO
        DEALLOCATE(coef_mfasis_nn%nn(i)%out, STAT = err)
        THROW(err.NE.0)
      ENDIF
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%bias_i)) &
          DEALLOCATE(coef_mfasis_nn%nn(i)%bias_i, STAT = err)
      THROW(err.NE.0)
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%bias_h)) &
          DEALLOCATE(coef_mfasis_nn%nn(i)%bias_h, STAT = err)
      THROW(err.NE.0)
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%bias_o)) &
          DEALLOCATE(coef_mfasis_nn%nn(i)%bias_o, STAT = err)
      THROW(err.NE.0)
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%weight_i)) &
          DEALLOCATE(coef_mfasis_nn%nn(i)%weight_i, STAT = err)
      THROW(err.NE.0)
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%weight_h)) &
          DEALLOCATE(coef_mfasis_nn%nn(i)%weight_h, STAT = err)
      THROW(err.NE.0)
      IF (ASSOCIATED(coef_mfasis_nn%nn(i)%weight_o)) &
          DEALLOCATE(coef_mfasis_nn%nn(i)%weight_o, STAT = err)
      THROW(err.NE.0)
    ENDDO
    DEALLOCATE(coef_mfasis_nn%nn, STAT = err)
    THROW(err.NE.0)
  ENDIF

  CALL rttov_nullify_coef_mfasis_nn(coef_mfasis_nn)
  
  CATCH
END SUBROUTINE rttov_dealloc_coef_mfasis_nn
