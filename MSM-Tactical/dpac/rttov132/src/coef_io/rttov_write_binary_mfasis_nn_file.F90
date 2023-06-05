! Description:
!> @file
!!   Write a binary MFASIS NN file.
!
!> @brief
!!   Write a binary MFASIS NN file.
!!
!! @details
!!   The file unit must be open when this subroutine is called.
!!
!! @param[out]    err             status on exit
!! @param[in,out] coef_mfasis_nn  MFASIS NN coefficient structure
!! @param[in]     file_id         logical unit for input MFASIS file
!! @param[in]     verbose         flag to switch verbose output on/off (default TRUE), optional
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
SUBROUTINE rttov_write_binary_mfasis_nn_file( &
              err,            &
              coef_mfasis_nn, &
              file_id,        &
              verbose)
!INTF_OFF
#include "throw.h"
!INTF_ON
  USE rttov_types, ONLY : rttov_coef_mfasis_nn
  USE parkind1, ONLY : jpim, jplm
!INTF_OFF
  USE rttov_const, ONLY : rttov_magic_string, rttov_magic_number
!INTF_ON
  IMPLICIT NONE

  INTEGER(KIND=jpim),         INTENT(OUT)          :: err
  TYPE(rttov_coef_mfasis_nn), INTENT(IN)           :: coef_mfasis_nn
  INTEGER(KIND=jpim),         INTENT(IN)           :: file_id
  LOGICAL(KIND=jplm),         INTENT(IN), OPTIONAL :: verbose
!INTF_END
#include "rttov_errorreport.interface"

  INTEGER(KIND=jpim) :: j, k
  LOGICAL(KIND=jplm) :: lverbose
  CHARACTER(LEN=80)  :: errMessage
!- End of header --------------------------------------------------------
  TRY
  IF (PRESENT(verbose)) THEN
    lverbose = verbose
  ELSE
    lverbose = .TRUE._jplm
  ENDIF

  IF (lverbose) THEN
    WRITE (errMessage, '( "write coefficient to file_id ", i2, " in binary format")') file_id
    INFO(errMessage)
  ENDIF

  WRITE (file_id, iostat=err) rttov_magic_string, rttov_magic_number
  THROW(err.NE.0)

  WRITE (file_id, iostat=err)coef_mfasis_nn%file_type,      &
                             coef_mfasis_nn%version,        &
                             coef_mfasis_nn%creation_date,  &
                             coef_mfasis_nn%nchannels,      &
                             coef_mfasis_nn%nchannels_coef, &
                             coef_mfasis_nn%nparticles
  THROW(err.NE.0)

  WRITE (file_id, iostat=err)coef_mfasis_nn%channel_list(:)
  THROW(err.NE.0)

  WRITE (file_id, iostat=err)coef_mfasis_nn%readme_nn

  DO j = 1, coef_mfasis_nn%nchannels
    WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%nn_version,  &
                               coef_mfasis_nn%nn(j)%n_input,     &
                               coef_mfasis_nn%nn(j)%n_output,    &
                               coef_mfasis_nn%nn(j)%n_hidden,    &
                               coef_mfasis_nn%nn(j)%n_nodes_max, &
                               coef_mfasis_nn%nn(j)%actfunc_hl,  &
                               coef_mfasis_nn%nn(j)%actfunc_ol
    THROW(err.NE.0)

    WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%bias_i, &
                               coef_mfasis_nn%nn(j)%bias_h, &
                               coef_mfasis_nn%nn(j)%bias_o
    THROW(err.NE.0)

    WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%weight_i, &
                               coef_mfasis_nn%nn(j)%weight_h, &
                               coef_mfasis_nn%nn(j)%weight_o
    THROW(err.NE.0)

    DO k = 1, coef_mfasis_nn%nn(j)%n_input
      WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%in(k)%name,      &
                                 coef_mfasis_nn%nn(j)%in(k)%transform, &
                                 coef_mfasis_nn%nn(j)%in(k)%min,       &
                                 coef_mfasis_nn%nn(j)%in(k)%max,       &
                                 coef_mfasis_nn%nn(j)%in(k)%nauxparams
      THROW(err.NE.0)
      WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%in(k)%auxparams
      THROW(err.NE.0)
    ENDDO

    DO k = 1, coef_mfasis_nn%nn(j)%n_output
      WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%out(k)%name,      &
                                 coef_mfasis_nn%nn(j)%out(k)%transform, &
                                 coef_mfasis_nn%nn(j)%out(k)%min,       &
                                 coef_mfasis_nn%nn(j)%out(k)%max,       &
                                 coef_mfasis_nn%nn(j)%out(k)%nauxparams
      THROW(err.NE.0)
      WRITE (file_id, iostat=err)coef_mfasis_nn%nn(j)%out(k)%auxparams
      THROW(err.NE.0)
    ENDDO
  ENDDO

  CATCH
END SUBROUTINE rttov_write_binary_mfasis_nn_file
