! Description:
!> @file
!!   Write an ASCII MFASIS NN file.
!
!> @brief
!!   Write an ASCII MFASIS NN file.
!!
!! @details
!!   The file unit must be open when this subroutine is called.
!!
!! @param[out]    err             status on exit
!! @param[in]     coef            RTTOV optical depth coefficient structure
!! @param[in,out] coef_mfasis_nn  MFASIS NN coefficient structure
!! @param[in]     file_id         logical unit for output MFASIS file
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
SUBROUTINE rttov_write_ascii_mfasis_nn_file( &
              err,            &
              coef,           &
              coef_mfasis_nn, &
              file_id,        &
              verbose)
!INTF_OFF
#include "throw.h"
!INTF_ON
  USE rttov_types, ONLY : rttov_coef, rttov_coef_mfasis_nn
  USE parkind1, ONLY : jpim, jplm
!INTF_ON
  IMPLICIT NONE

  INTEGER(KIND=jpim),         INTENT(OUT)          :: err
  TYPE(rttov_coef),           INTENT(IN)           :: coef
  TYPE(rttov_coef_mfasis_nn), INTENT(IN)           :: coef_mfasis_nn
  INTEGER(KIND=jpim),         INTENT(IN)           :: file_id
  LOGICAL(KIND=jplm),         INTENT(IN), OPTIONAL :: verbose
!INTF_END
#include "rttov_errorreport.interface"

  INTEGER(KIND=jpim) :: j, k
  LOGICAL(KIND=jplm) :: lverbose
  CHARACTER(LEN=32)  :: section
  CHARACTER(LEN=80)  :: errMessage
  CHARACTER(LEN=*), PARAMETER :: routinename = 'rttov_write_ascii_mfasis_nn_file'
!- End of header --------------------------------------------------------
  TRY
  IF (PRESENT(verbose)) THEN
    lverbose = verbose
  ELSE
    lverbose = .TRUE._jplm
  END IF

  IF (lverbose) THEN
    WRITE (errMessage, '( "write coefficient to file_id ", i2, " in ASCII format")') file_id
    INFO(errMessage)
  END IF

  WRITE (file_id, '(a)', iostat=err) ' ! RTTOV coefficient file '//TRIM(coef%id_common_name)
  THROW(err.NE.0)

  WRITE (file_id, '(a)', iostat=err) ' ! Automatic creation by subroutine '//routinename
  THROW(err.NE.0)

  WRITE (file_id, '(a)', iostat=err) ' ! ------------------------------------------------------'
  THROW(err.NE.0)


  section = 'MFASIS_GENERAL'
  WRITE (file_id, '(a)', iostat=err) ' ! ------------------------------------------------------'
  THROW(err.NE.0)

  WRITE (file_id, '(a)', iostat=err) TRIM(section)
  THROW(err.NE.0)

  WRITE (file_id, '(a)', iostat=err) ' !'
  THROW(err.NE.0)

  WRITE(file_id,'(1x,i8,t20,a)', iostat=err) &
    coef_mfasis_nn%file_type, '! MFASIS file type: 1 => clouds, 2 => aerosols'
  THROW(err.NE.0)

  WRITE(file_id,'(1x,i8,t20,a)', iostat=err) &
    coef_mfasis_nn%version, '! MFASIS file version'
  THROW(err.NE.0)

  WRITE (file_id, '(1x,i4,1x,i2.2,1x,i2.2,t20,a)', iostat=err) &
    coef_mfasis_nn%creation_date, '! Creation date'
  THROW(err.NE.0)

  WRITE(file_id,'(1x,i8,t20,a)', iostat=err) &
    coef_mfasis_nn%nchannels_coef, '! Number of channels in associated rtcoef file'
  THROW(err.NE.0)

  WRITE(file_id,'(1x,i8,t20,a)', iostat=err) &
    coef_mfasis_nn%nchannels, '! Number of channels supported by MFASIS'
  THROW(err.NE.0)

  WRITE(file_id,'(1x,a)', iostat=err) '! Channels for which MFASIS NNs are stored'
  THROW(err.NE.0)
  WRITE(file_id,'(10i6)', iostat=err) coef_mfasis_nn%channel_list(:)
  THROW(err.NE.0)

  WRITE(file_id,'(1x,i8,t20,a)', iostat=err) &
    coef_mfasis_nn%nparticles, '! Number of particle types'
  THROW(err.NE.0)

  IF (coef_mfasis_nn%readme_nn(1) .NE. 'xxxx') THEN
    section = 'README_MFASIS'
    WRITE (file_id, '(a)', iostat=err) ' ! ------------------------------------------------------'
    THROW(err.NE.0)

    WRITE (file_id, '(a)', iostat=err) TRIM(section)
    THROW(err.NE.0)

    WRITE (file_id, '(a)', iostat=err) ' !'
    THROW(err.NE.0)

    DO j = 1, SIZE(coef_mfasis_nn%readme_nn)
      IF (coef_mfasis_nn%readme_nn(j) .EQ. 'xxxx') EXIT
      WRITE (file_id, '(a)', iostat=err)TRIM(coef_mfasis_nn%readme_nn(j))
      THROW(err.NE.0)
    ENDDO
  ENDIF

  section = 'MFASIS_NNS'
  WRITE (file_id, '(a)', iostat=err) ' ! ------------------------------------------------------'
  THROW(err.NE.0)

  WRITE (file_id, '(a)', iostat=err) TRIM(section)
  THROW(err.NE.0)

  WRITE (file_id, '(a)', iostat=err) ' !'
  THROW(err.NE.0)

  DO j = 1, coef_mfasis_nn%nchannels
    WRITE (file_id, '(a,i6)', iostat=err) '! NN data for channel ', coef_mfasis_nn%channel_list(j)
    THROW(err.NE.0)

    WRITE(file_id,'(1x,i8,t40,a)', iostat=err) &
      coef_mfasis_nn%nn(j)%nn_version, '! NN version'
    THROW(err.NE.0)

    WRITE(file_id,'(1x,3i8,t40,a)', iostat=err) &
      coef_mfasis_nn%nn(j)%n_input,     &
      coef_mfasis_nn%nn(j)%n_output,    &
      coef_mfasis_nn%nn(j)%n_nodes_max, &
      '! Number of nodes: input, output, max'
    THROW(err.NE.0)

    WRITE(file_id,'(1x,i8,t40,a)', iostat=err) &
      coef_mfasis_nn%nn(j)%n_hidden, &
      '! Number of hidden layers'
    THROW(err.NE.0)

    WRITE(file_id,'(1x,2i8,t40,a)', iostat=err) &
      coef_mfasis_nn%nn(j)%actfunc_hl, coef_mfasis_nn%nn(j)%actfunc_ol, &
      '! Activation functions: hidden layer, output layer'
    THROW(err.NE.0)

    WRITE (file_id, '(a)', iostat=err) '! Biases: input nodes'
    THROW(err.NE.0)
    WRITE(file_id,'(10e16.8)', iostat=err) coef_mfasis_nn%nn(j)%bias_i(:)
    THROW(err.NE.0)
    WRITE (file_id, '(a)', iostat=err) '! Biases: hidden nodes'
    THROW(err.NE.0)
    WRITE(file_id,'(10e16.8)', iostat=err) coef_mfasis_nn%nn(j)%bias_h(:,:)
    THROW(err.NE.0)
    WRITE (file_id, '(a)', iostat=err) '! Biases: output nodes'
    THROW(err.NE.0)
    WRITE(file_id,'(10e16.8)', iostat=err) coef_mfasis_nn%nn(j)%bias_o(:)
    THROW(err.NE.0)

    WRITE (file_id, '(a)', iostat=err) '! Weights: input nodes'
    THROW(err.NE.0)
    WRITE(file_id,'(10e16.8)', iostat=err) coef_mfasis_nn%nn(j)%weight_i(:,:)
    THROW(err.NE.0)
    WRITE (file_id, '(a)', iostat=err) '! Weights: hidden nodes'
    THROW(err.NE.0)
    WRITE(file_id,'(10e16.8)', iostat=err) coef_mfasis_nn%nn(j)%weight_h(:,:,:)
    THROW(err.NE.0)
    WRITE (file_id, '(a)', iostat=err) '! Weights: output nodes'
    THROW(err.NE.0)
    WRITE(file_id,'(10e16.8)', iostat=err) coef_mfasis_nn%nn(j)%weight_o(:,:)
    THROW(err.NE.0)

    WRITE (file_id, '(a)', iostat=err) '! Input nodes data'
    THROW(err.NE.0)
    DO k = 1, coef_mfasis_nn%nn(j)%n_input
      WRITE(file_id,'(1x,a,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%in(k)%name, '! Name'
      THROW(err.NE.0)
      WRITE(file_id,'(1x,a,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%in(k)%transform, '! Transform'
      THROW(err.NE.0)
      WRITE(file_id,'(1x,2e16.8,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%in(k)%min, &
        coef_mfasis_nn%nn(j)%in(k)%max, &
        '! Min, max'
      THROW(err.NE.0)
      WRITE(file_id,'(1x,i8,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%in(k)%nauxparams, &
        '! Number of auxiliary parameters'
      THROW(err.NE.0)

      IF (coef_mfasis_nn%nn(j)%in(k)%nauxparams > 0) THEN
        WRITE(file_id,'(10e16.8)', iostat=err) &
          coef_mfasis_nn%nn(j)%in(k)%auxparams(:)
        THROW(err.NE.0)
      ENDIF
    ENDDO

    WRITE (file_id, '(a)', iostat=err) '! Output nodes data'
    THROW(err.NE.0)
    DO k = 1, coef_mfasis_nn%nn(j)%n_output
      WRITE(file_id,'(1x,a,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%out(k)%name, '! Name'
      THROW(err.NE.0)
      WRITE(file_id,'(1x,a,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%out(k)%transform, '! Transform'
      THROW(err.NE.0)
      WRITE(file_id,'(1x,2e16.8,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%out(k)%min, &
        coef_mfasis_nn%nn(j)%out(k)%max, &
        '! Min, max'
      THROW(err.NE.0)
      WRITE(file_id,'(1x,i8,t40,a)', iostat=err) &
        coef_mfasis_nn%nn(j)%out(k)%nauxparams, &
        '! Number of auxiliary parameters'
      THROW(err.NE.0)

      IF (coef_mfasis_nn%nn(j)%out(k)%nauxparams > 0) THEN
        WRITE(file_id,'(10e16.8)', iostat=err) &
          coef_mfasis_nn%nn(j)%out(k)%auxparams(:)
        THROW(err.NE.0)
      ENDIF
    ENDDO

  ENDDO

  IF (lverbose) INFO("end of write coefficient")

  CATCH
END SUBROUTINE rttov_write_ascii_mfasis_nn_file
