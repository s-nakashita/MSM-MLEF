! Description:
!> @file
!!   Read an ASCII MFASIS NN file, optionally extracting a subset of channels.
!
!> @brief
!!   Read an ASCII MFASIS NN file, optionally extracting a subset of channels.
!!
!! @details
!!   The file unit must be open when this subroutine is called.
!!
!!   Note that after reading a subset of channels RTTOV will identify them by
!!   indexes 1...SIZE(channels), not by the original channel numbers.
!!
!! @param[out]    err             status on exit
!! @param[in]     coef            RTTOV optical depth coefficient structure
!! @param[in,out] coef_mfasis_nn  MFASIS NN coefficient structure
!! @param[in]     file_id         logical unit for input MFASIS file
!! @param[in]     channels        list of channels to read, optional
!! @param[in]     file_type       if present a check is done to ensure MFASIS file is of
!!                                  the specified type (1=>cloud, 2=>aerosol), optional
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
SUBROUTINE rttov_read_ascii_mfasis_nn_file( &
              err,            &
              coef,           &
              coef_mfasis_nn, &
              file_id,        &
              channels,       &
              file_type)
!INTF_OFF
#include "throw.h"
!INTF_ON
  USE rttov_types, ONLY : rttov_coef, rttov_coef_mfasis_nn
  USE parkind1, ONLY : jpim
!INTF_OFF
  USE rttov_const, ONLY : lensection,                       &
                          mfasis_nn_version_compatible_min, &
                          mfasis_nn_version_compatible_max
  USE parkind1, ONLY : jplm, jprb
!INTF_ON
  IMPLICIT NONE

  INTEGER(KIND=jpim),         INTENT(OUT)          :: err
  TYPE(rttov_coef),           INTENT(IN)           :: coef
  TYPE(rttov_coef_mfasis_nn), INTENT(INOUT)        :: coef_mfasis_nn
  INTEGER(KIND=jpim),         INTENT(IN)           :: file_id
  INTEGER(KIND=jpim),         INTENT(IN), OPTIONAL :: channels(:)
  INTEGER(KIND=jpim),         INTENT(IN), OPTIONAL :: file_type
!INTF_END
#include "rttov_errorreport.interface"
#include "rttov_skipcommentline.interface"
#include "rttov_findnextsection.interface"
#include "rttov_nullify_coef_mfasis_nn.interface"
#include "rttov_channel_extract_sublist.interface"

  INTEGER(KIND=jpim) :: nchannels_mfasis
  LOGICAL(KIND=jplm) :: all_channels
  INTEGER(KIND=jpim) :: io_status
  INTEGER(KIND=jpim) :: i, j, k
  INTEGER(KIND=jpim) :: n_input, n_output, n_nodes_max, n_hidden, nauxparams
  REAL(KIND=jprb),    ALLOCATABLE :: bias_i(:), bias_h(:,:), bias_o(:)
  REAL(KIND=jprb),    ALLOCATABLE :: weight_i(:,:), weight_h(:,:,:), weight_o(:,:)

  INTEGER(KIND=jpim), ALLOCATABLE :: channels_rtcoef(:)  ! List of rtcoef channels
  INTEGER(KIND=jpim), ALLOCATABLE :: channels_mfasis(:)  ! List of MFASIS channels
  INTEGER(KIND=jpim), ALLOCATABLE :: mfasis_ext_index(:) ! Indexes of NNs to extract

  CHARACTER(LEN=lensection)   :: section
!- End of header --------------------------------------------------------
  TRY

  all_channels = .NOT. PRESENT(channels)

  CALL rttov_nullify_coef_mfasis_nn(coef_mfasis_nn)

  readfile : DO
    CALL rttov_findnextsection(file_id, io_status, section)
    IF (io_status < 0) EXIT!end-of-file

    SELECT CASE (TRIM(section))
    CASE ('MFASIS_GENERAL')
      CALL rttov_skipcommentline(file_id, err)
      THROWM(err.NE.0, 'io status while reading section '//section)

      READ (file_id,  * , iostat=err)coef_mfasis_nn%file_type
      THROWM(err.NE.0, 'io status while reading section '//section)

      IF (PRESENT(file_type)) THEN
        IF (coef_mfasis_nn%file_type /= file_type) THEN
          err = errorstatus_fatal
          THROWM(err.NE.0, 'Error: MFASIS file type mismatch (cloud/aerosol)')
        ENDIF
      ENDIF

      READ (file_id,  * , iostat=err)coef_mfasis_nn%version
      THROWM(err.NE.0, 'io status while reading section '//section)

      IF (coef_mfasis_nn%version < mfasis_nn_version_compatible_min .OR. &
          coef_mfasis_nn%version > mfasis_nn_version_compatible_max) THEN
        err = errorstatus_fatal
        THROWM(err.NE.0, "Version of MFASIS NN file is incompatible with RTTOV library")
      ENDIF

      READ (file_id,  * , iostat=err)coef_mfasis_nn%creation_date
      THROWM(err.NE.0, 'io status while reading section '//section)

      READ (file_id,  * , iostat=err)coef_mfasis_nn%nchannels_coef
      THROWM(err.NE.0, 'io status while reading section '//section)

      IF (coef%fmv_ori_nchn /= coef_mfasis_nn%nchannels_coef) THEN
        err = errorstatus_fatal
        THROWM(err.NE.0, "Incompatible channels between rtcoef and MFASIS files")
      ENDIF

      IF (.NOT. all_channels) THEN
        ALLOCATE(channels_rtcoef(SIZE(channels)))
        channels_rtcoef = channels
      ELSE
        ALLOCATE(channels_rtcoef(coef%fmv_chn))
        channels_rtcoef = (/(i, i = 1, coef%fmv_chn)/)
      ENDIF

      ! Take care of the user list of channels
      ! coef_mfasis_nn%nchannels_coef is the total number of channels that the user requests

      IF (.NOT. all_channels) coef_mfasis_nn%nchannels_coef = SIZE(channels)

      READ (file_id,  * , iostat=err)nchannels_mfasis
      THROWM(err.NE.0, 'io status while reading section '//section)

      CALL rttov_skipcommentline(file_id, err)
      THROWM(err.NE.0, 'io status while reading section '//section)
      ALLOCATE(channels_mfasis(nchannels_mfasis))
      READ (file_id,  * , iostat=err)channels_mfasis(:)
      THROWM(err.NE.0, 'io status while reading section '//section)

      ! Determine MFASIS channels to be extracted
      ALLOCATE(mfasis_ext_index(nchannels_mfasis))
      CALL rttov_channel_extract_sublist( &
              err,                             &
              channels_mfasis,                 &
              channels_rtcoef,                 &
              coef_mfasis_nn%nchannels,        &
              coef_mfasis_nn%channel_list,     &
              coef_mfasis_nn%channel_nn_index, &
              mfasis_ext_index)
      THROW(err.NE.0)

      IF (coef_mfasis_nn%nchannels == 0) THEN
        DEALLOCATE(channels_rtcoef, channels_mfasis, mfasis_ext_index)
        err = errorstatus_fatal
        THROWM(err.NE.0, 'No MFASIS channels to extract')
      ENDIF

      ! At this point:
      !   nchannels_mfasis                   = total number of MFASIS channels in file
      !   channels_mfasis(:)                 = list of MFASIS channel numbers
      !   coef_mfasis_nn%nchannels           = number of MFASIS chans being extracted
      !   coef_mfasis_nn%channel_list(:)     = list of MFASIS channels to extract from file (indexes into extracted
      !                                        chan list, NOT original channel numbers)
      !   mfasis_ext_index(:)                = list of indexes into the NNs which are to be extracted
      !   coef_mfasis_nn%channel_nn_index(:) = indexes for each extracted channel into the NN array

      READ (file_id,  * , iostat=err)coef_mfasis_nn%nparticles
      THROWM(err.NE.0, 'io status while reading section '//section)

    CASE ('README_MFASIS')
      CALL rttov_skipcommentline(file_id, err)
      THROWM(err.NE.0, 'io status while reading section '//section)

      LoopRNN : DO i = 1, SIZE(coef_mfasis_nn%readme_nn)
        READ (file_id, '(a)', iostat=err)coef_mfasis_nn%readme_nn(i)
        DO j = 1, LEN(coef_mfasis_nn%readme_nn(i))

          SELECT CASE (coef_mfasis_nn%readme_nn(i)(j:j))
          CASE ('!')
            EXIT LoopRNN
          CASE (' ')
            CYCLE
          CASE DEFAULT
            EXIT
          END SELECT

        ENDDO
      ENDDO LoopRNN
      IF (i .LE. SIZE(coef_mfasis_nn%readme_nn)) coef_mfasis_nn%readme_nn(i) = 'xxxx'

    CASE ('MFASIS_NNS')
      CALL rttov_skipcommentline(file_id, err)
      THROWM(err.NE.0, 'io status while reading section '//section)

      ALLOCATE(coef_mfasis_nn%nn(coef_mfasis_nn%nchannels), STAT = err)
      THROWM(err.NE.0, "allocation of coef_mfasis_nn%nn array")

      i = 1
      DO j = 1, nchannels_mfasis
        CALL rttov_skipcommentline(file_id, err)
        THROWM(err.NE.0, 'io status while reading section '//section)

        IF (j == mfasis_ext_index(i)) THEN
          READ (file_id,  * , iostat=err) &
            coef_mfasis_nn%nn(i)%nn_version
          THROWM(err.NE.0, 'io status while reading section '//section)

          READ (file_id,  * , iostat=err) &
            coef_mfasis_nn%nn(i)%n_input,  &
            coef_mfasis_nn%nn(i)%n_output, &
            coef_mfasis_nn%nn(i)%n_nodes_max
          THROWM(err.NE.0, 'io status while reading section '//section)

          READ (file_id,  * , iostat=err) &
            coef_mfasis_nn%nn(i)%n_hidden
          THROWM(err.NE.0, 'io status while reading section '//section)

          READ (file_id,  * , iostat=err) &
            coef_mfasis_nn%nn(i)%actfunc_hl, &
            coef_mfasis_nn%nn(i)%actfunc_ol
          THROWM(err.NE.0, 'io status while reading section '//section)

          ALLOCATE(coef_mfasis_nn%nn(i)%bias_i(coef_mfasis_nn%nn(i)%n_nodes_max),  &
                   coef_mfasis_nn%nn(i)%bias_h(coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                               coef_mfasis_nn%nn(i)%n_hidden - 1), &
                   coef_mfasis_nn%nn(i)%bias_o(coef_mfasis_nn%nn(i)%n_output), stat=err)
          THROWM(err.NE.0, 'allocation of NN biases')

          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) coef_mfasis_nn%nn(i)%bias_i(:)
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) coef_mfasis_nn%nn(i)%bias_h(:,:)
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) coef_mfasis_nn%nn(i)%bias_o(:)
          THROWM(err.NE.0, 'io status while reading section '//section)

          ALLOCATE(coef_mfasis_nn%nn(i)%weight_i(coef_mfasis_nn%nn(i)%n_input,       &
                                                 coef_mfasis_nn%nn(i)%n_nodes_max),  &
                   coef_mfasis_nn%nn(i)%weight_h(coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                                 coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                                 coef_mfasis_nn%nn(i)%n_hidden - 1), &
                   coef_mfasis_nn%nn(i)%weight_o(coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                                 coef_mfasis_nn%nn(i)%n_output), stat=err)
          THROWM(err.NE.0, 'allocation of NN weights')

          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) coef_mfasis_nn%nn(i)%weight_i(:,:)
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) coef_mfasis_nn%nn(i)%weight_h(:,:,:)
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) coef_mfasis_nn%nn(i)%weight_o(:,:)
          THROWM(err.NE.0, 'io status while reading section '//section)

          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          ALLOCATE(coef_mfasis_nn%nn(i)%in(coef_mfasis_nn%nn(i)%n_input), STAT=err)
          THROWM(err.NE.0, 'allocating NN inputs data')
          DO k = 1, coef_mfasis_nn%nn(i)%n_input
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%in(k)%name
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%in(k)%transform
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%in(k)%min, &
              coef_mfasis_nn%nn(i)%in(k)%max
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%in(k)%nauxparams
            THROWM(err.NE.0, 'io status while reading section '//section)

            ALLOCATE(coef_mfasis_nn%nn(i)%in(k)%auxparams(coef_mfasis_nn%nn(i)%in(k)%nauxparams), STAT=err)
            THROWM(err.NE.0, 'allocating NN inputs data')
            IF (coef_mfasis_nn%nn(i)%in(k)%nauxparams > 0) THEN
              READ (file_id,  * , iostat=err) &
                coef_mfasis_nn%nn(i)%in(k)%auxparams(:)
              THROWM(err.NE.0, 'io status while reading section '//section)
            ENDIF
          ENDDO

          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          ALLOCATE(coef_mfasis_nn%nn(i)%out(coef_mfasis_nn%nn(i)%n_output), STAT=err)
          THROWM(err.NE.0, 'allocating NN outputs data')
          DO k = 1, coef_mfasis_nn%nn(i)%n_output
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%out(k)%name
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%out(k)%transform
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%out(k)%min, &
              coef_mfasis_nn%nn(i)%out(k)%max
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) &
              coef_mfasis_nn%nn(i)%out(k)%nauxparams
            THROWM(err.NE.0, 'io status while reading section '//section)

            ALLOCATE(coef_mfasis_nn%nn(i)%out(k)%auxparams(coef_mfasis_nn%nn(i)%out(k)%nauxparams), STAT=err)
            THROWM(err.NE.0, 'allocating NN outputs data')
            IF (coef_mfasis_nn%nn(i)%out(k)%nauxparams > 0) THEN
              READ (file_id,  * , iostat=err) &
                coef_mfasis_nn%nn(i)%out(k)%auxparams(:)
              THROWM(err.NE.0, 'io status while reading section '//section)
            ENDIF
          ENDDO

          i = i + 1
        ELSE
          ! Skip data

          ! Dimensions
          READ (file_id,  * , iostat=err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) n_input, n_output, n_nodes_max
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) n_hidden
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err)
          THROWM(err.NE.0, 'io status while reading section '//section)

          ALLOCATE(bias_i(n_nodes_max), &
                   bias_h(n_nodes_max, n_hidden - 1), &
                   bias_o(n_output), stat=err)
          THROWM(err.NE.0, 'allocation of NN biases')

          ! Biases
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) bias_i
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) bias_h
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) bias_o
          THROWM(err.NE.0, 'io status while reading section '//section)

          DEALLOCATE(bias_i, bias_h, bias_o, stat=err)
          THROWM(err.NE.0, 'deallocation of NN biases')

          ALLOCATE(weight_i(n_input, n_nodes_max), &
                   weight_h(n_nodes_max, n_nodes_max, n_hidden - 1), &
                   weight_o(n_nodes_max, n_output), stat=err)
          THROWM(err.NE.0, 'allocation of NN weights')

          ! Weights
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) weight_i
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) weight_h
          THROWM(err.NE.0, 'io status while reading section '//section)
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          READ (file_id,  * , iostat=err) weight_o
          THROWM(err.NE.0, 'io status while reading section '//section)

          DEALLOCATE(weight_i, weight_h, weight_o, stat=err)
          THROWM(err.NE.0, 'deallocation of NN biases')

          ! Input nodes data
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          DO k = 1, n_input
            READ (file_id,  * , iostat=err)
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err)
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err)
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) nauxparams
            THROWM(err.NE.0, 'io status while reading section '//section)
            IF (nauxparams > 0) THEN
              READ (file_id,  * , iostat=err)
              THROWM(err.NE.0, 'io status while reading section '//section)
            ENDIF
          ENDDO

          ! Output nodes data
          CALL rttov_skipcommentline(file_id, err)
          THROWM(err.NE.0, 'io status while reading section '//section)
          DO k = 1, n_output
            READ (file_id,  * , iostat=err)
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err)
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err)
            THROWM(err.NE.0, 'io status while reading section '//section)
            READ (file_id,  * , iostat=err) nauxparams
            THROWM(err.NE.0, 'io status while reading section '//section)
            IF (nauxparams > 0) THEN
              READ (file_id,  * , iostat=err)
              THROWM(err.NE.0, 'io status while reading section '//section)
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      IF (ALLOCATED(mfasis_ext_index)) DEALLOCATE(mfasis_ext_index)
      IF (ALLOCATED(channels_mfasis))  DEALLOCATE(channels_mfasis)

    CASE ('END')
      RETURN
    CASE DEFAULT
      CYCLE readfile
    END SELECT

  ENDDO readfile

  CATCH
END SUBROUTINE rttov_read_ascii_mfasis_nn_file
