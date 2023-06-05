! Description:
!> @file
!!   Read a binary MFASIS NN file, optionally extracting a subset of channels.
!
!> @brief
!!   Read a binary MFASIS NN file, optionally extracting a subset of channels.
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
SUBROUTINE rttov_read_binary_mfasis_nn_file( &
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
  USE rttov_const, ONLY : rttov_magic_string, rttov_magic_number, &
                          mfasis_nn_version_compatible_min,       &
                          mfasis_nn_version_compatible_max
  USE parkind1, ONLY : jprb, jplm
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
#include "rttov_nullify_coef_mfasis_nn.interface"
#include "rttov_channel_extract_sublist.interface"

  INTEGER(KIND=jpim) :: nchannels_mfasis
  LOGICAL(KIND=jplm) :: all_channels
  INTEGER(KIND=jpim) :: i, j, k

  INTEGER(KIND=jpim), ALLOCATABLE :: channels_rtcoef(:)  ! List of rtcoef channels
  INTEGER(KIND=jpim), ALLOCATABLE :: channels_mfasis(:)  ! List of MFASIS channels
  INTEGER(KIND=jpim), ALLOCATABLE :: mfasis_ext_index(:) ! Indexes of NNs to extract

  CHARACTER(LEN=16) :: bin_check_string
  REAL(KIND=jprb)   :: bin_check_number
  REAL(KIND=jprb)   :: bin_check_value
!- End of header --------------------------------------------------------
  TRY

  all_channels = .NOT. PRESENT(channels)

  CALL rttov_nullify_coef_mfasis_nn(coef_mfasis_nn)

  READ (file_id, iostat=err) bin_check_string, bin_check_number
  THROWM(err.NE.0,'io status while reading header')

  ! Verification of header string
  IF (bin_check_string /= rttov_magic_string) err = errorstatus_fatal
  THROWM(err.NE.0,'Wrong header string in file')

  ! Verification of single/double precision using a 5 digit number
  ! with exponent 12, which is always Ok for single precision
  bin_check_value = 1._jprb - ABS(bin_check_number - rttov_magic_number)
  IF (bin_check_value > 1.01_jprb .OR. bin_check_value < 0.99_jprb) err = errorstatus_fatal
  THROWM(err.NE.0,'File created with a different real precision (R4<->R8)')


  READ (file_id, iostat=err) coef_mfasis_nn%file_type,      &
                             coef_mfasis_nn%version,        &
                             coef_mfasis_nn%creation_date,  &
                             nchannels_mfasis,              &
                             coef_mfasis_nn%nchannels_coef, &
                             coef_mfasis_nn%nparticles
  THROWM(err.NE.0, 'reading MFASIS file header')

  IF (PRESENT(file_type)) THEN
    IF (coef_mfasis_nn%file_type /= file_type) THEN
      err = errorstatus_fatal
      THROWM(err.NE.0, 'Error: MFASIS file type mismatch (cloud/aerosol)')
    ENDIF
  ENDIF

  IF (coef_mfasis_nn%version < mfasis_nn_version_compatible_min .OR. &
      coef_mfasis_nn%version > mfasis_nn_version_compatible_max) THEN
    err = errorstatus_fatal
    THROWM(err.NE.0, "Version of MFASIS NN file is incompatible with RTTOV library")
  ENDIF

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

  ALLOCATE(channels_mfasis(nchannels_mfasis))
  READ (file_id, iostat=err)channels_mfasis(:)
  THROWM(err.NE.0, 'reading MFASIS channel list')

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

  ! See rttov_read_ascii_mfasis_nn_file.F90 for a description of what the various arrays contain.

  READ (file_id, iostat=err) coef_mfasis_nn%readme_nn

  ALLOCATE(coef_mfasis_nn%nn(coef_mfasis_nn%nchannels), STAT = err)
  THROWM(err.NE.0, "allocation of coef_mfasis_nn%nn array")

  i = 1
  DO j = 1, nchannels_mfasis
    IF (j == mfasis_ext_index(i)) THEN
      READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%nn_version,  &
                                coef_mfasis_nn%nn(i)%n_input,     &
                                coef_mfasis_nn%nn(i)%n_output,    &
                                coef_mfasis_nn%nn(i)%n_hidden,    &
                                coef_mfasis_nn%nn(i)%n_nodes_max, &
                                coef_mfasis_nn%nn(i)%actfunc_hl,  &
                                coef_mfasis_nn%nn(i)%actfunc_ol
      THROWM(err.NE.0, 'while reading NN header')

      ALLOCATE(coef_mfasis_nn%nn(i)%bias_i(coef_mfasis_nn%nn(i)%n_nodes_max),  &
               coef_mfasis_nn%nn(i)%bias_h(coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                           coef_mfasis_nn%nn(i)%n_hidden - 1), &
               coef_mfasis_nn%nn(i)%bias_o(coef_mfasis_nn%nn(i)%n_output), stat=err)
      THROWM(err.NE.0, 'allocation of NN biases')

      READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%bias_i, &
                                coef_mfasis_nn%nn(i)%bias_h, &
                                coef_mfasis_nn%nn(i)%bias_o
      THROWM(err.NE.0, 'while reading NN biases')

      ALLOCATE(coef_mfasis_nn%nn(i)%weight_i(coef_mfasis_nn%nn(i)%n_input,       &
                                             coef_mfasis_nn%nn(i)%n_nodes_max),  &
               coef_mfasis_nn%nn(i)%weight_h(coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                             coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                             coef_mfasis_nn%nn(i)%n_hidden - 1), &
               coef_mfasis_nn%nn(i)%weight_o(coef_mfasis_nn%nn(i)%n_nodes_max,   &
                                             coef_mfasis_nn%nn(i)%n_output), stat=err)
      THROWM(err.NE.0, 'allocation of NN weights')

      READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%weight_i, &
                                coef_mfasis_nn%nn(i)%weight_h, &
                                coef_mfasis_nn%nn(i)%weight_o
      THROWM(err.NE.0, 'while reading NN weights')

      ALLOCATE(coef_mfasis_nn%nn(i)%in(coef_mfasis_nn%nn(i)%n_input), STAT=err)
      THROWM(err.NE.0, 'allocating NN inputs data')
      DO k = 1, coef_mfasis_nn%nn(i)%n_input
        READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%in(k)%name,      &
                                  coef_mfasis_nn%nn(i)%in(k)%transform, &
                                  coef_mfasis_nn%nn(i)%in(k)%min,       &
                                  coef_mfasis_nn%nn(i)%in(k)%max,       &
                                  coef_mfasis_nn%nn(i)%in(k)%nauxparams
        THROWM(err.NE.0, 'while reading NN inputs data')

        ALLOCATE(coef_mfasis_nn%nn(i)%in(k)%auxparams(coef_mfasis_nn%nn(i)%in(k)%nauxparams), STAT=err)
        THROWM(err.NE.0, 'allocating NN inputs data')
        READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%in(k)%auxparams
        THROWM(err.NE.0, 'while reading NN inputs data')
      ENDDO

      ALLOCATE(coef_mfasis_nn%nn(i)%out(coef_mfasis_nn%nn(i)%n_output), STAT=err)
      THROWM(err.NE.0, 'allocating NN outputs data')
      DO k = 1, coef_mfasis_nn%nn(i)%n_output
        READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%out(k)%name,      &
                                  coef_mfasis_nn%nn(i)%out(k)%transform, &
                                  coef_mfasis_nn%nn(i)%out(k)%min,       &
                                  coef_mfasis_nn%nn(i)%out(k)%max,       &
                                  coef_mfasis_nn%nn(i)%out(k)%nauxparams
        THROW(err.NE.0)

        ALLOCATE(coef_mfasis_nn%nn(i)%out(k)%auxparams(coef_mfasis_nn%nn(i)%out(k)%nauxparams), STAT=err)
        THROWM(err.NE.0, 'allocating NN outputs data')
        READ (file_id, iostat=err)coef_mfasis_nn%nn(i)%out(k)%auxparams
        THROWM(err.NE.0, 'while reading NN outputs data')
      ENDDO

      i = i + 1
    ELSE
      ! Skip records
      READ (file_id, iostat=err)
      READ (file_id, iostat=err)
      READ (file_id, iostat=err)
      DO k = 1, coef_mfasis_nn%nn(i)%n_input
        READ (file_id, iostat=err)
        READ (file_id, iostat=err)
      ENDDO
      DO k = 1, coef_mfasis_nn%nn(i)%n_input
        READ (file_id, iostat=err)
        READ (file_id, iostat=err)
      ENDDO
    ENDIF
  ENDDO

  IF (ALLOCATED(mfasis_ext_index)) DEALLOCATE(mfasis_ext_index)
  IF (ALLOCATED(channels_mfasis))  DEALLOCATE(channels_mfasis)

  CATCH
END SUBROUTINE rttov_read_binary_mfasis_nn_file
