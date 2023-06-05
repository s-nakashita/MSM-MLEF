! Description:
!> @file
!!   Extract data for given channel list from an MFASIS NN structure.
!
!> @brief
!!   Extract data for given channel list from an MFASIS NN structure.
!!
!! @details
!!   This is used by HDF5 I/O code to read in a subset of channels from a
!!   coefficient file. The first coef argument contains the coefficients
!!   from the file. The second argument is an uninitialised structure
!!   which contains the extracted coefficients on exit.
!!
!!   NB The channel list MUST include at least one channel supported by
!!   the input MFASIS structure.
!!
!! @param[out]     err              status on exit
!! @param[in]      coef_mfasis_nn1  input coefficients read from file
!! @param[in,out]  coef_mfasis_nn2  output coefficients, uninitialised on entry
!! @param[in]      channels         list of channels to extract
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
SUBROUTINE rttov_channel_extract_mfasis_nn(err, coef_mfasis_nn1, coef_mfasis_nn2, channels)
!INTF_OFF
#include "throw.h"
!INTF_ON
      USE parkind1, ONLY : jpim
      USE rttov_types, ONLY : rttov_coef_mfasis_nn
!INTF_ON
      IMPLICIT NONE
    
      INTEGER(jpim),              INTENT(OUT)   :: err
      TYPE(rttov_coef_mfasis_nn), INTENT(IN)    :: coef_mfasis_nn1
      TYPE(rttov_coef_mfasis_nn), INTENT(INOUT) :: coef_mfasis_nn2
      INTEGER(jpim),              INTENT(IN)    :: channels(:)
!INTF_END
    
#include "rttov_errorreport.interface"
#include "rttov_channel_extract_sublist.interface"
    
      INTEGER(jpim)              :: j, m
      INTEGER(jpim), ALLOCATABLE :: channels_mfasis_nn(:)
      INTEGER(jpim), ALLOCATABLE :: mfasis_nn_ext_index(:)
    ! ----------------------------------------------------------------------------
    
      TRY
    
      ! Work out the new channel list
    
      coef_mfasis_nn2%nchannels_coef = SIZE(channels)
    
      ALLOCATE(channels_mfasis_nn(coef_mfasis_nn1%nchannels))
    
      ! Determine the solar channel numbers
      channels_mfasis_nn(:) = coef_mfasis_nn1%channel_list(:)
    
      ! Determine solar channels/phase functions to be extracted
      ALLOCATE(mfasis_nn_ext_index(coef_mfasis_nn1%nchannels))
      CALL rttov_channel_extract_sublist( &
            err,                              &
            channels_mfasis_nn,               &
            channels,                         &
            coef_mfasis_nn2%nchannels,        &
            coef_mfasis_nn2%channel_list,     &
            coef_mfasis_nn2%channel_nn_index, &
            mfasis_nn_ext_index)
      THROW(err.NE.0)
    
      IF (coef_mfasis_nn2%nchannels == 0) THEN
        DEALLOCATE(channels_mfasis_nn, mfasis_nn_ext_index)
        err = errorstatus_fatal
        THROWM(err.NE.0, 'No MFASIS channels to extract')
      ENDIF

      ! See rttov_read_ascii_mfasis_nn_file.F90 for a description of what the various arrays contain.
      ALLOCATE(coef_mfasis_nn2%nn(coef_mfasis_nn2%nchannels), stat=err)
      THROWM(err.NE.0, 'allocation of nn')
      DO j = 1, coef_mfasis_nn2%nchannels
        ALLOCATE(coef_mfasis_nn2%nn(j)%in(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_input), stat=err)
        THROWM(err.NE.0, 'allocation of nn%in')
        DO m = 1, SIZE(coef_mfasis_nn2%nn(j)%in)
          coef_mfasis_nn2%nn(j)%in(m)%max = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%in(m)%max
          coef_mfasis_nn2%nn(j)%in(m)%min = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%in(m)%min
          coef_mfasis_nn2%nn(j)%in(m)%name = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%in(m)%name
          coef_mfasis_nn2%nn(j)%in(m)%transform = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%in(m)%transform
          coef_mfasis_nn2%nn(j)%in(m)%nauxparams = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%in(m)%nauxparams
          ALLOCATE(coef_mfasis_nn2%nn(j)%in(m)%auxparams(coef_mfasis_nn2%nn(j)%in(m)%nauxparams), stat=err)
          THROWM(err.NE.0, 'allocation of nn%in%auxparams')
          coef_mfasis_nn2%nn(j)%in(m)%auxparams = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%in(m)%auxparams
        ENDDO
        ALLOCATE(coef_mfasis_nn2%nn(j)%out(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_output), stat=err)
        THROWM(err.NE.0, 'allocation of nn%out')
        DO m = 1, SIZE(coef_mfasis_nn2%nn(j)%out)
          coef_mfasis_nn2%nn(j)%out(m)%max = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%out(m)%max
          coef_mfasis_nn2%nn(j)%out(m)%min = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%out(m)%min
          coef_mfasis_nn2%nn(j)%out(m)%name = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%out(m)%name
          coef_mfasis_nn2%nn(j)%out(m)%transform = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%out(m)%transform
          ALLOCATE(coef_mfasis_nn2%nn(j)%out(m)%auxparams( &
                      SIZE(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%out(m)%auxparams)), stat=err)
          THROWM(err.NE.0, 'allocation of nn%in%auxparams')
          coef_mfasis_nn2%nn(j)%out(m)%auxparams = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%out(m)%auxparams
        ENDDO
        coef_mfasis_nn2%nn(j)%actfunc_hl = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%actfunc_hl
        coef_mfasis_nn2%nn(j)%actfunc_ol = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%actfunc_ol
        ALLOCATE(coef_mfasis_nn2%nn(j)%bias_i(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max), stat=err)
        THROWM(err.NE.0, 'allocation of nn%bias_i')
        coef_mfasis_nn2%nn(j)%bias_i = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%bias_i
        ALLOCATE(coef_mfasis_nn2%nn(j)%bias_h(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max, &
                                              coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_hidden - 1), stat=err)
        THROWM(err.NE.0, 'allocation of nn%bias_h')
        coef_mfasis_nn2%nn(j)%bias_h = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%bias_h
        ALLOCATE(coef_mfasis_nn2%nn(j)%bias_o(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_output), stat=err)
        THROWM(err.NE.0, 'allocation of nn%bias_o')
        coef_mfasis_nn2%nn(j)%bias_o = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%bias_o
        ALLOCATE(coef_mfasis_nn2%nn(j)%weight_i(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_input, &
                                                coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max), stat=err)
        THROWM(err.NE.0, 'allocation of nn%weight_i')
        coef_mfasis_nn2%nn(j)%weight_i = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%weight_i
        ALLOCATE(coef_mfasis_nn2%nn(j)%weight_h(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max, &
                                                coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max, &
                                                coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_hidden - 1), stat=err)
        THROWM(err.NE.0, 'allocation of nn%weight_h')
        coef_mfasis_nn2%nn(j)%weight_h = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%weight_h
        ALLOCATE(coef_mfasis_nn2%nn(j)%weight_o(coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max,  &
                                                coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_output), stat=err)
        THROWM(err.NE.0, 'allocation of nn%weight_o')
        coef_mfasis_nn2%nn(j)%weight_o = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%weight_o
        coef_mfasis_nn2%nn(j)%n_input = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_input
        coef_mfasis_nn2%nn(j)%n_output = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_output
        coef_mfasis_nn2%nn(j)%n_hidden = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_hidden
        coef_mfasis_nn2%nn(j)%n_nodes_max = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%n_nodes_max
        coef_mfasis_nn2%nn(j)%nn_version = coef_mfasis_nn1%nn(mfasis_nn_ext_index(j))%nn_version
      ENDDO
    
      IF (ALLOCATED(channels_mfasis_nn))  DEALLOCATE(channels_mfasis_nn)
      IF (ALLOCATED(mfasis_nn_ext_index)) DEALLOCATE(mfasis_nn_ext_index)
    
    
      ! Everything else is the same...
      coef_mfasis_nn2%version       = coef_mfasis_nn1%version
      coef_mfasis_nn2%file_type     = coef_mfasis_nn1%file_type
      coef_mfasis_nn2%creation_date = coef_mfasis_nn1%creation_date
      coef_mfasis_nn2%readme_nn     = coef_mfasis_nn1%readme_nn
      coef_mfasis_nn2%nparticles    = coef_mfasis_nn1%nparticles
    
      IF (coef_mfasis_nn1%file_type /= 1) THEN
        err=1
        THROWM(err.NE.0, 'MFASIS-NN file_type not yet implemented')
      ENDIF
    
      CATCH
    END SUBROUTINE rttov_channel_extract_mfasis_nn
    
