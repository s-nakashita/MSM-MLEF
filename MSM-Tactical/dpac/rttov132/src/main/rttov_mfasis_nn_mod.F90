! Description:
!> @file
!!   Subroutines needed by MFASIS-NN
!!
!> @brief
!!   Subroutines needed by MFASIS-NN
!!
!! @details
!!                         used in 
!!  comp_fbot              direct/TL/AD
!!                         computes weighting factors for 2 layer models 
!!                           in forward computations
!!  fornado_inference_nl   TL/AD
!!                         like nonlinear NN routine fornado_inference
!!                           but has additional output "sigo" (needed for TL and AD)  
!!  actfunc                direct/TL/AD
!!                         called by  fornado_inference and fornado_inference_nl
!!  nn_transform_input     direct/TL/AD
!!                         required when calling fornado_inference[_nl]
!!  nn_transform_output    direct/TL/AD
!!                         required when calling fornado_inference[_nl]
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
   ! History:
   ! Version   Date     Comment
   ! -------   ----     -------
   !   1.0     2022     first version    (O.Stiller, DWD)

MODULE rttov_mfasis_nn_mod

  USE parkind1, ONLY : jprb, jpim
  USE rttov_const, ONLY : pi                            
  USE rttov_const, ONLY : errorstatus_fatal

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: aa_sc, noinput, comp_fbot, fornado_inference_nl, actfunc, &
            nn_transform_input, nn_transform_output

  REAL(jprb), PARAMETER :: aa_sc = 0.3_jprb
  REAL(jprb), PARAMETER :: noinput = -99.9_jprb

CONTAINS

   !----------------------------------------------------------------------------------------------
   !   Compute weighting factors fbot for bottom layers
   !----------------------------------------------------------------------------------------------
  SUBROUTINE  comp_fbot(fbot, tau_thr, dtaul, nlay, aa_sc, nlev_surf)
  IMPLICIT NONE
  INTEGER(jpim),    INTENT(IN)    :: nlay
  REAL(jprb),       INTENT(OUT)   :: fbot(nlay)
  REAL(jprb),       INTENT(IN)    :: tau_thr
  REAL(jprb),       INTENT(IN)    :: dtaul(:)
  REAL(jprb),       INTENT(IN)    :: aa_sc
  INTEGER(jpim),    INTENT(IN)    :: nlev_surf

  REAL(jprb)                      :: ffunc(nlay)
  REAL(jprb)                      :: taui_int, fbot_old, ffunc_old
  REAL(jprb)                      :: aa
  INTEGER(jpim)                   :: j, jj

  taui_int = 0._jprb
  fbot(:) = 0._jprb
  fbot_old  = 0._jprb
  ffunc_old = 0._jprb
  aa = - tau_thr
  if(aa > -aa_sc * 0.5_jprb  * pi ) THEN
    ffunc_old = 0.5_jprb*(aa+ aa_sc*0.5_jprb*pi - aa_sc*COS(aa/aa_sc) )
  endif
  DO j = 1, nlay
    IF (j > nlev_surf - 1) EXIT    ! Layer is entirely below surface pressure so nothing more to do
    IF(dtaul(j) /= 0) THEN
      taui_int = taui_int + dtaul(j)
      aa = taui_int - tau_thr
      IF(    aa <=  -aa_sc * 0.5_jprb  * pi ) THEN
        ffunc(j) = 0
        fbot(j)  = 0
        cycle
      ELSEIF(aa >=   aa_sc * 0.5_jprb  * pi ) THEN
        ffunc(j) = aa
      ELSE
        ffunc(j) = 0.5_jprb*(aa+ aa_sc*0.5_jprb*pi - aa_sc*COS(aa/aa_sc) )
      ENDIF
      fbot(j) = (ffunc(j) - ffunc_old)/dtaul(j)
      fbot_old  = fbot(j)
      ffunc_old = ffunc(j)
    ELSE
      ffunc(j) = ffunc_old
      fbot(j)  = fbot_old
      cycle
    ENDIF
    IF( aa >   aa_sc * 0.5_jprb  * pi ) THEN !set all to one
      DO jj=j+1, nlay
        fbot(jj) = 1._jprb
      ENDDO
      EXIT
    ENDIF
  ENDDO
  END SUBROUTINE comp_fbot


  !----------------------------------------------------------------------------------------------
  !
  !   FORNADO NEURAL NETWORK INFERENCE CODE -- NON-LINEAR VERSION
  !   2019-10 L.Scheck
  !                               Compute "sigo" for uses in TL/AD 2022-08  O.Stiller
  !
  !   to generate a python module from this subroutine use
  !     f2py -c fornado_inference_d.F90 -m fornado_inference_d_f90 --f90flags="-O3"
  !   (for a debug version, use
  !     f2py -c fornado_inference_d.F90 -m fornado_inference_d_f90 --f90flags="-g -fcheck='all'"
  !   (see http://cens.ioc.ee/projects/f2py2e/usersguide/f2py_usersguide.pdf )
  !
  !----------------------------------------------------------------------------------------------
  subroutine fornado_inference_nl( n_samples,                             &
                                 n_input, n_output, n_hidden, n_nodes,  &
                                           act_h,    act_o,             &
                                 weight_i, weight_h, weight_o,          &
                                 bias_i,   bias_h,   bias_o,            &
                                 sigo, x,     y     )


  implicit none
  integer, parameter :: sp = jprb !CSt: sp = kind(1.0)
  integer(jpim), intent(in) :: n_samples, n_input, n_output, n_hidden, n_nodes

  integer(jpim), intent(in) :: act_h, act_o

  real(sp), dimension( n_input, n_nodes ),             intent(in) :: weight_i
  real(sp), dimension( n_nodes, n_nodes, n_hidden-1 ), intent(in) :: weight_h
  real(sp), dimension( n_nodes, n_output ),            intent(in) :: weight_o

  real(sp), dimension( n_nodes ),                      intent(in) :: bias_i
  real(sp), dimension( n_nodes, n_hidden-1 ),          intent(in) :: bias_h
  real(sp), dimension( n_output ),                     intent(in) :: bias_o

! real(sp), dimension(n_samples,0:n_hidden+1, n_nodes ), intent(out):: sigo
  real(sp), allocatable,                               intent(out):: sigo(:,:,:)

  real(sp), dimension( n_samples, n_input  ),          intent(in) :: x
  real(sp), dimension( n_samples, n_output ),       intent(inout) :: y

  real(sp), dimension( n_nodes ) :: sigi
  ! logical, dimension( n_nodes ) :: pos

  integer(jpim) :: i_sample, i, j, l

  allocate(sigo(n_samples,0:n_hidden, n_nodes ))
  ! unvectorized version: inner loop over nodes -------------------------------

  ! loop over all input data sets
  do i_sample = 1, n_samples
    
    ! propagate signal from input layer to first hidden layer ....................
    l=0
    do i=1,n_nodes
      sigo(i_sample,l,i)  = bias_i(i)
      do j=1,n_input
        sigo(i_sample,l,i)  =  sigo(i_sample,l,i) + &
                                   weight_i(j, i) * x( i_sample,j)
      end do
    end do
       
    ! apply activation function
    call actfunc ( act_h, sigo(i_sample,l,:),  sigi )

    ! propagate signal towards last hidden layer .................................
    do l=1, n_hidden-1

      ! progagate from layer l to l+1 (l=1 is first hidden layer)
      do i=1,n_nodes
        sigo(i_sample,l,i)  = bias_h(i,l)
        do j=1,n_nodes
          sigo(i_sample,l,i)  =  sigo(i_sample,l,i) + &
                                      weight_h(j, i, l) * sigi(j)
        end do
      end do
          
      ! apply activation function
      call actfunc ( act_h, sigo(i_sample,l,:),  sigi )

    end do ! layer loop

    ! propagate signal to output layer ...........................................
    l=n_hidden
    do i=1,n_output
      sigo(i_sample,l,i)  = bias_o(i)
      do j=1,n_nodes
        sigo(i_sample,l,i)  =  sigo(i_sample,l,i) + &
                                   weight_o(j, i) * sigi(j)
      end do
    end do
     
    ! apply activation function
    call actfunc ( act_o, sigo(i_sample,l,1:n_output), y(i_sample,:) )

  end do ! sample loop

  end subroutine fornado_inference_nl

  pure subroutine actfunc( functype, so, si )

  ! activation functions
  integer, parameter :: sp = jprb !CSt: sp = kind(1.0)

  integer(jpim), intent(in) :: functype

#ifdef VECTORIZE
  real(sp), dimension(:,:),    intent(in) :: so
  real(sp), dimension(:,:), intent(inout) :: si
#else
  real(sp), dimension(:),    intent(in) :: so
  real(sp), dimension(:), intent(inout) :: si
#endif

  select case (functype)
  case (0)               ! linear
         si  = so

  case (1)               ! relu
#ifdef IFLESS
    si = MAX( 0._jprb, so )
#else
    where( so .gt. 0 )
      si  = so
    elsewhere
      si  = 0._jprb
    endwhere
#endif

  case (2)               ! elu
#ifdef IFLESS
    si = MIN( ABS(so), exp(so) - 1._jprb )
#else
    where( so .gt. 0 )
      si  = so
    elsewhere
      si  = exp(so) - 1._jprb
    endwhere
#endif

  case (3)               ! softplus
    si  = log( exp(so) + 1._jprb )

  case (99) ! csu
#ifdef IFLESS
    si = MIN( ABS(so), -1._jprb + 0.25_jprb*MAX(0._jprb,so+2._jprb)**2 )
#else
    where( so .gt. 0 )
      si  = so
    elsewhere( so .lt. -2 )
      si = -1._jprb
    elsewhere
      si  = -1._jprb + 0.25_jprb*(so+2._jprb)**2
    endwhere
#endif

  case default
    si  = -999.999_jprb
  end select

  end subroutine actfunc
  !----------------------------------------------------------------------------------------------
  !
  !   Transform input variable for NN (train_model.py normalize())
  !
  !----------------------------------------------------------------------------------------------
  SUBROUTINE nn_transform_input( var, minv, maxv, trafo, quality, qflag, err )
  IMPLICIT NONE
  REAL(jprb),       INTENT(INOUT) :: var
  REAL(jprb),       INTENT(IN)    :: minv, maxv
  CHARACTER(LEN=32),INTENT(IN)    :: trafo
! INTEGER(KIND=jpim),INTENT(INOUT),POINTER, OPTIONAL :: quality
  INTEGER(KIND=jpim),INTENT(INOUT),         OPTIONAL :: quality
  INTEGER(KIND=jpim),INTENT(IN)   ,         OPTIONAL :: qflag
  INTEGER(jpim),    INTENT(OUT)   ,         OPTIONAL :: err

  REAL(jprb)  :: var_test

  IF(present(err)) err = 0
  IF(present(quality))  var_test = var
  var = MIN( MAX( var,minv ), maxv )
  IF(present(quality)) THEN
    IF (var_test /= var) quality = IBSET(quality, qflag)
  ENDIF

  IF (TRIM(trafo) == 'LIN') THEN ! uniform distribution between min and max
    var = (var - minv) / (maxv - minv)
  ELSE IF (TRIM(trafo) == 'LOG') THEN ! uniform distribution in log(parameter)
    var = (LOG(var) - LOG(minv)) / (LOG(maxv) - LOG(minv))
    !CSt: I have seen that someone pasted SQRT and SQRTNONE also as trafo for
    !input. 
    !CSt: Do these really exist? In this case the var = MIN( MAX(...)) should
    !not be done for the SQRTNONE trafo
    ! ELSE IF (TRIM(trafo) == 'SQRT') THEN
    !    var = (SQRT(var) - SQRT(minv)) / (SQRT(maxv) - SQRT(minv))
    ! ELSE IF (TRIM(trafo) == 'SQRTNONE') THEN
    !    var = SQRT(var)
  ELSE 
    IF(present(err)) err = errorstatus_fatal
!   THROWM(err.NE.0, "MFASIS-NN nn_transform_input selected trafo does not exist")
  ENDIF
  END SUBROUTINE nn_transform_input

  !----------------------------------------------------------------------------------------------
  !
  !   Transform output variable for NN (train_model.py denormalize(), added
  !   sqrtnone trafo)
  !
  !----------------------------------------------------------------------------------------------
  SUBROUTINE nn_transform_output( var, minv, maxv, trafo )
  IMPLICIT NONE
  REAL(jprb),       INTENT(INOUT) :: var
  REAL(jprb),       INTENT(IN)    :: minv, maxv
  CHARACTER(LEN=32),INTENT(IN)    :: trafo

  IF (TRIM(trafo) /= 'SQRTNONE') THEN
    var = MIN( MAX( var,minv ), maxv )
  ENDIF

  IF (TRIM(trafo) == 'LIN') THEN
    var = minv + var*(maxv - minv)
  ELSE IF (TRIM(trafo) == 'LOG') THEN
    var = EXP( LOG(minv) + var*( LOG(maxv) - LOG(minv) ) )
  ELSE IF (TRIM(trafo) == 'SQRT') THEN
    var = ( SQRT(minv) + var*( SQRT(maxv) - SQRT(minv) ) )**2
  ELSE IF (TRIM(trafo) == 'SQRTNONE') THEN
    var = var**2
  ENDIF
  END SUBROUTINE nn_transform_output

END MODULE rttov_mfasis_nn_mod
