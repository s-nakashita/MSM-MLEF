! Description:
!> @file
!!   Read polarization coefficient file.
!!
!> @brief
!!   Read polarization coefficient file.
!!
!! @details
!!   Read in the polarization coefficient (poltable) file. It contains the LUT of
!!   scaling factors to applied to the cloud optical properties.
!!
!!   The 5th dimension of the LUT contain the scaling factors for the optical properties:
!!   position 1 corresponds to extinction
!!      >>    2      >>     to asymmetry parameter
!!      >>    3      >>     to single scattering albedo
!!      >>    4      >>     to the backscattering
!!
!! @param[out]     err          status on exit
!! @param[in]      opts_scatt   RTTOV-SCATT options
!! @param[inout]   coeff_scatt  MW scattering coefficients
!! @param[in]      path         Path to coefficients directory, checked if file_coef is not specified
!! @param[in]      file_coef    Path of filename of LUT containing scaling coef; sensor independent; optional
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
! Method:
! - Barlakas, V., A. Geer and P. Eriksson, 2022: Cloud particle orientation and polarisation
!     for cross-track microwave sensors in RTTOV
!     NWP SAF Report No. NWPSAF-EC-VS-061
!
! History:
! Version   Date      Comment
! -------   ----      -------
! 1.0       06/2022   Initial version     (V. Barlakas)
! 1.1       08/2022   Adapt for storing directly in the scattering coefs (A. Geer)
!
Subroutine rttov_read_polcoeffs (err, opts_scatt, coef_scatt, path, file_pol)       

!INTF_OFF
#include "throw.h"
!INTF_ON

  Use parkind1,       Only : jpim
  Use rttov_types,    Only : rttov_scatt_coef, rttov_options_scatt
!INTF_OFF
  Use parkind1,       Only : jprb
  Use rttov_const,    Only : pol_coef_default_file_name
!INTF_ON

  Implicit None  
  Integer(Kind=jpim), Intent(out)        :: err
  Type(rttov_options_scatt), Intent(in)  :: opts_scatt
  Type(rttov_scatt_coef), Intent(inout)  :: coef_scatt
  Character(Len=*), optional, Intent(in) :: path
  Character(Len=*), optional, Intent(in) :: file_pol
!INTF_END

#include "rttov_errorreport.interface"

  ! Local variables
  Integer(kind=jpim)                :: nf, nT, nwc, nz, ios, allocs, fid = 16, i_lwc, i_temp, i_freq
  Integer(kind=jpim)                :: fi, Ti, wci, zi, i_frozen, i_type
  Real(kind=jprb), dimension(coef_scatt%mwc) :: lwc
  Real(kind=jprb), dimension(coef_scatt%mtemp) :: temp
  Real(kind=jprb)                   :: f_hz
  Character(len=500)                :: file_coef
  Real(kind=jprb)                   :: temp_offset
  Real(kind=jprb)                   :: fw, Tw, fw1, Tw1, WCw, WCw1                            
  Real(kind=jprb), allocatable      :: coef_v(:,:,:,:,:),coef_h(:,:,:,:,:)
  Real(kind=jprb), allocatable      :: f_arts(:), T_arts(:), wc_table(:)
  !- End of header --------------------------------------------------------

  TRY

  ! Water content grid [kg/m3]
  Do i_lwc = 1, coef_scatt%mwc
    lwc(i_lwc) = 1e-3_jprb * 10.0_JPRB ** ( (i_lwc + coef_scatt % offset_water) / coef_scatt % scale_water )
  Enddo

  ! Find a frozen hydrometeor for the temp offset
  i_frozen = -1
  do i_type = 1, coef_scatt%mtype 
    if (coef_scatt%is_frozen(i_type)) i_frozen = i_type
  enddo
  if (i_frozen /= -1) then
    temp_offset = coef_scatt%offset_temp(i_frozen)
  else
    err = errorstatus_fatal
    THROWM (err /= errorstatus_success , 'No frozen hydrometeor found')
  endif

  ! Temperature grid
  Do i_temp = 1, coef_scatt%mtemp
    temp(i_temp) = temp_offset + i_temp 
  Enddo

  ! Open LUT
  if (present(file_pol)) then
    file_coef = file_pol
  else
    if (present(path)) then
      file_coef = trim(path)//'/'//pol_coef_default_file_name
    else
      file_coef = pol_coef_default_file_name
    endif
  endif

  Open(unit=fid, status = 'old', iostat=ios, file=trim(file_coef))
  if (ios /= 0) err = errorstatus_fatal
  THROWM (err /= errorstatus_success , 'File does not exist: '//Trim(file_coef))

  if (opts_scatt%config%verbose) INFO('Open ASCII ARO scaled polarised scattering coefficient file '//Trim(file_coef))

  ! Parse file
  Read(fid, *, iostat=ios)                    ! comment line
  Read(fid, *, iostat=ios) nf, nT, nwc, nz    ! dimensions
  if (ios /= 0) err = errorstatus_fatal
  THROWM (err /= errorstatus_success , 'Problem reading: '//Trim(file_coef))

  ! Allocate variables
  Allocate (coef_v(nf,nT,nwc,nz,4),stat=allocs) ! polarization coefficients
  if (allocs /= 0) err = errorstatus_fatal
  THROWM(err /= errorstatus_success, "allocation of v pol ARO scaling array")
  Allocate (coef_h(nf,nT,nwc,nz,4),stat=allocs) ! polarization coefficients
  if (allocs /= 0) err = errorstatus_fatal
  THROWM(err /= errorstatus_success, "allocation of v pol ARO scaling array")
  Allocate (f_arts(nf),T_arts(nT))      ! frequency and temperature grids
  Allocate (wc_table(nwc))              ! water content grids

  ! Allocate interpolated variables
  Allocate(coef_scatt%coef_pol_v(coef_scatt%mfreqm,coef_scatt%mtemp,coef_scatt%mwc,nz,4),stat=allocs)
  if (allocs /= 0) err = errorstatus_fatal
  THROWM(err /= errorstatus_success, "allocation of v pol ARO scaling array")
  Allocate(coef_scatt%coef_pol_h(coef_scatt%mfreqm,coef_scatt%mtemp,coef_scatt%mwc,nz,4),stat=allocs)
  if (allocs /= 0) err = errorstatus_fatal
  THROWM(err /= errorstatus_success, "allocation of h pol ARO scaling array")
  Allocate(coef_scatt%z_table(nz),stat=allocs)
  if (allocs /= 0) err = errorstatus_fatal
  THROWM(err /= errorstatus_success, "allocation of z table ARO scaling array")
  coef_scatt%coef_pol_v = 0
  coef_scatt%coef_pol_h = 0

  ! Read rest of the data
  Read(fid, *, iostat=ios)                  ! comment line
  Read(fid, *, iostat=ios) f_arts
  if (ios /= 0) err = errorstatus_fatal
  THROWM (err /= errorstatus_success , 'f table: '//Trim(file_coef))

  Read(fid, *, iostat=ios)                  ! comment line
  Read(fid, *, iostat=ios) T_arts
  if (ios /= 0) err = errorstatus_fatal
  THROWM (err /= errorstatus_success , 'T table: '//Trim(file_coef))

  Read(fid, *, iostat=ios)                  ! comment line
  Read(fid, *, iostat=ios) wc_table
  if (ios /= 0) err = errorstatus_fatal
  THROWM (err /= errorstatus_success , 'wc table: '//Trim(file_coef))

  Read(fid, *, iostat=ios)                  ! comment line
  Read(fid, *, iostat=ios) coef_scatt%z_table 
  if (ios /= 0) err = errorstatus_fatal
  THROWM (err /= errorstatus_success , 'z table: '//Trim(file_coef))

  Read(fid, *, iostat=ios)                  ! comment line

  Do fi = 1,nf
    Do Ti = 1, nT
      Do wci = 1, nwc
        Do zi = 1, nz
          read(fid, *, iostat=ios) coef_v(fi,Ti,wci,zi,:) ! Read in LUT at V pol.
          if (ios /= 0) err = errorstatus_fatal
          THROWM (err /= errorstatus_success , 'Problem reading v table: '//Trim(file_coef))
        Enddo
      Enddo
    Enddo 
  Enddo

  Read(fid, *, iostat=ios)                   ! comment line

  Do fi = 1,nf
    Do Ti = 1, nT
      Do wci = 1, nwc
        Do zi = 1, nz
          read(fid, *, iostat=ios) coef_h(fi,Ti,wci,zi,:) ! Read in LUT at H pol.
          if (ios /= 0) err = errorstatus_fatal
          THROWM (err /= errorstatus_success , 'Problem reading h table: '//Trim(file_coef))
        Enddo
      Enddo
    Enddo 
  Enddo
  close(fid)

  ! Interpolate LUT to the :
  ! - frequency of the the specific instrument
  ! - temperature grid of coef_scatt
  ! - water grid of coef_scatt

  Do i_freq = 1, coef_scatt%mfreqm

    f_hz   = coef_scatt%freq(i_freq) ! [Hz]
    fi = 1
    Do while ( (fi<nf-1) .and. (f_arts(fi+1)<f_hz) )
      fi = fi+1
    Enddo

    Do i_temp = 1, coef_scatt%mtemp

      Ti = 1
      do while ( (Ti<nT-1) .and. (T_arts(Ti+1)<temp(i_temp)) )
        Ti = Ti+1
      enddo

      Do i_lwc = 1, coef_scatt%mwc

        wci = 1
        Do while ( (wci<nwc-1) .and. (wc_table(wci+1)<lwc(i_lwc)) )
          wci = wci+1
        Enddo

        fw  = (f_arts(fi+1)-f_hz)         / (f_arts(fi+1)-f_arts(fi))
        Tw  = (T_arts(Ti+1)-temp(i_temp)) / (T_arts(Ti+1)-T_arts(Ti))
        WCw = (wc_table(wci+1)-lwc(i_lwc))/ (wc_table(wci+1)-wc_table(wci))

        fw1 = 1.-fw
        Tw1 = 1.-Tw
        WCw1= 1.-WCw

        coef_scatt%coef_pol_v(i_freq,i_temp,i_lwc,:,:) = (coef_v(fi  ,Ti  ,wci  ,:,:)*fw *Tw *WCw + &
                                               coef_v(fi  ,Ti  ,wci+1,:,:)*fw *Tw *WCw1+ &
                                               coef_v(fi  ,Ti+1,wci  ,:,:)*fw *Tw1*WCw + & 
                                               coef_v(fi  ,Ti+1,wci+1,:,:)*fw *Tw1*WCw1+ &
                                               coef_v(fi+1,Ti  ,wci  ,:,:)*fw1*Tw *WCw + & 
                                               coef_v(fi+1,Ti  ,wci+1,:,:)*fw1*Tw *WCw1+ & 
                                               coef_v(fi+1,Ti+1,wci  ,:,:)*fw1*Tw1*WCw + &
                                               coef_v(fi+1,Ti+1,wci+1,:,:)*fw1*Tw1*WCw1)

        coef_scatt%coef_pol_h(i_freq,i_temp,i_lwc,:,:) = (coef_h(fi  ,Ti  ,wci  ,:,:)*fw *Tw *WCw + &
                                               coef_h(fi  ,Ti  ,wci+1,:,:)*fw *Tw *WCw1+ &
                                               coef_h(fi  ,Ti+1,wci  ,:,:)*fw *Tw1*WCw + &
                                               coef_h(fi  ,Ti+1,wci+1,:,:)*fw *Tw1*WCw1+ &
                                               coef_h(fi+1,Ti  ,wci  ,:,:)*fw1*Tw *WCw + &
                                               coef_h(fi+1,Ti  ,wci+1,:,:)*fw1*Tw *WCw1+ &
                                               coef_h(fi+1,Ti+1,wci  ,:,:)*fw1*Tw1*WCw + &
                                               coef_h(fi+1,Ti+1,wci+1,:,:)*fw1*Tw1*WCw1)

      Enddo ! lwc  loop
    Enddo  ! temp loop
  Enddo   ! freq loop

  Deallocate(coef_v,coef_h,f_arts,T_arts,wc_table)

  CATCH

End Subroutine rttov_read_polcoeffs
