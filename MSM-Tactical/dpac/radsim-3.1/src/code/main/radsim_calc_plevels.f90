!-------------------------------------------------------------------------------
! Description:
!
!   Calculate pressure levels for the ECMWF, HARMONIE and JMA models.
!
!   Half-level pressures are a linear function of surface pressure:
!
!   ph = a + b*psurf
!
!   Full-level pressures can be found by taking the mean of two half-levels.
!   (The number of full-levels is one fewer than half-levels).
!
!   Coefficients A and B have been included in the radsim code explicitly for
!   some level sets but optional input of these has been included for
!   flexibility and forward compatibility.
!
! Reference:
!
!   http://old.ecmwf.int/research/ifsdocs/DYNAMICS/Chap2_Discretization4.html#961180
!
! Copyright:
!
!   This software was developed within the context of the EUMETSAT Satellite
!   Application Facility on Numerical Weather Prediction (NWP SAF), under the
!   Cooperation Agreement dated 7 December 2016, between EUMETSAT and the
!   Met Office, UK, by one or more partners within the NWP SAF. The partners
!   in the NWP SAF are the Met Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2018, EUMETSAT, All Rights Reserved.
!
!-------------------------------------------------------------------------------

subroutine radsim_calc_plevels( &
  model,    & ! in: model name
  psurf,    & ! in: surface pressure
  p,        & ! out, optional: pressure levels
  ph,       & ! out, optional: pressure at half levels
  coeffs_A, & ! in, optional: coefficients A
  coeffs_B  ) ! in, optional: coefficients B

use radsim_mod_constants, only : &
  real64, &
  plevels_ECMWF_60_A,  &
  plevels_ECMWF_60_B,  &
  plevels_ECMWF_91_A,  &
  plevels_ECMWF_91_B,  &
  plevels_ECMWF_137_A, &
  plevels_ECMWF_137_B, &
  plevels_HARMONIE_65_A, &
  plevels_HARMONIE_65_B, &
  plevels_JMA_60_A,  &
  plevels_JMA_60_B,  &
  plevels_JMA_100_A, &
  plevels_JMA_100_B, &
  status_error, &
  status_warning

implicit none

include 'radsim_error_report.interface'

character(len=*),               intent(in)  :: model
real(real64),                   intent(in)  :: psurf(:)
real(real64), optional,         intent(out) :: p(:,:)
real(real64), optional, target, intent(out) :: ph(:,:)
real(real64), optional, target, intent(in)  :: coeffs_A(:)
real(real64), optional, target, intent(in)  :: coeffs_B(:)

integer :: nprofs, nlevels
real(real64), pointer :: A(:), B(:)
real(real64), pointer :: ph_calc(:,:)
logical :: got_coeffs
integer :: i, prof
character(len=80) :: message

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Check coeffs
!-------------------------------------------------------------------------------

if ( present(p) ) then
  nprofs = size(p, dim=1)
  nlevels = size(p, dim=2)
else if ( present(ph) ) then
  nprofs = size(ph, dim=1)
  nlevels = size(ph, dim=2) - 1
else
  return
end if
got_coeffs = present(coeffs_A) .and. present(coeffs_B)

! Input coeffs may be corrupted. Check from level 2 upwards, the first two may
! be zero. B should always be in the range 0->1.

if ( got_coeffs ) then
  A => coeffs_A
  B => coeffs_B
  do i = 2, nlevels
    if ( A(i) <= 0.0 .and. B(i) <= 0.0 .or. B(i) > 1.0+1.0E-6 ) then
      call radsim_error_report('Bad input pressure level coeffs', status_warning)
      print '(a)', '...Pre-calculated set will be used'
      got_coeffs = .false.
      exit
    end if
  end do
end if

!-------------------------------------------------------------------------------
! 2. Point to stored set if we haven't got them
!-------------------------------------------------------------------------------

if ( .not. got_coeffs ) then
  if ( trim(model) == 'ecmwf' ) then
    select case(nlevels)
      case(60)
        A => plevels_ECMWF_60_A
        B => plevels_ECMWF_60_B
      case(91)
        A => plevels_ECMWF_91_A
        B => plevels_ECMWF_91_B
      case(137)
        A => plevels_ECMWF_137_A
        B => plevels_ECMWF_137_B
      case default
        write(message, '(a,i0,a)') &
          'No pressure level coefficients available for ECMWF ', nlevels, &
          'levels, cannot continue'
        call radsim_error_report(message, status_error)
      end select
  elseif ( trim(model) == 'harmonie' ) then
    select case(nlevels)
      case(65)
        A => plevels_HARMONIE_65_A
        B => plevels_HARMONIE_65_B
      case default
        write(message, '(a,i0,a)') &
          'No pressure level coefficients available for HARMONIE ', nlevels, &
          'levels, cannot continue'
        call radsim_error_report(message, status_error)
    end select
  elseif ( trim(model) == 'jma' ) then
    select case(nlevels)
      case(60)
        A => plevels_JMA_60_A
        B => plevels_JMA_60_B
      case(100)
        A => plevels_JMA_100_A
        B => plevels_JMA_100_B
      case default
        write(message, '(a,i0,a)') &
          'No pressure level coefficients available for JMA ', nlevels, &
          'levels, cannot continue'
        call radsim_error_report(message, status_error)
    end select
  else
    write(message, '(a)') &
      'Unknown model, cannot calculate pressure levels'
    call radsim_error_report(message, status_error)
  end if
end if

!-------------------------------------------------------------------------------
! 3. Calculate ph and p
!-------------------------------------------------------------------------------

if ( present(ph) ) then
  ph_calc => ph
else
  allocate(ph_calc(nprofs, nlevels+1))
end if

do i = 1, nlevels+1
  do prof = 1, nprofs
    ph_calc(prof,i) = A(i) + B(i) * psurf(prof)
  end do
end do

if ( present(p) ) then
  do i = 1, nlevels
    do prof = 1, nprofs
      p(prof,i) = 0.5 * (ph_calc(prof,i) + ph_calc(prof,i+1))
    end do
  end do
end if

if ( .not. present(ph) ) deallocate(ph_calc)

end subroutine radsim_calc_plevels
