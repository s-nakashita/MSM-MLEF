!-------------------------------------------------------------------------------
! Description:
!
!   Allocate RTTOV data structures.
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

subroutine radsim_init_rttov_data( &
  alloc,          & ! in
  nchans,         & ! in
  nchansout,      & ! in
  nlevels,        & ! in
  opts,           & ! in
  coefs,          & ! in
  coef_scatt,     & ! in
  chanprof,       & ! out
  radiance,       & ! out
  radiance_k,     & ! out
  transmission,   & ! out
  transmission_k, & ! out
  pccomp,         & ! out
  pccomp_k,       & ! out
  emissivity,     & ! out
  emissivity_k,   & ! out
  reflectance,    & ! out
  reflectance_k,  & ! out
  profiles,       & ! out
  profiles_k,     & ! out
  profiles_k_pc,  & ! out
  profiles_k_rec, & ! out
  cld_profiles,   & ! out
  frequencies     ) ! out

use parkind1, only : &
  jpim, jplm

use radsim_mod_cfg

use radsim_mod_constants, only : &
  status_error, &
  output_debug

use rttov_const, only : &
  errorstatus_success

use rttov_types, only : &
  rttov_options,       &
  rttov_coefs,         &
  rttov_scatt_coef,    &
  rttov_profile,       &
  rttov_profile_cloud, &
  rttov_transmission,  &
  rttov_radiance,      &
  rttov_pccomp,        &
  rttov_chanprof,      &
  rttov_emissivity,    &
  rttov_reflectance

implicit none

include 'radsim_error_report.interface'
#include "rttov_alloc_pccomp.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_alloc_scatt_prof.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_init_emis_refl.interface"
#include "rttov_scatt_setupindex.interface"

integer(kind=jpim),                     intent(in)    :: alloc ! 1=allocate, 0=deallocate
integer(kind=jpim),                     intent(in)    :: nchans
integer(kind=jpim),                     intent(in)    :: nchansout
integer(kind=jpim),                     intent(in)    :: nlevels
type(rttov_options),                    intent(in)    :: opts
type(rttov_coefs),                      intent(in)    :: coefs
type(rttov_scatt_coef),                 intent(in)    :: coef_scatt
type(rttov_chanprof),      allocatable, intent(inout) :: chanprof(:)
type(rttov_radiance),                   intent(inout) :: radiance
type(rttov_radiance),                   intent(inout) :: radiance_k
type(rttov_transmission),               intent(inout) :: transmission
type(rttov_transmission),               intent(inout) :: transmission_k
type(rttov_pccomp),                     intent(inout) :: pccomp
type(rttov_pccomp),                     intent(inout) :: pccomp_k
type(rttov_emissivity),    allocatable, intent(inout) :: emissivity(:)
type(rttov_emissivity),    allocatable, intent(inout) :: emissivity_k(:)
type(rttov_reflectance),   allocatable, intent(inout) :: reflectance(:)
type(rttov_reflectance),   allocatable, intent(inout) :: reflectance_k(:)
type(rttov_profile),       allocatable, intent(inout) :: profiles(:)
type(rttov_profile),       allocatable, intent(inout) :: profiles_k(:)
type(rttov_profile),       allocatable, intent(inout) :: profiles_k_pc(:)
type(rttov_profile),       allocatable, intent(inout) :: profiles_k_rec(:)
type(rttov_profile_cloud), allocatable, intent(inout) :: cld_profiles(:)
integer(kind=jpim),        allocatable, intent(inout) :: frequencies(:)

character(len=80) :: message
integer :: i, j, chan
integer(kind=jpim) :: nprofchans
integer(kind=jpim) :: errorstatus
logical(kind=jplm), allocatable :: usechans(:,:)

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 1. Set up indices
!-------------------------------------------------------------------------------

nprofchans = nchans * nprofs_per_call

if ( alloc == 1 ) then

  if ( output_mode >= output_debug ) then
    print '(a)', ' Setting up channel arrays'
  end if

  allocate(chanprof(nprofchans))

  if ( .not. run_scatt ) then

    ! Build the list of chanprof indices (doesn't take into account that rttov may
    ! define channels differently).

    do j = 1, nprofs_per_call
      do i = 1, nchans
        chanprof((j-1)*nchans+i) % prof = j
        if ( addpc ) then
          chanprof((j-1)*nchans+i) % chan = &
            coefs % coef_pccomp % pcreg(ipcbnd,ipcreg) % predictindex(i)
        else if ( .not. htfrtc ) then
          chanprof((j-1)*nchans+i) % chan = channels(i)
        end if
      end do
    end do

  else

    allocate(frequencies(nprofchans))
    frequencies = 0

    allocate(usechans(nprofs_per_call, coefs % coef % fmv_chn))
    usechans = .false.
    do i = 1, nchans
      chan = channels(i)
      usechans(:,chan) = .true.
    end do

    call rttov_scatt_setupindex ( &
      errorstatus,                & ! out
      nprofs_per_call,            & ! in
      coefs % coef % fmv_chn,     & ! in
      coefs,                      & ! in
      coef_scatt,                 & ! in
      nprofchans,                 & ! in
      chanprof,                   & ! out
      frequencies,                & ! out
      lchannel_subset=usechans    ) ! in, optional

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_scatt_setupindx ', errorstatus
      call radsim_error_report(message, status_error)
    end if

    deallocate(usechans)

  end if

else

  deallocate(chanprof)
  if ( allocated(frequencies) ) deallocate(frequencies)

end if

!-------------------------------------------------------------------------------
! 2. Allocate other RTTOV structures
!-------------------------------------------------------------------------------

!----------------
! 2.1 Direct code
!----------------

! Profile structure

if ( alloc == 1 ) then
  if ( output_mode >= output_debug ) then
    print '(a)', ' Allocating RTTOV structures'
  end if
  allocate(profiles(nprofs_per_call))
end if

call rttov_alloc_prof( &
  errorstatus,       &
  nprofs_per_call,   &
  profiles,          &
  nlevels,           &
  opts,              &
  alloc,             &
  coefs = coefs,     &
  init = .true._jplm ) ! zero contents?

if ( errorstatus /= errorstatus_success ) then
  write(message, '(a,i0)') 'Error in rttov_alloc_prof = ', errorstatus
  call radsim_error_report(message, status_error)
end if

if ( alloc == 0 ) then
  deallocate(profiles)
end if

if ( .not. htfrtc ) then

  ! Radiance structure

  call rttov_alloc_rad( &
    errorstatus, &
    nprofchans,  &
    radiance,    &
    nlevels,     &
    alloc,       & ! 1=allocate, 0=deallocate
    init = .true._jplm )

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_alloc_rad = ', errorstatus
    call radsim_error_report(message, status_error)
  end if

  ! Transmittance structure

  call rttov_alloc_transmission( &
    errorstatus,       &
    transmission,      &
    nlevels,           &
    nprofchans,        &
    alloc,             & ! 1=allocate, 0=deallocate
    init = .true._jplm )

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_alloc_transmission = ', errorstatus
    call radsim_error_report(message, status_error)
  end if

end if

if ( addpc .or. htfrtc ) then

  ! PC output structure

  call rttov_alloc_pccomp( &
    errorstatus,               &
    pccomp,                    &
    npcscores*nprofs_per_call, &
    alloc,                     & ! 1=allocate, 0=deallocate
    .true._jplm,               &
    nchansout*nprofs_per_call, &
    opts, nlevels)

  if ( errorstatus /= errorstatus_success ) then
    write(message, '(a,i0)') 'Error in rttov_alloc_pccomp = ', errorstatus
    call radsim_error_report(message, status_error)
  end if

end if

! Emissivity structure

if ( alloc == 1 ) then
  allocate(emissivity(nprofchans))
  call rttov_init_emis_refl(emis=emissivity)
else
  deallocate(emissivity)
end if

! BRDF structure

if ( alloc == 1 ) then
  allocate(reflectance(nprofchans))
  call rttov_init_emis_refl(refl=reflectance)
else
  deallocate(reflectance)
end if

!-----------
! 4.2 K code
!-----------

if ( run_k ) then

  ! Profile structure

  if ( alloc == 1 ) then
    if ( output_mode >= output_debug ) then
      print '(a)', ' Allocating RTTOV K structures'
    end if
    allocate(profiles_k(nprofchans))

    if ( addpc .or. htfrtc ) then
      allocate(profiles_k_rec(nchansout*nprofs_per_call))
    end if
    if ( htfrtc ) then
      allocate(profiles_k_pc(npcscores*nprofs_per_call))
    end if

  end if

  if ( .not. htfrtc ) then

    call rttov_alloc_prof( &
      errorstatus,         &
      nprofchans,          &
      profiles_k,          &
      nlevels,             &
      opts,                &
      alloc,               & ! 1=allocate, 0=deallocate
      coefs = coefs,       &
      init = .true._jplm   ) ! zero contents?

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_alloc_prof = ', errorstatus
      call radsim_error_report(message, status_error)
    end if

  end if

  if ( addpc .or. htfrtc ) then

    call rttov_alloc_prof( &
      errorstatus,               &
      nchansout*nprofs_per_call, &
      profiles_k_rec,            &
      nlevels,                   &
      opts,                      &
      alloc,                     & ! 1=allocate, 0=deallocate
      coefs = coefs,             &
      init = .true._jplm         ) ! zero contents?

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_alloc_prof = ', errorstatus
      call radsim_error_report(message, status_error)
    end if

  end if

  if ( htfrtc ) then

    call rttov_alloc_prof( &
      errorstatus,               &
      npcscores*nprofs_per_call, &
      profiles_k_pc,             &
      nlevels,                   &
      opts,                      &
      alloc,                     & ! 1=allocate, 0=deallocate
      coefs = coefs,             &
      init = .true._jplm         ) ! zero contents?

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_alloc_prof = ', errorstatus
      call radsim_error_report(message, status_error)
    end if

  end if

  if ( alloc == 0 ) then
    deallocate(profiles_k)
    if ( addpc .or. htfrtc ) deallocate(profiles_k_rec)
    if ( htfrtc ) deallocate(profiles_k_pc)
  end if

  if ( .not. htfrtc ) then

    ! Radiance structure

    call rttov_alloc_rad( &
      errorstatus,       &
      nprofchans,        &
      radiance_k,        &
      nlevels,           &
      alloc,             & ! 1=allocate, 0=deallocate
      init = .true._jplm )

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_alloc_rad = ', errorstatus
      call radsim_error_report(message, status_error)
    end if

    ! Transmittance structure

    call rttov_alloc_transmission( &
      errorstatus,       &
      transmission_k,    &
      nlevels,           &
      nprofchans,        &
      alloc,             & ! 1=allocate, 0=deallocate
      init = .true._jplm )

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_alloc_transmission = ', errorstatus
      call radsim_error_report(message, status_error)
    end if

  end if

  if ( addpc ) then

    ! PC output structure (not used by HTFRTC)

    call rttov_alloc_pccomp( &
      errorstatus,               &
      pccomp_k,                  &
      npcscores*nprofs_per_call, &
      alloc,                     & ! 1=allocate, 0=deallocate
      .true._jplm,               &
      nchansout*nprofs_per_call)

    if ( errorstatus /= errorstatus_success ) then
      write(message, '(a,i0)') 'Error in rttov_alloc_pccomp = ', errorstatus
      call radsim_error_report(message, status_error)
    end if

  end if

  if ( alloc == 1 ) then
    allocate(emissivity_k(nprofchans))
    call rttov_init_emis_refl(emis=emissivity_k)
  else
    deallocate(emissivity_k)
  end if

  if ( alloc == 1 ) then
    allocate(reflectance_k(nprofchans))
    call rttov_init_emis_refl(refl=reflectance_k)
  else
    deallocate(reflectance_k)
  end if
end if

!---------------------------
! 4.3 Scattering direct code
!---------------------------

if ( run_scatt ) then

  if ( alloc == 1 ) then
    allocate(cld_profiles(nprofs_per_call))
  end if

  call rttov_alloc_scatt_prof( &
    errorstatus,         &
    nprofs_per_call,     &
    cld_profiles,        &
    nlevels,             &
    coef_scatt % nhydro, & ! nhydro in hydrotable
    1_jpim,              & ! nhydro_frac - number of cloud fraction profiles
    alloc,               & ! 1=allocate, 0=deallocate
    init=.true._jplm)      ! zero contents?

  if ( alloc == 0 ) then
    deallocate(cld_profiles)
  end if

end if

end subroutine radsim_init_rttov_data
