!-------------------------------------------------------------------------------
! Description:
!
!   Interpolate all model fields to observation positions.
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

subroutine radsim_interp(obs, lat_sim, lon_sim, model_in, model_out, keep_input)

use radsim_mod_constants, only : &
  real64, &
  real32

use radsim_mod_types, only : &
  model_type, &
  obs_type

use radsim_mod_cfg, only : &
  temporal_data

implicit none

include 'radsim_grid_init.interface'
include 'radsim_interp_horiz.interface'
include 'radsim_interp_unstructured.interface'
include 'radsim_interp_index.interface'

type(obs_type),    intent(in)    :: obs         ! Observation data
real(real32),      intent(in)    :: lat_sim(:)  ! Latitudes for simulated profiles
real(real32),      intent(in)    :: lon_sim(:)  ! Longitudes for simulated profiles
type(model_type),  intent(inout) :: model_in(:) ! Input model data
type(model_type),  intent(inout) :: model_out   ! Output model data
logical, optional, intent(in)    :: keep_input  ! Keep input model data

integer :: i, j, k
integer :: nobs, nlevels, model_ntimes
real(real64), allocatable :: input_array(:,:,:)
real(real64), pointer :: output_array1d(:)
real(real64), pointer :: output_array2d(:,:)
logical :: keep
integer, allocatable :: x_index(:)
integer, allocatable :: y_index(:)
integer, allocatable :: z_index(:)
real(real64), allocatable :: t_weight(:)

!-------------------------------------------------------------------------------

model_ntimes = size(model_in)
nobs = size(lon_sim)    ! number of simulated profiles, not input obs
if ( present(keep_input) ) then
  keep = keep_input
else
  keep = .true.
end if

! Create indices locating position of observation points in the model arrays

call radsim_interp_index( &
  model_in(1) % grid, &
  lon_sim, &
  lat_sim, &
  x_index, &
  y_index, &
  mask = obs % qcflags(obs % sim_to_obs) /= 0 )

! Create indices for temporal interpolation

allocate(z_index(nobs), t_weight(nobs))

if ( temporal_data ) then

  do i = 1, nobs
    k = obs % sim_to_obs(i)
    ! No extrapolation if obs times lie beyond range of model data
    if (obs % ref_time(k) <= model_in(1) % ref_time) then
      z_index(i) = 1
      t_weight(i) = 1.0_real64
    else if (obs % ref_time(k) >= model_in(model_ntimes) % ref_time) then
      z_index(i) = model_ntimes - 1
      t_weight(i) = 0.0_real64
    else
      do j = 2, model_ntimes
        if (obs % ref_time(k) < model_in(j) % ref_time) then
          z_index(i) = j - 1
          t_weight(i) = real(model_in(j) % ref_time - obs % ref_time(k)) / &
                        real(model_in(j) % ref_time - model_in(j-1) % ref_time)
          exit
        end if
      end do
    end if
  end do

else
  z_index = 1
  t_weight = 1.0_real64
end if


! Allocate space for nlevels+1 because half-level pressures may have an
! additional value.

allocate(input_array(model_in(1)%nprofs, model_in(1)%nlevels+1, model_ntimes))

do i = 1, 60

  nlevels = 0

  select case(i)

    !--------------------
    ! Single-level fields
    !--------------------

    case(1)
      if ( associated(model_in(1) % pstar) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % pstar
          if (.not. keep) deallocate(model_in(j) % pstar)
        end do
        if ( associated(model_out % pstar) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % pstar)
        end if
        if ( .not. associated(model_out % pstar) ) allocate(model_out % pstar(nobs))
        output_array1d => model_out % pstar
      end if
    case(2)
      if ( associated(model_in(1) % t2) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % t2
          if (.not. keep) deallocate(model_in(j) % t2)
        end do
        if ( associated(model_out % t2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % t2)
        end if
        if ( .not. associated(model_out % t2) ) allocate(model_out % t2(nobs))
        output_array1d => model_out % t2
      end if
    case(3)
      if ( associated(model_in(1) % td2) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % td2
          if (.not. keep) deallocate(model_in(j) % td2)
        end do
        if ( associated(model_out % td2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % td2)
        end if
        if ( .not. associated(model_out % td2) ) allocate(model_out % td2(nobs))
        output_array1d => model_out % td2
      end if
    case(4)
      if ( associated(model_in(1) % q2) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % q2
          if (.not. keep) deallocate(model_in(j) % q2)
        end do
        if ( associated(model_out % q2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % q2)
        end if
        if ( .not. associated(model_out % q2) ) allocate(model_out % q2(nobs))
        output_array1d => model_out % q2
      end if
    case(5)
      if ( associated(model_in(1) % rh2) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % rh2
          if (.not. keep) deallocate(model_in(j) % rh2)
        end do
        if ( associated(model_out % rh2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % rh2)
        end if
        if ( .not. associated(model_out % rh2) ) allocate(model_out % rh2(nobs))
        output_array1d => model_out % rh2
      end if
    case(6)
      if ( associated(model_in(1) % tskin) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % tskin
          if (.not. keep) deallocate(model_in(j) % tskin)
        end do
        if ( associated(model_out % tskin) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % tskin)
        end if
        if ( .not. associated(model_out % tskin) ) allocate(model_out % tskin(nobs))
        output_array1d => model_out % tskin
      end if
    case(7)
      if ( associated(model_in(1) % u10) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % u10
          if (.not. keep) deallocate(model_in(j) % u10)
        end do
        if ( associated(model_out % u10) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % u10)
        end if
        if ( .not. associated(model_out % u10) ) allocate(model_out % u10(nobs))
        output_array1d => model_out % u10
      end if
    case(8)
      if ( associated(model_in(1) % v10) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % v10
          if (.not. keep) deallocate(model_in(j) % v10)
        end do
        if ( associated(model_out % v10) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % v10)
        end if
        if ( .not. associated(model_out % v10) ) allocate(model_out % v10(nobs))
        output_array1d => model_out % v10
      end if
    case(9)
      if ( associated(model_in(1) % seaice) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % seaice
          where(input_array(:,1,j) < 0.0 .or. input_array(:,1,j) > 1.0) input_array(:,1,j) = 0.0 ! remove missing data
          if (.not. keep) deallocate(model_in(j) % seaice)
        end do
        if ( associated(model_out % seaice) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % seaice)
        end if
        if ( .not. associated(model_out % seaice) ) allocate(model_out % seaice(nobs))
        output_array1d => model_out % seaice
      end if
    case(10)
      if ( associated(model_in(1) % zsurf) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % zsurf
          where(input_array(:,1,j) < 0.0) input_array(:,1,j) = 0.0 ! remove missing data
          if (.not. keep) deallocate(model_in(j) % zsurf)
        end do
        if ( associated(model_out % zsurf) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % zsurf)
        end if
        if ( .not. associated(model_out % zsurf) ) allocate(model_out % zsurf(nobs))
        output_array1d => model_out % zsurf
      end if
    case(11)
      if ( associated(model_in(1) % lsm) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % lsm
          if (.not. keep) deallocate(model_in(j) % lsm)
        end do
        if ( associated(model_out % lsm) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % lsm)
        end if
        if ( .not. associated(model_out % lsm) ) allocate(model_out % lsm(nobs))
        output_array1d => model_out % lsm
      end if
    case(12)
      if ( associated(model_in(1) % total_cc) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % total_cc
          if (.not. keep) deallocate(model_in(j) % total_cc)
        end do
        if ( associated(model_out % total_cc) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % total_cc)
        end if
        if ( .not. associated(model_out % total_cc) ) allocate(model_out % total_cc(nobs))
        output_array1d => model_out % total_cc
      end if
    case(13)
      if ( associated(model_in(1) % low_cc) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % low_cc
          if (.not. keep) deallocate(model_in(j) % low_cc)
        end do
        if ( associated(model_out % low_cc) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % low_cc)
        end if
        if ( .not. associated(model_out % low_cc) ) allocate(model_out % low_cc(nobs))
        output_array1d => model_out % low_cc
      end if
    case(14)
      if ( associated(model_in(1) % medium_cc) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % medium_cc
          if (.not. keep) deallocate(model_in(j) % medium_cc)
        end do
        if ( associated(model_out % medium_cc) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % medium_cc)
        end if
        if ( .not. associated(model_out % medium_cc) ) allocate(model_out % medium_cc(nobs))
        output_array1d => model_out % medium_cc
      end if
    case(15)
      if ( associated(model_in(1) % high_cc) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % high_cc
          if (.not. keep) deallocate(model_in(j) % high_cc)
        end do
        if ( associated(model_out % high_cc) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % high_cc)
        end if
        if ( .not. associated(model_out % high_cc) ) allocate(model_out % high_cc(nobs))
        output_array1d => model_out % high_cc
      end if
    case(16)
      if ( associated(model_in(1) % snow_depth) ) then
        nlevels = 1
        do j = 1, model_ntimes
          input_array(:,1,j) = model_in(j) % snow_depth
          if (.not. keep) deallocate(model_in(j) % snow_depth)
        end do
        if ( associated(model_out % snow_depth) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % snow_depth)
        end if
        if ( .not. associated(model_out % snow_depth) ) allocate(model_out % snow_depth(nobs))
        output_array1d => model_out % snow_depth
      end if

    !-------------------
    ! Multi-level fields
    !-------------------

    case(21)
      if ( associated(model_in(1) % p)     ) then
        nlevels = size(model_in(1) % p, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % p
          if (.not. keep) deallocate(model_in(j) % p)
        end do
        if ( associated(model_out % p) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % p)
        end if
        if ( .not. associated(model_out % p) ) allocate(model_out % p(nobs,nlevels))
        output_array2d => model_out % p
      end if
    case(22)
      if ( associated(model_in(1) % ph)    ) then
        nlevels = size(model_in(1) % ph, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % ph
          if (.not. keep) deallocate(model_in(j) % ph)
        end do
        if ( associated(model_out % ph) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % ph)
        end if
        if ( .not. associated(model_out % ph) ) allocate(model_out % ph(nobs,nlevels))
        output_array2d => model_out % ph
      end if
    case(23)
      if ( associated(model_in(1) % t)     ) then
        nlevels = size(model_in(1) % t, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % t
          if (.not. keep) deallocate(model_in(j) % t)
        end do
        if ( associated(model_out % t) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % t)
        end if
        if ( .not. associated(model_out % t) ) allocate(model_out % t(nobs,nlevels))
        output_array2d => model_out % t
      end if
    case(24)
      if ( associated(model_in(1) % theta) ) then
        nlevels = size(model_in(1) % theta, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % theta
          if (.not. keep) deallocate(model_in(j) % theta)
        end do
        if ( associated(model_out % theta) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % theta)
        end if
        if ( .not. associated(model_out % theta) ) allocate(model_out % theta(nobs,nlevels))
        output_array2d => model_out % theta
      end if
    case(25)
      if ( associated(model_in(1) % q)     ) then
        nlevels = size(model_in(1) % q, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % q
          if (.not. keep) deallocate(model_in(j) % q)
        end do
        if ( associated(model_out % q) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % q)
        end if
        if ( .not. associated(model_out % q) ) allocate(model_out % q(nobs,nlevels))
        output_array2d => model_out % q
      end if
    case(26)
      if ( associated(model_in(1) % rh)    ) then
        nlevels = size(model_in(1) % rh, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % rh
          if (.not. keep) deallocate(model_in(j) % rh)
        end do
        if ( associated(model_out % rh) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % rh)
        end if
        if ( .not. associated(model_out % rh) ) allocate(model_out % rh(nobs,nlevels))
        output_array2d => model_out % rh
      end if
    case(27)
      if ( associated(model_in(1) % clw)   ) then
        nlevels = size(model_in(1) % clw, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % clw
          if (.not. keep) deallocate(model_in(j) % clw)
        end do
        if ( associated(model_out % clw) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % clw)
        end if
        if ( .not. associated(model_out % clw) ) allocate(model_out % clw(nobs,nlevels))
        output_array2d => model_out % clw
      end if
    case(28)
      if ( associated(model_in(1) % ciw)   ) then
        nlevels = size(model_in(1) % ciw, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % ciw
          if (.not. keep) deallocate(model_in(j) % ciw)
        end do
        if ( associated(model_out % ciw) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % ciw)
        end if
        if ( .not. associated(model_out % ciw) ) allocate(model_out % ciw(nobs,nlevels))
        output_array2d => model_out % ciw
      end if
    case(29)
      if ( associated(model_in(1) % rain)  ) then
        nlevels = size(model_in(1) % rain, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % rain
          if (.not. keep) deallocate(model_in(j) % rain)
        end do
        if ( associated(model_out % rain) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % rain)
        end if
        if ( .not. associated(model_out % rain) ) allocate(model_out % rain(nobs,nlevels))
        output_array2d => model_out % rain
      end if
    case(30)
      if ( associated(model_in(1) % cfrac) ) then
        nlevels = size(model_in(1) % cfrac, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cfrac
          if (.not. keep) deallocate(model_in(j) % cfrac)
        end do
        if ( associated(model_out % cfrac) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cfrac)
        end if
        if ( .not. associated(model_out % cfrac) ) allocate(model_out % cfrac(nobs,nlevels))
        output_array2d => model_out % cfrac
      end if
    case(31)
      if ( associated(model_in(1) % o3)    ) then
        nlevels = size(model_in(1) % o3, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % o3
          if (.not. keep) deallocate(model_in(j) % o3)
        end do
        if ( associated(model_out % o3) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % o3)
        end if
        if ( .not. associated(model_out % o3) ) allocate(model_out % o3(nobs,nlevels))
        output_array2d => model_out % o3
      end if
    case(32)
      if ( associated(model_in(1) % co2)    ) then
        nlevels = size(model_in(1) % co2, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % co2
          if (.not. keep) deallocate(model_in(j) % co2)
        end do
        if ( associated(model_out % co2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % co2)
        end if
        if ( .not. associated(model_out % co2) ) allocate(model_out % co2(nobs,nlevels))
        output_array2d => model_out % co2
      end if
    case(33)
      if ( associated(model_in(1) % n2o)    ) then
        nlevels = size(model_in(1) % n2o, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % n2o
          if (.not. keep) deallocate(model_in(j) % n2o)
        end do
        if ( associated(model_out % n2o) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % n2o)
        end if
        if ( .not. associated(model_out % n2o) ) allocate(model_out % n2o(nobs,nlevels))
        output_array2d => model_out % n2o
      end if
    case(34)
      if ( associated(model_in(1) % co)    ) then
        nlevels = size(model_in(1) % co, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % co
          if (.not. keep) deallocate(model_in(j) % co)
        end do
        if ( associated(model_out % co) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % co)
        end if
        if ( .not. associated(model_out % co) ) allocate(model_out % co(nobs,nlevels))
        output_array2d => model_out % co
      end if
    case(35)
      if ( associated(model_in(1) % ch4)    ) then
        nlevels = size(model_in(1) % ch4, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % ch4
          if (.not. keep) deallocate(model_in(j) % ch4)
        end do
        if ( associated(model_out % ch4) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % ch4)
        end if
        if ( .not. associated(model_out % ch4) ) allocate(model_out % ch4(nobs,nlevels))
        output_array2d => model_out % ch4
      end if
    case(36)
      if ( associated(model_in(1) % so2)    ) then
        nlevels = size(model_in(1) % so2, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % so2
          if (.not. keep) deallocate(model_in(j) % so2)
        end do
        if ( associated(model_out % so2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % so2)
        end if
        if ( .not. associated(model_out % so2) ) allocate(model_out % so2(nobs,nlevels))
        output_array2d => model_out % so2
      end if
    case(37)
      if ( associated(model_in(1) % z)     ) then
        nlevels = size(model_in(1) % z, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % z
          if (.not. keep) deallocate(model_in(j) % z)
        end do
        if ( associated(model_out % z) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % z)
        end if
        if ( .not. associated(model_out % z) ) allocate(model_out % z(nobs,nlevels))
        output_array2d => model_out % z
      end if
    case(38)
      if ( associated(model_in(1) % cfrac_liq) ) then
        nlevels = size(model_in(1) % cfrac_liq, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cfrac_liq
          if (.not. keep) deallocate(model_in(j) % cfrac_liq)
        end do
        if ( associated(model_out % cfrac_liq) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cfrac_liq)
        end if
        if ( .not. associated(model_out % cfrac_liq) ) allocate(model_out % cfrac_liq(nobs,nlevels))
        output_array2d => model_out % cfrac_liq
      end if
    case(39)
      if ( associated(model_in(1) % cfrac_ice) ) then
        nlevels = size(model_in(1) % cfrac_ice, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cfrac_ice
          if (.not. keep) deallocate(model_in(j) % cfrac_ice)
        end do
        if ( associated(model_out % cfrac_ice) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cfrac_ice)
        end if
        if ( .not. associated(model_out % cfrac_ice) ) allocate(model_out % cfrac_ice(nobs,nlevels))
        output_array2d => model_out % cfrac_ice
      end if
    case(40)
      if ( associated(model_in(1) % cfrac_conv) ) then
        nlevels = size(model_in(1) % cfrac_conv, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cfrac_conv
          if (.not. keep) deallocate(model_in(j) % cfrac_conv)
        end do
        if ( associated(model_out % cfrac_conv) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cfrac_conv)
        end if
        if ( .not. associated(model_out % cfrac_conv) ) allocate(model_out % cfrac_conv(nobs,nlevels))
        output_array2d => model_out % cfrac_conv
      end if
    case(41)
      if ( associated(model_in(1) % conv_cloud) ) then
        nlevels = size(model_in(1) % conv_cloud, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % conv_cloud
          if (.not. keep) deallocate(model_in(j) % conv_cloud)
        end do
        if ( associated(model_out % conv_cloud) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % conv_cloud)
        end if
        if ( .not. associated(model_out % conv_cloud) ) allocate(model_out % conv_cloud(nobs,nlevels))
        output_array2d => model_out % conv_cloud
      end if
    case(42)
      if ( associated(model_in(1) % snow) ) then
        nlevels = size(model_in(1) % snow, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % snow
          if (.not. keep) deallocate(model_in(j) % snow)
        end do
        if ( associated(model_out % snow) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % snow)
        end if
        if ( .not. associated(model_out % snow) ) allocate(model_out % snow(nobs,nlevels))
        output_array2d => model_out % snow
      end if
    case(43)
      if ( associated(model_in(1) % density) ) then
        nlevels = size(model_in(1) % density, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % density
          if (.not. keep) deallocate(model_in(j) % density)
        end do
        if ( associated(model_out % density) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % density)
        end if
        if ( .not. associated(model_out % density) ) allocate(model_out % density(nobs,nlevels))
        output_array2d => model_out % density
      end if
    case(44)
      if ( associated(model_in(1) % clw_deff) ) then
        nlevels = size(model_in(1) % clw_deff, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % clw_deff
          if (.not. keep) deallocate(model_in(j) % clw_deff)
        end do
        if ( associated(model_out % clw_deff) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % clw_deff)
        end if
        if ( .not. associated(model_out % clw_deff) ) allocate(model_out % clw_deff(nobs,nlevels))
        output_array2d => model_out % clw_deff
      end if
    case(45)
      if ( associated(model_in(1) % ciw_deff) ) then
        nlevels = size(model_in(1) % ciw_deff, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % ciw_deff
          if (.not. keep) deallocate(model_in(j) % ciw_deff)
        end do
        if ( associated(model_out % ciw_deff) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % ciw_deff)
        end if
        if ( .not. associated(model_out % ciw_deff) ) allocate(model_out % ciw_deff(nobs,nlevels))
        output_array2d => model_out % ciw_deff
      end if
    case(46)
      if ( associated(model_in(1) % cams_sea_salt1) ) then
        nlevels = size(model_in(1) % cams_sea_salt1, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_sea_salt1
          if (.not. keep) deallocate(model_in(j) % cams_sea_salt1)
        end do
        if ( associated(model_out % cams_sea_salt1) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_sea_salt1)
        end if
        if ( .not. associated(model_out % cams_sea_salt1) ) allocate(model_out % cams_sea_salt1(nobs,nlevels))
        output_array2d => model_out % cams_sea_salt1
      end if
    case(47)
      if ( associated(model_in(1) % cams_sea_salt2) ) then
        nlevels = size(model_in(1) % cams_sea_salt2, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_sea_salt2
          if (.not. keep) deallocate(model_in(j) % cams_sea_salt2)
        end do
        if ( associated(model_out % cams_sea_salt2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_sea_salt2)
        end if
        if ( .not. associated(model_out % cams_sea_salt2) ) allocate(model_out % cams_sea_salt2(nobs,nlevels))
        output_array2d => model_out % cams_sea_salt2
      end if
    case(48)
      if ( associated(model_in(1) % cams_sea_salt3) ) then
        nlevels = size(model_in(1) % cams_sea_salt3, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_sea_salt3
          if (.not. keep) deallocate(model_in(j) % cams_sea_salt3)
        end do
        if ( associated(model_out % cams_sea_salt3) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_sea_salt3)
        end if
        if ( .not. associated(model_out % cams_sea_salt3) ) allocate(model_out % cams_sea_salt3(nobs,nlevels))
        output_array2d => model_out % cams_sea_salt3
      end if
    case(49)
      if ( associated(model_in(1) % cams_dust1) ) then
        nlevels = size(model_in(1) % cams_dust1, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_dust1
          if (.not. keep) deallocate(model_in(j) % cams_dust1)
        end do
        if ( associated(model_out % cams_dust1) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_dust1)
        end if
        if ( .not. associated(model_out % cams_dust1) ) allocate(model_out % cams_dust1(nobs,nlevels))
        output_array2d => model_out % cams_dust1
      end if
    case(50)
      if ( associated(model_in(1) % cams_dust2) ) then
        nlevels = size(model_in(1) % cams_dust2, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_dust2
          if (.not. keep) deallocate(model_in(j) % cams_dust2)
        end do
        if ( associated(model_out % cams_dust2) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_dust2)
        end if
        if ( .not. associated(model_out % cams_dust2) ) allocate(model_out % cams_dust2(nobs,nlevels))
        output_array2d => model_out % cams_dust2
      end if
    case(51)
      if ( associated(model_in(1) % cams_dust3) ) then
        nlevels = size(model_in(1) % cams_dust3, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_dust3
          if (.not. keep) deallocate(model_in(j) % cams_dust3)
        end do
        if ( associated(model_out % cams_dust3) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_dust3)
        end if
        if ( .not. associated(model_out % cams_dust3) ) allocate(model_out % cams_dust3(nobs,nlevels))
        output_array2d => model_out % cams_dust3
      end if
    case(52)
      if ( associated(model_in(1) % cams_hphil_omat) ) then
        nlevels = size(model_in(1) % cams_hphil_omat, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_hphil_omat
          if (.not. keep) deallocate(model_in(j) % cams_hphil_omat)
        end do
        if ( associated(model_out % cams_hphil_omat) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_hphil_omat)
        end if
        if ( .not. associated(model_out % cams_hphil_omat) ) allocate(model_out % cams_hphil_omat(nobs,nlevels))
        output_array2d => model_out % cams_hphil_omat
      end if
    case(53)
      if ( associated(model_in(1) % cams_hphob_bcar) ) then
        nlevels = size(model_in(1) % cams_hphob_bcar, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_hphob_bcar
          if (.not. keep) deallocate(model_in(j) % cams_hphob_bcar)
        end do
        if ( associated(model_out % cams_hphob_bcar) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_hphob_bcar)
        end if
        if ( .not. associated(model_out % cams_hphob_bcar) ) allocate(model_out % cams_hphob_bcar(nobs,nlevels))
        output_array2d => model_out % cams_hphob_bcar
      end if
    case(54)
      if ( associated(model_in(1) % cams_sulphate) ) then
        nlevels = size(model_in(1) % cams_sulphate, dim=2)
        do j = 1, model_ntimes
          input_array(:,1:nlevels,j) = model_in(j) % cams_sulphate
          if (.not. keep) deallocate(model_in(j) % cams_sulphate)
        end do
        if ( associated(model_out % cams_sulphate) .and. model_out % nprofs /= nobs ) then
          deallocate(model_out % cams_sulphate)
        end if
        if ( .not. associated(model_out % cams_sulphate) ) allocate(model_out % cams_sulphate(nobs,nlevels))
        output_array2d => model_out % cams_sulphate
      end if
    case default
      cycle
  end select

  if ( nlevels > 1 ) then
    do j = 1, nlevels
      if ( model_in(1) % grid % type == 101 ) then
        call radsim_interp_unstructured( &
          model_in(1) % rmdi, &
          input_array(:,j,:), &
          nobs, &
          x_index, &
          z_index, &
          t_weight, &
          output_array2d(:,j), &
          mask = obs % qcflags(obs % sim_to_obs) /= 0 )
      else
        call radsim_interp_horiz( &
          model_in(1) % rmdi, &
          model_in(1) % grid, &
          input_array(:,j,:), &
          lon_sim, &
          lat_sim, &
          x_index, &
          y_index, &
          z_index, &
          t_weight, &
          output_array2d(:,j), &
          mask = obs % qcflags(obs % sim_to_obs) /= 0 )
      end if
    end do
  else if ( nlevels == 1 ) then
    if ( model_in(1) % grid % type == 101 ) then
      call radsim_interp_unstructured( &
        model_in(1) % rmdi, &
        input_array(:,1,:), &
        nobs, &
        x_index, &
        z_index, &
        t_weight, &
        output_array1d, &
        mask = obs % qcflags(obs % sim_to_obs) /= 0 )
    else
      call radsim_interp_horiz( &
        model_in(1) % rmdi, &
        model_in(1) % grid, &
        input_array(:,1,:), &
        lon_sim, &
        lat_sim, &
        x_index, &
        y_index, &
        z_index, &
        t_weight, &
        output_array1d, &
        mask = obs % qcflags(obs % sim_to_obs) /= 0 )
    end if
  end if

end do

deallocate(x_index, y_index, z_index, t_weight, input_array)

! Copy meta-data

model_out % nprofs = nobs
model_out % nlevels = model_in(1) % nlevels
model_out % rmdi = model_in(1) % rmdi
model_out % imdi = model_in(1) % imdi

! Model data is now on the observation grid. Reset and assign the correct
! values.

call radsim_grid_init(model_out % grid)
allocate(model_out % grid % lat(nobs))
allocate(model_out % grid % lon(nobs))
model_out % grid % lat = lat_sim
model_out % grid % lon = lon_sim

end subroutine radsim_interp
