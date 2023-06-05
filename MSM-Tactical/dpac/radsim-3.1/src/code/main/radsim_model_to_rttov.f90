!-------------------------------------------------------------------------------
! Description:
!
!   Transfer model data to RTTOV profiles. A list of index values for the model
!   fields specify which profiles to transfer.
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

subroutine radsim_model_to_rttov( &
  coeffs,      & ! in
  prof_list,   & ! in
  model,       & ! inout
  obs,         & ! inout
  profiles,    & ! inout
  cld_profiles ) ! inout

use parkind1, only : &
  jpim, jprb

use rttov_const, only : &
  qmin, o3min, gas_id_co2, &
  gas_id_watervapour, &
  mair, gas_mass, &
  clw_scheme_deff, &
  ice_scheme_baum, &
  surftype_land, &
  surftype_sea, &
  surftype_seaice, &
  martin_clwde_min, &
  martin_k_sea, &
  martin_ntot_sea, &
  martin_k_land, &
  martin_ntot_land, &
  rho_water, pi, &
  errorstatus_success

use radsim_mod_cfg, only : &
  seaice_threshold, &
  coast_threshold, &
  run_scatt, &
  clw_data, &
  fixed_sun_angles, &
  run_scatt, &
  addsolar, &
  ir_addaerosols_cams, &
  ir_addclouds, &
  ircloud_icede_param, &
  ircloud_ice_scheme, &
  ircloud_clw_scheme, &
  ircloud_use_model_clw_deff, &
  ircloud_use_model_ice_deff, &
  ozone_data, &
  zen_angle, &
  azi_angle, &
  sun_zen_angle, &
  sun_azi_angle, &
  co2_max_ppmv, &
  output_mode

use radsim_mod_constants, only : &
  real64, &
  output_debug, &
  qcinfo_coast, &
  status_error, &
  icon_clip_water, &
  icon_clip_ice

use radsim_mod_process, only : &
  z_direction

use radsim_mod_types, only : &
  model_type, &
  obs_type

use rttov_types, only : &
  rttov_coefs, &
  rttov_profile, &
  rttov_profile_cloud

implicit none

include 'radsim_error_report.interface'
include 'rttov_scale_ref_gas_prof.interface'

type(rttov_coefs),                      intent(in)    :: coeffs
integer,                                intent(in)    :: prof_list(:)
type(model_type),                       intent(inout) :: model
type(obs_type),                         intent(inout) :: obs
type(rttov_profile),                    intent(inout) :: profiles(:)
type(rttov_profile_cloud), allocatable, intent(inout) :: cld_profiles(:)

integer :: i, j
integer(kind=jpim) :: nprofs, nlevels, nhalflevels, err
integer :: level1, layer1, halflevel1
integer :: prof
integer :: lsm, offset, profmap
logical :: fill_top, has_density
real(real64) :: cloud, cfrac, density, k, ntot, clwde, icede
real(real64) :: zsurf, landfrac

!-------------------------------------------------------------------------------

nprofs = size(prof_list)
nlevels = profiles(1) % nlevels
has_density = associated(model % density)

!-------------------------------------------------------------------------------
! 1. Set date and time
!-------------------------------------------------------------------------------

if ( size(model % validity_time, dim=1) == 1 ) then
  do i = 1, nprofs
    profiles(i) % date      = model % validity_time(1,1:3)
    profiles(i) % time(1:2) = model % validity_time(1,4:5)
  end do
else
  do i = 1, nprofs
    prof = prof_list(i)
    profiles(i) % date      = model % validity_time(prof,1:3)
    profiles(i) % time(1:2) = model % validity_time(prof,4:5)
  end do
end if

!-------------------------------------------------------------------------------
! 2. Single-level variables
!-------------------------------------------------------------------------------

do i = 1, nprofs

  prof = prof_list(i)
  profmap = obs % sim_to_obs(prof)

  !-----------------
  ! 2.1 Surface skin
  !-----------------

  ! Surface type. The input LSM is technically a land fraction so take the
  ! nearest integer value. If available, the provided observation value
  ! is preferred over the model value which may have been interpolated.

  if ( associated(obs % lsm) ) then
    landfrac = obs % lsm(profmap)
  else
    landfrac = model % lsm(prof)
  end if

  lsm = nint(landfrac)

  if ( lsm == 1 ) then
    profiles(i) % skin % surftype = surftype_land
  else
    profiles(i) % skin % surftype = surftype_sea
  end if

  ! Set sea points to ice if seaice is over the defined threshold

  if ( associated(model % seaice) ) then
    if ( profiles(i) % skin % surftype == surftype_sea .and. &
         model % seaice(prof) > seaice_threshold .and. &
         model % seaice(prof) /= model %rmdi ) then
      profiles(i) % skin % surftype = surftype_seaice
    end if
  end if

  ! Flag point as coastline if the difference between the derived surface type
  ! and the model land fraction is over the defined threshold.

  landfrac = model % lsm(prof)
  if ( abs(landfrac - lsm) > coast_threshold ) then
    obs % qcinfo(profmap) = ibset(obs % qcinfo(profmap), qcinfo_coast)
  end if

  profiles(i) % skin % watertype = 1 ! 1=Ocean, 0=Freshwater
  profiles(i) % skin % t = model % tskin(prof)

  ! Set snow fraction according to snow depth

  profiles(i) % skin % snow_fraction = 0.
  if ( associated(model % snow_depth) ) then
    if ( model % snow_depth(prof) /= model % rmdi ) then
      profiles(i) % skin % snow_fraction = &
          min(max(model % snow_depth(prof) / 0.1_real64, 0._real64), 1._real64)
    end if
  end if

  ! Set salinity based on watertype

  if ( profiles(i) % skin % watertype == 1 ) then
    profiles(i) % skin % salinity = 35._real64
  else
    profiles(i) % skin % salinity = 0._real64
  endif

  ! Fastem parameters for land/sea-ice are not used as RadSim specifies
  ! fixed land/sea-ice emissivity values.

  !---------------
  ! 2.2 Surface 2m
  !---------------

  profiles(i) % s2m % t     = model % t2(prof)
  profiles(i) % s2m % q     = max(model % q2(prof), qmin)
  profiles(i) % s2m % p     = model % pstar(prof) * 0.01_real64
  profiles(i) % s2m % u     = model % u10(prof)
  profiles(i) % s2m % v     = model % v10(prof)
  profiles(i) % s2m % wfetc = 100000.0_real64

  ! Ozone (s2m % o) ignored as it isn't used in RTTOV

  !--------------------------------------
  ! 2.3 Miscellaneous single-level fields
  !--------------------------------------

  ! Geolocation

  if ( associated(obs % zsurf) ) then
    zsurf = obs % zsurf(profmap)
  else
    zsurf = model % zsurf(prof)
  end if
  profiles(i) % elevation = zsurf / 1000.0_real64
  profiles(i) % latitude  = model % grid % lat(prof)
  if ( model % grid % lon(prof) < 0.0_real64 ) then
    profiles(i) % longitude = model % grid % lon(prof) + 360.0_real64
  else
    profiles(i) % longitude = model % grid % lon(prof)
  end if

  profiles(i) % zenangle = zen_angle
  profiles(i) % azangle  = azi_angle
  if ( associated(obs % satzen)  ) profiles(i) % zenangle = obs % satzen(profmap)
  if ( associated(obs % satazim) ) profiles(i) % azangle  = obs % satazim(profmap)

  profiles(i) % sunzenangle = sun_zen_angle
  profiles(i) % sunazangle  = sun_azi_angle
  if ( addsolar .and. .not. fixed_sun_angles ) then
    if ( associated(obs % solzen)  ) profiles(i) % sunzenangle = obs % solzen(profmap)
    if ( associated(obs % solazim) ) profiles(i) % sunazangle  = obs % solazim(profmap)
  end if

  ! Cloud parameters

  profiles(i) % ctp = 850.0_real64
  profiles(i) % cfraction = 0.0_real64
  if ( ir_addclouds ) then
    profiles(i) % icede_param = ircloud_icede_param
    profiles(i) % ice_scheme = ircloud_ice_scheme
    profiles(i) % clw_scheme = ircloud_clw_scheme
  end if

end do

!-------------------------------------------------------------------------------
! 3. Atmosphere variables
!-------------------------------------------------------------------------------

! nlevels and nlayers already set when initialised

! Use the first profile to determine direction of the model z-axis. In RTTOV
! the 1st level is at the top of the atmosphere (-ve z axis) but input profiles
! are often the other way round so we need to check whether they should be
! inverted.

z_direction = sign(1.0_8, model % p(prof_list(1),1)-model % p(prof_list(1),2))
if ( z_direction < 0 ) then
  level1 = -1
  layer1 = -1
else
  level1 = nlevels
  layer1 = nlevels-1
end if

!-------------------------
! 3.1 Compulsory variables
!-------------------------

! Pressure (hPa from Pa)

do j = 1, nlevels
  do i = 1, nprofs
    prof = prof_list(i)
    profiles(i) % p(abs(level1-j+1)) = model % p(prof,j) * 0.01_real64
  end do
end do

! Temperature (K)

do j = 1, nlevels
  do i = 1, nprofs
    prof = prof_list(i)
    profiles(i) % t(abs(level1-j+1)) = model % t(prof,j)
  end do
end do

! Humidity (kg/kg)
! Note that 'specific' values are the mass ratio of the gas to the total mass
! of air including that gas i.e., moist air. This is gas units option 1 in
! RTTOV. Note that ALL GASES MUST BE IN THE SAME UNITS.

! Some models occasionally give negative values for gas concentrations
! so ensure these are not passed into RTTOV.

do j = 1, nlevels
  do i = 1, nprofs
    prof = prof_list(i)
    profiles(i) % q(abs(level1-j+1)) = max(model % q(prof,j), qmin)
    profiles(i) % gas_units = 1
  end do
end do

!-------------------
! 3.2 Optional gases
!-------------------

! Ozone (kg/kg)

if ( associated(model % o3) .and. ozone_data ) then
  do j = 1, nlevels
    do i = 1, nprofs
      prof = prof_list(i)
      profiles(i) % o3(abs(level1-j+1)) = max(model % o3(prof,j), o3min)
    end do
  end do
end if

! CO2 - RTTOV provides a subroutine to generate scaled copies of the
! reference profile. The p, T and q arrays must already be populated.
! Store the CO2 profiles in the model data so that they are written to the
! output file with other profile data.

if ( co2_max_ppmv > 0. ) then
  call rttov_scale_ref_gas_prof(err, coeffs, profiles, co2_max_ppmv=real(co2_max_ppmv, jprb))
  if (err /= errorstatus_success) then
    call radsim_error_report('Error in rttov_scale_ref_gas_prof', status_error)
  end if

  ! Store the CO2 profile in the model data array
  do i = 1, nprofs
    prof = prof_list(i)
    if ( z_direction < 0 ) then
      model % co2(prof,:) = profiles(i) % co2(:)
    else
      model % co2(prof,:) = profiles(i) % co2(nlevels:1:-1)
    end if
  end do
end if

! n2o, co, ch4, so2 currently ignored

!-------------
! 3.3 Aerosols
!-------------

! CAMS aerosol species amounts (kg/kg)

if ( ir_addaerosols_cams ) then

    profiles(:) % mmr_cldaer = .true.   ! Units kg/kg

    ! Make sure we always use the bottom level as the top level never has
    ! aerosol in it.

    if ( z_direction < 0 ) then
      offset = 1
    else
      offset = 0
    end if

    do j = 1, nlevels-1
      do i = 1, nprofs
        prof = prof_list(i)
  
        profiles(i) % aerosols(:,j) = 0

        if ( associated(model % cams_hphob_bcar) ) &
          profiles(i) % aerosols(1,abs(layer1-j+1)) = model % cams_hphob_bcar(prof,j+offset)
        if ( associated(model % cams_dust1) ) &
          profiles(i) % aerosols(2,abs(layer1-j+1)) = model % cams_dust1(prof,j+offset)
        if ( associated(model % cams_dust2) ) &
          profiles(i) % aerosols(3,abs(layer1-j+1)) = model % cams_dust2(prof,j+offset)
        if ( associated(model % cams_dust3) ) &
          profiles(i) % aerosols(4,abs(layer1-j+1)) = model % cams_dust3(prof,j+offset)
        if ( associated(model % cams_sulphate) ) &
          profiles(i) % aerosols(5,abs(layer1-j+1)) = model % cams_sulphate(prof,j+offset)
        if ( associated(model % cams_sea_salt1) ) &
          profiles(i) % aerosols(6,abs(layer1-j+1)) = model % cams_sea_salt1(prof,j+offset)
        if ( associated(model % cams_sea_salt2) ) &
          profiles(i) % aerosols(7,abs(layer1-j+1)) = model % cams_sea_salt2(prof,j+offset)
        if ( associated(model % cams_sea_salt3) ) &
          profiles(i) % aerosols(8,abs(layer1-j+1)) = model % cams_sea_salt3(prof,j+offset)
        if ( associated(model % cams_hphil_omat) ) &
          profiles(i) % aerosols(9,abs(layer1-j+1)) = model % cams_hphil_omat(prof,j+offset)

      end do
    end do

end if

!-----------
! 3.4 Clouds
!-----------

! See also single-level variables above to set size distribution and crystal shape

if ( .not. run_scatt ) then

  !--------------
  ! 3.4.1 MW only
  !--------------

  if ( associated(model % clw) .and. clw_data ) then
    do j = 1, nlevels
      do i = 1, nprofs
        prof = prof_list(i)
        profiles(i) % clw(abs(level1-j+1)) = max(model % clw(prof,j), 0._jprb)
      end do
    end do
  else
    if ( output_mode >= output_debug ) then
      print '(a)', 'Running MW calculations without CLW data'
    end if
  end if

  !--------------
  ! 3.4.2 IR only
  !--------------

  if ( ir_addclouds ) then

    ! Make sure we always use the bottom level as the top level never has
    ! cloud in it.

    if ( z_direction < 0 ) then
      offset = 1
    else
      offset = 0
    end if

    ! Total cloud fraction

    if ( associated(model % cfrac) ) then
      do j = 1, nlevels-1
        do i = 1, nprofs
          prof = prof_list(i)
          profiles(i) % cfrac(abs(layer1-j+1)) = model % cfrac(prof,j+offset)
        end do
      end do
    end if

    ! Cloud amounts (kg/kg)

    ! Note: RTTOV assumes that the model cloud concentrations are grid box
    !       means and units are kg/kg.

    ! The RTTOV OPAC CLW properties define 5 types of liquid cloud plus ice.
    ! These are:
    ! 1 = Stratiform continental
    ! 2 = Stratiform maritime
    ! 3 = Cumulus continental clean
    ! 4 = Cumulus continental polluted
    ! 5 = Cumulus maritime
    ! 6 = Cirrus (i.e. ice)

    ! The CLW Deff properties define just 1 liquid cloud plus ice.
    ! RTTOV permits the liquid cloud concentration under any of the first 5
    ! indices of the profile cloud array. Ice always goes in the 6th index.

    profiles(:) % mmr_cldaer = .true.   ! Units kg/kg

    ! OPAC stratiform liquid cloud or RTTOV "Deff" liquid cloud properties

    if ( associated(model % clw) ) then
      do j = 1, nlevels-1
        do i = 1, nprofs
          prof = prof_list(i)
          cloud = max(model % clw(prof,j+offset), 0._jprb)
          cfrac = model % cfrac(prof,j+offset)

          ! cfrac = 0.
          ! if ( associated(model % cfrac_liq) ) then
          !   cfrac = model % cfrac_liq(prof,j+offset)
          ! else if ( associated(model % cfrac) ) then
          !   cfrac = model % cfrac(prof,j+offset)
          ! end if

          if ( ircloud_clw_scheme == clw_scheme_deff ) then
            if ( ircloud_use_model_clw_deff .and. cfrac > 0. ) then

              ! Take CLW Deff from model field and apply clipping as done by DWD
              clwde = model % clw_deff(prof,j+offset)
              if ( clwde > 0 .and. clwde < icon_clip_water ) clwde = icon_clip_water
              profiles(i) % clwde(abs(layer1-j+1)) = clwde

              ! If CLW Deff is zero set the cloud to zero
              if ( clwde == 0 ) cloud = 0.

            else if ( has_density .and. cloud > 0. .and. cfrac > 0. ) then

              ! CLW Deff parameterisation from Martin et al (1994) as implemented in RTTOV v13
              ! Use cloud concentration in cloudy fraction of layer
              if ( profiles(i) % skin % surftype == surftype_land ) then
                k = martin_k_land
                ntot = martin_ntot_land
              else
                k = martin_k_sea
                ntot = martin_ntot_sea
              end if
              density = model % density(prof,j+offset)
              clwde = 2._jprb * 1.E6_jprb * &
                (0.75_jprb * (cloud / cfrac) * density / &
                (pi * k * ntot * rho_water))**(1._jprb / 3._jprb)

              if ( clwde < martin_clwde_min ) clwde = martin_clwde_min
              profiles(i) % clwde(abs(layer1-j+1)) = clwde

            else

              ! Use RTTOV CLW Deff parameterisation
              profiles(i) % clwde(abs(layer1-j+1)) = 0.

            end if
          end if

          if ( profiles(i) % skin % surftype == surftype_land ) then
            profiles(i) % cloud(1,abs(layer1-j+1)) = cloud
            profiles(i) % cloud(2,abs(layer1-j+1)) = 0.
          else
            profiles(i) % cloud(2,abs(layer1-j+1)) = cloud
            profiles(i) % cloud(1,abs(layer1-j+1)) = 0.
          end if
        end do
      end do
    end if

    ! Ice cloud

    if ( associated(model % ciw) ) then
      do j = 1, nlevels-1
        do i = 1, nprofs
          prof = prof_list(i)
          cloud = max(model % ciw(prof,j+offset), 0._jprb)
          cfrac = model % cfrac(prof,j+offset)

          if ( ircloud_ice_scheme == ice_scheme_baum ) then
            if ( ircloud_use_model_ice_deff .and. cfrac > 0. ) then

              ! Take ice Deff from model field and apply clipping as done by DWD
              ! Cloud concentration is also scaled for small icede
              icede = model % ciw_deff(prof,j+offset)
              if ( icede > 0 .and. icede < icon_clip_ice ) then
                cloud = cloud * icon_clip_ice / icede
                icede = icon_clip_ice
              end if
              profiles(i) % icede(abs(layer1-j+1)) = icede

              ! If ice Deff is zero set the cloud to zero
              if ( icede == 0 ) cloud = 0.

            else

              ! Use selected RTTOV ice Deff parameterisation
              profiles(i) % icede(abs(layer1-j+1)) = 0.

            end if
          end if

          ! cfrac = 0.
          ! if ( associated(model % cfrac_ice) ) then
          !   cfrac = model % cfrac_ice(prof,j+offset)
          ! else if ( associated(model % cfrac) ) then
          !   cfrac = model % cfrac(prof,j+offset)
          ! end if
          profiles(i) % cloud(6,abs(layer1-j+1)) = cloud
        end do
      end do
    end if

    ! Convective cloud is not output by UM GA6 or IFS
    ! This code needs modifying for UM GA7 (not yet operational)

!     if ( associated(model % conv_cloud) ) then
!       do j = 1, nlevels-1
!         do i = 1, nprofs
!           prof = prof_list(i)
!           cloud = max(model % conv_cloud(prof,j+offset), 0._jprb)
!           if ( profiles(i) % skin % surftype == surftype_land ) then
!             profiles(i) % cloud(3,abs(layer1-j+1)) = cloud
!             profiles(i) % cloud(5,abs(layer1-j+1)) = 0.
!           else
!             profiles(i) % cloud(5,abs(layer1-j+1)) = cloud
!             profiles(i) % cloud(3,abs(layer1-j+1)) = 0.
!           end if
!         end do
!       end do
!     end if

  end if

else

  !-----------------------------
  ! 3.4.3 MW scattering profiles
  !-----------------------------

  ! half-level pressures. In RTTOV, there are more half-levels than levels.

  nhalflevels = size(model % ph, dim=2)
  if ( z_direction < 0 ) then
    halflevel1 = -1
  else
    halflevel1 = nlevels + 1
  end if

  fill_top = .false.
  if ( nhalflevels > nlevels ) then
    offset = 1
  else if ( nhalflevels == nlevels ) then
    if ( minval(model % ph(prof_list(1),:)) > minval(model % p(prof_list(1),:)) ) then
      offset = 1
      fill_top = .true.
    else
      offset = 0
    end if
  else
    offset = 0
    fill_top = .true.
  end if

  do j = 1, nhalflevels
    do i = 1, nprofs
      prof = prof_list(i)
      cld_profiles(i) % ph(abs(halflevel1-j+offset)) = model % ph(prof,j) * 0.01_real64
    end do
  end do

  ! Lowest half-level pressure must be the same as the surface. If the top level
  ! isn't set then fill that with a small value.

  do i = 1, nprofs
    prof = prof_list(i)
    cld_profiles(i) % ph(nlevels+1) = profiles(i) % s2m % p
    if ( fill_top ) cld_profiles(i) % ph(1) = 0.001_real64
  end do

  ! Cloud cover (area fraction)

  do j = 1, nlevels
    do i = 1, nprofs
      prof = prof_list(i)
      cld_profiles(i) % hydro_frac(abs(level1-j+1),1) = model % cfrac(prof,j)
    end do
  end do

  ! RadSim assumes the default hydrotable files which contain 5 hydrometeor types:
  !   rain, snow, graupel (not currently used in RadSim), cloud liquid, cloud ice

  ! CLW (kg/kg)

  do j = 1, nlevels
    do i = 1, nprofs
      prof = prof_list(i)
      cld_profiles(i) % hydro(abs(level1-j+1),4) = max(model % clw(prof,j), 0._jprb)
    end do
  end do

  ! CIW (kg/kg)

  do j = 1, nlevels
    do i = 1, nprofs
      prof = prof_list(i)
      cld_profiles(i) % hydro(abs(level1-j+1),5) = max(model % ciw(prof,j), 0._jprb)
    end do
  end do

  ! Rain (kg/kg); not compulsory

  if ( associated(model % rain) ) then
    do j = 1, nlevels
      do i = 1, nprofs
        prof = prof_list(i)
        cld_profiles(i) % hydro(abs(level1-j+1),1) = max(model % rain(prof,j), 0._jprb)
      end do
    end do
  else
    if ( output_mode >= output_debug ) then
      print '(a)', 'Running scattering code without rain data'
    end if
  end if

  ! Snow (kg/kg); not compulsory

  if ( associated(model % snow) ) then
    do j = 1, nlevels
      do i = 1, nprofs
        prof = prof_list(i)
        cld_profiles(i) % hydro(abs(level1-j+1),2) = max(model % snow(prof,j), 0._jprb)
      end do
    end do
  end if

end if

end subroutine radsim_model_to_rttov
