program lmlef
!=======================================================================
!
! [PURPOSE:] Main program of MLEF with observation space localization
!
! [HISTORY:]
!   12/14/2022 created for MSM
!
!=======================================================================
!$USE OMP_LIB
  use kind_module
  use co_module
  use nml_module
  use rsmcom_module
  use corsm_module
  use obs_module, only: write_obsout
  use mlef_module, only: mlef_init
  use lmlef_tools, only: init_das_lmlef, das_lmlefy
  use lmlef_obs, only: nbslot, nslots, &
  & set_lmlef_obs, obsdasort
!  &, monit_cntl, monit_mean

  implicit none
  real(kind=dp),allocatable :: gues3dc(:,:,:)[:]  !control
  real(kind=dp),allocatable :: gues2dc(:,:)[:]    !control
  real(kind=dp),allocatable :: anal3dc(:,:,:)[:]  !control
  real(kind=dp),allocatable :: anal2dc(:,:)[:]    !control
  real(kind=dp),allocatable :: gues3d(:,:,:,:)[:] !ensemble
  real(kind=dp),allocatable :: gues2d(:,:,:)[:]   !ensemble
  real(kind=dp),allocatable :: anal3d(:,:,:,:)[:] !ensemble
  real(kind=dp),allocatable :: anal2d(:,:,:)[:]   !ensemble
  real(kind=dp) :: rtimer00,rtimer
  integer :: im,ierr
  character(8) :: stdoutf='NOUT-000'
  character(filelenmax) :: guesf,analf,obsf
  integer :: ltype=1 ! 0=CW, 1=Y
!  NAMELIST /NAMLST_LMLEF/ ltype
!-----------------------------------------------------------------------
! Initial settings
!-----------------------------------------------------------------------
  call cpu_time(rtimer00)
  call initialize_co
  open(5,file='STDIN')
!
  write(stdoutf(6:8), '(I3.3)') myimage
  write(6,'(3A,I3.3)') 'STDOUT goes to ',stdoutf,' for MYIMAGE ', myimage
  open(6,FILE=stdoutf)
  write(6,'(A,I3.3,2A)') 'MYIMAGE=',myimage,', STDOUTF=',stdoutf
!
! read namelist
!
  call read_nml_ens
  call read_nml_obsope
!  call mlef_init
!  call read_nml_lmlef
  call init_das_lmlef
!
  write(6,'(A)') '============================================='
  write(6,'(A)') ' LOCAL MAXIMUM LIKELIHOOD ENSEMBLE FILTERING '
  write(6,'(A)') '                                             '
  write(6,'(A)') '   LL      M    M  LL      EEEEEE  FFFFFF    '
  write(6,'(A)') '   LL      MM  MM  LL      EE      FF        '
  write(6,'(A)') '   LL      M MM M  LL      EEEEEE  FFFFF     '
  write(6,'(A)') '   LL      M    M  LL      EE      FF        '
  write(6,'(A)') '   LLLLLL  M    M  LLLLLL  EEEEEE  FF        '
  write(6,'(A)') '                                             '
  write(6,'(A)') '             WITHOUT LOCAL PATCH             '
  write(6,'(A)') '                                             '
  write(6,'(A)') '          Coded by Saori Nakashita           '
  write(6,'(A)') ' Based on Zupanski (2005) and Zupanski (2021)'
  !write(6,'(A)') '  Tested by Miyoshi and Yamane (2006)        '
!  if (ltype==0) then
!  write(6,'(A)') '       Local cost function formulation       '
!  else
  write(6,'(A)') '           and Yokota et al. (2016)          '
  write(6,'(A)') '          Local gradient formulation         '
!  endif
  write(6,'(A)') '============================================='
  write(6,'(A)') '             LMLEF  PARAMETERS               '
  write(6,'(A)') ' ------------------------------------------- '
  write(6,'(A,I15)')   '   member     :',member
  write(6,'(A,I15)')   '   nslots     :',nslots
  write(6,'(A,I15)')   '   nbslot     :',nbslot
  write(6,'(A,F15.2)') '   sigma_obs  :',sigma_obs
  write(6,'(A,F15.2)') '   sigma_obsv :',sigma_obsv
  write(6,'(A,F15.2)') '   sigma_obst :',sigma_obst
  write(6,'(A)') '============================================='
!
! initial setting
!
  call file_member_replace(0,gues_in_basename,guesf)
  call set_rsmparm(guesf)
  call set_corsm
  allocate(gues3dc(nij1max,nlev,nv3d)[*])
  allocate(gues2dc(nij1max,     nv2d)[*])
  allocate(anal3dc(nij1max,nlev,nv3d)[*])
  allocate(anal2dc(nij1max,     nv2d)[*])
  allocate(gues3d(nij1max,nlev,member,nv3d)[*])
  allocate(gues2d(nij1max,     member,nv2d)[*])
  allocate(anal3d(nij1max,nlev,member,nv3d)[*])
  allocate(anal2d(nij1max,     member,nv2d)[*])
  sync all
!
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
  sync all
!-----------------------------------------------------------------------
! Observations
!-----------------------------------------------------------------------
  !
  ! CONVENTIONAL OBS
  !
  call set_lmlef_obs
!
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(READ_OBS):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
!-----------------------------------------------------------------------
! First guess ensemble
!-----------------------------------------------------------------------
  !
  ! READ GUES
  !
  sync all
  if(.not.mean) THEN
    call file_member_replace(0,gues_in_basename,guesf)
    call read_cntl(guesf,gues3dc,gues2dc)
    sync all
  else
    gues3dc = 0.0d0
    gues2dc = 0.0d0
  end if
  call read_ens(gues_in_basename,gues3d,gues2d)
  !
  ! write ENS MEAN and SPRD
  !
  call write_ensmspr(gues_in_basename,gues3d,gues2d)
  sync all
!
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(READ_GUES):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
!-----------------------------------------------------------------------
! Data Assimilation
!-----------------------------------------------------------------------
  !
  ! LMLEF
  !
  !sync all
!  if(ltype==0) then
!    call das_lmlefcw(gues3dc,gues2dc,gues3d,gues2d,anal3dc,anal2dc,anal3d,anal2d)
!  else
   call das_lmlefy (gues3dc,gues2dc,gues3d,gues2d,anal3dc,anal2dc,anal3d,anal2d)
!  end if
!
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(DAS_LMLEF):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
!-----------------------------------------------------------------------
! Analysis ensemble
!-----------------------------------------------------------------------
  !
  ! write analysis
  !
  sync all
  if(.not.mean) then
    call file_member_replace(0,anal_out_basename,analf)
    call write_cntl(analf,anal3dc,anal2dc)
    sync all
  END if
  call write_ens(anal_out_basename,anal3d,anal2d)
  !
  ! write ensemble mean and spread
  !
  call write_ensmspr(anal_out_basename,anal3d,anal2d)
  sync all
  !
  ! (optional) write y-H(xa)
  !
  if(obsanal_output) then
    ! departure => raw values
    obsdasort%hxf(:) = obsdasort%dat(:) - obsdasort%hxf(:)
    if(.not.mean) then
      call file_member_replace(0,obsda_out_basename,obsf)
    else
      call file_member_replace(member+1,obsda_out_basename,obsf)
    end if
    call write_obsout(obsf,obsdasort,0)
    do im=1,member
      obsdasort%hxe(im,:) = obsdasort%hxe(im,:) + obsdasort%hxf(:)
      call file_member_replace(im,obsda_out_basename,obsf)
      call write_obsout(obsf,obsdasort,im)
    end do
  end if
!
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(write_anal):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

!!-----------------------------------------------------------------------
!! Monitor
!!-----------------------------------------------------------------------
!  if(mean) THEN
!    call monit_mean('gues')
!    call monit_mean('anal')
!  else
!    call monit_cntl(guesf)
!    call monit_cntl('anal')
!  end if
!
!  call cpu_time(rtimer)
!  write(6,'(A,2F10.2)') '### TIMER(MONIT_CNTL):',rtimer,rtimer-rtimer00
!  rtimer00=rtimer
!-----------------------------------------------------------------------
! Finalize
!-----------------------------------------------------------------------
  sync all

  close(5)
!  STOP
end program lmlef
