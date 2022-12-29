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
  use obs_module, only: get_nobs, read_obs, write_obsout, monit_obsin, &
          obsin_allocate
  use obsope_module, only: obsope_serial,obsope_parallel
  use mlef_module, only: mlef_init
  use lmlef_tools, only: init_das_lmlef, das_lmlefy
  use lmlef_obs, only: nbslot, nslots, &
  & set_lmlef_obs, obs, obsda, obsdasort, nobs_ext
!  &, monit_cntl, monit_mean

  implicit none
  real(kind=dp),allocatable :: gues3dc(:,:,:,:)[:]  !control
  real(kind=dp),allocatable :: gues2dc(:,:,:)[:]    !control
  real(kind=dp),allocatable :: anal3dc(:,:,:,:)[:]  !control
  real(kind=dp),allocatable :: anal2dc(:,:,:)[:]    !control
  real(kind=dp),allocatable :: gues3d(:,:,:,:,:)[:] !ensemble
  real(kind=dp),allocatable :: gues2d(:,:,:,:)[:]   !ensemble
  real(kind=dp),allocatable :: anal3d(:,:,:,:,:)[:] !ensemble
  real(kind=dp),allocatable :: anal2d(:,:,:,:)[:]   !ensemble
  real(kind=dp) :: rtimer00,rtimer
  integer :: im,iof,ierr
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
  call mlef_init
  call read_nml_lmlef
!
! initial setting
!
  if(.not.mean) then
    im=0
  else
    im=1
  end if
  call file_member_replace(im,gues_in_basename,guesf)
  call set_rsmparm(guesf)
  call set_corsm
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

  allocate(gues3dc(0:ni1max+1,0:nj1max+1,nlev,nv3d)[*])
  allocate(gues2dc(0:ni1max+1,0:nj1max+1,     nv2d)[*])
  allocate(anal3dc(0:ni1max+1,0:nj1max+1,nlev,nv3d)[*])
  allocate(anal2dc(0:ni1max+1,0:nj1max+1,     nv2d)[*])
  allocate(gues3d(0:ni1max+1,0:nj1max+1,nlev,member,nv3d)[*])
  allocate(gues2d(0:ni1max+1,0:nj1max+1,     member,nv2d)[*])
  allocate(anal3d(0:ni1max+1,0:nj1max+1,nlev,member,nv3d)[*])
  allocate(anal2d(0:ni1max+1,0:nj1max+1,     member,nv2d)[*])
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
  ! read observation
  !
  allocate( obs(obsin_num) )
  do iof=1,obsin_num
    call get_nobs(obsin_name(iof),6,obs(iof)%nobs)
    call obsin_allocate(obs(iof))
    call read_obs(obsin_name(iof),obs(iof))
    call monit_obsin(obs(iof)%nobs,obs(iof)%elem,obs(iof)%dat)
  end do
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(READ_OBS):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
  !
  ! observation operator
  !
  if(obsda_in) then
    ! get the number of externally processed observations
    ! assuming all member have the same number of observations
    im = mod(myimage,member)
    call file_member_replace(im,obsda_in_basename,obsf)
    write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is reading a file ',obsf
    call get_nobs(obsf,9,nobs_ext)
  else
    nobs_ext=0
  end if
  ! apply observation operator with additional space for externally processed observations
  obsda%nobs = nobs_ext
  if(single_obs) then !single observation option is valid only for serial operator
    call obsope_serial(obs,obsda)
  else
    call obsope_parallel(obs,obsda)
  end if
  sync all
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(OBSOPE):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
  !
  ! process observation data
  !
  call set_lmlef_obs
!
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(SET_OBS):',rtimer,rtimer-rtimer00
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
