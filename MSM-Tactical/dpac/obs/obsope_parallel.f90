program obsope
!
! observation operator parallel program
! history:
! 22-12-09 SN create
!
  use kind_module
  use co_module
  use nml_module
  use rsmcom_module
  use corsm_module
  use obs_module, only : nobstype, nqctype, obstype, obstype2, &
   &  obsin_allocate, get_nobs, read_obs, monit_obsin
  use obsope_module, only: obsope_parallel, monit_dep, monit_print
  implicit none
  type(obstype), allocatable :: obs(:)
  type(obstype2) :: obsout
  character(len=filelenmax) :: guesf
  real(kind=dp),allocatable :: gues3dc(:,:,:,:)[:]  !control
  real(kind=dp),allocatable :: gues2dc(:,:,:)[:]    !control
  real(kind=dp),allocatable :: gues3d(:,:,:,:,:)[:] !ensemble
  real(kind=dp),allocatable :: gues2d(:,:,:,:)[:]   !ensemble
! monitor
  real(kind=dp), allocatable :: dep(:)
  integer :: nobs(nobstype)
  real(kind=dp) :: bias(nobstype)
  real(kind=dp) :: rmse(nobstype)
  integer :: nqc(nobstype,nqctype)
  integer :: iof,im,n
! timer
  real(kind=dp) :: rtimer, rtimer00
! stdout
  character(8) :: stdoutf='NOUT-000'

  call cpu_time(rtimer00)
! initialize
  call initialize_co
  open(5,file='STDIN')
!
  write(stdoutf(6:8), '(I3.3)') myimage
  write(6,'(3A,I3.3)') 'STDOUT goes to ',stdoutf,' for MYIMAGE ', myimage
  open(6,FILE=stdoutf)
  write(6,'(A,I3.3,2A)') 'MYIMAGE=',myimage,', STDOUTF=',stdoutf
!
  call read_nml_ens
  call read_nml_obsope
  call read_nml_corsm
  call read_nml_lmlef
  call file_member_replace(0,fguess_basename,guesf)
  call set_rsmparm(guesf)
  call set_corsm
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

  allocate(gues3dc(0:ni1max+1,0:nj1max+1,nlev,nv3d)[*])
  allocate(gues2dc(0:ni1max+1,0:nj1max+1,     nv2d)[*])
  allocate(gues3d(0:ni1max+1,0:nj1max+1,nlev,member,nv3d)[*])
  allocate(gues2d(0:ni1max+1,0:nj1max+1,     member,nv2d)[*])
! read first guess
  if(.not.mean) THEN
    call file_member_replace(0,gues_in_basename,guesf)
    call read_cntl(guesf,gues3dc,gues2dc)
    sync all
  else
    gues3dc = 0.0d0
    gues2dc = 0.0d0
  end if
  call read_ens(gues_in_basename,gues3d,gues2d)
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(READ_GUES):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
! read observation
  allocate( obs(obsin_num) )
  do iof=1,obsin_num
    call get_nobs(obsin_name(iof),6,obs(iof)%nobs)
    call obsin_allocate(obs(iof))
    call read_obs(obsin_name(iof),obs(iof))
    !write(6,'(3a,i8)') 'reading obs ',trim(obsin_name(iof)),' #',obs(iof)%nobs
    call monit_obsin(obs(iof)%nobs,obs(iof)%elem,obs(iof)%dat)
  end do
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(READ_OBS):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

! observation operator
  call obsope_parallel(obs,obsout,gues3dc,gues2dc,gues3d,gues2d)
!  write(6,'(a,i8)') 'obsope #',obsout%nobs
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(OBSOPE):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

! monitor
  if(myimage.eq.1) then
  allocate( dep(obsout%nobs) )
  do im=0,member
    write(6,'(a,i4)') 'member ',im
    do n=1,obsout%nobs
      if(im.eq.0) then
        dep(n) = obsout%dat(n) - obsout%hxf(n)
      else
        dep(n) = obsout%dat(n) - obsout%hxe(im,n)
      end if
    end do
    call monit_dep(obsout%nobs,obsout%elem,dep,obsout%qc,&
     &  nobs,bias,rmse,nqc)
    call monit_print(nobs,bias,rmse,nqc)
  end do
  end if
  sync all
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(MONITOR):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
! finalize
  call clean_rsmparm

end program obsope
