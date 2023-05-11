program obsope
!
! observation operator serial program
! history:
! 22-12-09 SN create
!
  use kind_module
  use nml_module
  use rsmcom_module
  use obs_module, only : nobstype, nqctype, obstype, obstype2, &
   &  obsin_allocate, get_nobs, read_obs, monit_obsin
  use obsope_module, only: obsope_serial, monit_dep, monit_print
  use lmlef_tools, only: init_das_lmlef
  implicit none
  type(obstype), allocatable :: obs(:)
  type(obstype2) :: obsout
  character(len=filelenmax) :: guesf
! monitor
  real(kind=dp), allocatable :: dep(:)
  integer :: nobs(nobstype)
  real(kind=dp) :: bias(nobstype)
  real(kind=dp) :: rmse(nobstype)
  integer :: nqc(nobstype,nqctype)
  integer :: iof,im,n
! timer
  real(kind=dp) :: rtimer, rtimer00

  call cpu_time(rtimer00)
! initialize
  open(5,file='STDIN')
  call read_nml_ens
  call read_nml_obsope
  call read_nml_lmlef
  call file_member_replace(0,fguess_basename,guesf)
  call set_rsmparm(guesf)
  call init_das_lmlef
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
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
  call obsope_serial(obs,obsout)
!  write(6,'(a,i8)') 'obsope #',obsout%nobs
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(OBSOPE):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

! monitor
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
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(MONITOR):',rtimer,rtimer-rtimer00
  rtimer00=rtimer
! finalize
  call clean_rsmparm

end program obsope
