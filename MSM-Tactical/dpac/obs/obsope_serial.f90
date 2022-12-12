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

! initialize
  call read_nml_ens
  call read_nml_obsope
  call file_member_replace(0,fguess_basename,guesf)
  call set_rsmparm(guesf)

! read observation
  allocate( obs(obsin_num) )
  do iof=1,obsin_num
    call get_nobs(obsin_name(iof),7,obs(iof)%nobs)
    call obsin_allocate(obs(iof))
    call read_obs(obsin_name(iof),obs(iof))
    !write(6,'(3a,i8)') 'reading obs ',trim(obsin_name(iof)),' #',obs(iof)%nobs
    call monit_obsin(obs(iof)%nobs,obs(iof)%elem,obs(iof)%dat)
  end do

! observation operator
  call obsope_serial(obs,obsout)
!  write(6,'(a,i8)') 'obsope #',obsout%nobs

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
! finalize
  call clean_rsmparm

end program obsope
