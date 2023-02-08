program replacedate
!
! replace initial date and forecast hours
!
  use kind_module
  use phconst_module
  use rsmcom_module
  use read_module
  use write_module, only: write_sig, write_sfc
  use func_module, only: ndate
  implicit none
  real(kind=sp) :: newfhour=0.0 !new forecast hours
  integer       :: member=10 !ensemble size
  namelist /namlst_replace/ newfhour,member
  ! input files' units (base, prtb)
  character(len=15) :: file_basename='r_.@@@@.LEV.grd'
  character(len=15) :: filename
  integer, parameter :: nisig=11
  integer, parameter :: nisfc=12
  logical :: lexist
  ! output file's unit
  integer :: nosig=51
  integer :: nosfc=52
  real(kind=dp), allocatable :: dfld(:,:,:)
  real(kind=dp), allocatable :: dummapf(:,:,:), dumlat(:), dumlon(:)
!  character(len=8) :: label(4)
!  integer :: idate(4), nfldsig
  real(kind=sp) :: ext(nwext) 
  real(kind=sp) :: dhour
  integer :: ids(255), iparam(nfldflx)
  integer :: nfldsig, levs, km
  real(kind=dp), allocatable :: grid(:,:)
  ! for ndate
  integer :: date1(5),date2(5),dtmin
  !
  integer :: n,i,j,k,l,im

  read(5,namlst_replace)
  write(6,namlst_replace)

!!! get parameters
  filename=file_basename
  write(filename(1:2),'(a2)') 'ri'
  write(filename(4:7),'(i4.4)') 0
  write(filename(9:11),'(a3)') 'sig'
  call set_rsmparm(filename(1:7))
!  icld=1
  levs=nlev
  nfldsig=nflds
  ext(1) = real(iwav1,kind=sp)
  ext(2) = real(jwav1,kind=sp)
  ext(3) = real(igrd1,kind=sp)
  ext(4) = real(jgrd1,kind=sp)
  ext(5) = real(nlev,kind=sp)
  ext(6) = real(2+nlev*4+5,kind=sp)
  ext(7) = real(nproj,kind=sp)
  ext(8) = real(rtruth,kind=sp)
  ext(9) = real(rorient,kind=sp)
  ext(10)= real(rcenlat,kind=sp)
  ext(11)= real(rcenlon,kind=sp)
  ext(12)= real(rlftgrd,kind=sp)
  ext(13)= real(rbtmgrd,kind=sp)
  ext(14)= real(rdelx,kind=sp)
  ext(15)= real(rdely,kind=sp)
  ext(16)= real(nonhyd,kind=sp)
  allocate( dfld(igrd1,jgrd1,nfldsig) )
  allocate( dummapf(igrd1,jgrd1,3) )
  allocate( dumlat(jgrd1), dumlon(igrd1) )

  ! calculate new idate and fhour
  print *,'before ', idate(4),idate(2),idate(3),idate(1),'+',nint(fhour)
  dhour = fhour - newfhour
  dtmin=nint(dhour)*60
  date1(1)=idate(4)
  date1(2)=idate(2)
  date1(3)=idate(3)
  date1(4)=idate(1)
  date1(5)=0
  call ndate(date1,dtmin,date2)
  idate(4)=date2(1)
  idate(2)=date2(2)
  idate(3)=date2(3)
  idate(1)=date2(4)
  print *,'after ', idate(4),idate(2),idate(3),idate(1),'+',nint(newfhour)

  ! member
  do im=0,member
    filename=file_basename
    write(filename(1:2),'(a2)') 'ri'
    write(filename(4:7),'(i4.4)') im
    write(filename(9:11),'(a3)') 'sig'
    write(6,'(2a)') 'input= ',filename
    open(nisig,file=filename,form='unformatted',access='sequential',action='read')
    call read_sig(nisig,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sig,&
    dfld,dummapf,dumlat,dumlon)
    close(nisig)
    filename=file_basename
    write(filename(1:2),'(a2)') 'ro'
    write(filename(4:7),'(i4.4)') im
    write(filename(9:11),'(a3)') 'sig'
    write(6,'(2a)') 'output= ',filename
    open(nosig,file=filename,form='unformatted',access='sequential',action='write')
    call write_sig(nosig,label,idate,newfhour,sigh,sig,ext,&
&                igrd1,jgrd1,levs,nfldsig,nonhyd,icld,dfld,mapf,rlat,rlon)
    close(nosig)
  end do !im=1,member   
  deallocate( dfld )
  !! surface
  allocate( dfld(igrd1,jgrd1,nfldsfc) )
  do im=0,member
    filename=file_basename
    write(filename(1:2),'(a2)') 'ri'
    write(filename(4:7),'(i4.4)') im
    write(filename(9:11),'(a3)') 'sfc'
    write(6,'(2a)') 'input= ',filename
    open(nisfc,file=filename,form='unformatted',access='sequential',action='read')
    call read_sfc(nisfc,igrd1,jgrd1,dfld)
    close(nisfc)
    filename=file_basename
    write(filename(1:2),'(a2)') 'ro'
    write(filename(4:7),'(i4.4)') im
    write(filename(9:11),'(a3)') 'sfc'
    write(6,'(2a)') 'output= ',filename
    open(nosfc,file=filename,form='unformatted',access='sequential',action='write')
    call write_sfc(nosfc,igrd1,jgrd1,dfld,label,idate,newfhour)
    close(nosfc)
  end do
  deallocate( dfld )
end program
