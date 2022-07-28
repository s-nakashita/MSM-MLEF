program test_read
  use read_module
  use write_module
  implicit none
  integer, parameter :: nsig=11, nsfc=12, nflx=13
  integer, parameter :: nosig=51, nosfc=52, noflx=53
  real(kind=4), allocatable :: dfld(:,:,:)
  real(kind=4), allocatable :: mapf(:,:,:), clat(:), clon(:), slmsk(:,:)
  character(len=8) :: label(4)
  integer :: idate(4), nfldsig
  real(kind=4) :: fhour, ext(nwext) 
  real(kind=4) :: zhour
  real(kind=4) :: si(levmax+1), sl(levmax)
  real(kind=4) :: rdelx, rdely, rtruth, rorient, rproj
  integer :: ids(255), iparam(nfldflx)
  integer :: igrd1, jgrd1, levs, nonhyd
  integer :: n

  call read_header(nsig,label,idate,fhour,si,sl,ext,nfldsig)
  print*, label
  print*, idate
  print*, fhour
  print*, ext
  print*, nfldsig
  igrd1 = int(ext(3))
  jgrd1 = int(ext(4))
  print*, igrd1, jgrd1
  levs = int(ext(5))
  print*, levs
  rproj = ext(7)
  rtruth=ext(8); rorient=ext(9)
  rdelx=ext(14); rdely=ext(15)
  nonhyd=int(ext(16))
  print*, nonhyd
  print*, si(1:levs+1)
  print*, sl(1:levs)
  allocate( dfld(igrd1,jgrd1,nfldsig) )
  allocate( mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1), clon(igrd1) )
  call read_sig(nsig,igrd1,jgrd1,levs,nfldsig,nonhyd,fhour,sl,dfld,mapf,clat,clon)
!  print*, 3, maxval(dfld(:,:,3)), minval(dfld(:,:,3))
!  print*, 'clon ', clon
!  print*, 'clat ', clat
  
  call write_sig(nosig,label,idate,fhour,si(1:levs+1),sl(1:levs),ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,dfld,mapf,clat,clon)
  call read_sig(nosig,igrd1,jgrd1,levs,nfldsig,nonhyd,fhour,sl,dfld,mapf,clat,clon)

  deallocate( dfld ) 
  allocate( dfld(igrd1,jgrd1,nfldsfc) )
  allocate( slmsk(igrd1,jgrd1) )
  call read_sfc(nsfc,igrd1,jgrd1,dfld)
!  print*, 1, maxval(dfld(:,:,1)), minval(dfld(:,:,1))
  slmsk = dfld(:,:,16)
  call write_sfc(nosfc,igrd1,jgrd1,dfld,label,idate,fhour)
  call read_sfc(nosfc,igrd1,jgrd1,dfld)
  deallocate( dfld ) 
  allocate( dfld(igrd1,jgrd1,nfldflx) )
  ids(:)=0
  call read_flx(nflx,igrd1,jgrd1,dfld,ids,iparam,fhour,zhour)
  call write_flx(noflx,igrd1,jgrd1,dfld,ids,iparam,slmsk,&
               & idate,fhour,zhour,&
               & rdelx,rdely,clat,clon,rtruth,rorient,rproj)
  call read_flx(noflx,igrd1,jgrd1,dfld,ids,iparam,fhour,zhour)
!!  print*, 1, maxval(dfld(:,:,1)), minval(dfld(:,:,1))
!!  print*, ids
!!  print*, iparam
end program
