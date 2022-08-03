program ensmspr
  use read_module
  use write_module
  implicit none
  ! ensemble size
  integer, parameter :: nens=10
  ! input files' units (first member, increased linearly with members)
  integer, parameter :: nisig=11, nisfc=21, niflx=31
  integer            :: nsig,     nsfc,     nflx
  ! output files' units
  integer, parameter :: nmsig=51, nmsfc=52, nmflx=53 !mean
  integer, parameter :: nssig=54, nssfc=55, nsflx=56 !sprd
  real(kind=4), allocatable :: dfld(:,:,:)
  real(kind=4), allocatable :: dfldm(:,:,:), dflds(:,:,:)
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

!!! sigma files (r_sig.fNN)
  ! headers are assumed to be identical for all members
  call read_header(nisig,label,idate,fhour,si,sl,ext,nfldsig)
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
  allocate( dfldm(igrd1,jgrd1,nfldsig),dflds(igrd1,jgrd1,nfldsig) )
  allocate( mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1), clon(igrd1) )
  
  dfldm=0.0d0
  dflds=0.0d0
  nsig=nisig
  do n=1,nens
    call read_sig(nsig,igrd1,jgrd1,levs,nfldsig,nonhyd,fhour,sl,dfld,mapf,clat,clon)
    print*, n, maxval(dfld(:,:,3)),minval(dfld(:,:,3))
    dfldm = dfldm + dfld
    dflds = dflds + dfld**2
    nsig=nsig+1
  end do
  dfldm = dfldm / nens
  dflds = sqrt( dflds/nens - dfldm**2 )
  ! write output
  call write_sig(nmsig,label,idate,fhour,si(1:levs+1),sl(1:levs),ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,dfldm,mapf,clat,clon)
  call write_sig(nssig,label,idate,fhour,si(1:levs+1),sl(1:levs),ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,dflds,mapf,clat,clon)

  deallocate( dfld, dfldm, dflds ) 

!!! surface files (r_sfc.fNN)
  allocate( dfld(igrd1,jgrd1,nfldsfc) )
  allocate( dfldm(igrd1,jgrd1,nfldsfc), dflds(igrd1,jgrd1,nfldsfc) )
  allocate( slmsk(igrd1,jgrd1) ) !assuming sea-land mask identical for all members
  dfldm=0.0d0
  dflds=0.0d0
  nsfc=nisfc
  do n=1,nens
    call read_sfc(nsfc,igrd1,jgrd1,dfld)
    print*, n, maxval(dfld(:,:,1)), minval(dfld(:,:,1))
    dfldm = dfldm + dfld
    dflds = dflds + dfld**2
    nsfc = nsfc+1
  end do
  dfldm = dfldm / nens
  dflds = sqrt( dflds/nens - dfldm**2 )
  slmsk = dfld(:,:,16)
  ! write output
  call write_sfc(nmsfc,igrd1,jgrd1,dfldm,label,idate,fhour)
  call write_sfc(nssfc,igrd1,jgrd1,dflds,label,idate,fhour)
  deallocate( dfld, dfldm, dflds ) 

!!! flux files (r_flx.fNN)
  allocate( dfld(igrd1,jgrd1,nfldflx) )
  allocate( dfldm(igrd1,jgrd1,nfldflx), dflds(igrd1,jgrd1,nfldflx) )
  dfldm=0.0d0
  dflds=0.0d0
  nflx=niflx
  do n=1,nens
    ids(:)=0
    call read_flx(nflx,igrd1,jgrd1,dfld,ids,iparam,fhour,zhour)
    print*, n, maxval(dfld(:,:,1)), minval(dfld(:,:,1))
    dfldm = dfldm + dfld
    dflds = dflds + dfld**2
    nflx = nflx+1
  end do
  dfldm = dfldm / nens
  dflds = sqrt( dflds/nens - dfldm**2 )
  ! write output
  call write_flx(nmflx,igrd1,jgrd1,dfldm,ids,iparam,slmsk,&
               & idate,fhour,zhour,&
               & rdelx,rdely,clat,clon,rtruth,rorient,rproj)
  call write_flx(nsflx,igrd1,jgrd1,dflds,ids,iparam,slmsk,&
               & idate,fhour,zhour,&
               & rdelx,rdely,clat,clon,rtruth,rorient,rproj)
  deallocate( dfld, dfldm, dflds ) 
end program
