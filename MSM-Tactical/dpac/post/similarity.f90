program similarity
!
! calculate similarity index with total energy norm
! reference : Saito et al., (2012, Tellus A, doi: 10.3402/tellusa.v64i0.11594)
!
  use kind_module
  use phconst_module
  use read_module
  use write_module
  use norm_module, only: calc_te2
  implicit none
  integer :: nens=10 ! ensemble member
  real(kind=dp) :: epsq=1.0d0 ! weight for moist term
  real(kind=dp) :: lonw=-999.9d0, lone=-999.9d0 ! calculation region
  real(kind=dp) :: lats=-999.9d0, latn=-999.9d0 ! calculation region
  integer :: kmax=21 ! upper limit for energy calculation
  logical :: lmonit=.false.
  namelist /namlst_siml/ nens, epsq, lonw, lone, lats, latn, kmax, lmonit
  integer :: ilonw, ilone, jlats, jlatn ! calculation region indexes
  integer :: nlon, nlat
  character(len=10),parameter :: file_basename='r_sig.@@@@'
  character(len=10) :: filename
  ! for energy calculation
  integer :: m1, m2 !member index
  real(kind=dp), parameter :: p0=1000.0d2, ptheta=rd/cp ! potential temperature
  real(kind=dp), allocatable :: u1(:,:,:),v1(:,:,:),t1(:,:,:),q1(:,:,:)
  real(kind=dp), allocatable :: theta1(:,:,:)
  real(kind=dp), allocatable :: ps1(:,:)
  real(kind=dp), allocatable :: u2(:,:,:),v2(:,:,:),t2(:,:,:),q2(:,:,:)
  real(kind=dp), allocatable :: theta2(:,:,:)
  real(kind=dp), allocatable :: ps2(:,:)
  real(kind=dp) :: area,coef
  real(kind=dp) :: te1(4),te2(4),te12(4)
  real(kind=dp), allocatable :: simli(:,:) !nens x nens
  integer :: ips,it,iu,iv,iq
  character(len=8) :: ofile='siml.dat'
  character(len=4) :: cmem
  ! input files' units (initial, plus 1 for 12h forecast)
  integer, parameter :: nisig=11, nisfc=21, niflx=31
  integer            :: nsig,     nsfc,     nflx
  real(kind=dp), allocatable :: dfldb(:,:,:), dfldp(:,:,:,:)
  real(kind=dp), allocatable :: mapf(:,:,:), clat(:), clon(:), slmsk(:,:)
  character(len=8) :: label(4)
  integer :: idate(4), nfldsig
  real(kind=sp) :: fhour, ext(nwext) 
  real(kind=sp) :: zhour
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=dp) :: rdelx, rdely, rtruth, rorient, rproj
  integer :: ids(255), iparam(nfldflx)
  integer :: igrd1, jgrd1, levs, nonhyd, icld
  integer :: n,i,j,k

  read(5,namlst_siml)
  write(6,namlst_siml)
  
!!! sigma files (r_sig.fNN)
  ! headers are assumed to be identical for all members
  filename=file_basename
  write(filename(7:10),'(i4.4)') 0
  write(6,*) 'open file ',filename
  open(nisig,file=filename,access='sequential',form='unformatted',action='read')
  call read_header(nisig,icld,label,idate,fhour,si,sl,ext,nfldsig)
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
  allocate( dfldb(igrd1,jgrd1,nfldsig) )
  allocate( dfldp(igrd1,jgrd1,nfldsig,nens) )
  allocate( mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1), clon(igrd1) )
  
  ! read data
  dfldb=0.0d0
  dfldp=0.0d0
  !! control
  call read_sig(nisig,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,dfldb,mapf,clat,clon)
  close(nisig)
  !! setting boundaries
  if ((lonw.gt.-999.9d0).and.(lone.gt.-999.9d0)) then
    do i=1,igrd1
      if(clon(i).ge.lonw) then
        ilonw=i
        exit
      end if
    end do 
    do i=1,igrd1
      if(clon(i).ge.lone) then
        ilone=i
        exit
      end if
    end do 
  else
    ilonw=1
    ilone=igrd1
  end if
  if ((lats.gt.-999.9d0).and.(latn.gt.-999.9d0)) then
    do j=1,jgrd1
      if(clat(j).ge.lats) then
        jlats=j
        exit
      end if
    end do 
    do j=1,jgrd1
      if(clat(j).ge.latn) then
        jlatn=j
        exit
      end if
    end do 
  else
    jlats=1
    jlatn=jgrd1
  end if
  print *, 'boundary ',ilonw,'-',ilone,' lon ',clon(ilonw),'-',clon(ilone)
  print *, 'boundary ',jlats,'-',jlatn,' lat ',clat(jlats),'-',clat(jlatn)
  nlon = ilone - ilonw + 1
  nlat = jlatn - jlats + 1
  print *, 'nlon ',nlon,' nlat ',nlat
  ips=2
  it=3
  iu=it+levs
  iv=iu+levs
  iq=iv+levs
  allocate( u1(nlon,nlat,kmax),v1(nlon,nlat,kmax) )
  allocate( t1(nlon,nlat,kmax),q1(nlon,nlat,kmax) )
  allocate( theta1(nlon,nlat,kmax) )
  allocate( ps1(nlon,nlat) )
  allocate( u2(nlon,nlat,kmax),v2(nlon,nlat,kmax) )
  allocate( t2(nlon,nlat,kmax),q2(nlon,nlat,kmax) )
  allocate( theta2(nlon,nlat,kmax) )
  allocate( ps2(nlon,nlat) )

  !! member
  do n=1,nens
    filename=file_basename
    write(filename(7:10),'(i4.4)') n
    write(6,*) 'open file ',filename
    open(nisig,file=filename,access='sequential',form='unformatted',action='read')
    call read_sig(nisig,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,&
            dfldp(:,:,:,n),mapf,clat,clon)
    close(nisig)
    print*, n, maxval(dfldp(:,:,3,n)),minval(dfldp(:,:,3,n))
  end do
  
  ! start index calculation
  allocate( simli(nens,nens) )
  simli = 0.0d0
  do n=1,nens
    m1=n
    te1(:)=0.0d0
    u1=0.0d0
    v1=0.0d0
    t1=0.0d0
    theta1=0.0d0
    q1=0.0d0
    ps1=0.0d0
    do j=1,nlat
      do i=1,nlon
        ps1(i,j)=dfldp(i+ilonw-1,j+jlats-1,ips,m1)-dfldb(i+ilonw-1,j+jlats-1,ips)
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          t1(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,it+k-1,m1)
          theta1(i,j,k) = t1(i,j,k)*(p0/dfldp(i+ilonw-1,j+jlats-1,ips,m1)/sl(k))**ptheta
          t1(i,j,k) = dfldb(i+ilonw-1,j+jlats-1,it+k-1)
          theta1(i,j,k) = theta1(i,j,k) - &
                  t1(i,j,k)*(p0/dfldb(i+ilonw-1,j+jlats-1,ips)/sl(k))**ptheta
        end do
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          u1(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,iu+k-1,m1)-dfldb(i+ilonw-1,j+jlats-1,iu+k-1)
        end do
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          v1(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,iv+k-1,m1)-dfldb(i+ilonw-1,j+jlats-1,iv+k-1)
        end do
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          q1(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,iq+k-1,m1)-dfldb(i+ilonw-1,j+jlats-1,iq+k-1)
        end do
      end do
    end do
    !! monitor
    if(lmonit) call monitor(u1,v1,theta1,q1,ps1)

    ! calculate energy
    call calc_te2(u1,u1,v1,v1,theta1,theta1,q1,q1,ps1,ps1,&
            epsq,clat(jlats:jlatn),si,nlon,nlat,kmax,te1)
    print *, m1, 'te ', sum(te1)
    do m2=m1,nens
      te2(:)=0.0d0
      u2=0.0d0
      v2=0.0d0
      t2=0.0d0
      theta2=0.0d0
      q2=0.0d0
      ps2=0.0d0
      do j=1,nlat
        do i=1,nlon
          ps2(i,j)=dfldp(i+ilonw-1,j+jlats-1,ips,m2)-dfldb(i+ilonw-1,j+jlats-1,ips)
        end do
      end do
      do k=1,kmax
        do j=1,nlat
          do i=1,nlon
            t2(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,it+k-1,m2)
            theta2(i,j,k) = t2(i,j,k)*(p0/dfldp(i+ilonw-1,j+jlats-1,ips,m2)/sl(k))**ptheta
            t2(i,j,k) = dfldb(i+ilonw-1,j+jlats-1,it+k-1)
            theta2(i,j,k) = theta2(i,j,k) - &
                    t2(i,j,k)*(p0/dfldb(i+ilonw-1,j+jlats-1,ips)/sl(k))**ptheta
          end do
        end do
      end do
      do k=1,kmax
        do j=1,nlat
          do i=1,nlon
            u2(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,iu+k-1,m2)-dfldb(i+ilonw-1,j+jlats-1,iu+k-1)
          end do
        end do
      end do
      do k=1,kmax
        do j=1,nlat
          do i=1,nlon
            v2(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,iv+k-1,m2)-dfldb(i+ilonw-1,j+jlats-1,iv+k-1)
          end do
        end do
      end do
      do k=1,kmax
        do j=1,nlat
          do i=1,nlon
            q2(i,j,k) = dfldp(i+ilonw-1,j+jlats-1,iq+k-1,m2)-dfldb(i+ilonw-1,j+jlats-1,iq+k-1)
          end do
        end do
      end do
      !! monitor
      if(lmonit) call monitor(u2,v2,theta2,q2,ps2)

      ! calculate energy
      call calc_te2(u2,u2,v2,v2,theta2,theta2,q2,q2,ps2,ps2,&
              epsq,clat(jlats:jlatn),si,nlon,nlat,kmax,te2)
      print *, m2, 'te ', sum(te2)

      ! cross
      te12(:)=0.0d0
      ! calculate energy
      call calc_te2(u1,u2,v1,v2,theta1,theta2,q1,q2,ps1,ps2,&
              epsq,clat(jlats:jlatn),si,nlon,nlat,kmax,te12)
      print *, m1, m2, 'te ', sum(te12)
      simli(m1,m2) = sum(te12)/sqrt(sum(te1))/sqrt(sum(te2))
    end do !m2
  end do !m1

  ! write out
  write(cmem,'(i4)') nens
  open(51,file=ofile)
  do n=1,nens
    write(6,'('//trim(cmem)//'f11.6)') simli(n,:)
    write(51,'('//trim(cmem)//'f11.6)') simli(n,:)
  end do
  close(51)
  deallocate( dfldb,dfldp )
  deallocate( u1,v1,t1,q1,ps1,theta1 ) 
  deallocate( u2,v2,t2,q2,ps2,theta2 ) 
contains
  subroutine monitor(u,v,theta,q,ps)
    real(kind=dp), intent(in) :: u(:,:,:),v(:,:,:),theta(:,:,:),q(:,:,:),ps(:,:)
    do k=1,kmax
      print*, k
      print *, 'u(prtb,max)', maxval(u(:,:,k)),maxloc(u(:,:,k))
      print *, 'u(prtb,min)', minval(u(:,:,k)),minloc(u(:,:,k))
      print *, 'v(prtb,max)', maxval(v(:,:,k)),maxloc(v(:,:,k))
      print *, 'v(prtb,min)', minval(v(:,:,k)),minloc(v(:,:,k))
      print *, 'th(prtb,max)', maxval(theta(:,:,k)),maxloc(theta(:,:,k))
      print *, 'th(prtb,min)', minval(theta(:,:,k)),minloc(theta(:,:,k))
      print *, 'q(prtb,max)', maxval(q(:,:,k)),maxloc(q(:,:,k))
      print *, 'q(prtb,min)', minval(q(:,:,k)),minloc(q(:,:,k))
    end do
    print *, 'ps(prtb,max)', maxval(ps(:,:)),maxloc(ps(:,:))
    print *, 'ps(prtb,min)', minval(ps(:,:)),minloc(ps(:,:))
  end subroutine monitor
end program
