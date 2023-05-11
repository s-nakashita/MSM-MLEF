program calctesprd
!
! calculate full or perturbation moist total energy
!
  use kind_module
  use phconst_module
  use read_module
  use write_module
  use norm_module, only: calc_tegrd, calc_teprof
  use obs_module, only: ndate
  implicit none
  ! ensemble size
  integer :: nens=10
  character(len=10),parameter :: file_basename='r_LEV.@@@@'
  character(len=10) :: filename
  real(kind=dp) :: epsq=1.0d0 ! weight for moist term
  real(kind=dp) :: lonw=-999.9d0, lone=-999.9d0 ! calculation region
  real(kind=dp) :: lats=-999.9d0, latn=-999.9d0 ! calculation region
  integer :: kmax=21 ! upper limit for energy calculation
  logical :: wprof=.true. ! whether write out total energy profile or not
  namelist /namlst_prtb/ nens, epsq, lonw, lone, lats, latn, kmax, wprof
  integer :: ilonw, ilone, jlats, jlatn ! calculation region indexes
  integer :: nlon, nlat
  ! for energy calculation
  real(kind=dp), parameter :: p0=1000.0d2, ptheta=rd/cp ! potential temperature
  real(kind=dp), allocatable :: u(:,:,:),v(:,:,:),t(:,:,:),q(:,:,:)
  real(kind=dp), allocatable :: theta(:,:,:),fact(:,:,:)
  real(kind=dp), allocatable :: ps(:,:)
  real(kind=dp), allocatable :: up(:,:,:),vp(:,:,:),tp(:,:,:),qp(:,:,:)
  real(kind=dp), allocatable :: thp(:,:,:)
  real(kind=dp), allocatable :: psp(:,:)
  real(kind=dp) :: area,coef
  real(kind=dp), allocatable :: tegrd(:,:,:,:), tesprd(:,:,:,:)
  real(kind=dp), allocatable :: vwgt(:), teprof(:,:), teprofs(:,:)
  real(kind=sp), allocatable :: buf4(:,:)
  integer :: irec
  integer :: ips,it,iu,iv,iq
  character(len=10) :: ofile='teprof.dat'
  character(len=6) :: ogfile='te.grd'
  ! input files' units (initial, plus 1 for 12h forecast)
  integer, parameter :: nisig=11, nisfc=21, niflx=31
  integer            :: nsig,     nsfc,     nflx
  real(kind=dp), allocatable :: dfld(:,:,:)
  real(kind=dp), allocatable :: mapf(:,:,:), clat(:), clon(:), slmsk(:,:)
  character(len=8) :: label(4)
  integer :: idate(4), nfldsig
  integer :: idate2(4), iymdh, iymdh2
  ! for ndate
  integer :: date1(5),date2(5),dtmin
  real(kind=sp) :: fhour, ext(nwext) 
  real(kind=sp) :: fhour2
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=dp) :: rdelx, rdely, rtruth, rorient, rproj
  integer :: ids(255), iparam(nfldflx)
  integer :: igrd1, jgrd1, levs, nonhyd, icld
  integer :: n,i,j,k

  read(5,namlst_prtb)
  write(6,namlst_prtb)
  
  nsig=nisig
  filename=file_basename
  write(filename(3:5),'(a3)') 'sig'
  write(filename(7:10),'(a4)') 'mean'
  write(6,*) 'open file ',filename
  open(nsig,file=filename,access='sequential',form='unformatted',action='read')
  ! headers are assumed to be identical for all initial time
  icld=1
  call read_header(nsig,icld,label,idate,fhour,si,sl,ext,nfldsig)
  print*, label
  print*, idate
  print*, fhour
  dtmin=nint(fhour)*60
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
  iymdh = idate(4)*1000000+idate(2)*10000+idate(3)*100+idate(1)
  !print*, ext(1:16)
!  print*, nfldsig
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
!  print*, si(1:levs+1)
!  print*, sl(1:levs)
  allocate( dfld(igrd1,jgrd1,nfldsig) )
  allocate( mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1), clon(igrd1) )
  
  ips=2
  if(nonhyd.eq.1) then 
    it=2+7*levs
  else
    it=3
  end if
  iu=3+levs
  iv=iu+levs
  iq=iv+levs
  ! mean
!  call read_sig(nsig,igrd1,jgrd1,levs,nfldsig,nonhyd,fhour,sl,dfld,mapf,clat,clon)
  call read_sig(nsig,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,0.0,sl,dfld,mapf,clat,clon)
  close(nsig)
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
  allocate( u(nlon,nlat,kmax),v(nlon,nlat,kmax) )
  allocate( t(nlon,nlat,kmax),q(nlon,nlat,kmax) )
  allocate( theta(nlon,nlat,kmax),fact(nlon,nlat,kmax) )
  allocate( ps(nlon,nlat) )
  u=0.0d0
  v=0.0d0
  t=0.0d0
  q=0.0d0
  ps=0.0d0

  do j=1,nlat
    do i=1,nlon
      ps(i,j)=dfld(i+ilonw-1,j+jlats-1,ips)
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        t(i,j,k) = dfld(i+ilonw-1,j+jlats-1,it+k-1)
        theta(i,j,k) = t(i,j,k)*(p0/dfld(i+ilonw-1,j+jlats-1,ips)/sl(k))**ptheta
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        u(i,j,k) = dfld(i+ilonw-1,j+jlats-1,iu+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        v(i,j,k) = dfld(i+ilonw-1,j+jlats-1,iv+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        q(i,j,k) = dfld(i+ilonw-1,j+jlats-1,iq+k-1)
      end do
    end do
  end do
  do k=1,kmax
    print*, k
    print *, 'u(full,max)', maxval(u(:,:,k)),maxloc(u(:,:,k))
    print *, 'u(full,min)', minval(u(:,:,k)),minloc(u(:,:,k))
    print *, 'v(full,max)', maxval(v(:,:,k)),maxloc(v(:,:,k))
    print *, 'v(full,min)', minval(v(:,:,k)),minloc(v(:,:,k))
    print *, 't(full,max)', maxval(t(:,:,k)),maxloc(t(:,:,k))
    print *, 't(full,min)', minval(t(:,:,k)),minloc(t(:,:,k))
!    print *, 'th(full,max)', maxval(theta(:,:,k)),maxloc(theta(:,:,k))
!    print *, 'th(full,min)', minval(theta(:,:,k)),minloc(theta(:,:,k))
    print *, 'q(full,max)', maxval(q(:,:,k)),maxloc(q(:,:,k))
    print *, 'q(full,min)', minval(q(:,:,k)),minloc(q(:,:,k))
  end do
  print *, 'ps(full,max)', maxval(ps(:,:)),maxloc(ps(:,:))
  print *, 'ps(full,min)', minval(ps(:,:)),minloc(ps(:,:))

  allocate( up(nlon,nlat,kmax),vp(nlon,nlat,kmax) )
  allocate( tp(nlon,nlat,kmax),qp(nlon,nlat,kmax) )
  allocate( thp(nlon,nlat,kmax) )
  allocate( psp(nlon,nlat) )
  allocate( tegrd(nlon,nlat,kmax,4),tesprd(nlon,nlat,kmax,4) )
  allocate( vwgt(kmax), teprof(kmax,4), teprofs(kmax,4) )
  up=0.0d0
  vp=0.0d0
  tp=0.0d0
  qp=0.0d0
  psp=0.0d0
  tesprd=0.0d0
  teprofs=0.0d0
  do n=1,nens
    filename=file_basename
    write(filename(3:5),'(a3)') 'sig'
    write(filename(7:10),'(i4.4)') n
    write(6,*) 'open file ',filename
    open(nsig,file=filename,access='sequential',form='unformatted',action='read')
    !! consistency check
    call read_header(nsig,icld,label,idate2,fhour2,si,sl,ext,nfldsig)
    dtmin=nint(fhour2)*60
    date1(1)=idate2(4)
    date1(2)=idate2(2)
    date1(3)=idate2(3)
    date1(4)=idate2(1)
    date1(5)=0
    call ndate(date1,dtmin,date2)
    idate2(4)=date2(1)
    idate2(2)=date2(2)
    idate2(3)=date2(3)
    idate2(1)=date2(4)
    iymdh2 = idate2(4)*1000000+idate2(2)*10000+idate2(3)*100+idate2(1)
    if (iymdh.ne.iymdh2) then
      print *, 'valid dates are different, ',iymdh,' ',iymdh2
      stop 99
    end if
!  call read_sig(nsig,igrd1,jgrd1,levs,nfldsig,nonhyd,fhour,sl,dfld,mapf,clat,clon)
    call read_sig(nsig,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,0.0,sl,dfld,mapf,clat,clon)
    close(nsig)
    print*, n, maxval(dfld(:,:,3)),minval(dfld(:,:,3))
    do j=1,nlat
      do i=1,nlon
       psp(i,j)=ps(i,j)-dfld(i+ilonw-1,j+jlats-1,ips)
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          tp(i,j,k) = t(i,j,k)-dfld(i+ilonw-1,j+jlats-1,it+k-1)
          !tp(i,j,k) = dfld(i+ilonw-1,j+jlats-1,it+k-1)
          !thp(i,j,k) = theta(i,j,k) &
          !        - tp(i,j,k)*(p0/dfld(i+ilonw-1,j+jlats-1,ips)/sl(k))**ptheta
        end do
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          up(i,j,k) = u(i,j,k)-dfld(i+ilonw-1,j+jlats-1,iu+k-1)
        end do
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          vp(i,j,k) = v(i,j,k)-dfld(i+ilonw-1,j+jlats-1,iv+k-1)
        end do
      end do
    end do
    do k=1,kmax
      do j=1,nlat
        do i=1,nlon
          qp(i,j,k) = q(i,j,k)-dfld(i+ilonw-1,j+jlats-1,iq+k-1)
        end do
      end do
    end do
    print *, 'member=',n
    do k=1,kmax
      print*, k
      print *, 'u(prtb,max)', maxval(up(:,:,k)),maxloc(up(:,:,k))
      print *, 'u(prtb,min)', minval(up(:,:,k)),minloc(up(:,:,k))
      print *, 'v(prtb,max)', maxval(vp(:,:,k)),maxloc(vp(:,:,k))
      print *, 'v(prtb,min)', minval(vp(:,:,k)),minloc(vp(:,:,k))
      print *, 't(prtb,max)', maxval(tp(:,:,k)),maxloc(tp(:,:,k))
      print *, 't(prtb,min)', minval(tp(:,:,k)),minloc(tp(:,:,k))
!      print *, 'th(prtb,max)', maxval(thp(:,:,k)),maxloc(thp(:,:,k))
!      print *, 'th(prtb,min)', minval(thp(:,:,k)),minloc(thp(:,:,k))
      print *, 'q(prtb,max)', maxval(qp(:,:,k)),maxloc(qp(:,:,k))
      print *, 'q(prtb,min)', minval(qp(:,:,k)),minloc(qp(:,:,k))
    end do
    print *, 'ps(prtb,max)', maxval(psp(:,:)),maxloc(psp(:,:))
    print *, 'ps(prtb,min)', minval(psp(:,:)),minloc(psp(:,:))
    ! calculate energy for each grid
!    call calc_tegrd(up,vp,thp,qp,psp,epsq,clat(jlats:jlatn),&
    call calc_tegrd(up,vp,tp,qp,psp,epsq,clat(jlats:jlatn),&
            si,nlon,nlat,kmax,tegrd)
    tesprd = tesprd + tegrd
    if(wprof) then
      teprof(:,:) = 0.0d0
!      call calc_teprof(up,vp,thp,qp,psp,epsq,clat(jlats:jlatn),&
      call calc_teprof(up,vp,tp,qp,psp,epsq,clat(jlats:jlatn),&
            si,nlon,nlat,kmax,vwgt,teprof)
      teprofs = teprofs + teprof
    end if
  end do
  tesprd = tesprd / real(nens-1,kind=dp)
  teprofs = teprofs / real(nens-1,kind=dp)
  allocate( buf4(nlon,nlat) )
  open(55,file=ogfile,form='unformatted',access='direct',recl=4*nlon*nlat)
  irec=1
  !pe(ps)
  buf4 = real(tesprd(:,:,1,4),kind=sp)
  write(55,rec=irec) buf4
  irec=irec+1
  !ke
  do k=1,kmax
    buf4 = real(tesprd(:,:,k,1),kind=sp)
    write(55,rec=irec) buf4
    irec=irec+1
  end do
  !pe(t)
  do k=1,kmax
    buf4 = real(tesprd(:,:,k,2),kind=sp)
    write(55,rec=irec) buf4
    irec=irec+1
  end do
  !lh
  do k=1,kmax
    buf4 = real(tesprd(:,:,k,3),kind=sp)
    write(55,rec=irec) buf4
    irec=irec+1
  end do
  close(55)
  if(wprof) then
    open(55,file=ofile)
    write(55,'(A1,A12,5A13)') '#','vwgt','ke','pe(t)','lh','pe(ps)','sum'
    do k=1,kmax
      write(55,'(6F13.5)') vwgt(k),&
        teprofs(k,1),teprofs(k,2),teprofs(k,3),teprofs(k,4),sum(teprofs(k,:))
    end do
    close(55)
    deallocate( vwgt, teprof, teprofs )
  end if
  deallocate( dfld,u,v,t,q,ps,theta,fact ) 
end program
