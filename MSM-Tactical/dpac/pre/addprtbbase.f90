program addprtbbase
!
! add rescaled perturbations to base fields
!
  use kind_module
  use phconst_module
  use rsmcom_module
  use read_module
  use write_module, only: write_sig, write_sfc
  use func_module, only: calc_rh, calc_q2 !=> calc_q
  use obs_module, only: ndate
  use spectral_module, only: spectral_init, spectral_clean, spectral_trunc
  implicit none
  real(kind=dp) :: alpha=0.0d0 !rescaled factor
  integer       :: member=10 !ensemble size
  logical       :: adjust_q=.false. !whether super saturation and super dry are removed or not
  integer       :: ntrunc=0
  integer       :: pow=15
  namelist /namlst_prtb/ alpha,member,adjust_q,ntrunc,pow
  real(kind=dp) :: t1,p1,q1,rh1,qlim,tulim,tllim !for q adjustment
  integer :: ips,it,iu,iv,iq!,icw
  ! input files' units (base, prtb)
  character(len=15) :: file_basename='r_.@@@@.LEV.grd'
  character(len=15) :: filename
  integer, parameter :: nisigb=11
  integer, parameter :: nisfc=12
  integer :: nisigp1=13, nisigp2=14
  logical :: lexist
  ! output file's unit
  integer :: nosig=51
  integer :: nosfc=52
  real(kind=dp), allocatable :: dfld(:,:,:),dfldb(:,:,:)
  real(kind=dp), allocatable :: dfldm(:,:,:),dfldp(:,:,:,:)
  !real(kind=dp), allocatable :: mapf(:,:,:), clat(:), clon(:), slmsk(:,:)
  real(kind=dp), allocatable :: dummapf(:,:,:), dumlat(:), dumlon(:)
  ! lateral boundary relaxation
  real(kind=dp), allocatable :: rltbs(:,:)
!  character(len=8) :: label(4)
!  integer :: idate(4), nfldsig
  real(kind=sp) :: ext(nwext) 
!  real(kind=sp) :: fhour, zhour
!  real(kind=dp) :: si(levmax+1), sl(levmax)
!  real(kind=dp) :: rdelx, rdely, rtruth, rorient, rproj
  integer :: ids(255), iparam(nfldflx)
!  integer :: igrd1, jgrd1, levs, nonhyd, icld
!  integer :: lngrd, km
  integer :: nfldsig, levs, km
  real(kind=dp), allocatable :: grid(:,:)
  ! for ndate
  integer :: date1(5),date2(5),dtmin
  !
  integer :: n,i,j,k,l,im

  ntrunc=0

  read(5,namlst_prtb)
  write(6,namlst_prtb)

!!! get parameters
  filename=file_basename
  write(filename(1:2),'(a2)') 'rb'
  write(filename(4:7),'(i4.4)') 0
  write(filename(9:11),'(a3)') 'sig'
  call set_rsmparm(filename(1:7))
  if(ntrunc.gt.0) then
    call spectral_init(ntrunc=ntrunc)
  end if
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
  allocate( dfldb(igrd1,jgrd1,nfldsig) ) !base(control)
  allocate( dfldp(igrd1,jgrd1,nfldsig,member) ) !perturbation
  allocate( dfldm(igrd1,jgrd1,nfldsig) ) !perturbation mean
  allocate( dummapf(igrd1,jgrd1,3) )
  allocate( dumlat(jgrd1), dumlon(igrd1) )
  ! relaxation
  allocate( rltbs(igrd1,jgrd1) )
  call rltbini

  ips=2
  if(nonhyd.eq.1) then
    it=2+7*levs
  else
    it=3
  end if
  iu=2+levs
  iv=iu+levs
  iq=iv+levs
!  icw=iq+2*levs
  
  ! base field (control)
  filename=file_basename
  write(filename(1:2),'(a2)') 'rb'
  write(filename(4:7),'(i4.4)') 0
  write(filename(9:11),'(a3)') 'sig'
  write(6,'(2a)') 'input base= ',filename
  open(nisigb,file=filename,form='unformatted',access='sequential',action='read')
  call read_sig(nisigb,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sig,&
    dfldb,dummapf,dumlat,dumlon)

  allocate( dfld(igrd1,jgrd1,nfldsig) )
  dfldm = 0.0d0
  nisigp1=13
  nisigp2=14
  do im=1,member
    ! perturbation (nisigp1 - nisigp2)
    filename=file_basename
    write(filename(1:2),'(a2)') 'ri'
    write(filename(4:7),'(i4.4)') 2*im-1
    write(filename(9:11),'(a3)') 'sig'
    write(6,'(2a)') 'input prtb1= ',filename
    open(nisigp1,file=filename,form='unformatted',access='sequential',action='read')
!    call read_header(nisigp1,icld,label,idate,fhour,si,sl,ext,nfldsig)
    dfldp(:,:,:,im)=0.0
    call read_sig(nisigp1,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sig,&
    dfldp(:,:,:,im),dummapf,dumlat,dumlon)
    close(nisigp1)
    ! perturbation
    filename=file_basename
    write(filename(1:2),'(a2)') 'ri'
    write(filename(4:7),'(i4.4)') 2*im
    write(filename(9:11),'(a3)') 'sig'
    write(6,'(2a)') 'input prtb2= ',filename
    open(nisigp2,file=filename,form='unformatted',access='sequential',action='read')
!    call read_header(nisigp2,icld,label,idate,fhour,si,sl,ext,nfldsig)
    call read_sig(nisigp2,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sig,&
    dfld,dummapf,dumlat,dumlon)
    close(nisigp2)
    dfldp(:,:,:,im) = dfldp(:,:,:,im) - dfld
    dfldm = dfldm + dfldp(:,:,:,im)
    nisigp1=nisigp1+2
    nisigp2=nisigp2+2
  end do !im=1,member   
  deallocate( dfld )

  dfldm = dfldm / real(member,kind=dp)
  ! subtract mean and rescaling
  print*, 'rescaling factor = ', alpha
!  call read_header(nisigb,icld,label,idate,fhour,si,sl,ext,nfldsig)
  do im=1,member
    allocate( dfld(igrd1,jgrd1,nfldsig) )
    dfld = dfldp(:,:,:,im)
    dfld = dfld - dfldm
    dfld(:,:,:1) = dfldb(:,:,:1) !gz
    if(ntrunc.gt.0) then
      if(allocated(grid)) deallocate(grid)
      !cc
      km=nfldsig-2*levs-1 ! except for gz,u,v
      allocate( grid(lngrd,km) )
      l=0
      do k=2,nfldsig ! exclude gz
        if(k.ge.iu.and.k.lt.iq) cycle
        l=l+1
        do j=1,jgrd1
          do i=1,igrd1
            grid((j-1)*igrd1+i,l) = dfld(i,j,k)
          end do
        end do
      end do
      print *, 'km ',km,' l ',l
      call spectral_trunc(grid,km,"cc")
      l=0
      do k=2,nfldsig ! exclude gz
        if(k.ge.iu.and.k.lt.iq) cycle
        l=l+1
        do j=1,jgrd1
          do i=1,igrd1
            dfld(i,j,k)=grid((j-1)*igrd1+i,l)
          end do
        end do
      end do
      !cs
      deallocate(grid)
      km=levs ! u
      allocate( grid(lngrd,km) )
      l=0
      do k=2,nfldsig ! exclude gz
        if(k.lt.iu.or.k.ge.iv) cycle
        l=l+1
        do j=1,jgrd1
          do i=1,igrd1
            grid((j-1)*igrd1+i,l) = dfld(i,j,k)
          end do
        end do
      end do
      print *, 'km ',km,' l ',l
      call spectral_trunc(grid,km,"cs")
      l=0
      do k=2,nfldsig ! exclude gz
        if(k.lt.iu.or.k.ge.iv) cycle
        l=l+1
        do j=1,jgrd1
          do i=1,igrd1
            dfld(i,j,k)=grid((j-1)*igrd1+i,l)
          end do
        end do
      end do
      !sc
      deallocate(grid)
      km=levs ! v
      allocate( grid(lngrd,km) )
      l=0
      do k=2,nfldsig ! exclude gz
        if(k.lt.iv.or.k.ge.iq) cycle
        l=l+1
        do j=1,jgrd1
          do i=1,igrd1
            grid((j-1)*igrd1+i,l) = dfld(i,j,k)
          end do
        end do
      end do
      print *, 'km ',km,' l ',l
      call spectral_trunc(grid,km,"cs")
      l=0
      do k=2,nfldsig ! exclude gz
        if(k.lt.iv.or.k.ge.iq) cycle
        l=l+1
        do j=1,jgrd1
          do i=1,igrd1
            dfld(i,j,k)=grid((j-1)*igrd1+i,l)
          end do
        end do
      end do
    end if !trunc
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,2:) = dfldb(i,j,2:) + dfld(i,j,2:) * rltbs(i,j) * alpha !others
      end do
    end do
    if(adjust_q) then
      ! super saturation(dry) adjustment
      tllim = t0 - 30.0_dp
      tulim = t0 + 35.0_dp
      do k=1,levs
        do j=1,jgrd1
          do i=1,igrd1
            t1 = dfld(i,j,it+k-1)
            q1 = dfld(i,j,iq+k-1)
    !        cw1 = dfld(i,j,icw+k-1)
            p1 = dfld(i,j,ips)*sig(k)
            if(q1.lt.0.0_dp) then !super dry
              write(0,'(a,f10.2,a,es10.2,a)') &
                      'super dry adjustment: p=',p1,' q=',q1,' < 0.0'
              dfld(i,j,iq+k-1)=0.0_dp
            else if(p1.gt.20000.0_dp.and.(t1.gt.tllim.and.t1.lt.tulim)) then
              !saturation water vapor accuracy is acceptable for p > 200mb, -30 celsius < T < 35 celsius
              rh1=1.2_dp
              call calc_q2(t1,rh1,p1,qlim)
              if(q1.gt.qlim) then !super saturation
              write(0,'(a,f10.2,x,f10.2,a,f10.2,a,f10.2,x,a,es10.2,a,es10.2)') &
                      'super saturation adjustment: p=',p1,&
                      tllim,' < t=',t1,' < ',tulim,&
                      ' q=',q1,' > ',qlim
              dfld(i,j,iq+k-1)=qlim
              end if
            end if
!            if(cw1.lt.0.0d0) then !negative cloud water
!              print *, 'super dry adjustment: p=',p1,' cw=',cw1,'<0.0'
!              dfld(i,j,icw+k-1)=0.0_dp
!            end if
          end do
        end do
      end do
    end if
    ! write output
    print *, 'posting date = ',idate(4),idate(2),idate(3),idate(1),'+',nint(fhour)
    filename=file_basename
    write(filename(1:2),'(a2)') 'ro'
    write(filename(4:7),'(i4.4)') im
    write(filename(9:11),'(a3)') 'sig'
    write(6,'(2a)') 'output= ',filename
    open(nosig,file=filename,form='unformatted',access='sequential',action='write')
    call write_sig(nosig,label,idate,fhour,sigh,sig,ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,icld,dfld,mapf,rlat,rlon)
    close(nosig)
    ! read surface and change forecast date and hour, then write out
    deallocate( dfld )
    allocate( dfld(igrd1,jgrd1,nfldsfc) )
    filename=file_basename
    write(filename(1:2),'(a2)') 'rb'
    write(filename(4:7),'(i4.4)') 0
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
    call write_sfc(nosfc,igrd1,jgrd1,dfld,label,idate,fhour)
    close(nosfc)
    deallocate( dfld )
    nosig=nosig+2
    nosfc=nosfc+2
  end do
  deallocate( dfldm,dfldb,dfldp ) 
contains
  subroutine rltbini
    implicit none
    real(kind=dp) :: coeout, xl, yl, xc, yc
    integer :: i, j

    xc = float(igrd1-1)/2.0
    yc = float(jgrd1-1)/2.0
    do j=1,jgrd1
      yl = abs(float(j)-yc)
      do i=1,igrd1
        xl = abs(float(i)-xc)
        coeout = (min(max(xl/xc,yl/yc),1.))**pow
        rltbs(i,j) = coeout
      end do
    end do
    return
  end subroutine rltbini
end program
