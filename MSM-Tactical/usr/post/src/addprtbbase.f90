program addprtbbase
!
! add rescaled perturbations to base fields
!
  use kind_module
  use phconst_module
  use read_module
  use write_module, only: write_sig, write_sfc
  use func_module, only: ndate, calc_rh, calc_q2 !=> calc_q
  implicit none
  real(kind=dp) :: alpha=0.0d0 !rescaled factor
  integer       :: member=10 !ensemble size
  logical       :: adjust_q=.false. !whether super saturation and super dry are removed or not
  namelist /namlst_prtb/ alpha,member,adjust_q
  real(kind=dp) :: t1,p1,q1,rh1,qlim,tulim,tllim !for q adjustment
  integer :: ips,it,iu,iv,iq!,icw
  ! input files' units (base, prtb)
  integer, parameter :: nisigb=11
  integer, parameter :: nisfc=12
  integer :: nisigp1=13, nisigp2=14
  logical :: lexist
  ! output file's unit
  integer :: nosig=51
  integer :: nosfc=52
  real(kind=dp), allocatable :: dfld(:,:,:),dfldb(:,:,:)
  real(kind=dp), allocatable :: dfldm(:,:,:),dfldp(:,:,:,:)
  real(kind=dp), allocatable :: mapf(:,:,:), clat(:), clon(:), slmsk(:,:)
  character(len=8) :: label(4)
  integer :: idate(4), nfldsig
  real(kind=sp) :: fhour, ext(nwext) 
  real(kind=sp) :: zhour
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=dp) :: rdelx, rdely, rtruth, rorient, rproj
  integer :: ids(255), iparam(nfldflx)
  integer :: igrd1, jgrd1, levs, nonhyd, icld
  ! for ndate
  integer :: date1(5),date2(5),dtmin
  !
  integer :: n,i,j,k,l,im

  read(5,namlst_prtb)
  write(6,namlst_prtb)

!!! get parameters
  icld=1
  call read_header(nisigb,icld,label,idate,fhour,si,sl,ext,nfldsig)
  print*, label
  print*, idate
  print*, fhour
  print*, ext(1:16)
  print*, nfldsig
  igrd1 = int(ext(3))
  jgrd1 = int(ext(4))
!  print*, igrd1, jgrd1
  levs = int(ext(5))
!  print*, levs
  rproj = ext(7)
  rtruth=ext(8); rorient=ext(9)
  rdelx=ext(14); rdely=ext(15)
  nonhyd=int(ext(16))
!  print*, nonhyd
!  print*, si(1:levs+1)
!  print*, sl(1:levs)
  allocate( dfldb(igrd1,jgrd1,nfldsig) ) !base(control)
  allocate( dfldp(igrd1,jgrd1,nfldsig,member) ) !perturbation
  allocate( dfldm(igrd1,jgrd1,nfldsig) ) !perturbation mean
  allocate( mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1), clon(igrd1) )
  ips=2
  it=3
  iu=it+levs
  iv=iu+levs
  iq=iv+levs
!  icw=iq+2*levs
  
  ! base field (control)
  call read_sig(nisigb,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,&
    dfldb,mapf,clat,clon)

  allocate( dfld(igrd1,jgrd1,nfldsig) )
  dfldm = 0.0d0
  nisigp1=13
  nisigp2=14
  do im=1,member
    ! perturbation (nisigp1 - nisigp2)
    call read_header(nisigp1,icld,label,idate,fhour,si,sl,ext,nfldsig)
    dfldp(:,:,:,im)=0.0
    call read_sig(nisigp1,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,&
    dfldp(:,:,:,im),mapf,clat,clon)
    ! perturbation
    call read_header(nisigp2,icld,label,idate,fhour,si,sl,ext,nfldsig)
    call read_sig(nisigp2,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,&
    dfld,mapf,clat,clon)
    dfldp(:,:,:,im) = dfldp(:,:,:,im) - dfld
    dfldm = dfldm + dfldp(:,:,:,im)
    nisigp1=nisigp1+2
    nisigp2=nisigp2+2
  end do !im=1,member   
  deallocate( dfld )

  dfldm = dfldm / real(member,kind=dp)
  ! subtract mean and rescaling
  print*, 'rescaling factor = ', alpha
  call read_header(nisigb,icld,label,idate,fhour,si,sl,ext,nfldsig)
  do im=1,member
    allocate( dfld(igrd1,jgrd1,nfldsig) )
    dfld = dfldp(:,:,:,im)
    dfld = dfld - dfldm
    dfld = dfldb + dfld * alpha
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
            p1 = dfld(i,j,ips)*sl(k)
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
    call write_sig(nosig,label,idate,fhour,si,sl,ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,icld,dfld,mapf,clat,clon)
    ! read surface and change forecast date and hour, then write out
    deallocate( dfld )
    allocate( dfld(igrd1,jgrd1,nfldsfc) )
    call read_sfc(nisfc,igrd1,jgrd1,dfld)
    call write_sfc(nosfc,igrd1,jgrd1,dfld,label,idate,fhour)
    deallocate( dfld )
    nosig=nosig+2
    nosfc=nosfc+2
  end do
  deallocate( dfldm,dfldb,dfldp ) 
end program
