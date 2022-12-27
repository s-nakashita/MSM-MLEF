program addprtb
!
! add rescaled perturbations based on error moist total energy
!
  use kind_module
  use phconst_module
  use read_module
  use write_module, only: write_sig, write_sfc
  use norm_module, only: calc_te
  use func_module, only: calc_rh, calc_q2 !=> calc_q
  implicit none
  ! for energy calculation
  integer, parameter :: kmax=21
  !real(kind=dp), parameter :: teref=5.6d0 !rescaled total energy [J/kg/m2]
  real(kind=dp), parameter :: uscl=1.0d0,vscl=1.0d0&
                           &,thetascl=0.4d0,rhscl=5.0d-2,psscl=3.5d1
  real(kind=dp), parameter :: tr=300.0d0, pr=800.0d2![Pa]
  real(kind=dp), parameter :: p0=1000.0d2, ptheta=rd/cp ! potential temperature
  real(kind=dp)            :: tscl,qscl,pscl,tbase,qbase
  real(kind=dp) :: teref=0.0d0 !rescaled total energy [J/kg/m2]
  real(kind=dp) :: epsq=1.0d0   !weight for moist term (0.0=dry)
  logical       :: setnorm=.FALSE. !whether rescaling norm magnitude is given from namelist or not
  real(kind=dp) :: lonw=-999.9d0, lone=-999.9d0 !calculation region
  real(kind=dp) :: lats=-999.9d0, latn=-999.9d0 !calculation region
  integer       :: ilonw,ilone,jlats,jlatn !calculation region
  integer       :: nlon,nlat
  logical       :: adjust_q=.false. !whether super saturation and super dry are removed or not
  namelist /namlst_prtb/ setnorm,teref,epsq,lonw,lone,lats,latn,adjust_q
  real(kind=dp), allocatable :: u(:,:,:),v(:,:,:),t(:,:,:),q(:,:,:)
  real(kind=dp), allocatable :: fact(:,:,:),theta(:,:,:)
  real(kind=dp), allocatable :: ps(:,:)
  real(kind=dp) :: alpha !rescaled factor
  real(kind=dp) :: tecmp(4)
  real(kind=dp) :: area,te,coef
  real(kind=dp) :: t1,p1,q1,rh1,cw1,qlim !for q adjustment
  integer :: ips,it,iu,iv,iq
  ! input files' units (base, prtb)
  integer, parameter :: nisigb=11, nisigp1=12, nisigp2=13
  integer, parameter :: nisigib=15 !intermediate file
  integer, parameter :: nisfc=14
  logical :: lexist
  ! output file's unit
  integer, parameter :: nosigp=51, nosigm=52 !plus and minus
  integer, parameter :: nosfc=53 !, nosigi=54
  real(kind=dp), allocatable :: dfld(:,:,:)
  real(kind=dp), allocatable :: dfldb(:,:,:),dfldp(:,:,:)
  real(kind=dp), allocatable :: mapf(:,:,:), clat(:), clon(:), slmsk(:,:)
  character(len=8) :: label(4)
  integer :: idate(4), nfldsig
  real(kind=sp) :: fhour, ext(nwext) 
  real(kind=sp) :: zhour
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=dp) :: rdelx, rdely, rtruth, rorient, rproj
  integer :: ids(255), iparam(nfldflx)
  integer :: igrd1, jgrd1, levs, nonhyd, icld
  integer :: n,i,j,k,l

  read(5,namlst_prtb)
  write(6,namlst_prtb)

!!! sigma files (r_sig.fNN)
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
  allocate( dfldb(igrd1,jgrd1,nfldsig) )
  allocate( mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1), clon(igrd1) )
  
  ips=2
  it=3
  iu=it+levs
  iv=iu+levs
  iq=iv+levs
  ! base field
  call read_sig(nisigb,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,dfldb,mapf,clat,clon)
  !! set boundaries
  if((lonw.gt.-999.9d0).and.(lone.gt.-999.9d0)) then
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
  if((lats.gt.-999.9d0).and.(latn.gt.-999.9d0)) then
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
  print *, "boundary ",ilonw,"-",ilone," lon ",clon(ilonw),"-",clon(ilone)
  print *, "boundary ",jlats,"-",jlatn," lat ",clat(jlats),"-",clat(jlatn)
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

  ! perturbation (nisigp1 - nisigp2)
  call read_header(nisigp1,icld,label,idate,fhour,si,sl,ext,nfldsig)
  allocate( dfld(igrd1,jgrd1,nfldsig) )
  allocate( dfldp(igrd1,jgrd1,nfldsig) )
  dfldp=0.0
  call read_sig(nisigp1,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,dfld,mapf,clat,clon)
  dfldp = dfld
!  dfldp(:,:,ips) = dfld(:,:,ips)
  do j=1,nlat
    do i=1,nlon
      ps(i,j)=dfld(i+ilonw-1,j+jlats-1,ips)
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        t(i,j,k) = dfld(i+ilonw-1,j+jlats-1,it+k-1)
        theta(i,j,k) = t(i,j,k) * (p0/dfld(i+ilonw-1,j+jlats-1,ips)/sl(k))**ptheta
!    dfldp(:,:,it+k-1)=dfld(:,:,it+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        u(i,j,k) = dfld(i+ilonw-1,j+jlats-1,iu+k-1)
!    dfldp(:,:,iu+k-1)=dfld(:,:,iu+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        v(i,j,k) = dfld(i+ilonw-1,j+jlats-1,iv+k-1)
!    dfldp(:,:,iv+k-1)=dfld(:,:,iv+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        q(i,j,k) = dfld(i+ilonw-1,j+jlats-1,iq+k-1)
!    dfldp(:,:,iq+k-1)=dfld(:,:,iq+k-1)
      end do
    end do
  end do
  print*, 'u(full)', maxval(u),minval(u)
  print*, 'v(full)', maxval(v),minval(v)
  print*, 'theta(full)', maxval(theta),minval(theta)
  print*, 'q(full)', maxval(q),minval(q)
  print*, 'ps(full)', maxval(ps),minval(ps)
  ! perturbation
  call read_header(nisigp2,icld,label,idate,fhour,si,sl,ext,nfldsig)
  deallocate( dfld )
  allocate( dfld(igrd1,jgrd1,nfldsig) )
  call read_sig(nisigp2,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,dfld,mapf,clat,clon)
  dfldp = dfldp - dfld
  do j=1,nlat
    do i=1,nlon
      ps(i,j)=ps(i,j)-dfld(i+ilonw-1,j+jlats-1,ips)
!  dfldp(:,:,ips) = dfldp(:,:,ips) - dfld(:,:,ips)
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        !t(i,j,k) = t(i,j,k)-real(dfld(i+ilonw-1,j+jlats-1,it+k-1),kind=dp)/fact(i,j,k)
        t(i,j,k) = dfld(i+ilonw-1,j+jlats-1,it+k-1)
        theta(i,j,k) = theta(i,j,k) - t(i,j,k) * (p0/dfld(i+ilonw-1,j+jlats-1,ips)/sl(k))**ptheta
!    dfldp(:,:,it+k-1)=dfldp(:,:,it+k-1)-dfld(:,:,it+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        u(i,j,k) = u(i,j,k)-dfld(i+ilonw-1,j+jlats-1,iu+k-1)
!    dfldp(:,:,iu+k-1)=dfldp(:,:,iu+k-1)-dfld(:,:,iu+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        v(i,j,k) = v(i,j,k)-dfld(i+ilonw-1,j+jlats-1,iv+k-1)
!    dfldp(:,:,iv+k-1)=dfldp(:,:,iv+k-1)-dfld(:,:,iv+k-1)
      end do
    end do
  end do
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        q(i,j,k) = q(i,j,k)-dfld(i+ilonw-1,j+jlats-1,iq+k-1)
!    dfldp(:,:,iq+k-1)=dfldp(:,:,iq+k-1)-dfld(:,:,iq+k-1)
      end do
    end do
  end do
  print*, 'u(prtb)', maxval(u),minval(u)
  print*, 'v(prtb)', maxval(v),minval(v)
  print*, 'theta(prtb)', maxval(theta),minval(theta)
  print*, 'q(prtb)', maxval(q),minval(q)
  print*, 'ps(prtb)', maxval(ps),minval(ps)
  deallocate( dfld )

  if(.not.setnorm) then
  ! calculate reference energy
  teref=0.0d0
  area=0.0d0
  do k=1,kmax
    pscl=p0*sl(k)
    tscl=thetascl*sl(k)**(1.0d0/ptheta)
    tbase=tr
    call calc_q2(tbase,rhscl,pscl,qbase)
    tbase=tr+tscl
    call calc_q2(tbase,rhscl,pscl,qscl)
    qscl=qscl-qbase
    print '(5(a,es11.4))', 'uscl ',uscl,' vscl ',vscl,' tscl ',tscl,' qscl ',qscl,' psscl ',psscl 
    do j=1,nlat
      coef=(si(k)-si(k+1))*cos(clat(j+jlats-1)*deg2rad)
      do i=1,nlon
        !KE
        teref=teref+(uscl*uscl+vscl*vscl)*coef
        !PE(T)
        teref=teref+cp/tr*thetascl*thetascl*coef
        !LE
        teref=teref+epsq*lh**2/cp/tr*qscl*qscl*coef
      end do
    end do
  end do
  do j=1,nlat
    coef=cos(clat(j+jlats-1)*deg2rad)
    do i=1,nlon
      !PE(Ps)
      teref=teref+rd*tr*psscl*psscl/pr/pr*coef
      area=area+coef
    end do
  end do
  teref=teref*0.5d0/area
  end if
  print*, 'normalized total energy = ', teref
  ! calculate energy
  call calc_te(u,v,theta,q,ps,epsq,clat(jlats:jlatn),si,nlon,nlat,tecmp)
  te=sum(tecmp)
  print *, tecmp
  print*, 'perturbation total energy = ', te
  ! rescaling
  alpha = sqrt(teref / te)
  print*, 'rescaling factor = ', alpha
  ! write output
  call read_header(nisigb,icld,label,idate,fhour,si,sl,ext,nfldsig)
  allocate( dfld(igrd1,jgrd1,nfldsig) )
  !! reset forecast hour
  idate(1)=idate(1)+nint(fhour)
  if(idate(1).ge.24) then
    idate(1)=idate(1)-24
    idate(3)=idate(3)+1
  end if
  fhour=0.0
  print *, idate(4),idate(2),idate(3),idate(1),'+',nint(fhour)
  !! add perturbations
  dfld = dfldb + dfldp * alpha
  if(adjust_q) then
  ! super saturation(dry) adjustment
  do k=1,levs
    do j=1,jgrd1
      do i=1,igrd1
        t1 = dfld(i,j,it+k-1)
        q1 = dfld(i,j,iq+k-1)
        cw1 = dfld(i,j,icw+k-1)
        p1 = dfld(i,j,ips)*sl(k)
        if(q1.lt.0.0_dp) then !super dry
          print *, 'super dry adjustment: p=',p1,' q=',q1,'<0.0'
          dfld(i,j,iq+k-1)=0.0_dp
        else 
          rh1=1.0_dp
          call calc_q2(t1,rh1,p1,qlim)
          if(q1.gt.qlim) then !super saturation
          print *, 'super saturation adjustment: p=',p1,' q=',q1,'>',qlim
          print *, 'Q before =',dfld(i,j,iq+k-1)
          dfld(i,j,iq+k-1)=qlim
          print *, 'Q after =',dfld(i,j,iq+k-1)
        end if
        if(cw1.lt.0.0d0) then !negative cloud water
          print *, 'super dry adjustment: p=',p1,' cw=',cw1,'<0.0'
          dfld(i,j,icw+k-1)=0.0_dp
        end if
      end do
    end do
  end do
  end if
  call write_sig(nosigp,label,idate,fhour,si(1:levs+1),sl(1:levs),ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,icld,dfld,mapf,clat,clon)
  !! subtract perturbations
  dfld = dfldb - dfldp * alpha
  if(adjust_q) then
  ! super saturation(dry) adjustment
  do k=1,levs
    do j=1,jgrd1
      do i=1,igrd1
        t1 = dfld(i,j,it+k-1)
        q1 = dfld(i,j,iq+k-1)
        cw1 = dfld(i,j,icw+k-1)
        p1 = dfld(i,j,ips)*sl(k)
        if(q1.lt.0.0_dp) then !super dry
          print *, 'super dry adjustment: p=',p1,' q=',q1,'<0.0'
          dfld(i,j,iq+k-1)=0.0_dp
        else 
          rh1=1.0_dp
          call calc_q2(t1,rh1,p1,qlim)
          if(q1.gt.qlim) then !super saturation
          print *, 'super saturation adjustment: p=',p1,' q=',q1,'>',qlim
          print *, 'Q before =',dfld(i,j,iq+k-1)
          dfld(i,j,iq+k-1)=qlim
          print *, 'Q after =',dfld(i,j,iq+k-1)
        end if
        if(cw1.lt.0.0d0) then !negative cloud water
          print *, 'super dry adjustment: p=',p1,' cw=',cw1,'<0.0'
          dfld(i,j,icw+k-1)=0.0_dp
        end if
      end do
    end do
  end do
  end if
  call write_sig(nosigm,label,idate,fhour,si(1:levs+1),sl(1:levs),ext,&
&                    igrd1,jgrd1,levs,nfldsig,nonhyd,icld,dfld,mapf,clat,clon)
!!  ! intermediate field
!!  icld=0
!!  call read_header(nisigib,icld,label,idate,fhour,si,sl,ext,nfldsig)
!!  deallocate( dfldb )
!!  allocate( dfldb(igrd1,jgrd1,nfldsig) )
!!  call read_sig(nisigib,igrd1,jgrd1,levs,nfldsig,nonhyd,icld,fhour,sl,dfldb,mapf,clat,clon)
!!  print*, 'ui(full)', maxval(dfldb(:,:,iu:iu+levs-1)),minval(dfldb(:,:,iu:iu+levs-1))
!!  print*, 'vi(full)', maxval(dfldb(:,:,iv:iv+levs-1)),minval(dfldb(:,:,iv:iv+levs-1))
!!  print*, 'ti(full)', maxval(dfldb(:,:,it:it+levs-1)),minval(dfldb(:,:,it:it+levs-1))
!!  print*, 'qi(full)', maxval(dfldb(:,:,iq:iq+levs-1)),minval(dfldb(:,:,iq:iq+levs-1))
!!  print*, 'psi(full)', maxval(dfldb(:,:,ips)),minval(dfldb(:,:,ips))
!!  ! add perturbations
!!  !dfldb = dfldb + dfldp * alpha
!!  dfldb(:,:,ips)=dfldb(:,:,ips)+dfldp(:,:,ips)*alpha
!!  do k=1,levs
!!    dfldb(:,:,it+k-1)=dfldb(:,:,it+k-1)+dfldp(:,:,it+k-1)*alpha
!!    dfldb(:,:,iu+k-1)=dfldb(:,:,iu+k-1)+dfldp(:,:,iu+k-1)*alpha
!!    dfldb(:,:,iv+k-1)=dfldb(:,:,iv+k-1)+dfldp(:,:,iv+k-1)*alpha
!!    dfldb(:,:,iq+k-1)=dfldb(:,:,iq+k-1)+dfldp(:,:,iq+k-1)*alpha
!!  end do
!!  ! write output
!!  !! reset forecast hour
!!  idate(1)=idate(1)+nint(fhour)
!!  if(idate(1).ge.24) then
!!    idate(1)=idate(1)-24
!!    idate(3)=idate(3)+1
!!  end if
!!  fhour=0.0
!!  print *, idate(4),idate(2),idate(3),idate(1),'+',nint(fhour)
!!  call write_sig(nosigi,label,idate,fhour,si(1:levs+1),sl(1:levs),ext,&
!!&                    igrd1,jgrd1,levs,nfldsig,nonhyd,icld,dfldb,mapf,clat,clon)
  deallocate( dfld,dfldb,dfldp,u,v,t,q,ps,fact ) 
  ! read surface and change forecast date and hour, then write out
  allocate( dfld(igrd1,jgrd1,nfldsfc) )
  call read_sfc(nisfc,igrd1,jgrd1,dfld)
  call write_sfc(nosfc,igrd1,jgrd1,dfld,label,idate,fhour)
  deallocate( dfld )
!contains
!  subroutine calc_q(t,rh,p,q)
!    implicit none
!    real(kind=dp),parameter :: t0=273.15d0
!    real(kind=dp),parameter :: e0c=6.11d0
!    real(kind=dp),parameter :: al=17.3d0
!    real(kind=dp),parameter :: bl=237.3d0
!    real(kind=dp),parameter :: e0i=6.1121d0
!    real(kind=dp),parameter :: ai=22.587d0
!    real(kind=dp),parameter :: bi=273.86d0
!    real(kind=dp),intent(in) :: t,rh,p
!    real(kind=dp),intent(out) :: q
!    real(kind=dp) :: e, es, tc
!
!    tc=t-t0
!    if(tc>=0.0d0) then
!      es=e0c*exp(al*tc/(bl+tc))
!    else if(tc<=-15.0d0) then
!      es=e0i*exp(ai*tc/(bi+tc))
!    else
!      es=e0c*exp(al*tc/(bl+tc))*(15.0d0+tc)/15.0d0 &
!        +e0i*exp(ai*tc/(bi+tc))*(-tc)/15.0d0
!    endif
!    e=rh*es
!    q=0.622d0*e/(p*0.01d0-e*0.378d0)
!    return
!  end subroutine calc_q
end program
