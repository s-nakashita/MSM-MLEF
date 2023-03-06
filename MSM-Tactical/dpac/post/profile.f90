program profile
!
! calculate vertical profile of r_sig.fNN
!
  use kind_module
  use phconst_module
  use rsmcom_module
  use read_module
  use func_module, only: ndate
  implicit none
  logical       :: lprtb=.false. ! False=>calculate for full field
  real(kind=dp) :: lonw=-999.9d0, lone=-999.9d0 ! calculation region
  real(kind=dp) :: lats=-999.9d0, latn=-999.9d0 ! calculation region
  namelist /namlst_prof/ lprtb, lonw, lone, lats, latn
  integer :: ilonw, ilone, jlats, jlatn ! calculation region indexes
  integer :: nlonl, nlatl, ilon, jlat
  !
  real(kind=dp) :: area,coef
  real(kind=dp), allocatable :: vwgt(:), prof(:,:)
  real(kind=sp), allocatable :: buf4(:)
  integer :: irec
  integer :: ips,it,iu,iv,iq
  integer, allocatable :: idvars(:)
  character(len=13) :: ofile='prof_fhNN.bin'
  character(len=13) :: cfile='prof_fhNN.ctl'
  ! input files
  character(len=4) :: inf1='base'
  character(len=4) :: inf2='prtb' !prtb+base
  integer, parameter :: nisig=11, nisfc=21, niflx=31
  integer            :: nsig,     nsfc,     nflx
  real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
  real(kind=dp), allocatable :: v3dp(:,:,:,:), v2dp(:,:,:)
  ! for ndate
  integer :: date1(5),date2(5),dtmin,iymdh,iymdh2
  integer :: idate2(4)
  real(kind=sp) :: fhour2
  integer :: n,nn,i,j,k

  read(5,namlst_prof)
  write(6,namlst_prof)
  
!!! set parameters
  call set_rsmparm(inf1)
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
  print*, igrd1, jgrd1
  print*, nlev
  print*, nonhyd
  print*, sigh(1:nlev+1)
  print*, sig(1:nlev)
  allocate( v3dg(nlon,nlat,nlev,nv3d) )
  allocate( v2dg(nlon,nlat,nv2d) ) !not used
  !! setting boundaries
  if ((lonw.gt.-999.9d0).and.(lone.gt.-999.9d0)) then
    do i=1,igrd1
      if(rlon(i).ge.lonw) then
        ilonw=i
        exit
      end if
    end do 
    do i=1,igrd1
      if(rlon(i).ge.lone) then
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
      if(rlat(j).ge.lats) then
        jlats=j
        exit
      end if
    end do 
    do j=1,jgrd1
      if(rlat(j).ge.latn) then
        jlatn=j
        exit
      end if
    end do 
  else
    jlats=1
    jlatn=jgrd1
  end if
  print *, 'boundary ',ilonw,'-',ilone,' lon ',rlon(ilonw),'-',rlon(ilone)
  print *, 'boundary ',jlats,'-',jlatn,' lat ',rlat(jlats),'-',rlat(jlatn)
  nlonl = ilone - ilonw + 1
  nlatl = jlatn - jlats + 1
  print *, 'nlonl ',nlonl,' nlatl ',nlatl
  
  if(nonhyd.eq.1) then 
    iv3d_t=iv3d_tn
    allocate( idvars(nv3d-1) )
    idvars(1)=iv3d_t
    idvars(2)=iv3d_u
    idvars(3)=iv3d_v
    idvars(4)=iv3d_q
    idvars(5)=iv3d_oz
    idvars(6)=iv3d_cw
    idvars(7)=iv3d_pn
    idvars(8)=iv3d_wn
  else
    iv3d_t=iv3d_th
    allocate( idvars(nv3d) )
    idvars(1)=iv3d_t
    idvars(2)=iv3d_u
    idvars(3)=iv3d_v
    idvars(4)=iv3d_q
    idvars(5)=iv3d_oz
    idvars(6)=iv3d_cw
  end if
  ! base
  call read_restart(inf1,v3dg,v2dg)
  do k=1,nlev
    print*, k
    print *, 'u(full,max)', maxval(v3dg(:,:,k,iv3d_u)),maxloc(v3dg(:,:,k,iv3d_u))
    print *, 'u(full,min)', minval(v3dg(:,:,k,iv3d_u)),minloc(v3dg(:,:,k,iv3d_u))
    print *, 'v(full,max)', maxval(v3dg(:,:,k,iv3d_v)),maxloc(v3dg(:,:,k,iv3d_v))
    print *, 'v(full,min)', minval(v3dg(:,:,k,iv3d_v)),minloc(v3dg(:,:,k,iv3d_v))
    print *, 't(full,max)', maxval(v3dg(:,:,k,iv3d_t)),maxloc(v3dg(:,:,k,iv3d_t))
    print *, 't(full,min)', minval(v3dg(:,:,k,iv3d_t)),minloc(v3dg(:,:,k,iv3d_t))
    print *, 'q(full,max)', maxval(v3dg(:,:,k,iv3d_q)),maxloc(v3dg(:,:,k,iv3d_q))
    print *, 'q(full,min)', minval(v3dg(:,:,k,iv3d_q)),minloc(v3dg(:,:,k,iv3d_q))
  end do
  if(lprtb) then
  ! perturbed field
  allocate( v3dp(nlon,nlat,nlev,nv3d) )
  allocate( v2dp(nlon,nlat,nv2d) ) !not used
  !! consistency check
  call clean_rsmparm
  call set_rsmparm(inf2)
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
  iymdh2 = idate(4)*1000000+idate(2)*10000+idate(3)*100+idate(1)
  if (iymdh.ne.iymdh2) then
    print *, 'valid dates are different, ',iymdh,' ',iymdh2
    stop 99
  end if
  call read_restart(inf2,v3dp,v2dp)
  v3dg = v3dg - v3dp
  do k=1,nlev
    print*, k
    print *, 'u(prtb,max)', maxval(v3dg(:,:,k,iv3d_u)),maxloc(v3dg(:,:,k,iv3d_u))
    print *, 'u(prtb,min)', minval(v3dg(:,:,k,iv3d_u)),minloc(v3dg(:,:,k,iv3d_u))
    print *, 'v(prtb,max)', maxval(v3dg(:,:,k,iv3d_v)),maxloc(v3dg(:,:,k,iv3d_v))
    print *, 'v(prtb,min)', minval(v3dg(:,:,k,iv3d_v)),minloc(v3dg(:,:,k,iv3d_v))
    print *, 't(prtb,max)', maxval(v3dg(:,:,k,iv3d_t)),maxloc(v3dg(:,:,k,iv3d_t))
    print *, 't(prtb,min)', minval(v3dg(:,:,k,iv3d_t)),minloc(v3dg(:,:,k,iv3d_t))
    print *, 'q(prtb,max)', maxval(v3dg(:,:,k,iv3d_q)),maxloc(v3dg(:,:,k,iv3d_q))
    print *, 'q(prtb,min)', minval(v3dg(:,:,k,iv3d_q)),minloc(v3dg(:,:,k,iv3d_q))
  end do
  deallocate( v3dp, v2dp )
  end if

  write(ofile(8:9),'(i2.2)') nint(fhour)
  write(cfile(8:9),'(i2.2)') nint(fhour)
  nn=size(idvars)
  allocate( vwgt(nlev), prof(nlev,nn) )
  prof(:,:) = 0.0d0
  do n=1,nn
    print *, idvars(n)
    do k=1,nlev
      vwgt(k) = sigh(k) - sigh(k+1)
      area=0.0d0
      do j=1,nlatl
        jlat=jlats+j-1
        coef=cos(rlat(jlat)*deg2rad)
        do i=1,nlonl
          ilon=ilonw+i-1
          prof(k,n)=prof(k,n)+v3dg(ilon,jlat,k,idvars(n))*coef
          area=area+coef
        end do
      end do
      prof(k,n)=prof(k,n)/area
    end do
  end do
  open(55,file=ofile,form='unformatted',access='direct',recl=4*nlev)
  buf4 = real(vwgt(:),kind=sp)
  write(55,rec=1) buf4
  do n=1,nn
    buf4 = real(prof(:,n),kind=sp)
    write(55,rec=n+1) buf4
  end do
  close(55)
  open(60,file=cfile)
  call genctl(60,ofile)
  close(60)
  deallocate( vwgt, prof )
  deallocate( v3dg, v2dg )
contains
  subroutine genctl(nctl,cfile)
    implicit none
    integer, intent(in) :: nctl
    character(len=*), intent(in) :: cfile
    integer :: i,j
    integer :: ihr,idy,imo,iyr
    integer :: days(12),daysl(12)
    data days/31,28,31,30,31,30,31,31,30,31,30,31/
    data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
    character(len=2) hour, day
    character(len=3) mon(12)
    data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
&            'JUL','AUG','SEP','OCT','NOV','DEC'/

    write(nctl,'(2a)') 'dset ^',cfile
    write(nctl,'(a)') 'options big_endian'
    write(nctl,'(a)') 'undef -9.99E+33'
    write(nctl,108) 1
 108  format('xdef',I5,' linear 0 1')
    write(nctl,110) 1
 110  format('ydef',I5,' linear 0 1')
    write(nctl,112) nlev
 112  format('zdef',I5,' linear 1 1')
    ihr = idate(1) + nint(fhour)
    idy = idate(3)
    imo = idate(2)
    iyr = idate(4)
    do while (ihr .ge. 24)
      idy=idy+1
      ihr=ihr-24
    end do
    if (mod(iyr,4).eq.0) then
      if(idy.gt.daysl(imo)) then
        imo=imo+1
        idy=idy-daysl(imo-1)
      end if
    else
      if(idy.gt.days(imo)) then
        imo=imo+1
        idy=idy-days(imo-1)
      end if
    end if
    if (imo.gt.12) then
      iyr=iyr+1
      imo=imo-12
    end if
    write(hour,'(i2.2)') ihr
    write(day, '(i2.2)') idy
    write(nctl,114) hour,day,mon(imo),iyr
 114 format('tdef 1 linear ',A2,'Z',A2,A3,I4,'   1hr')
    if(nonhyd.eq.1) then
    write(nctl,'(a)') 'vars 9'
    else
    write(nctl,'(a)') 'vars 7'
    end if
    write(nctl,'(a,i5,a)') 'vwgt ',nlev,' 99 layer thickness'
    write(nctl,'(a,i5,a)') 't ',nlev,' 99 temperature [K]'
    write(nctl,'(a,i5,a)') 'q ',nlev,' 99 specific humidity [g/kg]'
    write(nctl,'(a,i5,a)') 'oz ',nlev,' 99 ozone mixing ratio'
    write(nctl,'(a,i5,a)') 'cw ',nlev,' 99 cloud water'
    if(nonhyd.eq.1) then
    write(nctl,'(a,i5,a)') 'pn ',nlev,' 99 full pressure'
    write(nctl,'(a,i5,a)') 'wn ',nlev,' 99 vertical velocity'
    end if
    write(nctl,'(a,i5,a)') 'u ',nlev,' 99 zonal wind [m/s]'
    write(nctl,'(a,i5,a)') 'v ',nlev,' 99 meridional wind [m/s]'
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end program
