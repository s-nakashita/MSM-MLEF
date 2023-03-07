program spectra
!
! calculate full or perturbation power spectrum
!
  use kind_module
  use phconst_module
  use rsmcom_module
  use read_module
  use spectral_module
  use obs_module, only: ndate
  implicit none
  logical :: lprtb=.true. ! False=>calculate for full field
  namelist /namlst_spectra/ lprtb
  ! for calculation
  real(kind=dp), allocatable :: grid(:,:)
  real(kind=dp), allocatable :: coef(:,:)
  real(kind=sp), allocatable :: buf4(:)
  integer, allocatable :: idvars(:)
  character(len=2), allocatable :: cvars(:)
  integer :: nvarall, irec
  character(len=17) :: ofile='spectrum_fhNN.bin'
  character(len=17) :: cfile='spectrum_fhNN.ctl'
  ! input files
  character(len=4) :: inf1='base' !base
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

  read (5,namlst_spectra)
  write(6,namlst_spectra)
  
!!! set parameters
  ! headers are assumed to be identical for all initial time
  call set_rsmparm(inf1)
  print*, label
  print*, idate
  print*, fhour
  print*, nflds
  print*, igrd1, jgrd1
  print*, nlev
  print*, nonhyd
  print*, sigh(1:nlev+1)
  print*, sig (1:nlev)
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
  allocate( v3dg(nlon,nlat,nlev,nv3d) )
  allocate( v2dg(nlon,nlat,nv2d) ) !not used
  
  if(nonhyd.eq.1) then 
    iv3d_t=iv3d_tn
    allocate( idvars(nv3d-1) )
    allocate( cvars(nv3d-1) )
    idvars(1)=iv3d_t;cvars(1)='t'
    idvars(2)=iv3d_q;cvars(2)='q'
    idvars(3)=iv3d_oz;cvars(3)='oz'
    idvars(4)=iv3d_cw;cvars(4)='cw'
    idvars(5)=iv3d_pn;cvars(5)='p'
    idvars(6)=iv3d_wn;cvars(6)='w'
    idvars(7)=iv3d_u;cvars(7)='u'
    idvars(8)=iv3d_v;cvars(8)='v'
  else
    iv3d_t=iv3d_th
    allocate( idvars(nv3d) )
    allocate( cvars(nv3d) )
    idvars(1)=iv3d_t;cvars(1)='t'
    idvars(2)=iv3d_q;cvars(2)='q'
    idvars(3)=iv3d_oz;cvars(3)='oz'
    idvars(4)=iv3d_cw;cvars(4)='cw'
    idvars(5)=iv3d_u;cvars(5)='u'
    idvars(6)=iv3d_v;cvars(6)='v'
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
  print *, 'ps(full,max)', maxval(v2dg(:,:,iv2d_ps)),maxloc(v2dg(:,:,iv2d_ps))
  print *, 'ps(full,min)', minval(v2dg(:,:,iv2d_ps)),minloc(v2dg(:,:,iv2d_ps))
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
  v2dg = v2dg - v2dp
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
  print *, 'ps(prtb,max)', maxval(v2dg(:,:,iv2d_ps)),maxloc(v2dg(:,:,iv2d_ps))
  print *, 'ps(prtb,min)', minval(v2dg(:,:,iv2d_ps)),minloc(v2dg(:,:,iv2d_ps))
  deallocate( v3dp, v2dp )
  end if
  nn=size(idvars)
  nvarall=1+nn*nlev
  print *, 'nvarall=',nvarall
  ! calculate spectrm
  call spectral_init
  allocate( grid(lngrd,nlev) )
  allocate( coef(lnwav,nlev) )
  allocate( buf4(lnwav) )
  ! output
  write(ofile(12:13),'(i2.2)') nint(fhour)
  write(cfile(12:13),'(i2.2)') nint(fhour)
  open(55,file=ofile,form='unformatted',access='direct',recl=4*lnwav)
  n=1
  irec=1
  do j=1,jgrd1
    do i=1,igrd1
      grid(i+(j-1)*igrd1,n)=v2dg(i,j,iv2d_ps)
    end do
  end do
  call gdtocc(grid(:,1),coef(:,1),1)
  print *, 'ps(grid,max)', maxval(grid(:,1)),maxloc(grid(:,1))
  print *, 'ps(grid,min)', minval(grid(:,1)),minloc(grid(:,1))
  print *, 'ps(coef,max)', maxval(coef(:,1)),maxloc(coef(:,1))
  print *, 'ps(coef,min)', minval(coef(:,1)),minloc(coef(:,1))
  buf4 = real(coef(:,1),kind=sp)
  write(55,rec=irec) buf4
  irec=irec+1
  do n=1,nn
    do k=1,nlev
      do j=1,jgrd1
        do i=1,igrd1
          grid(i+(j-1)*igrd1,k) = v3dg(i,j,k,idvars(n))
        end do
      end do
    end do
    if(idvars(n)==iv3d_u) then
      !u
      call gdtosc(grid,coef,nlev)
    else if(idvars(n)==iv3d_v) then
      !v
      call gdtocs(grid,coef,nlev)
    else
      !others
      call gdtocc(grid,coef,nlev)
    end if
    do k=1,nlev
      print*, k
      print *, cvars(n),'(grid,max)', maxval(grid(:,k)),maxloc(grid(:,k))
      print *, cvars(n),'(grid,min)', minval(grid(:,k)),minloc(grid(:,k))
      print *, cvars(n),'(coef,max)', maxval(coef(:,k)),maxloc(coef(:,k))
      print *, cvars(n),'(coef,min)', minval(coef(:,k)),minloc(coef(:,k))
      buf4 = real(coef(:,k),kind=sp)
      write(55,rec=irec) buf4
      irec=irec+1
    end do
  end do
  close(55)
  open(60,file=cfile)
  call genctl(60,ofile)
  close(60)

  call spectral_clean
  deallocate( v3dg,v2dg )
  deallocate( grid,coef )
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
    write(nctl,108) iwav1
 108  format('xdef',I5,' linear 0 1')
    write(nctl,110) jwav1
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
    write(nctl,'(a)') 'vars 10'
    else
    write(nctl,'(a)') 'vars 7'
    end if
    write(nctl,'(a)') 'ps 0 99 surface pressure [Pa]'
    write(nctl,'(a,i5,a)') 't ',nlev,' 99 temperature [K]'
    write(nctl,'(a,i5,a)') 'u ',nlev,' 99 zonal wind [m/s]'
    write(nctl,'(a,i5,a)') 'v ',nlev,' 99 meridional wind [m/s]'
    write(nctl,'(a,i5,a)') 'q ',nlev,' 99 specific humidity [g/kg]'
    write(nctl,'(a,i5,a)') 'oz ',nlev,' 99 ozone mixing ratio'
    write(nctl,'(a,i5,a)') 'cw ',nlev,' 99 cloud water'
    if(nonhyd.eq.1) then
    write(nctl,'(a,i5,a)') 'pn ',nlev,' 99 full log pressure'
    write(nctl,'(a,i5,a)') 'wn ',nlev,' 99 vertical velocity'
    end if
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end program
