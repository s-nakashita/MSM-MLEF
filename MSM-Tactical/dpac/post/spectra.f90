program spectra
!
! calculate full or perturbation power spectrum
!
  use kind_module
  use phconst_module
  use rsmcom_module
  use read_module
  use spectral_module
  implicit none
  logical :: lprtb=.true. ! False=>calculate for full field
  namelist /namlst_spectra/ lprtb
  ! for calculation
  real(kind=dp), allocatable :: u(:,:,:),v(:,:,:),t(:,:,:),q(:,:,:)
  real(kind=dp), allocatable :: theta(:,:,:),fact(:,:,:)
  real(kind=dp), allocatable :: ps(:,:)
  real(kind=dp), allocatable :: grid(:,:)
  real(kind=dp), allocatable :: coef(:,:)
  real(kind=sp), allocatable :: buf4(:)
  integer :: nvarall
  integer :: ips,it,iu,iv,iq
  character(len=17) :: ofile='spectrum_fhNN.bin'
  character(len=17) :: cfile='spectrum_fhNN.ctl'
  ! input files
  character(len=4) :: inf1='base' !base
  character(len=4) :: inf2='prtb' !prtb+base
  integer, parameter :: nisig=11, nisfc=21, niflx=31
  integer            :: nsig,     nsfc,     nflx
  real(kind=dp), allocatable :: dfld(:,:,:)
  real(kind=dp), allocatable :: dummap(:,:),dumlon(:),dumlat(:)
  integer :: n,i,j,k

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
  allocate( dfld(igrd1,jgrd1,nflds) )
  allocate( dummap(lngrd,3),dumlon(lngrd),dumlat(lngrd) )
  allocate( u(igrd1,jgrd1,nlev),v(igrd1,jgrd1,nlev) )
  allocate( t(igrd1,jgrd1,nlev),q(igrd1,jgrd1,nlev) )
  allocate( theta(igrd1,jgrd1,nlev),fact(igrd1,jgrd1,nlev) )
  allocate( ps(igrd1,jgrd1) )
  u=0.0d0
  v=0.0d0
  t=0.0d0
  q=0.0d0
  ps=0.0d0

  
  ips=2
  it=3
  iu=it+nlev
  iv=iu+nlev
  iq=iv+nlev
  ! base
  nsig=nisig
  open(nsig,file=inf1//'.sig.grd',form='unformatted',access='sequential',action='read')
  call read_sig(nsig,igrd1,jgrd1,nlev,nflds,nonhyd,icld,fhour,&
          sig,dfld,dummap,dumlat,dumlon)
  close(nsig)
  nvarall=0
  !ps
  nvarall=nvarall+1
  do j=1,jgrd1
    do i=1,igrd1
      ps(i,j)=dfld(i,j,ips)
    end do
  end do
  !te
  nvarall=nvarall+nlev
  do k=1,nlev
    do j=1,jgrd1
      do i=1,igrd1
        t(i,j,k) = dfld(i,j,it+k-1)
!        theta(i,j,k) = t(i,j,k)*(p0/dfld(i+ilonw-1,j+jlats-1,ips)/sig(k))**ptheta
      end do
    end do
  end do
  !u
  nvarall=nvarall+nlev
  do k=1,nlev
    do j=1,jgrd1
      do i=1,igrd1
        u(i,j,k) = dfld(i,j,iu+k-1)
      end do
    end do
  end do
  !v
  nvarall=nvarall+nlev
  do k=1,nlev
    do j=1,jgrd1
      do i=1,igrd1
        v(i,j,k) = dfld(i,j,iv+k-1)
      end do
    end do
  end do
  !q
  nvarall=nvarall+nlev
  do k=1,nlev
    do j=1,jgrd1
      do i=1,igrd1
        q(i,j,k) = dfld(i,j,iq+k-1)*1.0e3 ![g/kg]
      end do
    end do
  end do
  print *, 'nvarall=',nvarall
  do k=1,nlev
    print*, k
    print *, 'u(full,max)', maxval(u(:,:,k)),maxloc(u(:,:,k))
    print *, 'u(full,min)', minval(u(:,:,k)),minloc(u(:,:,k))
    print *, 'v(full,max)', maxval(v(:,:,k)),maxloc(v(:,:,k))
    print *, 'v(full,min)', minval(v(:,:,k)),minloc(v(:,:,k))
    print *, 't(full,max)', maxval(t(:,:,k)),maxloc(t(:,:,k))
    print *, 't(full,min)', minval(t(:,:,k)),minloc(t(:,:,k))
    print *, 'q(full,max)', maxval(q(:,:,k)),maxloc(q(:,:,k))
    print *, 'q(full,min)', minval(q(:,:,k)),minloc(q(:,:,k))
  end do
  print *, 'ps(full,max)', maxval(ps(:,:)),maxloc(ps(:,:))
  print *, 'ps(full,min)', minval(ps(:,:)),minloc(ps(:,:))
  if(lprtb) then
  ! perturbation
    nsig=nisig
    open(nsig,file=inf2//'.sig.grd',form='unformatted',access='sequential',action='read')
    call read_sig(nsig,igrd1,jgrd1,nlev,nflds,nonhyd,icld,fhour&
          ,sig,dfld,dummap,dumlat,dumlon)
    close(nsig)
    do j=1,jgrd1
      do i=1,igrd1
       ps(i,j)=ps(i,j)-dfld(i,j,ips)
      end do
    end do
    do k=1,nlev
      do j=1,jgrd1
        do i=1,igrd1
          t(i,j,k) = t(i,j,k)-dfld(i,j,it+k-1)
          !t(i,j,k) = dfld(i+ilonw-1,j+jlats-1,it+k-1)
          !theta(i,j,k) = theta(i,j,k) - t(i,j,k)*(p0/dfld(i+ilonw-1,j+jlats-1,ips)/sig(k))**ptheta
        end do
      end do
    end do
    do k=1,nlev
      do j=1,jgrd1
        do i=1,igrd1
          u(i,j,k) = u(i,j,k)-dfld(i,j,iu+k-1)
        end do
      end do
    end do
    do k=1,nlev
      do j=1,jgrd1
        do i=1,igrd1
          v(i,j,k) = v(i,j,k)-dfld(i,j,iv+k-1)
        end do
      end do
    end do
    do k=1,nlev
      do j=1,jgrd1
        do i=1,igrd1
          q(i,j,k) = q(i,j,k)-dfld(i,j,iq+k-1)*1.0e3 ![g/kg]
        end do
      end do
    end do
    do k=1,nlev
      print*, k
      print *, 'u(prtb,max)', maxval(u(:,:,k)),maxloc(u(:,:,k))
      print *, 'u(prtb,min)', minval(u(:,:,k)),minloc(u(:,:,k))
      print *, 'v(prtb,max)', maxval(v(:,:,k)),maxloc(v(:,:,k))
      print *, 'v(prtb,min)', minval(v(:,:,k)),minloc(v(:,:,k))
      print *, 't(prtb,max)', maxval(t(:,:,k)),maxloc(t(:,:,k))
      print *, 't(prtb,min)', minval(t(:,:,k)),minloc(t(:,:,k))
      print *, 'q(prtb,max)', maxval(q(:,:,k)),maxloc(q(:,:,k))
      print *, 'q(prtb,min)', minval(q(:,:,k)),minloc(q(:,:,k))
    end do
    print *, 'ps(prtb,max)', maxval(ps(:,:)),maxloc(ps(:,:))
    print *, 'ps(prtb,min)', minval(ps(:,:)),minloc(ps(:,:))
  end if
  allocate( grid(lngrd,nvarall) )
  n=1
  do j=1,jgrd1
    do i=1,igrd1
     grid(i+(j-1)*igrd1,n)=ps(i,j)
    end do
  end do
  do k=1,nlev
    n=n+1
    do j=1,jgrd1
      do i=1,igrd1
        grid(i+(j-1)*igrd1,n) = t(i,j,k)
      end do
    end do
  end do
  do k=1,nlev
    n=n+1
    do j=1,jgrd1
      do i=1,igrd1
        grid(i+(j-1)*igrd1,n) = q(i,j,k)
      end do
    end do
  end do
  do k=1,nlev
    n=n+1
    do j=1,jgrd1
      do i=1,igrd1
        grid(i+(j-1)*igrd1,n) = u(i,j,k)
      end do
    end do
  end do
  do k=1,nlev
    n=n+1
    do j=1,jgrd1
      do i=1,igrd1
        grid(i+(j-1)*igrd1,n) = v(i,j,k)
      end do
    end do
  end do
  ! calculate spectrm
  allocate( coef(lnwav,nvarall) )
  allocate( buf4(lnwav) )
  call spectral_init
  !ps,te,q
  call gdtocc(grid(:,1:2*nlev+1),coef(:,1:2*nlev+1),2*nlev+1)
  !u
  call gdtosc(grid(:,2*nlev+2:3*nlev+1),coef(:,2*nlev+2:3*nlev+1),nlev)
  !v
  call gdtocs(grid(:,3*nlev+2:),coef(:,3*nlev+2:),nlev)
  print *, 'ps(coef,max)', maxval(coef(:,1)),maxloc(coef(:,1))
  print *, 'ps(coef,min)', minval(coef(:,1)),minloc(coef(:,1))
  do k=1,nlev
      print*, k
      print *, 't(coef,max)', maxval(coef(:,1+k)),maxloc(coef(:,k))
      print *, 't(coef,min)', minval(coef(:,1+k)),minloc(coef(:,k))
      print *, 'q(coef,max)', maxval(coef(:,1+nlev+k)),maxloc(coef(:,1+nlev+k))
      print *, 'q(coef,min)', minval(coef(:,1+nlev+k)),minloc(coef(:,1+nlev+k))
      print *, 'u(coef,max)', maxval(coef(:,1+2*nlev+k)),maxloc(coef(:,1+2*nlev+k))
      print *, 'u(coef,min)', minval(coef(:,1+2*nlev+k)),minloc(coef(:,1+2*nlev+k))
      print *, 'v(coef,max)', maxval(coef(:,1+3*nlev+k)),maxloc(coef(:,1+3*nlev+k))
      print *, 'v(coef,min)', minval(coef(:,1+3*nlev+k)),minloc(coef(:,1+3*nlev+k))
  end do
  ! output
  write(ofile(12:13),'(i2.2)') nint(fhour)
  write(cfile(12:13),'(i2.2)') nint(fhour)
  open(55,file=ofile,form='unformatted',access='direct',recl=4*lnwav)
  do n=1,nvarall
    buf4 = real(coef(:,n),kind=sp)
    write(55,rec=n) buf4
  end do
  close(55)
  open(60,file=cfile)
  call genctl(60,ofile)
  close(60)

  call spectral_clean
  deallocate( dfld,u,v,t,q,ps,theta,fact ) 
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
    write(nctl,'(a)') 'vars 5'
    write(nctl,'(a)') 'ps 0 99 surface pressure [Pa]'
    write(nctl,'(a,i5,a)') 't ',nlev,' 99 temperature [K]'
    write(nctl,'(a,i5,a)') 'q ',nlev,' 99 specific humidity [g/kg]'
!    write(nctl,'(a)') 'oz 1 99 ozone mixing ratio'
!    write(nctl,'(a)') 'cw 1 99 cloud water'
!    write(nctl,'(a)') 'pn 1 99 full log pressure'
!    write(nctl,'(a)') 'tn 1 99 full virtual temperature'
!    write(nctl,'(a)') 'wn 1 99 vertical velocity'
    write(nctl,'(a,i5,a)') 'u ',nlev,' 99 zonal wind [m/s]'
    write(nctl,'(a,i5,a)') 'v ',nlev,' 99 meridional wind [m/s]'
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end program
