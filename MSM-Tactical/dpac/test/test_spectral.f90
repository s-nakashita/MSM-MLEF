program test_spectral
  use kind_module
  use rsmcom_module
  use read_module
  use spectral_module
  implicit none
  character(len=4), parameter :: sigf='init'
  character(len=12) :: cfile
  integer :: nsig
  integer :: ntrunc
  real(kind=sp) :: ext(nwext) 
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=dp), allocatable :: dfld(:,:,:),clat(:),clon(:),map(:,:,:)
  real(kind=dp), allocatable :: grid(:,:),coef(:,:),grid2(:,:)
  real(kind=sp), allocatable :: buf4(:)
  real(kind=dp) :: l2norm,linfnorm
  integer :: i,j,k,km,ilev

  call set_rsmparm(sigf)
  ntrunc=100
  call spectral_init(ntrunc=ntrunc)
  nsig=11
  cfile=sigf//'.sig.grd'
  open(nsig,file=cfile,form='unformatted',access='sequential',action='read')
  call read_header(nsig,icld,label,idate,fhour,si,sl,ext,nflds)
  allocate( dfld(igrd1,jgrd1,nflds), &
   & map(igrd1,jgrd1,3), & !map factor
   & clat(jgrd1),clon(igrd1) )
  call read_sig(nsig,igrd1,jgrd1,nlev,nflds,nonhyd,icld,fhour,sl,&
                  & dfld,map,clat,clon,convert=.false.)
  close(nsig)
  km=9 !gz,lnps,te,q,oz,cw,pn,tn,wn
  ilev=1
  allocate( grid(lngrd,km+2), coef(lnwav,km+2), grid2(lngrd,km+2) )
  do j=1,jgrd1
    do i=1,igrd1
      grid((j-1)*igrd1+i,1) = real(dfld(i,j,1),kind=dp) !gz
      grid((j-1)*igrd1+i,2) = real(dfld(i,j,2),kind=dp) !lnps
      grid((j-1)*igrd1+i,3) = real(dfld(i,j,2       +ilev),kind=dp) !te
      grid((j-1)*igrd1+i,4) = real(dfld(i,j,2+3*nlev+ilev),kind=dp) !q
      grid((j-1)*igrd1+i,5) = real(dfld(i,j,2+4*nlev+ilev),kind=dp) !oz
      grid((j-1)*igrd1+i,6) = real(dfld(i,j,2+5*nlev+ilev),kind=dp) !cw
      grid((j-1)*igrd1+i,7) = real(dfld(i,j,2+6*nlev+ilev),kind=dp) !pn
      grid((j-1)*igrd1+i,8) = real(dfld(i,j,2+7*nlev+ilev),kind=dp) !tn
      grid((j-1)*igrd1+i,9) = real(dfld(i,j,2+8*nlev+ilev),kind=dp) !wn
      grid((j-1)*igrd1+i,10) = real(dfld(i,j,2+  nlev+ilev),kind=dp) !du
      grid((j-1)*igrd1+i,11) = real(dfld(i,j,2+2*nlev+ilev),kind=dp) !zv
    end do
  end do
  grid2 = grid
  !cos-cos
  call gdtocc(grid2(:,1:km),coef(:,1:km),km)
  call cctogd(coef(:,1:km),grid2(:,1:km),km)
  !sin-cos(u)
  k=km+1
  call gdtosc(grid2(:,k),coef(:,k),1)
  call sctogd(coef(:,k),grid2(:,k),1)
  !cos-sin(v)
  k=km+2
  call gdtocs(grid2(:,k),coef(:,k),1)
  call cstogd(coef(:,k),grid2(:,k),1)
  do k=1,km+2
    print *, "k ",k," grid ", maxval(grid(:,k)), minval(grid(:,k))
    print *, "k ",k," coef ", maxval(coef(:,k))," at ", maxloc(coef(:,k)),&
            minval(coef(:,k))," at ",minloc(coef(:,k))
    print *, "k ",k," grid ", maxval(grid2(:,k)), minval(grid2(:,k))
    l2norm=0.0d0
    do i=1,lngrd
      l2norm=l2norm+(grid(i,k)-grid2(i,k))**2
    end do
    print *, "l2norm=",sqrt(l2norm)
    linfnorm=0.0d0
    do i=1,lngrd
      linfnorm=max(linfnorm,abs(grid(i,k)-grid2(i,k)))
    end do
    print *, "linfnorm=",linfnorm
  end do
  ! output
  allocate( buf4(lngrd) )
  nsig=51
  cfile=sigf//'.old.bin'
  open(nsig,file=cfile,form='unformatted',access='direct',recl=4*lngrd)
  i=0
  do k=1,km+2
    buf4 = real(grid(:,k),kind=sp)
    i=i+1
    write(nsig,rec=i) buf4
  end do
  close(nsig)
  nsig=61
  open(nsig,file=sigf//'.old.ctl')
  call genctl(nsig,cfile)
  close(nsig)
  nsig=51
  cfile=sigf//'.new.bin'
  open(nsig,file=cfile,form='unformatted',access='direct',recl=4*lngrd)
  i=0
  do k=1,km+2
    buf4 = real(grid2(:,k),kind=sp)
    i=i+1
    write(nsig,rec=i) buf4
  end do
  close(nsig)
  nsig=61
  open(nsig,file=sigf//'.new.ctl')
  call genctl(nsig,cfile)
  close(nsig)

  !! truncation
  !cos-cos
  call spectral_trunc(grid2(:,1:km),km,"cc")
  !sin-cos(u)
  k=km+1
  call spectral_trunc(grid2(:,k),1,"sc")
  !cos-sin(v)
  k=km+2
  call spectral_trunc(grid2(:,k),1,"cs")

  nsig=51
  cfile=sigf//'.trc.bin'
  open(nsig,file=cfile,form='unformatted',access='direct',recl=4*lngrd)
  i=0
  do k=1,km+2
    buf4 = real(grid2(:,k),kind=sp)
    i=i+1
    write(nsig,rec=i) buf4
  end do
  close(nsig)
  nsig=61
  open(nsig,file=sigf//'.trc.ctl')
  call genctl(nsig,cfile)
  close(nsig)

  call spectral_clean
  call clean_rsmparm
  !stop
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
    write(nctl,108) igrd1,clon(1),clon(2)-clon(1)
 108  format('xdef',I5,' linear',2G14.6)
    write(nctl,110) jgrd1
 110  format('ydef',I5,' levels')
    write(nctl,111) (clat(j),j=1,jgrd1)
 111  format(5G14.6)
    write(nctl,112) 1
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
    write(nctl,'(a,i4)') 'vars ',km+2
    write(nctl,'(a)') 'gz 1 99 terrain height'
    write(nctl,'(a)') 'lnps 1 99 log surface pressure'
    write(nctl,'(a)') 'te 1 99 virtual temperature'
    write(nctl,'(a)') 'rq 1 99 specific humidity'
    write(nctl,'(a)') 'oz 1 99 ozone mixing ratio'
    write(nctl,'(a)') 'cw 1 99 cloud water'
    write(nctl,'(a)') 'pn 1 99 full log pressure'
    write(nctl,'(a)') 'tn 1 99 full virtual temperature'
    write(nctl,'(a)') 'wn 1 99 vertical velocity'
    write(nctl,'(a)') 'du 1 99 zonal wind'
    write(nctl,'(a)') 'zv 1 99 meridional wind'
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end program test_spectral
