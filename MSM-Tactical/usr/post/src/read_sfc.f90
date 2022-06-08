program read_sfc
!
! main program : read_sfc   read surface file and convert into 
!                           readable file with GrADS
! history:
! 22-06-09 create
! 
! namelist:
!
! input :
! unit 11 sfc file (sequential, with header)
! 
! output:
! unit 51 sfc file (direct, no header)
!
  implicit none
  integer, parameter :: iunit=11, ounit=51, cunit=61
  character(len=8) :: label(4)
  integer :: idate(4), iymdh
  integer :: igrd1, jgrd1, version
  integer :: iret
  integer, parameter :: lsoil=2, nflds=26
  real(kind=4) :: fhour
  integer :: nwf, irec
  integer :: i,j,k,l, ifld
  integer :: itsea, ismc, isheleg, istc, itg3, izorl, icv, icvb, &
&            icvt, islmsk, ivfrac, if10m, icanopy, ivtype, istype, iuustar, iffmm, &
&            iffhh, ialvsf, ialvwf, ialnsf, ialnwf, ifacsf, ifacwf
  real(kind=4), allocatable :: sfld(:), sfldl(:), dfld(:,:,:)
  real(kind=4), allocatable :: tmps2(:,:), tmps4(:,:)
  real(kind=4), parameter :: rd=2.8705e2, rv=4.6150e2, fvirt=rv/rd-1.0
  real(kind=4), parameter :: pi=3.141592, rad2deg=180.0/pi
  
  print *, 'read and extract header record'
  iret = 0
  rewind(iunit)
! read label
  read(iunit) label
  print '(4a8)', label
! read header
  read(iunit) fhour, idate, igrd1, jgrd1, version
  iymdh = idate(4)*1000000+idate(2)*10000+idate(3)*100+idate(1)
  print *, 'posting date ', iymdh, '+', nint(fhour)
  print *, 'igrd1, jgrd1'
  print '(2i4)', igrd1, jgrd1
  print *, 'version', version
  nwf = igrd1*jgrd1
  print *, 'nwf', nwf
  allocate( sfld(nwf), sfldl(nwf*lsoil), dfld(igrd1,jgrd1,nflds) )
  allocate( tmps2(nwf,2), tmps4(nwf,4) )
  print *, 'start reading and writing data'
  ifld = 1
! tsea
  itsea = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read tsea ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! smc
  ismc = ifld
  read(iunit) (sfldl(i), i=1,nwf*lsoil)
  l=1
  do k=1,lsoil
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,ifld) = sfldl(l)
        l=l+1
      end do
    end do
    print *,ifld, 'read smc ', 'soil_l=',k, dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
    ifld=ifld+1
  end do 
! sheleg
  isheleg = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read sheleg ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! stc
  istc = ifld
  read(iunit) (sfldl(i), i=1,nwf*lsoil)
  l=1
  do k=1,lsoil
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,ifld) = sfldl(l)
        l=l+1
      end do
    end do
    print *,ifld, 'read stc ', 'soil_l=',k, dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
    ifld=ifld+1
  end do 
! tg3
  itg3 = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read tg3 ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! zorl
  izorl = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read zorl ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! cv
  icv = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read cv ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! cvb
  icvb = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read cvb ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! cvt
  icvt = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read cvt ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! albedo
  read(iunit) tmps4
  k=1
  ialvsf = ifld
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = tmps4(l,k)
      l=l+1
    end do
  end do
  print *,ifld, 'read alvsf ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
  k=k+1
  ialvwf = ifld
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = tmps4(l,k)
      l=l+1
    end do
  end do
  print *,ifld, 'read alvwf ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
  k=k+1
  ialnsf = ifld
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = tmps4(l,k)
      l=l+1
    end do
  end do
  print *,ifld, 'read alnsf ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
  k=k+1
  ialnwf = ifld
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = tmps4(l,k)
      l=l+1
    end do
  end do
  print *,ifld, 'read alnwf ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! slmsk
  islmsk = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read slmsk ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! vfrac
  ivfrac = ifld
  read(iunit) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read vfrac ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! canopy
  icanopy = ifld
  read(iunit,err=5000) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read canopy ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! f10m
  if10m = ifld
  read(iunit,err=5000) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read f10m ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! vtype
  ivtype = ifld
  read(iunit,err=5000) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read vtype ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! stype
  istype = ifld
  read(iunit,err=5000) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read stype ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
! facswf
  read(iunit,err=5000) tmps2
  k=1
  ifacsf = ifld
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = tmps2(l,k)
      l=l+1
    end do
  end do 
  print *,ifld, 'read facsf ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld=ifld+1
  k=k+1
  ifacwf = ifld
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = tmps2(l,k)
      l=l+1
    end do
  end do 
  print *,ifld, 'read facwf ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
  ifld = ifld + 1
!
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = 1.
      dfld(i,j,ifld+1) = log(30.)
      dfld(i,j,ifld+1) = log(30.)
    end do
  end do
! uustar
  iuustar = ifld
  read(iunit,end=200) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read uustar ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
200 continue
  ifld=ifld+1
! ffmm
  iffmm = ifld
  read(iunit,end=201) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read ffmm ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
201 continue
  ifld=ifld+1
! ffhh
  iffhh = ifld
  read(iunit,end=202) (sfld(i), i=1,nwf)
  l=1
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ifld) = sfld(l)
      l=l+1
    end do
  end do 
  print *,ifld, 'read ffhh ', dfld(1,1,ifld), maxval(dfld(:,:,ifld)), minval(dfld(:,:,ifld))
202 continue
! end reading
  print *, 'ifld', ifld, 'nflds ', nflds
  print *, 'start write output'
  ! open output
  open(ounit, FORM='unformatted', access='direct',&
&      convert='big_endian', recl=4*nwf)
  irec=1
  !tsea
  write(ounit, rec=irec) dfld(:,:,itsea)
  irec=irec+1
  !smc
  do k=1,lsoil
    write(ounit, rec=irec) dfld(:,:,ismc+k-1)
    irec=irec+1
  end do
  !sheleg
  write(ounit, rec=irec) dfld(:,:,isheleg)
  irec=irec+1
  !stc
  do k=1,lsoil
    write(ounit, rec=irec) dfld(:,:,istc+k-1)
    irec=irec+1
  end do
  !tg3
  write(ounit, rec=irec) dfld(:,:,itg3)
  irec=irec+1
  !zorl
  write(ounit, rec=irec) dfld(:,:,izorl)
  irec=irec+1
  !cv
  write(ounit, rec=irec) dfld(:,:,icv)
  irec=irec+1
  !cvb
  write(ounit, rec=irec) dfld(:,:,icvb)
  irec=irec+1
  !cvt
  write(ounit, rec=irec) dfld(:,:,icvt)
  irec=irec+1
  !albedo
  write(ounit, rec=irec) dfld(:,:,ialvsf)
  irec=irec+1
  write(ounit, rec=irec) dfld(:,:,ialvwf)
  irec=irec+1
  write(ounit, rec=irec) dfld(:,:,ialnsf)
  irec=irec+1
  write(ounit, rec=irec) dfld(:,:,ialnwf)
  irec=irec+1
  !slmsk
  write(ounit, rec=irec) dfld(:,:,islmsk)
  irec=irec+1
  !vfrac
  write(ounit, rec=irec) dfld(:,:,ivfrac)
  irec=irec+1
  !canopy
  write(ounit, rec=irec) dfld(:,:,icanopy)
  irec=irec+1
  !f10m
  write(ounit, rec=irec) dfld(:,:,if10m)
  irec=irec+1
  !vtype
  write(ounit, rec=irec) dfld(:,:,ivtype)
  irec=irec+1
  !stype
  write(ounit, rec=irec) dfld(:,:,istype)
  irec=irec+1
  !facswf
  write(ounit, rec=irec) dfld(:,:,ifacsf)
  irec=irec+1
  write(ounit, rec=irec) dfld(:,:,ifacwf)
  irec=irec+1
  !uustar
  write(ounit, rec=irec) dfld(:,:,iuustar)
  irec=irec+1
  !ffmm
  write(ounit, rec=irec) dfld(:,:,iffmm)
  irec=irec+1
  !ffhh
  write(ounit, rec=irec) dfld(:,:,iffhh)
  irec=irec+1
  close(ounit)
  print *, 'end write output'
  print *, 'generate control file'
  call genctl(cunit,idate,fhour)
  stop
5000 print *, ' error reading'
  call abort
contains
  subroutine genctl(nctl,idate,fhour)
    implicit none
    integer, intent(in) :: nctl
    integer, intent(in) :: idate(4)
    real(kind=4), intent(in) :: fhour
    integer :: i,j
    integer :: ihr,idy,imo,iyr
    integer :: days(12),daysl(12)
    data days/31,28,31,30,31,30,31,31,30,31,30,31/
    data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
    character(len=2) hour, day
    character(len=3) mon(12)
    data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
&            'JUL','AUG','SEP','OCT','NOV','DEC'/

    write(nctl,112) lsoil
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
    write(nctl,'(a)') 'vars 24'
    write(nctl,'(a)') 'tsea 0 99 land and sea surface temperature (K)'
    write(nctl,'(a,i2,a)') 'smc ',lsoil,' 99 two layers of soil moisture contents (0.47 - 0.1)'
    write(nctl,'(a)') 'sheleg 0 99 snow depth (cm)'
    write(nctl,'(a,i2,a)') 'stc ',lsoil,' 99 two layers of soil temperature (K)'
    write(nctl,'(a)') 'tg3 0 99 the lowest soil temperature at 3m (K)'
    write(nctl,'(a)') 'zorl 0 99 surface roughness (cm)'
    write(nctl,'(a)') 'cv 0 99 cloud amount'
    write(nctl,'(a)') 'cvb 0 99 cloud base (sigma layer number)'
    write(nctl,'(a)') 'cvt 0 99 cloud top (sigma layer number)'
    write(nctl,'(a)') 'alvsf 0 99 albedo (vis with strong cosz dependency)'
    write(nctl,'(a)') 'alvwf 0 99 albedo (vis with weak cosz dependency)'
    write(nctl,'(a)') 'alnsf 0 99 albedo (nir with strong cosz dependency)'
    write(nctl,'(a)') 'alnwf 0 99 albedo (nir with weak cosz dependency)'
    write(nctl,'(a)') 'slmsk 0 99 sea land mask (sea=0,land=1,ice=2)'
    write(nctl,'(a)') 'vfrac 0 99 vegetation coverage (%)'
    write(nctl,'(a)') 'canopy 0 99 surface canopy'
    write(nctl,'(a)') 'f10m 0 99 10m height factor for wind'
    write(nctl,'(a)') 'vtype 0 99 vegetation type'
    write(nctl,'(a)') 'stype 0 99 soil type'
    write(nctl,'(a)') 'facsf 0 99 fractional coverage with strong cosz dependency'
    write(nctl,'(a)') 'facwf 0 99 fractional coverage with weak cosz dependency'
    write(nctl,'(a)') 'ustar 0 99 frictional speed (m/s)'
    write(nctl,'(a)') 'ffmm 0 99 momentum exchange similarity function'
    write(nctl,'(a)') 'ffhh 0 99 heat exchange similarity function'
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end
