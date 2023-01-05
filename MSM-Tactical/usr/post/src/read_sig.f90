program readsig
!
! main program : read_sig   read sigma file and convert into 
!                           readable file with GrADS
! history:
! 22-05-19 create
! 22-07-28 add phys3d
! 22-09-14 replaced by module
! 
! namelist:
!
! input :
! unit 11 sigma file (sequential, with header)
! 
! output:
! unit 51 sigma file (direct, no header)
!
  use kind_module
  use phconst_module
  use read_module
  implicit none
  integer, parameter :: iunit=11, ounit=51, cunit=61
  character(len=8) :: label(4)
  integer :: idate(4), iymdh
  integer :: iret
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=sp) :: fhour, ext(nwext) 
  integer :: icld=1 !1=include 3D physics, 0=not include
  namelist /namlst_cld/ icld
  ! components of ext
  integer :: iwav1,jwav1,igrd1,jgrd1,levs,nfldx,proj,nonhyd
  real(kind=sp) :: rtruth, rorient, rcenlat, rcenlon, rgrdlft, rgrdbtm, &
 &                delx, dely
  integer :: nflds, nwf, irec
  integer :: i,j,k,l,m
  integer :: igz, ips, it, iu, iv, iq, ioz, icw, ipn, itn, iwn, &
 &           im2
  integer :: iphys3d(3)
  real(kind=dp), allocatable :: dfld(:,:,:), mapf(:,:,:)
  real(kind=dp), allocatable :: clat(:), clon(:), factor(:,:,:)
  real(kind=sp), allocatable :: buf4(:,:)
  
  ! read namelist
  read(5,nml=namlst_cld)
  write(6,nml=namlst_cld)

  print *, 'read and extract header record'
  call read_header(iunit,icld,label,idate,fhour,si,sl,ext,nflds)
  print '(4a8)', label
  iymdh = idate(4)*1000000+idate(2)*10000+idate(3)*100+idate(1)
  print *, 'posting date ', iymdh, '+', nint(fhour)
  iwav1 = int(ext(1)); jwav1 = int(ext(2))
  igrd1 = int(ext(3)); jgrd1 = int(ext(4))
  levs  = int(ext(5)); nfldx = int(ext(6))
  proj  = int(ext(7))
  rtruth= ext(8); rorient=ext(9)
  rcenlat=ext(10); rcenlon=ext(11)
  rgrdlft=ext(12); rgrdbtm=ext(13)
  delx  = ext(14); dely = ext(15)
  nonhyd= int(ext(16))
  print *, 'iwav1, jwav1'
  print '(2i4)', iwav1, jwav1
  print *, 'igrd1, jgrd1'
  print '(2i4)', igrd1, jgrd1
  print *, 'levs , nfldx'
  print '(2i4)', levs , nfldx
  print '(a,i4)', 'proj ', proj
  print *, 'rtruth, rorient, rcenlat, rcenlon'
  print '(4f9.3)', rtruth, rorient, rcenlat, rcenlon
  print *, 'rgrdlft, rgrdbtm, delx, dely'
  print '(4f9.3)', rgrdlft, rgrdbtm, delx, dely
  if ( nonhyd.eq.1 ) then
    print *, 'Input is a nonhydrostatic'
  else
    print *, 'Input is a hydrostatic'
  end if
  print *, 'si', si(1:levs+1)
  print *, 'sl', sl(1:levs)
  print *, 'nflds', nflds
  nwf = igrd1*jgrd1
  print *, 'nwf', nwf
  allocate( dfld(igrd1,jgrd1,nflds), mapf(igrd1,jgrd1,3) )
  allocate( clat(jgrd1),clon(igrd1), factor(igrd1,jgrd1,levs) )
  allocate( buf4(igrd1,jgrd1) )
  print *, 'start reading and writing data'
  call read_sig(iunit,igrd1,jgrd1,levs,nflds,nonhyd,icld,fhour,sl,&
    dfld,mapf,clat,clon)
  igz=1
  ips=igz+1
  it=ips+1
  iu=it+levs
  iv=iu+levs
  iq=iv+levs
  ioz=iq+levs
  icw=ioz+levs
  ! conversion from virtual temperature to temperature is already done in read_sig
  !! factor to fully modify the virtual temperature
  !do k=1,levs
  !  do j=1,jgrd1
  !    do i=1,igrd1
  !      factor(i,j,k) = 1.0+fvirt*dfld(i,j,iq+k-1)
  !    end do
  !  end do
  !end do
  ipn=icw+levs
  itn=ipn+levs
  !if(nonhyd.eq.1) then
  !  do k=1,levs
  !    do j=1,jgrd1
  !      do i=1,igrd1
  !        dfld(i,j,itn+k-1) = dfld(i,j,itn+k-1)/factor(i,j,k)
  !      end do
  !    end do
  !  end do
  !end if
  iwn=itn+levs
  if(icld==1.and.fhour > 0.0) then
  iphys3d(1)=iwn+1
  do m=1,3
    if(m<3) then
      iphys3d(m+1)=iphys3d(m)+levs
    end if
  end do
  end if
  print *, 'start write output'
  ! open output
  open(ounit, FORM='unformatted', access='direct',&
&      convert='big_endian', recl=4*nwf)
  irec=1
  !gz
  buf4 = real(dfld(:,:,igz),kind=sp)
  write(ounit, rec=irec) buf4
  irec=irec+1
  !ps
  buf4 = real(dfld(:,:,ips),kind=sp)
  write(ounit, rec=irec) buf4
  irec=irec+1
  !t
  do k=1,levs
    buf4 = real(dfld(:,:,it+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !u
  do k=1,levs
    buf4 = real(dfld(:,:,iu+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !v
  do k=1,levs
    buf4 = real(dfld(:,:,iv+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !q
  do k=1,levs
    buf4 = real(dfld(:,:,iq+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !oz
  do k=1,levs
    buf4 = real(dfld(:,:,ioz+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !cw
  do k=1,levs
    buf4 = real(dfld(:,:,icw+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !pn
  do k=1,levs
    buf4 = real(dfld(:,:,ipn+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !tn
  do k=1,levs
    buf4 = real(dfld(:,:,itn+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !wn(k=1)
  buf4 = real(dfld(:,:,iwn),kind=sp)
  write(ounit, rec=irec) buf4
  irec=irec+1
  !wn(k>1)
  do k=2,levs+1
    buf4 = real(dfld(:,:,iwn+k-1),kind=sp)
    write(ounit, rec=irec) buf4
    irec=irec+1
  end do
  !phys3d
  if(icld==1.and.fhour > 0.0) then
  do m=1,3
    do k=1,levs
      buf4 = real(dfld(:,:,iphys3d(m)+k-1),kind=sp)
      write(ounit, rec=irec) buf4
      irec=irec+1
    end do
  end do
  end if
  close(ounit)
  print *, 'end write output'
  print *, 'generate control file'
  call genctl(cunit,igrd1,jgrd1,levs,proj,idate,fhour,&
&             clat,clon,nonhyd,icld)
  stop
contains
  subroutine genctl(nctl,igrd1,jgrd1,levs,proj,idate,fhour,&
& clat,clon,nonhyd,icld)
    implicit none
    integer, intent(in) :: nctl,igrd1,jgrd1,levs,proj,nonhyd,icld
    integer, intent(in) :: idate(4)
    real(kind=sp), intent(in) :: fhour
    real(kind=dp), intent(in) :: clat(:), clon(:)
    integer :: i,j
    integer :: ihr,idy,imo,iyr
    integer :: days(12),daysl(12)
    data days/31,28,31,30,31,30,31,31,30,31,30,31/
    data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
    character(len=2) hour, day
    character(len=3) mon(12)
    data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
&            'JUL','AUG','SEP','OCT','NOV','DEC'/

    write(nctl,'(a)') 'dset ^DATAFILE'
    write(nctl,'(a)') 'options big_endian'
    write(nctl,'(a)') 'undef -9.99E+33'
    if (proj .eq. 0) then
      write(nctl,108) igrd1,clon(1),clon(2)-clon(1)
 108  format('xdef',I5,' linear',2G14.6)
      write(nctl,110) jgrd1
 110  format('ydef',I5,' levels')
      write(nctl,111) (clat(j),j=1,jgrd1)
 111  format(5G14.6)
    end if
    write(nctl,112) levs
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
    if(icld==1.and.fhour > 0.0) then
    write(nctl,'(a)') 'vars 15'
    else
    write(nctl,'(a)') 'vars 12'
    end if
    write(nctl,'(a)') 'gz 0 99 surface geopotential'
    write(nctl,'(a)') 'ps 0 99 surface pressure'
    write(nctl,'(a,i2,a)') 't ',levs,' 99 virtual temperature'
    write(nctl,'(a,i2,a)') 'u ',levs,' 99 x-wind'
    write(nctl,'(a,i2,a)') 'v ',levs,' 99 y-wind'
    write(nctl,'(a,i2,a)') 'q ',levs,' 99 specific humidity'
    write(nctl,'(a,i2,a)') 'oz ',levs,' 99 ozone mixing ratio'
    write(nctl,'(a,i2,a)') 'cw ',levs,' 99 cloud water'
    if (nonhyd.eq.1) then
    write(nctl,'(a,i2,a)') 'pn ',levs,' 99 pressure(nonhydro)'
    write(nctl,'(a,i2,a)') 'tn ',levs,' 99 temperature(nonhydro)'
    else
    write(nctl,'(a,i2,a)') 'pn ',levs,' 99 pressure(hydro)'
    write(nctl,'(a,i2,a)') 'tn ',levs,' 99 temperature(hydro)'
    end if
    write(nctl,'(a)') 'w0 0 99 surface vertical velocity'
    write(nctl,'(a,i2,a)') 'wn ',levs,' 99 vertical velocity'
  if(icld==1.and.fhour > 0.0) then
    write(nctl,'(a,i2,a)') 'f_ice ',levs,' 99 ice fraction'
    write(nctl,'(a,i2,a)') 'f_rain ',levs,' 99 rain fraction'
    write(nctl,'(a,i2,a)') 'f_rimef ',levs,' 99 mixed fraction'
  end if
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end 
