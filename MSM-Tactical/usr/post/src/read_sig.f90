program read_sig
!
! main program : read_sig   read sigma file and convert into 
!                           readable file with GrADS
! history:
! 22-05-19 create
! 
! namelist:
!
! input :
! unit 11 sigma file (sequential, with header)
! 
! output:
! unit 51 sigma file (direct, no header)
!
  implicit none
  integer, parameter :: iunit=11, ounit=51, cunit=61
  character(len=8) :: label(4)
  integer :: idate(4), iymdh
  integer :: iret
  integer, parameter :: levmax=100, nwext=512-(6+2*levmax)
  real(kind=4) :: fhour, si(levmax+1), sl(levmax), sisl(2*levmax+1), &
 &                ext(nwext) 
  ! components of ext
  integer :: iwav1,jwav1,igrd1,jgrd1,levs,nfldx,proj,nonhyd
  real(kind=4) :: rtruth, rorient, rcenlat, rcenlon, rgrdlft, rgrdbtm, &
 &                delx, dely
  integer :: nflds, nwf, irec
  integer :: i,j,k,l
  integer :: igz, ips, it, iu, iv, iq, ioz, icw, ipn, itn, iwn, &
 &           im2
  real(kind=4), allocatable :: sfld(:), dfld(:,:,:)
  real(kind=4), allocatable :: clat(:), clon(:), factor(:,:,:)
  real(kind=4), parameter :: rd=2.8705e2, rv=4.6150e2, fvirt=rv/rd-1.0
  real(kind=4), parameter :: pi=3.141592, rad2deg=180.0/pi
  
  print *, 'read and extract header record'
  iret = 0
  rewind(iunit)
! read label
  read(iunit) label
  print '(4a8)', label
! read header
  read(iunit) fhour, idate, sisl, ext
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
  si(1:levs+1) = sisl(1:levs+1)
  sl(1:levs)   = sisl(levs+2:2*levs+1)
  print *, 'si', si(1:levs+1)
  print *, 'sl', sl(1:levs)
  nflds = 8+13*levs
  print *, 'nflds', nflds
  nwf = igrd1*jgrd1
  print *, 'nwf', nwf
  allocate( sfld(nwf), dfld(igrd1,jgrd1,nflds) )
  allocate( clat(jgrd1),clon(igrd1), factor(igrd1,jgrd1,levs) )
  print *, 'start reading and writing data'
  l=1
  ! gz
  igz=1
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,igz) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,igz, 'read gz ', dfld(1,1,igz), maxval(dfld(:,:,igz)), minval(dfld(:,:,igz))
  ! ln(ps)
  ips=igz+1
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ips) = EXP(sfld(i+(j-1)*igrd1))
    end do
  end do 
  print *,ips, 'read ps ', dfld(1,1,ips), maxval(dfld(:,:,ips)), minval(dfld(:,:,ips))
  ! T
  it=ips+1
  do k=1, levs
    read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,it+k-1) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,it+k-1, 'read T at lev=',k, dfld(1,1,it+k-1),&
&  maxval(dfld(:,:,it+k-1)), minval(dfld(:,:,it+k-1))
  end do  
  ! U,V
  iu=it+levs
  iv=iu+levs
  do k=1, levs
    read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,iu+k-1) = sfld(i+(j-1)*igrd1)
    end do
  end do
    read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,iv+k-1) = sfld(i+(j-1)*igrd1)
    end do
  end do
  print *,iu+k-1, 'read U at lev=',k, dfld(1,1,iu+k-1),&
&  maxval(dfld(:,:,iu+k-1)), minval(dfld(:,:,iu+k-1))
  print *,iv+k-1, 'read V at lev=',k, dfld(1,1,iv+k-1),&
&  maxval(dfld(:,:,iv+k-1)), minval(dfld(:,:,iv+k-1))
  end do
  ! Q
  iq = iv+levs
  do k=1, levs
    read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,iq+k-1) = sfld(i+(j-1)*igrd1)
    end do
  end do
  print *,iq+k-1, 'read Q at lev=',k, dfld(1,1,iq+k-1), &
&  maxval(dfld(:,:,iq+k-1)), minval(dfld(:,:,iq+k-1))
  end do
  ! OZ
  ioz=iq+levs
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,ioz+k-1)=sfld(i+(j-1)*igrd1)
    end do
  end do
  print *,ioz+k-1, 'read OZ at lev=',k, dfld(1,1,ioz+k-1),&
&  maxval(dfld(:,:,ioz+k-1)), minval(dfld(:,:,ioz+k-1))
  end do
  ! CW
  icw=ioz+levs
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,icw+k-1)=sfld(i+(j-1)*igrd1)
    end do
  end do
  print *,icw+k-1, 'read CW at lev=',k, dfld(1,1,icw+k-1),&
&  maxval(dfld(:,:,icw+k-1)), minval(dfld(:,:,icw+k-1))
  end do
  ! factor to fully modify the virtual temperature
  do k=1,levs
    do j=1,jgrd1
      do i=1,igrd1
        factor(i,j,k) = 1.0+fvirt*dfld(i,j,iq+k-1)
      end do
    end do
  end do
  if (nonhyd.eq.1) then
  ! pn
  ipn=icw+levs
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,ipn+k-1) = EXP(sfld(i+(j-1)*igrd1))
        if (k.eq.1) then
          dfld(i,j,ips) = dfld(i,j,ipn)/sl(1)
        end if
      end do
    end do
  print *,ipn+k-1, 'read PN at lev=',k, dfld(1,1,ipn+k-1),&
&  maxval(dfld(:,:,ipn+k-1)), minval(dfld(:,:,ipn+k-1))
  end do
  ! tn
  itn=ipn+levs
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,itn+k-1) = sfld(i+(j-1)*igrd1)/factor(i,j,k)
      end do
    end do
  print *,itn+k-1, 'read TN at lev=',k, dfld(1,1,itn+k-1),&
&  maxval(dfld(:,:,itn+k-1)), minval(dfld(:,:,itn+k-1))
  end do  
  ! wn
  iwn=itn+levs
  do k=1,levs+1
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,iwn+k-1) = sfld(i+(j-1)*igrd1)
      end do
    end do
  print *,iwn+k-1, 'read WN at lev=',k, dfld(1,1,iwn+k-1),&
&  maxval(dfld(:,:,iwn+k-1)), minval(dfld(:,:,iwn+k-1))
  end do
  else
  ! hydro p
  ipn=icw+levs
  do k=1,levs
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,ipn+k-1) = dfld(i,j,ips) * sl(k)
      end do
    end do
  print *, 'calc P at lev=',k, maxval(dfld(:,:,ipn+k-1)), minval(dfld(:,:,ipn+k-1))
  end do
  ! hydro t
  itn=ipn+levs
  do k=1,levs
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,itn+k-1) = dfld(i,j,it+k-1)/factor(i,j,k)
      end do
    end do
  print *, 'calc T at lev=',k, maxval(dfld(:,:,itn+k-1)), minval(dfld(:,:,itn+k-1))
  itn=itn+1
  end do  
  ! w=0
  iwn=itn+levs
  do k=1,levs+1
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,iwn+k-1) = 0.0
      end do
    end do
  print *, 'zero W at lev=',k, maxval(dfld(:,:,iwn+k-1)), minval(dfld(:,:,iwn+k-1))
  end do
  end if
! map factor**2
  im2=iwn+levs+1
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,im2) = sfld(i+(j-1)*igrd1)
    end do
  end do
  print *,im2, 'read XM2 ', dfld(1,1,im2), &
&  maxval(dfld(:,:,im2)), minval(dfld(:,:,im2))
! modify variables by map factor
 ! U,V
  do k=1,levs
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,iu+k-1)=dfld(i,j,iu+k-1)*sqrt(dfld(i,j,im2))
        dfld(i,j,iv+k-1)=dfld(i,j,iv+k-1)*sqrt(dfld(i,j,im2))
      end do
    end do
  end do
! latitude and longitude
  read(iunit) (sfld(i),i=1,nwf) ! fm2x
  read(iunit) (sfld(i),i=1,nwf) ! fm2y
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
    clat(j) = sfld(1+(j-1)*igrd1)*rad2deg
    end do
  end do
  print *, 'latitude ', clat(1), clat(jgrd1)
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      clon(i) = sfld(i+(j-1)*igrd1)*rad2deg
    end do
  end do
  print *, 'longitude ', clon(1), clon(igrd1)
  print *, 'start write output'
  ! open output
  open(ounit, FORM='unformatted', access='direct',&
&      convert='big_endian', recl=4*nwf)
  irec=1
  !gz
  write(ounit, rec=irec) dfld(:,:,igz)
  irec=irec+1
  !ps
  write(ounit, rec=irec) dfld(:,:,ips)
  irec=irec+1
  !t
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,it+k-1)
    irec=irec+1
  end do
  !u
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,iu+k-1)
    irec=irec+1
  end do
  !v
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,iv+k-1)
    irec=irec+1
  end do
  !q
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,iq+k-1)
    irec=irec+1
  end do
  !oz
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,ioz+k-1)
    irec=irec+1
  end do
  !cw
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,icw+k-1)
    irec=irec+1
  end do
  !pn
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,ipn+k-1)
    irec=irec+1
  end do
  !tn
  do k=1,levs
    write(ounit, rec=irec) dfld(:,:,itn+k-1)
    irec=irec+1
  end do
  !wn(k=1)
  write(ounit, rec=irec) dfld(:,:,iwn)
  irec=irec+1
  !wn(k>1)
  do k=2,levs+1
    write(ounit, rec=irec) dfld(:,:,iwn+k-1)
    irec=irec+1
  end do
  close(ounit)
  print *, 'end write output'
  print *, 'generate control file'
  call genctl(cunit,igrd1,jgrd1,levs,proj,idate,fhour,&
&             clat,clon,nonhyd)
  stop
contains
  subroutine genctl(nctl,igrd1,jgrd1,levs,proj,idate,fhour,&
& clat,clon,nonhyd)
    implicit none
    integer, intent(in) :: nctl,igrd1,jgrd1,levs,proj,nonhyd
    integer, intent(in) :: idate(4)
    real(kind=4), intent(in) :: fhour
    real(kind=4), intent(in) :: clat(:), clon(:)
    integer :: i,j
    integer :: ihr,idy,imo,iyr
    integer :: days(12),daysl(12)
    data days/31,28,31,30,31,30,31,31,30,31,30,31/
    data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
    character(len=2) hour, day
    character(len=3) mon(12)
    data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
&            'JUL','AUG','SEP','OCT','NOV','DEC'/

    write(nctl,'(a)') 'dset DATAFILE'
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
    write(nctl,'(a)') 'vars 12'
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
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end 
