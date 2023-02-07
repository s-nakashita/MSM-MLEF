program read_info
!
! main program : read_info   read DA information file and convert into GrADS file
!                           
! history:
! 23-01-10 create by modifing read_sig.f90
! 
! input :
! unit 11 sigma file (sequential, with header)
! 
! output:
! unit 51 sigma file (direct, no header)
!
  use kind_module
  use phconst_module
  use read_module, only : read_header, levmax, nwext
  implicit none
  integer, parameter :: iunit=11, ounit=51, cunit=61
  character(len=8) :: label(4)
  integer :: idate(4), iymdh
  integer :: iret
  real(kind=dp) :: si(levmax+1), sl(levmax)
  real(kind=sp) :: fhour, ext(nwext) 
  integer,parameter :: icld=0 !not include 3D physics
  logical :: emem=.false. !T:ensemble weights, F:DA information
  integer :: member=10
  logical :: writectl=.true.
  namelist /namlst_info/ emem, member, writectl
  ! components of ext
  integer :: iwav1,jwav1,igrd1,jgrd1,levs,nfldx,proj,nonhyd
  real(kind=sp) :: rtruth, rorient, rcenlat, rcenlon, rgrdlft, rgrdbtm, &
 &                delx, dely
  integer :: nflds, nwf, nskip, irec
  integer :: i,j,k,l,m
  integer, parameter :: ijb=1,ijo=2,idfs=3,idfn=4,inobs=5,irloc=6
  real(kind=dp), allocatable :: clat(:), clon(:)
  real(kind=sp), allocatable :: sfld(:),buf4(:,:)
  
  read (5,nml=namlst_info)
  write(6,nml=namlst_info)

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
  allocate( sfld(nwf) )
  allocate( clat(jgrd1),clon(igrd1) )
  allocate( buf4(igrd1,jgrd1) )

  print *, 'start reading and writing data'
  ! open output
  open(ounit, FORM='unformatted', access='direct',&
&      convert='big_endian', recl=4*nwf)
  irec=1
  rewind(iunit)
  read(iunit) !label
  read(iunit) !ext
  read(iunit) (sfld(i),i=1,nwf) ! dummy
  read(iunit) (sfld(i),i=1,nwf) ! dummy
  if(emem) then
  !weights
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  !eigenvalues
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  nskip=4*levs
  else
  !Jb
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  !Jo
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  !DFS
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  !DFN
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  !NOBS
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  !Rloc
  do k=1,levs
    read(iunit) (sfld(i),i=1,nwf)
    do j=1,jgrd1
      do i=1,igrd1
        buf4(i,j) = sfld(i+(j-1)*igrd1)
      end do
    end do
    write(ounit,rec=irec) buf4
    irec=irec+1
  end do
  nskip=0
  end if !emem

  if(nonhyd.eq.1) then
    nskip=3*levs+1 !pn,tn,wn
  end if
  nskip=nskip+3 ! fm2,fm2x,fm2y
  do k=1,nskip
    read(iunit) (sfld(i),i=1,nwf)
  end do
! latitude and longitude
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
    clat(j) = real(sfld(1+(j-1)*igrd1),kind=dp)*rad2deg
    end do
  end do
  print *, 'latitude ', clat(1), clat(jgrd1)
  read(iunit) (sfld(i),i=1,nwf)
  do j=1,jgrd1
    do i=1,igrd1
      clon(i) = real(sfld(i+(j-1)*igrd1),kind=dp)*rad2deg
    end do
  end do
  print *, 'longitude ', clon(1), clon(igrd1)

  close(ounit)
  print *, 'end write output'
  if(writectl) then
  print *, 'generate control file'
  call genctl(cunit)
  end if
!  stop
contains
  subroutine genctl(nctl)
    implicit none
    integer, intent(in) :: nctl
    integer :: i,j
    integer :: ihr,idy,imo,iyr
    integer :: days(12),daysl(12)
    data days/31,28,31,30,31,30,31,31,30,31,30,31/
    data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
    character(len=2) hour, day
    character(len=3) mon(12)
    data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
&            'JUL','AUG','SEP','OCT','NOV','DEC'/
    integer :: ncmem
    character(len=256) :: cmem 

    write(nctl,'(a)') 'dset ^DATAFILE'
    if(emem) then
    write(nctl,'(a)') 'options big_endian template'
    else
    write(nctl,'(a)') 'options big_endian'
    end if
    write(nctl,'(a)') 'undef -9.99E+33'
    if (proj .eq. 0) then
      write(6,108) igrd1,clon(1),clon(2)-clon(1)
      write(nctl,108) igrd1,clon(1),clon(2)-clon(1)
 108  format('xdef',I5,' linear',2G14.6)
      write(6,110) jgrd1
      write(nctl,110) jgrd1
 110  format('ydef',I5,' levels')
      write(6,111) (clat(j),j=1,jgrd1)
      write(nctl,111) (clat(j),j=1,jgrd1)
 111  format(5G14.6)
    end if
    write(6,112) levs
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
    write(6,114) hour,day,mon(imo),iyr
    write(nctl,114) hour,day,mon(imo),iyr
 114 format('tdef 1 linear ',A2,'Z',A2,A3,I4,'   1hr')
    if(emem) then
    ncmem=4*member
    do i=1,member
      write(cmem(4*(i-1)+1:4*i),'(I4.3)') i
    end do
    write(nctl,115) member,cmem(1:ncmem)
 115 format('edef ',I2,' names ',A)
    write(nctl,'(a)') 'vars 2'
    write(nctl,'(a,i2,a)') 'ewgt ',levs,' 99 ensemble weights'
    write(nctl,'(a,i2,a)') 'eval ',levs,' 99 eigenvalues'
    else
    write(nctl,'(a)') 'vars 6'
    write(nctl,'(a,i2,a)') 'jb ',levs,' 99 background cost'
    write(nctl,'(a,i2,a)') 'jo ',levs,' 99 observation cost'
    write(nctl,'(a,i2,a)') 'dfs ',levs,' 99 local Degree of Freedom for Signal'
    write(nctl,'(a,i2,a)') 'dfn ',levs,' 99 local Degree of Freedom for Noise'
    write(nctl,'(a,i2,a)') 'nobs ',levs,' 99 local observation number'
    write(nctl,'(a,i2,a)') 'rlocsum ',levs,' 99 sum of local observation error'
    end if
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end 
