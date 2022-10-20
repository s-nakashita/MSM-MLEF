module obs_module
!
! observation IO and observation operators
!
! history:
! 22-10-06 create
!
  use kind_module
  implicit none
  private
!
! observation type
!
  type :: obstype
    integer              :: elem
    integer,dimension(2) :: date !(hour,minu)
    real(kind=dp)        :: lon
    real(kind=dp)        :: lat
    real(kind=dp)        :: lev  !pressure[hPa]
    real(kind=dp)        :: y
    real(kind=dp)        :: err
  end type obstype 
!
! parameters and variables for decoding dcd
!
  integer,parameter :: irec_time = 0
  integer,parameter :: irec_info = 10
  integer,parameter :: irec_data = 120
  integer,parameter :: irec_undef = 32767
  integer,save      :: nrec_time=0, nrec_info=0, nrec_data=0
  integer,parameter :: nrec1 = 14
  integer           :: nrecall,nrec2,nrec3,nrec4,nrec5
!
! observation ID
!
  ! upper
  integer,parameter,public :: nobstype_upper=4
  integer,parameter,public :: id_t_obs=130   !Temperature[K]
  integer,parameter,public :: id_td_obs=3017 !Dew point temperature[K]
  integer,parameter,public :: id_wd_obs=3031 !Wind direction[degree]
  integer,parameter,public :: id_ws_obs=10   !Wind speed[m/s]
  real(kind=dp),parameter,public :: obserr(nobstype_upper) = &
  & (/1.0d0,1.0d0,5.0d0,1.0d0/)
! 
! debug
!
  logical, parameter :: debug=.false.

  public :: obstype, openfile, get_nobs, read_upper 
contains
  subroutine get_nobs(inf,cfile,ndataall,nobs)
    implicit none
    integer, intent(in) :: inf
    character(len=*), intent(in) :: cfile
    integer, intent(out) :: ndataall
    integer, intent(out) :: nobs
    integer :: ioffset
    integer :: idrec
    integer :: ntype
    integer :: data1(7)
    integer,dimension(:),allocatable :: did,ndata,nobseach
    integer :: iyy, imm, idd, ihh, inn, iwk
    integer(2) :: ibuf2
    integer(2) :: iymdhnw(6)
    integer :: i,j,irec
    logical :: lopened

    inquire(unit=inf,opened=lopened)
    if(.not.lopened) then
      call openfile(inf,cfile)
    end if
    ! get date
    ioffset=0
    call read_part1(inf,ioffset,idrec,data1)
    if(debug) then
      print *, 'reading part 1 of record ',idrec
      print *, '# of address', nrecall
      print *, nrec1,nrec2,nrec3,nrec4,nrec5
      print *, data1
    end if
    nrec_time = nrecall
    irec=1+nrec1+nrec2
    do i=1,nrec3
      read(inf,rec=irec) iymdhnw(i)
      irec=irec+1
    end do
    iyy = int(iymdhnw(1),kind=4)
    imm = int(iymdhnw(2),kind=4)
    idd = int(iymdhnw(3),kind=4)
    ihh = int(iymdhnw(4),kind=4)
    inn = int(iymdhnw(5),kind=4)
    iwk = int(iymdhnw(6),kind=4)
    print *, iyy,imm,idd,ihh,inn,iwk
    ! get info
    ioffset=nrec_time
    call read_part1(inf,ioffset,idrec,data1)
    if(debug) then
      print *, 'reading part 1 of record ',idrec
      print *, '# of address', nrecall
      print *, nrec1,nrec2,nrec3,nrec4,nrec5
      print *, data1
    end if
    nrec_info = nrecall
    irec = nrec_time+nrec1+nrec2+nrec3+1
    ntype = int(nrec4/3)
    allocate( did(ntype),ndata(ntype),nobseach(ntype) )
    ! count all data types
    ndataall=0
    do i=1,ntype
      read(inf,rec=irec) ibuf2
      did(i) = int(ibuf2,kind=4)
      irec=irec+1
      read(inf,rec=irec) ibuf2
      ndata(i) = int(ibuf2,kind=4)
      irec=irec+1
      read(inf,rec=irec) ibuf2
      nobseach(i) = int(ibuf2,kind=4)
      irec=irec+1
      ndataall=ndataall+ndata(i)
    end do
    if(debug) print *, ntype,ndataall
    ! count all observation number
    nobs=0
    ioffset=nrec_time+nrec_info
    do i=1,ntype
      if(debug) print *, did(i), ndata(i), nobseach(i)
      do j=1,ndata(i)
        ! read part 1 and extract information
        call read_part1(inf,ioffset,idrec,data1)
!        if(debug) then
!          print *, 'reading part 1 of record ',idrec
!          print *, '# of address', nrecall
!          print *, nrec1,nrec2,nrec3,nrec4,nrec5
!          print *, data1
!        end if
        ! use only 3XXX data type
        if(floor(real(did(i),kind=dp)*1.0d-3).eq.3) then
          nobs=nobs+(nrec4-2)/10*nobstype_upper
        end if
        ioffset=ioffset+nrecall
      end do
    end do
    return
  end subroutine get_nobs

  subroutine read_upper(inf,cfile,ndataall,nobs,nobsuse,obs)
    implicit none
    integer, intent(in) :: inf
    character(len=*), intent(in) :: cfile
    integer, intent(in) :: ndataall
    integer, intent(in) :: nobs
    integer, intent(out) :: nobsuse
    type(obstype), intent(out) :: obs(nobs)
    integer :: ioffset
    integer :: idrec
    integer :: data1(7)
    integer :: dtype
    type(obstype), allocatable :: tmpobs(:)
    integer :: tmphour, tmpminu, tmpelm
    real(kind=dp) :: tmplon, tmplat
    real(kind=dp) :: lonb, latb
    real(kind=dp) :: tmplev, tmpdat, tmperr
    integer(2) :: ibuf2
    integer :: nloop
    character(len=16) :: acc
    integer :: n,nn,i,irec
    
    ioffset=nrec_time+nrec_info
    print *, 'ioffset ',ioffset
    allocate( tmpobs(nobs) )
    ! start reading data
    nobsuse=0
    nn=0
    do n=1,20 !ndataall
      ! read part 1 and extract information
      call read_part1(inf,ioffset,idrec,data1)
      if(debug) then
        print *, 'reading part 1 of record ',idrec
        print *, '# of address', nrecall
        print *, nrec1,nrec2,nrec3,nrec4,nrec5
        print *, data1
      end if
      nrec_data = nrecall
      dtype = int(data1(1),kind=4)
      tmplat = real(data1(2),kind=dp)*0.01d0
      tmplon = real(data1(3),kind=dp)*0.01d0
      if(nn==0) then
        latb=tmplat
        lonb=tmplon
      else if((tmplat/=latb).and.(tmplon/=lonb)) then
        call sortobs(nn,tmpobs)
        obs(nobsuse+1:nobsuse+nn)=tmpobs(1:nn)
        nobsuse=nobsuse+nn
        nn=0
      end if
      ! read hour and minutes
      irec=ioffset+nrec1+6
      read(inf,rec=irec) ibuf2
      tmphour=int(ibuf2,kind=4)/100
      tmpminu=int(ibuf2,kind=4)-tmphour*100
      if(debug) then
        print *, 'dtype,lat,lon,hour,minutes'
        print *, dtype,tmplat,tmplon,tmphour,tmpminu
      end if
      ! use only 3XXX data type
      if(floor(real(dtype,kind=dp)*1.0d-3).eq.3) then
        irec=ioffset+nrec1+nrec2+nrec3+1
        read(inf,rec=irec) ibuf2
        if(debug) print *, 'sst ',real(ibuf2,kind=dp)*0.1
        irec=irec+1
        read(inf,rec=irec) ibuf2
        nloop = int(ibuf2,kind=4)
        if(debug) print *, 'nloop ',nloop 
        do i=1,nloop
          irec=irec+2
          read(inf,rec=irec) ibuf2 !accuracy
          write(acc,'(b16.16)') ibuf2
          if(debug) then
            print *, ibuf2
            print '(a16)', acc
          end if
          if(ibuf2<0) cycle
          irec=irec+1
          read(inf,rec=irec) ibuf2
          !if(acc(11:11)=='0') then
          if(ibuf2>0) then
            tmplev=real(ibuf2,kind=8)*0.1d0 ![hPa]
          else
            irec=irec+7
            cycle
          end if
          irec=irec+3
          read(inf,rec=irec) ibuf2
          !if(acc(13:13)=='0') then
          if(ibuf2>0) then
            tmpelm=id_t_obs
            tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
            tmperr=obserr(1)
            nn=nn+1
            call setobs(tmpobs(nn),&
            &  tmpelm,tmphour,tmpminu,&
            &  tmplat,tmplon,tmplev,&
            &  tmpdat,tmperr)
          end if   
          irec=irec+1
          read(inf,rec=irec) ibuf2
          !if(acc(14:14)=='0') then
          if(ibuf2>0) then
            tmpelm=id_td_obs
            tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
            tmperr=obserr(2)
            nn=nn+1
            call setobs(tmpobs(nn),&
            &  tmpelm,tmphour,tmpminu,&
            &  tmplat,tmplon,tmplev,&
            &  tmpdat,tmperr)
          end if   
          irec=irec+2 
          read(inf,rec=irec) ibuf2
          !if(acc(15:15)=='0') then
          if(ibuf2>0) then
            tmpelm=id_wd_obs
            tmpdat=real(ibuf2,kind=dp) ![degree]
            tmperr=obserr(3)
            nn=nn+1
            call setobs(tmpobs(nn),&
            &  tmpelm,tmphour,tmpminu,&
            &  tmplat,tmplon,tmplev,&
            &  tmpdat,tmperr)
          end if   
          irec=irec+1
          read(inf,rec=irec) ibuf2
          !if(acc(16:16)=='0') then
          if(ibuf2>0) then
            tmpelm=id_ws_obs
            tmpdat=real(ibuf2,kind=dp)*0.1d0 ![m/s]
            tmperr=obserr(4)
            nn=nn+1
            call setobs(tmpobs(nn),&
            &  tmpelm,tmphour,tmpminu,&
            &  tmplat,tmplon,tmplev,&
            &  tmpdat,tmperr)
          end if   
        end do
      end if
      ioffset=ioffset+nrec_data
    end do
    close(inf)

    return
  end subroutine read_upper

  subroutine read_part1(inf,ioffset,idrec,data)
    implicit none
    integer, intent(in) :: inf
    integer, intent(in) :: ioffset
    integer, intent(out) :: idrec
    integer, intent(out) :: data(7)
    integer(2) :: ibuf2(nrec1)
    integer :: i, irec

    irec=1+ioffset
    do i=1,nrec1
      read(inf,rec=irec) ibuf2(i)
      irec=irec+1
    end do
    nrecall=int(ibuf2(1),kind=4)
    nrec2=int(ibuf2(3),kind=4);nrec3=int(ibuf2(4),kind=4)
    nrec4=int(ibuf2(5),kind=4);nrec5=int(ibuf2(6),kind=4)
    idrec=int(ibuf2(7),kind=4)
    data=int(ibuf2(8:),kind=4)
    return
  end subroutine read_part1

  subroutine setobs(obs1,elem,hour,minu,lat,lon,lev,dat,err)
    implicit none
    type(obstype), intent(out) :: obs1
    integer, intent(in) :: elem
    integer, intent(in) :: hour, minu
    real(kind=dp), intent(in) :: lat,lon,lev,dat,err

    obs1%elem = elem
    obs1%date(1) = hour; obs1%date(2) = minu
    obs1%lat = lat; obs1%lon = lon; obs1%lev = lev
    obs1%y = dat; obs1%err = err
    return
  end subroutine setobs

  subroutine sortobs(nobs,obs)
    implicit none
    integer, intent(inout) :: nobs
    type(obstype), intent(inout) :: obs(:)
    integer :: ntim, nobs1
    integer, allocatable :: ndate(:), ndatesum(:)
    integer :: nelm(0:nobstype_upper), nelmsum(nobstype_upper)
    integer, dimension(nobstype_upper) :: elemlist=(/id_t_obs,id_td_obs,id_wd_obs,id_ws_obs/)
    real(kind=dp), allocatable :: tmplev(:)
    integer :: n,nn,ns,ne
    integer :: i,j
    
    print *, nobs
    ! time sort
    ntim=24
    allocate( ndate(0:ntim-1), ndatesum(ntim) )
    ndate(:)=0
    do i=1,ntim-1
      do n=1,nobs
        if( obs(n)%date(1) >= i .or. obs(n)%date(1) < i-1) cycle
        ndate(i) = ndate(i) + 1
      end do
    end do
    ndatesum(:)=0
    do i=1,ntim
      ndatesum(i) = sum(ndate(0:i-1))
    end do
    print *, ndate
    print *, ndatesum
    do i=1,ntim-1
      nn=0
      do n=1,nobs
        if( obs(n)%date(1) >= i .or. obs(n)%date(1) < i-1) cycle
        nn=nn+1
        obs(nn+ndatesum(i)) = obs(n)
      end do
    end do 
    ! element sort
    do i=1,ntim-1
      ns=ndatesum(i)
      ne=ndatesum(i+1)
      if(ns==ne) cycle
      print *, i, ns, ne
      nelm(:)=0
      do j=1,nobstype_upper
        do n=ns+1,ne
          if( obs(n)%elem /= elemlist(j) ) cycle
          nelm(j)=nelm(j)+1
        end do
      end do
      nelmsum(:)=0
      do j=1,nobstype_upper
        nelmsum(j) = sum(nelm(0:j-1))
      end do
      print *, nelm
      print *, nelmsum
      do j=1,nobstype_upper
        nn=ns
        do n=ns+1,ne
          if( obs(n)%elem /= elemlist(j) ) cycle
          nn=nn+1
          obs(nn+nelmsum(j)) = obs(n)
        end do
      end do
    end do
    return
  end subroutine sortobs

  subroutine openfile(inf,cfile)
    implicit none
    integer, intent(in) :: inf
    character(len=*), intent(in) :: cfile

    open(inf,file=cfile,status='old',form='unformatted',&
         convert='big_endian',access='direct',recl=2)
 
    return
  end subroutine openfile
  
end module obs_module
