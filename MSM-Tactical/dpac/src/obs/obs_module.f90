module obs_module
!
! observation IO and observation operators
!
! history:
! 22-10-06 create
!
  use kind_module
  use rsmcom_module, only: ndate,nhour
  implicit none
  private
!
! observation type (row)
!
  type :: obstype
    integer :: nobs
    integer,allocatable  :: elem(:)
!    integer,dimension(2) :: date !(hour,minu)
    real(kind=dp),allocatable :: lon(:)
    real(kind=dp),allocatable :: lat(:)
    real(kind=dp),allocatable :: lev(:) !pressure[hPa]
    real(kind=dp),allocatable :: dat(:)
    real(kind=dp),allocatable :: err(:)
    real(kind=dp),allocatable :: dtrel(:) ! observation time relative to analysis time (minutes)
  end type obstype 
!
! observation type (after obsope)
!
  type :: obstype2
    integer :: nobs
    integer,allocatable  :: elem(:)
!    integer,dimension(2) :: date !(hour,minu)
    real(kind=dp),allocatable :: lon(:)
    real(kind=dp),allocatable :: lat(:)
    real(kind=dp),allocatable :: lev(:) !pressure[hPa]
    real(kind=dp),allocatable :: dat(:)
    real(kind=dp),allocatable :: err(:)
    real(kind=dp),allocatable :: dtrel(:) ! observation time relative to analysis time (minutes)
    real(kind=dp),allocatable :: hxf(:,:)   ! h(x) ensemble
    integer,allocatable       :: qc(:)    ! QC flag
  end type obstype2
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
! QC flags
!
  integer,parameter,public :: iqc_good=0
  integer,parameter,public :: iqc_gross_err=5
!  integer,parameter,public :: iqc_ps_ter=10
!  integer,parameter,public :: iqc_ref_low=11
!  integer,parameter,public :: iqc_ref_mem=12
  integer,parameter,public :: iqc_out_vhi=20
  integer,parameter,public :: iqc_out_vlo=21
  integer,parameter,public :: iqc_out_h=22
  integer,parameter,public :: iqc_otype=90
  integer,parameter,public :: iqc_time=91
! 
! debug
!
  logical, parameter :: debug=.false.

  public :: obstype, obstype2, openfile, get_nobs, read_upper 
contains
  subroutine get_nobs(inf,cfile,atime,smin,emin,ndataall,nobs)
    implicit none
    integer, intent(in) :: inf
    character(len=*), intent(in) :: cfile
    integer, intent(in) :: atime(5) !year,month,day,hour,minutes
    integer, intent(in) :: smin !observation time window (minutes)
    integer, intent(in) :: emin !(smin<=otime-atime<=emin)
    integer, intent(out) :: ndataall
    integer, intent(out) :: nobs
    integer :: ioffset
    integer :: idrec
    integer :: ntype
    integer :: data1(7)
    integer :: otime(5)
    integer :: imin
    integer,dimension(:),allocatable :: did,ndata,nobseach
    integer :: iyy, imm, idd, ihh, inn, iwk
    integer(2) :: ibuf2
    integer(2) :: iymdhnw(6)
    integer :: i,j,irec,nloop
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
    otime(1)=iyy
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
    ntype = nrec4/3
    allocate( did(ntype),ndata(ntype),nobseach(ntype) )
    ! count all data types
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
      print *, did(i), ndata(i), nobseach(i)
    end do
    ! count all observation number
    ndataall=0
    nobs=0
    ioffset=nrec_time+nrec_info
    do i=1,ntype
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
        if(did(i)/1000.eq.3) then
          ! read hour and minutes
          irec=ioffset+nrec1+5
          read(inf,rec=irec) ibuf2
          imm=int(ibuf2,kind=4)/100
          idd=int(ibuf2,kind=4)-imm*100
          otime(2)=imm; otime(3)=idd
          irec=irec+1
          read(inf,rec=irec) ibuf2
          ihh=int(ibuf2,kind=4)/100
          inn=int(ibuf2,kind=4)-ihh*100
          otime(4)=ihh;otime(5)=inn
          call nhour(atime,otime,imin)
          if((imin.ge.smin).and.(imin.lt.emin)) then
            if(debug) print *, 'obs time =',otime
            if(debug) print *, 'difference(minutes) =',imin
            irec=ioffset+nrec1+nrec2+nrec3+2
            read(inf,rec=irec) ibuf2
            nloop = int(ibuf2,kind=4)
            nobs=nobs+nloop*nobstype_upper
          end if
        end if
        ioffset=ioffset+nrecall
        ndataall=ndataall+1
      end do
    end do
    print *, ntype,ndataall
    return
  end subroutine get_nobs

  subroutine read_upper(inf,cfile,atime,smin,emin,ndataall,nobs,obs)
    implicit none
    integer, intent(in) :: inf
    character(len=*), intent(in) :: cfile
    integer, intent(in) :: atime(5) !year,month,day,hour,minutes
    integer, intent(in) :: smin !observation time window (minutes)
    integer, intent(in) :: emin !(smin<=otime-atime<=emin)
    integer, intent(in) :: ndataall
    integer, intent(in) :: nobs !before time selecting
    type(obstype), intent(inout) :: obs
    integer :: nobsuse
    integer :: ioffset
    integer :: idrec
    integer :: data1(7)
    integer :: dtype
    type(obstype) :: tmpobs,tmpobs2
    integer :: imin
    integer :: otime(5)
    integer :: tmpelm !, tmphour, tmpminu
    real(kind=dp) :: tmplon, tmplat
    real(kind=dp) :: tmplev, tmpdat, tmperr
    real(kind=dp) :: tmpdt
    integer(2) :: ibuf2
    integer :: lonb, latb
    integer :: nloop, nsort
    character(len=16) :: acc
    integer :: n,nn,i,irec
    
    ioffset=nrec_time+nrec_info
    print *, 'ioffset ',ioffset
    ! set obs arrays
    tmpobs%nobs = nobs
    tmpobs2%nobs = nobs
    call obsin_allocate( tmpobs )
    call obsin_allocate( tmpobs2 )
    ! start reading data
    nobsuse=0
    nn=0
    nsort=0
    otime(1)=atime(1)
    do n=1,ndataall
      ! read part 1 and extract information
      call read_part1(inf,ioffset,idrec,data1)
      nrec_data = nrecall
      dtype = data1(1)
      tmplat = real(data1(2),kind=dp)*0.01d0
      tmplon = real(data1(3),kind=dp)*0.01d0
      if(nn==0) then
        latb=data1(2);lonb=data1(3)
      end if
      if((data1(2)/=latb).and.(data1(3)/=lonb)) then
        if(debug) then
          print *, 'latlon(previous) ',latb, lonb
          print *, 'latlon(current)  ',data1(2), data1(3)
        end if
        print *, 'before sort ',nn
        call sortobs(nn,tmpobs)
        print *, 'after sort ',nn
!        obs(nobsuse+1:nobsuse+nn)=tmpobs(1:nn)
!        call copyobs(nn,1,nobsuse+1,tmpobs,obs)
        tmpobs2%elem(nobsuse+1:nobsuse+nn) = tmpobs%elem(1:nn)
        tmpobs2%lon (nobsuse+1:nobsuse+nn) = tmpobs%lon (1:nn)
        tmpobs2%lat (nobsuse+1:nobsuse+nn) = tmpobs%lat (1:nn)
        tmpobs2%lev (nobsuse+1:nobsuse+nn) = tmpobs%lev (1:nn)
        tmpobs2%dat (nobsuse+1:nobsuse+nn) = tmpobs%dat (1:nn)
        tmpobs2%err (nobsuse+1:nobsuse+nn) = tmpobs%err (1:nn)
        tmpobs2%dtrel(nobsuse+1:nobsuse+nn) = tmpobs%dtrel(1:nn)
        nobsuse=nobsuse+nn
        print *, 'nobsuse = ',nobsuse,'/',nobs
        nn=0
        latb=data1(2);lonb=data1(3)
        nsort=nsort+1
!        if(nsort.gt.5) exit !debug
      end if
      ! use only 3XXX data type
      if(dtype/1000.eq.3) then
        if(debug) then
          print *, 'reading part 1 of record ',idrec
          print *, '# of address', nrecall
          print *, nrec1,nrec2,nrec3,nrec4,nrec5
          print *, data1
        end if
        ! read hour and minutes
        irec=ioffset+nrec1+5
        read(inf,rec=irec) ibuf2
        otime(2)=int(ibuf2,kind=4)/100
        otime(3)=int(ibuf2,kind=4)-otime(2)*100
        irec=irec+1
        read(inf,rec=irec) ibuf2
        otime(4)=int(ibuf2,kind=4)/100
        otime(5)=int(ibuf2,kind=4)-otime(4)*100
        call nhour(atime,otime,imin)
        if((imin.ge.smin).and.(imin.lt.emin)) then
          if(debug) print *, 'obs time =',otime
          if(debug) print *, 'difference(minutes) =',imin
          tmpdt = real(imin,kind=dp)
          if(debug) then
            print *, 'dtype,lat,lon,dtrel'
            print *, dtype,tmplat,tmplon,tmpdt
          end if
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
              print *, 'accuracy'
              print *, ibuf2
              print *, acc
            end if
            if(ibuf2/=0) then
            !if(ibuf2<0) then
              irec=irec+8
              cycle
            end if
            irec=irec+1
            read(inf,rec=irec) ibuf2
            if(acc(11:11)=='0'.and.ibuf2>0) then
              tmplev=real(ibuf2,kind=dp)*0.1d0 ![hPa]
            else
              irec=irec+7
              cycle
            end if
            irec=irec+3
            read(inf,rec=irec) ibuf2
            if(acc(13:13)=='0'.and.ibuf2>0) then
              tmpelm=id_t_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
              tmperr=obserr(1)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
            irec=irec+1
            read(inf,rec=irec) ibuf2
            if(acc(14:14)=='0'.and.ibuf2>0) then
              tmpelm=id_td_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
              tmperr=obserr(2)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
            irec=irec+2 
            read(inf,rec=irec) ibuf2
            if(acc(15:15)=='0'.and.ibuf2>0) then
              tmpelm=id_wd_obs
              tmpdat=real(ibuf2,kind=dp) ![degree]
              tmperr=obserr(3)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
            irec=irec+1
            read(inf,rec=irec) ibuf2
            if(acc(16:16)=='0'.and.ibuf2>0) then
              tmpelm=id_ws_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![m/s]
              tmperr=obserr(4)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,& 
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
          end do
        end if
      end if
      ioffset=ioffset+nrec_data
    end do
    if(nn>0) then
      print *, 'before sort ',nn
      call sortobs(nn,tmpobs)
      print *, 'after sort ',nn
!      call copyobs(nn,1,nobsuse+1,tmpobs,obs)
!      obs(nobsuse+1:nobsuse+nn)=tmpobs(1:nn)
      tmpobs2%elem(nobsuse+1:nobsuse+nn) = tmpobs%elem(1:nn)
      tmpobs2%lon (nobsuse+1:nobsuse+nn) = tmpobs%lon (1:nn)
      tmpobs2%lat (nobsuse+1:nobsuse+nn) = tmpobs%lat (1:nn)
      tmpobs2%lev (nobsuse+1:nobsuse+nn) = tmpobs%lev (1:nn)
      tmpobs2%dat (nobsuse+1:nobsuse+nn) = tmpobs%dat (1:nn)
      tmpobs2%err (nobsuse+1:nobsuse+nn) = tmpobs%err (1:nn)
      tmpobs2%dtrel(nobsuse+1:nobsuse+nn) = tmpobs%dtrel(1:nn)
      nobsuse=nobsuse+nn
      print *, 'nobsuse = ',nobsuse,'/',nobs
      nsort=nsort+1
    end if
    print *, 'nsort = ',nsort
    ! output
    obs%nobs = nobsuse
    call obsin_allocate( obs )
    obs%elem(:) = tmpobs2%elem(1:nobsuse)
    obs%lon (:) = tmpobs2%lon (1:nobsuse)
    obs%lat (:) = tmpobs2%lat (1:nobsuse)
    obs%lev (:) = tmpobs2%lev (1:nobsuse)
    obs%dat (:) = tmpobs2%dat (1:nobsuse)
    obs%err (:) = tmpobs2%err (1:nobsuse)
    obs%dtrel(:) = tmpobs2%dtrel(1:nobsuse)
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

  subroutine setobs(n,obs,elem,lat,lon,lev,dat,err,dtrel)
    implicit none
    integer, intent(in) :: n
    type(obstype), intent(inout) :: obs
    integer, intent(in) :: elem
!    integer, intent(in) :: hour, minu
    real(kind=dp), intent(in) :: lat,lon,lev,dat,err,dtrel

    obs%elem(n) = elem
!    obs%date(1) = hour; obs%date(2) = minu
    obs%lat(n) = lat; obs%lon(n) = lon; obs%lev(n) = lev
    obs%dat(n) = dat; obs%err(n) = err
    obs%dtrel(n) = dtrel
    return
  end subroutine setobs

  subroutine sortobs(nobs,obs)
    use stdlib_sorting
    implicit none
    integer, intent(inout) :: nobs
    type(obstype), intent(inout) :: obs
    integer :: nobs1
    integer :: nelm(0:nobstype_upper), nelmsum(nobstype_upper)
    integer, dimension(nobstype_upper) :: elemlist=(/id_t_obs,id_td_obs,id_wd_obs,id_ws_obs/)
    type(obstype) :: tmpobs
    real(kind=dp), allocatable :: tmplev(:)
    real(kind=dp) :: clev
    integer(int_size), allocatable :: idx(:)
    integer :: n,nn,ns,ne,ns2
    integer :: i,j
    
    if(debug) print *, nobs
    tmpobs%nobs = nobs
    call obsin_allocate( tmpobs )
    if(debug) then
      print *, tmpobs%nobs, size(tmpobs%elem), size(tmpobs%lon), size(tmpobs%lat),size(tmpobs%lev),&
       & size(tmpobs%dat),size(tmpobs%err),size(tmpobs%dtrel)
    end if
!    allocate( tmpobs(nobs) )
    ! element sort
    nelm(:)=0
    do j=1,nobstype_upper
      do n=1,nobs
        if( obs%elem(n) /= elemlist(j) ) cycle
        nelm(j)=nelm(j)+1
      end do
    end do
    nelmsum(:)=0
    do j=1,nobstype_upper
      nelmsum(j) = sum(nelm(0:j-1))
    end do
    if(debug) print *, nelm
    if(debug) print *, nelmsum
!    call copyobs(nobs,1,1,obs,tmpobs)
!    tmpobs(1:nobs) = obs(1:nobs)
    tmpobs%elem(1:nobs) = obs%elem(1:nobs)
    tmpobs%lon (1:nobs) = obs%lon (1:nobs)
    tmpobs%lat (1:nobs) = obs%lat (1:nobs)
    tmpobs%lev (1:nobs) = obs%lev (1:nobs)
    tmpobs%dat (1:nobs) = obs%dat (1:nobs)
    tmpobs%err (1:nobs) = obs%err (1:nobs)
    tmpobs%dtrel(1:nobs) = obs%dtrel(1:nobs)
    do j=1,nobstype_upper
      nn=0
      do n=1,nobs
        if( tmpobs%elem(n) /= elemlist(j) ) cycle
        nn=nn+1
!        call copyobs(1,n,nn+nelmsum(j),tmpobs,obs)
!        obs(nn+nelmsum(j)) = tmpobs(n)
        obs%elem(nn+nelmsum(j)) = tmpobs%elem(n)
        obs%lon (nn+nelmsum(j)) = tmpobs%lon (n)
        obs%lat (nn+nelmsum(j)) = tmpobs%lat (n)
        obs%lev (nn+nelmsum(j)) = tmpobs%lev (n)
        obs%dat (nn+nelmsum(j)) = tmpobs%dat (n)
        obs%err (nn+nelmsum(j)) = tmpobs%err (n)
        obs%dtrel(nn+nelmsum(j)) = tmpobs%dtrel(n)
      end do
    end do
    ! level sort & delete duplicate data
!    call copyobs(nobs,1,1,obs,tmpobs)
!    tmpobs(1:nobs) = obs(1:nobs)
    tmpobs%elem(1:nobs) = obs%elem(1:nobs)
    tmpobs%lon (1:nobs) = obs%lon (1:nobs)
    tmpobs%lat (1:nobs) = obs%lat (1:nobs)
    tmpobs%lev (1:nobs) = obs%lev (1:nobs)
    tmpobs%dat (1:nobs) = obs%dat (1:nobs)
    tmpobs%err (1:nobs) = obs%err (1:nobs)
    tmpobs%dtrel(1:nobs) = obs%dtrel(1:nobs)
    nobs=0
    do i=1,nobstype_upper
      if( nelm(i)==0 ) cycle
      ns=nelmsum(i)
      ne=ns+nelm(i)
      if(debug) print *, ns, ne
      allocate( tmplev(1:nelm(i)) )
      allocate( idx(1:size(tmplev)) )
      do j=1,nelm(i)
        tmplev(j) = tmpobs%lev(ns+j)
      end do
      if(debug) print *, tmplev
      ! sorting descending order (lower->upper)
      call sort_index(tmplev, idx, reverse=.true.)
      if(debug) print *, tmplev
      if(debug) print *, idx
      nobs=nobs+1
      ns2=ns+int(idx(1),kind=4)
      if(debug) print *, ns2
!      call copyobs(1,ns2,nobs,tmpobs,obs)
!      obs(nobs)=tmpobs(ns+idx(1))
      obs%elem(nobs) = tmpobs%elem(ns2)
      obs%lon (nobs) = tmpobs%lon (ns2)
      obs%lat (nobs) = tmpobs%lat (ns2)
      obs%lev (nobs) = tmpobs%lev (ns2)
      obs%dat (nobs) = tmpobs%dat (ns2)
      obs%err (nobs) = tmpobs%err (ns2)
      obs%dtrel(nobs) = tmpobs%dtrel(ns2)
      clev=tmplev(1)
      if( nelm(i).gt.1 ) then
        do j=2,nelm(i)
          if(tmplev(j)==clev) cycle
          nobs=nobs+1
!          obs(nobs)=tmpobs(ns+idx(j))
          ns2=ns+int(idx(j),kind=4)
          if(debug) print *, ns2
!          call copyobs(1,ns2,nobs,tmpobs,obs)
          obs%elem(nobs) = tmpobs%elem(ns2)
          obs%lon (nobs) = tmpobs%lon (ns2)
          obs%lat (nobs) = tmpobs%lat (ns2)
          obs%lev (nobs) = tmpobs%lev (ns2)
          obs%dat (nobs) = tmpobs%dat (ns2)
          obs%err (nobs) = tmpobs%err (ns2)
          obs%dtrel(nobs) = tmpobs%dtrel(ns2)
          clev=tmplev(j)
        end do
      end if
      deallocate(tmplev,idx)
    end do
    call obsin_deallocate(tmpobs)
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
  
  subroutine obsin_allocate(obs)
    implicit none
    type(obstype),intent(inout) :: obs

    call obsin_deallocate(obs)

    allocate( obs%elem(obs%nobs) )
    allocate( obs%lon (obs%nobs) )
    allocate( obs%lat (obs%nobs) )
    allocate( obs%lev (obs%nobs) )
    allocate( obs%dat (obs%nobs) )
    allocate( obs%err (obs%nobs) )
    allocate( obs%dtrel(obs%nobs) )

    obs%elem = 0
    obs%lon = 0.0_dp
    obs%lat = 0.0_dp
    obs%lev = 0.0_dp
    obs%dat = 0.0_dp
    obs%err = 0.0_dp
    obs%dtrel = 0.0_dp

    return
  end subroutine obsin_allocate

  subroutine obsin_deallocate(obs)
    implicit none
    type(obstype), intent(inout) :: obs
    if(allocated(obs%elem)) deallocate(obs%elem)
    if(allocated(obs%lon))  deallocate(obs%lon)
    if(allocated(obs%lat))  deallocate(obs%lat)
    if(allocated(obs%lev))  deallocate(obs%lev)
    if(allocated(obs%dat))  deallocate(obs%dat)
    if(allocated(obs%err))  deallocate(obs%err)
    if(allocated(obs%dtrel)) deallocate(obs%dtrel)
    return
  end subroutine obsin_deallocate
!!
!! don't work
!!
!!  subroutine copyobs(n,ns1,ns2,obs1,obs2)
!!    implicit none
!!    integer, intent(in) :: n,ns1,ns2
!!    type(obstype), intent(in) :: obs1
!!    type(obstype), intent(out):: obs2
!!    integer, allocatable :: tmpelm(:)
!!    real(kind=dp), allocatable :: tmplon(:),tmplat(:),tmplev(:),&
!!     & tmpdat(:),tmperr(:),tmpdt(:)
!!    integer :: i
!!
!!    if(debug) then
!!      print *, n, ns1, ns2
!!      print *, obs2%nobs, size(obs2%elem), size(obs2%lon), size(obs2%lat),size(obs2%lev),&
!!       & size(obs2%dat),size(obs2%err),size(obs2%dtrel)
!!    end if
!!    allocate( tmpelm(n) )
!!    allocate( tmplon(n),tmplat(n),tmplev(n),tmpdat(n),tmperr(n),tmpdt(n) )
!!    do i=1,n
!!      if(debug) then
!!        print *, i+ns1-1
!!        print *, obs1%elem(i+ns1-1)
!!        print *, i+ns2-1
!!        print *, obs2%elem(i+ns2-1)
!!      end if
!!      tmpelm(i) = obs1%elem(i+ns1-1)
!!      tmplon(i) = obs1%lon (i+ns1-1)
!!      tmplat(i) = obs1%lat (i+ns1-1)
!!      tmplev(i) = obs1%lev (i+ns1-1)
!!      tmpdat(i) = obs1%dat (i+ns1-1)
!!      tmperr(i) = obs1%err (i+ns1-1)
!!      tmpdt (i) = obs1%dtrel(i+ns1-1)
!!!      obs2%elem(i+ns2-1)=obs1%elem(i+ns1-1)
!!!      obs2%lon (i+ns2-1)=obs1%lon (i+ns1-1)
!!!      obs2%lat (i+ns2-1)=obs1%lat (i+ns1-1)
!!!      obs2%lev (i+ns2-1)=obs1%lev (i+ns1-1)
!!!      obs2%dat (i+ns2-1)=obs1%dat (i+ns1-1)
!!!      obs2%err (i+ns2-1)=obs1%err (i+ns1-1)
!!!      obs2%dtrel(i+ns2-1)=obs1%dtrel(i+ns1-1)
!!    end do
!!    obs2%elem(ns2:ns2+n-1) = tmpelm(:)
!!    obs2%lon (ns2:ns2+n-1) = tmplon(:)
!!    obs2%lat (ns2:ns2+n-1) = tmplat(:)
!!    obs2%lev (ns2:ns2+n-1) = tmplev(:)
!!    obs2%dat (ns2:ns2+n-1) = tmpdat(:)
!!    obs2%err (ns2:ns2+n-1) = tmperr(:)
!!    obs2%dtrel(ns2:ns2+n-1) = tmpdt(:)
!!    deallocate( tmpelm,tmplon,tmplat,tmplev,tmpdat,tmperr,tmpdt )
!!    return
!!  end subroutine copyobs
end module obs_module
