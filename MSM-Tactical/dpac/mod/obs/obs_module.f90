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
! observation type (row)
!
  type :: obstype
    integer :: nobs
    integer,allocatable  :: elem(:)
    real(kind=dp),allocatable :: lon(:)
    real(kind=dp),allocatable :: lat(:)
    real(kind=dp),allocatable :: lev(:) !pressure[hPa]
    real(kind=dp),allocatable :: dat(:)
    real(kind=dp),allocatable :: err(:)
    real(kind=dp),allocatable :: dmin(:) ! observation time relative to analysis time (minutes)
  end type obstype 
!
! observation type (after obsope)
!
  type :: obstype2
    integer :: nobs
    integer,allocatable  :: elem(:)
    real(kind=dp),allocatable :: lon(:)
    real(kind=dp),allocatable :: lat(:)
    real(kind=dp),allocatable :: lev(:) !pressure[hPa]
    real(kind=dp),allocatable :: dat(:)
    real(kind=dp),allocatable :: err(:)
    real(kind=dp),allocatable :: dmin(:) ! observation time relative to analysis time (minutes)
    real(kind=dp),allocatable :: hxf(:)   ! h(x) control
    real(kind=dp),allocatable :: hxe(:,:)   ! h(x) ensemble
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
  ! conventional
  integer,parameter,public :: nobstype_conv=5 !U,V,T,Q,RH
  integer,parameter,public :: id_u_obs=2819
  integer,parameter,public :: id_v_obs=2820
  integer,parameter,public :: id_q_obs=3330
  integer,parameter,public :: id_rh_obs=3331
  real(kind=dp),parameter,public :: obserr_conv(nobstype_conv) = &
  & (/1.0d0,1.0d0,1.0d0,1.0d-3,10.0d0/)
  ! surface
  integer,parameter,public :: nobstype_surf=1 !Ps[,rain]
  integer,parameter,public :: id_ps_obs=14593
!  integer,parameter,public :: id_rain_obs=19999
  real(kind=dp),parameter,public :: obserr_surf(nobstype_surf) = &
  & (/100.0d0/)
  ! dcdf upper
  integer,parameter,public :: nobstype_upper=4 !T,Td,Wd,Ws
  integer,parameter,public :: id_t_obs=130   !Temperature[K]
  integer,parameter,public :: id_td_obs=3017 !Dew point temperature[K]
  integer,parameter,public :: id_wd_obs=3031 !Wind direction[degree]
  integer,parameter,public :: id_ws_obs=10   !Wind speed[m/s]
  real(kind=dp),parameter,public :: obserr_upper(nobstype_upper) = &
  & (/1.0d0,1.0d0,5.0d0,1.0d0/)
  ! all
  integer,parameter,public :: nobstype=9
  integer,parameter,public :: elem_id(nobstype)= &
  & (/id_u_obs,id_v_obs,id_t_obs,id_q_obs,id_rh_obs,id_ps_obs,&
  &   id_td_obs,id_wd_obs,id_ws_obs/)
  character(len=3),parameter,public :: obelmlist(nobstype) = &
  & (/'  U','  V','  T','  Q',' RH',' Ps',&
  &   ' Td',' Wd',' Ws'/)

!
! QC flags
!
  integer,parameter,public :: nqctype=5
  character(len=6),parameter,public :: qctype(nqctype)=&
  & (/'  Pass','Gerror','  High','   Low','Outreg'/)
  integer,parameter,public :: iqc_good=0
  integer,parameter,public :: iqc_gross_err=5
!  integer,parameter,public :: iqc_ps_ter=10
!  integer,parameter,public :: iqc_ref_low=11
!  integer,parameter,public :: iqc_ref_mem=12
  integer,parameter,public :: iqc_out_vhi=20
  integer,parameter,public :: iqc_out_vlo=21
  integer,parameter,public :: iqc_out_h=22
!  integer,parameter,public :: iqc_otype=90
!  integer,parameter,public :: iqc_time=91

! 
! debug
!
  logical, parameter :: debug=.false.

  public :: obstype, obstype2, uid_obs, &
   &  opendcdf, get_nobs_upper, read_upper, &
   &  obsin_allocate, obsin_deallocate, obsout_allocate, obsout_deallocate, &
   &  get_nobs, read_obs, write_obs, read_obsout, write_obsout, &
   &  obs_preproc, monit_obsin, ndate, nhour
contains
!
! convert obsID to sequential number
!
  function uid_obs(id_obs)
    implicit none
    integer :: id_obs, uid_obs

    select case(id_obs)
    case(id_u_obs)
      uid_obs=1
    case(id_v_obs)
      uid_obs=2
    case(id_t_obs)
      uid_obs=3
    case(id_q_obs)
      uid_obs=4
    case(id_rh_obs)
      uid_obs=5
    case(id_ps_obs)
      uid_obs=6
    case(id_td_obs)
      uid_obs=7
    case(id_wd_obs)
      uid_obs=8
    case(id_ws_obs)
      uid_obs=9
    case default
      uid_obs=-1
    end select
  end function uid_obs
!
! get # of observation for upper dcdf
!
  subroutine get_nobs_upper(cfile,atime,lmin,rmin,ndataall,nobs)
    implicit none
    character(len=*), intent(in) :: cfile
    integer, intent(in) :: atime(5) !year,month,day,hour,minutes
    integer, intent(in) :: lmin !observation time window (minutes)
    integer, intent(in) :: rmin !(lmin<=otime-atime<=rmin)
    integer, intent(out) :: ndataall
    integer, intent(out) :: nobs
    integer :: iunit
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

    iunit=91
    call opendcdf(iunit,cfile)
    ! get date
    ioffset=0
    call read_part1(iunit,ioffset,idrec,data1)
    if(debug) then
      print *, 'reading part 1 of record ',idrec
      print *, '# of address', nrecall
      print *, nrec1,nrec2,nrec3,nrec4,nrec5
      print *, data1
    end if
    nrec_time = nrecall
    irec=1+nrec1+nrec2
    do i=1,nrec3
      read(iunit,rec=irec) iymdhnw(i)
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
    call read_part1(iunit,ioffset,idrec,data1)
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
      read(iunit,rec=irec) ibuf2
      did(i) = int(ibuf2,kind=4)
      irec=irec+1
      read(iunit,rec=irec) ibuf2
      ndata(i) = int(ibuf2,kind=4)
      irec=irec+1
      read(iunit,rec=irec) ibuf2
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
        call read_part1(iunit,ioffset,idrec,data1)
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
          read(iunit,rec=irec) ibuf2
          imm=int(ibuf2,kind=4)/100
          idd=int(ibuf2,kind=4)-imm*100
          otime(2)=imm; otime(3)=idd
          irec=irec+1
          read(iunit,rec=irec) ibuf2
          ihh=int(ibuf2,kind=4)/100
          inn=int(ibuf2,kind=4)-ihh*100
          otime(4)=ihh;otime(5)=inn
          call nhour(atime,otime,imin)
          if((imin.ge.lmin).and.(imin.lt.rmin)) then
            if(debug) print *, 'obs time =',otime
            if(debug) print *, 'difference(minutes) =',imin
            irec=ioffset+nrec1+nrec2+nrec3+2
            read(iunit,rec=irec) ibuf2
            nloop = int(ibuf2,kind=4)
            nobs=nobs+nloop*nobstype_upper
          end if
        end if
        ioffset=ioffset+nrecall
        ndataall=ndataall+1
      end do
    end do
    print *, ntype,ndataall
    close(iunit)

    return
  end subroutine get_nobs_upper
!
! get observation for upper dcdf
!
  subroutine read_upper(cfile,atime,lmin,rmin,ndataall,obs)
    implicit none
    character(len=*), intent(in) :: cfile
    integer, intent(in) :: atime(5) !year,month,day,hour,minutes
    integer, intent(in) :: lmin !observation time window (minutes)
    integer, intent(in) :: rmin !(lmin<=otime-atime<=rmin)
    integer, intent(in) :: ndataall
    type(obstype), intent(inout) :: obs !input:obs%nobs=before time selecting
                                        !output:obs%nobs=after time selecting
    integer :: nobsuse
    integer :: iunit
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
    
    iunit=91
    call opendcdf(iunit,cfile)
    ioffset=nrec_time+nrec_info
    print *, 'ioffset ',ioffset
    ! set temporal obs arrays
    tmpobs%nobs = obs%nobs
    tmpobs2%nobs = obs%nobs
    call obsin_allocate( tmpobs )
    call obsin_allocate( tmpobs2 )
    ! start reading data
    nobsuse=0
    nn=0
    nsort=0
    otime(1)=atime(1)
    do n=1,ndataall
      ! read part 1 and extract information
      call read_part1(iunit,ioffset,idrec,data1)
      nrec_data = nrecall
      dtype = data1(1)
      tmplat = real(data1(2),kind=dp)*0.01d0
      tmplon = real(data1(3),kind=dp)*0.01d0
      if(nn==0) then
        latb=data1(2);lonb=data1(3)
      end if
      if((data1(2)/=latb).or.(data1(3)/=lonb)) then
        if(debug) then
          print *, 'latlon(previous) ',latb, lonb
          print *, 'latlon(current)  ',data1(2), data1(3)
        end if
        if(debug) print *, 'before sort ',nn
        call sortobs(nn,tmpobs)
        if(debug) print *, 'after sort ',nn
!        obs(nobsuse+1:nobsuse+nn)=tmpobs(1:nn)
!        call copyobs(nn,1,nobsuse+1,tmpobs,obs)
        tmpobs2%elem(nobsuse+1:nobsuse+nn) = tmpobs%elem(1:nn)
        tmpobs2%lon (nobsuse+1:nobsuse+nn) = tmpobs%lon (1:nn)
        tmpobs2%lat (nobsuse+1:nobsuse+nn) = tmpobs%lat (1:nn)
        tmpobs2%lev (nobsuse+1:nobsuse+nn) = tmpobs%lev (1:nn)
        tmpobs2%dat (nobsuse+1:nobsuse+nn) = tmpobs%dat (1:nn)
        tmpobs2%err (nobsuse+1:nobsuse+nn) = tmpobs%err (1:nn)
        tmpobs2%dmin(nobsuse+1:nobsuse+nn) = tmpobs%dmin(1:nn)
        nobsuse=nobsuse+nn
        if(debug) print *, 'nobsuse = ',nobsuse,'/',tmpobs%nobs
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
        read(iunit,rec=irec) ibuf2
        otime(2)=int(ibuf2,kind=4)/100
        otime(3)=int(ibuf2,kind=4)-otime(2)*100
        irec=irec+1
        read(iunit,rec=irec) ibuf2
        otime(4)=int(ibuf2,kind=4)/100
        otime(5)=int(ibuf2,kind=4)-otime(4)*100
        call nhour(atime,otime,imin)
        if((imin.ge.lmin).and.(imin.lt.rmin)) then
          if(debug) print *, 'obs time =',otime
          if(debug) print *, 'difference(minutes) =',imin
          tmpdt = real(imin,kind=dp)
          if(debug) then
            print *, 'dtype,lat,lon,dmin'
            print *, dtype,tmplat,tmplon,tmpdt
          end if
          irec=ioffset+nrec1+nrec2+nrec3+1
          read(iunit,rec=irec) ibuf2
          if(debug) print *, 'sst ',real(ibuf2,kind=dp)*0.1
          irec=irec+1
          read(iunit,rec=irec) ibuf2
          nloop = int(ibuf2,kind=4)
          if(debug) print *, 'nloop ',nloop 
          do i=1,nloop
            irec=irec+2
            read(iunit,rec=irec) ibuf2 !accuracy
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
            read(iunit,rec=irec) ibuf2
            if(acc(11:11)=='0'.and.ibuf2>0) then
              tmplev=real(ibuf2,kind=dp)*10.0d0 ![Pa]
            else
              irec=irec+7
              cycle
            end if
            irec=irec+3
            read(iunit,rec=irec) ibuf2
            if(acc(13:13)=='0'.and.ibuf2>0) then
              tmpelm=id_t_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
              tmperr=obserr_upper(1)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            if(acc(14:14)=='0'.and.ibuf2>0) then
              tmpelm=id_td_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
              tmperr=obserr_upper(2)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
            irec=irec+2 
            read(iunit,rec=irec) ibuf2
            if(acc(15:15)=='0'.and.ibuf2>0) then
              tmpelm=id_wd_obs
              tmpdat=real(ibuf2,kind=dp) ![degree]
              tmperr=obserr_upper(3)
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmperr,tmpdt)
            end if   
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            if(acc(16:16)=='0'.and.ibuf2>0) then
              tmpelm=id_ws_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![m/s]
              tmperr=obserr_upper(4)
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
      if(debug) print *, 'before sort ',nn
      call sortobs(nn,tmpobs)
      if(debug) print *, 'after sort ',nn
!      call copyobs(nn,1,nobsuse+1,tmpobs,obs)
!      obs(nobsuse+1:nobsuse+nn)=tmpobs(1:nn)
      tmpobs2%elem(nobsuse+1:nobsuse+nn) = tmpobs%elem(1:nn)
      tmpobs2%lon (nobsuse+1:nobsuse+nn) = tmpobs%lon (1:nn)
      tmpobs2%lat (nobsuse+1:nobsuse+nn) = tmpobs%lat (1:nn)
      tmpobs2%lev (nobsuse+1:nobsuse+nn) = tmpobs%lev (1:nn)
      tmpobs2%dat (nobsuse+1:nobsuse+nn) = tmpobs%dat (1:nn)
      tmpobs2%err (nobsuse+1:nobsuse+nn) = tmpobs%err (1:nn)
      tmpobs2%dmin(nobsuse+1:nobsuse+nn) = tmpobs%dmin(1:nn)
      nobsuse=nobsuse+nn
      if(debug) print *, 'nobsuse = ',nobsuse,'/',tmpobs%nobs
      nsort=nsort+1
    end if
    if(debug) print *, 'nsort = ',nsort
    ! output
    obs%nobs = nobsuse
    call obsin_allocate( obs )
    obs%elem(:) = tmpobs2%elem(1:nobsuse)
    obs%lon (:) = tmpobs2%lon (1:nobsuse)
    obs%lat (:) = tmpobs2%lat (1:nobsuse)
    obs%lev (:) = tmpobs2%lev (1:nobsuse)
    obs%dat (:) = tmpobs2%dat (1:nobsuse)
    obs%err (:) = tmpobs2%err (1:nobsuse)
    obs%dmin(:) = tmpobs2%dmin(1:nobsuse)
    close(iunit)

    return
  end subroutine read_upper

  subroutine read_part1(iunit,ioffset,idrec,data)
    implicit none
    integer, intent(in) :: iunit
    integer, intent(in) :: ioffset
    integer, intent(out) :: idrec
    integer, intent(out) :: data(7)
    integer(2) :: ibuf2(nrec1)
    integer :: i, irec

    irec=1+ioffset
    do i=1,nrec1
      read(iunit,rec=irec) ibuf2(i)
      irec=irec+1
    end do
    nrecall=int(ibuf2(1),kind=4)
    nrec2=int(ibuf2(3),kind=4);nrec3=int(ibuf2(4),kind=4)
    nrec4=int(ibuf2(5),kind=4);nrec5=int(ibuf2(6),kind=4)
    idrec=int(ibuf2(7),kind=4)
    data=int(ibuf2(8:),kind=4)
    return
  end subroutine read_part1

  subroutine setobs(n,obs,elem,lat,lon,lev,dat,err,dmin)
    implicit none
    integer, intent(in) :: n
    type(obstype), intent(inout) :: obs
    integer, intent(in) :: elem
    real(kind=dp), intent(in) :: lat,lon,lev,dat,err,dmin

    obs%elem(n) = elem
    obs%lat(n) = lat; obs%lon(n) = lon; obs%lev(n) = lev
    obs%dat(n) = dat; obs%err(n) = err
    obs%dmin(n) = dmin
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
       & size(tmpobs%dat),size(tmpobs%err),size(tmpobs%dmin)
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
    tmpobs%dmin(1:nobs) = obs%dmin(1:nobs)
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
        obs%dmin(nn+nelmsum(j)) = tmpobs%dmin(n)
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
    tmpobs%dmin(1:nobs) = obs%dmin(1:nobs)
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
      obs%dmin(nobs) = tmpobs%dmin(ns2)
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
          obs%dmin(nobs) = tmpobs%dmin(ns2)
          clev=tmplev(j)
        end do
      end if
      deallocate(tmplev,idx)
    end do
    call obsin_deallocate(tmpobs)
    return
  end subroutine sortobs

  subroutine opendcdf(iunit,cfile)
    implicit none
    integer, intent(in) :: iunit
    character(len=*), intent(in) :: cfile

    open(iunit,file=cfile,status='old',form='unformatted',&
         convert='big_endian',access='direct',recl=2)
 
    return
  end subroutine opendcdf
!
! convert (ws,wd)=>(u,v) and/or (t,td)=>(t,q)
!
  subroutine obs_preproc(obs,iwnd,iq)
    use func_module, only: calc_q, calc_uv
    implicit none
    type(obstype), intent(inout) :: obs
    integer, optional, intent(in) :: iwnd !wind
    integer, optional, intent(in) :: iq !humidity
    integer :: iwnd_, iq_
    type(obstype) :: tmpobs
    real(kind=dp) :: u, v
    real(kind=dp) :: td, q, p
    
    real(kind=dp) :: latb, lonb
    integer, parameter :: npointmax=10000
    integer :: nobseach(nobstype_upper,npointmax)
    integer :: n_ws, n_wd
    integer :: n,nn,npoint,i,j,itype,iter,itermax

    iwnd_=1
    if(present(iwnd)) iwnd_=iwnd
    iq_=1
    if(present(iq)) iq_=iq
    
    tmpobs%nobs = obs%nobs
    call obsin_allocate( tmpobs )

    n=0
    nn=0
    do while(.true.)
    if(n.ge.obs%nobs) exit
    npoint=0
    nobseach=0
    latb=0.0_dp
    lonb=0.0_dp
!    do n=1,obs%nobs
    do while (n.le.obs%nobs)
      n=n+1
      if(npoint.eq.0) then
        latb=obs%lat(n)
        lonb=obs%lon(n)
        npoint=npoint+1
      else if(obs%lat(n)/=latb.or.obs%lon(n)/=lonb) then
        print *, 'lat, lon=',latb,lonb
        print *, 'nobseach=',nobseach(:,npoint)
        latb=obs%lat(n)
        lonb=obs%lon(n)
        npoint=npoint+1
      end if
      if(npoint > npointmax ) exit
      select case(obs%elem(n))
      case(id_t_obs)
        nobseach(1,npoint)=nobseach(1,npoint)+1
      case(id_td_obs)
        nobseach(2,npoint)=nobseach(2,npoint)+1
      case(id_wd_obs)
        nobseach(3,npoint)=nobseach(3,npoint)+1
      case(id_ws_obs)
        nobseach(4,npoint)=nobseach(4,npoint)+1
      end select
    end do
    n=n-1
    npoint=npoint-1
    print *, 'nobs,npoint=',n,npoint

    do i=1,npoint
      do j=1,sum(nobseach(:,i))
        nn=nn+1
        if(iq_.eq.1.and.obs%elem(nn).eq.id_td_obs) then
          td = obs%dat(nn)
          p  = obs%lev(nn)
          call calc_q(td,p,q)
          obs%elem(nn)=id_q_obs
          obs%dat(nn) = q
          obs%err(nn) = obserr_conv(3)
        end if
        if(iwnd_.eq.1.and.obs%elem(nn).eq.id_wd_obs) then
          n_ws=nn+nobseach(3,i)
          !print *, obs%elem(nn), obs%elem(n_ws)
          !print *, obs%lev(nn), obs%lev(n_ws)
          call calc_uv(obs%dat(n_ws),obs%dat(nn),u,v)
          obs%elem(nn)=id_u_obs
          obs%dat(nn) = u
          obs%err(nn) = obserr_conv(1)
          obs%elem(n_ws) = id_v_obs
          obs%dat(n_ws) = v
          obs%err(n_ws) = obserr_conv(2)
        end if
      end do
    end do
    end do
    return
  end subroutine obs_preproc
!
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
    allocate( obs%dmin(obs%nobs) )

    obs%elem = 0
    obs%lon = 0.0_dp
    obs%lat = 0.0_dp
    obs%lev = 0.0_dp
    obs%dat = 0.0_dp
    obs%err = 0.0_dp
    obs%dmin = 0.0_dp

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
    if(allocated(obs%dmin)) deallocate(obs%dmin)
    return
  end subroutine obsin_deallocate
!
  subroutine obsout_allocate(obs,member)
    implicit none
    type(obstype2),intent(inout) :: obs
    integer, intent(in) :: member

    call obsout_deallocate(obs)

    allocate( obs%elem(obs%nobs) )
    allocate( obs%lon (obs%nobs) )
    allocate( obs%lat (obs%nobs) )
    allocate( obs%lev (obs%nobs) )
    allocate( obs%dat (obs%nobs) )
    allocate( obs%err (obs%nobs) )
    allocate( obs%dmin(obs%nobs) )
    allocate( obs%hxf (obs%nobs) )
    allocate( obs%qc  (obs%nobs) )

    obs%elem = 0
    obs%lon  = 0.0_dp
    obs%lat  = 0.0_dp
    obs%lev  = 0.0_dp
    obs%dat  = 0.0_dp
    obs%err  = 0.0_dp
    obs%dmin = 0.0_dp
    obs%hxf  = 0.0_dp
    obs%qc   = 0

    if(member.gt.0) then
      allocate( obs%hxe (member,obs%nobs) )
      obs%hxe = 0.0_dp
    end if

    return
  end subroutine obsout_allocate
!
  subroutine obsout_deallocate(obs)
    implicit none
    type(obstype2),intent(inout) :: obs

    if(allocated(obs%elem)) deallocate(obs%elem)
    if(allocated(obs%lon))  deallocate(obs%lon)
    if(allocated(obs%lat))  deallocate(obs%lat)
    if(allocated(obs%lev))  deallocate(obs%lev)
    if(allocated(obs%dat))  deallocate(obs%dat)
    if(allocated(obs%err))  deallocate(obs%err)
    if(allocated(obs%dmin)) deallocate(obs%dmin)
    if(allocated(obs%hxf)) deallocate(obs%hxf)
    if(allocated(obs%hxe)) deallocate(obs%hxe)
    if(allocated(obs%qc)) deallocate(obs%qc)

    return
  end subroutine obsout_deallocate
!
  subroutine get_nobs(cfile,nrec,nn)
    implicit none
    character(len=*), intent(in) :: cfile
    integer, intent(in) :: nrec
    integer, intent(out) :: nn
    real(kind=sp), allocatable :: wk(:)
    integer :: iunit,ios
    logical :: ex

    allocate( wk(nrec) )
    iunit=91
    nn=0
    inquire(file=cfile,exist=ex)
    if(ex) then
      open(iunit,file=cfile,form='unformatted',access='sequential')
      do
        read(iunit,iostat=ios) wk
        if(ios/=0) exit
        nn=nn+1
      end do
      close(iunit)
    else
      write(6,'(2a)') cfile,' does not exist'
    end if
    deallocate(wk)
    return
  end subroutine get_nobs
!
  subroutine read_obs(cfile,obs)
    implicit none
    character(len=*), intent(in) :: cfile
    type(obstype), intent(inout) :: obs
    real(kind=sp) :: wk(7)
    integer :: n, iunit

    iunit=91
    open(iunit,file=cfile,form='unformatted',access='sequential')
    do n=1,obs%nobs
      read(iunit) wk
      select case(nint(wk(1)))
      case(id_ps_obs)
        wk(5)=wk(5)*100.0 !hPa -> Pa
        wk(6)=wk(6)*100.0 !hPa -> Pa
      case default
        wk(4)=wk(4)*100.0 !hPa -> Pa
      end select
      if(debug) print *, wk
      obs%elem(n) = nint(wk(1))
      obs%lon (n) = real(wk(2),kind=dp)
      obs%lat (n) = real(wk(3),kind=dp)
      obs%lev (n) = real(wk(4),kind=dp)
      obs%dat (n) = real(wk(5),kind=dp)
      obs%err (n) = real(wk(6),kind=dp)
      obs%dmin(n) = real(wk(7),kind=dp)
    end do
    close(iunit)

    return
  end subroutine read_obs
!
  subroutine write_obs(cfile,obs)
    implicit none
    character(len=*), intent(in) :: cfile
    type(obstype), intent(in) :: obs
    real(kind=sp) :: wk(7)
    integer :: n, iunit

    iunit=92
    open(iunit,file=cfile,form='unformatted',access='sequential')

    do n=1,obs%nobs
      wk(1)=real(obs%elem(n),kind=sp)
      wk(2)=real(obs%lon (n),kind=sp)
      wk(3)=real(obs%lat (n),kind=sp)
      wk(4)=real(obs%lev (n),kind=sp)
      wk(5)=real(obs%dat (n),kind=sp)
      wk(6)=real(obs%err (n),kind=sp)
      wk(7)=real(obs%dmin(n),kind=sp)
      select case(nint(wk(1)))
      case(id_ps_obs)
        wk(5) = wk(5) * 0.01 !Pa->hPa
        wk(6) = wk(6) * 0.01 !Pa->hPa
      case default
        wk(4) = wk(4) * 0.01 !Pa->hPa
      end select
      if(debug) print *, wk
      write(iunit) wk
    end do
    close(iunit)

    return
  end subroutine write_obs
!
  subroutine read_obsout(cfile,obs,mem)
    implicit none
    character(len=*), intent(in) :: cfile
    type(obstype2), intent(inout) :: obs
    integer, intent(in) :: mem
    real(kind=sp) :: wk(9)
    integer :: n, iunit

    iunit=91
    open(iunit,file=cfile,form='unformatted',access='sequential')
    do n=1,obs%nobs
      read(iunit) wk
      select case(nint(wk(1)))
      case(id_ps_obs)
        wk(5)=wk(5)*100.0 !hPa -> Pa
        wk(6)=wk(6)*100.0 !hPa -> Pa
        wk(8)=wk(8)*100.0 !hPa -> Pa
      case default
        wk(4)=wk(4)*100.0 !hPa -> Pa
      end select
      if(debug) print *, wk
      obs%elem(n) = nint(wk(1))
      obs%lon (n) = real(wk(2),kind=dp)
      obs%lat (n) = real(wk(3),kind=dp)
      obs%lev (n) = real(wk(4),kind=dp)
      obs%dat (n) = real(wk(5),kind=dp)
      obs%err (n) = real(wk(6),kind=dp)
      obs%dmin(n) = real(wk(7),kind=dp)
      if(mem.eq.0) then
      obs%hxf(n) = real(wk(8),kind=dp)
      else
      obs%hxe(mem,n) = real(wk(8),kind=dp)
      end if
      obs%qc  (n) = nint(wk(9))
    end do
    close(iunit)

    return
  end subroutine read_obsout
!
  subroutine write_obsout(cfile,obs,mem)
    implicit none
    character(len=*), intent(in) :: cfile
    type(obstype2), intent(in) :: obs
    integer, intent(in) :: mem
    real(kind=sp) :: wk(9)
    integer :: n, iunit

    iunit=92
    open(iunit,file=cfile,form='unformatted',access='sequential')

    do n=1,obs%nobs
      wk(1)=real(obs%elem(n),kind=sp)
      wk(2)=real(obs%lon (n),kind=sp)
      wk(3)=real(obs%lat (n),kind=sp)
      wk(4)=real(obs%lev (n),kind=sp)
      wk(5)=real(obs%dat (n),kind=sp)
      wk(6)=real(obs%err (n),kind=sp)
      wk(7)=real(obs%dmin(n),kind=sp)
      if(mem.eq.0) then
      wk(8)=real(obs%hxf(n),kind=sp)
      else
      wk(8)=real(obs%hxe(mem,n),kind=sp)
      end if
      wk(9)=real(obs%qc  (n),kind=sp)
      select case(nint(wk(1)))
      case(id_ps_obs)
        wk(5) = wk(5) * 0.01 !Pa->hPa
        wk(6) = wk(6) * 0.01 !Pa->hPa
        wk(8) = wk(8) * 0.01 !Pa->hPa
      case default
        wk(4) = wk(4) * 0.01 !Pa->hPa
      end select
      if(debug) print *, wk
      write(iunit) wk
    end do
    close(iunit)

    return
  end subroutine write_obsout
!
! Monitor input observation
!
  subroutine monit_obsin(nobsall,elem,dat)
    implicit none
    integer, intent(in) :: nobsall
    integer, intent(in) :: elem(nobsall)
    real(kind=dp), intent(in) :: dat(nobsall)
    integer :: nobs(nobstype)
    real(kind=dp) :: mean(nobstype)
    real(kind=dp) :: stdv(nobstype)
    integer :: n,i

    character(len=12) :: var_show(nobstype)
    character(len=12) :: nobs_show(nobstype)
    character(len=12) :: mean_show(nobstype)
    character(len=12) :: stdv_show(nobstype)
    character(len=12) :: nqc_show(nobstype,nqctype)

    character(len=4) :: nstr
    character(len=12) :: tmpstr(nobstype), tmpstr2(nobstype)

    nobs(:) = 0
    mean(:) = 0.0_dp
    stdv(:) = 0.0_dp
    do n=1,nobsall
      i = uid_obs(elem(n))
      nobs(i)=nobs(i)+1
      mean(i)=mean(i)+dat(n)
      stdv(i)=stdv(i)+dat(n)**2
    end do
    do i=1,nobstype
      if(nobs(i).eq.0) then
        mean(i)=undef
        stdv(i)=undef
      else
        mean(i)=mean(i)/real(nobs(i),kind=dp)
        stdv(i)=sqrt(stdv(i)/real(nobs(i),kind=dp)-mean(i)**2)
      end if
    end do

    n=0
    do i=1,nobstype
      n=n+1
      write(var_show(n),'(a12)') obelmlist(i)
      write(nobs_show(n),'(i12)') nobs(i)
      if(nobs(i).gt.0) then
        write(mean_show(n),'(es12.3)') mean(i)
        write(stdv_show(n),'(es12.3)') stdv(i)
      else
        write(mean_show(n),'(a12)') 'N/A'
        write(stdv_show(n),'(a12)') 'N/A'
      end if
    end do
    write(nstr,'(i4)') n
    tmpstr(1:n) = '============'
    tmpstr2(1:n) = '------------'

    write(6,'(a,'//trim(nstr)//'a)') '======',tmpstr(1:n)
    write(6,'(6x,'//trim(nstr)//'a)') var_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') '------',tmpstr2(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'MEAN  ',mean_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'STD   ',stdv_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'NUMBER',nobs_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') '======',tmpstr(1:n)
    return
  end subroutine monit_obsin
!
! calculate date before or after several hours
! [note] use library w3_4 in sys/lib
!
  subroutine ndate(date0,dt,date1)
    implicit none
    integer, intent(in)  :: date0(5) !year,month,day,hour,minutes
    integer, intent(in)  :: dt    !minutes
    integer, intent(out) :: date1(5) !year,month,day,hour,minutes 
    integer :: idat(8),jdat(8) !year,month,day,timezone,hour,minutes,seconds,milliseconds
    real(kind=sp) :: rinc(5) !days,hours,minutes,seconds,milliseconds

!    print *, date0
    idat = 0
    idat(1:3) = date0(1:3)
    idat(5:6) = date0(4:5)
!    print *, idat
    rinc = 0.0
    rinc(3) = real(dt,kind=sp)
!    print *, rinc

    call w3movdat(rinc,idat,jdat)
!    print *, jdat
    date1(1:3)=jdat(1:3)
    date1(4:5)=jdat(5:6)

    return
  end subroutine ndate
!
! calculate hours between two dates
! [note] use library w3_4 in sys/lib
!
  subroutine nhour(date0,date1,dt)
    implicit none
    integer, intent(in)  :: date0(5) !year,month,day,hour,minutes
    integer, intent(in)  :: date1(5) !year,month,day,hour,minutes 
    integer, intent(out) :: dt    !minutes (date1 - date0)
    integer :: idat(8),jdat(8) !year,month,day,timezone,hour,minutes,seconds,milliseconds
    real(kind=sp) :: rinc(5) !days,hours,minutes,seconds,milliseconds

!    print *, date0
    idat = 0
    idat(1:3) = date0(1:3)
    idat(5:6) = date0(4:5)
!    print *, idat
!    print *, date1
    jdat = 0
    jdat(1:3) = date1(1:3)
    jdat(5:6) = date1(4:5)
!    print *, jdat

    call w3difdat(jdat,idat,3,rinc)
!    print *, rinc
    dt = nint(rinc(3))

    return
  end subroutine nhour
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
!!       & size(obs2%dat),size(obs2%err),size(obs2%dmin)
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
!!      tmpdt (i) = obs1%dmin(i+ns1-1)
!!!      obs2%elem(i+ns2-1)=obs1%elem(i+ns1-1)
!!!      obs2%lon (i+ns2-1)=obs1%lon (i+ns1-1)
!!!      obs2%lat (i+ns2-1)=obs1%lat (i+ns1-1)
!!!      obs2%lev (i+ns2-1)=obs1%lev (i+ns1-1)
!!!      obs2%dat (i+ns2-1)=obs1%dat (i+ns1-1)
!!!      obs2%err (i+ns2-1)=obs1%err (i+ns1-1)
!!!      obs2%dmin(i+ns2-1)=obs1%dmin(i+ns1-1)
!!    end do
!!    obs2%elem(ns2:ns2+n-1) = tmpelm(:)
!!    obs2%lon (ns2:ns2+n-1) = tmplon(:)
!!    obs2%lat (ns2:ns2+n-1) = tmplat(:)
!!    obs2%lev (ns2:ns2+n-1) = tmplev(:)
!!    obs2%dat (ns2:ns2+n-1) = tmpdat(:)
!!    obs2%err (ns2:ns2+n-1) = tmperr(:)
!!    obs2%dmin(ns2:ns2+n-1) = tmpdt(:)
!!    deallocate( tmpelm,tmplon,tmplat,tmplev,tmpdat,tmperr,tmpdt )
!!    return
!!  end subroutine copyobs
end module obs_module
