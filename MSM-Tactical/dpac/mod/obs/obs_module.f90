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
    real(kind=dp),allocatable :: lev(:) !pressure[hPa] or elevation[m]
    real(kind=dp),allocatable :: dat(:)
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
    real(kind=dp),allocatable :: lev(:) !pressure[hPa] or elevation[m]
    real(kind=dp),allocatable :: dat(:)
    real(kind=dp),allocatable :: err(:)
    real(kind=dp),allocatable :: dmin(:) ! observation time relative to analysis time (minutes)
    real(kind=dp),allocatable :: hxf(:)   ! h(x) control
    real(kind=dp),allocatable :: hxe(:,:)   ! h(x) ensemble
    real(kind=dp),allocatable :: corr(:) ! (for ps and t2m) bias correction
    integer,allocatable       :: qc(:)    ! QC flag
    integer,allocatable       :: img(:)   ! image number whose domain includes obs location
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
  integer,save      :: filetime(6) !year,month,day,hour,minutes,day in week
!
! observation ID
!
  ! conventional
  integer,parameter,public :: nobstype_conv=5 !U,V,T,Q,RH
  integer,parameter,public :: id_u_obs=2819
  integer,parameter,public :: id_v_obs=2820
  integer,parameter,public :: id_q_obs=3330
  integer,parameter,public :: id_rh_obs=3331
!  real(kind=dp),parameter,public :: obserr_conv(nobstype_conv) = &
!  & (/1.0d0,1.0d0,1.0d-3,1.0d-1/)
  ! surface
  integer,parameter,public :: nobstype_synop=2 !Ps,T2m[,rain]
  integer,parameter,public :: id_ps_obs=14593
  integer,parameter,public :: id_t2m_obs=10167
!  integer,parameter,public :: id_rain_obs=19999
!  real(kind=dp),parameter,public :: obserr_surf(nobstype_synop) = &
!  & (/100.0d0/)
  ! dcdf upper
  integer,parameter,public :: nobstype_upper=4 !T,Td,Wd,Ws
  integer,parameter,public :: id_t_obs=130   !Temperature[K]
  integer,parameter,public :: id_td_obs=3017 !Dew point temperature[K]
  integer,parameter,public :: id_wd_obs=3031 !Wind direction[degree]
  integer,parameter,public :: id_ws_obs=10   !Wind speed[m/s]
!  real(kind=dp),parameter,public :: obserr_upper(nobstype_upper) = &
!  & (/1.0d0,2.0d0,10.0d0,1.0d0/)
  ! all
  integer,parameter,public :: nobstype=10
  integer,parameter,public :: elem_id(nobstype)= &
  & (/id_u_obs,id_v_obs,id_t_obs,id_q_obs,id_rh_obs,id_ps_obs,&
  &   id_t2m_obs,id_td_obs,id_wd_obs,id_ws_obs/)
  integer,parameter,public :: nobstyperaw=6
  integer,parameter,public :: elem_idraw(nobstyperaw)= &
  & (/id_t_obs,id_td_obs,id_wd_obs,id_ws_obs,id_ps_obs,id_t2m_obs/)

  ! mandatory levels
  integer,parameter,public :: nlevfix=25
  real(kind=dp),parameter,public :: plevfix(25) = &
  & (/1000.0d2,925.0d2,900.0d2,850.0d2,800.0d2,700.0d2,600.0d2,500.0d2,400.0d2,&
  &            350.0d2,300.0d2,250.0d2,200.0d2,175.0d2,150.0d2,125.0d2,100.0d2,&
  &             70.0d2, 50.0d2, 40.0d2, 30.0d2, 20.0d2, 15.0d2, 10.0d2,  5.0d2/)
  character(len=3),parameter,public :: obelmlist(nobstype) = &
  & (/'  U','  V','  T','  Q',' RH',' Ps',&
  &   'T2m',' Td',' Wd',' Ws'/)
  real(kind=dp),parameter,public :: obserr(nlevfix,nobstype) = reshape( &
  & (/1.4d0,1.5d0,1.5d0,1.5d0,1.6d0,1.6d0,1.9d0,2.1d0,2.6d0,& !U
  &   2.8d0,3.0d0,3.2d0,2.7d0,2.6d0,2.4d0,2.3d0,2.1d0,2.1d0,& !U
  &   2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,&             !U
  &   1.4d0,1.5d0,1.5d0,1.5d0,1.6d0,1.6d0,1.9d0,2.1d0,2.6d0,& !V
  &   2.8d0,3.0d0,3.2d0,2.7d0,2.6d0,2.4d0,2.3d0,2.1d0,2.1d0,& !V
  &   2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,&             !V
  &   1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,& !T
  &   1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,0.9d0,0.8d0,0.8d0,& !T
  &   0.9d0,0.9d0,1.0d0,1.3d0,1.4d0,1.5d0,1.5d0,&             !T
  &   2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,& !Q
  &   2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,& !Q
  &   2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,2.0d-3,&               !Q
  &   1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,& !RH
  &   1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,& !RH
  &   1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,1.0d-1,&               !RH
  &   1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,& !Ps
  &   1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,& !Ps
  &   1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,1.1d2,&             !Ps
  &   1.2d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,& !T2m
  &   1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,0.9d0,0.8d0,0.8d0,& !T2m
  &   0.9d0,0.9d0,1.0d0,1.3d0,1.4d0,1.5d0,1.5d0,&             !T2m
  &   1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,& !Td
  &   1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,1.0d0,0.9d0,0.8d0,0.8d0,& !Td
  &   0.9d0,0.9d0,1.0d0,1.3d0,1.4d0,1.5d0,1.5d0,&             !Td
  &   1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,& !Wd
  &   1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,& !Wd
  &   1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,1.0d1,&             !Wd
  &   1.4d0,1.5d0,1.5d0,1.5d0,1.6d0,1.6d0,1.9d0,2.1d0,2.6d0,& !Ws
  &   2.8d0,3.0d0,3.2d0,2.7d0,2.6d0,2.4d0,2.3d0,2.1d0,2.1d0,& !Ws
  &   2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,2.1d0,2.1d0 &             !Ws
  &   /),(/nlevfix,nobstype/))
!
! QC flags
!
  integer,parameter,public :: nqctype=6
  character(len=6),parameter,public :: qctype(nqctype)=&
  & (/'  Pass','Gerror','  High','   Low','Outreg','Notuse'/)
  integer,parameter,public :: iqc_good=0
  integer,parameter,public :: iqc_gross_err=5
!  integer,parameter,public :: iqc_ps_ter=10
!  integer,parameter,public :: iqc_ref_low=11
!  integer,parameter,public :: iqc_ref_mem=12
  integer,parameter,public :: iqc_out_vhi=20
  integer,parameter,public :: iqc_out_vlo=21
  integer,parameter,public :: iqc_out_h=22
  integer,parameter,public :: iqc_otype=90
!  integer,parameter,public :: iqc_time=91
!
! IO
!
  character(len=4) :: filesuffix='.dat'
! 
! debug
!
  logical, parameter :: debug=.false.

  public :: obstype, obstype2, uid_obs, &
   &  opendcdf, get_nobs_dcdf, read_upper, read_synop, &
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
    case(id_t2m_obs)
      uid_obs=7
    case(id_td_obs)
      uid_obs=8
    case(id_wd_obs)
      uid_obs=9
    case(id_ws_obs)
      uid_obs=10
    case default
      uid_obs=-1
    end select
  end function uid_obs
!
! get # of observation for dcdf
!
  subroutine get_nobs_dcdf(dtypename,cfile,atime,lmin,rmin,ndataall,nobs)
    implicit none
    character(len=5), intent(in) :: dtypename !upper,surf,tovs,misc
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
    integer :: otime(5),tmptime(5)
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
    ! file time
    do i=1,nrec3
      read(iunit,rec=irec) iymdhnw(i)
      filetime(i) = int(iymdhnw(i),kind=4)
      irec=irec+1
    end do
    print '(a,6i5)', 'filetime ',filetime
    otime(1)=filetime(1)
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
      if(dtypename=='upper') then
        ! use only 3XXX data type
        if(did(i)/1000.eq.3) then
          ! read hour and minutes
          irec=ioffset+nrec1+5
          read(iunit,rec=irec) ibuf2
          imm=int(ibuf2,kind=4)/100
          idd=int(ibuf2,kind=4)-imm*100
          irec=irec+1
          read(iunit,rec=irec) ibuf2
          ihh=int(ibuf2,kind=4)/100
          inn=int(ibuf2,kind=4)-ihh*100
          otime(2)=imm; otime(3)=idd
          otime(4)=ihh;otime(5)=inn
          if (filetime(4).eq.0 .and. ihh.gt.20 ) then !00UTC=>21~20UTC
            tmptime=otime
            call ndate(tmptime,-24*60,otime) !a day before
          end if
          call nhour(atime,otime,imin)
!          if(((lmin.ne.rmin).and.(imin.ge.lmin).and.(imin.lt.rmin))&
!                  .or.(imin.eq.lmin))then
            if(debug) print *, 'obs time =',otime
            if(debug) print *, 'difference(minutes) =',imin
            irec=ioffset+nrec1+nrec2+nrec3+2
            read(iunit,rec=irec) ibuf2
            nloop = int(ibuf2,kind=4)
            nobs=nobs+nloop*nobstype_upper
!          end if
        end if
      else if(dtypename=='surf ') then
        ! use all data type except METAR(18XX)
        if(did(i)/100.ne.18) then
          ! read hour and minutes
          irec=ioffset+nrec1+5
          read(iunit,rec=irec) ibuf2
          imm=int(ibuf2,kind=4)/100
          idd=int(ibuf2,kind=4)-imm*100
          irec=irec+1
          read(iunit,rec=irec) ibuf2
          ihh=int(ibuf2,kind=4)/100
          inn=int(ibuf2,kind=4)-ihh*100
          otime(2)=imm; otime(3)=idd
          otime(4)=ihh;otime(5)=inn
          if (filetime(4).eq.0 .and. ihh.gt.20 ) then !00UTC=>21~20UTC
            tmptime=otime
            call ndate(tmptime,-24*60,otime) !a day before
          end if
          call nhour(atime,otime,imin)
          if(((lmin.ne.rmin).and.(imin.ge.lmin).and.(imin.lt.rmin))&
                  .or.(imin.eq.lmin))then
            if(debug) print *, 'obs time =',otime
            if(debug) print *, 'difference(minutes) =',imin
            nobs=nobs+nobstype_synop
          end if
        end if
      end if
        ioffset=ioffset+nrecall
        ndataall=ndataall+1
      end do
    end do
    print *, ntype,ndataall
    close(iunit)

    return
  end subroutine get_nobs_dcdf
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
    integer :: otime(5),tmptime(5)
    integer :: tmpelm !, tmphour, tmpminu
    real(kind=dp) :: tmplon, tmplat
    real(kind=dp) :: tmplev, tmpdat, tmperr
    real(kind=dp) :: tmpdt
    integer(2) :: ibuf2
    integer :: lonb, latb
    integer :: nloop, nsort
    character(len=16) :: acc
    character(len=8) :: sta
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
    nn=-1
    nsort=0
    otime(1)=atime(1)
    do n=1,ndataall
      ! read part 1 and extract information
      call read_part1(iunit,ioffset,idrec,data1)
      nrec_data = nrecall
      dtype = data1(1)
      tmplat = real(data1(2),kind=dp)*0.01d0
      tmplon = real(data1(3),kind=dp)*0.01d0
      if((abs(tmplon).gt.180.0).or.(abs(tmplat).gt.90.0)) then
        ! invalid data
        ioffset=ioffset+nrec_data
        cycle
      end if
      if(tmplon.lt.0.0d0) then !-180.0<=lon<180 -> 0.0<=lon<360.0
        tmplon=tmplon+360.0
      end if
      if(nn<0) then
        latb=data1(2);lonb=data1(3)
        nn=nn+1
      end if
      if((data1(2)/=latb).or.(data1(3)/=lonb)) then
!        if(debug) then
          print *, 'latlon(previous) ',latb, lonb
          print *, 'latlon(current)  ',data1(2), data1(3)
!        end if
        if(nn>0) then
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
        tmpobs2%dmin(nobsuse+1:nobsuse+nn) = tmpobs%dmin(1:nn)
        nobsuse=nobsuse+nn
        !if(debug)
        print *, 'nn=',nn,' nobsuse = ',nobsuse,'/',tmpobs%nobs
        nsort=nsort+1
        end if
        nn=0
        latb=data1(2);lonb=data1(3)
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
        if (filetime(4).eq.0 .and. otime(4).gt.20 ) then !00UTC=>21~20UTC
          tmptime=otime
          call ndate(tmptime,-24*60,otime) !a day before
        end if
        call nhour(atime,otime,imin)
        if(((lmin.ne.rmin).and.(imin.ge.lmin).and.(imin.lt.rmin))&
                .or.(imin.eq.lmin))then
          if(debug) print *, 'obs time =',otime
          if(debug) print *, 'difference(minutes) =',imin
          tmpdt = real(imin,kind=dp)
          if(debug) then
            print *, 'dtype,lat,lon,dmin'
            print *, dtype,tmplat,tmplon,tmpdt
          end if
          ! station number / call sign
          irec=ioffset+nrec1+nrec2+1
          do i=1,4
            read(iunit,rec=irec) sta(2*i-1:2*i)
            irec=irec+1
          end do
          !if ((sta(1:4)=='7KBR').or.(sta(1:4)=='7JEJ').or.(sta(1:4)=='7JJW')) then
          !!! skip 3 ships' observations
          !else
          !print *, 'station ', sta
          ! part4
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
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmpdt)
            end if   
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            if(acc(14:14)=='0'.and.ibuf2>0) then
              tmpelm=id_td_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmpdt)
            end if   
            irec=irec+2 
            read(iunit,rec=irec) ibuf2
            if(acc(15:15)=='0'.and.ibuf2>0) then
              tmpelm=id_wd_obs
              tmpdat=real(ibuf2,kind=dp) ![degree]
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,&
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmpdt)
            end if   
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            if(acc(16:16)=='0'.and.ibuf2>0) then
              tmpelm=id_ws_obs
              tmpdat=real(ibuf2,kind=dp)*0.1d0 ![m/s]
              nn=nn+1
              call setobs(nn,tmpobs,&
              &  tmpelm,& 
              &  tmplat,tmplon,tmplev,&
              &  tmpdat,tmpdt)
            end if   
          end do
          !end if ! skip ship obs
        end if ! time window
      end if ! dtype
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
      tmpobs2%dmin(nobsuse+1:nobsuse+nn) = tmpobs%dmin(1:nn)
      nobsuse=nobsuse+nn
      !if(debug)
      print *, 'nn = ',nn,' nobsuse = ',nobsuse,'/',tmpobs%nobs
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
    obs%dmin(:) = tmpobs2%dmin(1:nobsuse)
    close(iunit)

    return
  end subroutine read_upper
!
! get observation for synoptic dcdf
!
  subroutine read_synop(cfile,atime,lmin,rmin,ndataall,obs)
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
    integer :: otime(5),tmptime(5)
    integer :: tmpelm !, tmphour, tmpminu
    real(kind=dp) :: tmplon, tmplat
    real(kind=dp) :: tmplev, tmpdat, tmperr
    real(kind=dp) :: tmpdt
    integer(2) :: ibuf2
    integer :: lonb, latb
    integer :: nsort, relelv
    character(len=16) :: acc
    integer :: n,nn,i,irec
    
    iunit=91
    call opendcdf(iunit,cfile)
    ioffset=nrec_time+nrec_info
    print *, 'ioffset ',ioffset
    ! set temporal obs arrays
    tmpobs%nobs = obs%nobs
    call obsin_allocate( tmpobs )
    ! start reading data
    nobsuse=0
    nsort=0
    latb=-32767
    lonb=-32767
    otime(1)=atime(1)
    do n=1,ndataall
      ! read part 1 and extract information
      call read_part1(iunit,ioffset,idrec,data1)
      nrec_data = nrecall
      dtype = data1(1)
      tmplat = real(data1(2),kind=dp)*0.01d0
      tmplon = real(data1(3),kind=dp)*0.01d0
      if((abs(tmplon).gt.180.0).or.(abs(tmplat).gt.90.0)) then
        ! invalid data
        ioffset=ioffset+nrec_data
        cycle
      end if
      if(tmplon.lt.0.0d0) then !-180.0<=lon<180 -> 0.0<=lon<360.0
        tmplon=tmplon+360.0
      end if
      if(dtype.ne.1250.and.mod(dtype,100).eq.50) then !BUFR(deprecated)
        ioffset=ioffset+nrec_data
        cycle
      end if
      ! use all data type except for METAR(18XX)
      if(dtype/100.ne.18) then
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
        if (filetime(4).eq.0 .and. otime(4).gt.20 ) then !00UTC=>21~20UTC
          tmptime=otime
          call ndate(tmptime,-24*60,otime) !a day before
        end if
        call nhour(atime,otime,imin)
        if(((lmin.ne.rmin).and.(imin.ge.lmin).and.(imin.lt.rmin))&
                .or.(imin.eq.lmin))then
          if((data1(2).eq.latb).and.(data1(3).eq.lonb)) then !deprecated point
            ioffset=ioffset+nrec_data
            cycle
          else
            latb=data1(2); lonb=data1(3)
          end if
          if(debug) then
            print *, 'obs time =',otime
            print *, 'difference(minutes) =',imin
          end if
          tmpdt = real(imin,kind=dp)
          if(debug) then
            print *, 'dtype,lat,lon,dmin'
            print *, dtype,tmplat,tmplon,tmpdt
          end if
          ! get elevation for SYNOP
          if(dtype.lt.2000) then
            irec=ioffset+nrec1+nrec2+7
            read(iunit,rec=irec) ibuf2
            tmplev=real(ibuf2,kind=dp) ![m]
            if(tmplev.lt.0) then
              ioffset=ioffset+nrec_data
              cycle
            end if
            if(dtype.eq.1300) then !synop mobil
              irec=ioffset+nrec1+nrec2+11
              read(iunit,rec=irec) ibuf2
              relelv=int(ibuf2)
              if(relelv.gt.3) then !bad elevation
                ioffset=ioffset+nrec_data
                cycle
              end if
            end if
          else
            tmplev=0.0d0
          end if
          if(debug) print *, 'elev ',tmplev
          ! accuracy check for BUOY
          if(dtype.eq.2800) then
            irec=ioffset+nrec1+nrec2+9
            read(iunit,rec=irec) ibuf2
            write(acc,'(b16.16)') ibuf2
            if(debug) then
              print *, 'accuracy'
              print *, ibuf2
              print *, acc
            end if
            if(acc(1:4)/='0000') then !pressure measurement is low quality
              ioffset=ioffset+nrec_data
              cycle
            end if 
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            write(acc,'(b16.16)') ibuf2
            if(debug) then
              print *, 'accuracy'
              print *, ibuf2
              print *, acc
            end if
            if(acc(13:16)=='0010') then !buoy location is low quality
              ioffset=ioffset+nrec_data
              cycle
            end if 
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            write(acc,'(b16.16)') ibuf2
            if(debug) then
              print *, 'accuracy'
              print *, ibuf2
              print *, acc
            end if
            if((acc(9:12)/='0000').and.(acc(9:12)/='0001')) then !buoy location is low quality
              ioffset=ioffset+nrec_data
              cycle
            end if 
            ! replace location
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            tmplat = real(ibuf2,kind=dp)*0.01d0
            irec=irec+1
            read(iunit,rec=irec) ibuf2
            tmplon = real(ibuf2,kind=dp)*0.01d0
            if(tmplon.lt.0.0d0) then !-180.0<=lon<180 -> 0.0<=lon<360.0
              tmplon=tmplon+360.0
            end if
          end if
          !! Ps
          irec=ioffset+nrec1+nrec2+nrec3+1
          read(iunit,rec=irec) ibuf2
          tmpelm=id_ps_obs
          tmpdat=real(ibuf2,kind=dp)*10.0d0 ![Pa]
          if(tmpdat.gt.0) then
            if(debug) print *, 'Ps ',tmpdat
            nobsuse=nobsuse+1
            call setobs(nobsuse,tmpobs,&
            &  tmpelm,&
            &  tmplat,tmplon,tmplev,&
            &  tmpdat,tmpdt)
          end if
          !! T2m
          !irec=irec+6
          !read(iunit,rec=irec) ibuf2
          !tmpelm=id_t2m_obs
          !tmpdat=real(ibuf2,kind=dp)*0.1d0 ![K]
          !if(tmpdat.gt.0) then
          !  if(debug) print *, 'T2m ',tmpdat
          !  nobsuse=nobsuse+1
          !  call setobs(nobsuse,tmpobs,&
          !  &  tmpelm,&
          !  &  tmplat,tmplon,tmplev,&
          !  &  tmpdat,tmpdt)
          !end if
        end if
      end if
      ioffset=ioffset+nrec_data
    end do
    ! output
    obs%nobs = nobsuse
    call obsin_allocate( obs )
    obs%elem(:) = tmpobs%elem(1:nobsuse)
    obs%lon (:) = tmpobs%lon (1:nobsuse)
    obs%lat (:) = tmpobs%lat (1:nobsuse)
    obs%lev (:) = tmpobs%lev (1:nobsuse)
    obs%dat (:) = tmpobs%dat (1:nobsuse)
    obs%dmin(:) = tmpobs%dmin(1:nobsuse)
    close(iunit)

    return
  end subroutine read_synop

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

  subroutine setobs(n,obs,elem,lat,lon,lev,dat,dmin)
    implicit none
    integer, intent(in) :: n
    type(obstype), intent(inout) :: obs
    integer, intent(in) :: elem
    real(kind=dp), intent(in) :: lat,lon,lev,dat,dmin

    obs%elem(n) = elem
    obs%lat(n) = lat; obs%lon(n) = lon; obs%lev(n) = lev
    obs%dat(n) = dat; obs%dmin(n) = dmin
    return
  end subroutine setobs

  subroutine sortobs(nobs,obs)
    use stdlib_sorting
    implicit none
    integer, intent(inout) :: nobs
    type(obstype), intent(inout) :: obs
    integer :: nobs1
    integer :: nelm(0:nobstype_upper), nelmsum(nobstype_upper)
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
       & size(tmpobs%dat),size(tmpobs%dmin)
    end if
!    allocate( tmpobs(nobs) )
    ! element sort
    nelm(:)=0
    do j=1,nobstype_upper
      do n=1,nobs
        if( obs%elem(n) /= elem_idraw(j) ) cycle
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
    tmpobs%dmin(1:nobs) = obs%dmin(1:nobs)
    do j=1,nobstype_upper
      if( nelm(j)==0 ) cycle
      nn=0
      do n=1,nobs
        if( tmpobs%elem(n) /= elem_idraw(j) ) cycle
        nn=nn+1
!        call copyobs(1,n,nn+nelmsum(j),tmpobs,obs)
!        obs(nn+nelmsum(j)) = tmpobs(n)
        obs%elem(nn+nelmsum(j)) = tmpobs%elem(n)
        obs%lon (nn+nelmsum(j)) = tmpobs%lon (n)
        obs%lat (nn+nelmsum(j)) = tmpobs%lat (n)
        obs%lev (nn+nelmsum(j)) = tmpobs%lev (n)
        obs%dat (nn+nelmsum(j)) = tmpobs%dat (n)
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
! convert (ws,wd)=>(u,v) and/or (t,td)=>(t,q[iq=1] or rh[iq=2])
!
  subroutine obs_preproc(obs,iwnd,iq)
    use func_module, only: calc_q, calc_rh, calc_uv
    implicit none
    type(obstype), intent(inout) :: obs
    integer, optional, intent(in) :: iwnd !wind
    integer, optional, intent(in) :: iq !humidity
    integer :: iwnd_, iq_
    type(obstype) :: tmpobs
    real(kind=dp) :: u, v
    real(kind=dp) :: t, td, q, p, rh
    
    real(kind=dp) :: latb, lonb
    integer, parameter :: npointmax=10000
!!debug    real(kind=dp) :: lats(npointmax),lons(npointmax)
    integer :: nobseach(nobstype_upper,npointmax)
    integer :: n_t, n_ws, n_wd
    integer :: n,nn,n2,npoint,i,j,itype,iter,itermax

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
!!debug        lats(npoint)=latb
!!debug        lons(npoint)=lonb
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

    n2=0
    do i=1,npoint
!!debug      print *, 'lat, lon=',lats(i),lons(i)
      do j=1,sum(nobseach(:,i))
        nn=nn+1
        if(iq_.ge.1.and.obs%elem(nn).eq.id_td_obs) then
          td = obs%dat(nn)
          p  = obs%lev(nn)
          if(iq_.eq.1) then !Td=>Q
            call calc_q(td,p,q)
            obs%elem(nn)=id_q_obs
            obs%dat(nn) = q
          else if(iq_.eq.2) then !Td=>rh
            do n_t=n2+1,n2+sum(nobseach(:,i))
              if(obs%elem(n_t)==id_t_obs.and.obs%lev(n_t)==p) exit
            end do
            if (n_t.gt.n2+sum(nobseach(:,i))) cycle
            !n_t=nn-nobseach(1,i)
            !print *, obs%elem(nn), obs%elem(n_t)
            !print *, obs%lev(nn), obs%lev(n_t)
            t = obs%dat(n_t)
            !!! debug
            !print '(3(a,f10.2))', 'p', p, ' t', t, ' td',td
            !!! debug
            call calc_q(td,p,q)
            call calc_rh(t,q,p,rh)
            !!! debug
            !print '(2(a,f10.2),a,es10.2)', 'p', p, ' t', t, ' rh',rh
            !!! debug
            obs%elem(nn)=id_rh_obs
            obs%dat(nn) = rh
          end if
        end if
        if(iwnd_.eq.1.and.obs%elem(nn).eq.id_wd_obs) then
          do n_ws=n2+1,n2+sum(nobseach(:,i))
            if(obs%elem(n_ws)==id_ws_obs.and.obs%lev(n_ws)==obs%lev(nn)) exit
          end do
          if (n_ws.gt.n2+sum(nobseach(:,i))) cycle
          !n_ws=nn+nobseach(3,i)
          !print *, obs%elem(nn), obs%elem(n_ws)
          !print *, obs%lev(nn), obs%lev(n_ws)
          call calc_uv(obs%dat(n_ws),obs%dat(nn),u,v)
          obs%elem(nn)=id_u_obs
          obs%dat(nn) = u
          obs%elem(n_ws) = id_v_obs
          obs%dat(n_ws) = v
        end if
      end do
      n2=n2+sum(nobseach(:,i))
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
    allocate( obs%dmin(obs%nobs) )

    obs%elem = 0
    obs%lon = 0.0_dp
    obs%lat = 0.0_dp
    obs%lev = 0.0_dp
    obs%dat = 0.0_dp
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
    allocate( obs%corr(obs%nobs) )
    allocate( obs%qc  (obs%nobs) )
    allocate( obs%img (obs%nobs) )

    obs%elem = 0
    obs%lon  = 0.0_dp
    obs%lat  = 0.0_dp
    obs%lev  = 0.0_dp
    obs%dat  = 0.0_dp
    obs%err  = 0.0_dp
    obs%dmin = 0.0_dp
    obs%hxf  = 0.0_dp
    obs%corr = 0.0_dp
    obs%qc   = 0
    obs%img  = 0

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
    if(allocated(obs%corr)) deallocate(obs%corr)
    if(allocated(obs%hxe)) deallocate(obs%hxe)
    if(allocated(obs%qc)) deallocate(obs%qc)
    if(allocated(obs%img)) deallocate(obs%img)

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
    inquire(file=trim(cfile)//filesuffix,exist=ex)
    if(ex) then
      open(iunit,file=trim(cfile)//filesuffix,form='unformatted',access='sequential')
      do
        read(iunit,iostat=ios) wk
        if(ios/=0) exit
        nn=nn+1
      end do
      close(iunit)
    else
      write(6,'(2a)') trim(cfile)//filesuffix,' does not exist'
    end if
    deallocate(wk)
    return
  end subroutine get_nobs
!
  subroutine read_obs(cfile,obs)
    implicit none
    character(len=*), intent(in) :: cfile
    type(obstype), intent(inout) :: obs
    real(kind=sp) :: wk(6)
    integer :: n, iunit

    iunit=91
    open(iunit,file=trim(cfile)//filesuffix,form='unformatted',access='sequential')
    do n=1,obs%nobs
      read(iunit) wk
      select case(nint(wk(1)))
      case(id_ps_obs)
        wk(5)=wk(5)*100.0 !hPa -> Pa
      case(id_rh_obs)
        wk(4)=wk(4)*100.0 !hPa -> Pa
        wk(5)=wk(5)*0.01  !% -> nondimensional
      case default
        wk(4)=wk(4)*100.0 !hPa -> Pa
      end select
      if(debug) print *, wk
      obs%elem(n) = nint(wk(1))
      obs%lon (n) = real(wk(2),kind=dp)
      obs%lat (n) = real(wk(3),kind=dp)
      obs%lev (n) = real(wk(4),kind=dp)
      obs%dat (n) = real(wk(5),kind=dp)
      obs%dmin(n) = real(wk(6),kind=dp)
    end do
    close(iunit)

    return
  end subroutine read_obs
!
  subroutine write_obs(cfile,obs)
    implicit none
    character(len=*), intent(in) :: cfile
    type(obstype), intent(in) :: obs
    real(kind=sp) :: wk(6)
    integer :: n, iunit

    iunit=92
    open(iunit,file=trim(cfile)//filesuffix,form='unformatted',access='sequential')

    do n=1,obs%nobs
      wk(1)=real(obs%elem(n),kind=sp)
      wk(2)=real(obs%lon (n),kind=sp)
      wk(3)=real(obs%lat (n),kind=sp)
      wk(4)=real(obs%lev (n),kind=sp)
      wk(5)=real(obs%dat (n),kind=sp)
      wk(6)=real(obs%dmin(n),kind=sp)
      select case(nint(wk(1)))
      case(id_ps_obs)
        wk(5)=wk(5)*0.01 !Pa->hPa
      case(id_rh_obs)
        wk(4)=wk(4)*0.01 !Pa->hPa
        wk(5)=wk(5)*100.0 !nondimensional->%
      case default
        wk(4)=wk(4)*0.01 !Pa->hPa
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
    real(kind=sp) :: wk(10)
    integer :: n, iunit

    iunit=91
    open(iunit,file=trim(cfile)//filesuffix,form='unformatted',access='sequential')
    do n=1,obs%nobs
      read(iunit) wk
      select case(nint(wk(1)))
      case(id_t2m_obs)
      case(id_ps_obs)
        !wk(4)=wk(4)*100.0 !hPa -> Pa lev
        wk(5)=wk(5)*100.0 !hPa -> Pa dat
        wk(6)=wk(6)*100.0 !hPa -> Pa err
        wk(8)=wk(8)*100.0 !hPa -> Pa hxf
        wk(9)=wk(9)*100.0 !hPa -> Pa corr
      case(id_rh_obs)
        wk(4)=wk(4)*100.0 !hPa -> Pa lev
        wk(5)=wk(5)*0.01 !% -> nondimensional dat
        wk(6)=wk(6)*0.01 !% -> nondimensional err
        wk(8)=wk(8)*0.01 !% -> nondimensional hxf
        wk(9)=wk(9)*0.01 !% -> nondimensional corr
      case default
        wk(4)=wk(4)*100.0 !hPa -> Pa lev
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
      obs%corr(n) = real(wk(9),kind=dp)
      obs%qc  (n) = nint(wk(10))
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
    real(kind=sp) :: wk(10)
    integer :: n, iunit

    iunit=92
    open(iunit,file=trim(cfile)//filesuffix,form='unformatted',access='sequential')

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
      wk(9)=real(obs%corr(n),kind=sp)
      wk(10)=real(obs%qc  (n),kind=sp)
      select case(nint(wk(1)))
      case(id_t2m_obs)
      case(id_ps_obs)
        !wk(4) = wk(4) * 0.01 !Pa->hPa lev
        wk(5) = wk(5) * 0.01 !Pa->hPa dat
        wk(6) = wk(6) * 0.01 !Pa->hPa err
        wk(8) = wk(8) * 0.01 !Pa->hPa hxf
        wk(9) = wk(9) * 0.01 !Pa->hPa corr
      case(id_rh_obs)
        wk(4) = wk(4) * 0.01 !Pa->hPa lev
        wk(5) = wk(5) * 100.0 !nondimensional->% dat
        wk(6) = wk(6) * 100.0 !nondimensional->% err
        wk(8) = wk(8) * 100.0 !nondimensional->% hxf
        wk(9) = wk(9) * 100.0 !nondimensional->% corr
      case default
        wk(4) = wk(4) * 0.01 !Pa->hPa lev
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
! calculate date before or after several minutes
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
! calculate minutes between two dates
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
