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
    integer                    :: elem
    real(kind=dp),dimension(3) :: date !hour,min,sec
    real(kind=dp)              :: lon
    real(kind=dp)              :: lat
    real(kind=dp)              :: lev  !pressure[hPa]
    real(kind=dp)              :: y
    real(kind=dp)              :: err
  end type obstype 
!
! parameters and variables for decoding dcd
!
  integer,parameter :: irec_time = 0
  integer,parameter :: irec_info = 10
  integer,parameter :: irec_data = 120
  integer,parameter :: irec_undef = 32767
  integer,save      :: nrec_time, nrec_info, nrec_data
  integer,parameter :: nrec1 = 14
  integer           :: nrecall,nrec2,nrec3,nrec4,nrec5
!
! observation ID
!
  ! upper
  integer,parameter,public :: id_t_obs=130   !Temperature[K]
  integer,parameter,public :: id_td_obs=3017 !Dew point temperature[K]
  integer,parameter,public :: id_ws_obs=10   !Wind speed[m/s]
  integer,parameter,public :: id_wd_obs=3031 !Wind direction[degree]
! 
! debug
!
  logical, parameter :: debug=.true.

  public :: obstype, get_nobs, read_upper 
contains
  subroutine get_nobs(inf,cfile,nobs)
    implicit none
    integer, intent(in) :: inf
    character(len=*), intent(in) :: cfile
    integer, intent(out) :: nobs
    integer :: ioffset
    integer :: idrec
    integer :: ntype
    integer,dimension(:),allocatable :: did,ndata,nobseach
    integer :: iyy, imm, idd, ihh, inn, iwk
    integer(2) :: ibuf2
    integer(2) :: iymdhnw(6)
    integer :: i,irec

    ! open file
    open(inf,file=cfile,status='old',form='unformatted',&
         convert='big_endian',access='direct',recl=2)
    ! get date
    ioffset=0
    call read_part1(inf,ioffset,idrec)
    if(debug) then
      print *, nrecall
      print *, nrec1,nrec2,nrec3,nrec4,nrec5
      print *, idrec
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
    call read_part1(inf,ioffset,idrec)
    if(debug) then
      print *, nrecall
      print *, nrec1,nrec2,nrec3,nrec4,nrec5
      print *, idrec
    end if
    nrec_info = nrecall
    irec = nrec_time+nrec1+nrec2+nrec3+1
    ntype = int(nrec4/3)
    allocate( did(ntype),ndata(ntype),nobseach(ntype) )
    nobs=0
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
      if(did(i).ge.3000.and.did(i).lt.4000) then
        nobs = nobs + nobseach(i)
      end if
    end do
    print *, ntype
    do i=1,ntype
      print *, did(i), ndata(i), nobseach(i)
    end do
    !
    return
  end subroutine get_nobs

  subroutine read_upper(inf,nobs,obs)
    implicit none
    integer, intent(in) :: inf
    integer, intent(in) :: nobs
    type(obstype), intent(out) :: obs(nobs)

    return
  end subroutine read_upper

  subroutine read_part1(inf,ioffset,idrec)
    implicit none
    integer, intent(in) :: inf
    integer, intent(in) :: ioffset
    integer, intent(out) :: idrec
    integer :: idata(7)
    integer(2) :: ibuf2(nrec1)
    integer :: i, irec

    irec=1+ioffset
    do i=1,nrec1
      read(inf,rec=irec) ibuf2(i)
      irec=irec+1
      if(debug) print *, i, int(ibuf2(i),kind=4)
    end do
    nrecall=int(ibuf2(1),kind=4)
    nrec2=int(ibuf2(3),kind=4);nrec3=int(ibuf2(4),kind=4)
    nrec4=int(ibuf2(5),kind=4);nrec5=int(ibuf2(6),kind=4)
    idrec=int(ibuf2(7),kind=4)
    idata=int(ibuf2(8:),kind=4)
    return
  end subroutine read_part1
end module obs_module
