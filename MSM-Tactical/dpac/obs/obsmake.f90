program obsmake
!
! make observations for OSSE
! history:
! 23-02-02 SN create
!
  use kind_module
  use co_module
  use nml_module
  use rsmcom_module
  use corsm_module
  use func_module, only: distll_1
  use obs_module, only : nobstype, elem_id, obstype, &
   &  obsin_allocate, monit_obsin, write_obs, ndate
  use obsope_module, only: obsmake_cal
  use lmlef_tools, only: init_das_lmlef
  implicit none
  character(len=5), dimension(4), parameter :: datatype=&
   & (/'upper','surf ','tovs ','misc '/)
  integer, dimension(4), parameter :: iuse=(/1,1,0,0/)
  character(len=100) :: ofile
  character(len=15) :: odate='yymmddhhnn-hhnn'
  character(len=6) :: cyymmdd
  character(len=4) :: chhnn
  integer :: inn, ihh, idd, imm, iyy
  integer :: ndataall
  integer, dimension(5) :: btime
  integer, dimension(5) :: atime = (/2022,1,1,0,0/)
  integer :: lmin=0, rmin=1
  integer :: ibuf=0 ! zonal buffer region 
  integer :: jbuf=0 ! meridional buffer region
  integer :: kint=1 ! level interval
  real(kind=dp) :: dist_obs_upper=300.0d3 ! distance between 2 upper stations
  real(kind=dp) :: dist_obs_synop=60.0d3 ! distance between 2 synop stations
  logical :: stationin=.false. ! whether locations of observation are given by station_fname or not
  character(filelenmax) :: station_fname(nobsfilemax) = 'sta'
  namelist /param_obsmake/ atime, lmin, rmin, &
          ibuf, jbuf, kint, dist_obs_upper, dist_obs_synop, &
          stationin, station_fname

  type(obstype), allocatable :: obs(:)
  character(len=filelenmax) :: guesf
  real(kind=dp),allocatable :: gues3dc(:,:,:,:)[:]  !control
  real(kind=dp),allocatable :: gues2dc(:,:,:)[:]    !control
  real(kind=dp) :: dist_obs,clon1,clat1,dist1
  integer :: ilon,ilat,ielm,ilev,iof,ios,ij
  integer :: nobsmake,nelm,nlayer,n0,n1
  integer,dimension(nobstype) :: elemiduse
! timer
  real(kind=dp) :: rtimer, rtimer00
! stdout
  character(8) :: stdoutf='NOUT-000'

  call cpu_time(rtimer00)
! initialize
  call initialize_co

  open(5,file='STDIN')
!
  write(stdoutf(6:8), '(I3.3)') myimage
  write(6,'(3A,I3.3)') 'STDOUT goes to ',stdoutf,' for MYIMAGE ', myimage
  open(6,FILE=stdoutf)
  write(6,'(A,I3.3,2A)') 'MYIMAGE=',myimage,', STDOUTF=',stdoutf
!
  call read_nml_ens
  call read_nml_obsope
  call read_nml_lmlef
  call file_member_replace(0,fguess_basename,guesf)
  call set_rsmparm(guesf)
  call set_corsm
  call init_das_lmlef
  rewind(5)
  read (5,nml=param_obsmake)
  write(6,nml=param_obsmake)
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

  allocate(gues3dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*])
  allocate(gues2dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d)[*])
! read reference state
  call read_cntl(guesf,gues3dc,gues2dc)
  sync all

! create observation
!! set file timestamp
  call ndate(atime,lmin,btime)
  iyy = btime(1) - (btime(1)/100)*100
  imm = btime(2)
  idd = btime(3)
  write(cyymmdd,'(3i2.2)') iyy,imm,idd
  write(chhnn,'(2i2.2)') btime(4:5)
  print *, cyymmdd
  print *, chhnn
  write(odate(1:6),'(a6)') cyymmdd
  write(odate(7:10),'(a4)') chhnn
  call ndate(atime,rmin,btime)
  write(chhnn,'(2i2.2)') btime(4:5)
  print *, chhnn
  write(odate(12:15),'(a4)') chhnn
  print *, odate

  iyy = atime(1) - (atime(1)/100)*100
  imm = atime(2)
  idd = atime(3)
  write(cyymmdd,'(3i2.2)') iyy,imm,idd

  allocate( obs(obsin_num) )
  do iof=1,obsin_num
    if(iuse(iof)/=1) cycle
!! prepare stations
    elemiduse(:)=0
    if(trim(datatype(iof))=='upper') then
      dist_obs=dist_obs_upper
      nelm=0
      nlayer=nlev/kint
      do ielm=1,nobstype
        if(luseobs(ielm)) then
          if(elem_id(ielm)<10000) then
            nelm=nelm+1
            elemiduse(nelm)=elem_id(ielm)
          end if
        end if
      end do
    else if(trim(datatype(iof))=='surf') then
      dist_obs=dist_obs_synop
      nelm=0
      nlayer=1
      do ielm=1,nobstype
        if(luseobs(ielm)) then
          if(elem_id(ielm)>10000) then
            nelm=nelm+1
            elemiduse(nelm)=elem_id(ielm)
          end if
        end if
      end do
    else
      print *, 'data type '//trim(datatype(iof))//' is not implemented yet.'
      exit
    end if
    if(nelm==0) cycle
    print *, trim(datatype(iof)),' elem ',elemiduse(1:nelm)
    if(stationin) then
      open(10,file=trim(station_fname(iof)))
      nobsmake=0
      do
        read(10,'(2f6.2)',iostat=ios) clon1,clat1
        if(ios/=0) exit
        nobsmake=nobsmake+1
      end do
      print *, trim(datatype(iof))//' #obs ',nobsmake*nelm*nlayer
      obs(iof)%nobs = nobsmake*nelm*nlayer
      call obsin_allocate(obs(iof))
      !! set stations and elemend ID
      nobsmake=0
      do ielm=1,nelm
        do ilev=1,nlayer
          rewind(10)
          n0=nobsmake
          do
            read(10,'(2f6.2)',iostat=ios) clon1, clat1
            if(ios/=0) exit
            nobsmake=nobsmake+1
            obs(iof)%elem(nobsmake)=elemiduse(ielm)
            obs(iof)%lon(nobsmake)=clon1
            obs(iof)%lat(nobsmake)=clat1
            obs(iof)%lev(nobsmake)=real((ilev-1)*kint+1,kind=dp)
          end do ![read(10)]
!        print *, 'obslon=',obs(iof)%lon(n0+1:nobsmake)
!        print *, 'obslat=',obs(iof)%lat(n0+1:nobsmake)
!        print *, 'obslev=',obs(iof)%lev(n0+1:nobsmake)
        end do ![ilev=1,nlayer]
      end do ![ielm=1,nelm]
      
    else
      nobsmake=0
      do ilat=1+jbuf,nlat-jbuf
        if(nobsmake>0) then
          call distll_1(0.0d0,clat1,0.0d0,rlat(ilat),dist1)
          if(dist1.lt.dist_obs) cycle
        endif
        clat1=rlat(ilat)
        do ilon=1+ibuf,nlon-ibuf
          if(ilon.gt.1+ibuf) then
            call distll_1(clon1,clat1,rlon(ilon),rlat(ilat),dist1)
            if(dist1.lt.dist_obs) cycle
          end if
          nobsmake=nobsmake+1
          clon1=rlon(ilon)
          clat1=rlat(ilat)
        end do
      end do
      print *, trim(datatype(iof))//' #obs ',nobsmake*nelm*nlayer
      obs(iof)%nobs = nobsmake*nelm*nlayer
      call obsin_allocate(obs(iof))

      !! set stations and elemend ID
      nobsmake=0
      do ielm=1,nelm
        do ilev=1,nlayer
          n0=nobsmake
          do ilat=1+jbuf,nlat-jbuf
            if(nobsmake.gt.n0) then
              call distll_1(0.0d0,clat1,0.0d0,rlat(ilat),dist1)
              if(dist1.lt.dist_obs) cycle
            endif
            clat1=rlat(ilat)
            do ilon=1+ibuf,nlon-ibuf
              if(ilon.gt.1+ibuf) then
                call distll_1(clon1,clat1,rlon(ilon),rlat(ilat),dist1)
                if(dist1.lt.dist_obs) cycle
              end if
              nobsmake=nobsmake+1
              clon1=rlon(ilon)
              clat1=rlat(ilat)
              obs(iof)%elem(nobsmake)=elemiduse(ielm)
              obs(iof)%lon(nobsmake)=clon1
              obs(iof)%lat(nobsmake)=clat1
              obs(iof)%lev(nobsmake)=real((ilev-1)*kint+1,kind=dp)
            end do ![ilon]
          end do ![ilat]
!        print *, 'obslon=',obs(iof)%lon(n0+1:nobsmake)
!        print *, 'obslat=',obs(iof)%lat(n0+1:nobsmake)
!        print *, 'obslev=',obs(iof)%lev(n0+1:nobsmake)
        end do ![ilev=1,nlayer]
      end do ![ielm=1,nelm]
    end if
  end do ![iof=1,obsin_num]
  sync all
!! simulate observation
  call obsmake_cal(obs,gues3dc,gues2dc)
!! monitor and write output
  if(myimage.eq.1) then
  do iof=1,obsin_num
    call monit_obsin(obs(iof)%nobs,obs(iof)%elem,obs(iof)%dat)
    ofile=trim(datatype(iof))//'.siml.'//odate
    print *, ofile
    call write_obs(ofile,obs(iof))
  end do
  end if
  sync all
  call cpu_time(rtimer)
  write(6,'(A,2F10.2)') '### TIMER(MONITOR):',rtimer,rtimer-rtimer00
  rtimer00=rtimer

! finalize
  call clean_rsmparm

end program obsmake
