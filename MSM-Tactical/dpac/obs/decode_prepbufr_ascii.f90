program decode_prepbufr_ascii
!
! decode preprocessed PREPBUFR ASCII file 
! preprocess code : https://github.com/NCAR/rda-prepbufr-decode.git
!
! Observation types
! ADPUPA:  Upper-Air (RAOB, PIBAL, RECCO, DROPS) Reports.
! AIRCAR:  ACARS Aircraft Reports.
! AIRCFT:  Conventional (AIREP, PIREP) and ASDAR Aircraft Reports.
! SATWND:  Satellite-derived Wind Reports.
! PROFLR:  Wind Profiler Reports.
! VADWND:  VAD (NEXRAD) Wind Reports.
! SATBOG:  Satellite Moisture Bogus Reports.
! SATEMP:  TOVS Satellite Data (Soundings, Retrievals, Radiances).
! ADPSFC:  Surface Land (Synoptic, Metar) Reports.
! SFCSHP:  Surface Marine (Ship, Buoy, C-man, Platform) Reports.
! SFCBOG:  Mean Sea-Level Pressure Bogus Reports.
! SPSSMI:  SSM/I Retrieval Products (Reprocessed Wind Speed, TPW).
! SYNDAT:  Synthetic Tropical Cyclone Bogus Reports.
! ERS1DA:  ERS Scatterometer Data (Reprocessed Wind Speed).
! GOESND:  Quikscat Scatterometer Data (Reprocessed Wind Speed).
!
  use kind_module
  use obs_module, only : obstype, &
     & id_t_obs, id_q_obs, id_u_obs, id_v_obs, id_ps_obs, &
     & obsin_allocate, obsin_deallocate, obs_preproc, monit_obsin, &
     & write_obs, ndate, nhour
  implicit none

! file columns
  character(len=8) :: sid ! station ID
  real(kind=dp) :: xob ! lon[degE]
  real(kind=dp) :: yob ! lat[degN]
  real(kind=dp) :: dhr ! obs time - cycle time [hours]
  real(kind=dp) :: elv ! station elevation [m]
  real(kind=dp) :: typ ! report type
  real(kind=dp) :: t29 ! input report type
  real(kind=dp) :: itp ! instrument type
  integer :: ilev ! observation level index
  character(len=5) :: cvar ! observation variable
  real(kind=dp) :: ob  ! observation value
  real(kind=dp) :: qm  ! quality marker
  real(kind=dp) :: pc  ! program code
  real(kind=dp) :: rc  ! reason code
  real(kind=dp) :: fc  ! forecast value
  real(kind=dp) :: an  ! analyzed value
  real(kind=dp) :: oe  ! observation error
  real(kind=dp) :: cat ! PREPBUFR level category
  character(len=150) :: cdummy
  character(len=1) :: cint(16)
  character(len=76), parameter :: infmt=&
    '(a8,a1,2f7.2,a1,f8.1,a1,f7.3,a1,f8.1,a1,f7.1,a1,f6.1,a1,i4,a1,a5,8(a1,f8.1))'

! common variables to subroutines
  real(kind=dp), parameter :: R8BFMS = 10.0e10 ! "Missing" value for BUFR data
  integer, parameter       :: NHR8PM = 8       ! Actual number of BUFR parameters in header
  integer, parameter       :: MXR8PM = 10      ! Maximum number of BUFR parameters
  integer, parameter       :: MXR8LV = 400     ! Maximum number of BUFR levels
  integer, parameter       :: MXR8VN = 10      ! Maximum number of BUFR event sequences
  integer, parameter       :: MXR8VT = 6       ! Maximum number of BUFR variable types
  integer, parameter       :: MXSTRL = 80      ! Maximum size of a string

  integer, parameter :: NFILO = 15 
  integer, parameter :: iunso ( NFILO ) = &
    (/   51,   52,   53,   54,   55, &
         56,   57,   58,   59,   60, &
         61,   62,   63,   64,   65  /)

  character(len=6), parameter :: filo ( NFILO ) = &
     (/ 'ADPUPA', 'AIRCAR', 'AIRCFT', 'SATWND', 'PROFLR', &
        'VADWND', 'SATBOG', 'SATEMP', 'ADPSFC', 'SFCSHP', &
        'SFCBOG', 'SPSSMI', 'SYNDAT', 'ERS1DA', 'GOESND'  /)

  character(len=1), parameter ::  var ( MXR8VT ) = &
     (/'P','Q','T','Z','U','V'/)

  integer :: io,n
  logical :: ex

  integer :: tmpelm,tmpdmin
  real(kind=dp) :: tmplon, tmplat, tmplev, tmpdat, tmpdhr
        
  integer :: i,ii,j,jj,lv,kk,mm

  character(len=300) :: infhead='prepbufr' 
  character(len=300) :: ofile( NFILO )
  character(len=15) :: odate='yymmddhhnn-hhnn'
  character(len=6) :: cyymmdd
  character(len=4) :: chhnn
  integer :: inn, ihh, idd, imm, iyy
  integer :: ndataall
  type(obstype) :: obs
  integer, dimension(5) :: ltime, rtime, otime
  integer, dimension(5) :: ctime = (/2022,1,1,0,0/) !input prepbufr time
  integer, dimension(5) :: atime = (/2022,1,1,0,0/) !analysis time
  integer :: lmin=0, rmin=1
  logical :: lpreproc=.true.
  integer :: iwnd=1,iq=1
  character(len=300) :: inf, outf
  namelist /param_decode/ infhead, &
          ctime, atime, lmin, rmin, &
          lpreproc, iwnd, iq

  read(5,nml=param_decode)
  write(6,nml=param_decode)
  call ndate(atime,lmin,ltime)
  print *, 'Left: ',ltime
  call ndate(atime,rmin,rtime)
  print *, 'Right: ',rtime
  iyy = ltime(1) - (ltime(1)/100)*100
  imm = ltime(2)
  idd = ltime(3)
  write(cyymmdd,'(3i2.2)') iyy,imm,idd
  write(chhnn,'(2i2.2)') ltime(4:5)
  write(odate(1:6),'(a6)') cyymmdd
  write(odate(7:10),'(a4)') chhnn
  write(chhnn,'(2i2.2)') rtime(4:5)
  write(odate(12:15),'(a4)') chhnn
  print *, odate

! start processing files
  do n=1,NFILO
    inf=trim(infhead)//'.'//filo(n)
    print *, trim(inf)
    inquire(file=trim(inf),exist=ex)
    if(.not.ex) cycle
    outf=''
    outf(1:22)=filo(n)//'.'//odate
    if(lpreproc.and.(filo(n)=='ADPUPA')) then
      if(iq.eq.2) then
        outf(1:29)=filo(n)//'_preprh.'//odate
      else
        outf(1:27)=filo(n)//'_prep.'//odate
      end if
    end if
    print *, trim(outf)
    ! get #obs
    ndataall=0
    open(11,file=trim(inf))
    !! header
    read(11,'(a)',iostat=io) cdummy
    read(11,'(a)',iostat=io) cdummy
    read(11,'(a)',iostat=io) cdummy
    do
      read(11,'(a)',iostat=io) cdummy
      if(io/=0) exit
      ndataall=ndataall+1
    end do
    close(11)

    print *, filo(n),' total(input) ',ndataall
    obs%nobs = ndataall
    call obsin_allocate( obs )

    open(11,file=trim(inf))
    !! header
    read(11,'(a)',iostat=io) cdummy
    read(11,'(a)',iostat=io) cdummy
    read(11,'(a)',iostat=io) cdummy
    ndataall=0
    do
      read(11,infmt,iostat=io) &
        sid,cint(1),xob,yob,cint(2),elv,cint(3),dhr,&
        cint(4),typ,cint(5),t29,cint(6),itp,&
        cint(7),ilev,cint(8),cvar,cint(9),ob,&
        cint(10),qm,cint(11),pc,cint(12),rc,&
        cint(13),fc,cint(14),an,cint(15),oe,cint(16),cat
      if(io/=0) exit
!      if(cvar=='    Q') then
!      write(6,infmt) &
!        sid,cint(1),xob,yob,cint(2),elv,cint(3),dhr,&
!        cint(4),typ,cint(5),t29,cint(6),itp,&
!        cint(7),ilev,cint(8),cvar,cint(9),ob,&
!        cint(10),qm,cint(11),pc,cint(12),rc,&
!        cint(13),fc,cint(14),an,cint(15),oe,cint(16),cat
!      end if
      !--->missing values
      !if(ob.ge.1e10) cycle
      if(ob.eq.0.0.and.qm.eq.0.0.and.pc.eq.0.0) cycle
      !<---missing values
      tmplon=xob
      tmplat=yob
      !print *, tmplon, tmplat, dhr
      !--->check time window
      tmpdmin=nint(dhr*60)
      call ndate(ctime,tmpdmin,otime)
      !print *, tmpdmin, ' observation time : ',otime
      call nhour(otime,atime,tmpdmin)
      if((tmpdmin.lt.lmin).or.(tmpdmin.gt.rmin)) then
!        print *, 'out of assimilation window : ',&
!                tmpdmin, lmin, rmin
        cycle
      end if
      !<---check time window
      !--->quality marker
      if(qm.gt.3.0) cycle
      !<---quality marker
      !--->program code
      if(pc.gt.1.0) cycle
      !<---program code
      IF ((filo(n) .eq. 'ADPSFC').or.&
          (filo(n) .eq. 'SFCSHP')) THEN
        !==SURF==
        if(cvar.ne.'    P') cycle
        ndataall=ndataall+1
        tmpelm=id_ps_obs
        tmplev=elv !elevation
        tmpdat=ob*100.0 !mb->Pa
        obs%elem(ndataall)=tmpelm
        obs%lon (ndataall)=tmplon
        obs%lat (ndataall)=tmplat
        obs%lev (ndataall)=tmplev
        obs%dat (ndataall)=tmpdat
        obs%dmin(ndataall)=real(tmpdmin,kind=dp)
      ELSE !IF (filo(n) .eq. 'ADPUPA')  THEN
        !==UPPER==
        if(cvar.eq.'    P') then
          tmplev=ob*100.0 !mb->Pa
        elseif(cvar.eq.'    Z') then
          cycle
        else
          select case(cvar)
          case('    T')
            tmpelm=id_t_obs
            tmpdat=ob+273.15 !degC->degK
          case('    Q')
            tmpelm=id_q_obs
            tmpdat=ob*1.0e-6 !mg/kg->kg/kg
            if(pc.gt.1.0) cycle !duplicate
          case('    U')
            tmpelm=id_u_obs
            tmpdat=ob
          case('    V')
            tmpelm=id_v_obs
            tmpdat=ob
          end select
          ndataall=ndataall+1
          obs%elem(ndataall)=tmpelm
          obs%lon (ndataall)=tmplon
          obs%lat (ndataall)=tmplat
          obs%lev (ndataall)=tmplev
          obs%dat (ndataall)=tmpdat
          obs%dmin(ndataall)=real(tmpdmin,kind=dp)
        endif
        !==UPPER==
      END IF
    end do
    close(11)
    !
    ! output
    !
    obs%nobs=ndataall
    print *, filo(n),' total(use)  ',obs%nobs
    if (obs%nobs==0) cycle
    call monit_obsin(obs%nobs,obs%elem,obs%dat)

    if(lpreproc.and.(filo(n) .eq. 'ADPUPA')) then
      call obs_preproc(obs,iwnd,iq)
      call monit_obsin(obs%nobs,obs%elem,obs%dat)
    end if
    call write_obs(outf,obs)
    call obsin_deallocate(obs)
  end do
end program decode_prepbufr_ascii
