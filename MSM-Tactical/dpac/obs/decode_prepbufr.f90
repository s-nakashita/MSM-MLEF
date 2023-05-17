program decode_prepbufr
!
! decode PREPBUFR
! original : https://github.com/NCAR/rda-prepbufr-decode.git
! [NOTE] require BUFRLIB(https://github.com/JCSDA-internal/bufrlib.git)
!
  use kind_module
  use obs_module, only : obstype, &
     & id_t_obs, id_q_obs, id_u_obs, id_v_obs, id_ps_obs, &
     & obsin_allocate, obsin_deallocate, obs_preproc, monit_obsin, &
     & write_obs, ndate, nhour
  implicit none

! common variables to subroutines
  real(kind=dp), parameter :: R8BFMS = 10.0e10 ! "Missing" value for BUFR data
  integer, parameter       :: NHR8PM = 8       ! Actual number of BUFR parameters in header
  integer, parameter       :: MXR8PM = 10      ! Maximum number of BUFR parameters
  integer, parameter       :: MXR8LV = 400     ! Maximum number of BUFR levels
  integer, parameter       :: MXR8VN = 10      ! Maximum number of BUFR event sequences
  integer, parameter       :: MXR8VT = 6       ! Maximum number of BUFR variable types
  integer, parameter       :: MXSTRL = 80      ! Maximum size of a string

  real(kind=dp) :: hdr ( NHR8PM )
  real(kind=dp) :: evns ( MXR8PM, MXR8LV, MXR8VN, MXR8VT )
  integer       :: nlev
!      COMMON / PREPBC / hdr, evns, nlev

  integer, parameter :: STRLN = 180
  integer, parameter :: maxtype=15
  integer, parameter :: maxplat=15
  integer, parameter :: maxparm=5
  integer, parameter :: maxsaid=15

  character :: outstg*(150), subset*8, &
             crec*101, type(maxtype)*6, &
             parm(maxparm),id*4,idatec*10, &
             said(maxsaid)*5,sid*5,vtmp*5 
     
  logical :: skip_vtmp
 
  integer :: plat(maxplat)
      
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

  logical :: found 
  integer :: nout
  integer :: io,stat,n,inlength,np,nt,nplat, &
             count,k,flag,pflag,p,ns,s,platflag
  real(kind=dp) :: lat1,lat2,lon1,lon2,platform

  integer :: tmpelm,tmpdmin
  real(kind=dp) :: tmplon, tmplat, tmplev, tmpdat, tmpdhr
        
  integer :: tv_ev_idx, tvflag
  integer :: idate, ierrpb, iuno
  integer :: i,ii,j,jj,lv,kk,mm

  character(len=300) :: ofile( NFILO )
  character(len=15) :: odate='yymmddhhnn-hhnn'
  character(len=6) :: cyymmdd
  character(len=4) :: chhnn
  integer :: inn, ihh, idd, imm, iyy
  integer, allocatable :: ndataall(:)
  type(obstype),allocatable :: obs(:)
  integer, allocatable :: iof(:)
  integer, dimension(5) :: ltime, rtime, otime
  integer, dimension(5) :: ctime = (/2022,1,1,0,0/) !input prepbufr time
  integer, dimension(5) :: atime = (/2022,1,1,0,0/) !analysis time
  integer :: lmin=0, rmin=1
  logical :: lpreproc=.true.
  integer :: iwnd=1,iq=1
  character(len=300) :: inf, config, outf
  namelist /param_decode/ ctime, atime, lmin, rmin, &
          lpreproc, iwnd, iq, &
          inf, config

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

!
  np=0
  nt=0
  nplat=0
  ns=0
  lon1=0.0
  lon2=360.0
  lat1=90.0
  lat2=-90.0
      
! Omit virtual temperature in output (default)
  skip_vtmp = .false.

  print*, "infile = ",trim(inf)
  print*, "config file = ",trim(config)

! open the configuration file
  open (unit=10, file=config, form='formatted')
  do i = 1,10
    read(10, '(A100)', iostat=io) crec
    if (io < 0) exit
    inlength=len_trim(crec)-4
    select case (crec(1:4))
      case ("LATS")
        read (crec,*) id,lat1,lat2
        print *,"Latitude Values: ",lat1,lat2
      case ("LONS")
        read (crec,*) id,lon1,lon2
        print *,"Longitude Values: ",lon1,lon2
      case ("SAID")
        ns=inlength/6
        read (crec,*) id,(said(j), j=1,ns)
        print *,"Stations: ",(said(j)," ", j=1,ns)
      case ("PARM")
        np=inlength/2
        read (crec,*) id,(parm(j), j=1,np)
        print *,"Parameters: ",(parm(j)," ", j=1,np)
      case ("TYPE")
        nt=inlength/7
        read (crec,*) id,(type(j), j=1,nt)
        print *,"Report Types: ",(type(j)," ", j=1,nt)
      case ("PLAT")
        nplat=inlength/4
        read (crec,*) id,(plat(j), j=1,nplat)
        print *,"Reporting platforms: ",(plat(j)," ", j=1,nplat)
      case ("VTMP")
        read (crec,*) id,vtmp
        if (vtmp .eq. "FALSE") then
           print *,"*** Skipping virtual temperature ***"
           skip_vtmp = .true.
        else
           print *,"*** Retaining virtual temperature ***"
           skip_vtmp = .false.
        end if
    end select
  end do

!-----7---------------------------------------------------------------72
!    Set the output file(s)
!-----7---------------------------------------------------------------72
  do ii=1,NFILO
    ofile(ii)=''
  end do
  allocate( iof(NFILO) )
  iof(:)=0
  if(nt .gt. 0) then
    nout=nt
    allocate( obs(nt) )
    allocate( ndataall(nt) )
    ndataall(:)=0
    do kk = 1, nt
    obs(kk)%nobs=1e7
    call obsin_allocate( obs(kk) )
    outf=''
    do ii = 1, NFILO
      if (type(kk) .eq. filo(ii)) then
        iof(ii)=kk
        outf(1:22)=filo(ii)//'.'//odate
        if(lpreproc.and.(filo(ii)=='ADPUPA')) then
          if(iq.eq.2) then
            outf(1:29)=filo(ii)//'_preprh.'//odate
          else
            outf(1:27)=filo(ii)//'_prep.'//odate
          end if
        end if
        ofile(kk)=outf
        print *, trim(ofile(kk))
        !open (unit=iunso(ii), file=trim(outf) // '.' // filo(ii))
        !    WRITE (UNIT=iunso(ii), FMT=15)
        !    WRITE (UNIT=iunso(ii), FMT=20) &
        !    'SID','XOB','YOB','ELV','DHR','TYP','T29','ITP', &
        !    'lev','var','OB','QM', 'PC', 'RC', 'FC','AN','OE','CAT'
        !    WRITE (UNIT=iunso(ii), FMT=15)
        exit
      end if
    end do
    end do
  else
    nout=NFILO
    allocate( obs(NFILO) )
    allocate( ndataall(NFILO) )
    ndataall(:)=0
    do ii = 1, NFILO
      iof(ii)=ii
      obs(ii)%nobs=1e6
      call obsin_allocate( obs(ii) )
      outf=''
      outf(1:22)=filo(ii)//'.'//odate
      if(lpreproc.and.(filo(ii)=='ADPUPA')) then
        if(iq.eq.2) then
          outf(1:27)=filo(ii)//'_preprh.'//odate
        else
          outf(1:25)=filo(ii)//'_prep.'//odate
        end if
      end if
      ofile(ii)=outf
      !open (unit=iunso(ii), file=trim(outf) // '.' // filo(ii))
      !      WRITE (UNIT=iunso(ii), FMT=15)
      !      WRITE (UNIT=iunso(ii), FMT=20) &
      !      'SID','XOB','YOB','ELV','DHR','TYP','T29','ITP', &
      !      'lev','var','OB','QM', 'PC', 'RC', 'FC','AN','OE','CAT'
      !      WRITE (UNIT=iunso(ii), FMT=15)
    end do  
  end if
!  15  FORMAT ("#", 148("-"))
!  20  FORMAT ("#",a4,a11,a7,a9,a8,a9,a8,a7,a5,a6,8a9)
  
!-----7---------------------------------------------------------------72
!    Open the PREPBUFR input file
!-----7---------------------------------------------------------------72
      OPEN ( UNIT = 11, FILE = inf, FORM = 'UNFORMATTED' )
      CALL OPENBF  ( 11, 'IN', 11 )
      CALL DATELEN  ( 10 )

!
! Start reading input file
!
  readcycle : do
!-----7---------------------------------------------------------------72
!   Get the next station report from the input file.
!-----7---------------------------------------------------------------72
      evns=0.0
      CALL READPB  ( 11, subset, idate, ierrpb )
      IF ( ierrpb .eq. -1 )  THEN
        WRITE(6,*) 'All subsets read in and processed. Exiting.'
        exit readcycle
      END IF

!-----7---------------------------------------------------------------72
!    PREPBUFR data type subsetting filter
!-----7---------------------------------------------------------------72
      if (nt .gt. 0) then
        k = 1
        found = .false.
        do while ((.not. found) .and. (k .le. nt))
          if(subset(1:6) .eq. type(k)) then
            found = .true.
          else
            k = k + 1
          end if
        end do
        if ((.not. found) .and. (ierrpb .eq. 0)) then
          cycle
        end if
!       End program if no subset match found and we are processing
!       the last report in the prepbufr file (ierrpb = 1)
        if ((.not. found) .and. (ierrpb .eq. 1)) then
          WRITE(6,*) &
          'All subsets read in and processed. End of program.'
          exit readcycle
        end if
      end if

!-----7---------------------------------------------------------------72
!    Reporting platform (input report type) subsetting filter
!-----7---------------------------------------------------------------72
      if (nplat .gt. 0) then
        k = 1
        found = .false.
        do while ((.not. found) .and. (k .le. nplat))
          if(hdr(7) .eq. plat(k)) then
            found = .true.
          else
            k = k + 1
          end if
        end do
        if ((.not. found) .and. (ierrpb .eq. 0)) then
          cycle
        end if
!       End program if no subset match found and we are processing
!       the last report in the prepbufr file (ierrpb = 1)
        if ((.not. found) .and. (ierrpb .eq. 1)) then
          WRITE(6,*) &
          'All subsets read in and processed. End of program.'
          exit readcycle
        end if
      end if

!-----7---------------------------------------------------------------72
!     Station ID subsetting filter
!-----7---------------------------------------------------------------72
      if (ns .gt. 0) then
        k = 1
        found = .false.       
        write(unit=sid,fmt='(a5)') hdr(1)
        do while ((.not. found) .and. (k .le. ns))
          if(sid .eq. said(k)) then
            found = .true.
          else
            k = k + 1
          end if
        end do
        if ((.not. found) .and. (ierrpb .eq. 0)) then
          cycle
        end if
!       End program if no subset match found and we are processing
!       the last report in the prepbufr file (ierrpb = 1)
        if ((.not. found) .and. (ierrpb .eq. 1)) then
          WRITE(6,*) &
          'All subsets read in and processed. End of program.'
          exit readcycle
        end if
      end if
 
!-----7---------------------------------------------------------------72
!     Longitude/latitude subsetting filters.  This is ignored if
!     station ID subsetting is selected.
!-----7---------------------------------------------------------------72
      if (ns .eq. 0) then
        found = .false.
!      Case lon1 < lon2
        if(lon1 .lt. lon2) then 
          if ((hdr(2) .ge. lon1) .and. (hdr(2) .le. lon2)) then 
            if ((hdr(3) .le. lat1) .and. (hdr(3) .ge. lat2)) then
              found = .true.
            end if
          end if
        else
!      Case lon1 > lon2
          if ((hdr(2) .ge. lon1) .or. (hdr(2) .le. lon2)) then
            if ((hdr(3) .le. lat1) .and. (hdr(3) .ge. lat2)) then
              found = .true.
            end if
          end if
        end if
        if ((.not. found) .and. (ierrpb .eq. 0)) then
          cycle
        end if
!       End program if no subset match found and we are processing
!       the last report in the prepbufr file (ierrpb = 1)
        if ((.not. found) .and. (ierrpb .eq. 1)) then
          WRITE(6,*) &
          'All subsets read in and processed. End of program.'
          exit readcycle
        end if
      end if
      
!-----7---------------------------------------------------------------72
! Set the appropriate output file.
!-----7---------------------------------------------------------------72
  ii = 1
  found = .false.
  DO WHILE ((.not. found) .and. (ii .le. NFILO))
    IF (subset (1:6) .eq. filo (ii))  THEN
  found = .true.
    ELSE 
  ii = ii + 1
    END IF
  END DO
  IF ((.not. found) .and. (ierrpb .eq. 0)) THEN
    cycle
  END IF
!-----7---------------------------------------------------------------72
!     Loop through the event data array EVNS
!-----7---------------------------------------------------------------72
!debug  print *, nlev
!debug  print *, maxval(evns(1,:,:,:)), minval(evns(1,:,:,:))
  DO lv = 1, nlev
    DO kk = 1, MXR8VT

!-----7---------------------------------------------------------------72
!     Parameter subsetting filter (P, Q, T, Z, U, V)
!-----7---------------------------------------------------------------72
        if(np .gt. 0) then
          p = 1
          found = .false.
          do while ((.not. found) .and. (p .le. np))
            if(var(kk) .eq. parm(p)) then
              found = .true.
            else
              p = p + 1
            end if
          end do
          if (.not. found) then
            cycle
          end if
        end if

!-----7---------------------------------------------------------------72
!   Check for virtual temperature
!-----7---------------------------------------------------------------72
        tvflag=1
        if ((var(kk) .eq. 'T') .and. (skip_vtmp)) then
          call virtmp(lv, kk, tv_ev_idx, tvflag)
          if (tvflag .eq. -1) then
            cycle
          end if
        end if
                
!-----7---------------------------------------------------------------72                
!    Write the header and EVNS data for this station report to the
!    output file.
!-----7---------------------------------------------------------------72

        DO jj = 1, MXR8VN
!        Skip virtual temperature at tv_ev_idx
          if ((var(kk) .eq. 'T') .and. (jj .le. tv_ev_idx)) then
            cycle
          endif
          !--->missing values
          !if(evns(1,lv,jj,kk).ge.1e10) cycle
          !<---missing values
          tmplon=hdr(2)
          tmplat=hdr(3)
          tmpdhr=hdr(5) !obstime - cycletime in hours
          !print *, tmplon, tmplat, tmpdhr
          !--->check time window
          tmpdmin=nint(tmpdhr*60)
          call ndate(ctime,tmpdmin,otime)
          !print *, tmpdmin, ' observation time : ',otime
          call nhour(otime,atime,tmpdmin)
          if((tmpdmin.lt.lmin).or.(tmpdmin.gt.rmin)) then
          !  print *, 'out of assimilation window : ',&
          !          tmpdmin, lmin, rmin
            cycle
          end if
          !<---check time window
          !--->quality marker
          if(evns(2,lv,jj,kk).gt.3.0) cycle
          !<---quality marker
          IF (subset (1:6) .eq. 'ADPUPA')  THEN
            !==UPPER==
            if(var(kk).eq.'P') then
              tmplev=evns(1,lv,jj,kk)*100.0 !mb->Pa
            elseif(var(kk).eq.'Z') then
              cycle
            else
              select case(var(kk))
              case('T')
                tmpelm=id_t_obs
                tmpdat=evns(1,lv,jj,kk)+273.15 !degC->degK
              case('Q')
                tmpelm=id_q_obs
                tmpdat=evns(1,lv,jj,kk)*1.0e-6 !mg/kg->kg/kg
                if(evns(3,lv,jj,kk).ne.1.0) cycle !duplicate
              case('U')
                tmpelm=id_u_obs
                tmpdat=evns(1,lv,jj,kk)
              case('V')
                tmpelm=id_v_obs
                tmpdat=evns(1,lv,jj,kk)
              end select
              ndataall(iof(ii))=ndataall(iof(ii))+1
              obs(iof(ii))%elem(ndataall)=tmpelm
              obs(iof(ii))%lon (ndataall)=tmplon
              obs(iof(ii))%lat (ndataall)=tmplat
              obs(iof(ii))%lev (ndataall)=tmplev
              obs(iof(ii))%dat (ndataall)=tmpdat
              obs(iof(ii))%dmin(ndataall)=real(tmpdmin,kind=dp)
            endif
            !==UPPER==
          ELSE IF ((subset (1:6) .eq. 'ADPSFC').or.&
                   (subset (1:6) .eq. 'SFCSHP')) THEN
            !==SURF==
            if(var(kk).ne.'P') cycle
            ndataall(iof(ii))=ndataall(iof(ii))+1
            tmpelm=id_ps_obs
            tmplev=hdr(4) !elevation
            tmpdat=evns(1,lv,jj,kk)*100.0 !mb->Pa
            obs(iof(ii))%elem(ndataall)=tmpelm
            obs(iof(ii))%lon (ndataall)=tmplon
            obs(iof(ii))%lat (ndataall)=tmplat
            obs(iof(ii))%lev (ndataall)=tmplev
            obs(iof(ii))%dat (ndataall)=tmpdat
            obs(iof(ii))%dmin(ndataall)=real(tmpdmin,kind=dp)
          END IF
        END DO  ! End jj = 1, MXR8VN loop
      END DO  ! End kk = 1, MXR8VT loop
      END DO  ! End lv = 1, nlev loop

!-----7---------------------------------------------------------------72
      IF  ( ierrpb .eq. 0 )  THEN
        cycle
      END IF
   end do readcycle
!
! output
!
   do ii=1,nout
      outf=ofile(ii)
      print *, trim(outf)
      obs(ii)%nobs=ndataall(ii)
      print *, 'total(use)  ',obs(ii)%nobs
      if (obs(ii)%nobs==0) cycle
      call monit_obsin(obs(ii)%nobs,obs(ii)%elem,obs(ii)%dat)

      if(lpreproc.and.(type(ii) .eq. 'ADPUPA')) then
        call obs_preproc(obs(ii),iwnd,iq)
        call monit_obsin(obs(ii)%nobs,obs(ii)%elem,obs(ii)%dat)
      end if
      call write_obs(outf,obs(ii))
      call obsin_deallocate(obs(ii))
    end do
   STOP
contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        SUBROUTINE READPB  ( lunit, subset, idate, iret )
!
!      This subroutine will read and combine the mass and wind subsets
!      of the next station report in the prepbufr file.  It is styled
!      after entry point READNS, and it only requires the prepbufr file
!      to be opened for reading with OPENBF.  The combined station
!      report is returned to the caller in COMMON /PREPBC/.
!      This common area contains the number of levels in the report,
!      a one dimensional array with the header information, and a four
!      dimensional array containing all events from the variables POB,
!      QOB, TOB, ZOB, UOB, and VOB for the report.
!
!      The header array contains the following list of mnemonics:
!
!         HDR(1)  Station identification (SID)
!         HDR(2)  Longitude (XOB)
!         HDR(3)  Latitude  (YOB)
!         HDR(4)  Elevation (ELV)
!         HDR(5)  Observation time minus cycle time (DHR)
!         HDR(6)  PREPBUFR report type (TYP)
!         HDR(7) Input report type (T29)
!         HDR(8) Instrument type (ITP)
!
!      The 4-D array of data, EVNS ( ii, lv, jj, kk ), is indexed
!      as follows:
!
!      "ii" indexes the event data types; these consist of:
!          1) OBservation        (e.g., POB, ZOB, UOB, VOB, TOB, QOB, PWO)
!          2) Quality Mark       (e.g., PQM, ZRM, WQM, TQM, QQM, PWQ)
!          3) Program Code       (e.g., PPC, ZPC, WPC, TPC, QPC, PWP)
!          4) Reason Code        (e.g., PRC, ZRC, WRC, TRC, QRC, PWR)
!          5) ForeCast value     (e.g., PFC, ZFC, UFC, VFC, TFC, QFC, PWF)
!          6) ANalysed value     (e.g., PAN, ZAN, UAN, VAN, TAN, QAN, PWA)
!          7) Observation Error  (e.g., POE, ZOE, WOE, TOE, QOE, PWO)
!          8) PREPBUFR data level category (CAT)
!      "lv" indexes the levels of the report
!          1) Lowest level
!      "jj" indexes the event stacks
!          1) N'th event
!          2) (N-1)'th event (if present)
!          3) (N-2)'th event (if present)
!                ...
!         10) (N-9)'th event (if present)
!      "kk" indexes the variable types
!          1) Pressure
!          2) Specific humidity
!          3) Temperature
!          4) Height
!          5) U-component wind
!          6) V-component wind
!
!      Note that the structure of this array is identical to one
!      returned from UFBEVN, with an additional (4th) dimension to
!      include the six variable types into the same array.
!
!      The return codes are as follows:
!      iret =  0 - normal return
!           =  1 - the station report within COMMON /PREPBC/ contains the
!                  last available subset from within the prepbufr file
!           = -1 - there are no more subsets available from within the
!                  prepbufr file       
!
!        INCLUDE         'readpb.prm'
!
        integer, intent(in) :: lunit
        character*(*), intent(inout) :: subset
        integer, intent(inout) :: idate, iret
!
! 
        character*(MXSTRL) ::   head
   
        character*(MXSTRL) ::   ostr ( MXR8VT ) 
       
        DATA head  / 'SID XOB YOB ELV DHR TYP T29 ITP' /
!
        DATA ostr / 'POB PQM PPC PRC PFC PAN POE CAT', &
                    'QOB QQM QPC QRC QFC QAN QOE CAT', &
                    'TOB TQM TPC TRC TFC TAN TOE CAT', &
                    'ZOB ZQM ZPC ZRC ZFC ZAN ZOE CAT', &
                    'UOB WQM WPC WRC UFC UAN WOE CAT', &
                    'VOB WQM WPC WRC VFC VAN WOE CAT'  /
!
        real(kind=dp) ::       hdr2 ( NHR8PM ), &
     &                  evns2 ( MXR8PM, MXR8LV, MXR8VN, MXR8VT )
!
        real(kind=dp) ::       r8sid, r8sid2, pob1, pob2
!
        character*8  ::  csid, csid2
        character*8, save  ::  subst2
!
        logical, save ::       match 
        
        DATA match / .true. /
!
        EQUIVALENCE     ( r8sid, csid ), ( r8sid2, csid2 )
!
        integer, save :: idate2
        integer :: jret, lv2, nlev2
!-----------------------------------------------------------------------
        iret = 0
!
!      If the previous call to this subroutine did not yield matching
!      mass and wind subsets, then READNS is already pointing at an
!      unmatched subset.  Otherwise, call READNS to advance the subset
!      pointer to the next subset.
!
        IF  ( match )  THEN
            CALL READNS  ( lunit, subset, idate, jret )
            IF  ( jret .ne. 0 )  THEN
                iret = -1
                RETURN
            END IF
        ELSE
            subset = subst2
            idate = idate2
        END IF
!
!      Read the HDR and EVNS data for the subset that is currently
!      being pointed to.
!
        CALL UFBINT  ( lunit, hdr, NHR8PM, 1, jret, head )
        DO ii = 1, MXR8VT
            CALL UFBEVN  ( lunit, evns ( 1, 1, 1, ii ), MXR8PM, MXR8LV, &
                           MXR8VN, nlev, ostr (ii) )
        END DO
!
!      Now, advance the subset pointer to the following subset and
!      read its HDR data.
!
        CALL READNS  ( lunit, subst2, idate2, jret )
        IF  ( jret .ne. 0 )  THEN
            iret = 1
            RETURN
        END IF
        CALL UFBINT  ( lunit, hdr2, NHR8PM, 1, jret, head )
! 
!      Check whether these two subsets have identical SID, YOB, XOB,
!      ELV, and DHR values.  If so, then they are matching mass and
!      wind subsets for a single station report.
!
        match = .true.
!
        IF  ( subset .ne. subst2 )  THEN
            match = .false.
            RETURN
        END IF
! 
        r8sid = hdr (1)
        r8sid2 = hdr2 (1)
        IF  ( csid .ne. csid2 )  THEN
            match = .false.
            RETURN
        END IF
! 
        DO ii = 2, 5
            IF  ( hdr (ii) .ne. hdr2 (ii) )  THEN
                match = .false.
                RETURN
            END IF
        END DO
!
!      Read the EVNS data for the second of the two matching subsets.
! 
        DO ii = 1, MXR8VT
            CALL UFBEVN  ( lunit, evns2 ( 1, 1, 1, ii ), MXR8PM, MXR8LV, &
                          MXR8VN, nlev2, ostr (ii) )
        ENDDO
!
!      Combine the EVNS data for the two matching subsets into a
!      single 4-D array.  Do this by merging the EVNS2 array into
!      the EVNS array.
!
        outer: DO lv2 = 1, nlev2
            inner: DO lv = 1, nlev
                pob1 = evns ( 1, lv, 1, 1 )
                pob2 = evns2 ( 1, lv2, 1, 1 )
                IF  ( pob1 .eq. pob2 )  THEN
!
!                This pressure level from the second subset also exists
!                in the first subset, so overwrite any "missing" piece
!                of data for this pressure level in the first subset
!                with the corresponding piece of data from the second
!                subset (since this results in no net loss of data!).
!
                  DO kk = 1, MXR8VT
                    DO jj = 1, MXR8VN
                      DO ii = 1, MXR8PM
                        !IF  ( evns ( ii, lv, jj, kk ) .eq. R8BFMS ) THEN
                        IF  ( evns ( ii, lv, jj, kk ) .ge. 1e10 ) THEN
                          evns ( ii, lv, jj, kk ) = &
                               evns2 ( ii, lv2, jj, kk )
                        END IF
                      END DO
                    END DO
                  END DO
                  exit inner
                ELSE IF  (  ( pob2 .gt. pob1 )  .or. &
                            ( lv .eq. nlev )  )  THEN
!
!                Either all remaining pressure levels within the first
!                subset are less than this pressure level from the
!                second subset (since levels within each subset are
!                guaranteed to be in descending order wrt pressure!)
!                *OR* there are more total levels within the second
!                subset than in the first subset.  In either case, we
!                should now add this second subset level to the end of
!                the EVNS array.
!
                  nlev = nlev + 1
                  DO kk = 1, MXR8VT
                    DO jj = 1, MXR8VN
                      DO ii = 1, MXR8PM
                        evns ( ii, nlev, jj, kk ) = &
                             evns2 ( ii, lv2, jj, kk )
                      END DO
                    END DO
                  END DO
                  exit inner
                END IF
            END DO inner
        END DO outer
! 
        RETURN
        END subroutine READPB

!----7----------------------------------------------------------------72
      SUBROUTINE virtmp(lev,k,idx,flag)
!
!   // Do not write virtual temperature observations.
!   // PREPBUFR Table 14 describes the VIRTMP processing step:
!   //    http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_14.htm
!   // 
!   // For VIRTMP program code 8 with reason code 3, do not use this
!   // this observation of virtual temperature.
!   // For VIRTMP program code 8 with any other reason code, step down the
!   // event stack index to find sensible temperature.
!   //

      real(kind=dp), parameter :: virtmp_prog_code = 8.0
      real(kind=dp), parameter :: virtmp_reason_code = 3.0
      integer, intent(in) :: lev, k
      integer, intent(out) :: idx, flag
      integer :: j

      idx = 0
      flag = 1
      
      do j = 1, MXR8VN
        if (evns(3,lev,j,k) .eq. virtmp_prog_code) then
          idx = j
                    
! Skip if reason code = 3
          if (evns(4,lev,j,k) .eq. virtmp_reason_code) then
            flag = -1
            return
          endif
                    
        endif
      enddo
      
      return
      end subroutine virtmp
!
end program decode_prepbufr      
