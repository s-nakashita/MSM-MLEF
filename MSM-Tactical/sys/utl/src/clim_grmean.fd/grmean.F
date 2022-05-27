      PROGRAM grmean
c$$$  main program documentation block
c
c main program:  grmean    get grib mean from files
c   prgmmr: kanamitsu          org: w/np51     date: 01-03-31
c
c abstract: get grib mean from model files
c
c program history log:
c   01-03-31  hann-ming juang  add w3tag calls for nco implementation
c
c namelists:
c   namin:      parameters determining new date
c
c input files:
c   unit   11  sigma file(s)
c
c output files:
c   unit   51  sigma file
c
c subprograms called:
c
c attributes:
c   language: fortran
c
c$$$
C
C  Mean GRIB file generator 
C 
C  MTOTL ... Max number of fields in given index file(s) 
C  LKPDS ... Number of PDS words to be recorded 
C 
C     PARAMETER(MTOTL=300,MDATA=20000) 
C
      PARAMETER(MTOTL=500,MMDAT=20000000,MDATA=MMDAT/MTOTL) 
      CHARACTER*64 LABEL 
      CHARACTER*8  LABBR
      CHARACTER*8  LVABBR
      LOGICAL LLEVL,LLTYP
      COMMON/COMCTL/LABEL(3,MTOTL),LENLA(3,MTOTL),LABBR(MTOTL),
     1              LVABBR(MTOTL),RLEV(MTOTL),
     2              ILINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     3              NLTOTL(MTOTL),NVTOTL,NPTOTL,LLEVL,LLTYP
C
      INTEGER KPDS(25,MTOTL),KGDS(22,MTOTL)
      INTEGER LSKIP(MTOTL),LGRIB(MTOTL)
C
C  NAMELIST PARAMETERS:
C
C  FNGRIB ... GRIB file name(s). 
C             Dimension for history sequence (in forecast time or initial)
C  FNPRINT ... File name where printouts from premap program goes
C  LDEBUG  ... TRUE for debugging printout. Output goes to FNPRINT
C
C  NAMELIST VARIABLE TYPES
C 
      PARAMETER(MFILE=500)
      CHARACTER*128 FNGRIB(MFILE)
      CHARACTER*128 FNGRBO
      CHARACTER*128 FNPRINT
C
      DATA MSK1/32000/,MSK2/4000/
C
      CHARACTER*80 FNKPDS5,FNKPDS6
C
C  NAMELIST DEFAULTS
C
      DATA FNGRIB /MFILE*'        '/
      DATA FNPRINT/'./grmean.out'/
      DATA FNKPDS5/'/home/sgi90/wd23ln/etc/grib1.kpds5.vsn21'/
      DATA FNKPDS6/'/home/sgi90/wd23ln/etc/grib1.kpds6.vsn21'/
C
      LOGICAL LAINC
      DATA LAINC/.TRUE./
      LOGICAL LDEBUG
      DATA LDEBUG/.FALSE./
C
C  Fort Unit numbers used inside the program
C
C  LUPTR  ... Unit unmber for diagnostic/debug print output other than uNIPLO
C  LUPGB  ... Unit unmber for input (grib) file
C  LUPGO  ... Unit unmber for grib output file
C
      DATA LUPGB/21/
      DATA LUPGO/51/
      DATA LUPTR/50/
C
      PARAMETER(MBUF=1024*128*64)
      CHARACTER*1 CBUF(MBUF),DBUF(MBUF)
C
      CHARACTER*80 ASGNSTR
      CHARACTER*8 CLEV
      CHARACTER*2 CYEAR,CMONTH,CDAY,CHOUR
      CHARACTER*3 CFHOUR1,CFHOUR2
C
      DIMENSION COUNT(MDATA,MTOTL),AVGRID(MDATA,MTOTL)
C
      DIMENSION GRID(MDATA)
      LOGICAL*1 LBMS(MDATA)
C
      LOGICAL LDBG
      COMMON/COMDBG/ LDBG
C
      REAL*8 FR(MDATA)
      INTEGER IBM(MDATA)
      PARAMETER(LENPDS=28,LENGDS=32,MXBIT=16)
C     CHARACTER GRIB(30+LENPDS+LENGDS+MDATA*(MXBIT+1)/8)
      CHARACTER GRIB(30+LENPDS+LENGDS+MDATA*(MXBIT+1)/8)
C
      INTEGER*4 JPDS4(25),JGDS4(22),JENS4(5)
      INTEGER*4 MPDS4(25),MGDS4(22),MENS4(5)
      INTEGER*4 LUPGB4,LUPTR4,MSK14,MSK24,MNUM4,MBUF4
      INTEGER*4 NLEN4,NNUM4,IRET4,NDATA4
      INTEGER*4 KPDS4(25,MTOTL),KGDS4(22,MTOTL)
      INTEGER*4 LSKIP4(MTOTL),LGRIB4(MTOTL)
      REAL*4    GRID4(MDATA)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call w3tagb('clim_grmean',2001,0000,0000,'np51')
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  Open main program (debug) print file
C
      OPEN(LUPTR,FILE=FNPRINT,ERR=711)
      GO TO 712
  711 CONTINUE
      PRINT *,' ERROR IN OPENING FILE ',FNPRINT
      CALL ABORT
  712 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNPRINT(1:50),' opened.  Unit=',LUPTR
C
C  Read main program namelist
C
      READ (5,'(A80)') FNKPDS5
      READ (5,'(A80)') FNKPDS6
      WRITE(6,'(A80)') FNKPDS5
      WRITE(6,'(A80)') FNKPDS6
C
      LDBG=LDEBUG
C
      READ (5,'(I10)') INCHR
      WRITE(6,'(I10)') INCHR
      READ (5,'(A128)') FNGRBO
      WRITE(6,'(A128)') FNGRBO
C
      READ (5,*,END=908) NFILES
      GO TO 907
  908 CONTINUE
      WRITE(6,*) ' EOF on data'
      CALL ABORT
  907 CONTINUE
      WRITE(6,*) ' NFILES=',NFILES
      IF(NFILES.EQ.0) THEN
        CALL ABORT
      ENDIF 
C
      IF(NFILES.GT.MFILE) THEN
        WRITE(LUPTR,*) ' ERROR!!!  Number of files exceeded limit.'
        PRINT *, ' ERROR!!!  Number of files exceeded limit.'
        CALL ABORT
      ENDIF
C
      DO N=1,NFILES
        READ (5,'(A128)') FNGRIB(N)
        WRITE(6,'(8H FNGRIB=,A50)') FNGRIB(N)
      ENDDO
C
      READ(5,*) IFTS
      READ(5,*) IFTE
      WRITE(6,*) 'IFTS,IFTE=',IFTS,IFTE
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      CALL INIPARM(FNKPDS5,LUPTR)
      CALL INILEVL(FNKPDS6,LUPTR)
C
C  Get grib index buffer
C
      WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGB
      CALL ASSIGN(ASGNSTR)
C     OPEN(UNIT=LUPGB,FILE=FNGRIB(1),FORM='UNFORMATTED',ERR=978)
      LUPGB4=LUPGB
      CALL BAOPEN(LUPGB4,FNGRIB(1),IRET4)
      IRET=IRET4
      IF(IRET.NE.0) GO TO 978
      GO TO 977
  978 CONTINUE
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB(1)
      PRINT *,'ERROR IN OPENING FILE ',FNGRIB(1)
      CALL ABORT
  977 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNGRIB(1)(1:50),' opened. Unit=',LUPGB
C
      MNUM=0
C
C  BUILD VARIABLE NAME TABLE FROM THE FIRST GRIB FILE
C
      LUPGB4=LUPGB
      MSK14=MSK1
      MSK24=MSK2
      MNUM4=MNUM
      MBUF4=MBUF
      CALL GETGIR(LUPGB4,MSK14,MSK24,MNUM4,MBUF4,CBUF,NLEN4,NNUM4,IRET4)
      NLEN=NLEN4
      NNUM=NNUM4
      IRET=IRET4
C
      IF(IRET.NE.0) THEN
        WRITE(LUPTR,*) 'ERROR.  CBUF length too short in GETGIR'
        PRINT *,'ERROR.  CBUF length too short in GETGIR'
        CALL ABORT
      ENDIF
      IF(NNUM.EQ.0) THEN
        WRITE(LUPTR,*) 'ERROR. Not a grib file. Detected in GETGIR'
        PRINT *,'ERROR.  Not a grib file. Detected in GETGIR'
        CALL ABORT
      ENDIF
      IF(NLEN.EQ.0) THEN
        WRITE(LUPTR,*) 'ERROR. NLEN=0. Detected in GETGIR'
        PRINT *,'ERROR.  NLEN=0.  Detected in GETGIR'
        CALL ABORT
      ENDIF
      IF(NNUM.GT.MTOTL) THEN
        WRITE(LUPTR,*) 'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
        PRINT *,'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
        CALL ABORT
      ENDIF
C
      NLEN4=NLEN
      NNUM4=NNUM
      CALL UNPINDX(CBUF,NLEN4,NNUM4,KPDS4,KGDS4,LSKIP4,LGRIB4,IRET4)
      DO N=1,NNUM
        DO I=1,25
          KPDS(I,N)=KPDS4(I,N)
        ENDDO
        DO I=1,22
          KGDS(I,N)=KGDS4(I,N)
        ENDDO
        LSKIP(N)=LSKIP4(N)
        LGRIB(N)=LGRIB4(N)
      ENDDO
      IRET=IRET4
C
      IMAX=KGDS(2,1)
      JMAX=KGDS(3,1)
      IJMAX=IMAX*JMAX
      IF(IJMAX.GT.MDATA) THEN
        WRITE(6,*) 'INCREASE MDATA to ',IJMAX
        CALL ABORT
      ENDIF
C
      NTOTL=NNUM
C
      WRITE(LUPTR,*) ' NTOTL=',NTOTL
C
C     WRITE(LUPTR,*) ' KPDS=',(KPDS(I,1),I=1,25)
C     WRITE(LUPTR,*) ' KGDS=',(KGDS(I,1),I=1,22)
C
      DO N=1,NTOTL
        CALL GETPARM(KPDS(1,N),1,LABEL(1,N),LENLA(1,N),LABBR(N),
     1               LUPTR)
        CALL GETLEVL(KPDS(1,N),1,LABEL(1,N),LENLA(1,N),RLEV(N),
     1               LVABBR(N),LUPTR)
      ENDDO
C
C  Get rid of unnecessary spaces from LABEL
C
      DO N=1,NTOTL
        CALL RMBLNK(LABEL(1,N),LENLA(1,N),3)
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        WRITE(LUPTR,*) N,')',LABEL(1,N)(1:LENLA(1,N)),
     1                            ' (',LABBR(N),' ) ',
     2                ' at ',LABEL(2,N)(1:LENLA(2,N)),' ',
     3                       LABEL(3,N)(1:LENLA(3,N))
      ENDDO
C
      DO N=1,NTOTL
        DO IJ=1,IJMAX
          COUNT(IJ,N)=0.
          AVGRID(IJ,N)=0.
        ENDDO
      ENDDO
C
C  Loop through GRIB file
C
      DO NFILE=1,NFILES
C
        PRINT *,' Processing ', FNGRIB(NFILE)(1:50)
C
C  Get grib index buffer for this particular grib file
C
        WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGB
        CALL ASSIGN(ASGNSTR)
C       OPEN(UNIT=LUPGB,FILE=FNGRIB(NFILE),FORM='UNFORMATTED',ERR=878)
        LUPGB4=LUPGB
        CALL BAOPEN(LUPGB4,FNGRIB(NFILE),IRET4)
        IRET=IRET4
        GO TO 877
  878   CONTINUE
        WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB(NFILE)
        PRINT *,'ERROR IN OPENING FILE ',FNGRIB(NFILE)
        CALL ABORT
  877   CONTINUE
        WRITE(LUPTR,*) FNGRIB(NFILE)(1:50),' opened. Unit=',LUPGB
        MNUM=0
C
        DO IJ=1,IJMAX
          LBMS(IJ)=.TRUE.
        ENDDO
C
C       REWIND LUPGB
        MNUM4=0
        LUPGB4=LUPGB
        MSK14=MSK1
        MSK24=MSK2
        MBUF4=MBUF
        CALL GETGIR(LUPGB4,MSK14,MSK24,MNUM4,MBUF4,
     1              CBUF,NLEN4,NNUM4,IRET4)
        NLEN=NLEN4
        NNUM=NNUM4
        IRET=IRET4
C
        WRITE(LUPTR,*)'NLEN=',NLEN,' NNUM=',NNUM
        IF(IRET.NE.0) THEN
          WRITE(LUPTR,*) 'ERROR.  CBUF length too short in GETGIR'
          PRINT *,'ERROR.  CBUF length too short in GETGIR'
          CALL ABORT
        ENDIF
        IF(NNUM.EQ.0) THEN
          WRITE(LUPTR,*) 'ERROR. Not a grib file. Detected in GETGIR'
          PRINT *,'ERROR.  Not a grib file. Detected in GETGIR'
          CALL ABORT
        ENDIF
        IF(NLEN.EQ.0) THEN
          WRITE(LUPTR,*) 'ERROR. NLEN=0. Detected in GETGIR'
          PRINT *,'ERROR.  NLEN=0.  Detected in GETGIR'
          CALL ABORT
        ENDIF
        IF(NNUM.GT.MTOTL) THEN
          WRITE(LUPTR,*) 'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
          PRINT *,'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
          CALL ABORT
        ENDIF
C
C  Assuming that the record comes in the same order so that
C  file is not rewound.
C
        NN4=0
        DO N=1,NTOTL
          DO I=1,25
            JPDS4(I)=-1
          ENDDO
          DO I=5,7
            JPDS4(I)=KPDS(I,N)
          ENDDO
          DO I=1,22
            JGDS4(I)=-1
          ENDDO
          DO I=1,5
            JENS4(I)=-1
          ENDDO
          NLEN4=NLEN
          NNUM4=NNUM
C
          CALL GETGBSS(CBUF,NLEN4,NNUM4,NN4,JPDS4,JGDS4,JENS4,
     &                 K4,MPDS4,MGDS4,MENS4,MSKIP4,MGRIB4,IRET4)
          IF(MGRIB4.EQ.0) THEN
            WRITE(LUPTR,*) ' Error in GETGBSS.  Field not found.'
            CALL ABORT
          ENDIF
C
C  TIME CHECK. INCHR.LT.0 IS USED FOR CHECKING FORECAST HOURS
C
          IF(N.EQ.1) THEN
            PRINT *,'YMDHF=',
     &              MPDS4(8),MPDS4(9),MPDS4(10),MPDS4(11),MPDS4(14)
            WRITE(LUPTR,*) 'YMDHF=',
     &              MPDS4(8),MPDS4(9),MPDS4(10),MPDS4(11),MPDS4(14)
          ENDIF
C
          IF(NFILE.EQ.1.AND.N.EQ.1) THEN
            INIFH=KPDS(14,N)
            IFH=KPDS(15,N)
          ENDIF
C
          IF(INCHR.NE.0) THEN
            IF(N.EQ.1) THEN
              IF(INCHR.GT.0) THEN
    				  	IF(NFILE.GT.1) THEN
                  CALL INCDTE(IYPR,IMPR,IDPR,IHPR,JY,JM,JD,JH,INCHR)
                ENDIF
                IYPR=MPDS4(8)
                IMPR=MPDS4(9)
                IDPR=MPDS4(10)
                IHPR=MPDS4(11)
              ELSE
  			  	  	IF(NFILE.GT.1) THEN
                  JFH=IFPR-INCHR
                ENDIF
                IFPR=MPDS4(14)
              ENDIF
            ENDIF
C
            IF(NFILE.GT.1) THEN
              IF(INCHR.GT.0) THEN
                IF(JY.NE.MPDS4( 8).OR.JM.NE.MPDS4( 9).OR.
     &             JD.NE.MPDS4(10).OR.JH.NE.MPDS4(11)) THEN
                  PRINT *,'SHOULD BE:',JY,JM,JD,JH
                  WRITE(LUPTR,*) 'SHOULD BE:',JY,JM,JD,JH
                  PRINT *,'BUT ARE  :',MPDS4(8),MPDS4(9),
     &                                 MPDS4(10),MPDS4(11)
                  WRITE(LUPTR,*) 'BUT ARE  :',MPDS4(8),MPDS4(9),
     &                                 MPDS4(10),MPDS4(11)
                  PRINT *,'FILE NOT REGULAR INTERVAL IN ANALYSIS TIME'
                  WRITE(LUPTR,*) 'FILE NOT REGULAR INT IN ANALY TIME'
                  CALL ABORT
                ENDIF
              ELSE
                IF(JFH.NE.MPDS4(14)) THEN
                  PRINT *,'SHOULD BE:',JFH
                  PRINT *,'BUT ARE  :',MPDS4(14)
                  PRINT *,'FILE NOT REGULAR INTERVAL IN FORECAST TIME'
                  WRITE(LUPTR,*) 'SHOULD BE:',JFH
                  WRITE(LUPTR,*) 'BUT ARE  :',MPDS4(14)
                  WRITE(LUPTR,*) 'FILE NOT REGULAR INTVL IN FCST TIME'
                  CALL ABORT
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
          IF(IFTS.GE.0) THEN
            IF(MPDS4(14).LT.IFTS) THEN
              PRINT *,'File skipped'
              WRITE(LUPTR,*) 'File skipped'
              GO TO 8200
            ENDIF
          ENDIF
          IF(IFTE.GE.0) THEN
            IF(MPDS4(14).GT.IFTE) GO TO 8300
          ENDIF
C
          DO IJ=1,IJMAX
            LBMS(IJ)=.TRUE.
          ENDDO
C
          LUPGB4=LUPGB
          LUPTR4=LUPTR
          CALL RDGB(LUPGB4,MGRIB4,MSKIP4,
     1              MPDS4,MGDS4,NDATA4,LBMS,GRID4,LUPTR4)
C
          IF(NDATA4.NE.IJMAX) THEN
            WRITE(LUPTR,*) ' ERROR IN RDGB. NDATA.NE.IJMAX'
            WRITE(LUPTR,*) ' NDATA=',NDATA4,' IJMAX=',IJMAX
            WRITE(LUPTR,*) ' NFILE=',NFILE
            WRITE(LUPTR,*) ' FNGRIB=',FNGRIB(NFILE)
            PRINT *,' ERROR IN RDGB.   NDATA.NE.IJMAX'
            CALL ABORT
          ENDIF
					DO IJ=1,IJMAX
						IF(LBMS(IJ)) THEN
							COUNT(IJ,N)=COUNT(IJ,N)+1.
							AVGRID(IJ,N)=AVGRID(IJ,N)+GRID4(IJ)
						ENDIF
					ENDDO
        ENDDO
C
 8200 CONTINUE
      ENDDO
 8300 CONTINUE
C
C  Open output grib file
C
      WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGO
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUPGO,FILE=FNGRBO,FORM='UNFORMATTED',ERR=968)
      GO TO 967
  968 CONTINUE
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRBO
      PRINT *,'ERROR IN OPENING FILE ',FNGRBO
      CALL ABORT
  967 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNGRBO(1:128),' opened. Unit=',LUPGO
      PRINT *,' FILE ',FNGRBO(1:128),' opened. Unit=',LUPGO
C
      DO N=1,NTOTL
        DO IJ=1,IJMAX
          IF(COUNT(IJ,N).EQ.0.) THEN
            LBMS(IJ)=.FALSE.
            AVGRID(IJ,N)=0.
          ELSE
            LBMS(IJ)=.TRUE.
            AVGRID(IJ,N)=AVGRID(IJ,N)/COUNT(IJ,N)
          ENDIF
        ENDDO
C
C  MODIFY KPDS FOR AVERAGE
C
C  ADD PRECISION TO AVERAGE
C
        KPDS(22,N)=KPDS(22,N)+1
C
C  Number in the avarage
C
        KPDS(17,N) = NFILES
C
        IF(LAINC) THEN
C
C  This is many initial conditions
C
          IF((KPDS(16,N).EQ.10).AND.(KPDS(14,N).EQ.0)) THEN
C
C  THIS IS ANALYSIS AVERAGED 
C
            WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
            WRITE(LUPTR,*) ' INSTANT PROD VALID AT REFERENCE TIME + P1'
            KPDS(16,N) = 123
          ELSEIF ((KPDS(16,N).EQ.10).AND.(KPDS(14,N).NE.0)) THEN
C
C  THIS IS FORECASTS AT HOUR KPDS(14,1) AVERAGED
C
            WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
            WRITE(LUPTR,*) '  FORECAST PRODUCT VALID AT REF TIME + P1 '
            KPDS(16,N) = 113      
          ELSEIF (KPDS(16,N).EQ.3 ) THEN
C
C  THIS IS AVERAGE OF MANY AVERAGES BETWEEN THE FORECAST HOUR 
C  KPDS(14,N) and KPDS(15,N). EXAMPLE MANY FLUX FILES 
C
             IF(IFTS.EQ.0) THEN
               WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
               WRITE(LUPTR,*) ' AVERAGE ( REF TIME + P1 TO REF T + P2 )'
               KPDS(14,N) = KPDS(14,N)/2     ! Octet no. 19
               KPDS(16,N) = 113              ! Octet no. 21
             ELSEIF(IFTS.EQ.-1) THEN
               KPDS(14,N) = INIFH/24
               KPDS(15,N) = IFH/24
               KPDS(13,N) = 2
               KPDS(16,N) = 3
             ELSEIF(IFTS.EQ.-2) THEN
               KPDS(14,N) = (INIFH/24+1)/30
               KPDS(15,N) = (IFH/24+1)/30
               KPDS(13,N) = 3
               KPDS(16,N) = 3
             ENDIF
          ELSE
             WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS: ',KPDS(16,N)
             WRITE(LUPTR,*) ' OUTPUT GRIB MESSAGE MAY NOT BE CORRECT '
          ENDIF
        ELSE
C
C  THIS IS THE AVERAGE FOR THE FORECAST HOUR
C
           WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
           WRITE(LUPTR,*) ' SIMPLY A FORECAST HISTORY FILE'
             IF(IFTS.EQ.0) THEN
               KPDS(14,N) = INIFH
               KPDS(15,N) = IFH
             ELSEIF(IFTS.EQ.-1) THEN
               KPDS(14,N) = INIFH/24
               KPDS(15,N) = IFH/24
               KPDS(13,N) = 2
             ELSEIF(IFTS.EQ.-2) THEN
               KPDS(14,N) = (INIFH/24+1)/30
               KPDS(15,N) = (IFH/24+1)/30
               KPDS(13,N) = 3
             ENDIF
             KPDS(16,N) = 3
        ENDIF
C
C  Increment of data in the average
C
        KPDS(15,N) = INCHR
C
        WRITE(LUPTR,*) 'MAXMIN of AVGRID N=',N
        CALL MAXMIN(AVGRID(1,N),IJMAX,1,IJMAX,1,1,LUPTR)
C
        WRITE(LUPTR,*) 'IJMAX=',IJMAX
        WRITE(LUPTR,*) 'KPDS=',(KPDS(I,N),I=1,25)
        WRITE(LUPTR,*) 'KGDS=',(KGDS(I,N),I=1,22)
        CALL PUTGB(LUPGO,IJMAX,KPDS(1,N),KGDS(1,N),LBMS,
     1             AVGRID(1,N),IBM,FR,GRIB,IGRIB,LUPTR,IRET)
        IF(IRET.NE.0) THEN
          WRITE(LUPTR,*) 'ERROR IN PUTGB'
          CALL ABORT
        ELSE
          WRITE(LUPTR,*) 'GRIB-END=',(GRIB(I),I=IGRIB-3,IGRIB)
          WRITE(LUPTR,*) N,') GRIB RECORD WRITTEN. LGRIB=',IGRIB
        ENDIF
      ENDDO
C
      CLOSE(LUPGO)
      CLOSE(LUPTR)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL w3tage('clim_grmean')
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      STOP
      END
C
      SUBROUTINE INIPARM(FNKPDS5,LUPTR)
C
C     Initialize KPDS(5) table
C
      PARAMETER(LUKP5=90)
C
      CHARACTER*80 FNKPDS5
C
      PARAMETER (NKPDS5=255)
      CHARACTER*64 PINFO(0:NKPDS5)
      CHARACTER*64 PNAME(0:NKPDS5)
      CHARACTER*64 PUNIT(0:NKPDS5)
      DIMENSION LENPI(0:NKPDS5),LENPN(0:NKPDS5),LENPU(0:NKPDS5)
C
      COMMON/COMPRM/ PINFO,PUNIT,PNAME,LENPI,LENPU,LENPN
C
      CHARACTER*80 CPDS5
      CHARACTER*80 STRING
C
C  Open Parameter table (PDS5) file
C
      OPEN(LUKP5,FILE=FNKPDS5,STATUS='OLD',ERR=1)
      GO TO 2
    1 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKPDS5
      PRINT *,'ERROR IN OPENING FILE ',FNKPDS5
      CALL ABORT
    2 CONTINUE
      WRITE(LUPTR,*) 'FILE ',FNKPDS5(1:50),' opened.  Unit=',LUKP5
C
      N=1
  300 CONTINUE
      READ(LUKP5,100,END=200) STRING
  100 FORMAT(A80)
      IS=1
			NDELIM=0
      DO 500 I=1,80
			IF(STRING(I:I).EQ.':') THEN
        NDELIM=NDELIM+1
        NCHAR=I-1-IS+1
        IF(NCHAR.GT.0) THEN
          IF(NDELIM.EQ.1) THEN
            CPDS5(1:NCHAR)=STRING(IS:I-1)
						READ(CPDS5,110) M
  110       FORMAT(I3)
          ELSEIF(NDELIM.EQ.2) THEN
						JJ=0
            DO 505 J=IS+1,I-1
            IF(STRING(J-1:J-1).NE.' '.OR.STRING(J:J).NE.' ') THEN
	  					JJ=JJ+1
              PINFO(M)(JJ:JJ)=STRING(J-1:J-1)
						ENDIF
  505       CONTINUE
            IF(STRING(I-1:I-1).NE.' ') THEN
	  					JJ=JJ+1
              PINFO(M)(JJ:JJ)=STRING(I-1:I-1)
						ENDIF
            LENPI(M)=JJ
          ELSEIF(NDELIM.EQ.3) THEN
						JJ=0
            DO 510 J=IS,I-1
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              PUNIT(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  510       CONTINUE
            LENPU(M)=JJ
						JJ=0
            DO 520 J=I+1,80
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              PNAME(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  520       CONTINUE
            LENPN(M)=JJ
						GO TO 500
          ENDIF
  				IS=I+1
				ENDIF
			ENDIF
  500 CONTINUE
C
			IF(NDELIM.EQ.1) THEN
				PINFO(M)(1:1)='?'
				PUNIT(M)(1:1)='?'
				PNAME(M)(1:1)='?'
        LENPI(M)=1
        LENPU(M)=1
        LENPN(M)=1
			ELSEIF(NDELIM.EQ.2) THEN
				PUNIT(M)(1:1)='?'
				PNAME(M)(1:1)='?'
        LENPU(M)=1
        LENPN(M)=1
			ENDIF
C
      N=N+1
      GO TO 300
  200 CONTINUE
C
      CLOSE(UNIT=LUKP5)
      RETURN
      END
      SUBROUTINE INILEVL(FNKPDS6,LUPTR)
C
      PARAMETER (LUKP6=90)
      PARAMETER (NKPDS6=255)
      CHARACTER*80  PDS6 (0:NKPDS6)
C
      CHARACTER*64 LINFO(0:NKPDS6)
      CHARACTER*64 LUNIT(0:NKPDS6)
      CHARACTER*64 LFACT(0:NKPDS6)
      CHARACTER*64 LBASE(0:NKPDS6)
      CHARACTER*64 LTYPE(0:NKPDS6)
      CHARACTER*64 LEVLR(0:NKPDS6)
      CHARACTER*3  LEVAB(0:NKPDS6)
C
      DIMENSION LENLI(0:NKPDS6),LENLU(0:NKPDS6),LENLF(0:NKPDS6),
     1          LENLB(0:NKPDS6),LENLT(0:NKPDS6),LENLE(0:NKPDS6)
C
      COMMON/COMLVL/ LINFO,LUNIT,LFACT,LBASE,LTYPE,LEVLR,LEVAB,
     1               LENLI,LENLU,LENLF,LENLB,LENLT,LENLE
C
      CHARACTER*80 FNKPDS6
C
      CHARACTER*160 STRING
C
C  Open Level table (PDS6) file
C
      OPEN(LUKP6,FILE=FNKPDS6,STATUS='OLD',ERR=761)
      GO TO 762
  761 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKPDS6
      PRINT *,'ERROR IN OPENING FILE ',FNKPDS6
      CALL ABORT
  762 CONTINUE
      WRITE(LUPTR,*) 'FILE ',FNKPDS6(1:50),' opened.  Unit=',LUKP6
C
      N=1
  300 CONTINUE
      READ(LUKP6,100,END=200) STRING
  100 FORMAT(A80)
      IS=1
			NDELIM=0
      DO 500 I=1,80
			IF(STRING(I:I).EQ.':') THEN
        NDELIM=NDELIM+1
        NCHAR=I-1-IS+1
        IF(NCHAR.GT.0) THEN
          IF(NDELIM.EQ.1) THEN
            PDS6(N-1)(1:NCHAR)=STRING(IS:I-1)
						READ(PDS6(N-1),110) M
  110       FORMAT(I3)
          ELSEIF(NDELIM.EQ.2) THEN
						JJ=0
            DO 505 J=IS+1,I-1
            IF(STRING(J-1:J-1).NE.' '.OR.STRING(J:J).NE.' ') THEN
	  					JJ=JJ+1
              LINFO(M)(JJ:JJ)=STRING(J-1:J-1)
						ENDIF
  505       CONTINUE
            IF(STRING(I-1:I-1).NE.' ') THEN
	  					JJ=JJ+1
              LINFO(M)(JJ:JJ)=STRING(I-1:I-1)
						ENDIF
						LENLI(M)=JJ
          ELSEIF(NDELIM.EQ.3) THEN
						JJ=0
            DO 506 J=IS,I-1
            IF(STRING(J-1:J-1).NE.' ') THEN
	  					JJ=JJ+1
              LUNIT(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  506       CONTINUE
						LENLU(M)=JJ
            IF(LUNIT(M)(1:1).EQ.'-') THEN
              LUNIT(M)(1:1)=' '
              LENLU(M)=1
            ENDIF
          ELSEIF(NDELIM.EQ.4) THEN
						JJ=0
            DO 507 J=IS,I-1
            IF(STRING(J-1:J-1).NE.' ') THEN
	  					JJ=JJ+1
              LFACT(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  507       CONTINUE
						LENLF(M)=JJ
          ELSEIF(NDELIM.EQ.5) THEN
						JJ=0
            DO 508 J=IS,I-1
            IF(STRING(J-1:J-1).NE.' ') THEN
	  					JJ=JJ+1
              LBASE(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  508       CONTINUE
						LENLB(M)=JJ
          ELSEIF(NDELIM.EQ.6) THEN
						JJ=0
            DO 510 J=IS,I-1
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              LTYPE(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  510       CONTINUE
						LENLT(M)=JJ
          ELSEIF(NDELIM.EQ.7) THEN
						JJ=0
            DO 520 J=IS,I-1
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              LEVLR(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  520       CONTINUE
						LENLE(M)=JJ
						JJ=0
C           DO 530 J=I+1,80
            DO 530 J=I+1,I+3
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              LEVAB(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  530       CONTINUE
						GO TO 500
          ENDIF
  				IS=I+1
				ENDIF
			ENDIF
  500 CONTINUE
C
			IF(NDELIM.EQ.1) THEN
				LINFO(M)(1:1)='?'
				LUNIT(M)(1:1)='?'
        LFACT(M)(1:1)='?'
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLI(M)=1
        LENLU(M)=1
        LENLF(M)=1
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.2) THEN
				LUNIT(M)(1:1)='?'
        LFACT(M)(1:1)='?'
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLU(M)=1
        LENLF(M)=1
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.3) THEN
        LFACT(M)(1:1)='?'
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLF(M)=1
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.4) THEN
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.5) THEN
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.6) THEN
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLE(M)=1
			ENDIF
C
      N=N+1
      GO TO 300
  200 CONTINUE
C
      CLOSE(UNIT=LUKP6)
C
      RETURN
      END
      SUBROUTINE GETPARM(KPDS,NGI,LABEL,LENLA,LABBR,LUPTR)
C
      DIMENSION KPDS(22,*)
C
C     Transrates KPDS(5,*) parameter to variable names
C
      CHARACTER*64 LABEL(3,*)
      DIMENSION LENLA(3,*)
      CHARACTER*8 LABBR(*)
C
      PARAMETER (NKPDS5=255)
      CHARACTER*64 PINFO(0:NKPDS5)
      CHARACTER*64 PNAME(0:NKPDS5)
      CHARACTER*64 PUNIT(0:NKPDS5)
      DIMENSION LENPI(0:NKPDS5),LENPN(0:NKPDS5),LENPU(0:NKPDS5)
C
      SAVE NUNDEF
      DATA NUNDEF/0/
C
      COMMON/COMPRM/ PINFO,PUNIT,PNAME,LENPI,LENPU,LENPN
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
      DO N=1,NGI
				K=KPDS(5,N)
C
				IF(K.GT.NKPDS5) THEN
           WRITE(LUPTR,*) ' FATAL ERROR in INDEX file! ',
     1          ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           WRITE(LUPTR,*) ' KPDS(5,N) CANNOT BE GREATER THAN ',NKPDS5
           PRINT *,' FATAL ERROR in INDEX file! ',
     1          ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           PRINT *,' KPDS(5,N) CANNOT BE GREATER THAN ',NKPDS5
           CALL ABORT
        ENDIF
        IF(PNAME(K)(1:1).EQ.'?'.AND.PINFO(K)(1:1).EQ.'?'
     1     .AND.PUNIT(K)(1:1).EQ.'?') THEN
           WRITE(LUPTR,*) ' SERIOUS PROBLEM in INDEX file',
     1           ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           WRITE(LUPTR,*) ' THE PARAMETER IS NOT DEFINED ',
     1           'IN THE KPDS(5) TABLE FILE '
           PRINT *,' Serious problem in INDEX file',
     1           ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           PRINT *,' THE PARAMETER IS NOT DEFINED ',
     1           'IN THE KPDS(5) TABLE FILE '
           NUNDEF=NUNDEF+1
           IF(NUNDEF.GE.100) THEN
             WRITE(LUPTR,*) 'Too many undefined variables'
             PRINT *,'Too many undefined variables'
             CALL ABORT
           ENDIF
           IF(NUNDEF.LT.10) THEN
             WRITE(PINFO(K),'(6HUNDEF0,I1)') NUNDEF
           ELSEIF(NUNDEF.LT.100) THEN
             WRITE(PINFO(K),'(5HUNDEF,I2)') NUNDEF
           ENDIF
           LENPI(K)=7
           PNAME(K)=PINFO(K)
           LENPN(K)=7
           PUNIT(K)='?'
           LENPU(K)=1
           WRITE(LUPTR,*) 'NUNDEF=',NUNDEF,' PINFO(K)=',PINFO(K),
     1                    ' PNAME(K)=',PNAME(K),' PUNIT(K)=',PUNIT(K)
		    ENDIF
C
        LABEL(1,N)=PINFO(K)(1:LENPI(K))//' ('//PUNIT(K)(1:LENPU(K))//')'
        LENLA(1,N)=LENPI(K)+2+LENPU(K)+1
C
C       WRITE(LUPTR,*) 'N=',N,' K=',K,' ',LABEL(1,N)(1:LENLA(1,N))
        LABBR(N)='        '
        LABBR(N)=PNAME(K)(1:LENPN(K))
      ENDDO
C
      RETURN
			END
      SUBROUTINE GETLEVL(KPDS,NGI,LABEL,LENLA,RLEV,LVABBR,LUPTR)
C
C     Transrates KPDS(6,*) parameter to level names
C
      CHARACTER*64 LABEL(3,*)
      DIMENSION LENLA(3,*)
      DIMENSION RLEV(*)
      CHARACTER*8 LVABBR(*)
C
      DIMENSION KPDS(22,*)
C
      CHARACTER*160 FMT
C
      PARAMETER (NKPDS6=255)
      CHARACTER*64 LINFO(0:NKPDS6)
      CHARACTER*64 LUNIT(0:NKPDS6)
      CHARACTER*64 LFACT(0:NKPDS6)
      CHARACTER*64 LBASE(0:NKPDS6)
      CHARACTER*64 LTYPE(0:NKPDS6)
      CHARACTER*64 LEVLR(0:NKPDS6)
      CHARACTER*3  LEVAB(0:NKPDS6)
C
      DIMENSION LENLI(0:NKPDS6),LENLU(0:NKPDS6),LENLF(0:NKPDS6),
     1          LENLB(0:NKPDS6),LENLT(0:NKPDS6),LENLE(0:NKPDS6)
C
      COMMON/COMLVL/ LINFO,LUNIT,LFACT,LBASE,LTYPE,LEVLR,LEVAB,
     1               LENLI,LENLU,LENLF,LENLB,LENLT,LENLE
C
      CHARACTER*64 LTOPL,LBTML
C
      SAVE NUNDEF
      DATA NUNDEF/0/
C
      DO 100 N=1,NGI
				K=KPDS(6,N)
				IF(K.GT.NKPDS6.OR.K.LT.0) THEN
           WRITE(LUPTR,*) ' FATAL ERROR in INDEX FILE! ',
     1          'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' KPDS(6,N) CANNOT BE GREATER THAN ',NKPDS6
           PRINT *,' FATAL ERROR in INDEX FILE! ',
     1          'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           PRINT *,' KPDS(6,N) CANNOT BE GREATER THAN ',NKPDS6
           CALL ABORT
        ENDIF
        IF(LINFO(K)(1:1).EQ.'?'.AND.LUNIT(K)(1:1).EQ.'?'
     1     .AND.LEVLR(K)(1:1).EQ.'?') THEN
           WRITE(LUPTR,*) ' Serious problem in index FILE! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' THE LEVEL IS NOT DEFINED ',
     1        'IN THE FILE KPDS(6) TABLE FILE.'
           PRINT *,' Serious problem in index FILE! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           PRINT *,' THE LEVEL IS NOT DEFINED ',
     1        'IN THE FILE KPDS(6) TABLE FILE.'
           NUNDEF=NUNDEF+1
           IF(NUNDEF.GE.100) THEN
             WRITE(LUPTR,*) 'Too many undefined levels'
             PRINT *,'Too many undefined levels'
             CALL ABORT
           ENDIF
           IF(NUNDEF.LT.10) THEN
             WRITE(LINFO(K),'(6HUNDEF0,I1)') NUNDEF
           ELSEIF(NUNDEF.LT.100) THEN
             WRITE(LINFO(K),'(5HUNDEF,I2)') NUNDEF
           ENDIF
           LENLI(K)=7
           LUNIT(K)='?'
           LENLU(K)=1
           LEVLR(K)='level'
           LTYPE(K)='-'
		    ENDIF
				IF(K.GE.140.AND.K.LE.159) THEN
           WRITE(LUPTR,*) ' ERROR in utilizing index FILE ! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' Mixed precision level not programed',
     1        'to work in this program.'
           PRINT *,' ERROR in utilizing index FILE ! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           PRINT *,' Mixed precision level not programed',
     1        'to work in this program.'
  		     CALL ABORT
        ENDIF
C
        IF(LTYPE(K)(1:1).EQ.'?') THEN
          WRITE(LUPTR,*) ' FATAL ERROR! KPDS(6,N)=',
     1           KPDS(6,N),' FOR N=',N
          WRITE(LUPTR,*) ' LVTYP NOT SPECIFIED IN THE LEVEL subroutine'
          PRINT *,' FATAL ERROR! KPDS(6,N)=',
     1           KPDS(6,N),' FOR N=',N
          PRINT *,' LTYPE NOT SPECIFIED IN THE LEVEL subroutine'
          CALL ABORT
        ENDIF
C
        DO I=1,LENLF(K)
          IF(LFACT(K)(I:I).EQ.'.') THEN
            NDEC=LENLF(K)-I
          ENDIF
        ENDDO
        IF(LENLF(K).LT.10) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I1,1H.,I1,1H))') LENLF(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I1,1H.,I2,1H))') LENLF(K),NDEC
          ENDIF
        ELSEIF(LENLF(K).LT.100) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I2,1H.,I1,1H))') LENLF(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I2,1H.,I2,1H))') LENLF(K),NDEC
          ENDIF
        ELSE
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I3,1H.,I1,1H))') LENLF(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I3,1H.,I2,1H))') LENLF(K),NDEC
          ENDIF
        ENDIF
C
        READ(LFACT(K),FMT) RFACT
        DO I=1,LENLB(K)
          IF(LBASE(K)(I:I).EQ.'.') THEN
            NDEC=LENLB(K)-I
          ENDIF
        ENDDO
        IF(LENLB(K).LT.10) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I1,1H.,I1,1H))') LENLB(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I1,1H.,I2,1H))') LENLB(K),NDEC
          ENDIF
        ELSEIF(LENLB(K).LT.100) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I2,1H.,I1,1H))') LENLB(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I2,1H.,I2,1H))') LENLB(K),NDEC
          ENDIF
        ELSE
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I3,1H.,I1,1H))') LENLB(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I3,1H.,I2,1H))') LENLB(K),NDEC
          ENDIF
        ENDIF
C
        READ(LBASE(K),FMT) RBASE
C
        IF(LEVLR(K)(1:5).EQ.'layer') THEN
          NN=KPDS(7,N)/2**8
          TOPL=NN*RFACT+RBASE
          BTML=(KPDS(7,N)-NN*2**8)*RFACT+RBASE
        ELSEIF(LEVLR(K)(1:5).EQ.'level') THEN
          TOPL=KPDS(7,N)*RFACT+RBASE
          BTML=KPDS(7,N)*RFACT+RBASE
        ELSEIF(LEVLR(K)(1:6).EQ.'slevel') THEN
  		    IF(LTYPE(K)(1:7).EQ.'surface') THEN
            TOPL=0.
            BTML=0.
          ELSE
            TOPL=KPDS(7,N)*RFACT+RBASE
            BTML=KPDS(7,N)*RFACT+RBASE
          ENDIF
        ELSE
           WRITE(LUPTR,*) ' FATAL ERROR! KPDS(6,N)=',
     1             KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' ILLEGAL LEVEL TYPE ENCOUNTERED ',
     1             'IN THE FILE KPDS(6) TABLE FILE'
           WRITE(LUPTR,*) ' LEVLR(K)=',LEVLR(K)
           PRINT *,' FATAL ERROR! KPDS(6,N)=',
     1             KPDS(6,N),' FOR N=',N
           PRINT *,' ILLEGAL LEVEL TYPE ENCOUNTERED ',
     1             'IN THE FILE KPDS(6) TABLE FILE'
           PRINT *,' LEVLR(K)=',LEVLR(K)
  		     CALL ABORT
		    ENDIF
C
        RLEV(N)=BTML
C
        IF(FLOAT(INT(TOPL)).EQ.TOPL) THEN
          WRITE(LTOPL,'(I5)') INT(TOPL)
          LENT=5
        ELSE
          WRITE(LTOPL,'(F8.2)') TOPL
          LENT=8
        ENDIF
        IF(FLOAT(INT(BTML)).EQ.BTML) THEN
          WRITE(LBTML,'(I5)') INT(BTML)
          LENB=5
        ELSE
          WRITE(LBTML,'(F8.2)') BTML
          LENB=8
        ENDIF
C
        LABEL(2,N)=LINFO(K)(1:LENLI(K))
        LENLA(2,N)=LENLI(K)
C
  		  IF(LEVLR(K)(1:5).EQ.'level'.OR.LEVLR(K)(1:6).EQ.'slevel') THEN
  		    IF(LTYPE(K)(1:7).EQ.'surface'.OR.LTYPE(K)(1:1).EQ.'-') THEN
            LABEL(3,N)=' '
            LENLA(3,N)=1
	  	    ELSE
            LABEL(3,N)=LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
            LENLA(3,N)=LENT+LENLU(K)
		  		ENDIF
		 	  ELSEIF(LEVLR(K)(1:5).EQ.'layer') THEN
          LABEL(3,N)=LBTML(1:LENB)//LUNIT(K)(1:LENLU(K))//' and '//
     1               LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
          LENLA(3,N)=LENB+LENLU(K)+5+LENT+LENLU(K)
        ENDIF
C
        LVABBR(N)=LEVAB(K)
C
  100 CONTINUE
C
      RETURN
			END
      SUBROUTINE RMBLNK(LABEL,LENLA,N)
C
C  Get rid of two or more consecutive blanks here
C
      CHARACTER*64 LABEL(N)
      DIMENSION LENLA(N)
      CHARACTER*160 LABEX
C
      DO II=1,N
        JJ=0
        DO J=2,LENLA(II)
          IF(LABEL(II)(J-1:J-1).NE.' '.OR.LABEL(II)(J:J).NE.' ') THEN
	          JJ=JJ+1
            LABEX(JJ:JJ)=LABEL(II)(J-1:J-1)
          ENDIF
          IF(JJ.GT.1) THEN
            IF(LABEX(JJ-1:JJ-1).EQ.'='.AND.LABEX(JJ:JJ).EQ.' ') THEN
              JJ=JJ-1
            ENDIF
          ENDIF
        ENDDO
C
        IF(LABEL(II)(LENLA(II):LENLA(II)).NE.' ') THEN
  	    	JJ=JJ+1
          LABEX(JJ:JJ)=LABEL(II)(LENLA(II):LENLA(II))
        ENDIF
C
        IF(JJ.LE.0) THEN
          LENLA(II)=1
          LABEL(II)(1:1)=' '
        ELSE
          LENLA(II)=JJ
          LABEL(II)(1:LENLA(II))=LABEX(1:LENLA(II))
        ENDIF
C
      ENDDO
C
      RETURN
      END
      SUBROUTINE FLSRCH(NTOTL,LUPTR)
C
      PARAMETER(MTOTL=500)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*8  LVABBR
      LOGICAL LLEVL,LLTYP
      COMMON/COMCTL/LABEL(3,MTOTL),LENLA(3,MTOTL),LABBR(MTOTL),
     1              LVABBR(MTOTL),RLEV(MTOTL),
     2              ILINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     4              NLTOTL(MTOTL),NVTOTL,NPTOTL,LLEVL,LLTYP
      CHARACTER*64 LABTMP
C
      LOGICAL LFP(MTOTL),LPFND
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
C  Modify variable name if same variable name but with differenct level types
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      DO N=1,NTOTL
        IF(.NOT.LFP(N)) THEN
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M)) THEN
              IF(LABBR(N).EQ.LABBR(M).AND.
     1           LABEL(2,N)(1:LENLA(2,N)).NE.
     2           LABEL(2,M)(1:LENLA(2,M))) THEN
                J=0
                DO I=1,5
                  IF(LABBR(M)(I:I).NE.' ') THEN
                    J=J+1
                    LABTMP(J:J)=LABBR(M)(I:I)
                  ENDIF
                ENDDO
                LABBR(M)(1:J+3)=LABTMP(1:J)//LVABBR(M)(1:3)
                DO I=J+4,8
                  LABBR(M)(I:I)=' '
                ENDDO
                WRITE(LUPTR,*) 'Variable "',LABTMP(1:J),'" on ',
     1                        LABEL(2,M)(1:LENLA(2,M)),' modified to "',
     2                        LABBR(M)(1:J+3),'"'
                LFP(M)=.TRUE.
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C  Find number of variables 
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NVTOTL=0
      NN=1
      NV=1
      DO N=1,NTOTL
        IF(.NOT.LFP(N)) THEN
          NVTOTL=NVTOTL+1
          IVINDX(NVTOTL)=N
          LFP(N)=.TRUE.
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.
     1          LABBR(N).EQ.LABBR(M)) THEN
              LFP(M)=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NVTOTL=',NVTOTL
        DO K=1,NVTOTL
          WRITE(LUPTR,*) LABBR(IVINDX(K))
        ENDDO
      ENDIF
C
C  Find number of levels for each variable
C
      LLEVL=.FALSE.
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NNN=0
      DO NV=1,NVTOTL
        NVL=IVINDX(NV)
        NLTOTL(NV)=0
        DO N=1,NTOTL
          IF(.NOT.LFP(N).AND.
     1                       LABEL(1,N)(1:LENLA(1,N)).EQ.
     2                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     3                       LABBR(N).EQ.LABBR(NVL)) THEN
            NNN=NNN+1
            NLTOTL(NV)=NLTOTL(NV)+1
            ILINDX(NNN)=N
            LFP(N)=.TRUE.
            DO M=N+1,NTOTL
              IF(.NOT.LFP(M).AND.
     1                       LABEL(1,M)(1:LENLA(1,M)).EQ.
     1                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     2                       LABBR(M).EQ.LABBR(NVL).AND.
     3                       LABEL(3,M)(1:LENLA(3,M)).EQ.
     4                       LABEL(3,N)(1:LENLA(3,N))) THEN
                  LFP(M)=.TRUE.
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NLTOTL(NV)=',(NLTOTL(NV),NV=1,NVTOTL)
        NNN=0
        DO NV=1,NVTOTL
          WRITE(LUPTR,*) (LABEL(3,ILINDX(K+NNN))
     1                   (1:LENLA(3,ILINDX(K+NNN))),',',K=1,NLTOTL(NV))
          NNN=NNN+NLTOTL(NV)
        ENDDO
      ENDIF
      LLEVL=.FALSE.
      DO NV=1,NVTOTL
        IF(NLTOTL(NV).GT.1) THEN
          LLEVL=.TRUE.
          GO TO 101
        ENDIF
      ENDDO
  101 CONTINUE
C
C  Find number of multi-level level types
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NPTOTL=0
      NN=0
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LENLA(3,N).GT.1) THEN
          NPTOTL=NPTOTL+1
          LFP(N)=.TRUE.
          IPINDX(NPTOTL)=N
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.
     1                         LABEL(2,N)(1:LENLA(2,N)).EQ.
     2                         LABEL(2,M)(1:LENLA(2,M))) THEN
              LFP(M)=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      IF(NPTOTL.GT.1) THEN
        LLTYP=.TRUE.
      ELSE
        LLTYP=.FALSE.
      ENDIF
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NPTOTL=',NPTOTL
        DO K=1,NPTOTL
          WRITE(LUPTR,*) LABEL(2,IPINDX(K))(1:LENLA(2,IPINDX(K)))
        ENDDO
      ENDIF
C
      WRITE(LUPTR,*) ' '
      IF(LLEVL) THEN
        WRITE(LUPTR,*) ' Multi-level fields found in a given file'
        WRITE(LUPTR,*) ' Number of multi-level fields=',NLTOTL
      ENDIF
      IF(LLTYP) THEN
        WRITE(LUPTR,*) ' Same variable with different level type found',
     1                 ' in a given file'
        WRITE(LUPTR,*) ' Number of such fields=',NPTOTL
      ENDIF
C
      RETURN
      END
      SUBROUTINE WRYTE(LU,LC,C)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRYTE       WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: EFFICIENTLY WRITE UNFORMATTED A CHARACETER ARRAY.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL WRYTE(LU,LC,C)
C
C   INPUT ARGUMENT LIST:
C     LU       - INTEGER UNIT TO WHICH TO WRITE
C     LC       - INTEGER NUMBER OF CHARACTERS OR BYTES TO WRITE
C     C        - CHARACETER (LC) DATA TO WRITE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      CHARACTER C(LC)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      WRITE(LU) C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE PUTGB(LUGB,KF,KPDS,KGDS,LB,F,IBM,FR,GRIB,LGRIB,
     1                 LUPTR,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C SUBPROGRAM: PUTGB          PACKS AND WRITES A GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
C
C ABSTRACT: PACK AND WRITE A GRIB MESSAGE.
C   THIS SUBPROGRAM IS NEARLY THE INVERSE OF GETGB.
C
C PROGRAM HISTORY LOG:
C   94-04-01  IREDELL
C
C USAGE:    CALL PUTGB(LUGB,KF,KPDS,KGDS,LB,F,IBM,FR,GRIB,IRET)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE
C     KF           INTEGER NUMBER OF DATA POINTS
C     KPDS         INTEGER (25) PDS PARAMETERS
C     KGDS         INTEGER (22) GDS PARAMETERS
C     LB           LOGICAL (KF) BITMAP IF PRESENT
C     F            REAL (KF) DATA
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    OTHER  W3FI72 GRIB PACKER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   R63W72         MAP W3FI63 PARAMETERS ONTO W3FI72 PARAMETERS
C   GTBITS         GET NUMBER OF BITS AND ROUND DATA
C   W3FI72         PACK GRIB
C   WRYTE          WRITE DATA
C
C ATTRIBUTES:
C   LANGUAGE: F77 FORTRAN
C
C$$$
      INTEGER KPDS(25),KGDS(22)
      LOGICAL*1 LB(KF)
      REAL F(KF)
      INTEGER IBM(KF),IPDS(25),IGDS(18),IBDS(9)
      CHARACTER PDS(28)
C
C     CHARACTER GRIB(200+17*KF/8)
      CHARACTER*1 GRIB(*)
C
      REAL*8 FR(KF),FMAX,FMIN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET W3FI72 PARAMETERS
      CALL R63W72(KPDS,KGDS,IPDS,IGDS)
      DO I=1,9
      IBDS(I)=0
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COUNT VALID DATA
      KBM=KF
C     WRITE(LUPTR,*) 'IPDS(7)=',IPDS(7)
      IF(IPDS(7).NE.0) THEN
        KBM=0
        DO I=1,KF
          IF(LB(I)) THEN
            IBM(I)=1
            KBM=KBM+1
          ELSE
            IBM(I)=0
          ENDIF
        ENDDO
        IF(KBM.EQ.KF) IPDS(7)=0
      ENDIF
C     WRITE(LUPTR,*) 'MODIFIED IPDS(7)=',IPDS(7)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET NUMBER OF BITS AND ROUND DATA
      IF(KBM.EQ.0) THEN
        DO I=1,KF
          FR(I)=0.
        ENDDO
        NBIT=0
      ELSE
        CALL GTBITS(IPDS(7),IPDS(25),KF,IBM,F,FR,FMIN,FMAX,NBIT)
        NBIT=MIN(NBIT,16)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PACK AND WRITE GRIB DATA
C
C  FR IS REAL*8
C
      CALL W3FI68(IPDS,PDS)
C
      WRITE(LUPTR,*) 'IPDS(6)=',IPDS(6),' IPDS(7)=',IPDS(7)
      WRITE(LUPTR,*) 'NBIT=',NBIT,' KBM=',KBM
C
C     CALL W3FI72(0,FR,0,NBIT,0,IPDS,PDS,
      CALL W3FI72(0,FR,0,NBIT,1,IPDS,PDS,
     &            1,255,IGDS,0,0,IBM,KF,IBDS,
     &            KFO,GRIB,LGRIB,IRET)
      IF(IRET.EQ.0) CALL WRYTE(LUGB,LGRIB,GRIB)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
CFPP$ NOCONCUR R
      SUBROUTINE GTBITS(IBM,IDS,LEN,MG,G,GROUND,GMIN,GMAX,NBIT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GTBITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
C
C ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
C   AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
C   THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
C   THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
C   GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C
C USAGE:    CALL GTBITS(IBM,IDS,LEN,MG,G,GMIN,GMAX,NBIT)
C   INPUT ARGUMENT LIST:
C     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
C     IDS      - INTEGER DECIMAL SCALING
C                (E.G. IDS=3 TO ROUND FIELD TO NEAREST MILLI-VALUE)
C     LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
C     MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
C     G        - REAL (LEN) FIELD
C
C   OUTPUT ARGUMENT LIST:
C     GROUND   - REAL*8 (LEN) FIELD ROUNDED TO DECIMAL SCALING
C     GMIN     - REAL*8 MINIMUM VALID ROUNDED FIELD VALUE
C     GMAX     - REAL*8 MAXIMUM VALID ROUNDED FIELD VALUE
C     NBIT     - INTEGER NUMBER OF BITS TO PACK
C
C SUBPROGRAMS CALLED:
C   ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      DIMENSION MG(LEN),G(LEN)
      REAL*8 GROUND(LEN),GMAX,GMIN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      XMX=ABS(G(1))
      DO I=2,LEN
        XMX=MAX(XMX,ABS(G(I)))
      ENDDO
      RMAX=2.**31
      DS=10.**IDS
      RXMX=XMX*DS
      DO WHILE(RXMX.GT.RMAX)
        IDS=IDS-1
        DS=10.**IDS
        RXMX=XMX*DS
      ENDDO
      IF(IBM.EQ.0) THEN
        GROUND(1)=NINT(G(1)*DS)/DS
        GMAX=GROUND(1)
        GMIN=GROUND(1)
        DO I=2,LEN
          GROUND(I)=NINT(G(I)*DS)/DS
          GMAX=MAX(GMAX,GROUND(I))
          GMIN=MIN(GMIN,GROUND(I))
        ENDDO
      ELSE
        I1=ISRCHNE(LEN,MG,1,0)
        IF(I1.GT.0.AND.I1.LE.LEN) THEN
          GROUND(I1)=NINT(G(I1)*DS)/DS
          GMAX=GROUND(I1)
          GMIN=GROUND(I1)
          DO I=1,I1-1
            GROUND(I)=0.
          ENDDO
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GROUND(I)=NINT(G(I)*DS)/DS
              GMAX=MAX(GMAX,GROUND(I))
              GMIN=MIN(GMIN,GROUND(I))
            ELSE
              GROUND(I)=0.
            ENDIF
          ENDDO
        ELSE
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF BITS
      NBIT=LOG((GMAX-GMIN)*DS+0.9)/LOG(2.)+1.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE INCDTE(IY,IM,ID,IH,JY,JM,JD,JH,INCHR)
C
C  Compute JY,JM,JD,JH of forecast
C
      JY=IY
      JM=IM
      JD=ID
      INCDY=INCHR/24
      JH=IH+MOD(INCHR,24)
      INCDY=INCDY+JH/24
      JH=MOD(JH,24)
      DO INCD=1,INCDY
        JD=JD+1
        IF(JM.EQ.4.OR.JM.EQ.6.OR.JM.EQ.9.OR.JM.EQ.11) THEN
          MONDY=30
        ELSEIF(JM.EQ.2) THEN
          IF(MOD(JY,4).EQ.0) THEN
            MODNY=29
          ELSE
            MONDY=28
          ENDIF
        ELSE
          MONDY=31
        ENDIF
        IF(JD.GT.MONDY) THEN
          JM=JM+1
          JD=1
          IF(JM.GT.12) THEN
            JY=JY+1
            JM=1
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
      SUBROUTINE MAXMIN(F,IDIM,JDIM,IMAX,JMAX,KMAX,LUPTR)
C
      DIMENSION F(IDIM,JDIM,KMAX)
C
      DO 10 K=1,KMAX
C
      FMAX=F(1,1,K)
      FMIN=F(1,1,K)
C
      DO 20 J=1,JMAX
      DO 20 I=1,IMAX
      IF(FMAX.LE.F(I,J,K)) THEN
      FMAX=F(I,J,K)
      IIMAX=I
      JJMAX=J
      ENDIF
      IF(FMIN.GE.F(I,J,K)) THEN
      FMIN=F(I,J,K)
      IIMIN=I
      JJMIN=J
      ENDIF
   20 CONTINUE
C
      WRITE(LUPTR,100) K,FMAX,IIMAX,JJMAX,FMIN,IIMIN,JJMIN
  100 FORMAT(2X,'LEVEL=',I2,' MAX=',E10.4,' AT I=',I5,' J=',I5,
     1                      ' MIN=',E10.4,' AT I=',I5,' J=',I5)
C
   10 CONTINUE
C
      RETURN
      END
      FUNCTION ISRCHNE(N,X,INCX,TARGET)
      INTEGER X(*), TARGET
      J=1
      ISRCHNE=0
      IF(N.LE.0) RETURN
      IF(INCX.LT.0) J=1-(N-1)*INCX
      DO I=1,N
        IF(X(J).NE.TARGET) THEN
          ISRCHNE=I
          RETURN
        ENDIF
        J=J+INCX
      ENDDO
      RETURN
      END
      SUBROUTINE R63W72(KPDS,KGDS,IPDS,IGDS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    R63W72      CONVERT W3FI63 PARMS TO W3FI72 PARMS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: DETERMINES THE INTEGER PDS AND GDS PARAMETERS
C           FOR THE GRIB1 PACKING ROUTINE W3FI72 GIVEN THE PARAMETERS
C           RETURNED FROM THE GRIB1 UNPACKING ROUTINE W3FI63.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C   96-05-03  MARK IREDELL  CORRECTED SOME LEVEL TYPES AND
C                           SOME DATA REPRESENTATION TYPES
C   97-02-14  MARK IREDELL  ONLY ALTERED IPDS(26:27) FOR EXTENDED PDS
C   98-06-01  CHRIS CARUSO  Y2K FIX FOR YEAR OF CENTURY
C
C USAGE:    CALL R63W72(KPDS,KGDS,IPDS,IGDS)
C
C   INPUT ARGUMENT LIST:
C     KPDS     - INTEGER (200) PDS PARAMETERS FROM W3FI63
C     KGDS     - INTEGER (200) GDS PARAMETERS FROM W3FI63
C
C   OUTPUT ARGUMENT LIST:
C     IPDS     - INTEGER (200) PDS PARAMETERS FOR W3FI72
C     IGDS     - INTEGER (200) GDS PARAMETERS FOR W3FI72
C
C REMARKS: KGDS AND IGDS EXTEND BEYOND THEIR DIMENSIONS HERE
C          IF PL PARAMETERS ARE PRESENT.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      DIMENSION KPDS(200),KGDS(200),IPDS(200),IGDS(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE PRODUCT DEFINITION SECTION (PDS) PARAMETERS
      IF(KPDS(23).NE.2) THEN
        IPDS(1)=28                      ! LENGTH OF PDS
      ELSE
        IPDS(1)=45                      ! LENGTH OF PDS
      ENDIF
      IPDS(2)=KPDS(19)                  ! PARAMETER TABLE VERSION
      IPDS(3)=KPDS(1)                   ! ORIGINATING CENTER
      IPDS(4)=KPDS(2)                   ! GENERATING MODEL
      IPDS(5)=KPDS(3)                   ! GRID DEFINITION
      IPDS(6)=MOD(KPDS(4)/128,2)        ! GDS FLAG
      IPDS(7)=MOD(KPDS(4)/64,2)         ! BMS FLAG
      IPDS(8)=KPDS(5)                   ! PARAMETER INDICATOR
      IPDS(9)=KPDS(6)                   ! LEVEL TYPE
      IF(KPDS(6).EQ.101.OR.KPDS(6).EQ.104.OR.KPDS(6).EQ.106.OR.
     &   KPDS(6).EQ.108.OR.KPDS(6).EQ.110.OR.KPDS(6).EQ.112.OR.
     &   KPDS(6).EQ.114.OR.KPDS(6).EQ.116.OR.KPDS(6).EQ.121.OR.
     &   KPDS(6).EQ.128.OR.KPDS(6).EQ.141)  THEN
        IPDS(10)=MOD(KPDS(7)/256,256)   ! LEVEL VALUE 1
        IPDS(11)=MOD(KPDS(7),256)       ! LEVEL VALUE 2
      ELSE
        IPDS(10)=0                      ! LEVEL VALUE 1
        IPDS(11)=KPDS(7)                ! LEVEL VALUE 2
      ENDIF
      IPDS(12)=KPDS(8)                  ! YEAR OF CENTURY
      IPDS(13)=KPDS(9)                  ! MONTH
      IPDS(14)=KPDS(10)                 ! DAY
      IPDS(15)=KPDS(11)                 ! HOUR
      IPDS(16)=KPDS(12)                 ! MINUTE
      IPDS(17)=KPDS(13)                 ! FORECAST TIME UNIT
      IPDS(18)=KPDS(14)                 ! TIME RANGE 1
      IPDS(19)=KPDS(15)                 ! TIME RANGE 2
      IPDS(20)=KPDS(16)                 ! TIME RANGE INDICATOR
      IPDS(21)=KPDS(17)                 ! NUMBER IN AVERAGE
      IPDS(22)=KPDS(20)                 ! NUMBER MISSING IN AVERAGE
      IPDS(23)=KPDS(21)                 ! CENTURY
      IPDS(24)=KPDS(23)                 ! SUBCENTER
      IPDS(25)=KPDS(22)                 ! DECIMAL SCALING
      IF(IPDS(1).GT.28) THEN
        IPDS(26)=0                      ! PDS BYTE 29
        IPDS(27)=0                      ! PDS BYTE 30
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE GRID DEFINITION SECTION (GDS) PARAMETERS
      IGDS(1)=KGDS(19)                  ! NUMBER OF VERTICAL COORDINATES
      IGDS(2)=KGDS(20)                  ! VERTICAL COORDINATES
      IGDS(3)=KGDS(1)                   ! DATA REPRESENTATION
      IGDS(4)=KGDS(2)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(5)=KGDS(3)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(6)=KGDS(4)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(7)=KGDS(5)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(8)=KGDS(6)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(9)=KGDS(7)                   ! (UNIQUE TO REPRESENTATION)
      IGDS(10)=KGDS(8)                  ! (UNIQUE TO REPRESENTATION)
      IGDS(11)=KGDS(9)                  ! (UNIQUE TO REPRESENTATION)
      IGDS(12)=KGDS(10)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(13)=KGDS(11)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(14)=KGDS(12)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(15)=KGDS(13)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(16)=KGDS(14)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(17)=KGDS(15)                 ! (UNIQUE TO REPRESENTATION)
      IGDS(18)=KGDS(16)                 ! (UNIQUE TO REPRESENTATION)
C  EXCEPTIONS FOR LATLON OR GAUSSIAN
      IF(KGDS(1).EQ.0.OR.KGDS(1).EQ.4) THEN
        IGDS(11)=KGDS(10)
        IGDS(12)=KGDS(9)
C  EXCEPTIONS FOR MERCATOR
      ELSEIF(KGDS(1).EQ.1) THEN
        IGDS(11)=KGDS(13)
        IGDS(12)=KGDS(12)
        IGDS(13)=KGDS(9)
        IGDS(14)=KGDS(11)
C  EXCEPTIONS FOR LAMBERT CONFORMAL
      ELSEIF(KGDS(1).EQ.3) THEN
        IGDS(15)=KGDS(12)
        IGDS(16)=KGDS(13)
        IGDS(17)=KGDS(14)
        IGDS(18)=KGDS(15)
      ENDIF
C  EXTENSION FOR PL PARAMETERS
      IF(KGDS(1).EQ.0.AND.KGDS(19).EQ.0.AND.KGDS(20).NE.255) THEN
        DO J=1,KGDS(3)
          IGDS(18+J)=KGDS(21+J)
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
