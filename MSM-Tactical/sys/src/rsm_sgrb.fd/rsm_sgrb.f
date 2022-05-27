      PROGRAM RSMSGRB
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
!                .      .    .                                       .
! MAIN PROGRAM:  RSMSGRB 
!   PRGMMR: HANN-MING HENRY JUANG    ORG: W/NP20     DATE: 93-03-03            
!                                                                       
! ABSTRACT: PROGRAM TRANSFORMS SIGMA BINARY INPUT TO GRIB OUTPUT.     
!   THE OUTPUT CONSISTS OF DATA ON A REGULAR MODEL MAPPING GRID.              
!   GEOPOTENTIAL HEIGHT, WIND COMPONENTS, RELATIVE HUMIDITY,MPERATURE
!   AND VERTICAL VELOCITY ARE OUTPUT ON MANDATORY PRESSURES.
!   ALSO OUTPUT ARE SUNDRY FIELDS CONSISTING OF                         
!   PRECIPITABLE WATER, THREE LOWER LEVEL RELATIVE HUMIDITIES,          
!   LOWER LEVEL POTENTIAL TEMPERATURE AND WIND COMPONENTS,              
!   SURFACE TEMPERATURE, PRESSURE, OMEGA AND RELATIVE HUMIDITY,         
!   AND TROPOPAUSE TEMPERATURE, PRESSURE, WIND COMPONENTS AND SHEAR.    
!   FIRST NAMPGB NAMELIST IS READ TO DETERMINE OUTPUT FORMAT.           
!   THEN A SIGMA (GRID OR SPECTRAL) FILE IS READ FROM UNIT 11 AND       
!   THE PROGRAM PRODUCES AND WRITES A PRESSURE GRIB1 FILE TO UNIT 51    
!   AND A CORRESPONDING GRIB INDEX FILE TO UNIT 71.                     
!   THEN A SIGMA FILE IS READ FROM UNIT 12 AND                          
!   THE PROGRAM PRODUCES AND WRITES A PRESSURE GRIB1 FILE TO UNIT 52    
!   AND A CORRESPONDING GRIB INDEX FILE TO UNIT 72.                     
!   THE PROGRAM CONTINUES UNTIL AN EMPTY INPUT FILE IS ENCOUNTERED.     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   93-03-03  H.-M.H.JUANG
!   96-10-10  H.-M.H.JUANG ADD GRADS CONTROL FILE
!   98-04-06  H.-M.H.JUANG CHANGE FOR DYNAMIC MEMORY USE
!   98-04-21  H.-M.H.JUANG MODIFY OUTPUT YEAR AND CENTURY
!                                                                       
! NAMELISTS:                                                            
!                                                                       
! INPUT FILES:                                                          
!   UNIT   11  SIGMA FILE                                          
!                                                                       
! OUTPUT FILES:                                                         
!   UNIT   51  SIGMA GRIB FILE                                 
!   UNIT   61  GRADS CONTROL FILE
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   IDSDEF       SET DEFAULTS FOR DECIMAL SCALING                       
!   RSGB1        TRANSFORM ONE SIGMA FILE TO SIGMA GRIB              
!   W3LIB    - W3TAGB W3TAGE
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77
!   MACHINE:	CRAY
!
!$$$
      PARAMETER(LEVMAX=100,NWHDR=512,NWEXT=NWHDR-(6+2*LEVMAX))
      PARAMETER(ICEN=7,IGEN=99)
      CHARACTER*32 CLABA,CLABE
      DIMENSION IDATE(4),SISL(2*LEVMAX+1),EXT(NWEXT)
      DIMENSION IDS(255)
!
      CALL W3TAGB('RSMSGRB ',1998,0007,0050,'NP20   ')                  
!
      CALL IDSDEF(1,IDS)
      NCPUS=4
      MXBIT=16
      NSIG=11
      NSGB=51
      NCTL=61
      READ(NSIG)
      READ(NSIG) FHOUR,IDATE,SISL,EXT
      IWAV1  =EXT(1)
      JWAV1  =EXT(2)
      IGRD1  =EXT(3)
      JGRD1  =EXT(4)
      LEVR   =EXT(5)
      NFIELX =EXT(6)
      PROJ   =EXT(7)
      TRUTH  =EXT(8)
      ORIENT =EXT(9)
      CENLAT =EXT(10)
      CENLON =EXT(11)
      GRDLEFT=EXT(12)
      GRDBOTM=EXT(13)
      DELX   =EXT(14)
      DELY   =EXT(15)
      NONHYD =EXT(16)
      NCLOUD =EXT(31)
      write(*,*) 'NONHYD= ',NONHYD
!
      JCAP=SQRT((IWAV1-1)**2.+(JWAV1-1)**2.)
      IGRD11=IGRD1/4
      IF(MOD(IGRD1,4).EQ.0) THEN
         IGRD11=IGRD11*4
      ELSE
         IGRD11=(IGRD11+1)*4
      ENDIF
!
      NFLDS=12*LEVR+7-1
      CALL RSGB1(FHOUR,IDATE,NSIG,JCAP,IWAV1,JWAV1,                      &              
     &           IGRD1,IGRD11,JGRD1,LEVR,NFLDS,SISL(LEVR+2),             & 
     &           PROJ,TRUTH,ORIENT,CENLAT,CENLON,                        &
     &           GRDLEFT,GRDBOTM,DELX,DELY,                              &
     &           NSGB,NCTL,NCPUS,MXBIT,ICEN,IGEN,IDS,NONHYD,NCLOUD)
!
      CALL W3TAGE('RSMSGRB ')                                           
!
      STOP
      END
      SUBROUTINE RSGB1(FHOUR,IDATE,NSIG,JCAP,IWAV1,JWAV1,               &
     &           IGRD1,IGRD11,JGRD1,LEVR,NFLDS,SL,                      &
     &           PROJ,TRUTH,ORIENT,CENLAT,CENLON,                       &
     &           GRDLEFT,GRDBOTM,DELX,DELY,                             &
     &           NSGB,NCTL,NCPUS,MXBIT,ICEN,IGEN,IDS,NONHYD,NCLOUD)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM: RSGB1           TRANSFORMS A SIGMA BINARY TO SIGMA GRIB       
!   PRGMMR: H. JUANG          ORG: W/NMC23     DATE: 96-03-31            
!                                                                       
! ABSTRACT: TRANSFORMS A SIGMA GRID BINARY FILE TO SIGMA GRIB FILE.
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   96-10-10  H.-M.H.JUANG ADD GRADS CONTROL FILE
!                                                                       
! USAGE:    CALL RSGB1(FHOUR,IDATE,NSIG,JCAP,IWAV1,JWAV1,
!    &           IGRD1,IGRD11,JGRD1,LEVR,NFLDS,SL,
!    &           DELX,DELY,TRUTH,ORIENT,PROJ,GRDLEFT,GRDBOTM,
!    &           NSGB,NCTL,NCPUS,MXBIT,ICEN,IGEN,IDS)
!   INPUT ARGUMENTS:                                                    
!     FHOUR        REAL FORECAST HOUR                                   
!     IDATE        INTEGER (4) DATE                                     
!     NSIG         INTEGER UNIT FROM WHICH TO READ SIGMA FILE           
!     JCAP         INTEGER SPECTRAL TRUNCATION                          
!     IWAV1        INTEGER NUMBER OF SPECTRAL COEFFICIENTS IN I         
!     JWAV1        INTEGER NUMBER OF SPECTRAL COEFFICIENTS IN J         
!     IGRD1        INTEGER NUMBER OF GAUSSIAN LONGITUDES                
!     IGRD11       INTEGER I DIMENSION FOR PACK, FACTOR OF 4            
!     JGRD1        INTEGER HALF THE NUMBER OF GAUSSIAN LATITUDES        
!     LEVR         INTEGER NUMBER OF SIGMA LEVELS                       
!     NFLDS        INTEGER TOTAL NUMBER OF INPUT HORIZONTAL FIELDS      
!     SL           REAL (LEVS) SIGMA FULL LEVEL VALUES                  
!     DELX         REAL GRID SPACING IN X DIRECTION
!     DELY         REAL GRID SPACING IN Y DIRECTION
!     TRUTH        REAL TRUTH LATITUDE FOR MAP PROJECTION
!     ORIENT       REAL ORIENTATION IN LONGITUDE FOR MAP PROJECTION
!     PROJ         REAL MAP PROJECTION
!     GRDLEFT      REAL GRID NUMBER WITHIN (TRUTH,ORIENT) AND WEST BOUNDARY
!     GRDBOTM      REAL GRID NUMBER WITHIN (TRUTH,ORIENT) AND SOUTH BOUNDARY
!     NSGB         INTEGER UNIT TO WRITE SIGMA GRIB FILE
!     NCTL         INTEGER UNIT TO WRITE GRADS CONTROL FILE
!     NCPUS        INTEGER NUMBER OF CPUS OVER WHICH TO DISTRIBUTE WORK 
!     MXBIT        INTEGER MAXIMUM NUMBER OF BITS TO PACK DATA          
!     ICEN         INTEGER FORECAST CENTER IDENTIFIER                   
!     IGEN         INTEGER GENERATING MODEL IDENTIFIER                  
!     IDS          INTEGER (255) DECIMAL SCALING                        
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   RRDSGR       READ SIGMA GRIDDED DATA                                
!   GRIBIT       CREATE GRIB MESSAGE                                    
!   WRYTE        WRITE DATA BY BYTES                                    
!   CTLHEAD      WRITE HEAD RECORD FOR GRADS CONTROL
!   CTLVAR       WRITE EACH RECORD FOR GRADS CONTROL
!   CTLEND       WRITE END  RECORD FOR GRADS CONTROL
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION IDS(255),IDATE(4),SL(LEVR)
      DIMENSION LGRIB(NCPUS)
      DIMENSION CLAT(IGRD1,JGRD1),FLAT(JGRD1),IZ(LEVR)
      DIMENSION F(IGRD1,NFLDS,JGRD1),G(IGRD1,JGRD1,NFLDS)
      PARAMETER(IPUT=11,IPUU=33,IPUV=34,IPUVOR=43,IPUDIV=44,IPUQ=51)
      PARAMETER(IPUTH=24,IPUTV=12,IPUTN=11,IPUPN=1,IPUWN=40)
      PARAMETER(IPUQC=191,IPUQR=192)
      PARAMETER(IPUPW=39)
      PARAMETER(IPUP=1,IPUZ=7,IPUPX=181,IPUPY=182,IPUZX=183,IPUZY=184)
      PARAMETER(ISFC=1,ISGLEV=107)
      DIMENSION IPU(NFLDS),ITL(NFLDS),IL2(NFLDS)
      PARAMETER(LENPDS=28,LENGDS=32)
      CHARACTER GRIB(30+LENPDS+LENGDS+IGRD1*JGRD1*(MXBIT+1)/8,NCPUS)
      COMMON/IOINDX/ KSZ,KSD,KST,KSQ,KSPSX,KSPSY,KSU,KSV,KSPS,KSZS,      &               
     &               KSZSX,KSZSY,KSPW,KSPN,KSTN,KSWN,KSQC,KSQR
! -------------------------------------------------------------------
      CALL CTLINI
      PI=ACOS(-1.)
      JFHOUR=NINT(FHOUR)
      KSZ=1
      KSD=1+LEVR
      KST=1+2*LEVR
      KSQ=1+3*LEVR
      KSPSX=1+4*LEVR
      KSPSY=2+4*LEVR
      KSU=3+4*LEVR
      KSV=3+5*LEVR
      KSPS=3+6*LEVR
      KSZS=4+6*LEVR
      KSZSX=5+6*LEVR
      KSZSY=6+6*LEVR
      KSPW=7+6*LEVR
      KSQC=7+7*LEVR
      KSQR=7+8*LEVR
      KSPN=7+9*LEVR
      KSTN=7+10*LEVR
      KSWN=7+11*LEVR    ! use layers
C     NFLDS=7+12*LEVR-1
      IPUTT=IPUTV
      IF( NONHYD .EQ. 1 ) IPUTT=IPUTH
      DO K=1,LEVR
      IPU(KSU+K-1)=IPUU
      IPU(KSV+K-1)=IPUV
      IPU(KSZ+K-1)=IPUVOR
      IPU(KSD+K-1)=IPUDIV
      IPU(KST+K-1)=IPUTT
      IPU(KSQ+K-1)=IPUQ
      IPU(KSQC+K-1)=IPUQC
      IPU(KSQR+K-1)=IPUQR
      IPU(KSPW+K-1)=IPUPW
      IPU(KSPN+K-1)=IPUPN
      IPU(KSTN+K-1)=IPUTN
      IPU(KSWN+K-1)=IPUWN
      ENDDO
      IPU(KSPS)=IPUP
      IPU(KSZS)=IPUZ
      IPU(KSPSX)=IPUPX
      IPU(KSPSY)=IPUPY
      IPU(KSZSX)=IPUZX
      IPU(KSZSY)=IPUZY
      DO K=1,LEVR
      ITL(KSU+K-1)=ISGLEV
      ITL(KSV+K-1)=ISGLEV
      ITL(KSZ+K-1)=ISGLEV
      ITL(KSD+K-1)=ISGLEV
      ITL(KST+K-1)=ISGLEV
      ITL(KSQ+K-1)=ISGLEV
      ITL(KSQC+K-1)=ISGLEV
      ITL(KSQR+K-1)=ISGLEV
      ITL(KSPW+K-1)=ISGLEV
      ITL(KSPN+K-1)=ISGLEV
      ITL(KSTN+K-1)=ISGLEV
      ITL(KSWN+K-1)=ISGLEV
      ENDDO
      ITL(KSPS)=ISFC
      ITL(KSZS)=ISFC
      ITL(KSPSX)=ISFC
      ITL(KSPSY)=ISFC
      ITL(KSZSX)=ISFC
      ITL(KSZSY)=ISFC
      DO K=1,LEVR
      IL2(KSU+K-1)=NINT(SL(K)*1.E4)
      IL2(KSV+K-1)=NINT(SL(K)*1.E4)
      IL2(KSZ+K-1)=NINT(SL(K)*1.E4)
      IL2(KSD+K-1)=NINT(SL(K)*1.E4)
      IL2(KST+K-1)=NINT(SL(K)*1.E4)
      IL2(KSQ+K-1)=NINT(SL(K)*1.E4)
      IL2(KSQC+K-1)=NINT(SL(K)*1.E4)
      IL2(KSQR+K-1)=NINT(SL(K)*1.E4)
      IL2(KSPW+K-1)=NINT(SL(K)*1.E4)
      IL2(KSPN+K-1)=NINT(SL(K)*1.E4)
      IL2(KSTN+K-1)=NINT(SL(K)*1.E4)
      IL2(KSWN+K-1)=NINT(SL(K)*1.E4)
      ENDDO
      IL2(KSPS)=0
      IL2(KSZS)=0
      IL2(KSPSX)=0
      IL2(KSPSY)=0
      IL2(KSZSX)=0
      IL2(KSZSY)=0
!  LOOP OVER GROUPS OF LATITUDES
      RDELX2=0.5/DELX
      RDELY2=0.5/DELY
!  READ ENTIRE SIGMA FILE
        CALL RRDSGR(NSIG,IGRD1,JGRD1,NFLDS,LEVR,1,JGRD1,CLAT,            &                 
     &           RDELX2,RDELY2,RLAT1,RLON1,RLAT2,RLON2,                  &
     &           NONHYD,NCLOUD,F(1,1,1))
        DO N=1,NFLDS
          DO J=1,JGRD1
            DO I=1,IGRD1
              G(I,J,N)=F(I,N,J)
            ENDDO
          ENDDO
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      NPROJ=PROJ
      IF( NPROJ.EQ. 0 ) THEN
        IDRT=1                  ! MERCATER
        ORTRU=TRUTH
        PRINT *,' MERCATER PROJECTION.'
      ELSEIF( ABS(NPROJ).EQ.1 ) THEN
        IDRT=5                  ! POLAR PROJECTION
        ORTRU=ORIENT
        PRINT *,' POLAR PROJECTIO.'
      ELSEIF( ABS(NPROJ).EQ.2 ) THEN
        IDRT=3                  ! LAMBERT PROJECTION
        ORI=ORIENT
        TRU=TRUTH
        PRINT *,' LAMBERT PROJECTIO.'
      ELSE
        IDRT=0
        PRINT *,' UNDEFINE MAP PROJECTION.'
      ENDIF
!
! WRITE HEAD IN UNIT 61, 62 AND 63
      DX=DELX/1000.
      DY=DELY/1000.
      IFH=FHOUR
      IHR= IDATE(1)
      IDAY=IDATE(3)
      IMON=IDATE(2)
      IYR =IDATE(4)
      STRLON=RLON1*180./ACOS(-1.)
      ENDLON=RLON2*180./ACOS(-1.)
      IF (ENDLON.lt.STRLON) ENDLON=ENDLON+360.  ! add by zyf 20140603
      DLON=(ENDLON-STRLON)/FLOAT(IGRD1-1)
      DO J=1,JGRD1
        FLAT(J)=CLAT(1,J)*180./ACOS(-1.)
      ENDDO
      STRLAT=FLAT(1)
      DLAT=FLAT(2)-FLAT(1)
! SIGCTL
      DO K=1,LEVR
        IZ(K)=NINT(SL(K)*1.E4)
      ENDDO
      CALL CTLHEAD(NCTL,IGRD1,JGRD1,LEVR,PROJ,GRDLEFT,GRDBOTM,ORIENT,DX, &
     &             IHR,IDAY,IMON,IYR,IFH,STRLON,DLON,STRLAT,DLAT,        &
     &             FLAT,IZ)
!
!  LOOP OVER GROUPS OF HORIZONTAL FIELDS
      DO K1=1,NFLDS,NCPUS
        K2=MIN(K1+NCPUS-1,NFLDS)
!  UNPACK TRANSPOSED FIELDS AND INTERPOLATE TO OUTPUT GRID
!  AND ROUND TO THE NUMBER OF BITS AND ENGRIB THE FIELD IN PARALLEL
!MIC$ DO ALL
!MIC$1 SHARED(K1,K2,IDRT,IGRD1,JGRD1,IDELX,IDELY)
!MIC$1 SHARED(ILAT1,ILON1,ILAT2,ILON2,ITRUTH,IORIENT,IPROJ)
!MIC$1 SHARED(IPU,ITL,IL1,IL2,IES,ICEN,ICEN2,IGEN,IPDSX)
!MIC$1 SHARED(IDATE,LENPDS,JFHOUR,IDS,MXBIT)
!MIC$1 SHARED(RLAT1,RLAT2,RLON1,RLON2,PROJ,DELX,DELY,ORTRU)
!MIC$1 SHARED(IPTV,IBMS,IGRID,IFTU,IP2,ITR,INA,INM)
!MIC$1 SHARED(G,GRIB,LGRIB)
!MIC$1 PRIVATE(I,J,K,KAN,IENS,IERR)
        DO K=K1,K2
          KAN=K-K1+1
          LGRIB(KAN)=0
          IF(IPU(K).EQ.IPUP) THEN
            DO J=1,JGRD1
              DO I=1,IGRD1
                G(I,J,K)=G(I,J,K)*1.E3
              ENDDO
            ENDDO
          ENDIF
!MIC$ GUARD
          IPTV=2
          IBMS=0
          IGRID=255
          IL1=0
          IFTU=1
          IP2=0
          ITR=10
          INA=0
          INM=0
          CALL GRIBIT(G(1,1,K),.TRUE.,IDRT,IGRD1,JGRD1,MXBIT,0.,         &               
     &                LENPDS,IPTV,ICEN,IGEN,IBMS,                        &
     &                IPU(K),ITL(K),IL1,IL2(K),                          &
     &                IDATE(4),IDATE(2),IDATE(3),IDATE(1),               &
     &                IFTU,JFHOUR,IP2,ITR,                               &
     &                INA,INM,ICEN2,IDS(IPU(K)),IENS,                    &
     &                RLAT1,RLON1,RLAT2,RLON2,DELX,DELY,ORTRU,PROJ,      &
     &                GRIB(1,KAN),LGRIB(KAN),IERR)
!MIC$ ENDGUARD
        ENDDO
!  WRITE OUT GRIB MESSAGES SEQUENTIALLY
        DO K=K1,K2
          KAN=K-K1+1
          IF(LGRIB(KAN).GT.0) THEN
            CALL WRYTE(NSGB,LGRIB(KAN),GRIB(1,KAN),0)
            PRINT *,' GRIB1 WRITTEN TO ',NSGB,' OF LENGTH ',LGRIB(KAN)
            CALL CTLVAR(NCTL,IPU(K),ITL(K))
          ENDIF
        ENDDO
      ENDDO
! FLUSH OUTPUT
      CALL WRYTE(NSGB,LGRIB(1),GRIB(1,1),1)
!
      CALL CTLVAR(0,0,0)
      CALL CTLEND(NCTL)
      PRINT *,' NORMAL END OF RSGB '
!
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE IDSDEF(IPTV,IDS)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: IDSDEF         SETS DEFAULT DECIMAL SCALINGS
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: SETS DECIMAL SCALINGS DEFAULTS FOR VARIOUS PARAMETERS.
!   A DECIMAL SCALING OF -3 MEANS DATA IS PACKED IN KILO-SI UNITS.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!
! USAGE:    CALL IDSDEF(IPTV,IDS)
!   INPUT ARGUMENTS:
!     IPTV         PARAMTER TABLE VERSION (ONLY 1 OR 2 IS RECOGNIZED)
!   OUTPUT ARGUMENTS:
!     IDS          INTEGER (255) DECIMAL SCALINGS
!                  (UNKNOWN DECIMAL SCALINGS WILL NOT BE SET)
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      DIMENSION IDS(255)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IPTV.EQ.1.OR.IPTV.EQ.2) THEN
        IDS(001)=2      ! PRESSURE (PA)
        IDS(002)=2      ! SEA-LEVEL PRESSURE (PA)
        IDS(003)=5      ! PRESSURE TENDENCY (PA/S)
                        !
                        !
        IDS(006)=2      ! GEOPOTENTIAL (M2/S2)
        IDS(007)=3      ! GEOPOTENTIAL HEIGHT (M)
        IDS(008)=3      ! GEOMETRIC HEIGHT (M)
        IDS(009)=3      ! STANDARD DEVIATION OF HEIGHT (M)
        IDS(010)=3      ! TOTAL OZONE (DOBSON)
        IDS(011)=3      ! TEMPERATURE (K)
        IDS(012)=3      ! VIRTUAL TEMPERATURE (K)
        IDS(013)=3      ! POTENTIAL TEMPERATURE (K)
        IDS(014)=3      ! PSEUDO-ADIABATIC POTENTIAL TEMPERATURE (K)
        IDS(015)=3      ! MAXIMUM TEMPERATURE (K)
        IDS(016)=3      ! MINIMUM TEMPERATURE (K)
        IDS(017)=3      ! DEWPOINT TEMPERATURE (K)
        IDS(018)=3      ! DEWPOINT DEPRESSION (K)
        IDS(019)=6      ! TEMPERATURE LAPSE RATE (K/M)
        IDS(020)=2      ! VISIBILITY (M)
        IDS(021)=2      ! RADAR SPECTRA 1 ()
        IDS(022)=2      ! RADAR SPECTRA 2 ()
        IDS(023)=2      ! RADAR SPECTRA 3 ()
        IDS(024)=3      ! HYDROSTATIC TEMPERATURE (K)
        IDS(025)=3      ! TEMPERATURE ANOMALY (K)
        IDS(026)=2      ! PRESSURE ANOMALY (PA)
        IDS(027)=3      ! GEOPOTENTIAL HEIGHT ANOMALY (M)
        IDS(028)=3      ! WAVE SPECTRA 1 ()
        IDS(029)=3      ! WAVE SPECTRA 2 ()
        IDS(030)=3      ! WAVE SPECTRA 3 ()
        IDS(031)=2      ! WIND DIRECTION (DEGREES)
        IDS(032)=3      ! WIND SPEED (M/S)
        IDS(033)=3      ! ZONAL WIND (M/S)
        IDS(034)=3      ! MERIDIONAL WIND (M/S)
        IDS(035)=-1     ! STREAMFUNCTION (M2/S)
        IDS(036)=-1     ! VELOCITY POTENTIAL (M2/S)
        IDS(037)= 1     ! MONTGOMERY STREAM FUNCTION (M2/S2)
        IDS(038)=9      ! SIGMA VERTICAL VELOCITY (1/S)
        IDS(039)=5      ! PRESSURE VERTICAL VELOCITY (PA/S)
        IDS(040)=6      ! GEOMETRIC VERTICAL VELOCITY (M/S)
        IDS(041)=8      ! ABSOLUTE VORTICITY (1/S)
        IDS(042)=8      ! ABSOLUTE DIVERGENCE (1/S)
        IDS(043)=8      ! RELATIVE VORTICITY (1/S)
        IDS(044)=8      ! RELATIVE DIVERGENCE (1/S)
        IDS(045)=6      ! VERTICAL U SHEAR (1/S)
        IDS(046)=6      ! VERTICAL V SHEAR (1/S)
        IDS(047)=2      ! DIRECTION OF CURRENT (DEGREES)
        IDS(048)=2      ! SPEED OF CURRENT (M/S)
        IDS(049)=2      ! U OF CURRENT (M/S)
        IDS(050)=2      ! V OF CURRENT (M/S)
        IDS(051)=7      ! SPECIFIC HUMIDITY (KG/KG)
        IDS(052)=2      ! RELATIVE HUMIDITY (PERCENT)
        IDS(053)=7      ! HUMIDITY MIXING RATIO (KG/KG)
        IDS(054)=3      ! PRECIPITABLE WATER (KG/M2)
        IDS(055)=2      ! VAPOR PRESSURE (PA)
        IDS(056)=2      ! SATURATION DEFICIT (PA)
        IDS(057)=3      ! EVAPORATION (KG/M2)
        IDS(058)=3      ! CLOUD ICE (KG/M2)
        IDS(059)=8      ! PRECIPITATION RATE (KG/M2/S)
        IDS(060)=2      ! THUNDERSTORM PROBABILITY (PERCENT)
        IDS(061)=3      ! TOTAL PRECIPITATION (KG/M2)
        IDS(062)=3      ! LARGE-SCALE PRECIPITATION (KG/M2)
        IDS(063)=3      ! CONVECTIVE PRECIPITATION (KG/M2)
        IDS(064)=8      ! WATER EQUIVALENT SNOWFALL RATE (KG/M2/S)
        IDS(065)=2      ! WATER EQUIVALENT OF SNOW DEPTH (KG/M2)
        IDS(066)=5      ! SNOW DEPTH (M)
        IDS(067)=4      ! MIXED-LAYER DEPTH (M)
        IDS(068)=4      ! TRANSIENT THERMOCLINE DEPTH (M)
        IDS(069)=4      ! MAIN THERMOCLINE DEPTH (M)
        IDS(070)=4      ! MAIN THERMOCLINE ANOMALY (M)
        IDS(071)=2      ! TOTAL CLOUD COVER (PERCENT)
        IDS(072)=2      ! CONVECTIVE CLOUD COVER (PERCENT)
        IDS(073)=2      ! LOW CLOUD COVER (PERCENT)
        IDS(074)=2      ! MIDDLE CLOUD COVER (PERCENT)
        IDS(075)=2      ! HIGH CLOUD COVER (PERCENT)
        IDS(076)=3      ! CLOUD WATER (KG/M2)
                        !
        IDS(078)=3      ! CONVECTIVE SNOW (KG/M2)
        IDS(079)=3      ! LARGE SCALE SNOW (KG/M2)
        IDS(080)=3      ! WATER TEMPERATURE (K)
        IDS(081)=0      ! SEA-LAND MASK ()
        IDS(082)=4      ! DEVIATION OF SEA LEVEL FROM MEAN (M)
        IDS(083)=7      ! ROUGHNESS (M)
        IDS(084)=3      ! ALBEDO (PERCENT)
        IDS(085)=3      ! SOIL TEMPERATURE (K)
        IDS(086)=2      ! SOIL WETNESS (KG/M2)
        IDS(087)=2      ! VEGETATION (PERCENT)
        IDS(088)=2      ! SALINITY (KG/KG)
        IDS(089)=6      ! DENSITY (KG/M3)
        IDS(090)=3      ! RUNOFF (KG/M2)
        IDS(091)=2      ! ICE CONCENTRATION ()
        IDS(092)=2      ! ICE THICKNESS (M)
        IDS(093)=2      ! DIRECTION OF ICE DRIFT (DEGREES)
        IDS(094)=2      ! SPEED OF ICE DRIFT (M/S)
        IDS(095)=2      ! U OF ICE DRIFT (M/S)
        IDS(096)=2      ! V OF ICE DRIFT (M/S)
        IDS(097)=2      ! ICE GROWTH (M)
        IDS(098)=2      ! ICE DIVERGENCE (1/S)
        IDS(099)=3      ! SNOW MELT (KG/M2)
        IDS(100)=2      ! SIG HEIGHT OF WAVES AND SWELL (M)
        IDS(101)=2      ! DIRECTION OF WIND WAVES (DEGREES)
        IDS(102)=2      ! SIG HEIGHT OF WIND WAVES (M)
        IDS(103)=2      ! MEAN PERIOD OF WIND WAVES (S)
        IDS(104)=2      ! DIRECTION OF SWELL WAVES (DEGREES)
        IDS(105)=2      ! SIG HEIGHT OF SWELL WAVES (M)
        IDS(106)=2      ! MEAN PERIOD OF SWELL WAVES (S)
        IDS(107)=2      ! PRIMARY WAVE DIRECTION (DEGREES)
        IDS(108)=2     ! PRIMARY WAVE MEAN PERIOD (S)
        IDS(109)=2      ! SECONDARY WAVE DIRECTION (DEGREES)
        IDS(110)=2      ! SECONDARY WAVE MEAN PERIOD (S)
        IDS(111)=2      ! NET SOLAR RADIATIVE FLUX AT SURFACE (W/M2)
        IDS(112)=2      ! NET LONGWAVE RADIATIVE FLUX AT SURFACE (W/M2)
        IDS(113)=2      ! NET SOLAR RADIATIVE FLUX AT TOP (W/M2)
        IDS(114)=2      ! NET LONGWAVE RADIATIVE FLUX AT TOP (W/M2)
        IDS(115)=2      ! NET LONGWAVE RADIATIVE FLUX (W/M2)
        IDS(116)=2      ! NET SOLAR RADIATIVE FLUX (W/M2)
        IDS(117)=2      ! TOTAL RADIATIVE FLUX (W/M2)
                        !
                        !
                        !
        IDS(121)=2      ! LATENT HEAT FLUX (W/M2)
        IDS(122)=2      ! SENSIBLE HEAT FLUX (W/M2)
        IDS(123)=2      ! BOUNDARY LAYER DISSIPATION (W/M2)
        IDS(124)=5      ! U WIND STRESS (N/M2)
        IDS(125)=5      ! V WIND STRESS (N/M2)
        IDS(126)=5      ! WIND MIXING ENERGY (J)
        IDS(127)=5      ! IMMAGE DATA ()
        IDS(128)=2      ! MEAN SEA-LEVEL PRESSURE (STDATM) (PA)
        IDS(129)=2      ! MEAN SEA-LEVEL PRESSURE (MAPS) (PA)
        IDS(130)=2      ! MEAN SEA-LEVEL PRESSURE (ETA) (PA)
        IDS(131)=3      ! SURFACE LIFTED INDEX (K)
        IDS(132)=3      ! BEST LIFTED INDEX (K)
        IDS(133)=3      ! K INDEX (K)
        IDS(134)=3      ! SWEAT INDEX (K)
        IDS(135)=10     ! HORIZONTAL MOISTURE DIVERGENCE (KG/KG/S)
        IDS(136)=6      ! SPEED SHEAR (1/S)
        IDS(137)=5      ! 3-HR PRESSURE TENDENCY (PA/S)
        IDS(138)=8      ! BRUNT-VAISALA FREQUENCY SQUARED (1/S2)
        IDS(139)=13     ! POTENTIAL VORTICITY (MASS-WEIGHTED) (1/S/M)
        IDS(140)=2      ! RAIN MASK ()
        IDS(141)=0      ! FREEZING RAIN MASK ()
        IDS(142)=0      ! ICE PELLETS MASK ()
        IDS(143)=0      ! SNOW MASK ()
        IDS(144)=5      ! VOLUMETRIC SOIL MOISTURE CONTENT (FRACTION)
        IDS(145)=2      ! POTENTIAL EVAPORATION RATE (W/M2)
        IDS(146)=2      ! CLOUD WORKFUNCTION (J/KG)
        IDS(147)=5      ! U GRAVITY WAVE STRESS (N/M2)
        IDS(148)=5      ! V GRAVITY WAVE STRESS (N/M2)
        IDS(149)=10     ! POTENTIAL VORTICITY (M2/S/KG)
        IDS(150)=10     ! COVARIANCE BETWEEN V AND U (M2/S2)
        IDS(151)=10     ! COVARIANCE BETWEEN U AND T (K*M/S)
        IDS(152)=10     ! COVARIANCE BETWEEN V AND T (K*M/S)
        IDS(153)=8      ! CLOUD WATER MIXING RATIO (KG/KG)
        IDS(154)=9      ! OZONE MIXING RATIO (KG/KG)
        IDS(155)=2      ! GROUND HEAT FLUX (W/M2)
        IDS(156)=2      ! CONVECTIVE INHIBITION (J/KG)
        IDS(157)=2      ! CONVECTIVE APE (J/KG)
        IDS(158)=2      ! TURBULENT KE (J/KG)
        IDS(159)=2      ! CONDENSATION PRESSURE OF LIFTED PARCEL (PA)
        IDS(160)=2      ! CLEAR SKY UPWARD SOLAR FLUX (W/M2)
        IDS(161)=2      ! CLEAR SKY DOWNWARD SOLAR FLUX (W/M2)
        IDS(162)=2      ! CLEAR SKY UPWARD LONGWAVE FLUX (W/M2)
        IDS(163)=2      ! CLEAR SKY DOWNWARD LONGWAVE FLUX (W/M2)
        IDS(164)=2      ! CLOUD FORCING NET SOLAR FLUX (W/M2)
        IDS(165)=2      ! CLOUD FORCING NET LONGWAVE FLUX (W/M2)
        IDS(166)=2      ! VISIBLE BEAM DOWNWARD SOLAR FLUX (W/M2)
        IDS(167)=2      ! VISIBLE DIFFUSE DOWNWARD SOLAR FLUX (W/M2)
        IDS(168)=2      ! NEAR IR BEAM DOWNWARD SOLAR FLUX (W/M2)
        IDS(169)=2      ! NEAR IR DIFFUSE DOWNWARD SOLAR FLUX (W/M2)
                        !
                        !
        IDS(172)=5      ! MOMENTUM FLUX (N/M2)
        IDS(173)=2      ! MASS POINT MODEL SURFACE ()
        IDS(174)=2      ! VELOCITY POINT MODEL SURFACE ()
        IDS(175)=2      ! SIGMA LAYER NUMBER ()
        IDS(176)=4      ! LATITUDE (DEGREES)
        IDS(177)=4      ! EAST LONGITUDE (DEGREES)
                        !
                        !
                        !
        IDS(181)=9      ! X-GRADIENT LOG PRESSURE (1/M)
        IDS(182)=9      ! Y-GRADIENT LOG PRESSURE (1/M)
        IDS(183)=5      ! X-GRADIENT HEIGHT (M/M)
        IDS(184)=5      ! Y-GRADIENT HEIGHT (M/M)
                        !
                        !
                        !
                        !
                        !
                        !
        IDS(191)=3      ! mixing ratio of cloud water with ice (kg/kg)
        IDS(192)=3      ! mixing ratio of rain water with snow (kg/kg
        IDS(193)=3      ! mixing ratio of cloud water (kg/k)
        IDS(194)=3      ! mixing ratio of cloud ice (kg/kg)
        IDS(195)=3      ! mixing ratio of rain (kg/kg)
        IDS(196)=3      ! mixing ratio of snow (kg/kg)
        IDS(197)=0      ! mixing ratio of groupel (kg/kg)
                        !
                        !
                        !
        IDS(201)=0      ! ICE-FREE WATER SURCACE (PERCENT)
                        !
                        !
        IDS(204)=2      ! DOWNWARD SOLAR RADIATIVE FLUX (W/M2)
        IDS(205)=2      ! DOWNWARD LONGWAVE RADIATIVE FLUX (W/M2)
                        !
        IDS(207)=2      ! MOISTURE AVAILABILITY (PERCENT)
        IDS(208)=2      ! EXCHANGE COEFFICIENT (KG/M2/S)
        IDS(209)=2      ! NUMBER OF MIXED LAYER NEXT TO SFC ()
                        !
        IDS(211)=2      ! UPWARD SOLAR RADIATIVE FLUX (W/M2)
        IDS(212)=2      ! UPWARD LONGWAVE RADIATIVE FLUX (W/M2)
        IDS(213)=2      ! NON-CONVECTIVE CLOUD COVER (PERCENT)
        IDS(214)=2      ! CONVECTIVE PRECIPITATION RATE (KG/M2/S)
        IDS(215)=7      ! TOTAL DIABATIC HEATING RATE (K/S)
        IDS(216)=7      ! TOTAL RADIATIVE HEATING RATE (K/S)
        IDS(217)=7      ! TOTAL DIABATIC NONRADIATIVE HEATING RATE (K/S)
        IDS(218)=2      ! PRECIPITATION INDEX (FRACTION)
        IDS(219)=1      ! STD DEV OF IR T OVER 1X1 DEG AREA (K)
        IDS(220)=4      ! NATURAL LOG OF SURFACE PRESSURE OVER 1 KPA ()
        IDS(221)=3      ! PLANETARY BOUNDARY LAYER HEIGHT (M)
        IDS(222)=3      ! 5-WAVE GEOPOTENTIAL HEIGHT (M)
        IDS(223)=3      ! PLANT CANOPY SURFACE WATER (KG/M2)
                        !
                        !
        IDS(226)=3      ! BLACKADARS MIXING LENGTH (M)
        IDS(227)=3      ! ASYMPTOTIC MIXING LENGTH (M)
        IDS(228)=3      ! POTENTIAL EVAPORATION (KG/M2)
        IDS(229)=2      ! SNOW PHASE-CHANGE HEAT FLUX (W/M2)
                        !
        IDS(231)=5      ! CONVECTIVE CLOUD MASS FLUX (PA/S)
        IDS(232)=2      ! DOWNWARD TOTAL RADIATION FLUX (W/M2)
        IDS(233)=2      ! UPWARD TOTAL RADIATION FLUX (W/M2)
        IDS(234)=1      ! BASEFLOW-GROUNDWATER RUNOFF (KG/M2)
        IDS(235)=1      ! STORM SURFACE RUNOFF (KG/M2)
                        !
        IDS(237)=6      ! TOTAL OZONE (KG/M2)
        IDS(238)=0      ! SNOW COVER (PERCENT)
        IDS(239)=3      ! SNOW TEMPERATURE (K)
                        !
        IDS(241)=7      ! LARGE SCALE CONDENSATION HEATING RATE (K/S)
        IDS(242)=7      ! DEEP CONVECTIVE HEATING RATE (K/S)
        IDS(243)=10     ! DEEP CONVECTIVE MOISTENING RATE (KG/KG/S)
        IDS(244)=7      ! SHALLOW CONVECTIVE HEATING RATE (K/S)
        IDS(245)=10     ! SHALLOW CONVECTIVE MOISTENING RATE (KG/KG/S)
        IDS(246)=7      ! VERTICAL DIFFUSION HEATING RATE (KG/KG/S)
        IDS(247)=7      ! VERTICAL DIFFUSION ZONAL ACCELERATION (M/S/S)
        IDS(248)=7      ! VERTICAL DIFFUSION MERID ACCELERATION (M/S/S)
        IDS(249)=10     ! VERTICAL DIFFUSION MOISTENING RATE (KG/KG/S)
        IDS(250)=7      ! SOLAR RADIATIVE HEATING RATE (K/S)
        IDS(251)=7      ! LONGWAVE RADIATIVE HEATING RATE (K/S)
        IDS(252)=7      ! DRAG COEFFICIENT ()
        IDS(253)=7      ! FRICTION VELOCITY (M/S)
        IDS(254)=7      ! RICHARDSON NUMBER ()
                        !
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE RRDSGR(NSIG,IGRD1,JGRD1,NFLDS,LEVR,LAT1,LAT2,CLAT,         &
     &                  RDELX2,RDELY2,RLAT1,RLON1,RLAT2,RLON2,              &
     &                  NONHYD,NCLOUD,F)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    RRDSGR       READ SIGMA GRID FILE DATA RECORDS
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: READS THE DATA RECORDS FROM THE SIGMA GRID FILE.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!
! USAGE:    CALL RRDSGR(NSIG,IGRD1,JGRD1,NFLDS,LEVR,LAT1,LAT2,
!                       RDELX2,RDELY2,F)
!
!   INPUT ARGUMENT LIST:
!     NSIG     - INTEGER UNIT FROM WHICH TO READ DATA
!
!   OUTPUT ARGUMENT LIST:
!     NWF      - INTEGER NUMBER OF WORDS IN EACH HORIZONTAL FIELD
!     LAT      - INTEGER NUMBER OF J ROW
!     F        - REAL FIELD (NWF) READ
!
!   INPUT FILES:
!     NSIG     - SIGMA GRID FILE
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      PARAMETER(RD=2.8705E+2,RV=4.6150E+2,FVIRT=RV/RD-1.)
      DIMENSION F(IGRD1,NFLDS,LAT2-LAT1+1)
      DIMENSION GR1(IGRD1*JGRD1),GR2(IGRD1*JGRD1),GR3(IGRD1*JGRD1)
      DIMENSION CLAT(IGRD1*JGRD1)
      DIMENSION IDATE(4),SI( LEVR +1),SL( LEVR ),DEL( LEVR )
      DIMENSION DOT( LEVR +1),CG( LEVR )
      COMMON/IOINDX/ KSZ,KSD,KST,KSQ,KSPSX,KSPSY,KSU,KSV,KSPS,KSZS,      &           
     &               KSZSX,KSZSY,KSPW,KSPN,KSTN,KSWN,KSQC,KSQR
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! REWIND THEN SKIP LAB AND FHOUR RECORDS
      REWIND NSIG
      READ(NSIG)
      READ(NSIG) FHOUR,IDATE,SI,SL
      DO K=1,LEVR
        DEL(K)=SI(K)-SI(K+1)
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! PREPARE SOME CONSTANT
      NWF=IGRD1*JGRD1
      IF(LAT1.EQ.1) THEN
         LAT1X=MIN(2,LAT2)
      ELSE
         LAT1X=LAT1
      ENDIF
      IF(LAT2.EQ.JGRD1) THEN
         LAT2X=MAX(JGRD1-1,LAT1)
      ELSE
         LAT2X=LAT2
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! GZ
      READ(NSIG) (GR1(I),I=1,NWF)
      write(*,*) 'gz ',GR1(1)
      DO LAT=LAT1,LAT2
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO I=1,IGRD1
          F(I,KSZS,LAN)=GR1(I+JLAT)
        ENDDO
      ENDDO
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        JLATN=MIN(JLAT+IGRD1,NWF-IGRD1)
        JLATS=MAX(JLAT-IGRD1,0)
        JLATE=JLAT+1
        JLATW=JLAT-1
        DO I=2,IGRD1-1
          F(I,KSZSX,LAN)=RDELX2*(GR1(I+JLATE)-GR1(I+JLATW))
          F(I,KSZSY,LAN)=RDELY2*(GR1(I+JLATN)-GR1(I+JLATS))
        ENDDO
      ENDDO
! LNPS
      READ(NSIG) (GR1(I),I=1,NWF)
      write(*,*) 'ps ',GR1(1)
      DO LAT=LAT1,LAT2
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO I=1,IGRD1
          F(I,KSPS,LAN)=EXP(GR1(I+JLAT))
        ENDDO
      ENDDO
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        JLATN=MIN(JLAT+IGRD1,NWF-IGRD1)
        JLATS=MAX(JLAT-IGRD1,0)
        JLATE=JLAT+1
        JLATW=JLAT-1
        DO I=2,IGRD1-1
          F(I,KSPSX,LAN)=RDELX2*(GR1(I+JLATE)-GR1(I+JLATW))
          F(I,KSPSY,LAN)=RDELY2*(GR1(I+JLATN)-GR1(I+JLATS))
        ENDDO
      ENDDO
! T
      DO 10 K=1,LEVR
        KT=KST-1+K
        READ(NSIG) (GR1(I),I=1,NWF)
        write(*,*) 't  ',GR1(1)
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KT,LAN)=GR1(I+JLAT)
          ENDDO
        ENDDO
 10   CONTINUE
! U V
      DO 20 K=1,LEVR
        KU=KSU-1+K
        KV=KSV-1+K
        KZ=KSZ-1+K
        KD=KSD-1+K
        READ(NSIG) (GR1(I),I=1,NWF)
        write(*,*) 'u  ',GR1(1)
        READ(NSIG) (GR2(I),I=1,NWF)
        write(*,*) 'v  ',GR1(1)
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KU,LAN)=GR1(I+JLAT)
            F(I,KV,LAN)=GR2(I+JLAT)
          ENDDO
        ENDDO
        DO LAT=LAT1X,LAT2X
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          JLATN=MIN(JLAT+IGRD1,NWF-IGRD1)
          JLATS=MAX(JLAT-IGRD1,0)
          JLATE=JLAT+1
          JLATW=JLAT-1
          DO I=2,IGRD1-1
            F(I,KD,LAN)= RDELX2*(GR1(I+JLATE)-GR1(I+JLATW))              &      
     &                  +RDELY2*(GR2(I+JLATN)-GR2(I+JLATS))
            F(I,KZ,LAN)= RDELX2*(GR2(I+JLATE)-GR2(I+JLATW))              &
     &                  -RDELY2*(GR1(I+JLATN)-GR1(I+JLATS))
          ENDDO
        ENDDO
 20   CONTINUE
! Q
      DO 30 K=1,LEVR
        KQ=KSQ-1+K
!       KT=KST-1+K
        KK=KSWN-1+K        ! borrow WN for cloud waters
        READ(NSIG) (GR1(I),I=1,NWF)
        write(*,*) 'q  ',GR1(1)
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KQ,LAN)=GR1(I+JLAT)
!           F(I,KT,LAN)=F(I,KT,LAN)/(1.+FVIRT*GR1(I+JLAT))
            F(I,KK,LAN)=GR1(I+JLAT)
          ENDDO
        ENDDO
 30   CONTINUE
!
      DO K=1,LEVR
      READ(NSIG) (GR1(I),I=1,NWF)   ! ozone
      ENDDO
      DO K=1,LEVR
      READ(NSIG) (GR1(I),I=1,NWF)   ! cloud water
      ENDDO
!
      IF( NCLOUD .GE. 2 ) THEN
        DO K=1,LEVR
          KQC=KSQC-1+K
          KQR=KSQR-1+K
          KKK=KSWN-1+K
          READ(NSIG) (GR1(I),I=1,NWF)
          write(*,*) 'q2  ',GR1(1)
          DO LAT=LAT1,LAT2
            LAN=LAT-LAT1+1
            JLAT=(LAT-1)*IGRD1
            DO I=1,IGRD1
              F(I,KQC,LAN)=GR1(I+JLAT)
              F(I,KQR,LAN)=0.0
              F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
            ENDDO
          ENDDO
        ENDDO
        IF( NCLOUD .EQ. 3 ) THEN
          DO K=1,LEVR
            KQR=KSQR-1+K
            KKK=KSWN-1+K
            READ(NSIG) (GR1(I),I=1,NWF)
            write(*,*) 'q3  ',GR1(1)
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQR,LAN)=GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        IF( NCLOUD .EQ. 5 ) THEN
          DO K=1,LEVR
            KQC=KSQC-1+K
            KKK=KSWN-1+K
            READ(NSIG) (GR1(I),I=1,NWF)
            write(*,*) 'q4  ',GR1(1)
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQC,LAN)=F(I,KQC,LAN)+GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
          DO K=1,LEVR
            KQR=KSQR-1+K
            KKK=KSWN-1+K
            READ(NSIG) (GR1(I),I=1,NWF)
            write(*,*) 'q5  ',GR1(1)
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQR,LAN)=GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
          DO K=1,LEVR
            KQR=KSQR-1+K
            KKK=KSWN-1+K
            READ(NSIG) (GR1(I),I=1,NWF)
            write(*,*) 'q6  ',GR1(1)
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQR,LAN)=F(I,KQR,LAN)+GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C factor to fully modify the virtual temperature
      DO K=1,LEVR
        KQ=KSQ-1+K
        KK=KSWN-1+K             ! borrow PN for cloud waters
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KK,LAN)=(1.+FVIRT*F(I,KQ,LAN))*
     &                  (1.+F(I,KQ,LAN))/(1.+F(I,KK,LAN))
          ENDDO
        ENDDO
      ENDDO
C
      IF( NONHYD .EQ. 1 ) THEN
C PN
      DO K=1,LEVR
        KPN=KSPN-1+K
        READ(NSIG) (GR1(I),I=1,NWF)
        write(*,*) 'pn  ',GR1(1)
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KPN,LAN)=EXP(GR1(I+JLAT))
          ENDDO
        ENDDO
      ENDDO
      DO LAT=LAT1,LAT2
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO I=1,IGRD1
          F(I,KSPS,LAN)=F(I,KSPN,LAN)/SL(1)
          GR1(I+JLAT)=LOG(F(I,KSPS,LAN))
        ENDDO
      ENDDO
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        JLATN=MIN(JLAT+IGRD1,NWF-IGRD1)
        JLATS=MAX(JLAT-IGRD1,0)
        JLATE=JLAT+1
        JLATW=JLAT-1
        DO I=2,IGRD1-1
          F(I,KSPSX,LAN)=RDELX2*(GR1(I+JLATE)-GR1(I+JLATW))
          F(I,KSPSY,LAN)=RDELY2*(GR1(I+JLATN)-GR1(I+JLATS))
        ENDDO
      ENDDO
C TN
      DO K=1,LEVR
        KTN=KSTN-1+K
        KK=KSWN-1+K             ! borrow PW for cloud waters
        READ(NSIG) (GR1(I),I=1,NWF)
        write(*,*) 'tn  ',GR1(1)
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KTN,LAN)=GR1(I+JLAT)/F(I,KK,LAN)
          ENDDO
        ENDDO
      ENDDO
C WN
      DO K=1,LEVR+1
        KWN=KSWN-1+K
        READ(NSIG) (GR1(I),I=1,NWF)
        write(*,*) 'wn  ',GR1(1)
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            IF(K.GT.1) THEN
              F(I,KWN-1,LAN)=(F(I,KWN-1,LAN)+GR1(I+JLAT))*0.5
            ENDIF
            IF(K.LE.LEVR) THEN
              F(I,KWN,LAN)=GR1(I+JLAT)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      ELSE
CC hydro P
      DO K=1,LEVR
        KPN=KSPN-1+K
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KPN,LAN)=F(I,KSPS,LAN)*SL(K)
          ENDDO
        ENDDO
      ENDDO
C dry hydro T
      DO K=1,LEVR
        KT=KST-1+K
        KTN=KSTN-1+K
        KK=KSWN-1+K             ! borrow PW for cloud waters
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KTN,LAN)=F(I,KT,LAN)/F(I,KK,LAN)
          ENDDO
        ENDDO
      ENDDO
C zero W
      DO K=1,LEVR
        KWN=KSWN-1+K
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KWN,LAN)=0.0
          ENDDO
        ENDDO
      ENDDO
C
      ENDIF
C
! M**2
      READ(NSIG) (GR1(I),I=1,NWF)
      write(*,*) 'fm  ',GR1(1)
      DO LAT=LAT1,LAT2
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO K=1,LEVR
          KU=KSU-1+K
          KV=KSV-1+K
          DO I=1,IGRD1
            XM = SQRT( GR1(I+JLAT) )
            F(I,KU,LAN)=F(I,KU,LAN)*XM
            F(I,KV,LAN)=F(I,KV,LAN)*XM
          ENDDO
        ENDDO
      ENDDO
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO I=2,IGRD1-1
          XM = SQRT( GR1(I+JLAT) )
          F(I,KSPSX,LAN)=F(I,KSPSX,LAN)*XM
          F(I,KSPSY,LAN)=F(I,KSPSY,LAN)*XM
          F(I,KSZSX,LAN)=F(I,KSZSX,LAN)*XM
          F(I,KSZSY,LAN)=F(I,KSZSY,LAN)*XM
        ENDDO
        DO K=1,LEVR
          KD=KSD-1+K
          KZ=KSZ-1+K
          DO I=2,IGRD1-1
            F(I,KD,LAN)=F(I,KD,LAN)*GR1(I+JLAT)
            F(I,KZ,LAN)=F(I,KZ,LAN)*GR1(I+JLAT)
          ENDDO
        ENDDO
!  ------ DO VVEL in PACAL/SEC
        DO I=2,IGRD1-1
          DO K=1,LEVR+1
            DOT(K) = 0.0
          ENDDO
!...  COMPUTE C=V(TRUE)*DEL(LN(PS)).DIVIDE BY COS FOR DEL COS FOR V
          DO K=1,LEVR
            KU=KSU-1+K
            KV=KSV-1+K
            CG(K)=F(I,KU,LAN)*F(I,KSPSX,LAN)+                            &              
     &            F(I,KV,LAN)*F(I,KSPSY,LAN)
          ENDDO
          DB=DEL(1)*F(I,KSD,LAN)
          CB=DEL(1)*CG(1)
          DO K=1,LEVR-1
            KD=KSD-1+K
            DB=DB+DEL(K+1)*F(I,KD+1,LAN)
            CB=CB+DEL(K+1)*CG(K+1)
          ENDDO
!...    SIGMA DOT COMPUTED ONLY AT INTERIOR INTERFACES
          DO K=1,LEVR-1
            KD=KSD-1+K
            DOT(K+1)=DOT(K)+DEL(K)                                       &
     &               *(DB+CB-F(I,KD,LAN)-CG(K))
          ENDDO
          DO K=1,LEVR
            KW=KSPW-1+K
            F(I,KW,LAN)=SL(K)*(CG(K)-CB-DB)-                             &
     &                0.5*(DOT(K+1)+DOT(K))
            F(I,KW,LAN)=F(I,KW,LAN)*F(I,KSPS,LAN)*1000.0
            IF( NONHYD.EQ.0 ) THEN
              KWN=KSWN-1+K
              KP =KSPN-1+K
              KT =KST -1+K
              RHOG=F(I,KP,LAN)*G / RD /F(I,KT,LAN) * 10.
              F(I,KWN,LAN)=-F(I,KW,LAN)/RHOG
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!
! MODIFY  DIV AND VOR
      READ(NSIG) (GR2(I),I=1,NWF)
      write(*,*) 'fm2x  ',GR1(1)
      READ(NSIG) (GR3(I),I=1,NWF)
      write(*,*) 'fm2y  ',GR1(1)
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO K=1,LEVR
          KD=KSD-1+K
          KZ=KSZ-1+K
          KU=KSU-1+K
          KV=KSV-1+K
          DO I=2,IGRD1-1
            XM = SQRT( GR1(I+JLAT) )
            F(I,KD,LAN)=F(I,KD,LAN)                                      &             
     &            +0.5*(F(I,KU,LAN)*GR2(I+JLAT)+                         &
     &                  F(I,KV,LAN)*GR3(I+JLAT))/XM
            F(I,KZ,LAN)=F(I,KZ,LAN)                                      &
     &            +0.5*(F(I,KV,LAN)*GR2(I+JLAT)-                         &
     &                  F(I,KU,LAN)*GR3(I+JLAT))/XM
          ENDDO
        ENDDO
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! MODIFY LATERAL BOUNDARY TO BE THE SAME AS NEXT INNER GRID FOR GRADIENT
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        F(1,KSPSX,LAN)=F(2,KSPSX,LAN)
        F(1,KSPSY,LAN)=F(2,KSPSY,LAN)
        F(1,KSZSX,LAN)=F(2,KSZSX,LAN)
        F(1,KSZSY,LAN)=F(2,KSZSY,LAN)
        F(IGRD1,KSPSX,LAN)=F(IGRD1-1,KSPSX,LAN)
        F(IGRD1,KSPSY,LAN)=F(IGRD1-1,KSPSY,LAN)
        F(IGRD1,KSZSX,LAN)=F(IGRD1-1,KSZSX,LAN)
        F(IGRD1,KSZSY,LAN)=F(IGRD1-1,KSZSY,LAN)
        DO K=1,LEVR
          KD=KSD-1+K
          KZ=KSZ-1+K
          F(1,KD,LAN)=F(2,KD,LAN)
          F(1,KZ,LAN)=F(2,KZ,LAN)
          F(IGRD1,KD,LAN)=F(IGRD1-1,KD,LAN)
          F(IGRD1,KZ,LAN)=F(IGRD1-1,KZ,LAN)
        ENDDO
      ENDDO
      IF(LAT1.EQ.1) THEN
        DO I=1,IGRD1
          F(I,KSPSX,1)=F(I,KSPSX,2)
          F(I,KSPSY,1)=F(I,KSPSY,2)
          F(I,KSZSX,1)=F(I,KSZSX,2)
          F(I,KSZSY,1)=F(I,KSZSY,2)
        ENDDO
        DO K=1,LEVR
          KD=KSD-1+K
          KZ=KSZ-1+K
          DO I=1,IGRD1
            F(I,KD,1)=F(I,KD,2)
            F(I,KZ,1)=F(I,KZ,2)
          ENDDO
        ENDDO
      ENDIF
      IF(LAT2.EQ.JGRD1) THEN
        LANLAST=LAT2-LAT1+1
        DO I=1,IGRD1
          F(I,KSPSX,LANLAST)=F(I,KSPSX,LANLAST-1)
          F(I,KSPSY,LANLAST)=F(I,KSPSY,LANLAST-1)
          F(I,KSZSX,LANLAST)=F(I,KSZSX,LANLAST-1)
          F(I,KSZSY,LANLAST)=F(I,KSZSY,LANLAST-1)
        ENDDO
        DO K=1,LEVR
          KD=KSD-1+K
          KZ=KSZ-1+K
          DO I=1,IGRD1
            F(I,KD,LANLAST)=F(I,KD,LANLAST-1)
            F(I,KZ,LANLAST)=F(I,KZ,LANLAST-1)
          ENDDO
        ENDDO
      ENDIF
! READ LATITUDE.
      READ(NSIG) (GR1(I),I=1,NWF)
      write(*,*) 'lat   ',GR1(1)
      DO IJ=1,NWF
        CLAT(IJ)=GR1(IJ)
      ENDDO
      RLAT1=GR1(1)
      RLAT2=GR1(NWF)
      READ(NSIG) (GR1(I),I=1,NWF)
      write(*,*) 'lon   ',GR1(1)
      RLON1=GR1(1)
      RLON2=GR1(NWF)
      write(*,*) rlon1,rlon2
      PRINT *,' END OF READ WITHIN ',LAT1,LAT2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE WRYTE(LU,LC,C)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    WRYTE       WRITE DATA OUT BY BYTES
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: EFFICIENTLY WRITE UNFORMATTED A CHARACETER ARRAY.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!   95-10-31  HANN-MING HENRY JUANG MODIFY TO BE WORKSTATION VERSION
!
! USAGE:    CALL WRYTE(LU,LC,C,IACT)
!
!   INPUT ARGUMENT LIST:
!     LU       - INTEGER UNIT TO WHICH TO WRITE
!     LC       - INTEGER NUMBER OF CHARACTERS OR BYTES TO WRITE
!     C        - CHARACETER (LC) DATA TO WRITE
!     IACT     - FLUSH OUTPUT TO FILE IF IACT=1
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      CHARACTER*1 C(LC)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      WRITE(LU) (C(I),I=1,LC)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R
      SUBROUTINE GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,                   &                           
     &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,       &
     &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,                &
     &                  INA,INM,ICEN2,IDS,IENS,                          &
     &                  XLAT1,XLON1,XLAT2,XLON2,DELX,DELY,ORITRU,PROJ,   &
     &                  GRIB,LGRIB,IERR)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    GRIBIT      CREATE GRIB MESSAGE
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
!
! ABSTRACT: CREATE A GRIB MESSAGE FROM A FULL FIELD.
!   AT PRESENT, ONLY GLOBAL LATLON GRIDS AND GAUSSIAN GRIDS
!   AND REGIONAL POLAR PROJECTIONS ARE ALLOWED.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   94-05-04  JUANG (FOR GSM AND RSM USE)
!
! USAGE:    CALL GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
!    &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
!    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,
!    &                  INA,INM,ICEN2,IDS,IENS,
!    &                  XLAT1,XLON1,DELX,DELY,ORITRU,PROJ,
!    &                  GRIB,LGRIB,IERR)
!   INPUT ARGUMENT LIST:
!     F        - REAL (IM*JM) FIELD DATA TO PACK INTO GRIB MESSAGE
!     LBM      - LOGICAL (IM*JM) BITMAP TO USE IF IBMS=1
!     IDRT     - INTEGER DATA REPRESENTATION TYPE
!                (0 FOR LATLON OR 4 FOR GAUSSIAN OR 5 FOR POLAR)
!     IM       - INTEGER LONGITUDINAL DIMENSION
!     JM       - INTEGER LATITUDINAL DIMENSION
!     MXBIT    - INTEGER MAXIMUM NUMBER OF BITS TO USE (0 FOR NO LIMIT)
!     COLAT1   - REAL FIRST COLATITUDE OF GRID IF IDRT=4 (RADIANS)
!     ILPDS    - INTEGER LENGTH OF THE PDS (USUALLY 28)
!     IPTV     - INTEGER PARAMETER TABLE VERSION (USUALLY 1)
!     ICEN     - INTEGER FORECAST CENTER (USUALLY 7)
!     IGEN     - INTEGER MODEL GENERATING CODE
!     IBMS     - INTEGER BITMAP FLAG (0 FOR NO BITMAP)
!     IPU      - INTEGER PARAMETER AND UNIT INDICATOR
!     ITL      - INTEGER TYPE OF LEVEL INDICATOR
!     IL1      - INTEGER FIRST LEVEL VALUE (0 FOR SINGLE LEVEL)
!     IL2      - INTEGER SECOND LEVEL VALUE
!     IYR      - INTEGER YEAR
!     IMO      - INTEGER MONTH
!     IDY      - INTEGER DAY
!     IHR      - INTEGER HOUR
!     IFTU     - INTEGER FORECAST TIME UNIT (1 FOR HOUR)
!     IP1      - INTEGER FIRST TIME PERIOD
!     IP2      - INTEGER SECOND TIME PERIOD (0 FOR SINGLE PERIOD)
!     ITR      - INTEGER TIME RANGE INDICATOR (10 FOR SINGLE PERIOD)
!     INA      - INTEGER NUMBER INCLUDED IN AVERAGE
!     INM      - INTEGER NUMBER MISSING FROM AVERAGE
!     ICEN2    - INTEGER FORECAST SUBCENTER
!                (USUALLY 0 BUT 1 FOR REANAL OR 2 FOR ENSEMBLE)
!     IDS      - INTEGER DECIMAL SCALING
!     IENS     - INTEGER (5) ENSEMBLE EXTENDED PDS VALUES
!                (APPLICATION,TYPE,IDENTIFICATION,PRODUCT,SMOOTHING)
!                (USED ONLY IF ICEN2=2 AND ILPDS>=45)
!     XLAT1    - REAL FIRST POINT OF REGIONAL LATITUDE (RADIANS)
!     XLON1    - REAL FIRST POINT OF REGIONAL LONGITUDE (RADIANS)
!     XLAT2    - REAL LAST  POINT OF REGIONAL LATITUDE (RADIANS)
!     XLON2    - REAL LAST  POINT OF REGIONAL LONGITUDE (RADIANS)
!     DELX     - REAL DX ON 60N FOR REGIONAL (M)
!     DELY     - REAL DY ON 60N FOR REGIONAL (M)
!     PROJ     - REAL POLAR PROJECTION FLAG 1 FOR NORTH -1 FOR SOUTH
!                     MERCATER PROJECTION 0
!     ORITRU   - REAL ORIENTATION OF REGIONAL POLAR PROJECTION OR
!                     TRUTH FOR REGIONAL MERCATER PROJECTION
!
!   OUTPUT ARGUMENT LIST:
!     GRIB     - CHARACTER (LGRIB) GRIB MESSAGE
!     LGRIB    - INTEGER LENGTH OF GRIB MESSAGE
!                (NO MORE THAN 100+ILPDS+IM*JM*(MXBIT+1)/8)
!     IERR     - INTEGER ERROR CODE (0 FOR SUCCESS)
!
! SUBPROGRAMS CALLED:
!   GTBITS     - COMPUTE NUMBER OF BITS AND ROUND DATA APPROPRIATELY
!   W3FI72     - ENGRIB DATA INTO A GRIB1 MESSAGE
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      INTEGER IENS(5)
      REAL F(IM*JM)
      LOGICAL LBM(IM*JM)
      CHARACTER GRIB(*)
      INTEGER IPDS(100),IGDS(100),IBDS(100)
      INTEGER IBM(IM*JM)
      REAL FR(IM*JM)
      CHARACTER PDS(28)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DETERMINE GRID PARAMETERS
      PI=ACOS(-1.)
      NF=IM*JM
      IF(IDRT.EQ.0) THEN
        IF(IM.EQ.144.AND.JM.EQ.73) THEN
          IGRID=2
        ELSEIF(IM.EQ.360.AND.JM.EQ.181) THEN
          IGRID=3
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3)
        LON1=0
        LATI=NINT(180.E3/(JM-1))
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
      ELSEIF(IDRT.EQ.4) THEN
        IF(IM.EQ.192.AND.JM.EQ.94) THEN
          IGRID=98
        ELSEIF(IM.EQ.384.AND.JM.EQ.190) THEN
          IGRID=126
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3-180.E3/PI*COLAT1)
        LON1=0
        LATI=JM/2
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.5) THEN    ! POLAR PROJECTION
        IGRID=255
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)
        IRESFL=8
        IGDS09=NINT(ORITRU*1.E3)
        IGDS10=DELX
        IGDS11=DELY
        IF( NINT(PROJ).EQ.1  ) IGDS12=0         ! NORTH POLAR PROJ
        IF( NINT(PROJ).EQ.-1 ) IGDS12=128       ! SOUTH POLAT PROJ
        ISCAN=64
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.1) THEN    ! MERCATER PROJECTION
        IGRID=255
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)
        IRESFL=8
        IGDS09=NINT(180.E3/ACOS(-1.) * XLAT2)
        IGDS10=NINT(180.E3/ACOS(-1.) * XLON2)
        IGDS11=DELX
        IGDS12=DELY
        IGDS13=NINT(ORITRU*1.E3)
        ISCAN=64
        IGDS14=ISCAN
      ELSE
        IERR=40
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  RESET TIME RANGE PARAMETER IN CASE OF OVERFLOW
      IF(ITR.GE.2.AND.ITR.LE.5.AND.IP2.GE.256) THEN
        JP1=IP2
        JP2=0
        JTR=10
      ELSE
        JP1=IP1
        JP2=IP2
        JTR=ITR
      ENDIF
!  FIX YEAR AND CENTURY
      IYR4=IYR
      IF(IYR.LE.100) IYR4=2050-MOD(2050-IYR,100)
      IYC=MOD(IYR4-1,100)+1
      ICY=(IYR4-1)/100+1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FILL PDS PARAMETERS
      IPDS(01)=ILPDS    ! LENGTH OF PDS
      IPDS(02)=IPTV     ! PARAMETER TABLE VERSION ID
      IPDS(03)=ICEN     ! CENTER ID
      IPDS(04)=IGEN     ! GENERATING MODEL ID
      IPDS(05)=IGRID    ! GRID ID
      IPDS(06)=1        ! GDS FLAG
      IPDS(07)=IBMS     ! BMS FLAG
      IPDS(08)=IPU      ! PARAMETER UNIT ID
      IPDS(09)=ITL      ! TYPE OF LEVEL ID
      IPDS(10)=IL1      ! LEVEL 1 OR 0
      IPDS(11)=IL2      ! LEVEL 2
      IPDS(12)=IYC      ! YEAR
      IPDS(13)=IMO      ! MONTH
      IPDS(14)=IDY      ! DAY
      IPDS(15)=IHR      ! HOUR
      IPDS(16)=0        ! MINUTE
      IPDS(17)=IFTU     ! FORECAST TIME UNIT ID
      IPDS(18)=JP1      ! TIME PERIOD 1
      IPDS(19)=JP2      ! TIME PERIOD 2 OR 0
      IPDS(20)=JTR      ! TIME RANGE INDICATOR
      IPDS(21)=INA      ! NUMBER IN AVERAGE
      IPDS(22)=INM      ! NUMBER MISSING
      IPDS(23)=ICY       ! CENTURY
      IPDS(24)=ICEN2    ! FORECAST SUBCENTER
      IPDS(25)=IDS      ! DECIMAL SCALING
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FILL GDS AND BDS PARAMETERS
      IGDS(01)=0        ! NUMBER OF VERTICAL COORDS
      IGDS(02)=255      ! VERTICAL COORD FLAG
      IGDS(03)=IDRT     ! DATA REPRESENTATION TYPE
      IGDS(04)=IM       ! EAST-WEST POINTS
      IGDS(05)=JM       ! NORTH-SOUTH POINTS
      IGDS(06)=LAT1     ! LATITUDE OF ORIGIN
      IGDS(07)=LON1     ! LONGITUDE OF ORIGIN
      IGDS(08)=IRESFL   ! RESOLUTION FLAG
      IGDS(09)=IGDS09   ! LATITUDE OF END OR ORIENTATION
      IGDS(10)=IGDS10   ! LONGITUDE OF END OR DX IN METER ON 60N
      IGDS(11)=IGDS11   ! LAT INCREMENT OR GAUSSIAN LATS OR DY IN METER
      IGDS(12)=IGDS12   ! LONGITUDE INCREMENT OR PROJECTION
      IGDS(13)=IGDS13   ! SCANNING MODE OR LAT OF INTERCUT ON EARTH FOR
      IGDS(14)=IGDS14   ! NOT USED OR SCANNING MODE FOR MERCATER
      IGDS(15)=0     ! NOT USED
      IGDS(16)=0     ! NOT USED
      IGDS(17)=0     ! NOT USED
      IGDS(18)=0     ! NOT USED
      DO I=1,9
      IBDS(I)=0       ! BDS FLAGS
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FILL BITMAP AND COUNT VALID DATA.  RESET BITMAP FLAG IF ALL VALID.
      NBM=NF
      IF(IBMS.NE.0) THEN
        NBM=0
        DO I=1,NF
          IF(LBM(I)) THEN
            IBM(I)=1
            NBM=NBM+1
          ELSE
            IBM(I)=0
          ENDIF
        ENDDO
        IF(NBM.EQ.NF) IPDS(7)=0
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND DATA AND DETERMINE NUMBER OF BITS
      IF(NBM.EQ.0) THEN
        DO I=1,NF
          FR(I)=0.
        ENDDO
        NBIT=0
      ELSE
        CALL GTBITS(IPDS(7),IDS,NF,IBM,F,FR,FMIN,FMAX,NBIT)
!       WRITE(0,'("GTBITS:",4I4,4X,2I4,4X,2G16.6)')
!    &   IPU,ITL,IL1,IL2,IDS,NBIT,FMIN,FMAX
        IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CREATE PRODUCT DEFINITION SECTION
      CALL W3FI68(IPDS,PDS)
      IF(ICEN2.EQ.2.AND.ILPDS.GE.45) THEN
        ILAST=45
        CALL PDSENS(IENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CREATE GRIB MESSAGE
      CALL W3FI72(0,FR,0,NBIT,1,IPDS,PDS,                                &  
     &            1,255,IGDS,0,0,IBM,NF,IBDS,                            &
     &            NFO,GRIB,LGRIB,IERR)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R
      SUBROUTINE GTBITS(IBM,IDS,LEN,MG,G,GROUND,GMIN,GMAX,NBIT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    GTBITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
!
! ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
!   AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
!   THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
!   THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
!   GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!
! USAGE:    CALL GTBITS(IBM,IDS,LEN,MG,G,GMIN,GMAX,NBIT)
!   INPUT ARGUMENT LIST:
!     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!     IDS      - INTEGER DECIMAL SCALING
!                (E.G. IDS=3 TO ROUND FIELD TO NEAREST MILLI-VALUE)
!     LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
!     MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!     G        - REAL (LEN) FIELD
!
!   OUTPUT ARGUMENT LIST:
!     GROUND   - REAL (LEN) FIELD ROUNDED TO DECIMAL SCALING
!                (SET TO ZERO WHERE BITMAP IS 0 IF IBM=1)
!     GMIN     - REAL MINIMUM VALID ROUNDED FIELD VALUE
!     GMAX     - REAL MAXIMUM VALID ROUNDED FIELD VALUE
!     NBIT     - INTEGER NUMBER OF BITS TO PACK
!
! SUBPROGRAMS CALLED:
!   ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      DS=10.**IDS
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
          DO I=1,I1-1
            GROUND(I)=0.
          ENDDO
          GROUND(I1)=NINT(G(I1)*DS)/DS
          GMAX=GROUND(I1)
          GMIN=GROUND(I1)
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
          DO I=1,LEN
            GROUND(I)=0.
          ENDDO
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      NBIT=LOG((GMAX-GMIN)*DS+0.9)/LOG(2.)+1.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
!FPP$ NOCONCUR R
	  SUBROUTINE PDSENS(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,MSGA)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    PDSENS.F    PACKS GRIB PDS EXTENSION 41- FOR ENSEMBLE
!   PRGMMR: ZOLTAN TOTH      ORG: W/NMC20    DATE: 95-03-14
!
! ABSTRACT: PACKS BRIB PDS EXTENSION STARTING ON BYTE 41 FOR ENSEMBLE
!	FORECAST PRODUCTS. FOR FORMAT OF PDS EXTENSION, SEE NMC OFFICE NOTE 38
!
! PROGRAM HISTORY LOG:
!   95-03-14  ZOLTAN TOTH AND MARK IREDELL
!
! USAGE:    CALL PDSENS.F(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,MSGA)
!   INPUT ARGUMENT LIST:
!     KENS(5)  - BYTES 41-45 (GENERAL SECTION, ALWAYS PRESENT.)
!     KPROB(2) - BYTES 46-47 (PROBABILITY SECTION, PRESENT ONLY IF NEEDE
!     XPROB(2) - BYTES 48-51&52-55 (PROBABILITY SECTION, IF NEEDED.)
!     KCLUST(16)-BYTES 61-76 (CLUSTERING SECTION, IF NEEDED.)
!     KMEMBR(80)-BYTES 77-86 (CLUSTER MEMBERSHIP SECTION, IF NEEDED.)
!     ILAST    - LAST BYTE TO BE PACKED (IF GREATER OR EQUAL TO FIRST BY
!                IN ANY OF FOUR SECTIONS ABOVE, WHOLE SECTION IS PACKED.
!
!   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
!     MSGA     - FULL PDS SECTION, INCLUDING NEW ENSEMBLE EXTENSION
!
! REMARKS: USE PDSEUP.F FOR UNPACKING PDS ENSEMBLE EXTENSION.
!
! ATTRIBUTES:
!   LANGUAGE: CF77 FORTRAN
!   MACHINE:  CRAY, WORKSTATIONS
!
!$$$
!	TESTING GRIB EXTENSION 41- PACKER AND UNPACKER SUBROUTINES
!
	  INTEGER KENS(5),KPROB(2),KCLUST(16),KMEMBR(80)
      DIMENSION XPROB(2)
	  CHARACTER*1 MSGA(100)
	  IF(ILAST.LT.41) THEN
	  PRINT *,'WARNING - SUBROUTINE IS FOR PACKING BYTES 41 AND ABOVE'
	  GO TO 333
	  ENDIF
!	PACKING IS DONE IN FOUR SECTIONS ENDING AT BYTE IL
	  IF(ILAST.GE.41) IL=45
	  IF(ILAST.GE.46) IL=55
	  IF(ILAST.GE.61) IL=76
	  IF(ILAST.GE.77) IL=86
!	CHANGING THE NUMBER OF BYTES (FIRST THREE BYTES IN PDS)
     	  CALL   SBYTE(MSGA, IL, 0,24)
!-AFA	  CALL MYSBYTE(MSGA, IL, 0,24)
!	PACKING FIRST SECTION (GENERAL INTORMATION SECTION)
        IF(IL.GE.45) CALL   SBYTES(MSGA,KENS,40*8,8,0,5)
!-AFA   IF(IL.GE.45) CALL MYSBYTES(MSGA,KENS,40*8,8,0,5)
!	PACKING 2ND SECTION (PROBABILITY SECTION)
      IF(IL.GE.55) THEN
          CALL   SBYTES(MSGA,KPROB,45*8,8,0,2)
!-AFA     CALL MYSBYTES(MSGA,KPROB,45*8,8,0,2)
	  CALL W3FI01(LW)
	  CALL W3FI76(XPROB(1),IEXP,IMANT,8*LW)
          CALL   SBYTE(MSGA,IEXP,47*8,8)
!-AFA     CALL MYSBYTE(MSGA,IEXP,47*8,8)
          CALL   SBYTE(MSGA,IMANT,48*8,24)
!-AFA     CALL MYSBYTE(MSGA,IMANT,48*8,24)
	  CALL W3FI76(XPROB(2),IEXP,IMANT,8*LW)
          CALL   SBYTE(MSGA,IEXP,51*8,8)
!-AFA     CALL MYSBYTE(MSGA,IEXP,51*8,8)
          CALL   SBYTE(MSGA,IMANT,52*8,24)
!-AFA     CALL MYSBYTE(MSGA,IMANT,52*8,24)
      ENDIF
!	PACKING 3RD SECTION (CLUSTERING INFORMATION)
      IF(IL.GE.76) CALL   SBYTES(MSGA,KCLUST,60*8,8,0,16)
!-AFA IF(IL.GE.76) CALL MYSBYTES(MSGA,KCLUST,60*8,8,0,16)
!	PACKING 4TH SECTION (CLUSTER MEMBERSHIP)
      IF(IL.GE.86) CALL   SBYTES(MSGA,KMEMBR,76*8,1,0,10)
!-AFA IF(IL.GE.86) CALL MYSBYTES(MSGA,KMEMBR,76*8,1,0,10)
!
 333  CONTINUE
	  RETURN
	  END
      FUNCTION ISRCHNE(N,A,IS,VAL)
      DIMENSION A(N)
      ISRCHNE=N
      DO NN=IS,N
      IF( A(NN).NE.VAL ) THEN
        ISRCHNE=NN
        RETURN
      ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE CTLHEAD(NN,IM,JM,KM,PROJ,PI,PJ,OR,DX,                   &               
     &                  IHR,IDAY,IMON,IYR,IFH,                           &
     &                  STRLON,DLON,STRLAT,DLAT,                         &
     &                  RLAT,IZ)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                .      .    .                                       .  
! SUBPROGRAM:    CTLHEAD    WRITE OUT THE GRADS CONTROL HEADER
!   PRGMMR: HANN-MING HENRY JUANG      ORG: W/NMC20    DATE: 96-10-14             
!                                                                       
! ABSTRACT:  	WRITE OUT THE DATAFILE NAME, DATA TYPE, INDEX FILE NAME AND
!		THE ALL DEFINITION OF THE GRID INCLUDES MAP PROJECTION,
!		X, Y, Z DEFINITIONS.
!
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   96-10-14  HANN-MING HENRY JUANG
!                                                                       
! USAGE: CALL CTLHEAD(NN,IM,JM,KM,PROJ,PI,PJ,OR,DX,IHR,IDAY,IMON,IYR,IFH,
!                     STRLON,DLON,STRLAT,DLAT,RLAT,IZ)
!   INPUT ARGUMENT LIST:                                                
!	NN	FILE UNIT NUMBER TO WRITE THE HEADER
!	IM	DIMENSION IN X DIRECTION
!	JM	DIMENSION IN Y DIRECTION
!	KM	DIMENSION IN Z DIRECTION
!	PROJ	MAP PROJECTION
!	PI	I GRID AT POLE FOR POLAR PROJECTION
!	PJ	J GRID AT POLE FOR POLAR PROJECTION
!	OR	ORIENTATION IN LONGITUDE
!	DX	GRID SPACING IN METER
!	IHR	HOUR OF THE DATE
!	IDAY	DAT OF THE DATE
!	IMON	MONTH OF THE DATE
!	IYR	YEAR OF THE DATE
!	IFH	FORECAST HOUR
!	STRLON	START LONGIUTUDE
!	DLON	INCREMENT OF LONGITUDE
!	STRLAT	START LATITUDE
!	DLAT	INCREMENT OF LATITUDE
!	RLAT	ALL LATITUDE FOR MERCATER TYPE IN DIMENSION OF (JM)
!	IZ	VERTICAL SURFACE INDICATOR IN DIMENSION OF (KM)
!                                                                       
!   OUTPUT ARGUMENT LIST:
!                                                                       
! REMARKS: 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CF77 FORTRAN                                              
!   MACHINE:  CRAY, WORKSTATIONS                                        
!                                                                       
!$$$                                                                    
      DIMENSION RLAT(JM),IZ(KM)
      CHARACTER*2 HOUR,DAY
      CHARACTER*3 MON(12)
      DATA MON/'JAN','FEB','MAR','APR','MAY','JUN',                      &   
     &         'JUL','AUG','SEP','OCT','NOV','DEC'/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      PRINT *,' WRITE CONTROL HEADER TO CTLHEAD '
      WRITE(NN,101)
      WRITE(NN,102)
      WRITE(NN,103)
      WRITE(NN,104)
      WRITE(NN,105)
      WRITE(NN,106)
 101  FORMAT('dset DATAFILE')
 102  FORMAT('dtype grib')
 103  FORMAT('options template')
 104  FORMAT('index MAPFILE')
 105  FORMAT('undef -9.99E+33')
 106  FORMAT('title EXP1')
!
      IF( PROJ.EQ.0.0 ) THEN
        WRITE(NN,108) IM,STRLON,DLON
        WRITE(NN,110) JM
        WRITE(NN,111) (RLAT(J),J=1,JM)
      ELSE
        WRITE(NN,107) IM,JM,PI,PJ,OR,DX
        IMP=360.*111/DX
        STRLONP=0.0
        DLONP=360./IMP
        JMP=IMP/4
        STRLATP=0.0
        DLATP=DLONP
        WRITE(NN,108) IMP,STRLONP,DLONP
        WRITE(NN,109) JMP,STRLATP,DLATP
      ENDIF
 107  FORMAT('pdef',2I5,' nps',4F9.3)
 108  FORMAT('xdef',I5,' linear',2F9.3)
 109  FORMAT('ydef',I5,' linear',2F9.3)
 110  FORMAT('ydef',I5,' levels')
 111  FORMAT(10F8.3)
!
      WRITE(NN,112) KM
      WRITE(NN,113) (IZ(K),K=1,KM)
      IF( IHR.LT.10 ) THEN
        WRITE(HOUR,90) IHR
      ELSE
        WRITE(HOUR,91) IHR
      ENDIF
      IF( IDAY.LT.10 ) THEN
        WRITE(DAY,90) IDAY
      ELSE
        WRITE(DAY,91) IDAY
      ENDIF
  90  FORMAT('0',I1)
  91  FORMAT(I2)
      IF( IFH.EQ.0 ) IFH=1
      IF(IYR.LE.100) IYR=2050-MOD(2050-IYR,100)
      WRITE(NN,114) HOUR,DAY,MON(IMON),IYR,IFH
 112  FORMAT('zdef',I5,' levels')
 113  FORMAT(10I8)
 114  FORMAT('tdef 2 linear ',A2,'Z',A2,A3,I4,I10,'hr')
      WRITE(NN,115)
 115  FORMAT('vars TOTALNUM')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      RETURN
      END
      SUBROUTINE CTLEND(NN)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                .      .    .                                       .  
! SUBPROGRAM:    CTLEND    WRITE OUT THE GRADS CONTROL END MARK
!   PRGMMR: HANN-MING HENRY JUANG      ORG: W/NMC20    DATE: 96-10-14             
!                                                                       
! ABSTRACT:  	WRITE OUT THE END MARK OF THE GRADS CONTRL FILE.
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   96-10-14  HANN-MING HENRY JUANG
!                                                                       
! USAGE: CALL CTLEND(NN)
!                     
!   INPUT ARGUMENT LIST:                                                
!	NN	FILE UNIT NUMBER FOR GRADS CONTROL FILE
!                                                                       
!   OUTPUT ARGUMENT LIST:
!                                                                       
! REMARKS: 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CF77 FORTRAN                                              
!   MACHINE:  CRAY, WORKSTATIONS                                        
!                                                                       
!$$$                                                                    
      WRITE(NN,100)
 100  FORMAT('endvars')
      RETURN
      END
      SUBROUTINE CTLVAR(NN,NUMVAR,NUMSFC)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                .      .    .                                       .  
! SUBPROGRAM:    CTLVAR    WRITE OUT THE GRADS RECORD TO CONTROL FILE
!   PRGMMR: HANN-MING HENRY JUANG      ORG: W/NMC20    DATE: 96-10-14             
!                                                                       
! ABSTRACT:  	WRITE OUT THE RECORD NAME, VARIABLE TYPE, SURFACE AND ITS
! 		DESCRIPTION.
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   96-10-14  HANN-MING HENRY JUANG
!                                                                       
! USAGE: CALL CTLVAR(NN,NUMVAR,NUMSFC)
!                   
!   INPUT ARGUMENT LIST:                                                
!	NN	FILE UNIT NUMBER TO WRITE THE GRADS RECORD
!	NUMVAR	VARIABLE NUMBER
!	NUMSFC	SURFACE NUMBER
!                                                                       
!   OUTPUT ARGUMENT LIST:
!                                                                       
! REMARKS: 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CF77 FORTRAN                                              
!   MACHINE:  CRAY, WORKSTATIONS                                        
!                                                                       
!$$$                                                                    
      CHARACTER VARSFC*8,VARWRT*8
      CHARACTER VAR*5,SFC*3,DSL*50
      COMMON /COMCTL/ VAR(255),SFC(255),DSL(255)
      SAVE NUMVARS,NUMSFCS,NUMS,NNS
      DATA NUMVARS,NUMSFCS,NUMS,NNS/4*0/
!
      IF( NUMVAR.NE.NUMVARS .OR. NUMSFC.NE.NUMSFCS ) THEN
        IF(NUMVARS.NE.0) THEN
          VARSFC=VAR(NUMVARS)//SFC(NUMSFCS)
          VARWRT='        '
          M=0
          DO N=1,8
            IF(VARSFC(N:N).NE.'-') THEN
              M=M+1
              VARWRT(M:M)=VARSFC(N:N)
            ENDIF
          ENDDO
          IF(NUMS.EQ.1) NUMS=0
          WRITE(NNS,200) VARWRT,NUMS,NUMVARS,NUMSFCS,DSL(NUMVARS)
200       FORMAT(A8,I8,I4,',',I3,',0 ',A50)
        ENDIF
        NUMS=1
      ELSE
        NUMS=NUMS+1
      ENDIF
!
      NNS=NN
      NUMVARS=NUMVAR
      NUMSFCS=NUMSFC
!
      RETURN
      END
      SUBROUTINE CTLINI
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                .      .    .                                       .  
! SUBPROGRAM:    CTLINI    PREDEFINE ALL VARIABLE FOR GRADS CONTROL FILE
!   PRGMMR: HANN-MING HENRY JUANG      ORG: W/NMC20    DATE: 96-10-14             
!                                                                       
! ABSTRACT:  	DEFINE COMMON PLACE CALLED COMCTL FOR GRADS CONTROL FILE
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   96-10-14  HANN-MING HENRY JUANG
!                                                                       
! USAGE: CALL CTLINI
!                  
!   INPUT ARGUMENT LIST:                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:
!                                                                       
! REMARKS: 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CF77 FORTRAN                                              
!   MACHINE:  CRAY, WORKSTATIONS                                        
!                                                                       
!$$$                                                                    
      CHARACTER VAR*5,SFC*3,DSL*50
      COMMON /COMCTL/ VAR(255),SFC(255),DSL(255)
!x    DATA SFC/255*'   '/
      DO I=1,255
       SFC(I)='   '
      ENDDO
       SFC(  1)='sfc'	! earth surface (ground and sea)
       SFC(  2)='cbl'	! cloud bottom level
       SFC(  3)='ctl'	! cloud top level
       SFC(  4)='t0l'	! 0 deg C isothermal level
       SFC(  5)='lcl'	! level of covection
       SFC(  6)='mwl'	! maximal wind level
       SFC(  7)='trp'	! trapopause
       SFC(  8)='toa'	! top of atmosphere
       SFC(  9)='msl'	! mean sea level
       SFC(100)='prs'	! pressure surface
       SFC(102)='msl'	! reduced to mean sea level
       SFC(105)='hag'
       SFC(107)='sig'	! sigma layer
       SFC(108)='slr'	! soil layes	100 94 72
       SFC(112)='dlr'	! deep soil layer 200 10
       SFC(116)='plg'
       SFC(200)='lls'	! low layer sum or mean
       SFC(212)='lcb'	! low cloud bottom
       SFC(213)='lct'	! low cloud top
       SFC(214)='lcl'	! low cloud level
       SFC(222)='mcb'	! mid-level cloud bottom
       SFC(223)='mct'	! mid-level cloud top
       SFC(224)='mcl'	! mid-level cloud level
       SFC(232)='hcb'	! high cloud bottom
       SFC(233)='hct'	! high cloud top
       SFC(234)='hcl'	! high cloud level
!
!x    DATA VAR/255*'UNKWN'/
      DO I=1,255
       VAR(I)='UNKWN'
      ENDDO
       VAR(001)='pres-'
       VAR(002)='prmsl'
       VAR(003)='ptend'
		!     '
		!     '
       VAR(006)='gp---'
       VAR(007)='hgt--'
       VAR(008)='dist-'
       VAR(009)='hstdv'
		!     '
       VAR(011)='tmp--'
       VAR(012)='vtmp-'
       VAR(013)='pot--'
       VAR(014)='epot-'
       VAR(015)='t-max'
       VAR(016)='t-min'
       VAR(017)='dpt--'
       VAR(018)='depr-'
       VAR(019)='lapr-'
       VAR(020)='vis--'
       VAR(021)='rdsp1'
       VAR(022)='rdsp2'
       VAR(023)='rdsp3'
       VAR(024)='hytmp'
       VAR(025)='tmp-a'
       VAR(026)='presa'
       VAR(027)='gp-a-'
       VAR(028)='wvsp1'
       VAR(029)='wvsp2'
       VAR(030)='wvsp3'
       VAR(031)='wdir-'
       VAR(032)='wind-'
       VAR(033)='u-grd'
       VAR(034)='v-grd'
       VAR(035)='strm-'
       VAR(036)='v-pot'
       VAR(037)='mntsf'
       VAR(038)='sgcvv'
       VAR(039)='v-vel'
       VAR(040)='dzdt-'
       VAR(041)='abs-v'
       VAR(042)='abs-d'
       VAR(043)='rel-v'
       VAR(044)='rel-d'
       VAR(045)='vucsh'
       VAR(046)='vvcsh'
       VAR(047)='dir-c'
       VAR(048)='sp-c-'
       VAR(049)='uogrd'
       VAR(050)='vogrd'
       VAR(051)='spf-h'
       VAR(052)='r-h--'
       VAR(053)='mixr-'
       VAR(054)='p-wat'
       VAR(055)='vapp-'
       VAR(056)='sat-d'
       VAR(057)='evp--'
       VAR(058)='c-ice'
       VAR(059)='prate'
       VAR(060)='tstm-'
       VAR(061)='a-pcp'
       VAR(062)='ncpcp'
       VAR(063)='acpcp'
       VAR(064)='srweq'
       VAR(065)='weasd'
       VAR(066)='sno-d'
       VAR(067)='mixht'
       VAR(068)='tthdp'
       VAR(069)='mthd-'
       VAR(070)='mth-a'
       VAR(071)='t-cdc'
       VAR(072)='cdcon'
       VAR(073)='l-cdc'
       VAR(074)='m-cdc'
       VAR(075)='h-cdc'
       VAR(076)='c-wat'
                !     '
       VAR(078)='sno-c'
       VAR(079)='sno-l'
       VAR(080)='wtmp-'
       VAR(081)='land-'
       VAR(082)='dsl-m'
       VAR(083)='sfc-r'
       VAR(084)='albdo'
       VAR(085)='tsoil'
       VAR(086)='soilm'
       VAR(087)='veg--'
       VAR(088)='salty'
       VAR(089)='den--'
       VAR(090)='wat-r'
       VAR(091)='ice-c'
       VAR(092)='icetk'
       VAR(093)='diced'
       VAR(094)='siced'
       VAR(095)='u-ice'
       VAR(096)='v-ice'
       VAR(097)='ice-g'
       VAR(098)='ice-d'
       VAR(099)='sno-m'
       VAR(100)='htsgw'
       VAR(101)='wvdir'
       VAR(102)='wvhgt'
       VAR(103)='wvper'
       VAR(104)='swdir'
       VAR(105)='swell'
       VAR(106)='swper'
       VAR(107)='dirpw'
       VAR(108)='perpw'
       VAR(109)='dirsw'
       VAR(110)='persw'
       VAR(111)='nswrs'
       VAR(112)='nlwrs'
       VAR(113)='nswrt'
       VAR(114)='nlwrt'
       VAR(115)='lwavr'
       VAR(116)='swavr'
       VAR(117)='g-rad'
		!     '
		!     '
		!     '
       VAR(121)='lhtfl'
       VAR(122)='shtfl'
       VAR(123)='blydp'
       VAR(124)='u-flx'
       VAR(125)='v-flx'
       VAR(126)='wmixe'
       VAR(127)='img-d'
       VAR(128)='mslsa'
       VAR(129)='mslma'
       VAR(130)='mslet'
       VAR(131)='lft-x'
       VAR(132)='blftx'
       VAR(133)='k-x--'
       VAR(134)='s-x--'
       VAR(135)='mconv'
       VAR(136)='vw-sh'
       VAR(137)='tslsa'
       VAR(138)='bvf-2'
       VAR(139)='pv-mw'
       VAR(140)='crain'
       VAR(141)='cfrzn'
       VAR(142)='cicep'
       VAR(143)='csnow'
       VAR(144)='vsmc-'	! named by hmhj
       VAR(145)='poter'	! named by hmhj
       VAR(146)='cwork'	! named by hmhj
       VAR(147)='u-gws'	! named by hmhj
       VAR(148)='v-gws'	! named by hmhj
       VAR(149)='pot-v'	! named by hmhj
       VAR(150)='covmz'
       VAR(151)='covtz'
       VAR(152)='covtm'
		!     '
		!     '
       VAR(155)='gflux'
       VAR(156)='cin--'
       VAR(157)='cape-'
       VAR(158)='tke--'
       VAR(159)='condp'
       VAR(160)='csusf'
       VAR(161)='csdsf'
       VAR(162)='csulf'
       VAR(163)='csdlf'
       VAR(164)='cfnsf'
       VAR(165)='cfnlf'
       VAR(166)='vbdsf'
       VAR(167)='vddsf'
       VAR(168)='nbdsf'
       VAR(169)='nddsf'
		!     '
		!     '
       VAR(172)='m-flx'
       VAR(173)='lmh--'
       VAR(174)='lmv--'
       VAR(175)='mlyno'
       VAR(176)='nlat-'
       VAR(177)='elon-'
		!     '
		!     '
		!     '
       VAR(181)='lps-x'
       VAR(182)='lps-y'
       VAR(183)='hgt-x'
       VAR(184)='hgt-y'
		!     '
		!     '
		!     '
		!     '
		!     '
       VAR(191)='qci--'
       VAR(192)='qrs--'
		!     '
		!     '
		!     '
		!     '
		!     '
		!     '
		!     '
		!     '
		!     '
       VAR(201)='icwat'
		!     '
		!     '
       VAR(204)='dswrf'
       VAR(205)='dlwrf'
		!     '
       VAR(207)='mstav'
       VAR(208)='sfexc'
       VAR(209)='mixly'
		!     '
       VAR(211)='uswrf'
       VAR(212)='ulwrf'
       VAR(213)='cdlyr'
       VAR(214)='cprat'
       VAR(215)='ttdia'
       VAR(216)='ttrad'
       VAR(217)='ttphy'
       VAR(218)='preix'
       VAR(219)='tsd1d'
       VAR(220)='nlgsp'
       VAR(221)='h-pbl'
       VAR(222)='wav5h'
       VAR(223)='c-wat'
		!     '
		!     '
       VAR(226)='bmixl'
       VAR(227)='amixl'
       VAR(228)='pevap'
       VAR(229)='snohf'
		!     '
       VAR(231)='mflux'
       VAR(232)='dtrf-'
       VAR(233)='utrf-'
       VAR(234)='bgrun'
       VAR(235)='ssrun'
		!     '
		!     '
       VAR(238)='sno-c'
       VAR(239)='sno-t'
		!     '
       VAR(241)='lrghr'
       VAR(242)='cnvhr'
       VAR(243)='cnvmr'
       VAR(244)='shahr'
       VAR(245)='shamr'
       VAR(246)='vdfhr'
       VAR(247)='vdfua'
       VAR(248)='vdfva'
       VAR(249)='vdfmr'
       VAR(250)='swhr-'
       VAR(251)='lwhr-'
       VAR(252)='cd---'
       VAR(253)='fric-'
       VAR(254)='ri---'
		!     '
!
!x    DATA DSL/255*
!x   1          'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'/
                !12345678901234567890123456789012345678901234567890
      DO I=1,255
       DSL(I)=  'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
      ENDDO
       DSL(001)='PRESSURE (PA)                                     '
       DSL(002)='SEA-LEVEL PRESSURE (PA)                           '
       DSL(003)='PRESSURE TENDENCY (PA/S)                          '
		!                                                  '
		!                                                  '
       DSL(006)='GEOPOTENTIAL (M2/S2)                              '
       DSL(007)='GEOPOTENTIAL HEIGHT (M)                           '
       DSL(008)='GEOMETRIC HEIGHT (M)                              '
       DSL(009)='STANDARD DEVIATION OF HEIGHT (M)                  '
		!                                                  '
       DSL(011)='TEMPERATURE (K)                                   '
       DSL(012)='VIRTUAL TEMPERATURE (K)                           '
       DSL(013)='POTENTIAL TEMPERATURE (K)                         '
       DSL(014)='PSEUDO-ADIABATIC POTENTIAL TEMPERATURE (K)        '
       DSL(015)='MAXIMUM TEMPERATURE (K)                           '
       DSL(016)='MINIMUM TEMPERATURE (K)                           '
       DSL(017)='DEWPOINT TEMPERATURE (K)                          '
       DSL(018)='DEWPOINT DEPRESSION (K)                           '
       DSL(019)='TEMPERATURE LAPSE RATE (K/M)                      '
       DSL(020)='VISIBILITY (M)                                    '
       DSL(021)='RADAR SPECTRA 1 ()                                '
       DSL(022)='RADAR SPECTRA 2 ()                                '
       DSL(023)='RADAR SPECTRA 3 ()                                '
       DSL(024)='HYDROSTATIC TEMPERATURE (K)                       '
       DSL(025)='TEMPERATURE ANOMALY (K)                           '
       DSL(026)='PRESSURE ANOMALY (PA)                             '
       DSL(027)='GEOPOTENTIAL HEIGHT ANOMALY (M)                   '
       DSL(028)='WAVE SPECTRA 1 ()                                 '
       DSL(029)='WAVE SPECTRA 2 ()                                 '
       DSL(030)='WAVE SPECTRA 3 ()                                 '
       DSL(031)='WIND DIRECTION (DEGREES)                          '
       DSL(032)='WIND SPEED (M/S)                                  '
       DSL(033)='ZONAL WIND (M/S)                                  '
       DSL(034)='MERIDIONAL WIND (M/S)                             '
       DSL(035)='STREAMFUNCTION (M2/S)                             '
       DSL(036)='VELOCITY POTENTIAL (M2/S)                         '
       DSL(037)='MONTGOMERY STREAM FUNCTION (M2/S2)                '
       DSL(038)='SIGMA VERTICAL VELOCITY (1/S)                     '
       DSL(039)='PRESSURE VERTICAL VELOCITY (PA/S)                 '
       DSL(040)='GEOMETRIC VERTICAL VELOCITY (M/S)                 '
       DSL(041)='ABSOLUTE VORTICITY (1/S)                          '
       DSL(042)='ABSOLUTE DIVERGENCE (1/S)                         '
       DSL(043)='RELATIVE VORTICITY (1/S)                          '
       DSL(044)='RELATIVE DIVERGENCE (1/S)                         '
       DSL(045)='VERTICAL U SHEAR (1/S)                            '
       DSL(046)='VERTICAL V SHEAR (1/S)                            '
       DSL(047)='DIRECTION OF CURRENT (DEGREES)                    '
       DSL(048)='SPEED OF CURRENT (M/S)                            '
       DSL(049)='U OF CURRENT (M/S)                                '
       DSL(050)='V OF CURRENT (M/S)                                '
       DSL(051)='SPECIFIC HUMIDITY (KG/KG)                         '
       DSL(052)='RELATIVE HUMIDITY (PERCENT)                       '
       DSL(053)='HUMIDITY MIXING RATIO (KG/KG)                     '
       DSL(054)='PRECIPITABLE WATER (KG/M2)                        '
       DSL(055)='VAPOR PRESSURE (PA)                               '
       DSL(056)='SATURATION DEFICIT (PA)                           '
       DSL(057)='EVAPORATION (KG/M2)                               '
       DSL(058)='CLOUD ICE (KG/M2)                                 '
       DSL(059)='PRECIPITATION RATE (KG/M2/S)                      '
       DSL(060)='THUNDERSTORM PROBABILITY (PERCENT)                '
       DSL(061)='TOTAL PRECIPITATION (KG/M2)                       '
       DSL(062)='LARGE-SCALE PRECIPITATION (KG/M2)                 '
       DSL(063)='CONVECTIVE PRECIPITATION (KG/M2)                  '
       DSL(064)='WATER EQUIVALENT SNOWFALL RATE (KG/M2/S)          '
       DSL(065)='WATER EQUIVALENT OF SNOW DEPTH (KG/M2)            '
       DSL(066)='SNOW DEPTH (M)                                    '
       DSL(067)='MIXED-LAYER DEPTH (M)                             '
       DSL(068)='TRANSIENT THERMOCLINE DEPTH (M)                   '
       DSL(069)='MAIN THERMOCLINE DEPTH (M)                        '
       DSL(070)='MAIN THERMOCLINE ANOMALY (M)                      '
       DSL(071)='TOTAL CLOUD COVER (PERCENT)                       '
       DSL(072)='CONVECTIVE CLOUD COVER (PERCENT)                  '
       DSL(073)='LOW CLOUD COVER (PERCENT)                         '
       DSL(074)='MIDDLE CLOUD COVER (PERCENT)                      '
       DSL(075)='HIGH CLOUD COVER (PERCENT)                        '
       DSL(076)='CLOUD WATER (KG/M2)                               '
                !                                                  '
       DSL(078)='CONVECTIVE SNOW (KG/M2)                           '
       DSL(079)='LARGE SCALE SNOW (KG/M2)                          '
       DSL(080)='WATER TEMPERATURE (K)                             '
       DSL(081)='SEA-LAND MASK ()                                  '
       DSL(082)='DEVIATION OF SEA LEVEL FROM MEAN (M)              '
       DSL(083)='ROUGHNESS (M)                                     '
       DSL(084)='ALBEDO (PERCENT)                                  '
       DSL(085)='SOIL TEMPERATURE (K)                              '
       DSL(086)='SOIL WETNESS (KG/M2)                              '
       DSL(087)='VEGETATION (PERCENT)                              '
       DSL(088)='SALINITY (KG/KG)                                  '
       DSL(089)='DENSITY (KG/M3)                                   '
       DSL(090)='RUNOFF (KG/M2)                                    '
       DSL(091)='ICE CONCENTRATION ()                              '
       DSL(092)='ICE THICKNESS (M)                                 '
       DSL(093)='DIRECTION OF ICE DRIFT (DEGREES)                  '
       DSL(094)='SPEED OF ICE DRIFT (M/S)                          '
       DSL(095)='U OF ICE DRIFT (M/S)                              '
       DSL(096)='V OF ICE DRIFT (M/S)                              '
       DSL(097)='ICE GROWTH (M)                                    '
       DSL(098)='ICE DIVERGENCE (1/S)                              '
       DSL(099)='SNOW MELT (KG/M2)                                 '
       DSL(100)='SIG HEIGHT OF WAVES AND SWELL (M)                 '
       DSL(101)='DIRECTION OF WIND WAVES (DEGREES)                 '
       DSL(102)='SIG HEIGHT OF WIND WAVES (M)                      '
       DSL(103)='MEAN PERIOD OF WIND WAVES (S)                     '
       DSL(104)='DIRECTION OF SWELL WAVES (DEGREES)                '
       DSL(105)='SIG HEIGHT OF SWELL WAVES (M)                     '
       DSL(106)='MEAN PERIOD OF SWELL WAVES (S)                    '
       DSL(107)='PRIMARY WAVE DIRECTION (DEGREES)                  '
       DSL(108)='PRIMARY WAVE MEAN PERIOD (S)                      '
       DSL(109)='SECONDARY WAVE DIRECTION (DEGREES)                '
       DSL(110)='SECONDARY WAVE MEAN PERIOD (S)                    '
       DSL(111)='NET SOLAR RADIATIVE FLUX AT SURFACE (W/M2)        '
       DSL(112)='NET LONGWAVE RADIATIVE FLUX AT SURFACE (W/M2)     '
       DSL(113)='NET SOLAR RADIATIVE FLUX AT TOP (W/M2)            '
       DSL(114)='NET LONGWAVE RADIATIVE FLUX AT TOP (W/M2)         '
       DSL(115)='NET LONGWAVE RADIATIVE FLUX (W/M2)                '
       DSL(116)='NET SOLAR RADIATIVE FLUX (W/M2)                   '
       DSL(117)='TOTAL RADIATIVE FLUX (W/M2)                       '
		!                                                  '
		!                                                  '
		!                                                  '
       DSL(121)='LATENT HEAT FLUX (W/M2)                           '
       DSL(122)='SENSIBLE HEAT FLUX (W/M2)                         '
       DSL(123)='BOUNDARY LAYER DISSIPATION (W/M2)                 '
       DSL(124)='U WIND STRESS (N/M2)                              '
       DSL(125)='V WIND STRESS (N/M2)                              '
       DSL(126)='WIND MIXING ENERGY (J)                            '
       DSL(127)='IMAGE DATA ()                                     '
       DSL(128)='MEAN SEA-LEVEL PRESSURE (STDATM) (PA)             '
       DSL(129)='MEAN SEA-LEVEL PRESSURE (MAPS) (PA)               '
       DSL(130)='MEAN SEA-LEVEL PRESSURE (ETA) (PA)                '
       DSL(131)='SURFACE LIFTED INDEX (K)                          '
       DSL(132)='BEST LIFTED INDEX (K)                             '
       DSL(133)='K INDEX (K)                                       '
       DSL(134)='SWEAT INDEX (K)                                   '
       DSL(135)='HORIZONTAL MOISTURE DIVERGENCE (KG/KG/S)          '
       DSL(136)='SPEED SHEAR (1/S)                                 '
       DSL(137)='3-HR PRESSURE TENDENCY (PA/S)                     '
       DSL(138)='BRUNT-VAISALA FREQUENCY SQUARED (1/S2)            '
       DSL(139)='POTENTIAL VORTICITY (MASS-WEIGHTED) (1/S/M)       '
       DSL(140)='RAIN MASK ()                                      '
       DSL(141)='FREEZING RAIN MASK ()                             '
       DSL(142)='ICE PELLETS MASK ()                               '
       DSL(143)='SNOW MASK ()                                      '
       DSL(144)='VOLUMETRIC SOIL MOISTURE CONTENT (FRACTION)       '
       DSL(145)='POTENTIAL EVAPORATION RATE (W/M2)                 '
       DSL(146)='CLOUD WORKFUNCTION (J/KG)                         '
       DSL(147)='U GRAVITY WAVE STRESS (N/M2)                      '
       DSL(148)='V GRAVITY WAVE STRESS (N/M2)                      '
       DSL(149)='POTENTIAL VORTICITY (M2/S/KG)                     '
       DSL(150)='COVARIANCE BETWEEN V AND U (M2/S2)                '
       DSL(151)='COVARIANCE BETWEEN U AND T (K*M/S)                '
       DSL(152)='COVARIANCE BETWEEN V AND T (K*M/S)                '
		!                                                  '
		!                                                  '
       DSL(155)='GROUND HEAT FLUX (W/M2)                           '
       DSL(156)='CONVECTIVE INHIBITION (W/M2)                      '
       DSL(157)='CONVECTIVE APE (J/KG)                             '
       DSL(158)='TURBULENT KE (J/KG)                               '
       DSL(159)='CONDENSATION PRESSURE OF LIFTED PARCEL (PA)       '
       DSL(160)='CLEAR SKY UPWARD SOLAR FLUX (W/M2)                '
       DSL(161)='CLEAR SKY DOWNWARD SOLAR FLUX (W/M2)              '
       DSL(162)='CLEAR SKY UPWARD LONGWAVE FLUX (W/M2)             '
       DSL(163)='CLEAR SKY DOWNWARD LONGWAVE FLUX (W/M2)           '
       DSL(164)='CLOUD FORCING NET SOLAR FLUX (W/M2)               '
       DSL(165)='CLOUD FORCING NET LONGWAVE FLUX (W/M2)            '
       DSL(166)='VISIBLE BEAM DOWNWARD SOLAR FLUX (W/M2)           '
       DSL(167)='VISIBLE DIFFUSE DOWNWARD SOLAR FLUX (W/M2)        '
       DSL(168)='NEAR IR BEAM DOWNWARD SOLAR FLUX (W/M2)           '
       DSL(169)='NEAR IR DIFFUSE DOWNWARD SOLAR FLUX (W/M2)        '
		!                                                  '
		!                                                  '
       DSL(172)='MOMENTUM FLUX (N/M2)                              '
       DSL(173)='MASS POINT MODEL SURFACE ()                       '
       DSL(174)='VELOCITY POINT MODEL SURFACE ()                   '
       DSL(175)='SIGMA LAYER NUMBER ()                             '
       DSL(176)='LATITUDE (DEGREES)                                '
       DSL(177)='EAST LONGITUDE (DEGREES)                          '
		!                                                  '
		!                                                  '
		!                                                  '
       DSL(181)='X-GRADIENT LOG PRESSURE (1/M)                     '
       DSL(182)='Y-GRADIENT LOG PRESSURE (1/M)                     '
       DSL(183)='X-GRADIENT HEIGHT (M/M)                           '
       DSL(184)='Y-GRADIENT HEIGHT (M/M)                           '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
       DSL(191)='mixing ratio of cloud water with ice (kg/kg)      '
       DSL(192)='mixing ratio of rain water with snow (kg/kg)      '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
		!                                                  '
       DSL(201)='ICE-FREE WATER SURCACE (PERCENT)                  '
		!                                                  '
		!                                                  '
       DSL(204)='DOWNWARD SOLAR RADIATIVE FLUX (W/M2)              '
       DSL(205)='DOWNWARD LONGWAVE RADIATIVE FLUX (W/M2)           '
		!                                                  '
       DSL(207)='MOISTURE AVAILABILITY (PERCENT)                   '
       DSL(208)='EXCHANGE COEFFICIENT (KG/M2/S)                    '
       DSL(209)='NUMBER OF MIXED LAYER NEXT TO SFC ()              '
		!                                                  '
       DSL(211)='UPWARD SOLAR RADIATIVE FLUX (W/M2)                '
       DSL(212)='UPWARD LONGWAVE RADIATIVE FLUX (W/M2)             '
       DSL(213)='NON-CONVECTIVE CLOUD COVER (PERCENT)              '
       DSL(214)='CONVECTIVE PRECIPITATION RATE (KG/M2/S)           '
       DSL(215)='TOTAL DIABATIC HEATING RATE (K/S)                 '
       DSL(216)='TOTAL RADIATIVE HEATING RATE (K/S)                '
       DSL(217)='TOTAL DIABATIC NONRADIATIVE HEATING RATE (K/S)    '
       DSL(218)='PRECIPITATION INDEX (FRACTION)                    '
       DSL(219)='STD DEV OF IR T OVER 1X1 DEG AREA (K)             '
       DSL(220)='NATURAL LOG OF SURFACE PRESSURE OVER 1 KPA ()     '
       DSL(221)='PLANETARY BOUNDARY LAYER HEIGHT (M)               '
       DSL(222)='5-WAVE GEOPOTENTIAL HEIGHT (M)                    '
       DSL(223)='PLANT CANOPY SURFACE WATER (KG/M2)                '
		!                                                  '
		!                                                  '
       DSL(226)='BLACKADARS MIXING LENGTH (M)                      '
       DSL(227)='ASYMPTOTIC MIXING LENGTH (M)                      '
       DSL(228)='POTENTIAL EVAPORATION (KG/M2)                     '
       DSL(229)='SNOW PHASE-CHANGE HEAT FLUX (W/M2)                '
		!                                                  '
       DSL(231)='CONVECTIVE CLOUD MASS FLUX (PA/S)                 '
       DSL(232)='DOWNWARD TOTAL RADIATION FLUX (W/M2)              '
       DSL(233)='UPWARD TOTAL RADIATION FLUX (W/M2)                '
       DSL(234)='BASEFLOW-GROUNDWATER RUNOFF (KG/M2)               '
       DSL(235)='STORM SURFACE RUNOFF (KG/M2)                      '
		!                                                  '
		!                                                  '
       DSL(238)='SNOW COVER (PERCENT)                              '
       DSL(239)='SNOW TEMPERATURE (K)                              '
		!                                                  '
       DSL(241)='LARGE SCALE CONDENSATION HEATING RATE (K/S)       '
       DSL(242)='DEEP CONVECTIVE HEATING RATE (K/S)                '
       DSL(243)='DEEP CONVECTIVE MOISTENING RATE (KG/KG/S)         '
       DSL(244)='SHALLOW CONVECTIVE HEATING RATE (K/S)             '
       DSL(245)='SHALLOW CONVECTIVE MOISTENING RATE (KG/KG/S)      '
       DSL(246)='VERTICAL DIFFUSION HEATING RATE (KG/KG/S)         '
       DSL(247)='VERTICAL DIFFUSION ZONAL ACCELERATION (M/S/S)     '
       DSL(248)='VERTICAL DIFFUSION MERID ACCELERATION (M/S/S)     '
       DSL(249)='VERTICAL DIFFUSION MOISTENING RATE (KG/KG/S)      '
       DSL(250)='SOLAR RADIATIVE HEATING RATE (K/S)                '
       DSL(251)='LONGWAVE RADIATIVE HEATING RATE (K/S)             '
       DSL(252)='DRAG COEFFICIENT ()                               '
       DSL(253)='FRICTION VELOCITY (M/S)                           '
       DSL(254)='RICHARDSON NUMBER ()                              '
		!                                                  '
      RETURN
      END
