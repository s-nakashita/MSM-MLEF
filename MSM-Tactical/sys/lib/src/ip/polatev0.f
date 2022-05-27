C-----------------------------------------------------------------------
      SUBROUTINE POLATEV0(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
     &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLATEV0   INTERPOLATE VECTOR FIELDS (BILINEAR)
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS BILINEAR INTERPOLATION
C           FROM ANY GRID TO ANY GRID FOR VECTOR FIELDS.
C           OPTIONS ALLOW VARYING THE MINIMUM PERCENTAGE FOR MASK,
C           I.E. PERCENT VALID INPUT DATA REQUIRED TO MAKE OUTPUT DATA,
C           (IPOPT(1)) WHICH DEFAULTS TO 50 (IF IPOPT(1)=-1).
C           ONLY HORIZONTAL INTERPOLATION IS PERFORMED.
C           THE GRIDS ARE DEFINED BY THEIR GRID DESCRIPTION SECTIONS
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63).
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL (SPECTRAL NATIVE)
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL (ETA NATIVE)
C           WHERE KGDS COULD BE EITHER INPUT KGDSI OR OUTPUT KGDSO.
C           THE INPUT AND OUTPUT VECTORS ARE ROTATED SO THAT THEY ARE
C           EITHER RESOLVED RELATIVE TO THE DEFINED GRID
C           IN THE DIRECTION OF INCREASING X AND Y COORDINATES
C           OR RESOLVED RELATIVE TO EASTERLY AND NORTHERLY DIRECTIONS,
C           AS DESIGNATED BY THEIR RESPECTIVE GRID DESCRIPTION SECTIONS.
C           AS AN ADDED BONUS THE NUMBER OF OUTPUT GRID POINTS
C           AND THEIR LATITUDES AND LONGITUDES ARE ALSO RETURNED
C           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
C           ON THE OTHER HAND, THE OUTPUT CAN BE A SET OF STATION POINTS
C           IF KGDSO(1)<0, IN WHICH CASE THE NUMBER OF POINTS
C           AND THEIR LATITUDES AND LONGITUDES MUST BE INPUT 
C           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
C           INPUT BITMAPS WILL BE INTERPOLATED TO OUTPUT BITMAPS.
C           OUTPUT BITMAPS WILL ALSO BE CREATED WHEN THE OUTPUT GRID
C           EXTENDS OUTSIDE OF THE DOMAIN OF THE INPUT GRID.
C           THE OUTPUT FIELD IS SET TO 0 WHERE THE OUTPUT BITMAP IS OFF.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C 1999-04-08  IREDELL  SPLIT IJKGDS INTO TWO PIECES
C 2001-06-18  IREDELL  INCLUDE MINIMUM MASK PERCENTAGE OPTION
C 2002-01-17  IREDELL  SAVE DATA FROM LAST CALL FOR OPTIMIZATION
C 2007-05-22  IREDELL  EXTRAPOLATE UP TO HALF A GRID CELL
C
C USAGE:    CALL POLATEV0(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
C    &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C
C   INPUT ARGUMENT LIST:
C     IPOPT    - INTEGER (20) INTERPOLATION OPTIONS
C                IPOPT(1) IS MINIMUM PERCENTAGE FOR MASK
C                (DEFAULTS TO 50 IF IPOPT(1)=-1)
C     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
C     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
C                (KGDSO(1)<0 IMPLIES RANDOM STATION POINTS)
C     MI       - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF INPUT GRID FIELDS IF KM=1
C     MO       - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF OUTPUT GRID FIELDS IF KM=1
C     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
C     IBI      - INTEGER (KM) INPUT BITMAP FLAGS
C     LI       - LOGICAL*1 (MI,KM) INPUT BITMAPS (IF SOME IBI(K)=1)
C     UI       - REAL (MI,KM) INPUT U-COMPONENT FIELDS TO INTERPOLATE
C     VI       - REAL (MI,KM) INPUT V-COMPONENT FIELDS TO INTERPOLATE
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)<0)
C     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
C     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
C     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)<0)
C     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)<0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C   OUTPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
C     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
C     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
C     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)>=0)
C     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)>=0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C     IBO      - INTEGER (KM) OUTPUT BITMAP FLAGS
C     LO       - LOGICAL*1 (MO,KM) OUTPUT BITMAPS (ALWAYS OUTPUT)
C     UO       - REAL (MO,KM) OUTPUT U-COMPONENT FIELDS INTERPOLATED
C     VO       - REAL (MO,KM) OUTPUT V-COMPONENT FIELDS INTERPOLATED
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL INTERPOLATION
C                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
C                3    UNRECOGNIZED OUTPUT GRID
C
C SUBPROGRAMS CALLED:
C   GDSWIZ       GRID DESCRIPTION SECTION WIZARD
C   IJKGDS0      SET UP PARAMETERS FOR IJKGDS1
C   (IJKGDS1)    RETURN FIELD POSITION FOR A GIVEN GRID POINT
C   (MOVECT)     MOVE A VECTOR ALONG A GREAT CIRCLE
C   POLFIXV      MAKE MULTIPLE POLE VECTOR VALUES CONSISTENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
CFPP$ EXPAND(IJKGDS1,MOVECT)
      INTEGER IPOPT(20)
      INTEGER KGDSI(200),KGDSO(200)
      INTEGER IBI(KM),IBO(KM)
      LOGICAL*1 LI(MI,KM),LO(MO,KM)
      REAL UI(MI,KM),VI(MI,KM),UO(MO,KM),VO(MO,KM)
      REAL RLAT(MO),RLON(MO)
      REAL CROT(MO),SROT(MO)
      REAL XPTS(MO),YPTS(MO)
      REAL WO(MO)
      INTEGER N11(MO),N21(MO),N12(MO),N22(MO)
      REAL W11(MO),W21(MO),W12(MO),W22(MO)
      REAL C11(MO),C21(MO),C12(MO),C22(MO)
      REAL S11(MO),S21(MO),S12(MO),S22(MO)
      INTEGER IJKGDSA(20)
      PARAMETER(FILL=-9999.)
      INTEGER,SAVE:: MIX=-1,KGDSIX(200)=-1
      REAL,ALLOCATABLE,SAVE:: XPTI(:),YPTI(:),RLOI(:),RLAI(:),
     &                        CROI(:),SROI(:)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
      IRET=0
      IF(KGDSO(1).GE.0) THEN
        CALL GDSWIZ(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,1,CROT,SROT)
        IF(NO.EQ.0) IRET=3
      ENDIF
      IF(ANY(KGDSI.NE.KGDSIX)) THEN
        IF(MIX.NE.MI) THEN
          IF(MIX.GE.0) DEALLOCATE(XPTI,YPTI,RLOI,RLAI,CROI,SROI)
          ALLOCATE(XPTI(MI),YPTI(MI),RLOI(MI),RLAI(MI),
     &             CROI(MI),SROI(MI))
          MIX=MI
        ENDIF
        CALL GDSWIZ(KGDSI, 0,MI,FILL,XPTI,YPTI,RLOI,RLAI,NV,1,CROI,SROI)
        KGDSIX=KGDSI
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET PARAMETERS
      MP=IPOPT(1)
      IF(MP.EQ.-1.OR.MP.EQ.0) MP=50
      IF(MP.LT.0.OR.MP.GT.100) IRET=32
      PMP=MP*0.01
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS AND ROTATIONS
      CALL GDSWIZ(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV,0,DUM,DUM)
      IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
      CALL IJKGDS0(KGDSI,IJKGDSA)
      DO N=1,NO
        XI=XPTS(N)
        YI=YPTS(N)
        IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
          I1=XI
          I2=I1+1
          J1=YI
          J2=J1+1
          XF=XI-I1
          YF=YI-J1
          N11(N)=IJKGDS1(I1,J1,IJKGDSA)
          N21(N)=IJKGDS1(I2,J1,IJKGDSA)
          N12(N)=IJKGDS1(I1,J2,IJKGDSA)
          N22(N)=IJKGDS1(I2,J2,IJKGDSA)
          IF(N11(N).GT.0) THEN
            W11(N)=(1-XF)*(1-YF)
            CALL MOVECT(RLAI(N11(N)),RLOI(N11(N)),RLAT(N),RLON(N),
     &                  CM11,SM11)
            C11(N)=CM11*CROI(N11(N))+SM11*SROI(N11(N))
            S11(N)=SM11*CROI(N11(N))-CM11*SROI(N11(N))
          ELSE
            W11(N)=0.
            C11(N)=0.
            S11(N)=0.
          ENDIF
          IF(N21(N).GT.0) THEN
            W21(N)=XF*(1-YF)
            CALL MOVECT(RLAI(N21(N)),RLOI(N21(N)),RLAT(N),RLON(N),
     &                  CM21,SM21)
            C21(N)=CM21*CROI(N21(N))+SM21*SROI(N21(N))
            S21(N)=SM21*CROI(N21(N))-CM21*SROI(N21(N))
          ELSE
            W21(N)=0.
            C21(N)=0.
            S21(N)=0.
          ENDIF
          IF(N12(N).GT.0) THEN
            W12(N)=(1-XF)*YF
            CALL MOVECT(RLAI(N12(N)),RLOI(N12(N)),RLAT(N),RLON(N),
     &                  CM12,SM12)
            C12(N)=CM12*CROI(N12(N))+SM12*SROI(N12(N))
            S12(N)=SM12*CROI(N12(N))-CM12*SROI(N12(N))
          ELSE
            W12(N)=0.
            C12(N)=0.
            S12(N)=0.
          ENDIF
          IF(N22(N).GT.0) THEN
            W22(N)=XF*YF
            CALL MOVECT(RLAI(N22(N)),RLOI(N22(N)),RLAT(N),RLON(N),
     &                  CM22,SM22)
            C22(N)=CM22*CROI(N22(N))+SM22*SROI(N22(N))
            S22(N)=SM22*CROI(N22(N))-CM22*SROI(N22(N))
          ELSE
            W22(N)=0.
            C22(N)=0.
            S22(N)=0.
          ENDIF
        ELSE
          N11(N)=0
          N21(N)=0
          N12(N)=0
          N22(N)=0
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE WITH OR WITHOUT BITMAPS
CMIC$ DO ALL AUTOSCOPE PRIVATE(WO)
      DO K=1,KM
        DO N=1,NO
          UO(N,K)=0
          VO(N,K)=0
          WO(N)=0
          IF(N11(N).GT.0.AND.(IBI(K).EQ.0.OR.LI(N11(N),K))) THEN
            U11=C11(N)*UI(N11(N),K)-S11(N)*VI(N11(N),K)
            V11=S11(N)*UI(N11(N),K)+C11(N)*VI(N11(N),K)
            UO(N,K)=UO(N,K)+W11(N)*U11
            VO(N,K)=VO(N,K)+W11(N)*V11
            WO(N)=WO(N)+W11(N)
          ENDIF
          IF(N21(N).GT.0.AND.(IBI(K).EQ.0.OR.LI(N21(N),K))) THEN
            U21=C21(N)*UI(N21(N),K)-S21(N)*VI(N21(N),K)
            V21=S21(N)*UI(N21(N),K)+C21(N)*VI(N21(N),K)
            UO(N,K)=UO(N,K)+W21(N)*U21
            VO(N,K)=VO(N,K)+W21(N)*V21
            WO(N)=WO(N)+W21(N)
          ENDIF
          IF(N12(N).GT.0.AND.(IBI(K).EQ.0.OR.LI(N12(N),K))) THEN
            U12=C12(N)*UI(N12(N),K)-S12(N)*VI(N12(N),K)
            V12=S12(N)*UI(N12(N),K)+C12(N)*VI(N12(N),K)
            UO(N,K)=UO(N,K)+W12(N)*U12
            VO(N,K)=VO(N,K)+W12(N)*V12
            WO(N)=WO(N)+W12(N)
          ENDIF
          IF(N22(N).GT.0.AND.(IBI(K).EQ.0.OR.LI(N22(N),K))) THEN
            U22=C22(N)*UI(N22(N),K)-S22(N)*VI(N22(N),K)
            V22=S22(N)*UI(N22(N),K)+C22(N)*VI(N22(N),K)
            UO(N,K)=UO(N,K)+W22(N)*U22
            VO(N,K)=VO(N,K)+W22(N)*V22
            WO(N)=WO(N)+W22(N)
          ENDIF
        ENDDO
        IBO(K)=IBI(K)
        DO N=1,NO
          LO(N,K)=WO(N).GE.PMP
          IF(LO(N,K)) THEN
            UO(N,K)=UO(N,K)/WO(N)
            VO(N,K)=VO(N,K)/WO(N)
            UROT=CROT(N)*UO(N,K)-SROT(N)*VO(N,K)
            VROT=SROT(N)*UO(N,K)+CROT(N)*VO(N,K)
            UO(N,K)=UROT
            VO(N,K)=VROT
          ELSE
            IBO(K)=1
            UO(N,K)=0.
            VO(N,K)=0.
          ENDIF
        ENDDO
      ENDDO
      IF(KGDSO(1).EQ.0) CALL POLFIXV(NO,MO,KM,RLAT,RLON,IBO,LO,UO,VO)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
