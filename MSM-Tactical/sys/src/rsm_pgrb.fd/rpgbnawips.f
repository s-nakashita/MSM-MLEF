!-----------------------------------------------------------------------
      PROGRAM RPGB                                                      
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
!                                                                       
! MAIN PROGRAM:  RPGB        TRANSFORM SIGMA TO PRESSURE GRIB           
!                            FOR REGIONAL SPECTRAL MODEL                
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31            
!                                                                       
! ABSTRACT: PROGRAM TRANSFORMS SIGMA INPUT TO PRESSURE GRIB OUTPUT.     
!   THE OUTPUT CONSISTS OF DATA ON A REGULAR LAT/LON GRID.              
!   GEOPOTENTIAL HEIGHT, WIND COMPONENTS, RELATIVE HUMIDITY,            
!   TEMPERATURE AND VERTICAL VELOCITY ARE OUTPUT ON MANDATORY PRESSURES.
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
!   92-10-31  IREDELL                                                   
!   93-08-16  H.-M.H.JUANG                                              
!   94-08-26  H.-M.H.JUANG CORRECT ISACN AND ADD MERCATER               
!   94-10-12  H.-M.H.JUANG UPDATE TO HAVE MORE FIELDS                   
!   94-10-28  H.-M.H.JUANG UPDATE TO HAVE MORE FIELDS                   
!   94-11-01  H.-M.H.JUANG ADD MORE PRESSURE LAYERS                     
!   95-11-01  H.-M.H.JUANG MODIFY TO BE WORKSTATION VERSION
!   96-08-29  H.-M.H.JUANG ADD CONTROL FILE FOR GRADS
!   97-05-22  H.-M.H.JUANG ADD NONHYDROSTATIC VERSION
!                                                                       
! NAMELISTS:                                                            
!   NAMPGB:      PARAMETERS DETERMINING OUTPUT FORMAT                   
!     IO         NUMBER OF OUTPUT LONGITUDE POINTS                      
!     JO         NUMBER OF OUTPUT LATITUDE POINTS                       
!     KO         NUMBER OF OUTPUT PRESSURE LEVELS                       
!     PO(KO)     PRESSURES IN MB (DEFAULT: MANDATORY LEVELS)            
!     KZZ        NUMBER OF CONSTANT HEIGHT LEVELS (DEFAULT: 3)
!     ZZ(KZZ)    CONSTANT HEIGHTS IN M (DEFAULT: 1829.,2743.,3658.)
!     NROW       NUMBER OF REQUIRED ROW FOR COMPUTING (DEFAULT: 4)
!     MXBIT      MAXIMUM NUMBER OF BITS TO PACK DATA (DEFAULT: 16)      
!     IDS(255)   DECIMAL SCALING OF PACKED DATA                         
!                (DEFAULT: SET BY SUBPROGRAM IDSDEF)                    
!     POT(255)   HIGHEST PRESSURE IN MB TO OUTPUT DATA                  
!                AS A FUNCTION OF PARAMETER INDICATOR                   
!                (DEFAULT: 300 FOR RH, 100 FOR OMEGA, 0 OTHERWISE)      
!     ICEN       FORECAST CENTER IDENTIFIER (DEFAULT: 7)                
!     IGEN       MODEL GENERATING CODE (DEFAULT: FROM SIGMA FILE)       
!     CRUN       6 CHARACTER RUN IDENTIFIER (DEFAULT: '      ')         
!                                                                       
! INPUT FILES:                                                          
!   UNIT   11-?  SIGMA FILE(S)                                          
!                                                                       
! OUTPUT FILES:                                                         
!   UNIT   51-?  PRESSURE GRIB1 FILE(S)                                 
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   IDSDEF       SET DEFAULTS FOR DECIMAL SCALING                       
!   GPVS         COMPUTE SATURATED VAPOR PRESSURE TABLE                 
!   GTDP         COMPUTE DEWPOINT TEMERATURE TABLE                      
!   GTHE         COMPUTE EQUIVALENT POTENTIAL TEMPERATURE TABLE         
!   GTMA         COMPUTE MOIST ADIABAT TABLE                            
!   RRDSGH       READ A SIGMA FILE HEADER                               
!   RPGB1        TRANSFORM ONE SIGMA FILE TO PRESSURE GRIB              
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      PARAMETER(LEVO=20,LEVT=6,LEVZ=3,NROW=4,MYBIT=16)
      PARAMETER(LEVMAX=100,KOMAX=100)                                   
      DIMENSION IDATE(4)                                                
      DIMENSION SI(LEVMAX+1),SL(LEVMAX)                                 
      DIMENSION PO(KOMAX),ZZ(KOMAX)                                     
      DIMENSION IDS(255),POT(255)
      NAMELIST/NAMPGB/ IO,JO,KO,PO,KT,PT,NCPUS,MXBIT,IDS,POT,KTT,       & 
     &                 ICEN,ICEN2,IGEN,KZZ,ZZ,                          & 
     &                 PROJO,TRUTHO,COTRUO,ORIENTO,IGRIDO,              &
     &                 DELXO,DELYO,RLAT1O,RLON1O,RLAT2O,RLON2O,         &
     &                 NTRAC,NCLD,NONHYD,NEWSLM
      DATA IO/0/,JO/0/,KO/0/                                            
      DATA PROJO/-999./,TRUTHO/-999./,COTRUO/-999./,ORIENTO/-999./      
      DATA DELXO/-999./,DELYO/-999./,RLAT1O/-999./,RLON1O/-999./        
      DATA RLAT2O/-999./,RLON2O/-999./,IGRIDO/255/                      
      DATA PO/1000.,950.,925.,900.,850.,800.,750.,700.,650.,600.,       &
     &         550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,       &
     &          70., 50., 30., 20., 10.,  7.,  5.,  3.,  2.,  1.,       &
     &     70*0./                                                       
!     DATA PO/1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,  
!    &        70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,0.4,78*0./             
      DATA KT/LEVT/,PT/30./                                                
      DATA KZZ/LEVZ/,ZZ/1829.,2743.,3658.,97*0./
      DATA NCPUS/NROW/,MXBIT/MYBIT/                                           
      DATA IDS/255*0/,POT/255*0./,KTT/1/                                
      DATA ICEN/7/,ICEN2/0/,IGEN/0/
      DATA NEWSLM/0/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!hmhj CALL W3TAGB('RSMPGRB ',1998,0139,0061,'NP20   ')
!
      PRINT *,' ++++++++ START RPGB +++++++++++'                        
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SET DEFAULTS AND READ NAMELIST                                       
      PRINT *,' SET DEFAULTS '                                          
      CALL IDSDEF(2,IDS)                                                
      CALL CTLINI                                                       
! change without limitation on Feb 21 1996                              
!X    POT(52)=300.                                                      
!X    POT(39)=100.                                                      
      PRINT *,' before READ NAMELIST '                                         
      READ(*,NAMPGB,END=5)                                              
!     READ(5,NAMPGB)                                              
!     print*,IO,JO,KO,PO,KT,PT,NCPUS,MXBIT,IDS,POT,KTT,
!    &                 ICEN,ICEN2,IGEN,NTRAC,NCLD,NONHYD
!     PRINT *,' after READ NAMELIST '                                         
!     RAD=ACOS(-1.)/180.                                                
!     RLAT1O=RLAT1O*RAD                                                 
!     RLAT2O=RLAT2O*RAD                                                 
!     RLON1O=RLON1O*RAD                                                 
!     RLON2O=RLON2O*RAD                                                 
5     CONTINUE                                                          
!     WRITE(*,NAMPGB)                                                   
!     WRITE(6,NAMPGB)                                                   
      IF (PROJO.EQ.4) THEN
         ORIENTO=(RLON2O-RLON1O)/2.0
         TRUTHO=(RLAT2O-RLAT1O)/2.0
         print*,'ORIENTO,TRUTHO=',ORIENTO,TRUTHO
      ENDIF
      IGEN0=IGEN                                                        
      print*,'before call GPVS'
      CALL GPVS                                                         
      print*,'before call GTDP'
      CALL GTDP                                                         
      print*,'before call GTHE'
      CALL GTHE                                                         
      print*,'before call GTMA'
      CALL GTMA                                                         
      NSIG=11                                                           
      NFLX=21                                                           
      NPGB=51                                                           
      NFGB=56                                                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  READ FIRST SIGMA HEADER RECORD                                       
      PRINT *,' READ FIRST SIGMA HEADER RECORD '                        
      CALL RRDSGH(NSIG,FHOUR,IDATE,SI,SL,                               &
     &                  IWAV1,JWAV1,IGRD1,JGRD1,LEVS,NONHYD,NFLDS,      &
     &                  PROJ,TRUTH,ORIENT,CENLAT,CENLON,                &
     &                  GRDLEFT,GRDBOTM,DELX,DELY,IRET,NCLD)             
      PRINT *,' RETURN FROM RRDSGH IS ',IRET                            
      IGSGR=1                                                           
      JCAP=SQRT((IWAV1-1)**2.+(JWAV1-1)**2.)                            
      IGRD11=IGRD1/4                                                    
      IF(MOD(IGRD1,4).EQ.0) THEN                                        
         IGRD11=IGRD11*4                                                
      ELSE                                                              
         IGRD11=(IGRD11+1)*4                                            
      ENDIF                                                             
      PRINT *,' IGRD1=',IGRD1,' IGRD11(FOR PACK)=',IGRD11               
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  TRANSFORM TO PRESSURE GRIB AND ATTEMPT TO READ NEXT SIGMA HEADER     
      DOWHILE(IRET.EQ.0)                                                
        IF(IGEN0.EQ.0) IGEN=99                                          
        IF(IO.EQ.0) IO=IGRD1                                            
        IF(JO.EQ.0) JO=JGRD1                                            
        IF(KO.EQ.0) KO=20
!       IF(KO.EQ.0) THEN                                                
!         IF(LEVS.EQ.18) THEN                                           
!           KO=13                                                       
!         ELSEIF(LEVS.EQ.28) THEN                                       
!           KO=16                                                       
!         ELSE                                                          
!           KO=16                                                       
!         ENDIF                                                         
!       ENDIF                                                           
!  PRESET DIMENSION OF NFLDS FOR GRID DATA FROM RRDSGR                  
        IYMDH=IDATE(4)*1000000+IDATE(2)*10000+IDATE(3)*100+IDATE(1)     
        PRINT *,' RSM POSTING DATE ',IYMDH,'+',NINT(FHOUR),             &
     &          ' FROM SIGMA TO PRESSURE GRID ',IO,'*',JO,'*',KO        
        print *,'nflds',nflds
        CALL RPGB1(FHOUR,IDATE,                                         &
     &            NSIG,NFLX,IGSGR,JCAP,IWAV1,JWAV1,                     &
     &            IGRD1,IGRD11,JGRD1,LEVS,NFLDS,SI,SL,                  &
     &            IO,JO,KO,PO,KT,PT,KZZ,ZZ,NONHYD,                      &
     &            PROJ,TRUTH,ORIENT,CENLAT,CENLON,                      &
     &            GRDLEFT,GRDBOTM,DELX,DELY,                            &
     &            NPGB,NFGB,NCPUS,MXBIT,IDS,POT,KTT,                    &
     &            ICEN,ICEN2,IGEN,IGRIDO,                               &
     &            PROJO,ORIENTO,TRUTHO,COTRUO,DELXO,DELYO,              &
     &            RLAT1O,RLON1O,RLAT2O,RLON2O,                          &
     &            NTRAC,NCLD,NEWSLM)

        CLOSE(NSIG)                                                     
        CLOSE(NFLX)                                                     
        CLOSE(NPGB)                                                     
        CLOSE(NFGB)                                                     
        PRINT *,' ---- FINISH RPGB1 FOR UNIT = ',NSIG                   
        PRINT *,' ---- WRITE PRESS AND FLUX GRIB IN UNIT = ',NPGB       
        PRINT *,' ---- WRITE FLUX  GRIB IN UNIT = ',NFGB                
        NSIG=NSIG+1                                                     
        NFLX=NFLX+1                                                     
        NPGB=NPGB+1                                                     
        NFGB=NFGB+1                                                     
        PRINT *,' STAR NEXT FILE, UNIT = ',NSIG                         
        CALL RRDSGH(NSIG,FHOUR,IDATE,SI,SL,                              &
     &                  IWAV1,JWAV1,IGRD1,JGRD1,LEVS,NONHYD,NFLDS,       &
     &                  PROJ,TRUTH,ORIENT,CENLAT,CENLON,                 &
     &                  GRDLEFT,GRDBOTM,DELX,DELY,IRET,NCLD)             
        PRINT *,' RETURN FROM RRDSGH IS ',IRET                          

      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!hmhj CALL W3TAGE('RSMPGRB ')
      STOP                                                              
      END                                                               
!--------------------------------------------------------------------   
      FUNCTION FPKAP(P)                                                 
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK                                 
!                                                                       
! SUBPROGRAM: FPKAP        RAISE SURFACE PRESSURE TO THE KAPPA POWER.   
!   AUTHOR: PHILLIPS         ORG: W/NMC2X2   DATE: 29 DEC 82            
!                                                                       
! ABSTRACT: RAISE SURFACE PRESSURE OVER 100 KPA TO THE KAPPA POWER      
!   USING THE RATIO OF TWO POLYNOMIALS IN PRESSURE. THE POLYNOMIAL      
!   COEFFICIENTS WERE OBTAINED FROM THE IMSL PROGRAM IRATCU             
!   WITH INPUT P/100 RANGE OF 0.5-1.1 AND KAPPA EQUAL TO 0.2856219.     
!   THE ACCURACY IS ABOUT THE SAME AS 32-BIT ARITHMETIC.                
!   THIS FUNCTION CAN BE EXPANDED INLINE IN CALLING ROUTINE.            
!                                                                       
! USAGE:  PKAP=FPKAP(P)                                                 
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     P        - REAL SURFACE PRESSURE IN KILOPASCALS (CB)              
!                P SHOULD BE IN THE RANGE 50. TO 110.                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FPKAP    - REAL P/100 TO THE KAPPA POWER                          
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(CN0=3.47575490E-1,CN1=4.36732956E-2,CN2= 3.91557032E-4,  &
     &   CD0=1.,CD1=5.44053037E-2,CD2=2.27693825E-4,CD3=-8.69930591E-8) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      FPKAP=(CN0+P*(CN1+P*CN2))/(CD0+P*(CD1+P*(CD2+P*CD3)))             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      FUNCTION FPVS(T)                                                  
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: FPVS         COMPUTE SATURATION VAPOR PRESSURE            
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.     
!   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE     
!   COMPUTED IN GPVS. SEE DOCUMENTATION FOR GPVS FOR DETAILS.           
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.        
!   THIS FUNCTION CAN BE EXPANDED INLINE IN CALLING ROUTINE.            
!                                                                       
! USAGE:   PVS=FPVS(T)                                                  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION          
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     T        - REAL TEMPERATURE IN KELVIN                             
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FPVS     - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)     
!                                                                       
! COMMON BLOCKS:                                                        
!   COMPVS   - SCALING PARAMETERS AND TABLE COMPUTED IN GPVS.           
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      PARAMETER(NX=1501)                                                
      DIMENSION TBPVS(NX)                                               
      COMMON/COMPVS/ C1XPVS,C2XPVS,ANXPVS,TBPVS                         
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),ANXPVS)                            
      JX=XJ                                                             
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))                    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      FUNCTION FTDP(PV)                                                 
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: FTDP         COMPUTE SATURATION VAPOR PRESSURE            
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE DEWPOINT TEMPERATURE FROM VAPOR PRESSURE.           
!   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE     
!   COMPUTED IN GTDP. SEE DOCUMENTATION FOR GTDP FOR DETAILS.           
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.        
!   THIS FUNCTION CAN BE EXPANDED INLINE IN CALLING ROUTINE.            
!                                                                       
! USAGE:   TDP=FTDP(PV)                                                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION          
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     PV       - REAL VAPOR PRESSURE IN KILOPASCALS (CB)                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FTDP     - REAL DEWPOINT TEMPERATURE IN KELVIN                    
!                                                                       
! COMMON BLOCKS:                                                        
!   COMTDP   - SCALING PARAMETERS AND TABLE COMPUTED IN GTDP.           
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(NX=2000)                                                
      DIMENSION TBTDP(NX)                                               
      COMMON/COMTDP/ C1XTDP,C2XTDP,ANXTDP,TBTDP                         
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XJ=MIN(MAX(C1XTDP+C2XTDP*PV,1.),ANXTDP)                           
      JX=XJ                                                             
      FTDP=TBTDP(JX)+(XJ-JX)*(TBTDP(JX+1)-TBTDP(JX))                    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      FUNCTION FTHE(T,PK)                                               
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: FTHE         COMPUTE SATURATION VAPOR PRESSURE            
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE EQUIVALENT POTENTIAL TEMPERATURE AT THE LCL         
!   FROM TEMPERATURE AND PRESSURE OVER 100 KPA TO THE KAPPA POWER.      
!   A BILINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE   
!   COMPUTED IN GTHE. SEE DOCUMENTATION FOR GTHE FOR DETAILS.           
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA,        
!   EXCEPT ZERO IS RETURNED FOR TOO COLD OR HIGH LCLS.                  
!   THIS FUNCTION CAN BE EXPANDED INLINE IN CALLING ROUTINE.            
!                                                                       
! USAGE:   THE=FTHE(PV)                                                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION          
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     T        - REAL LCL TEMPERATURE IN KELVIN                         
!     PK       - REAL LCL PRESSURE OVER 100 KPA TO THE KAPPA POWER      
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FTHE     - REAL EQUIVALENT POTENTIAL TEMPERATURE IN KELVIN        
!                                                                       
! COMMON BLOCKS:                                                        
!   COMTHE   - SCALING PARAMETERS AND TABLE COMPUTED IN GTHE.           
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(NX=101,NY=25)                                           
      DIMENSION TBTHE(NX,NY)                                            
      COMMON/COMTHE/ C1XTHE,C2XTHE,ANXTHE,C1YTHE,C2YTHE,ANYTHE,TBTHE    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XJ=MIN(C1XTHE+C2XTHE*T,ANXTHE)                                    
      YJ=MIN(C1YTHE+C2YTHE*PK,ANYTHE)                                   
      IF(XJ.GE.1..AND.YJ.GE.1.) THEN                                    
        JX=XJ                                                           
        JY=YJ                                                           
        F1=TBTHE(JX,JY)+(XJ-JX)*(TBTHE(JX+1,JY)-TBTHE(JX,JY))           
        F2=TBTHE(JX,JY+1)+(XJ-JX)*(TBTHE(JX+1,JY+1)-TBTHE(JX,JY+1))     
        FTHE=F1+(YJ-JY)*(F2-F1)                                         
      ELSE                                                              
        FTHE=0.                                                         
      ENDIF                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      FUNCTION FTLCL(T,TDPD)                                            
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK                                 
!                                                                       
! SUBPROGRAM: FTLCL        COMPUTE LCL TEMPERATURE.                     
!   AUTHOR: PHILLIPS         ORG: W/NMC2X2   DATE: 29 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE TEMPERATURE AT THE LIFTING CONDENSATION LEVEL       
!   FROM TEMPERATURE AND DEWPOINT DEPRESSION. THE FORMULA USED IS       
!   A POLYNOMIAL TAKEN FROM PHILLIPS MSTADB ROUTINE. ITS ACCURAY IS     
!   ON THE ORDER OF 0.03 KELVIN FOR A DEWPOINT DEPRESSION OF 30 KELVIN. 
!   THIS FUNCTION CAN BE EXPANDED INLINE IN CALLING ROUTINE.            
!                                                                       
! USAGE:  TLCL=FTLCL(T,TDPD)                                            
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     T        - REAL TEMPERATURE IN KELVIN                             
!     TDPD     - REAL DEWPOINT DEPRESSION IN KELVIN                     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FTLCL    - REAL TEMPERATURE AT THE LCL IN KELVIN                  
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(CLCL1= 0.954442E+0,CLCL2= 0.967772E-3,                   &
     &          CLCL3=-0.710321E-3,CLCL4=-0.270742E-5)                  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      FTLCL=T-TDPD*(CLCL1+CLCL2*T+TDPD*(CLCL3+CLCL4*T))                 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      FUNCTION FTMA(THE,PK,QMA)                                         
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: FTMA         COMPUTE MOIST ADIABAT TEMPERATURE            
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE TEMPERATURE AND SPECIFIC HUMIDITY OF A PARCEL       
!   LIFTED UP A MOIST ADIABAT FROM EQUIVALENT POTENTIAL TEMPERATURE     
!   AT THE LCL AND PRESSURE OVER 100 KPA TO THE KAPPA POWER.            
!   A BILINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE   
!   COMPUTED IN GTMA. SEE DOCUMENTATION FOR GTMA FOR DETAILS.           
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.        
!   THIS FUNCTION CAN BE EXPANDED INLINE IN CALLING ROUTINE.            
!                                                                       
! USAGE:   TMA=FTMA(THE,PK,QMA)                                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION          
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     THE      - REAL EQUIVALENT POTENTIAL TEMPERATURE IN KELVIN        
!     PK       - REAL PRESSURE OVER 100 KPA TO THE KAPPA POWER          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FTMA     - REAL PARCEL TEMPERATURE IN KELVIN                      
!     QMA      - REAL PARCEL SPECIFIC HUMIDITY IN KG/KG                 
!                                                                       
! COMMON BLOCKS:                                                        
!   COMTMA   - SCALING PARAMETERS AND TABLE COMPUTED IN GTMA.           
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(NX=61,NY=51)                                            
      DIMENSION TBTMA(NX,NY),TBQMA(NX,NY)                               
      COMMON/COMMA/ C1XMA,C2XMA,ANXMA,C1YMA,C2YMA,ANYMA,TBTMA,TBQMA     
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XJ=MIN(MAX(C1XMA+C2XMA*THE,1.),ANXMA)                             
      YJ=MIN(MAX(C1YMA+C2YMA*PK,1.),ANYMA)                              
      JX=XJ                                                             
      JY=YJ                                                             
      F1=TBTMA(JX,JY)+(XJ-JX)*(TBTMA(JX+1,JY)-TBTMA(JX,JY))             
      F2=TBTMA(JX,JY+1)+(XJ-JX)*(TBTMA(JX+1,JY+1)-TBTMA(JX,JY+1))       
      FTMA=F1+(YJ-JY)*(F2-F1)                                           
      F1=TBQMA(JX,JY)+(XJ-JX)*(TBQMA(JX+1,JY)-TBQMA(JX,JY))             
      F2=TBQMA(JX,JY+1)+(XJ-JX)*(TBQMA(JX+1,JY+1)-TBQMA(JX,JY+1))       
      QMA=F1+(YJ-JY)*(F2-F1)                                            
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
!FPP$ EXPAND(FPVS)                                                      
      SUBROUTINE GETRH(IM,IX,KM,PP,Q,T,QS,R)                         
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GETRH       CALCULATE RELATIVE HUMIDITY                
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: CALCULATES RELATIVE HUMIDITY AS A FUNCTION OF PRESSURE,     
!   SPECIFIC HUMIDITY AND TEMPERATURE ON THE SIGMA LAYERS.              
!   SATURATION SPECIFIC HUMIDITY IS CALCULATED FROM SATURATION VAPOR    
!   PRESSURE WHICH IS RETURNED FROM A LOOKUP TABLE ROUTINE FPVS.        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!   97-05-22  HANN-MING HENRY JUANG MODIFY FOR NONHYDROSTATIC
!                                                                       
! USAGE:    CALL GETRH(IM,IX,KM,PP,Q,T,QS,R)                         
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF LEVELS                               
!     PP       - REAL (IM) PRESSURE IN KPA
!     Q        - REAL (IX,KM) SPECIFIC HUMIDITY IN KG/KG                
!     T        - REAL (IX,KM) TEMPERATURE IN K                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     QS       - REAL (IX,KM) SATURATED SPECIFIC HUMIDITY IN KG/KG      
!     R        - REAL (IX,KM) RELATIVE HUMIDITY IN PERCENT              
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   (FPVS)   - FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE            
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION PP(IX,KM),Q(IX,KM),T(IX,KM),QS(IX,KM),R(IX,KM)      
      PARAMETER(RD= 2.8705E+2 ,RV= 4.6150E+2 ,EPS=RD/RV,EPSM1=RD/RV-1.) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      DO K=1,KM                                                         
        DO I=1,IM                                                       
          ES=FPVS(T(I,K))                                               
          QS(I,K)=EPS*ES/(PP(I,K)+EPSM1*ES)                         
          R(I,K)=MIN(MAX(Q(I,K)/QS(I,K),0.),1.)*100.                    
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE GPVS                                                   
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: GPVS         COMPUTE SATURATION VAPOR PRESSURE TABLE      
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF    
!   TEMPERATURE FOR FUNCTION FPVS. THE WATER MODEL ASSUMES A PERFECT GAS
!   CONSTANT SPECIFIC HEATS FOR GAS AND LIQUID, AND NEGLECTS            
!   THE VOLUME OF THE LIQUID. THE ICE OPTION IS NO LONGER INCLUDED.     
!   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT         
!   OF CONDENSATION WITH TEMPERATURE. THE CLAUSIUS-CLAPEYRON EQUATION   
!   IS INTEGRATED FROM THE TRIPLE POINT TO GET THE FORMULA              
!       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))                              
!   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS           
!   DEFINED IN PARAMETER STATEMENTS IN THE CODE.                        
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH           
!   OF 1501 FOR TEMPERATURES RANGING FROM 180. TO 330. KELVIN.          
!                                                                       
! USAGE:  CALL GPVS                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL                                                   
!                                                                       
! COMMON BLOCKS:                                                        
!   COMPVS   - SCALING PARAMETERS AND TABLE FOR FUNCTION FPVS.          
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      PARAMETER(CP= 1.0046E+3 ,RD= 2.8705E+2 ,RV= 4.6150E+2 ,            &
     &          TTP= 2.7316E+2 ,HVAP= 2.5000E+6 ,PSATK= 6.1078E+2 *1.E-3 &
     &,                                                                  &
     &          CLIQ= 4.1855E+3 ,CVAP= 1.8460E+3 )                      
      PARAMETER(DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))         
      PARAMETER(NX=1501)                                                
      DIMENSION TBPVS(NX)                                               
      COMMON/COMPVS/ C1XPVS,C2XPVS,ANXPVS,TBPVS                         
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XMIN=180.0                                                        
      XMAX=330.0                                                        
      XINC=(XMAX-XMIN)/(NX-1)                                           
      C1XPVS=1.-XMIN/XINC                                               
      C2XPVS=1./XINC                                                    
      ANXPVS=NX-0.01                                                    
      DO JX=1,NX                                                        
        X=XMIN+(JX-1)*XINC                                              
        T=X                                                             
        TR=TTP/T                                                        
        TBPVS(JX)=PSATK*(TR**XA)*EXP(XB*(1.-TR))                        
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE GTDP                                                   
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: GTDP         COMPUTE DEWPOINT TEMPERATURE TABLE           
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE DEWPOINT TEMPERATURE TABLE AS A FUNCTION OF         
!   VAPOR PRESSURE FOR FUNCTION FTDP. THE WATER MODEL ASSUMES           
!   A PERFECT GAS, CONSTANT SPECIFIC HEATS FOR GAS AND LIQUID,          
!   AND NEGLECTS THE VOLUME OF THE LIQUID AND ICE FORMATION.            
!   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT         
!   OF CONDENSATION WITH TEMPERATURE. THE CLAUSIUS-CLAPEYRON EQUATION   
!   IS INTEGRATED FROM THE TRIPLE POINT TO GET THE FORMULA              
!   FOR SATURATION VAPOR PRESSURE PVS AS A FUNCTION OF TEMPERATURE T    
!       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))                              
!   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS           
!   DEFINED IN PARAMETER STATEMENTS IN THE CODE.                        
!   THE FORMULA IS INVERTED BY ITERATING NEWTONIAN APPROXIMATIONS       
!   FOR EACH PVS UNTIL T IS FOUND TO WITHIN 1.E-6 KELVIN.               
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH           
!   OF 2000 FOR VAPOR PRESSURES RANGING FROM 0.005 TO 10.000 KILOPASCALS
!   GIVING A DEWPOINT TEMPERATURE RANGE OF 221.0 TO 319.0 KELVIN.       
!                                                                       
! USAGE:  CALL GTDP                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL                                                   
!                                                                       
! COMMON BLOCKS:                                                        
!   COMTDP   - SCALING PARAMETERS AND TABLE FOR FUNCTION FTDP.          
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(CP= 1.0046E+3 ,RD= 2.8705E+2 ,RV= 4.6150E+2 ,           
     &          TTP= 2.7316E+2 ,HVAP= 2.5000E+6 ,PSATK= 6.1078E+2 *1.E-3
     &,                                                                 
     &          CLIQ= 4.1855E+3 ,CVAP= 1.8460E+3 )                      
      PARAMETER(DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))         
      PARAMETER(NX=2000)                                                
      DIMENSION TBTDP(NX)                                               
      COMMON/COMTDP/ C1XTDP,C2XTDP,ANXTDP,TBTDP                         
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XMIN= 0.005                                                       
      XMAX=10.000                                                       
      XINC=(XMAX-XMIN)/(NX-1)                                           
      C1XTDP=1.-XMIN/XINC                                               
      C2XTDP=1./XINC                                                    
      ANXTDP=NX-0.01                                                    
      TERRM=1.E-4                                                       
      T=TTP                                                             
      PVT=PSATK                                                         
      DPVT=HVAP*PSATK/(RV*TTP**2)                                       
      DO JX=1,NX                                                        
        X=XMIN+(JX-1)*XINC                                              
        PV=X                                                            
        TERR=(PVT-PV)/DPVT                                              
        DOWHILE(ABS(TERR).GT.TERRM)                                     
          T=T-TERR                                                      
          TR=TTP/T                                                      
          PVT=PSATK*(TR**XA)*EXP(XB*(1.-TR))                            
          EL=HVAP+DLDT*(T-TTP)                                          
          DPVT=EL*PVT/(RV*T**2)                                         
          TERR=(PVT-PV)/DPVT                                            
        ENDDO                                                           
        TBTDP(JX)=T-TERR                                                
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE GTHE                                                   
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: GTHE        COMPUTE EQUIVALENT POTENTIAL TEMPERATURE TABLE
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE EQUIVALENT POTENTIAL TEMPERATURE TABLE              
!   AS A FUNCTION OF LCL TEMPERATURE AND PRESSURE OVER 100 KPA          
!   TO THE KAPPA POWER FOR FUNCTION FTHE. ROSSBY SHOWED THAT THE        
!   EQUIVALENT POTENTIAL TEMPERATURE IS CONSTANT FOR A SATURATED PARCEL 
!   RISING ADIABATICALLY UP A MOIST ADIABAT WHEN THE HEAT AND MASS      
!   OF THE CONDENSED WATER ARE NEGLECTED. THE FORMULA FOR               
!   EQUIVALENT POTENTIAL TEMPERATURE (DERIVED IN HOLTON) IS             
!       THE=T*(PD**(-ROCP))*EXP(EL*EPS*PV/(CP*T*PD))                    
!   WHERE T IS THE TEMPERATURE, PV IS THE SATURATED VAPOR PRESSURE,     
!   PD IS THE DRY PRESSURE P-PV, EL IS THE TEMPERATURE DEPENDENT        
!   LATENT HEAT OF CONDENSATION HVAP+DLDT*(T-TTP), AND OTHER VALUES     
!   ARE PHYSICAL CONSTANTS DEFINED IN PARAMETER STATEMENTS IN THE CODE. 
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A FIRST DIMENSION  
!   OF 101 FOR TEMPERATURES RANGING FROM 203.16 TO 303.16 KELVIN        
!   AND A SECOND DIMENSION OF 25 FOR PRESSURE OVER 100 KPA              
!   TO THE KAPPA POWER RANGING FROM 0.1**ROCP TO 1.1**ROCP.             
!                                                                       
! USAGE:  CALL GTHE                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL                                                   
!                                                                       
! COMMON BLOCKS:                                                        
!   COMTHE   - SCALING PARAMETERS AND TABLE FOR FUNCTION FTHE.          
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(CP= 1.0046E+3 ,RD= 2.8705E+2 ,RV= 4.6150E+2 ,            &
     &          TTP= 2.7316E+2 ,HVAP= 2.5000E+6 ,PSATK= 6.1078E+2 *1.E-3 &
     &,                                                                  &
     &          CLIQ= 4.1855E+3 ,CVAP= 1.8460E+3 )                      
      PARAMETER(ROCP=RD/CP,CPOR=CP/RD,PSATB=PSATK*1.E-2,EPS=RD/RV,       &
     &          DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))         
      PARAMETER(NX=101,NY=25)                                           
      DIMENSION TBTHE(NX,NY)                                            
      COMMON/COMTHE/ C1XTHE,C2XTHE,ANXTHE,C1YTHE,C2YTHE,ANYTHE,TBTHE    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XMIN=TTP-70.                                                      
      XMAX=TTP+30.                                                      
      XINC=(XMAX-XMIN)/(NX-1)                                           
      C1XTHE=1.-XMIN/XINC                                               
      C2XTHE=1./XINC                                                    
      ANXTHE=NX-0.01                                                    
      YMIN=0.1**ROCP                                                    
      YMAX=1.1**ROCP                                                    
      YINC=(YMAX-YMIN)/(NY-1)                                           
      C1YTHE=1.-YMIN/YINC                                               
      C2YTHE=1./YINC                                                    
      ANYTHE=NY-0.01                                                    
      DO JY=1,NY                                                        
        Y=YMIN+(JY-1)*YINC                                              
        P=Y**CPOR                                                       
        DO JX=1,NX                                                      
          X=XMIN+(JX-1)*XINC                                            
          T=X                                                           
          TR=TTP/T                                                      
          PV=PSATB*(TR**XA)*EXP(XB*(1.-TR))                             
          PD=P-PV                                                       
          EL=HVAP+DLDT*(T-TTP)                                          
          EXPO=EL*EPS*PV/(CP*T*PD)                                      
          TBTHE(JX,JY)=T*PD**(-ROCP)*EXP(EXPO)                          
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE GTMA                                                   
!$$$     SUBPROGRAM DOCUMENTATION BLOCK                                 
!                                                                       
! SUBPROGRAM: GTMA         COMPUTE MOIST ADIABAT TABLES                 
!   AUTHOR: N PHILLIPS            W/NMC2X2   DATE: 30 DEC 82            
!                                                                       
! ABSTRACT: COMPUTE TEMPERATURE AND SPECIFIC HUMIDITY TABLES            
!   AS A FUNCTION OF EQUIVALENT POTENTIAL TEMPERATURE AND               
!   PRESSURE OVER 100 KPA TO THE KAPPA POWER FOR FUNCTION FTMA.         
!   EQUIVALENT POTENTIAL TEMPERATURE IS CONSTANT FOR A SATURATED PARCEL 
!   RISING ADIABATICALLY UP A MOIST ADIABAT WHEN THE HEAT AND MASS      
!   OF THE CONDENSED WATER ARE NEGLECTED. THE FORMULA FOR               
!   EQUIVALENT POTENTIAL TEMPERATURE (DERIVED IN HOLTON) IS             
!       THE=T*(PD**(-ROCP))*EXP(EL*EPS*PV/(CP*T*PD))                    
!   WHERE T IS THE TEMPERATURE, PV IS THE SATURATED VAPOR PRESSURE,     
!   PD IS THE DRY PRESSURE P-PV, EL IS THE TEMPERATURE DEPENDENT        
!   LATENT HEAT OF CONDENSATION HVAP+DLDT*(T-TTP), AND OTHER VALUES     
!   ARE PHYSICAL CONSTANTS DEFINED IN PARAMETER STATEMENTS IN THE CODE. 
!   THE FORMULA IS INVERTED BY ITERATING NEWTONIAN APPROXIMATIONS       
!   FOR EACH THE AND P UNTIL T IS FOUND TO WITHIN 1.E-4 KELVIN.         
!   THE SPECIFIC HUMIDITY IS THEN COMPUTED FROM PV AND PD.              
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A FIRST DIMENSION  
!   OF 61 FOR EQUIVALENT POTENTIAL TEMPERATURES RANGING FROM 200 TO 500 
!   KELVIN AND A SECOND DIMENSION OF 51 FOR PRESSURE OVER 100 KPA       
!   TO THE KAPPA POWER RANGING FROM 0.01**ROCP TO 1.1**ROCP.            
!                                                                       
! USAGE:  CALL GTMA                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-05-07  IREDELL                                                   
!                                                                       
! COMMON BLOCKS:                                                        
!   COMMA    - SCALING PARAMETERS AND TABLE FOR FUNCTION FTMA.          
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77.                                               
!   MACHINE:  CRAY.                                                     
!                                                                       
!$$$                                                                    
      PARAMETER(CP= 1.0046E+3 ,RD= 2.8705E+2 ,RV= 4.6150E+2 ,            &
     &          TTP= 2.7316E+2 ,HVAP= 2.5000E+6 ,PSATK= 6.1078E+2 *1.E-3 &
     &,                                                                  &
     &          CLIQ= 4.1855E+3 ,CVAP= 1.8460E+3 )                      
      PARAMETER(ROCP=RD/CP,CPOR=CP/RD,PSATB=PSATK*1.E-2,EPS=RD/RV,       &
     &          DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))         
      PARAMETER(NX=61,NY=51)                                            
      DIMENSION TBTMA(NX,NY),TBQMA(NX,NY)                               
      COMMON/COMMA/ C1XMA,C2XMA,ANXMA,C1YMA,C2YMA,ANYMA,TBTMA,TBQMA     
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      XMIN=200.                                                         
      XMAX=500.                                                         
      XINC=(XMAX-XMIN)/(NX-1)                                           
      C1XMA=1.-XMIN/XINC                                                
      C2XMA=1./XINC                                                     
      ANXMA=NX-0.01                                                     
      YMIN=0.01**ROCP                                                   
      YMAX=1.1**ROCP                                                    
      YINC=(YMAX-YMIN)/(NY-1)                                           
      C1YMA=1.-YMIN/YINC                                                
      C2YMA=1./YINC                                                     
      ANYMA=NY-0.01                                                     
      TERRM=1.E-4                                                       
      DO JY=1,NY                                                        
        Y=YMIN+(JY-1)*YINC                                              
        P=Y**CPOR                                                       
        T=XMIN*Y                                                        
        TR=TTP/T                                                        
        PV=PSATB*(TR**XA)*EXP(XB*(1.-TR))                               
        PD=P-PV                                                         
        EL=HVAP+DLDT*(T-TTP)                                            
        EXPO=EL*EPS*PV/(CP*T*PD)                                        
        THET=T*PD**(-ROCP)*EXP(EXPO)                                    
        DTHET=THET/T*(1.+EXPO*(DLDT*T/EL+EL*P/(RV*T*PD)))               
        DO JX=1,NX                                                      
          X=XMIN+(JX-1)*XINC                                            
          THE=X                                                         
          TERR=(THET-THE)/DTHET                                         
          DOWHILE(ABS(TERR).GT.TERRM)                                   
            T=T-TERR                                                    
            TR=TTP/T                                                    
            PV=PSATB*(TR**XA)*EXP(XB*(1.-TR))                           
            PD=P-PV                                                     
            EL=HVAP+DLDT*(T-TTP)                                        
            EXPO=EL*EPS*PV/(CP*T*PD)                                    
            THET=T*PD**(-ROCP)*EXP(EXPO)                                
            DTHET=THET/T*(1.+EXPO*(DLDT*T/EL+EL*P/(RV*T*PD)))           
            TERR=(THET-THE)/DTHET                                       
          ENDDO                                                         
          TBTMA(JX,JY)=T-TERR                                           
          TR=TTP/TBTMA(JX,JY)                                           
          PV=PSATB*(TR**XA)*EXP(XB*(1.-TR))                             
          PD=P-PV                                                       
          Q=EPS*PV/(PD+EPS*PV)                                          
          TBQMA(JX,JY)=Q                                                
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE HYDRO(IM,IX,KM,SI,SL,ZS,TV,Z,ZI)                      
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    HYDRO       CALCULATE GEOPOTENTIAL HEIGHTS             
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: CALCULATES GEOPOTENTIAL HEIGHTS ON BOTH THE SIGMA INTERFACES
!   AND THE SIGMA FULL LEVELS AS A FUNCTION OF OROGRAPHY, TEMPERATURE   
!   AND MOISTURE.  VIRTUAL TEMPERATURE IS CALCULATED FROM TEMPERATURE   
!   AND MOISTURE AND THE HYDROSTATIC EQUATION IS INTEGRATED             
!     DZ=RD/G*TV*DLNP                                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!                                                                       
! USAGE:    CALL HYDRO(IM,IX,KM,SI,SL,ZS,TV,Z,ZI)                      
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF LEVELS                               
!     SI       - REAL (KM+1) SIGMA INTERFACE VALUES                     
!     SL       - REAL (KM) SIGMA VALUES                                 
!     ZS       - REAL (IM) OROGRAPHY IS M                               
!     TV       - REAL (IX,KM) VIRTUAL TEMPERATURE IN K                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     Z        - REAL (IX,KM) HEIGHTS ON THE FULL LEVELS IN M           
!     ZI       - REAL (IX,KM) HEIGHTS ON THE INTERFACES IN M            
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION SI(KM+1),SL(KM),ZS(IM),TV(IX,KM)
      DIMENSION Z(IX,KM),ZI(IX,KM)                                      
      PARAMETER(G= 9.8000E+0 ,RD= 2.8705E+2 ,RV= 4.6150E+2 )            
      PARAMETER(ROG=RD/G,FVIRT=RV/RD-1.)                                
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      DO I=1,IM                                                         
        ZI(I,1)=ZS(I)                                                   
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      DO K=1,KM-1                                                       
        CA=ROG*LOG(SI(K)/SL(K))                                         
        CB=ROG*LOG(SL(K)/SI(K+1))                                       
        DO I=1,IM                                                       
          Z(I,K)=ZI(I,K)+CA*TV(I,K)                                          
          ZI(I,K+1)=Z(I,K)+CB*TV(I,K)                                       
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      CA=ROG*LOG(SI(KM)/SL(KM))                                         
      DO I=1,IM                                                         
        Z(I,KM)=ZI(I,KM)+CA*TV(I,K)                                          
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE LIFTIX(IM,IX,KT,PT,KM,SI,SL,PS,T5,TPT,QPT,T,Q,          &
     &                  SLI,SCAPE,SCIN,BLI,BCAPE,BCIN)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
!  SUBPROGRAM:    LIFTIX      COMPUTE LIFTED INDICES FROM SIGMA
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
!
! ABSTRACT: COMPUTES BOTH THE SURFACE LIFTED INDEX AND BEST LIFTED INDEX
!   FROM PROFILES IN CONSTANT PRESSURE THICKNESS LAYERS ABOVE GROUND.
!   THE SURFACE LIFTED INDEX IS COMPUTED BY RAISING THE LOWEST LAYER
!   TO 500 MB AND SUBTRACTING ITS PARCEL TEMPERATURE
!   FROM THE ENVIRONMENT TEMPERATURE.
!   THE BEST LIFTED INDEX IS COMPUTED BY FINDING THE PARCEL
!   WITH THE WARMEST EQUIVALENT POTENTIAL TEMPERATURE,
!   THEN RAISING IT TO 500 MB AND SUBTRACTING ITS PARCEL TEMPERATURE
!   FROM THE ENVIRONMENT TEMPERATURE.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   94-04-28  IREDELL   FIXED PARAMETERS
!   94-06-03  IREDELL   RETURNED TWO INDICES
!   94-07-29  IREDELL   USED CONSTANT PRESSURE THICKNESS PROFILES
!
! USAGE:    CALL LIFTIX(IM,IX,KT,PT,KM,SI,SL,PS,T5,TPT,QPT,T,Q,
!    &                  SLI,SCAPE,SCIN,BLI,BCAPE,BCIN)
!
!   INPUT ARGUMENT LIST:
!     IM       - INTEGER NUMBER OF POINTS
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA
!     KT       - INTEGER NUMBER OF PRESSURE THICKNESS LAYERS
!     PT       - REAL PRESSURE THICKNESS IN KPA
!     KM       - INTEGER NUMBER OF SIGMA LAYERS
!     SI       - REAL (KM+1) SIGMA INTERFACES
!     SL       - REAL (KM) SIGMA VALUES
!     PS       - REAL (IM) SURFACE PRESSURE IN KPA
!     T5       - REAL (IM) 500 MB TEMPERATURE IN K
!     TPT      - REAL (IX,KT) PBL TEMPERATURE IN K
!     QPT      - REAL (IX,KT) PBL SPECIFIC HUMIDITY IN KG/KG
!     T        - REAL (IX,KM) SIG TEMPERATURE IN K
!     Q        - REAL (IX,KM) SIG SPECIFIC HUMIDITY IN KG/KG
!
!   OUTPUT ARGUMENT LIST:
!     SLI      - REAL (IM) SURFACE LIFTED INDEX IN K
!     SCAPE    - REAL (IM) SURFACE CONVECTIVE APE IN J/KG
!     SCIN     - REAL (IM) SURFACE CONVECTIVE INHIBITION IN J/KG
!     BLI      - REAL (IM) BEST LIFTED INDEX IN K
!     BCAPE    - REAL (IM) BEST CONVECTIVE APE IN J/KG
!     BCIN     - REAL (IM) BEST CONVECTIVE INHIBITION IN J/KG
!
! SUBPROGRAMS CALLED:
!   (FPKAP)   - FUNCTION TO COMPUTE PRESSURE TO THE KAPPA
!   (FTDP)    - FUNCTION TO COMPUTE DEWPOINT TEMPERATURE
!   (FTLCL)   - FUNCTION TO COMPUTE LIFTING CONDENSATION LEVEL
!   (FTHE)    - FUNCTION TO COMPUTE EQUIVALENT POTENTIAL TEMPERATURE
!   (FTMA)    - FUNCTION TO COMPUTE MOIST ADIABAT TEMPERATURE
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
!FPP$ EXPAND(FPKAP,FTDP,FTLCL,FTHE,FTMA)
      REAL SI(KM+1),SL(KM)
      REAL PS(IM),T5(IM),TPT(IX,KT),QPT(IX,KT),T(IX,KM),Q(IX,KM)
      REAL SLI(IM),SCAPE(IM),SCIN(IM),BLI(IM),BCAPE(IM),BCIN(IM)
      PARAMETER(CP= 1.0046E+3 ,RD= 2.8705E+2 ,RV= 4.6150E+2 )
      PARAMETER(RK=RD/CP,EPS=RD/RV,EPSM1=RD/RV-1.,FVIRT=RV/RD-1.)
      PARAMETER(PLIFT=50.,SIMIN=0.040)
      REAL PKMAS(IM),THEMAS(IM),PKMAB(IM),THEMAB(IM)
      REAL DLS(KM),SLK(KM),PSK(IM)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SELECT THE WARMEST EQUIVALENT POTENTIAL TEMPERATURE
      DO K=1,KT
        DO I=1,IM
          P=PS(I)-(K-0.5)*PT
          PV=P*QPT(I,K)/(EPS-EPSM1*QPT(I,K))
          TDPD=MAX(TPT(I,K)-FTDP(PV),0.)
          TLCL=FTLCL(TPT(I,K),TDPD)
          PKLCL=FPKAP(P)*TLCL/TPT(I,K)
          THELCL=FTHE(TLCL,PKLCL)
          IF(K.EQ.1) THEN
            PKMAS(I)=PKLCL
            THEMAS(I)=THELCL
            PKMAB(I)=PKLCL
            THEMAB(I)=THELCL
          ELSEIF(THELCL.GT.THEMAB(I)) THEN
            PKMAB(I)=PKLCL
            THEMAB(I)=THELCL
          ENDIF
        ENDDO
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LIFT THE PARCEL TO 500 MB ALONG A DRY ADIABAT BELOW THE LCL
!  OR ALONG A MOIST ADIABAT ABOVE THE LCL.
!  THE LIFTED INDEX IS THE ENVIRONMENT MINUS PARCEL TEMPERATURE.
      PLIFTK=(PLIFT/100.)**RK
      DO I=1,IM
        IF(PS(I).GT.PLIFT) THEN
          PKS=MIN(PLIFTK,PKMAS(I))
          SLI(I)=T5(I)-PLIFTK/PKS*FTMA(THEMAS(I),PKS,QMA)
          PKB=MIN(PLIFTK,PKMAB(I))
          BLI(I)=T5(I)-PLIFTK/PKB*FTMA(THEMAB(I),PKB,QMA)
        ELSE
          SLI(I)=0.
          BLI(I)=0.
        ENDIF
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LIFT THE PARCEL TO ITS HIGHEST BUOYANT LAYER (BELOW 40 MB).
!  SEPARATELY INTEGRATE BETWEEN THE LCL AND THIS TOP LAYER
!  POSITIVELY BUOYANT AREA (CAPE) AND NEGATIVELY BUOYANT AREA (CIN).
      DO K=1,KM
        IF(SI(K+1).GT.SIMIN) THEN
          DLS(K)=LOG(SI(K)/SI(K+1))
          SLK(K)=SL(K)**RK
        ENDIF
      ENDDO
      DO I=1,IM
        PSK(I)=FPKAP(PS(I))
        SCAPE(I)=0.
        SCIN(I)=0.
        BCAPE(I)=0.
        BCIN(I)=0.
      ENDDO
      DO K=KM,1,-1
        IF(SI(K+1).GT.SIMIN) THEN
          DO I=1,IM
            PK=SLK(K)*PSK(I)
            TVENV=T(I,K)*(1.+FVIRT*Q(I,K))
            GDZ=RD*TVENV*DLS(K)
            IF(PK.LE.PKMAS(I)) THEN
              TMA=FTMA(THEMAS(I),PK,QMA)
              TVCLD=TMA*(1.+FVIRT*QMA)
              IF(TVCLD.GT.TVENV) THEN
                SCAPE(I)=SCAPE(I)+GDZ*LOG(TVCLD/TVENV)
              ELSEIF(SCAPE(I).GT.0.) THEN
                SCIN(I)=SCIN(I)+GDZ*LOG(TVCLD/TVENV)
              ENDIF
            ENDIF
            IF(PK.LE.PKMAB(I)) THEN
              TMA=FTMA(THEMAB(I),PK,QMA)
              TVCLD=TMA*(1.+FVIRT*QMA)
              IF(TVCLD.GT.TVENV) THEN
                BCAPE(I)=BCAPE(I)+GDZ*LOG(TVCLD/TVENV)
              ELSEIF(BCAPE(I).GT.0.) THEN
                BCIN(I)=BCIN(I)+GDZ*LOG(TVCLD/TVENV)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END

      SUBROUTINE LIFTIX_old(IM,IX,KT,PT,PS,T,Q,TM,SLI,BLI)                  
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    LIFTIX      COMPUTE LIFTED INDICES FROM SIGMA         
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: COMPUTES BOTH THE SURFACE LIFTED INDEX AND BEST LIFTED INDEX
!   FROM PROFILES IN CONSTANT PRESSURE THICKNESS LAYERS ABOVE GROUND.   
!   THE SURFACE LIFTED INDEX IS COMPUTED BY RAISING THE LOWEST LAYER    
!   TO 500 MB AND SUBTRACTING ITS PARCEL TEMPERATURE                    
!   FROM THE ENVIRONMENT TEMPERATURE.                                   
!   THE BEST LIFTED INDEX IS COMPUTED BY FINDING THE PARCEL             
!   WITH THE WARMEST EQUIVALENT POTENTIAL TEMPERATURE,                  
!   THEN RAISING IT TO 500 MB AND SUBTRACTING ITS PARCEL TEMPERATURE    
!   FROM THE ENVIRONMENT TEMPERATURE.                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!   94-04-28  IREDELL   FIXED PARAMETERS                                
!   94-06-03  IREDELL   RETURNED TWO INDICES                            
!   94-07-29  IREDELL   USED CONSTANT PRESSURE THICKNESS PROFILES       
!                                                                       
! USAGE:    CALL LIFTIX(IM,IX,KT,PT,PS,T,Q,TM,SLI,BLI)                  
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KT       - INTEGER NUMBER OF LAYERS IN PROFILE                    
!     PT       - REAL PRESSURE THICKNESS IN KPA                         
!     PS       - REAL (IM) SURFACE PRESSURE IN KPA                      
!     T        - REAL (IX,KT) TEMPERATURE IN K                          
!     Q        - REAL (IX,KT) SPECIFIC HUMIDITY IN KG/KG                
!     TM       - REAL (IM) 500 MB TEMPERATURE IN K                      
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SLI      - REAL (IX) SURFACE LIFTED INDEX IN K                    
!     BLI      - REAL (IX) BEST LIFTED INDEX IN K                       
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   (FPKAP)   - FUNCTION TO COMPUTE PRESSURE TO THE KAPPA               
!   (FTDP)    - FUNCTION TO COMPUTE DEWPOINT TEMPERATURE                
!   (FTLCL)   - FUNCTION TO COMPUTE LIFTING CONDENSATION LEVEL          
!   (FTHE)    - FUNCTION TO COMPUTE EQUIVALENT POTENTIAL TEMPERATURE    
!   (FTMA)    - FUNCTION TO COMPUTE MOIST ADIABAT TEMPERATURE           
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
!FPP$ EXPAND(FPKAP,FTDP,FTLCL,FTHE,FTMA)                                
      DIMENSION PS(IM),T(IX,KT),Q(IX,KT),TM(IM),SLI(IM),BLI(IM)         
      PARAMETER(CP= 1.0046E+3 ,RD= 2.8705E+2 ,RV= 4.6150E+2 )           
      PARAMETER(RK=RD/CP,EPS=RD/RV,EPSM1=RD/RV-1.)                      
      PARAMETER(PLIFT=50.)                                              
      DIMENSION P2KMAS(IM),THEMAS(IM),P2KMAB(IM),THEMAB(IM)             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SELECT THE WARMEST EQUIVALENT POTENTIAL TEMPERATURE                  
      DO K=1,KT                                                         
        DO I=1,IM                                                       
          P=PS(I)-(K-0.5)*PT                                            
          PV=P*Q(I,K)/(EPS-EPSM1*Q(I,K))                                
          TDPD=MAX(T(I,K)-FTDP(PV),0.)                                  
          TLCL=FTLCL(T(I,K),TDPD)                                       
          P2KLCL=FPKAP(P)*TLCL/T(I,K)                                   
          THELCL=FTHE(TLCL,P2KLCL)                                      
          IF(K.EQ.1) THEN                                               
            P2KMAS(I)=P2KLCL                                            
            THEMAS(I)=THELCL                                            
            P2KMAB(I)=P2KLCL                                            
            THEMAB(I)=THELCL                                            
          ELSEIF(THELCL.GT.THEMAB(I)) THEN                              
            P2KMAB(I)=P2KLCL                                            
            THEMAB(I)=THELCL                                            
          ENDIF                                                         
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  LIFT THE PARCEL TO 500 MB ALONG A DRY ADIABAT BELOW THE LCL          
!  OR ALONG A MOIST ADIABAT ABOVE THE LCL.                              
!  THE LIFTED INDEX IS THE ENVIRONMENT MINUS PARCEL TEMPERATURE.        
      PLIFTK=(PLIFT/100.)**RK                                           
      DO I=1,IM                                                         
        IF(PS(I).GT.PLIFT) THEN                                         
          P2KS=MIN(PLIFTK,P2KMAS(I))                                    
          SLI(I)=TM(I)-PLIFTK/P2KS*FTMA(THEMAS(I),P2KS,QMA)             
          P2KB=MIN(PLIFTK,P2KMAB(I))                                    
          BLI(I)=TM(I)-PLIFTK/P2KB*FTMA(THEMAB(I),P2KB,QMA)             
        ELSE                                                            
          SLI(I)=0.                                                     
          BLI(I)=0.                                                     
        ENDIF                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE OMEGAW(IM,IX,KM,SI,SL,PS,PSX,PSY,D,U,V,                &
     &                  PP,TT,WW,O,OS,W,NONHYD)            
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    OMEGA       CALCULATE PRESSURE VERTICAL VELOCITY       
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: CALCULATES PRESSURE VERTICAL VELOCITY OMEGA AS A FUNCTION   
!   OF SURFACE PRESSURE, SURFACE PRESSURE GRADIENTS, AND DIVERGENCE     
!   AND WIND COMPONENTS ON THE SIGMA SURFACES.  THE FORMULA FOR OMEGA   
!   IS DERIVED FROM THE CONTINUITY EQUATION                             
!     O=(SIG*V.GRAD(LNPS)-SUM((D+V.GRAD(LNPS))*DSIG))*PS*1.E3           
!   WHERE THE SUM IS TAKEN FROM THE TOP OF THE ATMOSPHERE.              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!   97-05-22  HANN_MING HENRY JUANG FOR NONHYDROSTATIC AND HYDROSTATIC
!                                   SURFACE PRESSURE TENDENCY IS TREATED
!                                   AS HYDROSTATIC.
!                                                                       
! USAGE:    CALL OMEGA(IM,IX,KM,SI,SL,PS,PSX,PSY,D,U,V,
!    &                 PP,TT,WW,O,OS,W.NONHYD)            
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF LEVELS                               
!     SI       - REAL (KM+1) SIGMA INTERFACE VALUES                     
!     SL       - REAL (KM) SIGMA VALUES                                 
!     PS       - REAL (IM) SURFACE PRESSURE IN KPA                      
!     PSX      - REAL (IM) ZONAL GRADIENT OF LOG PRESSURE IN 1/M        
!     PSY      - REAL (IM) MERID GRADIENT OF LOG PRESSURE IN 1/M        
!     D        - REAL (IX,KM) DIVERGENCE IN 1/S                         
!     U        - REAL (IX,KM) ZONAL WIND IN M/S                         
!     V        - REAL (IX,KM) MERID WIND IN M/S                         
!     PP       - REAL (IX,KM) TOTAKL PRESSURE (CB)
!     TT       - REAL (IX,KM) TOTAL TEMPERATURE (K)
!     WW       - REAL (IX,KM+1) VERTICAL VELOCITY (M/S)
!     NONHYD   - INDEX FOR 1:NONHYDROSTATIC AND 0:HYDROSTATIC
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     O        - REAL (IX,KM) PRESSURE VERTICAL VELOCITY IN PA/S        
!     OS       - REAL (IM) SURFACE PRESSURE TENDENCY IN PA/S            
!     W        - REAL (IX,KM) VERTICAL VELOCITY IN M/S
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION SI(KM+1),SL(KM)                                         
      DIMENSION PS(IM),PSX(IM),PSY(IM)                                  
      DIMENSION PP(IX,KM),TT(IX,KM),WW(IX,KM+1),W(IX,KM)
      DIMENSION D(IX,KM),U(IX,KM),V(IX,KM),O(IX,KM),OS(IM)              
      DIMENSION SUM(IM)                                                 
      PARAMETER(RD= 2.8705E+2, G= 9.8000E+0)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      ROG= RD / G
      DO I=1,IM                                                         
        SUM(I)=0.                                                       
      ENDDO                                                             
      DO K=KM,1,-1                                                      
        DO I=1,IM                                                       
          VGRADP=U(I,K)*PSX(I)+V(I,K)*PSY(I)                            
          GRADPV=VGRADP+D(I,K)                                          
          SUM(I)=SUM(I)+GRADPV*(SL(K)-SI(K+1))                          
          O(I,K)=(VGRADP*SL(K)-SUM(I))*PS(I)*1.E3                       
          SUM(I)=SUM(I)+GRADPV*(SI(K)-SL(K))                            
          W(I,K)=-ROG*TT(I,K)/PP(I,K)*O(I,K)/1.E3
        ENDDO                                                           
      ENDDO                                                             
      DO I=1,IM                                                         
        OS(I)=-SUM(I)*PS(I)*1.E3                                        
      ENDDO                                                             
!
      IF( NONHYD.EQ.1 ) THEN
        DO K=1,KM
          DO I=1,IM
            W(I,K)=0.5*(WW(I,K)+WW(I,K+1))
            O(I,K)=-PP(I,K)/ROG/TT(I,K)*W(I,K)*1.E3
          ENDDO
        ENDDO
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE RPGB1(FHOUR,IDATE,                                      &
     &                NSIG,NFLX,IGSGR,JCAP,IWAV1,JWAV1,                  &
     &                IGRD1,IGRD11,JGRD1,LEVS,NFLDS,SI,SL,               &
     &                IO,JO,KO,PO,KT,PT,KZZ,ZZ,NONHYD,                   &
     &                PROJ,TRUTH,ORIENT,CENLAT,CENLON,                   &
     &                GRDLEFT,GRDBOTM,DELX,DELY,                         &
     &                NPGB,NFGB,NCPUS,MXBIT,IDS,POT,KTT,                 &
     &                ICEN,ICEN2,IGEN,IGRIDO,                            &
     &                PROJO,ORIENTO,TRUTHO,COTRUO,DELXO,DELYO,           &
     &                RLAT1O,RLON1O,RLAT2O,RLON2O,                       &
     &                NTRAC,NCLD,NEWSLM)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM: RPGB1           TRANSFORMS A SIGMA TO PRESSURE GRIB       
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31            
!                                                                       
! ABSTRACT: TRANSFORMS A SIGMA (GRID OR SPECTRAL) FILE TO PRESSURE GRIB1
!   ONE GAUSSIAN LATITUDE SLICE AT A TIME, THE SIGMA GRID DATA IS EITHER
!   READ DIRECTLY OR TRANSFORMED FROM SPECTRAL COEFFICIENTS.            
!   THE INPUT DATA CONSISTS OF VORTICITY, DIVERGENCE, WIND COMPONENTS,  
!   TEMPERATURE AND SPECIFIC HUMIDITY ON THE SIGMA SURFACES AS WELL AS  
!   SURFACE PRESSURE AND OROGRAPHY AND THEIR HORIZONTAL GRADIENTS.      
!   RELATIVE HUMIDITY, VERTICAL VELOCITY AND GEOPOTENTIAL HEIGHTS       
!   ARE COMPUTED ON THE SIGMA SURFACES AND THEN INTERPOLATED TO PRESSURE
!   ALONG WITH WIND AND TEMPERATURE.  SUNDRY FIELDS ARE ALSO COMPUTED.  
!   THE OUTPUT DATA IS QUARTERPACKED AND TRANSPOSED TO HORIZONTAL FIELDS
!   WHICH ARE THEN INTERPOLATED TO THE OUTPUT GRID AND ROUNDED          
!   AND PACKED INTO GRIB MESSAGES AND WRITTEN TO THE PRESSURE GRIB FILE.
!   CONCURRENTLY, THE GRIB INDEX FILE IS WRITTEN.                       
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!   93- 8-17  HANN-MING HENRY JUANG  MODIFIED FOR REGIONAL SPECTRAL MODE
!   94-10-12  HANN-MING HENRY JUANG ADD MORE FIELDS                     
!                                                                       
! USAGE:    CALL RPGB1(FHOUR,IDATE,                                     
!    &                NSIG,NFLX,IGSGR,JCAP,IWAV1,JWAV1,                 
!    &                IGRD1,IGRD11,JGRD1,LEVS,NFLDS,SI,SL,              
!    &                IO,JO,KO,PO,KT,PT,KZZ,ZZ,NONHYD,                                
!    &                DELX,DELY,TRUTH,ORIENT,PROJ,                      
!    &                NPGB,NFGB,NCPUS,MXBIT,IDS,POT,KTT,                
!    &                ICEN,ICEN2,IGEN)                            
!   INPUT ARGUMENTS:                                                    
!     FHOUR        REAL FORECAST HOUR                                   
!     IDATE        INTEGER (4) DATE                                     
!     NSIG         INTEGER UNIT FROM WHICH TO READ SIGMA FILE           
!     NFLX         INTEGER UNIT FROM WHICH TO READ S2D FILE             
!     IGSGR        INTEGER SIGMA FILE EDITION (SPECTRAL IF 0)           
!     JCAP         INTEGER SPECTRAL TRUNCATION                          
!     IWAV1        INTEGER NUMBER OF SPECTRAL COEFFICIENTS IN I         
!     JWAV1        INTEGER NUMBER OF SPECTRAL COEFFICIENTS IN J         
!     IGRD1        INTEGER NUMBER OF GAUSSIAN LONGITUDES                
!     IGRD11       INTEGER I DIMENSION FOR PACK, FACTOR OF 4            
!     JGRD1        INTEGER HALF THE NUMBER OF GAUSSIAN LATITUDES        
!     LEVS         INTEGER NUMBER OF SIGMA LEVELS                       
!     NFLDS        INTEGER TOTAL NUMBER OF INPUT HORIZONTAL FIELDS      
!     SI           REAL (LEVS+1) SIGMA INTERFACE VALUES                 
!     SL           REAL (LEVS) SIGMA FULL LEVEL VALUES                  
!     IO           INTEGER NUMBER OF OUTPUT LONGITUDES                  
!     JO           INTEGER NUMBER OF OUTPUT LATITUDES                   
!     KO           INTEGER NUMBER OF OUTPUT PRESSURE LEVELS             
!     PO           REAL (KO) MANDATORY PRESSURES IN KPA                 
!     NPGB         INTEGER UNIT TO WRITE PRES AND FLUX GRIB MESSAGES    
!     NFGB         INTEGER UNIT TO WHICH TO WRITE FLUX GRIB MESSAGES    
!     NCPUS        INTEGER NUMBER OF CPUS OVER WHICH TO DISTRIBUTE WORK 
!     MXBIT        INTEGER MAXIMUM NUMBER OF BITS TO PACK DATA          
!     IDS          INTEGER (255) DECIMAL SCALING                        
!     POT          REAL (255) TOPMOST PRESSURE IN KPA                   
!     ICEN         INTEGER FORECAST CENTER IDENTIFIER                   
!     ICEN2        INTEGER FORECAST SUBCENTER IDENTIFIER                
!     IGEN         INTEGER GENERATING MODEL IDENTIFIER                  
!     NTRAC        INTEGER NUMBER OF TRACERS
C     IDVT         INTEGER TRACER VARIABLE ID
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   SUNPRM       SET PARAMETERS FOR SUNDRY FIELDS                       
!   RRDSGR       READ SIGMA GRIDDED DATA                                
!   GETRH        COMPUTE RELATIVE HUMIDITY                              
!   OMEGA        COMPUTE VERTICAL VELOCITY                              
!   HYDRO        COMPUTE GEOPOTENTIAL HEIGHTS                           
!   SIG2P        INTERPOLATE SIGMA TO PRESSURE                          
!   SUNDRY       COMPUTE SUNDRY FIELDS                                  
!   PTRANW       QUARTERPACK AND TRANSPOSE DATA                         
!   PTRANR       UNPACK QUARTERPACKED TRANSPOSED DATA                   
!   GRIBIT       CREATE GRIB MESSAGE                                    
!   MKHEAD       MAKE HEADER RECORD                                     
!   WRGIR        WRITE GRIB INDEX RECORD                                
!   WRYTE        WRITE DATA BY BYTES                                    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      PARAMETER(LEVO=20,LEVT=6,LEVZ=3,NROW=4,MYBIT=16)
      DIMENSION IDATE(4),SI(LEVS+1),SL(LEVS),PO(KO),ZZ(KZZ)
      real ts(levs),qs(levs),plry(levs),plvl(levs+1)
      integer iwx(IGRD1*JGRD1)
      DIMENSION IDS(255),POT(255)                                       
      PARAMETER(L2=2)                                                   
      PARAMETER(G= 9.8000E+0 ,RD= 2.8705E+2 ,RV= 4.6150E+2 )            
      PARAMETER(GOR2=G/RD*L2,EPS=RD/RV,EPSM1=RD/RV-1.)                  
      PARAMETER(NUPA=13,NUPT=6,NUZZ=3,NSUN=35,NUPAS=5)  
      PARAMETER(LENPDS=28,LENGDS=32)                                    
      DIMENSION IPO(KO),NPO(KO),POKPA(KO)                               
      DIMENSION NPT1(KT),NPT2(KT)                                       
      DIMENSION CLAT(IGRD1,JGRD1),FLAT(JGRD1),IZ(KO)                    
      DIMENSION FXS(IGRD1,NFLDS,JGRD1)                                  
      DIMENSION OXS(IGRD1,LEVS),OSXS(IGRD1,LEVS)                        
      DIMENSION WXS(IGRD1,LEVS)
      DIMENSION RXS(IGRD1,LEVS),QSXS(IGRD1,LEVS)                        
      DIMENSION ZXS(IGRD1,LEVS),ZXI(IGRD1,LEVS)                         
      DIMENSION QXP(IGRD1,KT)                                           
      PARAMETER (NUMALL=NUPA*LEVO+NUPT*LEVT+NUZZ*LEVZ+NSUN)
      DIMENSION FXP(IGRD1,NUMALL,JGRD1)                   
      DIMENSION FXY(IGRD1,JGRD1,NCPUS)                                  
      DIMENSION AWIPS(IO,JO)                                            
      DIMENSION LGRIB(NCPUS)                                            
      DIMENSION IPU(NUMALL)
      DIMENSION ITL(NUMALL)     
      DIMENSION IL1(NUMALL)
      DIMENSION IL2(NUMALL)     
      CHARACTER GRIB(30+LENPDS+LENGDS+IO*JO*(MXBIT+1)/8,JGRD1)          
      PARAMETER(IPUU=33,IPUV=34,IPUO=39,IPUZ=7,IPUT=11)
      PARAMETER(IPUQ=51,IPUR=52,IPUA=41) 
      PARAMETER(IPUCL=153,IPUOZ=154)
!      PARAMETER(IPUOZ=34,IPUCL=223)
      PARAMETER(IPUW=40)
      PARAMETER(IPUQC=191,IPUQR=192)
      DIMENSION IPUSUN(NSUN)                                            
      DIMENSION ITLSUN(NSUN),IL1SUN(NSUN),IL2SUN(NSUN)                  
      DIMENSION KSLP(2)                                                 
      DIMENSION IENS(5)                                                 
      DIMENSION FLX(IGRD1*JGRD1)                                        
      DIMENSION XMAP(IGRD1*JGRD1)                                        
      LOGICAL LBM(IGRD1*JGRD1)                                          
      DIMENSION FXYSEA(IGRD1,JGRD1),FXYLAND(IGRD1,JGRD1)
      DIMENSION IWORK(IGRD1,JGRD1),RR(IO,JO)
      DIMENSION RLBM1(IGRD1*JGRD1),RLBM2(IO*JO)
      DIMENSION LBM2(IO*JO)
      LOGICAL LBMO(IO*JO)                                          
      DIMENSION                                                         &
     &          N00(IO*JO),N10(IO*JO)                                   &
     &         ,N11(IO*JO),N01(IO*JO)                                   &
     &         ,D00(IO*JO),D10(IO*JO)                                   &
     &         ,D11(IO*JO),D01(IO*JO)                       
!                                                                       
      DIMENSION T2(IGRD1*JGRD1),Q2(IGRD1*JGRD1),PS(IGRD1*JGRD1)         
      COMMON/IOINDX/ KSZ,KSD,KST,KSQ,KSPSX,KSPSY,KSU,KSV,KSPS,KSZS,       &
     &               KSZSX,KSZSY,KSPN,KSTN,KSWN,KSQC,KSQR,KSOZ,KSCL,      &
     &               KPZ,KPU,KPV,KPR,KPQ,KPT,KPO,KPW,KPA,KPQC,KPQR,       &
     &               KPOZ,KPCL,                                           &
     &               KPTU,KPTV,KPTT,KPTR,KPTOZ,KPTCL,KPSUN        

      real*4 FLX_s(IGRD1*JGRD1),COLAT1_s,RLAT1_s,RLON1_s,RLAT2_s,       &
     &       RLON2_s,DELX_s,DELY_s,ORTRU_s,PROJ_s
!     read new land-sea mask
      print *,'fhour',fhour 
      IF (NEWSLM.EQ.1) THEN 
         READ(71) LBM2
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SET BOTH INPUT AND OUTPUT INDICES                                    
      PRINT *,' SET BOTH INPUT AND OUTPUT INDICES '                     
      KSZS=1
      KSZSX=2
      KSZSY=3
      KSPS=4
      KSPSX=5
      KSPSY=6
      KSD=7
      KSZ=LEVS+7
      KSU=2*LEVS+7
      KSV=3*LEVS+7
      KST=4*LEVS+7
      KSQ=5*LEVS+7
      KSQC=6*LEVS+7
      KSQR=7*LEVS+7
      KSPN=8*LEVS+7
      KSTN=9*LEVS+7
      KSWN=10*LEVS+7
      KSOZ=11*LEVS+8
      KSCL=12*LEVS+8
      NFLDS=8+13*LEVS
! for KO
      KPZ=1                                                             
      KPU=1+KO                                                          
      KPV=1+2*KO                                                        
      KPT=1+3*KO                                                        
      KPO=1+4*KO                                                        
      KPW=1+5*KO
      KPR=1+6*KO                                                        
      KPQ=1+7*KO
      KPA=1+8*KO                                                        
      KPQC=1+9*KO
      KPQR=1+10*KO
      KPCL=1+11*KO
      KPOZ=1+12*KO
! for KT
      KPTU=13*KO+1                                                       
      KPTV=13*KO+KT+1                                                    
      KPTT=13*KO+2*KT+1                                                  
      KPTR=13*KO+3*KT+1                                                  
      KPTOZ=13*KO+4*KT+1
      KPTCL=13*KO+5*KT+1
      KZZU =13*KO+6*KT+1
      KZZV =13*KO+6*KT+KZZ+1
      KZZT =13*KO+6*KT+2*KZZ+1
      KPSUN=13*KO+6*KT+3*KZZ+1                                                 
!  SET SOME PARAMETERS                                                  
      JFHOUR=NINT(FHOUR)                                                
      NFLDP=NUPA*KO+NUPT*KT+NUZZ*KZZ+NSUN                                        
      NFLDPS=NUPAS*KO                                                   
      DO K=1,KO                                                         
        POKPA(K)=PO(K)/10.                                              
        IF(FLOAT(NINT(PO(K))).EQ.PO(K).OR.PO(K).GT.655.) THEN           
          IPO(K)=100                                                    
          NPO(K)=NINT(PO(K))                                            
        ELSE                                                            
          IPO(K)=120                                                    
          NPO(K)=NINT(PO(K)*100.)                                       
        ENDIF                                                           
      ENDDO                                                             
      PTKPA=PT/10.                                                      
      DO K=1,KT                                                         
        NPT1(K)=K*PT                                                    
        NPT2(K)=(K-1)*PT                                                
      ENDDO                                                             
      KPMU=ISRCHFLT(KO,PO,1,POT(IPUU))-1                                
      KPMV=ISRCHFLT(KO,PO,1,POT(IPUV))-1                                
      KPMO=ISRCHFLT(KO,PO,1,POT(IPUO))-1                                
      KPMZ=ISRCHFLT(KO,PO,1,POT(IPUZ))-1                                
      KPMT=ISRCHFLT(KO,PO,1,POT(IPUT))-1                                
      KPMR=ISRCHFLT(KO,PO,1,POT(IPUR))-1                                
      KPMQ=ISRCHFLT(KO,PO,1,POT(IPUQ))-1                                
      KPMW=ISRCHFLT(KO,PO,1,POT(IPUW))-1                                
      KPMA=ISRCHFLT(KO,PO,1,POT(IPUA))-1                                
      KPMOZ=ISRCHFLT(KO,PO,1,POT(IPUOZ))-1                                
      KPMCL=ISRCHFLT(KO,PO,1,POT(IPUCL))-1                                
      KPMQC=ISRCHFLT(KO,PO,1,POT(IPUQC))-1
      KPMQR=ISRCHFLT(KO,PO,1,POT(IPUQR))-1
      CALL SUNPRM(KO,PO,KT,KTT,PT,                                        &
     &            IPUSUN,ITLSUN,IL1SUN,IL2SUN,KSLP,KLI,K5Z,L5Z)           
!                                                                       
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      IENS(1)=1                                                         
      IENS(2)=0                                                         
      IENS(3)=0                                                         
      IENS(4)=1                                                         
      IENS(5)=255                                                       
      IPU(1:NFLDP)=0                                                    
      IPU(KPU:KPU+KPMU-1)=IPUU                                          
      IPU(KPV:KPV+KPMV-1)=IPUV                                          
      IPU(KPO:KPO+KPMO-1)=IPUO                                          
      IPU(KPW:KPW+KPMW-1)=IPUW                                          
      IPU(KPZ:KPZ+KPMZ-1)=IPUZ                                          
      IPU(KPT:KPT+KPMT-1)=IPUT                                          
      IPU(KPQC:KPQC+KPMQC-1)=IPUQC
      IPU(KPQR:KPQR+KPMQR-1)=IPUQR
      IPU(KPOZ:KPOZ+KPMOZ-1)=IPUOZ                                          
      IPU(KPCL:KPCL+KPMCL-1)=IPUCL                                          
      IPU(KPR:KPR+KPMR-1)=IPUR                                          
      IPU(KPQ:KPQ+KPMQ-1)=IPUQ                                          
      IPU(KPA:KPA+KPMA-1)=IPUA                                          
!
      IPU(KPTU:KPTU+KTT-1)=IPUU                                         
      IPU(KPTV:KPTV+KTT-1)=IPUV                                         
      IPU(KPTT:KPTT+KTT-1)=IPUT                                         
      IPU(KPTR:KPTR+KTT-1)=IPUR                                         
      IPU(KPTOZ:KPTOZ+KTT-1)=IPUOZ                                          
      IPU(KPTCL:KPTCL+KTT-1)=IPUCL                                          
      IPU(KPSUN:KPSUN+NSUN-1)=IPUSUN                                    
!
      IPU(KZZU:KZZU+KZZ-1)=IPUU
      IPU(KZZV:KZZV+KZZ-1)=IPUV
      IPU(KZZT:KZZT+KZZ-1)=IPUT
!
      ITL(KPU:KPU+KO-1)=IPO                                             
      ITL(KPV:KPV+KO-1)=IPO                                             
      ITL(KPO:KPO+KO-1)=IPO                                             
      ITL(KPW:KPW+KO-1)=IPO                                             
      ITL(KPZ:KPZ+KO-1)=IPO                                             
      ITL(KPT:KPT+KO-1)=IPO                                             
      ITL(KPQC:KPQC+KO-1)=IPO                                             
      ITL(KPQR:KPQR+KO-1)=IPO                                             
      ITL(KPOZ:KPOZ+KO-1)=IPO                                             
      ITL(KPCL:KPCL+KO-1)=IPO                                             
      ITL(KPR:KPR+KO-1)=IPO                                             
      ITL(KPQ:KPQ+KO-1)=IPO                                             
      ITL(KPA:KPA+KO-1)=IPO                                             
      ITL(NUPA*KO+1:NUPA*KO+NUPT*KT)=116                                
      ITL(NUPA*KO+NUPT*KT+1:NUPA*KO+NUPT*KT+NUZZ*KZZ)=103
      ITL(KPSUN:KPSUN+NSUN-1)=ITLSUN                                    
!
      IL1(:)=0                                                          
      IL1(KPTU:KPTU+KT-1)=NPT1                                          
      IL1(KPTV:KPTV+KT-1)=NPT1                                          
      IL1(KPTT:KPTT+KT-1)=NPT1                                          
      IL1(KPTOZ:KPTOZ+KT-1)=NPT1                                          
      IL1(KPTCL:KPTCL+KT-1)=NPT1                                          
      IL1(KPTR:KPTR+KT-1)=NPT1                                          
      IL1(KPSUN:KPSUN+NSUN-1)=IL1SUN                                    
!
      IL2(KPU:KPU+KO-1)=NPO                                             
      IL2(KPV:KPV+KO-1)=NPO                                             
      IL2(KPO:KPO+KO-1)=NPO                                             
      IL2(KPW:KPW+KO-1)=NPO                                             
      IL2(KPZ:KPZ+KO-1)=NPO                                             
      IL2(KPT:KPT+KO-1)=NPO                                             
      IL2(KPQC:KPQC+KO-1)=NPO                                             
      IL2(KPQR:KPQR+KO-1)=NPO                                             
      IL2(KPOZ:KPOZ+KO-1)=NPO                                             
      IL2(KPCL:KPCL+KO-1)=NPO                                             
      IL2(KPR:KPR+KO-1)=NPO                                             
      IL2(KPQ:KPQ+KO-1)=NPO                                             
      IL2(KPA:KPA+KO-1)=NPO                                             
!
      IL2(KPTU:KPTU+KT-1)=NPT2                                          
      IL2(KPTV:KPTV+KT-1)=NPT2                                          
      IL2(KPTT:KPTT+KT-1)=NPT2                                          
      IL2(KPTR:KPTR+KT-1)=NPT2                                          
      IL2(KPTOZ:KPTOZ+KT-1)=NPT2                                             
      IL2(KPTCL:KPTCL+KT-1)=NPT2                                             
!       
      IL2(KZZU:KZZU+KZZ-1)=NPT2(1:KZZ)
      IL2(KZZV:KZZV+KZZ-1)=NPT2(1:KZZ)
      IL2(KZZT:KZZT+KZZ-1)=NPT2(1:KZZ)
      IL2(KPSUN:KPSUN+NSUN-1)=IL2SUN                                    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  LOOP OVER GROUPS OF LATITUDES                                        
      RDELX2=0.5/DELX                                                   
      RDELY2=0.5/DELY                                                   
      AOMEGA=2.0*ACOS(-1.0)/(24.*60.*60.)                               
!  READ SIGMA GRID DATA FOR ALL LATITUDE SLICE INTO FXS                 
        CALL RRDSGR(NSIG,IGRD1,JGRD1,NFLDS,LEVS,1,JGRD1,CLAT,SL,           &
     &           NONHYD,RDELX2,RDELY2,RLAT1,RLON1,RLAT2,RLON2,             &
     &           XMAP,NTRAC,NCLD,FXS)        

!..........................
!     DO LAT1=1,JGRD1,NCPUS
!        LAT2=MIN(LAT1+NCPUS-1,JGRD1)
!................................

!  COMPUTE AUXILIARY QUANTITIES ON SIGMA AND INTERPOLATE TO PRESSURE    
!  AND COMPUTE SUNDRY FIELDS AND PACK FOR TRANSPOSE IN PARALLEL         
!MIC$ DO ALL                                                            
!MIC$1 SHARED(AOMEGA,LEVS,SI,SL,KO,POKPA,FXS,FXP)                       
!MIC$1 SHARED(NSIG,IGRD1,JGRD1,NFLDS,NFLDP,CLAT)                        
!MIC$1 SHARED(RLAT1,RLAT2,RLON1,RLON2,RDELX2,RDELY2)                    
!MIC$1 SHARED(KSZ,KSD,KST,KSQ,KSPSX,KSPSY,KSU,KSV,KSPS,KSZS,KSZSX,KSZSY)
!MIC$1 SHARED(KPU,KPV,KPO,KPZ,KPT,KPR,KPQ,KPSUN,KT,PTKPA,KSLP,KLI)          
!MIC$1 SHARED(KPTU,KPTV,KPTT,KPTR,KPA)                                  
!MIC$1 PRIVATE(LAT,RXS,OXS,ZXS,ZXI,QSXS,OSXS,QXP)                       
!MIC$1 PRIVATE(I,J,K,IP1,IM1,LATP1,LATM1)                               
        DO LAT=1,JGRD1                                                  
          PRINT *,' START DOING LAT=',LAT                               
          CALL GETRH(IGRD1,IGRD1,LEVS,                                   &
     &               FXS(1,KSPN,LAT),FXS(1,KSQ,LAT),FXS(1,KSTN,LAT),     &
     &               QSXS,RXS)                                          
          CALL OMEGAW(IGRD1,IGRD1,LEVS,SI,SL,                            &
     &               FXS(1,KSPS,LAT),FXS(1,KSPSX,LAT),FXS(1,KSPSY,LAT),  &
     &               FXS(1,KSD,LAT),FXS(1,KSU,LAT),FXS(1,KSV,LAT),       &
     &               FXS(1,KSPN,LAT),FXS(1,KSTN,LAT),FXS(1,KSWN,LAT),    &
     &               OXS,OSXS,WXS,NONHYD)
          CALL HYDRO(IGRD1,IGRD1,LEVS,SI,SL,                             &
     &               FXS(1,KSZS,LAT),FXS(1,KST,LAT),                     &
     &               ZXS,ZXI)                                           
          CALL SIG2P(IGRD1,IGRD1,LEVS,SI,SL,                             &
     &               FXS(1,KSPS,LAT),FXS(1,KSPN,LAT),                    &
     &               FXS(1,KSU,LAT),FXS(1,KSV,LAT),OXS,WXS,              &
     &               ZXS,ZXI,FXS(1,KSTN,LAT),RXS,FXS(1,KSQ,LAT),         &
     &               FXS(1,KSQC,LAT),FXS(1,KSQR,LAT),                    &
     &               FXS(1,KSCL,LAT),FXS(1,KSOZ,LAT),                    &
     &               KO,POKPA,                                           &
     &               FXP(1,KPU,LAT),FXP(1,KPV,LAT),FXP(1,KPO,LAT),       &
     &               FXP(1,KPW,LAT),                                     &
     &               FXP(1,KPZ,LAT),FXP(1,KPT,LAT),FXP(1,KPR,LAT),       &
     &               FXP(1,KPQ,LAT),                                     &
     &               FXP(1,KPQC,LAT),FXP(1,KPQR,LAT),                    &
     &               FXP(1,KPCL,LAT),FXP(1,KPOZ,LAT))                    
          CALL SIG2PT(IGRD1,IGRD1,LEVS,SI,SL,FXS(1,KSPS,LAT),            &
     &               FXS(1,KSU,LAT),FXS(1,KSV,LAT),                      &
     &               FXS(1,KSTN,LAT),FXS(1,KSQ,LAT),QSXS,                &
     &               KT,PTKPA,                                           &
     &               FXP(1,KPTU,LAT),FXP(1,KPTV,LAT),                    &
     &               FXP(1,KPTT,LAT),QXP,FXP(1,KPTR,LAT))               
          CALL SIG2Z(IGRD1,IGRD1,LEVS,ZXS,                               &
     &               FXS(1,KSU,LAT),FXS(1,KSV,LAT),FXS(1,KSTN,LAT),      &
     &               KZZ,ZZ,FXP(1,KZZU,LAT),                             &
     &               FXP(1,KZZV,LAT),FXP(1,KZZT,LAT))
          CALL SUNDRY(IGRD1,IGRD1,LEVS,KSLP,KLI,K5Z,CLAT(1,LAT),         &
     &                SI,SL,KT,PTKPA,                                    &
     &                FXS(1,KSPS,LAT),OSXS,FXS(1,KSPN,LAT),              &
     &                FXS(1,KSU,LAT),FXS(1,KSV,LAT),OXS,                 &
     &                FXS(1,KSTN,LAT),RXS,FXS(1,KSQ,LAT),QSXS,ZXS,ZXI,   &
     &                FXP(1,KPTT,LAT),QXP,                               &
     &                FXP(1,KPZ,LAT),FXP(1,KPT,LAT),                     &
     &                FXS(1,KSOZ,LAT),FXS(1,KSCL,LAT),                   &
     &                FXP(1,KPSUN,LAT))                                 
        ENDDO                                                           
!................
!     ENDDO
!..................
                                                                        
! CALCULATE ABSOLUTE VORTICITY
        DO LAT=1,JGRD1                                                  
          LATM1=MAX( 1,LAT-1)                                           
          LATP1=MIN(JGRD1,LAT+1)                                        
          JLAT=(LAT-1)*IGRD1
          DO K=1,KO                                                     
           DO I=1,IGRD1                                                 
            IM1=MAX( 1,I-1)                                             
            IP1=MIN(IGRD1,I+1)                                          
            FXP(I,KPA-1+K,LAT)=((FXP(IP1,KPV-1+K,LAT  )                  &   
     &                          -FXP(IM1,KPV-1+K,LAT  ))*RDELX2          &
     &                         -(FXP(I  ,KPU-1+K,LATP1)                  &
     &                          -FXP(I  ,KPU-1+K,LATM1))*RDELY2)        
            FXP(I,KPA-1+K,LAT)=FXP(I,KPA-1+K,LAT)*XMAP(I+JLAT)           &
     &                        +2.0*AOMEGA*SIN(CLAT(I,LAT))              
           ENDDO                                                        
          ENDDO                                                         
        ENDDO                                                           
!                                                                       
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RAD=ACOS(-1.)/180.
      TWOPI=2.0*ACOS(-1.)
      JCHECK=0                                                          
      IF( PROJO.EQ.-999. ) THEN                                         
        PROJO=PROJ                                                      
      ELSE                                                              
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( DELXO.EQ.-999. ) THEN                                         
        DELXO=DELX                                                      
      ELSE                                                              
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( DELYO.EQ.-999. ) THEN                                         
        DELYO=DELY                                                      
      ELSE                                                              
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( ORIENTO.EQ.-999. ) THEN                                       
        ORIENTO=ORIENT                                                  
      ELSE                                                              
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( TRUTHO.EQ.-999. ) THEN                                        
        TRUTHO=TRUTH                                                    
      ELSE                                                              
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( COTRUO.EQ.-999. ) THEN                                        
        COTRUO=COTRU                                                    
      ELSE                                                              
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( RLAT1O.EQ.-999. ) THEN                                        
        RLAT1O=RLAT1                                                    
      ELSE                                                              
        RLAT1O=RLAT1O*RAD                                                    
        IF(RLAT1O.LT.0.E0) RLAT1O=TWOPI+RLAT1O
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( RLON1O.EQ.-999. ) THEN                                        
        RLON1O=RLON1                                                    
      ELSE                                                              
        RLON1O=RLON1O*RAD                                                    
        IF(RLON1O.LT.0.E0) RLON1O=TWOPI+RLON1O
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( RLAT2O.EQ.-999. ) THEN                                        
        RLAT2O=RLAT2                                                    
      ELSE                                                              
        RLAT2O=RLAT2O*RAD                                                   
        IF(RLAT2O.LT.0.E0) RLAT2O=TWOPI+RLAT2O
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      IF( RLON2O.EQ.-999. ) THEN                                        
        RLON2O=RLON2                                                    
      ELSE                                                              
        RLON2O=RLON2O*RAD
        IF(RLON2O.LT.0.E0) RLON2O=TWOPI+RLON2O
        JCHECK=JCHECK+1                                                 
      ENDIF                                                             
      print *,' icheck ',JCHECK
      print *,' projo ',projo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      NPROJ=PROJO                                                    
      IF( NPROJ.EQ. 0 ) THEN                                            
        IDRT=1			! MERCATER                                             
        ORTRU=TRUTH                                                     
        PRINT *,' MERCATER PROJECTION.'                                 
      ELSEIF( ABS(NPROJ).EQ.1 ) THEN                                    
        IDRT=5			! POLAR PROJECTION                                     
        ORTRU=ORIENT                                                    
        PRINT *,' POLAR PROJECTIO.'                                     
      ELSEIF( ABS(NPROJ).EQ.2 ) THEN                                    
        IDRT=3                  ! LAMBERT PROJECTION                    
        IPROJ=ORIENT                                                    
        PRINT *,' LAMBERT PROJECTION.'                                 
      ELSEIF( NPROJ.EQ. 4 ) THEN
        IDRT=0                  ! LATLON  
        PRINT *,' LATLON PROJECTION.'
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
      print *,'STARTLON DLON STARTLAT DLAT',STRLON,DLON,STRLAT,DLAT
! PRS                                                                   
      NPRS=61                                                           
      DO K=1,KO                                                         
        IZ(K)=NINT(PO(K))                                               
      ENDDO                                                             
      CALL CTLHEAD(NPRS,IGRD1,JGRD1,KO,PROJ,TRUTH,ORIENT,                &
     &             CENLAT,CENLON,GRDLEFT,GRDBOTM,DX,DY,                  & 
     &             IHR,IDAY,IMON,IYR,IFH,STRLON,DLON,STRLAT,DLAT,        &
     &             FLAT,IZ)                                             
! SLR                                                                   
      NSLR=62                                                           
      ITLSLR=108                                                        
      KSLR=3                                                            
      IZ(1)=100                                                         
      IZ(2)=94                                                          
      IZ(3)=72                                                          
      CALL CTLHEAD(NSLR,IGRD1,JGRD1,KSLR,PROJ,TRUTH,ORIENT,               &
     &             CENLAT,CENLON,GRDLEFT,GRDBOTM,DX,DY,                   &
     &             IHR,IDAY,IMON,IYR,IFH,STRLON,DLON,STRLAT,DLAT,         &
     &             FLAT,IZ)                                             
! DLR                                                                   
      NDLR=63                                                           
      ITLDLR=112                                                        
      KDLR=2                                                            
      IZ(1)=200                                                         
      IZ(2)=10                                                          
      CALL CTLHEAD(NDLR,IGRD1,JGRD1,KDLR,PROJ,TRUTH,ORIENT,               &
     &             CENLAT,CENLON,GRDLEFT,GRDBOTM,DX,DY,                   &
     &             IHR,IDAY,IMON,IYR,IFH,STRLON,DLON,STRLAT,DLAT,         &
     &             FLAT,IZ)                                             
!                                                                       
!  CHECK TO DO INTERPOLATION OR NOT                                     
!     IGRID=255                                                         
      IGRID=IGRIDO                                                    
      IF ( JCHECK.GT.0 ) THEN                                           
        IGRID=IGRIDO                                                    
        PRINT *,' PREPARE GRID TO GRID INTERPOLATION.'                  
        PRINT *,' INPUT GRID : '                                        
        PRINT *,' PROJ=',PROJ,' ORIENT=',ORIENT,                          &
     &        ' TRUTH=',TRUTH,' COTRU=',COTRU,                            &
     &        ' DELX=',DELX,' DELY=',DELY,                                &
     &        ' RLAT1=',RLAT1,' RLON1=',RLON1,                            &
     &        ' RLAT2=',RLAT2,' RLON2=',RLON2                           
        PRINT *,' OUTPUT GRID : '                                       
        PRINT *,' PROJO=',PROJO,' ORIENTO=',ORIENTO,                      &
     &        ' TRUTHO=',TRUTHO,' COTRUO=',COTRUO,                        &
     &        ' DELXO=',DELXO,' DELYO=',DELYO,                            &
     &        ' RLAT1O=',RLAT1O,' RLON1O=',RLON1O,                        &
     &        ' RLAT2O=',RLAT2O,' RLON2O=',RLON2O                       
        CALL I2OINI(PROJ ,ORIENT ,TRUTH ,COTRU ,                          &
     &              DELX ,DELY ,RLAT1 ,RLON1 ,RLAT2,RLON2,IGRD1, JGRD1,   &
     &              PROJO,ORIENTO,TRUTHO,COTRUO,                          &
     &              DELXO,DELYO,RLAT1O,RLON1O,RLAT2O,RLON2O,              &
     &              N00,N10,N11,N01,D00,D10,D11,D01,IO,JO)              
      ENDIF                                                             
!                                                                       
!  LOOP OVER GROUPS OF HORIZONTAL FIELDS                                
      DO K1=1,NFLDP,NCPUS                                               
        K2=MIN(K1+NCPUS-1,NFLDP)                                        
!  UNPACK TRANSPOSED FIELDS AND INTERPOLATE TO OUTPUT GRID              
!  AND ROUND TO THE NUMBER OF BITS AND ENGRIB THE FIELD IN PARALLEL     
!MIC$ DO ALL                                                            
!MIC$1 SHARED(K1,K2,POT,IGRD1,IGRD11,JGRD1,IO,JO,NFLDP,FXY,FXP)         
!MIC$1 SHARED(IPU,ITL,IL1,IL2,ICEN,IGEN,IDATE,LENPDS,JFHOUR,IDS,MXBIT)  
!MIC$1 SHARED(GRIB,LGRIB,ORTRUO,PROJO,DELXO,DELYO)                      
!MIC$1 SHARED(RLAT1O,RLON1O,RLAT2O,RLON2O,IDRT)                         
!MIC$1 SHARED(ICEN2,IENS,ILAT2,ILON2,IDRT,JCHECK)                       
!MIC$1 SHARED(N00,N10,N11,N01,D00,D10,D11,D01)                          
!MIC$1 PRIVATE(I,J,K,KAN,IERR,POK,POKTOP,AWIPS)                         
        DO K=K1,K2                                                      
          KAN=K-K1+1                                                    
          LGRIB(KAN)=0                                                  
!  didn't output IPUQC IPUQR
          IF(IPU(K).GT.0.AND.ITL(K).NE.107) THEN                        
!         IF(IPU(K).GT.0.AND.ITL(K).NE.107.AND.IPU(K).NE.191             &
!    &                  .AND.IPU(K).NE.192) THEN                        
            DO J=1,JGRD1                                                
              DO I=1,IGRD1                                              
                FXY(I,J,KAN)=FXP(I,K,J)                                 
              ENDDO                                                     
            ENDDO                                                       
! IF NEED TRANSFORM FROM ONE GRID TO ANOTHER                             
            IF( JCHECK.EQ.0 ) THEN                                      
              DO J=1,JGRD1                                                 
                DO I=1,IGRD1                                               
                  AWIPS(I,J)=FXY(I,J,KAN)                               
                ENDDO                                                   
              ENDDO                                                     
            ELSE                                                        
           print*,'before call TOAWIPS -- 1'
              CALL TOAWIPS(FXY(1,1,KAN),IGRD1*JGRD1,AWIPS,IO*JO,         &  
     &                     N00,N10,N11,N01,D00,D10,D11,D01)             
           print*,'after call TOAWIPS -- 1'
            ENDIF                                                       
            LBMO=.TRUE.
!MIC$ GUARD                                                             
            CALL GRIBIT(AWIPS,LBMO,IDRT,IGRID,IO,JO,MXBIT,0.,            &
     &                 LENPDS,2,ICEN,IGEN,0,                             & 
     &                 IPU(K),ITL(K),IL1(K),IL2(K),                      & 
     &                 IDATE(4),IDATE(2),IDATE(3),IDATE(1),              &
     &                 1,JFHOUR,0,10,                                    &
     &                 0,0,ICEN2,IDS(IPU(K)),IENS,                       &
     &                 RLAT1O,RLON1O,RLAT2O,RLON2O,                      &
     &                 DELXO,DELYO,ORIENTO,TRUTHO,PROJO,                 &      
     &                 GRIB(1,KAN),LGRIB(KAN),IERR)                     
!MIC$ ENDGUARD                                                          
          ENDIF                                                         
        ENDDO                                                           
!  WRITE OUT GRIB MESSAGES SEQUENTIALLY                                 
        DO K=K1,K2                                                      
          KAN=K-K1+1                                                    
          IF(LGRIB(KAN).GT.0) THEN                                      
            CALL WRYTE(NPGB,LGRIB(KAN),GRIB(1,KAN))                     
            PRINT *,' GRIB1 WRITTEN TO ',NPGB,' OF LENGTH ',LGRIB(KAN)  
            IF(ITL(K).EQ.ITLSLR) THEN                                   
              CALL CTLVAR(NSLR,IPU(K),ITL(K))                           
            ELSE                                                        
              CALL CTLVAR(NPRS,IPU(K),ITL(K))                           
            ENDIF                                                       
          ENDIF                                                         
        ENDDO                                                           
      ENDDO                                                             
!                                                                       
! ----------------------------------------------------------------------
! TABLE     KPDS IPDS NAME    KPDS IPDS NAME      KPDS IPD NAMES        
!                   1            7   11 IL2K       17   21 INA          
!                   2            8   12 IYR        20   22 INM          
!              1    3 ICEN       9   13 IMO        21   23 ICNT         
!              2    4 IGEN      10   14 IDY        23   24 ICEN2        
!              3    5 IGRID     11   15 IHR        22   25 IDSK         
!              4    6 IFLG      12   16 IMIN                            
!                   7 IBMS      13   17 IFTU                            
!              5    8 IPUK      14   18 IP1                             
!              6    9 ITLK      15   19 IP2                             
!              7   10 IL1K      16   20 ITR                             
! ----------------------------------------------------------------------
! READ FLUX OUTPUT FROM MODEL AND GRIBIT                                
      ICHECK=0                                                          
      KCHECK=0                                                          
      KF=IGRD1*JGRD1                                                    
      DO N=1,100                                                        
!     READ(NFLX,END=912,ERR=910)                                        
!    &                  FLX,LBM,IDRTX,IOX,JOX,MXBIT,COLAT1,             
!    &                  LNPDS,IPTV,ICEN,IGEN,IBMS,                      
!    &                  IPUK,ITLK,IL1K,IL2K,                            
!    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,               
!    &                  INA,INM,ICEN2,IDSK,IENS,                        
!    &                  RLAT1,RLON1,RLAT2,RLON2,DELX,DELY,ORTRU,PROJ    
      READ(NFLX,END=912,ERR=910)                                          &
     &                  FLX_s,LBM,IDRTX,IOX,JOX,MXBIT,COLAT1_s,           &
     &                  LNPDS,IPTV,ICEN,IGEN,IBMS,                        &
     &                  IPUK,ITLK,IL1K,IL2K,                              &
     &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,                 &
     &                  INA,INM,ICEN2,IDSK,IENS,                          &
     &                  RLAT1_s,RLON1_s,RLAT2_s,RLON2_s,DELX_s,DELY_s,    &
     &                  ORTRU_s,PROJ_s   
      FLX=FLX_s
      COLAT1=COLAT1_s
      RLAT1=RLAT1_s
      RLAT2=RLAT2_s
      RLON1=RLON1_s
      RLON2=RLON2_s
      DELX=DELX_s
      DELY=DELY_s
      ORTRU=ORTRU_s
      PROJ=PROJ_s

      PRINT *,' READ FLX RECORD IOX JOX =',N,IOX,JOX
!  CHANGE PRECIPITATION RATE TO PRECIPITATION                           
!  CHANGE SURFACE STRESS PARAMETERS                                     
!  CHANGE CLOUD PARAMETERS                                              
!  CHANGE SIGMA LEVEL ZERO                                              
!  CHANGE FIXED ELEVATION ABOVE GROUND                                  
!  SKIP SURFACE PRESSURE                                                
!  CHANGE BITMAP TO OFF FOR SOME FIELDS                                 
        IF((ITR.EQ.3.OR.ITR.EQ.4).AND.IP2.EQ.IP1) THEN                  
          PRINT '("...SKIPPED FIELD ",I5)',IPUK                         
          IPUK=0                                                        
        ELSEIF(IPUK.EQ.59.AND.IFTU.EQ.1.AND.ITR.EQ.3) THEN              
          IPUK=61                                                       
          ITR=4                                                         
          IDSK=IDS(IPUK)                                                
          DO I=1,KF                                                     
            FLX(I)=3600*(IP2-IP1)*FLX(I)                                
          ENDDO                                                         
          WRITE(*,'("...CHANGED FIELD 59 TO 61")')                      
        ELSEIF(IPUK.EQ.214.AND.IFTU.EQ.1.AND.ITR.EQ.3) THEN             
          IPUK=63                                                       
          ITR=4                                                         
          IDSK=IDS(IPUK)                                                
          DO I=1,KF                                                     
            FLX(I)=3600*(IP2-IP1)*FLX(I)                                
          ENDDO                                                         
          WRITE(*,'("...CHANGED FIELD 214 TO 63")')                     
        ELSEIF(IPUK.EQ.170.OR.IPUK.EQ.171) THEN                         
          WRITE(*,'("...CHANGED FIELD 170 AND 171 TO 134 AND 135")')    
          IPUK=IPUK-46                                                  
        ELSEIF(ITLK.EQ.12.OR.ITLK.EQ.13.OR.ITLK.EQ.14.OR.                 &
     &         ITLK.EQ.22.OR.ITLK.EQ.23.OR.ITLK.EQ.24.OR.                 &
     &         ITLK.EQ.32.OR.ITLK.EQ.33.OR.ITLK.EQ.34) THEN             
          WRITE(*,'("...CHANGED LEVEL TYPE ")')                         
          ITLK=ITLK+200                                                 
        ELSEIF(ITLK.EQ.107.AND.IL1K.EQ.0.AND.IL2K.EQ.0) THEN            
          ITLK=8                                                        
          WRITE(*,'("...CHANGED LEVEL TYPE 107 TO 8")')                 
        ELSEIF(ITLK.EQ.105.AND.IL1K.EQ.0.AND.IL2K.EQ.0) THEN            
          IF(IPUK.EQ.11.OR.IPUK.EQ.51) IL1K=2                           
          IF(IPUK.EQ.33.OR.IPUK.EQ.34) IL1K=10                          
          WRITE(*,'("...CHANGED ELEVATION ",I5," TO ",I5)')0,IL1K       
        ELSEIF(IPUK.EQ.1.AND.ITLK.EQ.1) THEN                            
          IPUK=0                                                        
          WRITE(*,'("...SKIPPED FIELD IPU=1 AND ITL=1")')               
        ELSEIF(IPUK.EQ.54) THEN                                         
          IPUK=0                                                        
          PRINT '("...SKIPPED FIELD 54")'                               
        ENDIF                                                           
!                                                                       
        IF(IBMS.NE.0.AND.(IPUK.EQ.61.OR.IPUK.EQ.63.OR.                    &
     &                    IPUK.EQ.65.OR.IPUK.EQ.71)) THEN               
          PRINT *,' BITMAP PROBLEM IPUK=',IPUK                          
          IBMS=0                                                        
          DO I=1,KF                                                     
            IF(.NOT.LBM(I)) FLX(I)=0.                                   
          ENDDO                                                         
        ENDIF                                                           
!                                                                       
        IF(IPUK.EQ.71.OR.IPUK.EQ.72) THEN                               
          IBMS=2                                                        
          DO I=1,KF                                                     
            LBM(I)=FLX(I).GT.0.                                         
          ENDDO                                                         
        ENDIF                                                           
! SAVE FOR LATER                                                        
        IF(IPUK.EQ.51.AND.ITLK.EQ.105) THEN                             
          ICHECK=ICHECK+1                                               
          DO I=1,KF                                                     
            Q2(I)=FLX(I)                                                
          ENDDO                                                         
        ENDIF                                                           
        IF(IPUK.EQ.11.AND.ITLK.EQ.105) THEN                             
          ICHECK=ICHECK+1                                               
          DO I=1,KF                                                     
            T2(I)=FLX(I)                                                
          ENDDO                                                         
        ENDIF                                                           
        IF(IPUK.EQ.1.AND.ITLK.EQ.1) THEN                                
          ICHECK=ICHECK+1                                               
          DO I=1,KF                                                     
            PS(I)=FLX(I)                                                
          ENDDO                                                         
        ENDIF                                                           
!----------------------ppt type------------
        IF(IPUK.EQ.61.AND.ITR.EQ.4) THEN              
          KCHECK=1
          ij=0
          do j=1,jgrd1
            do i=1,igrd1
              ij=ij+1
              do k=1,levs
                kk=levs+1-k
                ts(kk)=fxs(i,kstn+k-1,j)
                qs(kk)=fxs(i,ksq+k-1,j)
! change from cb to pa (1 centbar=1000 pa)
                plry(kk  )=fxs(i,ksps,j)*sl(k)*1000.  
                plvl(kk+1)=fxs(i,ksps,j)*si(k)*1000.
              enddo
              plvl(1)=fxs(i,ksps,j)*si(levs+1)*1000.
! category 4 precip types: iwx(ij)
              call calwxt1(levs,flx(ij),ts,qs,plry,plvl,iwx(ij))
            enddo
          enddo
        ENDIF
!                                                                       
        IF(IPUK.GT.0) THEN                                              
          IJ=0                                                          
          DO J=1,JGRD1                                                     
            DO I=1,IGRD1                                                   
              IJ=IJ+1                                                   
              FXY(I,J,1)=FLX(IJ)                                        
            ENDDO                                                       
          ENDDO                                                         
! IF NEED TRANSFORM FROM ONE GRID TO ANOTHER                             
          IF( JCHECK.EQ.0 ) THEN                                        
            DO J=1,JO                                                   
              DO I=1,IO                                                 
                AWIPS(I,J)=FXY(I,J,1)                                   
              ENDDO                                                     
            ENDDO                                                       
            LBMO=LBM
          ELSE                                                          
            print*,'before call TPAWIPS -- 2'
            IF (NEWSLM.EQ.1) THEN
               RLBM2=LBM2
            ELSE
               RLBM1=0.0
               DO I=1,IGRD1*JGRD1
                  if (LBM(I)) RLBM1(I)=1.0
               ENDDO
               CALL TOAWIPS(RLBM1,IGRD1*JGRD1,RLBM2,IO*JO,                      &
     &                      N00,N10,N11,N01,D00,D10,D11,D01)
            ENDIF
            DO I=1,IO*JO
               IF (RLBM2(I).GT.0.4) THEN
                  LBMO(I)=.TRUE. 
               ELSE
                  LBMO(I)=.FALSE.
               ENDIF  
            ENDDO
            IF ( ITLK.EQ.1 ) THEN
            CALL EXPND(FXY(1,1,1),RLBM2,0,FXYSEA,IWORK,IGRD1,JGRD1)
            CALL EXPND(FXY(1,1,1),RLBM2,1,FXYLAND,IWORK,IGRD1,JGRD1)
            CALL TOAWIPS(FXYSEA,IGRD1*JGRD1,AWIPS,IO*JO,                   &
     &                   N00,N10,N11,N01,D00,D10,D11,D01)
            CALL TOAWIPS(FXYLAND,IGRD1*JGRD1,RR,IO*JO,                     &
     &                   N00,N10,N11,N01,D00,D10,D11,D01)
            DO I=1,IO
            DO J=1,JO
               IF (LBMO((I-1)*JO+J)) AWIPS(I,J)=RR(I,J)
            ENDDO
            ENDDO
            ELSE
            CALL TOAWIPS(FXY(1,1,1),IGRD1*JGRD1,AWIPS,IO*JO,               &
     &                   N00,N10,N11,N01,D00,D10,D11,D01)               
            ENDIF
            print*,'after call TPAWIPS -- 2'
! use new landsfc
            IF (IPUK.EQ.81) THEN
              DO I=1,IO
              DO J=1,JO
                AWIPS(I,J)=RLBM2((J-1)*IO+I)
              ENDDO
              ENDDO
            ENDIF
          ENDIF                                                         

          CALL GRIBIT(AWIPS,LBMO,IDRT,IGRID,IO,JO,MXBIT,0.0,               &   
     &                LNPDS,IPTV,ICEN,IGEN,IBMS,                          &
     &                IPUK,ITLK,IL1K,IL2K,                                &
     &                IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,                   &
     &                INA,INM,ICEN2,IDSK,IENS,                            &
     &                RLAT1O,RLON1O,RLAT2O,RLON2O,                        &
     &                DELXO,DELYO,ORIENTO,TRUTHO,PROJO,                   &     
     &                GRIB(1,1),LGRIB(1),IERR)                          
          IF(LGRIB(1).GT.0) THEN                                        
            CALL WRYTE(NPGB,LGRIB(1),GRIB(1,1))                         
            PRINT *,' GRIB1 WRITTEN TO ',NPGB,' OF LENGTH ',LGRIB(1)    
            IF(ITLK.EQ.ITLSLR) THEN                                     
              CALL CTLVAR(NSLR,IPUK,ITLK)                               
            ELSEIF(ITLK.EQ.ITLDLR) THEN                                 
              CALL CTLVAR(NDLR,IPUK,ITLK)                               
            ELSE                                                        
              CALL CTLVAR(NPRS,IPUK,ITLK)                               
            ENDIF                                                       
          ENDIF                                                         
        ENDIF                                                           
        GO TO 911                                                       
!                                                                       
910     PRINT *,' ERROR READING FLX RECORD '                            
911     PRINT *,' READ NEXT RECORD.'                                    
      ENDDO                                                             
912   PRINT *,' END OF READ FLX FILE '                                  
      PRINT *,' ICHECK=',ICHECK                                         
      PRINT *,' JCHECK=',JCHECK                                         
      IF( ICHECK.EQ.3 ) THEN                                            
        DO I=1,KF                                                       
          P2=PS(I)*EXP(-GOR2/T2(I))*1.E-3                               
          PVS2=FPVS(T2(I))                                              
          QS2=EPS*PVS2/(P2+EPSM1*PVS2)                                  
          FLX(I)=MIN(MAX(Q2(I)/QS2,0.),1.)*100.                         
          LBM(I)=.TRUE.                                                 
        ENDDO                                                           
        IPUK=52                                                         
        ITLK=105                                                        
        IL1K=2                                                          
        IL2K=0                                                          
        IDSK=IDS(IPUK)                                                  
        IJ=0                                                            
        DO J=1,JGRD1                                                       
          DO I=1,IGRD1                                                     
            IJ=IJ+1                                                     
            FXY(I,J,1)=FLX(IJ)                                          
          ENDDO                                                         
        ENDDO                                                           
! IF NEED TRANSFORM FOM ONE GRID TO ANOTHER                             
        IF( JCHECK.EQ.0 ) THEN                                          
          DO J=1,JO                                                     
            DO I=1,IO                                                   
              AWIPS(I,J)=FXY(I,J,1)                                     
            ENDDO                                                       
          ENDDO                                                         
          LBMO=LBM
        ELSE                                                            
       print*,'before call TOAWIPS -- 3'
          CALL TOAWIPS(FXY(1,1,1),IGRD1*JGRD1,AWIPS,IO*JO,               &
     &                   N00,N10,N11,N01,D00,D10,D11,D01)               
       print*,'after call TOAWIPS -- 3'
          LBMO=.TRUE.
        ENDIF                                                           
        CALL GRIBIT(AWIPS,LBMO,IDRT,IGRID,IO,JO,MXBIT,0.0,                &    
     &            LNPDS,IPTV,ICEN,IGEN,IBMS,                             &
     &            IPUK,ITLK,IL1K,IL2K,                                   &
     &            IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,                      &
     &            INA,INM,ICEN2,IDSK,IENS,                               &
     &            RLAT1O,RLON1O,RLAT2O,RLON2O,                           &
     &            DELXO,DELYO,ORIENTO,TRUTHO,PROJO,                      &        
     &            GRIB(1,1),LGRIB(1),IERR)                              
        IF(LGRIB(1).GT.0) THEN                                          
          CALL WRYTE(NPGB,LGRIB(1),GRIB(1,1))                           
          PRINT *,' GRIB1 WRITTEN TO ',NPGB,' OF LENGTH ',LGRIB(1)      
          IF(ITLK.EQ.ITLSLR) THEN                                       
            CALL CTLVAR(NSLR,IPUK,ITLK)                                 
          ELSEIF(ITLK.EQ.ITLDLR) THEN                                   
            CALL CTLVAR(NDLR,IPUK,ITLK)                                 
          ELSE                                                          
            CALL CTLVAR(NPRS,IPUK,ITLK)                                 
          ENDIF                                                         
        ENDIF                                                           
      ENDIF                                                             
!                                                                       
! precipitation type (winter weather):
!                                ids ntype  ndx
!             crain=mod(iwx/8,2) 140   4     8
!             cfrzr=mod(iwx/4,2) 141   3     4
!             cicep=mod(iwx/2,2) 142   2     2
!             csnow=mod(iwx/1,2) 143   1     1
!
      IF( KCHECK.EQ.1 ) THEN                                            
        ITLK=1                                                        
        IL1K=0                                                          
        IL2K=0                                                          
        IP1=IP2
        IP2=0
        ITR=0
!       ITR=10
       do ntype=1,4
        ndx=2**(ntype-1)
        DO I=1,KF                                                       
          FLX(I)=MOD(IWX(I)/ndx,2)
        ENDDO                                                           
        IPUK=144-ntype                                                         
        IDSK=IDS(IPUK)                                                  
        IJ=0                                                            
        DO J=1,JGRD1                                                       
          DO I=1,IGRD1                                                     
            IJ=IJ+1                                                     
            FXY(I,J,1)=FLX(IJ)                                          
          ENDDO                                                         
        ENDDO                                                           
! IF NEED TRANSFORM FROM ONE GRID TO ANOTHER                             
        IF( JCHECK.EQ.0 ) THEN                                          
          DO J=1,JO                                                     
            DO I=1,IO                                                   
              AWIPS(I,J)=FXY(I,J,1)                                     
            ENDDO                                                       
          ENDDO                                                         
        ELSE                                                            
       print*,'before call TOAWIPS -- 4'
          CALL TOAWIPS(FXY(1,1,1),IGRD1*JGRD1,AWIPS,IO*JO,               &
     &    N00,N10,N11,N01,D00,D10,D11,D01)               
       print*,'after call TOAWIPS -- 4'
        ENDIF                                                           

! test
!       write(6,135) ((awips(i,j),i=45,70),j=jO,1,-1)
!135    format(26f3.1)
!      if(ntype.eq.4) then
!      print*,'after call TOAWIPS -- 4'
!      print*,'LBM=',LBM
!      print*,'IDRT=',IDRT
!      print*,'IGRID=',IGRID
!      print*,'IO=',IO
!      print*,'JO=',JO
!      print*,'MXBIT=',MXBIT
!      print*,'LNPDS=',LNPDS
!      print*,'IPTV=',IPTV
!      print*,'ICEN=',ICEN
!      print*,'IGEN=',IGEN
!      print*,'IBMS=',IBMS
!      print*,'IPUK=',IPUK
!      print*,'ITLK=',ITLK
!      print*,'IL1K=',IL1K
!      print*,'IL2K=',IL2K
!      print*,'RLAT1O=',RLAT1O
!      print*,'RLON1O=',RLON1O
!      print*,'RLAT2O=',RLAT2O
!      print*,'RLON2O=',RLON2O
!      print*,'DELXO=',DELXO
!      print*,'DELYO=',DELYO
!      print*,'ORIENTO=',ORIENTO
!      print*,'TRUTHO=',TRUTHO
!      print*,'PROJO=',PROJO
!      endif

! correct a bug
       LBM=.TRUE.
       IBMS=0
       LBMO=.TRUE.
        CALL GRIBIT(AWIPS,LBMO,IDRT,IGRID,IO,JO,MXBIT,0.0,                 &
     &            LNPDS,IPTV,ICEN,IGEN,IBMS,                              & 
     &            IPUK,ITLK,IL1K,IL2K,                                    &
     &            IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,                       &
     &            INA,INM,ICEN2,IDSK,IENS,                                &
     &            RLAT1O,RLON1O,RLAT2O,RLON2O,                            &
     &            DELXO,DELYO,ORIENTO,TRUTHO,PROJO,                       &     
     &            GRIB(1,1),LGRIB(1),IERR)                              
        IF(LGRIB(1).GT.0) THEN                                          
          CALL WRYTE(NPGB,LGRIB(1),GRIB(1,1))                           
          PRINT *,' GRIB1 WRITTEN TO ',NPGB,' OF LENGTH ',LGRIB(1)      
          IF(ITLK.EQ.ITLSLR) THEN                                       
            CALL CTLVAR(NSLR,IPUK,ITLK)                                 
          ELSEIF(ITLK.EQ.ITLDLR) THEN                                   
            CALL CTLVAR(NDLR,IPUK,ITLK)                                 
          ELSE                                                          
            CALL CTLVAR(NPRS,IPUK,ITLK)                                 
          ENDIF                                                         
        ENDIF                                                           
       enddo
      ENDIF                                                             
!                                                                       
      CALL CTLVAR(0,0,0)                                                
      CALL CTLEND(NSLR)                                                 
      CALL CTLEND(NDLR)                                                 
      CALL CTLEND(NPRS)                                                 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE RRDSGH(NSIG,FHOUR,IDATE,SI,SL,                          &
     &                  IWAV1,JWAV1,IGRD1,JGRD1,LEVS,NONHYD,NFLDS,       &
     &                  PROJ,TRUTH,ORIENT,CENLAT,CENLON,                 &
     &                  GRDLEFT,GRDBOTM,DELX,DELY,IRET,NCLD)               
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RRDSGH      READ SIGMA GRID FILE HEADER RECORD         
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31            
!                                                                       
! ABSTRACT: READS THE HEADER RECORD FROM THE SIGMA GRID FILE.           
!           IF THE 32-BYTE ON85 LABEL BEGINS WITH EBCDIC 'SGR',         
!           THE DATA IS ASSUMED TO BE ON GAUSSIAN GRID SIGMA SURFACES   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-10-31  MARK IREDELL                                              
!   93- 8-17  HANN-MING HENRY JUANG                                     
!                                                                       
! USAGE:     CALL RRDSGH(NSIG,FHOUR,IDATE,SI,SL,                        
!    &                  IWAV1,JWAV1,IGRD1,JGRD1,LEVS,NFLDS,             
!    &                  PROJ,TRUTH,ORIENT,CENLAT,CENLON,                
!    &                  GRDLEFT,GRDBOTM,DELX,DELY,IRET)                 
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     NSIG     - INTEGER UNIT FROM WHICH TO READ HEADER                 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     FHOUR    - REAL FORECAST HOUR                                     
!     IDATE    - INTEGER (4) DATE                                       
!     SI       - REAL (LEVS+1) SIGMA INTERFACES                         
!     SL       - REAL (LEVS) SIGMA LEVELS                               
!     JCAP     - INTEGER SPECTRAL TRUNCATION                            
!     LEVS     - INTEGER NUMBER OF LEVELS                               
!     ITRUN    - INTEGER TRUNCATION FLAG (=1 FOR TRIANGULAR)            
!     IORDER   - INTEGER COEFFICIENT ORDER FLAG (=2 FOR IBM ORDER)      
!     IREALF   - INTEGER FLOATING POINT FLAG (=1 FOR IBM)               
!     IGEN     - INTEGER MODEL GENERATING FLAG                          
!     IGSGR    - INTEGER SIGMA GRID FILE EDITION (=0 FOR SIGMA SPECTRAL)
!     JGRD1    - INTEGER NUMBER OF LATITUDE PAIRS IN GAUSSIAN GRID      
!     IGRD1    - INTEGER NUMBER OF VALID DATA POINTS PER LATITUDE PAIR  
!     NFLDS    - INTEGER NUMBER OF DATA FIELDS PER GRIDPOINT            
!     NWHDR    - INTEGER NUMBER OF WORDS IN THE HEADER RECORD           
!     NWFLD    - INTEGER NUMBER OF WORDS IN EACH DATA RECORD            
!     IRET     - INTEGER RETURN CODE (=0 FOR OK, =1 FOR EOF, =2 FOR ERR)
!                                                                       
!   INPUT FILES:                                                        
!     NSIG     - SIGMA GRID FILE                                        
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   W3FI47       CONVERT ON85 LABEL FROM EBCDIC TO ASCII                
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      PARAMETER(LEVMAX=100,NWEXT=512-(6+2*LEVMAX))                      
      CHARACTER*32 CLABE,CLABA                                          
!     CHARACTER*8 LABEL(4)                                          
      DIMENSION IDATE(4)                                                
      DIMENSION SI(LEVMAX+1),SL(LEVMAX),SISL(2*LEVMAX+1),EXT(NWEXT)     
      REAL*4    FHOURS,SIS(LEVMAX+1),SLS(LEVMAX),SISLS(2*LEVMAX+1),      & 
     &          EXTS(NWEXT)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  READ AND EXTRACT HEADER RECORD                                       
      PRINT *,' RRDSGH: READ AND EXTRACT HEADER RECORD '                
      IRET=0                                                            
      REWIND NSIG                                                       
!     READ(NSIG,END=91,ERR=92) CLABE                                    
      READ(NSIG,END=91,ERR=92)                                          
      PRINT *,' after read label '                                      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  READ SIGMA GRID FILE HEADER                                          
!     READ(NSIG,END=91,ERR=92) FHOUR,IDATE,SISL,EXT                     
      READ(NSIG,END=91,ERR=92) FHOURS,IDATE,SISLS,EXTS
      FHOUR=FHOURS
!     IWAV1  =EXT(1)                                                    
!     JWAV1  =EXT(2)                                                    
!     IGRD1  =EXT(3)                                                    
!     JGRD1  =EXT(4)                                                    
!     LEVS   =EXT(5)                                                    
!     NFLDSX =EXT(6)                                                    
!     PROJ   =EXT(7)                                                    
!     TRUTH  =EXT(8)                                                    
!     ORIENT =EXT(9)                                                    
!     CENLAT =EXT(10)                                                   
!     CENLON =EXT(11)                                                   
!     GRDLEFT=EXT(12)                                                   
!     GRDBOTM=EXT(13)                                                   
!     DELX   =EXT(14)                                                   
!     DELY   =EXT(15)                                                   

      IWAV1  =EXTS(1)                                                    
      JWAV1  =EXTS(2)                                                    
      IGRD1  =EXTS(3)                                                    
      JGRD1  =EXTS(4)                                                    
      LEVS   =EXTS(5)                                                    
      NFLDSX =EXTS(6)                                                    
      PROJ   =EXTS(7)                                                    
      TRUTH  =EXTS(8)                                                    
      ORIENT =EXTS(9)                                                    
      CENLAT =EXTS(10)                                                   
      CENLON =EXTS(11)                                                   
      GRDLEFT=EXTS(12)                                                   
      GRDBOTM=EXTS(13)                                                   
      DELX   =EXTS(14)                                                   
      DELY   =EXTS(15)                                                   
      NONHYD =EXTS(16)
!
      PRINT *,' DELX DELY ',DELX,DELY
      NFLDS  =8+13*LEVS
!
      IF( NONHYD .EQ. 1 ) THEN
         PRINT *,' INPUT DATA IS A NONHYDROSTATIC TYPE.'
      ELSE
         PRINT *,' INPUT DATA IS A HYDROSTATIC TYPE.'
      ENDIF
!                                                                       
!     SI(1:LEVS+1)=SISL(1:LEVS+1)                                       
!     SL(1:LEVS)=SISL(LEVS+2:2*LEVS+1)                                  
      SI(1:LEVS+1)=SISLS(1:LEVS+1)                                       
      SL(1:LEVS)=SISLS(LEVS+2:2*LEVS+1)                                  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  END OF FILE ENCOUNTERED                                              
91    IRET=1                                                            
      RETURN                                                            
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  I/O ERROR ENCOUNTERED                                                
92    IRET=2                                                            
      RETURN                                                            
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE RRDSGR(NSIG,IGRD1,JGRD1,NFLDS,LEVS,LAT1,LAT2,CLAT,SL,     &
     &                  NONHYD,RDELX2,RDELY2,RLAT1,RLON1,RLAT2,RLON2,      &
     &                  XMAP,NTRAC,NCLD,F)        
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
! USAGE:    CALL RRDSGR(NSIG,IGRD1,JGRD1,NFLDS,LEVS,LAT1,LAT2,          
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
      real*4 GR1_s(IGRD1*JGRD1),GR2_s(IGRD1*JGRD1),GR3_s(IGRD1*JGRD1)      
      DIMENSION CLAT(IGRD1*JGRD1),XMAP(IGRD1*JGRD1),SL(LEVS)            
      COMMON/IOINDX/ KSZ,KSD,KST,KSQ,KSPSX,KSPSY,KSU,KSV,KSPS,KSZS,       &
     &               KSZSX,KSZSY,KSPN,KSTN,KSWN,KSQC,KSQR,KSOZ,KSCL,      &
     &               KPZ,KPU,KPV,KPR,KPQ,KPT,KPO,KPW,KPA,KPQC,KPQR,       &
     &               KPOZ,KPCL,                                           &
     &               KPTU,KPTV,KPTT,KPTR,KPTOZ,KPTCL,KPSUN        
!                                                                       
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! REWIND THEN SKIP LAB AND FHOUR RECORDS                                
      REWIND NSIG                                                       
      READ(NSIG)                                                        
      READ(NSIG)                                                        
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
      PRINT *,' EXTRACTING J FROM ',LAT1,' TO ',LAT2                    
      PRINT *,' COMPUTING J FROM ',LAT1X,' TO ',LAT2X                   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! GZ                                                                    
      READ(NSIG) (GR1_s(I),I=1,NWF)                                       
       do I=1,NWF
        GR1(I)=GR1_s(I)
       enddo
      
      PRINT *,' READ  GZ ',GR1(1)                                       
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
      READ(NSIG) (GR1_s(I),I=1,NWF)                                       
       do I=1,NWF
        GR1(I)=GR1_s(I)
       enddo
      
      PRINT *,' READ  LOG(PSFC) ',GR1(1)                                
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
      DO 10 K=1,LEVS                                                    
        KT=KST-1+K                                                      
        READ(NSIG) (GR1_s(I),I=1,NWF)                                       
         do I=1,NWF
          GR1(I)=GR1_s(I)
         enddo
      
        PRINT *,' READ  T  AT LEV= ',K,GR1(1)                           
        DO LAT=LAT1,LAT2                                                
          LAN=LAT-LAT1+1                                                
          JLAT=(LAT-1)*IGRD1                                            
          DO I=1,IGRD1                                                  
            F(I,KT,LAN)=GR1(I+JLAT)                                     
          ENDDO                                                         
        ENDDO                                                           
 10   CONTINUE                                                          
! U V                                                                   
      DO 20 K=1,LEVS                                                    
        KU=KSU-1+K                                                      
        KV=KSV-1+K                                                      
        KZ=KSZ-1+K                                                      
        KD=KSD-1+K                                                      
        READ(NSIG) (GR1_s(I),I=1,NWF)                                       
        READ(NSIG) (GR2_s(I),I=1,NWF)                                       
         do I=1,NWF
          GR1(I)=GR1_s(I)
          GR2(I)=GR2_s(I)
         enddo
      
        PRINT *,' READ  U V  AT LEV= ',K,GR1(1),GR2(1)                  
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
            F(I,KD,LAN)= RDELX2*(GR1(I+JLATE)-GR1(I+JLATW))               &
     &                  +RDELY2*(GR2(I+JLATN)-GR2(I+JLATS))               
            F(I,KZ,LAN)= RDELX2*(GR2(I+JLATE)-GR2(I+JLATW))               &
     &                  -RDELY2*(GR1(I+JLATN)-GR1(I+JLATS))             
          ENDDO                                                         
        ENDDO                                                           
 20   CONTINUE                                                          
! Q                                                                     
      DO 30 K=1,LEVS                                                    
        KQ=KSQ-1+K                                                      
        KK=KSWN-1+K        ! borrow WN for cloud waters                  
        READ(NSIG) (GR1_s(I),I=1,NWF)                                       
         do I=1,NWF
          GR1(I)=GR1_s(I)
         enddo
      
        PRINT *,' READ  Q  AT LEV= ',K,GR1(1)                           
        DO LAT=LAT1,LAT2                                                
          LAN=LAT-LAT1+1                                                
          JLAT=(LAT-1)*IGRD1                                            
          DO I=1,IGRD1                                                  
            F(I,KQ,LAN)=GR1(I+JLAT)                                     
            F(I,KK,LAN)=GR1(I+JLAT)                                     
          ENDDO                                                         
        ENDDO                                                           
 30   CONTINUE                                                          
!ccj
      IF(NTRAC.GT.0) THEN  ! read ozone
       DO K=1,LEVS
         KOZ=KSOZ-1+K
         READ(NSIG) (GR1_s(I),I=1,NWF)

         do I=1,NWF
          GR1(I)=GR1_s(I)
         enddo
      
        PRINT *,' READ  OZ  AT LEV= ',K,GR1(1)                           
        DO LAT=LAT1,LAT2                                                
          LAN=LAT-LAT1+1                                                
          JLAT=(LAT-1)*IGRD1                                            
          DO I=1,IGRD1                                                  
            F(I,KOZ,LAN)=GR1(I+JLAT)                                     
          ENDDO
        ENDDO
       ENDDO 
      ENDIF
      IF(NCLD.GT.0) THEN  ! read cloud water
       DO K=1,LEVS
         KCL=KSCL-1+K
         READ(NSIG) (GR1_s(I),I=1,NWF)

         do I=1,NWF
          GR1(I)=GR1_s(I)
         enddo
      
        PRINT *,' READ  CW  AT LEV= ',K,GR1(1)                           
        DO LAT=LAT1,LAT2 
          LAN=LAT-LAT1+1                                                
          JLAT=(LAT-1)*IGRD1                                            
          DO I=1,IGRD1                                                  
            F(I,KCL,LAN)=GR1(I+JLAT)                                     
          ENDDO
        ENDDO
       ENDDO
      ENDIF
!cc
      IF( NCLD .GE. 2 ) THEN
        DO K=1,LEVS
          KQC=KSQC-1+K
          KQR=KSQR-1+K
          KKK=KSWN-1+K                  ! borrow PW for cloud waters
          READ(NSIG) (GR1(I),I=1,NWF)
          PRINT *,' READ  AT LEV= ',K,GR1(1)                           
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
        IF( NCLD .EQ. 4 ) THEN
          DO K=1,LEVS
            KQC=KSQC-1+K
            KKK=KSWN-1+K                ! borrow PW for cloud waters
            READ(NSIG) (GR1(I),I=1,NWF)
            PRINT *,' READ  AT LEV= ',K,GR1(1)
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQC,LAN)=F(I,KQC,LAN)+GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
          DO K=1,LEVS
            KQR=KSQR-1+K
            KKK=KSWN-1+K                ! borrow PW for cloud waters
            READ(NSIG) (GR1(I),I=1,NWF)
            PRINT *,' READ  AT LEV= ',K,GR1(1)                           
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
        IF( NCLD .EQ. 5 ) THEN
          DO K=1,LEVS
            KQC=KSQC-1+K
            KKK=KSWN-1+K                ! borrow PW for cloud waters
            READ(NSIG) (GR1(I),I=1,NWF)
            PRINT *,' READ  AT LEV= ',K,GR1(1)                           
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQC,LAN)=F(I,KQC,LAN)+GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
          DO K=1,LEVS
            KQR=KSQR-1+K
            KKK=KSWN-1+K                ! borrow PW for cloud waters
            READ(NSIG) (GR1(I),I=1,NWF)
            PRINT *,' READ  AT LEV= ',K,GR1(1)                           
            DO LAT=LAT1,LAT2
              LAN=LAT-LAT1+1
              JLAT=(LAT-1)*IGRD1
              DO I=1,IGRD1
                F(I,KQR,LAN)=GR1(I+JLAT)
                F(I,KKK,LAN)=F(I,KKK,LAN)+GR1(I+JLAT)
              ENDDO
            ENDDO
          ENDDO
          DO K=1,LEVS
            KQR=KSQR-1+K
            KKK=KSWN-1+K                ! borrow PW for cloud waters
            READ(NSIG) (GR1(I),I=1,NWF)
            PRINT *,' READ  AT LEV= ',K,GR1(1)                           
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
! factor to fully modify the virtual temperature
      DO K=1,LEVS
        KQ=KSQ-1+K
        KK=KSWN-1+K             ! borrow PN for cloud waters
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KK,LAN)=1.+FVIRT*F(I,KQ,LAN)
!
!           F(I,KK,LAN)=(1.+FVIRT*F(I,KQ,LAN))*
!    &                  (1.+F(I,KQ,LAN))/(1.+F(I,KK,LAN))
          ENDDO
        ENDDO
      ENDDO
!
      IF( NONHYD .EQ. 1 ) THEN
! PN
      DO K=1,LEVS
        KPN=KSPN-1+K
        READ(NSIG) (GR1(I),I=1,NWF)
        PRINT *,' READ PN AT LEV= ',K,GR1(1)                           
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
! TN
      DO K=1,LEVS
        KTN=KSTN-1+K
        KK=KSWN-1+K             ! borrow PW for cloud waters
        READ(NSIG) (GR1(I),I=1,NWF)
        PRINT *,' READ TN AT LEV= ',K,GR1(1)                           
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KTN,LAN)=GR1(I+JLAT)/F(I,KK,LAN)
          ENDDO
        ENDDO
      ENDDO
! WN
      DO K=1,LEVS+1
        KWN=KSWN-1+K
        READ(NSIG) (GR1(I),I=1,NWF)
        PRINT *,' READ WN AT LEV= ',K,GR1(1)                           
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KWN,LAN)=GR1(I+JLAT)
          ENDDO
        ENDDO
      ENDDO
!
      ELSE
!
! hydro P
      DO K=1,LEVS
        KPN=KSPN-1+K
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KPN,LAN)=F(I,KSPS,LAN)*SL(K)
          ENDDO
        ENDDO
      ENDDO
! hydro T
      DO K=1,LEVS
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
! zero W
      DO K=1,LEVS+1
        KWN=KSWN-1+K
        DO LAT=LAT1,LAT2
          LAN=LAT-LAT1+1
          JLAT=(LAT-1)*IGRD1
          DO I=1,IGRD1
            F(I,KWN,LAN)=0.0
          ENDDO
        ENDDO
      ENDDO
!
      ENDIF
!cc
! M**2                                                                  
      READ(NSIG) (GR1_s(I),I=1,NWF)                                       
       do I=1,NWF
        GR1(I)=GR1_s(I)
       enddo
      
      PRINT *,' READ  XM2 ',GR1(1)                                      
      DO I=1,NWF
        XMAP(I)=SQRT(GR1(I))
      ENDDO
      DO LAT=LAT1,LAT2                                                  
        PRINT *,' DO XM FOR U V AT LAT=',LAT                            
        LAN=LAT-LAT1+1                                                  
        JLAT=(LAT-1)*IGRD1                                              
        DO K=1,LEVS                                                     
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
        PRINT *,' DO XM FOR PSX PSY ZSX ZSY DIV VOR AT LAT=',LAT        
        LAN=LAT-LAT1+1                                                  
        JLAT=(LAT-1)*IGRD1                                              
        DO I=2,IGRD1-1                                                  
          XM = SQRT( GR1(I+JLAT) )                                      
          F(I,KSPSX,LAN)=F(I,KSPSX,LAN)*XM                              
          F(I,KSPSY,LAN)=F(I,KSPSY,LAN)*XM                              
          F(I,KSZSX,LAN)=F(I,KSZSX,LAN)*XM                              
          F(I,KSZSY,LAN)=F(I,KSZSY,LAN)*XM                              
        ENDDO                                                           
        DO K=1,LEVS                                                     
          KD=KSD-1+K                                                    
          KZ=KSZ-1+K                                                    
          DO I=2,IGRD1-1                                                
            F(I,KD,LAN)=F(I,KD,LAN)*GR1(I+JLAT)                         
            F(I,KZ,LAN)=F(I,KZ,LAN)*GR1(I+JLAT)                         
          ENDDO                                                         
        ENDDO                                                           
      ENDDO                                                             
! MODIFY  DIV AND VOR WITH MAP FACTOR DERIVATIVE
      READ(NSIG) (GR2_s(I),I=1,NWF)                                       
      READ(NSIG) (GR3_s(I),I=1,NWF)                                       
       do I=1,NWF
        GR2(I)=GR2_s(I)
        GR3(I)=GR3_s(I)
       enddo
      
      DO LAT=LAT1X,LAT2X
        LAN=LAT-LAT1+1
        JLAT=(LAT-1)*IGRD1
        DO K=1,LEVS
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
        PRINT *,' DO MODIFY THE LATERAL BOUNDARY AT LAT=',LAT           
        LAN=LAT-LAT1+1                                                  
        F(1,KSPSX,LAN)=F(2,KSPSX,LAN)                                   
        F(1,KSPSY,LAN)=F(2,KSPSY,LAN)                                   
        F(1,KSZSX,LAN)=F(2,KSZSX,LAN)                                   
        F(1,KSZSY,LAN)=F(2,KSZSY,LAN)                                   
        F(IGRD1,KSPSX,LAN)=F(IGRD1-1,KSPSX,LAN)                         
        F(IGRD1,KSPSY,LAN)=F(IGRD1-1,KSPSY,LAN)                         
        F(IGRD1,KSZSX,LAN)=F(IGRD1-1,KSZSX,LAN)                         
        F(IGRD1,KSZSY,LAN)=F(IGRD1-1,KSZSY,LAN)                         
        DO K=1,LEVS                                                     
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
        DO K=1,LEVS                                                     
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
        DO K=1,LEVS                                                     
          KD=KSD-1+K                                                    
          KZ=KSZ-1+K                                                    
          DO I=1,IGRD1                                                  
            F(I,KD,LANLAST)=F(I,KD,LANLAST-1)                           
            F(I,KZ,LANLAST)=F(I,KZ,LANLAST-1)                           
          ENDDO                                                         
        ENDDO                                                           
      ENDIF                                                             
! READ LATITUDE
      READ(NSIG) (GR1_s(I),I=1,NWF)                                       
       do I=1,NWF
        GR1(I)=GR1_s(I)
       enddo
      
      DO IJ=1,NWF                                                       
        CLAT(IJ)=GR1(IJ)                                                
      ENDDO                                                             
      RLAT1=GR1(1)                                                      
      RLAT2=GR1(NWF)                                                    
      READ(NSIG) (GR1_s(I),I=1,NWF)                                       
       do I=1,NWF
        GR1(I)=GR1_s(I)
       enddo
      
      RLON1=GR1(1)                                                      
      RLON2=GR1(NWF)                                                    
      PRINT *,'RLAT1 RLAT2 RLON1 RLON2',RLAT1,RLAT2,RLON1,RLON2
      PRINT *,' END OF READ WITHIN ',LAT1,LAT2                          
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
      SUBROUTINE SHALF(A,IM,JM)                                         
      DIMENSION A(IM,JM),B(IM)                                          
      PRINT *,' SHALF CALLED '                                          
      DO J=JM-2,2,-2                                                    
        DO I=1,IM                                                       
          B(I)=A(I,J)                                                   
        ENDDO                                                           
        DO JJ=J,JM-1                                                    
          DO I=1,IM                                                     
            A(I,JJ)=A(I,JJ+1)                                           
          ENDDO                                                         
        ENDDO                                                           
        DO I=1,IM                                                       
          A(I,JM)=B(I)                                                  
        ENDDO                                                           
      ENDDO                                                             
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE SIG2MW(IM,IX,KM,P,U,V,T,Z,PMW,UMW,VMW,TMW,ZMW)           
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SIG2MW      SIGMA TO MAXWIND INTERPOLATION            
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: LOCATES THE MAXIMUM WIND SPEED LEVEL (MAXWIND LEVEL) AND    
!   RETURNS THE WIND SPEED, COMPONENTS AND PRESSURE AT THAT LEVEL.      
!   THE MAXWIND LEVEL IS RESTRICTED TO BE BETWEEN 50KPA AND 7KPA.       
!   THE MAXWIND LEVEL IS IDENTIFIED BY CUBIC SPLINE INTERPOLATION       
!   OF THE WIND SPEEDS IN LOG PRESSURE.                                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!   94-08-01  IREDELL   RETURN MAXWIND TEMPERATURE TOO                  
!   97-08-01  JUANG     FOR USE WITH HYDROSTATIC AND NONHYDROSTATIC
!                                                                       
! USAGE:    CALL SIG2MW(IM,IX,KM,P,U,V,T,Z,PMW,UMW,VMW,TMW,ZMW)           
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF SIGMA LEVELS                         
!     P        - REAL (IX,IM) PRESSURE IN KPA
!     U        - REAL (IX,KM) ZONAL WIND IN M/S                         
!     V        - REAL (IX,KM) MERID WIND IN M/S                         
!     T        - REAL (IX,KM) TEMPERATURE IN K                          
!     Z        - REAL (IX,KM) HEIGHTS IN M
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     PMW      - REAL (IM) MAXWIND PRESSURE IN KPA                      
!     UMW      - REAL (IM) MAXWIND ZONAL WIND IN M/S                    
!     VMW      - REAL (IM) MAXWIND MERID WIND IN M/S                    
!     TMW      - REAL (IM) MAXWIND TEMPERATURE IN K                     
!     ZMW      - REAL (IM) MAXWIND HEIGHT IN M
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   SPCOEF       COMPUTE SECOND DERIVATIVES FOR CUBIC SPLINE            
!   SPFMAX       DETERMINE MAXIMUM VALUE OF CUBIC SPLINE                
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION P(IX,KM)                                           
      DIMENSION U(IX,KM),V(IX,KM),T(IX,KM),Z(IX,KM)                              
      DIMENSION PMW(IM),UMW(IM),VMW(IM),TMW(IM),ZMW(IM)                 
      DIMENSION SPDMW(IM),SMW(IM),S(IM,KM),SPD(IM,KM),D2SPD(IM,KM)         
      PARAMETER(PMWBOT=500.E-1,PMWTOP=70.E-1)                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  FIX VERTICAL COORDINATE PROPORTIONAL TO LOG PRESSURE                 
!  AND CALCULATE WIND SPEEDS BETWEEN PMWBOT AND PMWTOP                  
      DO K=1,KM                                                         
        DO I=1,IM                                                       
          S(I,K)=-LOG(P(I,K))                                           
          IF(P(I,K).LE.PMWBOT.AND.P(I,K).GE.PMWTOP) THEN                          
            SPD(I,K)=SQRT(U(I,K)**2+V(I,K)**2)                          
          ELSE                                                          
            SPD(I,K)=0.                                                 
          ENDIF                                                         
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  USE SPLINE ROUTINES TO DETERMINE MAXWIND LEVEL AND WIND SPEED        
      CALL SPCOEF(IM,KM,S,SPD,D2SPD)                                    
      CALL SPFMAX(IM,KM,S,SPD,D2SPD,SMW,PMW,SPDMW)                      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COMPUTE MAXWIND PRESSURE AND WIND COMPONENTS                         
      DO I=1,IM                                                         
        PMW(I)=EXP(-PMW(I))
        K=INT(SMW(I))                                                   
        IF(FLOAT(K).EQ.SMW(I)) THEN                                     
          UB=U(I,K)                                                     
          VB=V(I,K)                                                     
          TB=T(I,K)                                                     
          ZB=Z(I,K)
        ELSE                                                            
          UB=(K+1-SMW(I))*U(I,K)+(SMW(I)-K)*U(I,K+1)                    
          VB=(K+1-SMW(I))*V(I,K)+(SMW(I)-K)*V(I,K+1)                    
          TB=(K+1-SMW(I))*T(I,K)+(SMW(I)-K)*T(I,K+1)                    
          ZB=(K+1-SMW(I))*Z(I,K)+(SMW(I)-K)*Z(I,K+1)                    
        ENDIF                                                           
        SPDB=SQRT(UB**2+VB**2)                                          
        UMW(I)=UB*SPDMW(I)/SPDB                                         
        VMW(I)=VB*SPDMW(I)/SPDB                                         
        TMW(I)=TB                                                       
        ZMW(I)=ZB
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE SIG2P(IM,IX,KM,SI,SL,PS,PP,                               &
     &                 US,VS,OS,WS,ZS,ZI,TS,RS,QS,QCS,QRS,CLS,OZS,         &
     &                 KO,PO,UP,VP,OP,WP,ZP,TP,RP,QP,QCP,QRP,CLP,OZP)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SIG2P       SIGMA TO PRESSURE INTERPOLATION           
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: INTERPOLATES WINDS, OMEGA, HEIGHT, TEMPERATURE AND HUMIDITY 
!   FROM THE SIGMA COORDINATE SYSTEM TO THE MANDATORY PRESSURE LEVELS.  
!   ASSUMES THAT RELATIVE HUMIDITY, TEMPERATURE, GEOPOTENTIAL HEIGHTS,  
!   WIND COMPONENTS AND VERTICAL VELOCITY VARY LINEARLY IN THE VERTICAL 
!   WITH THE LOG OF PRESSURE.  UNDERGROUND HEIGHTS ARE OBTAINED USING   
!   THE SHUELL METHOD AND UNDERGROUND TEMPERATURES ARE OBTAINED USING   
!   A CONSTANT MOIST ADIABATIC LAPSE RATE.  HEIGHTS ABOVE THE TOP SIGMA 
!   LEVEL ARE INTEGRATED HYDROSTATICALLY.  OTHERWISE FIELDS ARE HELD    
!   CONSTANT OUTSIDE THE SIGMA STRUCTURE AND NO EXTRAPOLATION IS DONE.  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  SELA,NEWELL,GERRITY,BALLISH,DEAVEN,IREDELL                
!   97-10-31  JUANG FOR HUDRO AND NONHYDRO
!                                                                       
! USAGE:    CALL SIG2P(IM,IX,KM,SI,SL,PS,PP,                            
!    &                 US,VS,OS,WS,ZS,ZI,TS,RS,QS,QCS,QRS,CLS,OZS,      
!    &                 KO,PO,UP,VP,OP,WP,ZP,TP,RP,QCP,QRP,CLP,OZP)                         
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF SIGMA LEVELS                         
!     SI       - REAL (KM+1) SIGMA INTERFACE VALUES                     
!     SL       - REAL (KM) SIGMA VALUES                                 
!     PS       - REAL (IM) SURFACE PRESSURE IN KPA                      
!     PP       - REAL (IX,KM) RESSURE IN KPA
!     US       - REAL (IX,KM) ZONAL WIND IN M/S                         
!     VS       - REAL (IX,KM) MERID WIND IN M/S                         
!     OS       - REAL (IX,KM) VERTICAL VELOCITY IN PA/S                 
!     ZS       - REAL (IX,KM) HEIGHTS ON THE FULL LEVELS IN M           
!     ZI       - REAL (IX,KM) HEIGHTS ON THE INTERFACES IN M            
!     TS       - REAL (IX,KM) TEMPERATURE IN K                          
!     RS       - REAL (IX,KM) RELATIVE HUMIDITY IN PERCENT              
!     QS       - REAL (IX,KM) SPECIFIC HUMIDITY IN KG/KG                
!     CLS      - REAL (IX,KM) CLOUD WATER IN KG/KG
!     OZS      - REAL (IX,KM) OZONE IN KG/KG
!     TRS      - REAL (IX,KM,NT) TRACERS IN KG/KG
!     KO       - INTEGER NUMBER OF PRESSURE LEVELS                      
!     PO       - REAL (KO) MANDATORY PRESSURES IN KPA                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     UP       - REAL (IX,KO) ZONAL WIND IN M/S                         
!     VP       - REAL (IX,KO) MERID WIND IN M/S                         
!     OP       - REAL (IX,KO) VERTICAL VELOCITY IN PA/S                 
!     ZP       - REAL (IX,KO) HEIGHTS IN M                              
!     TP       - REAL (IX,KO) TEMPERATURE IN K                          
!     RP       - REAL (IX,KO) RELATIVE HUMIDITY IN PERCENT              
!     QP       - REAL (IX,KO)  SPECIFIC HUMIDITY IN KG/KG
!     CLP      - REAL (IX,KO) CLOUD WATER IN KG/KG
!     OZP      - REAL (IX,KO) OZONE IN KG/KG
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   ISRCHFLT - FIND FIRST VALUE IN AN ARRAY LESS THAN TARGET VALUE      
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION SI(KM+1),SL(KM),PS(IM),PP(IX,KM) 
      DIMENSION US(IX,KM),VS(IX,KM),OS(IX,KM),WS(IX,KM) 
      DIMENSION OZS(IX,KM),CLS(IX,KM)
      DIMENSION ZS(IX,KM),ZI(IX,KM),TS(IX,KM),RS(IX,KM)
      DIMENSION QS(IX,KM)  
      DIMENSION QCS(IX,KM),QRS(IX,KM)
      DIMENSION PO(KO)          
      DIMENSION UP(IX,KO),VP(IX,KO),OP(IX,KO),WP(IX,KO)
      DIMENSION OZP(IX,KO),CLP(IX,KO)
      DIMENSION ZP(IX,KO),TP(IX,KO),RP(IX,KO),QP(IX,KO)  
      DIMENSION QCP(IX,KO),QRP(IX,KO)
      DIMENSION ASI(2:KM),ASL(KM),APO(KO),APS(IM)    
      DIMENSION PPMAX(KM),PPMIN(KM)
      PARAMETER(G= 9.8000E+0 ,RD= 2.8705E+2 ,RV= 4.6150E+2 )  
      PARAMETER(ROG=RD/G,FVIRT=RV/RD-1.)                     
      PARAMETER(GAMMAM=-6.5E-3,ZSHUL=75.,TVSHUL=290.66)    
!yj      
      LOGICAL LCLASS(IX)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COMPUTE LOG PRESSURES FOR INTERPOLATION                              
      ASI=LOG(SI(2:KM))                                                 
      ASL=LOG(SL)                                                       
      APO=LOG(PO)                                                       
      APS=LOG(PS)                                                       
!yj      APSMIN=APS(ISMIN(IM,APS,1))                                       
!yj      APSMAX=APS(ISMAX(IM,APS,1))                                       
      DO K=1,KM
        PPMIN(K)=PP(ISMIN(IM,PP(:,K),1),K)
        PPMAX(K)=PP(ISMAX(IM,PP(:,K),1),K)
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  DETERMINE SIGMA LAYERS BRACKETING PRESSURE LAYER.                    
!  WITHIN SIGMA STRUCTURE, INTERPOLATE FIELDS LINEARLY IN LOG PRESSURE  
!  BETWEEN BRACKETING FULL SIGMA LAYERS EXCEPT HEIGHTS ARE INTERPOLATED 
!  BETWEEN THE NEAREST FULL SIGMA LAYER AND THE NEAREST SIGMA INTERFACE 
      KD1=1                                                             
      DO K=1,KO                                                         
!yj      
        LCLASS=.FALSE.
        IF(KM-KD1-1.GT.0)                                                &
     &  KD1=KD1+ISRCHFLT(KM-KD1-1,PPMIN(KD1+1),1,PO(K))-1         
        KD2=KD1
        IF(KM-KD2-1.GT.0)                                                &
     &  KD2=KD2+ISRCHFLT(KM-KD2-1,PPMAX(KD1+1),1,PO(K))        
!yj     &  KD2=KD2+ISRCHFLT(KM-KD2-1,PPMAX(KD1+1),1,PO(K))-1         
        DO KD=KD1,KD2                                                   
          KU=KD+1                                                       
          DO I=1,IM                                                     
            ASLKD=LOG(PP(I,KD))
            ASLKU=LOG(PP(I,KU))
            ASK=APO(K)
            IF(ASK.LE.ASLKD.AND.ASK.GT.ASLKU) THEN      
              WU=(ASLKD-ASK)/(ASLKD-ASLKU)                      
              WD=1.-WU                                                  
              UP(I,K)=WU*US(I,KU)+WD*US(I,KD)                           
              VP(I,K)=WU*VS(I,KU)+WD*VS(I,KD)                           
              OP(I,K)=WU*OS(I,KU)+WD*OS(I,KD)                           
              WP(I,K)=WU*WS(I,KU)+WD*WS(I,KD)
!              ZP(I,K)=WU*ZS(I,KU)+WD*ZS(I,KD)
              OZP(I,K)=WU*OZS(I,KU)+WD*OZS(I,KD) 
              CLP(I,K)=WU*CLS(I,KU)+WD*CLS(I,KD) 
              TP(I,K)=WU*TS(I,KU)+WD*TS(I,KD)                           
              RP(I,K)=WU*RS(I,KU)+WD*RS(I,KD)                           
              QP(I,K)=WU*QS(I,KU)+WD*QS(I,KD)                           
              QCP(I,K)=WU*QCS(I,KU)+WD*QCS(I,KD)
              QRP(I,K)=WU*QRS(I,KU)+WD*QRS(I,KD)
              KI=KD+1
              DI=ASI(KI)-(ASK-APS(I))
              KL=NINT(KI-0.5+SIGN(0.5,DI))
              WL=DI/(ASI(KI)-ASL(KL))
              WI=1.-WL
              ZP(I,K)=WI*ZI(I,KI)+WL*ZS(I,KL)
              KD2=MIN(KD2,KD)
!yj              
              LCLASS(i)=.TRUE.
            ENDIF                                                       
          ENDDO                                                         
        ENDDO                                                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  INTERPOLATE SIGMA TO PRESSURE OUTSIDE THE MODEL DOMAIN               
        DO I=1,IM                                                       
!yj          ASK=APO(K)-LOG(PP(I,K))+ASL(K)
          ASK=APO(K)
!  BELOW GROUND USE SHUELL METHOD TO OBTAIN HEIGHT, CONSTANT LAPSE RATE 
!  TO OBTAIN TEMPERATURE, AND HOLD OTHER FIELDS CONSTANT                
!yj          IF(ASK.GT.0.) THEN                                            
          IF(ASK.GT.APS(I)) THEN 
      lclass(I)=.true.
            UP(I,K)=US(I,1)                                             
            VP(I,K)=VS(I,1)                                             
            OP(I,K)=OS(I,1)                                             
            WP(I,K)=WS(I,1)
            TVSF=TS(I,1)*(1.+FVIRT*QS(I,1))-GAMMAM*(ZS(I,1)-ZI(I,1))    
            IF(ZI(I,1).GT.ZSHUL) THEN                                   
              TVSL=TVSF-GAMMAM*ZI(I,1)                                  
              IF(TVSL.GT.TVSHUL) THEN                                   
                IF(TVSF.GT.TVSHUL) THEN                                 
                  TVSL=TVSHUL-5.E-3*(TVSF-TVSHUL)**2                    
                ELSE                                                    
                  TVSL=TVSHUL                                           
                ENDIF                                                   
              ENDIF                                                     
              GAMMAS=(TVSF-TVSL)/ZI(I,1)                                
            ELSE                                                        
              GAMMAS=0.                                                 
            ENDIF                                                       
!yj            PART=ROG*ASK                                                
            PART=ROG*(ASK-APS(I))      
            ZP(I,K)=ZI(I,1)-TVSF*PART/(1.+0.5*GAMMAS*PART)              
            TP(I,K)=TS(I,1)+GAMMAM*(ZP(I,K)-ZS(I,1))                    
            RP(I,K)=RS(I,1)                                             
            QP(I,K)=QS(I,1)                                             
            QCP(I,K)=QCS(I,1)
            QRP(I,K)=QRS(I,1)
            OZP(I,K)=OZS(I,1)
            CLP(I,K)=CLS(I,1)
!  BETWEEN BOTTOM SIGMA AND GROUND INTERPOLATE HEIGHT,                  
!  EXTRAPOLATE TEMPERATURE AND HOLD OTHER FIELDS CONSTANT               
!yj          ELSEIF(ASK.GE.ASL(1)) THEN                                 
          ELSEIF(ASK.GT.LOG(PP(I,1))) THEN                                 
!yj          
            lclass(I)=.true.
            UP(I,K)=US(I,1)                                             
            VP(I,K)=VS(I,1)                                             
            OP(I,K)=OS(I,1)                                             
            WP(I,K)=WS(I,1)
!yj            WL=ASK/ASL(1)                                               
            WL=(ASK-APS(I))/ASL(1)    
            WI=1.-WL                                                    
            ZP(I,K)=WI*ZI(I,1)+WL*ZS(I,1)                               
!yj            WU=(ASL(1)-ASK)/(ASL(1)-ASL(2))                             
            WU=(ASL(1)-(ASK-APS(I)))/(ASL(1)-ASL(2))  
            WD=1.-WU                                                    
            TP(I,K)=WU*TS(I,2)+WD*TS(I,1)                               
            RP(I,K)=RS(I,1)                                             
            QP(I,K)=QS(I,1)                                             
            QCP(I,K)=QCS(I,1)
            QRP(I,K)=QRS(I,1)
            OZP(I,K)=OZS(I,1)
            CLP(I,K)=CLS(I,1)
!  ABOVE TOP SIGMA INTEGRATE HEIGHT HYDROSTATICALLY                     
!  AND HOLD OTHER FIELDS CONSTANT                                       
!yj          ELSEIF(ASK.LE.ASL(KM)) THEN                                   
          ELSEIF(ASK.LE.LOG(PP(I,KM))) THEN                                   
!yj          
            LCLASS(I)=.TRUE.
            UP(I,K)=US(I,KM)                                            
            VP(I,K)=VS(I,KM)                                            
            OP(I,K)=OS(I,KM)                                            
            WP(I,K)=WS(I,KM)
            TVKM=TS(I,KM)*(1.+FVIRT*QS(I,KM))                           
            ZP(I,K)=ZS(I,KM)+ROG*TVKM*(ASL(KM)-(ASK-APS(I)))
            TP(I,K)=TS(I,KM)                                            
            RP(I,K)=RS(I,KM)                                            
            QP(I,K)=QS(I,KM)                                            
            QCP(I,K)=QCS(I,KM)
            QRP(I,K)=QRS(I,KM)
            OZP(I,K)=OZS(I,KM)
            CLP(I,K)=CLS(I,KM)
          ENDIF                                                         
!yj >          
          IF (.NOT.LCLASS(I)) THEN
            print*,'not sort out in SIG2P (I,K)=',I,K
            print*,'PO(K)=',PO(K)
            print*,'PP(I,:)=',PP(I,:)
            print*,'break the program at SIG2P'
            STOP
          ENDIF
!yj <          

        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE SIG2PT(IM,IX,KM,SI,SL,                                   &
     &                  PS,US,VS,TS,QS,QSS,                               &
     &                  KT,PT,UPT,VPT,TPT,QPT,RPT)                      
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SIG2PT      SIGMA TO PRESSURE THICKNESS               
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: INTERPOLATES WINDS, TEMPERATURE AND HUMIDITY                
!   FROM THE SIGMA COORDINATE SYSTEM TO CONSTANT PRESSURE THICKNESSES   
!   ABOVE THE GROUND.                                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   94-07-08  IREDELL                                                   
!                                                                       
! USAGE:    CALL SIG2PT(IM,IX,KM,SI,SL,                                 
!    &                  PS,US,VS,TS,QS,QSS,                             
!    &                  KT,PT,UPT,VPT,TPT,QPT,RPT)                      
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF SIGMA LEVELS                         
!     SI       - REAL (KM+1) SIGMA INTERFACE VALUES                     
!     SL       - REAL (KM) SIGMA VALUES                                 
!     PS       - REAL (IM) SURFACE PRESSURE IN KPA                      
!     US       - REAL (IX,KM) ZONAL WIND IN M/S                         
!     VS       - REAL (IX,KM) MERID WIND IN M/S                         
!     TS       - REAL (IX,KM) TEMPERATURE IN K                          
!     QS       - REAL (IX,KM) SPECIFIC HUMIDITY IN KG/KG                
!     QSS      - REAL (IX,KM) SATURATED SPECIFIC HUMIDITY IN KG/KG      
!     KT       - INTEGER NUMBER OF PRESSURE THICKNESS LAYERS            
!     PT       - REAL PRESSURE THICKNESS IN KPA                         
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     UPT      - REAL (IX,KT) ZONAL WIND IN M/S                         
!     VPT      - REAL (IX,KT) MERID WIND IN M/S                         
!     TPT      - REAL (IX,KT) TEMPERATURE IN K                          
!     QPT      - REAL (IX,KT) SPECIFIC HUMIDITY IN KG/KG                
!     RPT      - REAL (IX,KT) RELATIVE HUMIDITY IN PERCENT              
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION SI(KM+1),SL(KM),PS(IM)                                  
      DIMENSION US(IX,KM),VS(IX,KM),TS(IX,KM),QS(IX,KM),QSS(IX,KM)      
      DIMENSION UPT(IX,KT),VPT(IX,KT),TPT(IX,KT),QPT(IX,KT),RPT(IX,KT)  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RMIN=PS(ISMIN(IM,PS,1))/PT                                        
      RMAX=PS(ISMAX(IM,PS,1))/PT                                        
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      DO K=1,KT                                                         
        DO I=1,IM                                                       
          UPT(I,K)=0.                                                   
          VPT(I,K)=0.                                                   
          TPT(I,K)=0.                                                   
          QPT(I,K)=0.                                                   
          RPT(I,K)=0.                                                   
        ENDDO                                                           
        DO KS=1,KM                                                      
          IF(K-(1-SI(KS))*RMIN.GT.0..AND.K-(1-SI(KS+1))*RMAX.LT.1.) THEN
            DO I=1,IM                                                   
              R=PS(I)/PT                                                
              FKS=MIN(K-(1-SI(KS))*R,1.)-MAX(K-(1-SI(KS+1))*R,0.)       
              IF(FKS.GT.0.) THEN                                        
                UPT(I,K)=UPT(I,K)+FKS*US(I,KS)                          
                VPT(I,K)=VPT(I,K)+FKS*VS(I,KS)                          
                TPT(I,K)=TPT(I,K)+FKS*TS(I,KS)                          
                QPT(I,K)=QPT(I,K)+FKS*QS(I,KS)                          
                RPT(I,K)=RPT(I,K)+FKS*QSS(I,KS)                         
              ENDIF                                                     
            ENDDO                                                       
          ENDIF                                                         
        ENDDO                                                           
        DO I=1,IM                                                       
          RPT(I,K)=MIN(MAX(QPT(I,K)/RPT(I,K),0.),1.)*100.               
        ENDDO                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE SIG2TP(IM,IX,KM,CLAT,SI,SL,                              &
     &                  P,U,V,T,ZL,ZI,                                    &
     &                  PTP,UTP,VTP,TTP,ZTP,SHTP)                           
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SIG2TP      SIGMA TO TROPOPAUSE INTERPOLATION         
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: LOCATES THE TROPOPAUSE PRESSURE LEVEL AND INTERPOLATES      
!   THE WINDS AND TEMPERATURE AND WIND SHEAR TO THE TROPOPAUSE.         
!   THE TROPOPAUSE IS IDENTIFIED BY THE LOWEST LEVEL ABOVE 450 MB       
!   AT THE POLES TO 350 MB AT THE EQUATOR                               
!   WHERE THE TEMPERATURE LAPSE RATE -DT/DZ BECOMES LESS THAN 2 K/KM.   
!   THE TROPOPAUSE IS NOT ALLOWED HIGHER THAN 85 MB.                    
!   INTERPOLATIONS ARE DONE LINEARLY IN LOG OF PRESSURE.                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  MCCALLA,IREDELL                                           
!   94-06-27  IREDELL            LOWEST TROPOPAUSE DEPENDENT ON LATITUDE
!                                                                       
! USAGE:    CALL SIG2TP(IM,IX,KM,CLAT,SI,SL                          
!    &                  P,U,V,T,ZL,ZI,                                       
!    &                  PTP,UTP,VTP,TTP,ZTP,SHTP)                           
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF SIGMA LEVELS                         
!     CLAT     - REAL (IM) LATITUDE IN RADIANCE                         
!     SI       - REAL (KM) SIGMA INTERFACES
!     SL       - REAL (KM) SIGMA VALUES                                 
!     P        - REAL (IM) SURFACE PRESSURE IN KPA                      
!     U        - REAL (IX,KM) ZONAL WIND IN M/S                         
!     V        - REAL (IX,KM) MERID WIND IN M/S                         
!     T        - REAL (IX,KM) TEMPERATURE IN K                          
!     ZL       - REAL (IX,KM) HEIGHTS ON THE FULL LEVELS IN M
!     ZI       - REAL (IX,KM) HEIGHTS ON THE INTERFACES IN M
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     PTP      - REAL (IM) TROPOPAUSE PRESSURE IN KPA                   
!     UTP      - REAL (IM) TROPOPAUSE ZONAL WIND IN M/S                 
!     VTP      - REAL (IM) TROPOPAUSE MERID WIND IN M/S                 
!     TTP      - REAL (IM) TROPOPAUSE TEMPERATURE IN K                  
!     ZTP      - REAL (IM) TROPOPAUSE HEIGHT IN K
!     SHTP     - REAL (IM) TROPOPAUSE WIND SPEED SHEAR IN (M/S)/M       
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   ISRCHEQ  - FIND FIRST VALUE IN AN ARRAY EQUAL TO TARGET VALUE       
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      REAL SI(KM+1),SL(KM),P(IX,KM),CLAT(IM)                                       
      REAL U(IX,KM),V(IX,KM),T(IX,KM),ZL(IX,KM),ZI(IX,KM)               
      REAL PTP(IM),UTP(IM),VTP(IM),TTP(IM),ZTP(IM),SHTP(IM)                     
      REAL ASI(KM),ASL(KM)                                                      
      LOGICAL LEFT(IM)                                                  
      PARAMETER(G= 9.8000E+0 ,RD= 2.8705E+2 )                           
      PARAMETER(ROG=RD/G)                                               
      PARAMETER(PTBOTP=450.E-1,PTBOTE=350.E-1,PTTOP=85.E-1,GAMT=2.E-3)  
      FGAMMA(K)=(T(I,K-1)-T(I,K+1))/(ROG*T(I,K)*(ASL(K-1)-ASL(K+1)))    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  IDENTIFY TROPOPAUSE AS FIRST LAYER ABOVE PTBOT BUT BELOW PTTOP       
!  WHERE THE TEMPERATURE LAPSE RATE DROPS BELOW GAMT.                   
!  INTERPOLATE TROPOPAUSE PRESSURE, TEMPERATURE, WINDS AND WIND SHEAR   
      ASI=LOG(SI(1:KM))                                                       
      ASL=LOG(SL)                                                       
      LEFT=.TRUE.                                                       
      I1=1                                                              
      DO KU=3,KM-1                                                      
        IF(I1.LE.IM) THEN                                               
          DO I=I1,IM                                                    
            PTBOT=PTBOTE+(PTBOTP-PTBOTE)*CLAT(I)/ACOS(0.)               
            IF(LEFT(I)) THEN                                            
              PU=P(I,KU)
              IF(PU.LE.PTBOT) THEN                                      
                GAMU=FGAMMA(KU)                                         
                IF(PU.LE.PTTOP) GAMU=MIN(GAMU,GAMT)                     
                IF(GAMU.LE.GAMT) THEN                                   
                  KD=KU-1                                               
                  GAMD=FGAMMA(KD)                                       
                  GAMD=MAX(GAMD,GAMT)                                   
                  WU=(GAMD-GAMT)/(GAMD-GAMU)                            
                  DLP=ASL(KU)-ASL(KD)                                   
                  PTP(I)=P(I,KD)*EXP(WU*DLP)                       
                  IF(PTP(I).GT.PTBOT) THEN                              
                    WU=WU+LOG(PTBOT/PTP(I))/DLP                         
                    PTP(I)=PTBOT                                        
                  ELSEIF(PTP(I).LT.PTTOP) THEN                          
                    WU=WU+LOG(PTTOP/PTP(I))/DLP                         
                    PTP(I)=PTTOP                                        
                  ENDIF                                                 
                  TTP(I)=T(I,KD)+WU*(T(I,KU)-T(I,KD))                   
                  KI=KD+1
                  DI=ASI(KI)-(ASL(KD)+WU*DLP)
                  KL=NINT(KI-0.5+SIGN(0.5,DI))
                  WL=DI/(ASI(KI)-ASL(KL))
                  ZTP(I)=ZI(I,KI)+WL*(ZL(I,KL)-ZI(I,KI))
                  UTP(I)=U(I,KD)+WU*(U(I,KU)-U(I,KD))                   
                  VTP(I)=V(I,KD)+WU*(V(I,KU)-V(I,KD))                   
                  SPDD=SQRT(U(I,KD)**2+V(I,KD)**2)                      
                  SPDU=SQRT(U(I,KU)**2+V(I,KU)**2)                      
                  SHTP(I)=(SPDU-SPDD)/(ROG*0.5*(T(I,KU)+T(I,KD))*DLP)   
                  LEFT(I)=.FALSE.                                       
                ENDIF                                                   
              ENDIF                                                     
            ENDIF                                                       
          ENDDO                                                         
          I1=I1+ISRCHEQ(IM-I1+1,LEFT(I1),1,.TRUE.)-1                    
        ENDIF                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
      SUBROUTINE SIG2Z(IM,IX,KM,ZS,US,VS,TS,KZZ,ZZ,UZZ,VZZ,TZZ)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
!  SUBPROGRAM:    SIG2Z       SIGMA TO HEIGHT INTERPOLATION
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
!
! ABSTRACT: INTERPOLATES WINDS
!   FROM THE SIGMA COORDINATE SYSTEM TO CONSTANT HEIGHT LEVELS.
!   ASSUMES THAT FIELDS VARY LINEARLY WITH HEIGHT.
!   FIELDS ARE HELD CONSTANT OUTSIDE THE SIGMA STRUCTURE.
!
! PROGRAM HISTORY LOG:
!   94-10-31  IREDELL
!
! USAGE:    CALL SIG2Z(IM,IX,KM,ZS,US,VS,TS,KZZ,ZZ,UZZ,VZZ,TZZ)
!
!   INPUT ARGUMENT LIST:
!     IM       - INTEGER NUMBER OF POINTS
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA
!     KM       - INTEGER NUMBER OF SIGMA LEVELS
!     ZS       - REAL (IX,KM) HEIGHTS ON THE FULL LEVELS IN M
!     US       - REAL (IX,KM) ZONAL WIND IN M/S
!     VS       - REAL (IX,KM) MERID WIND IN M/S
!     TS       - REAL (IX,KM) TEMPERATURE IN K
!     KZZ      - INTEGER NUMBER OF HEIGHT LEVELS
!     ZZ       - REAL (KZZ) CONSTANT HEIGHTS IN M
!
!   OUTPUT ARGUMENT LIST:
!     UZZ      - REAL (IX,KZZ) ZONAL WIND IN M/S
!     VZZ      - REAL (IX,KZZ) MERID WIND IN M/S
!     TZZ      - REAL (IX,KZZ) TEMPERATURE IN K
!
! SUBPROGRAMS CALLED:
!   ISRCHFGT - FIND FIRST VALUE IN AN ARRAY GREATER THAN TARGET VALUE
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      DIMENSION US(IX,KM),VS(IX,KM),TS(IX,KM),ZS(IX,KM)
      DIMENSION ZZ(KZZ)
      DIMENSION UZZ(IX,KZZ),VZZ(IX,KZZ),TZZ(IX,KZZ)
      DIMENSION ZSMIN(KM),ZSMAX(KM)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE BOUNDS FOR INTERPOLATION
      DO K=1,KM
        ZSMIN(K)=ZS(1,K)
        ZSMAX(K)=ZS(1,K)
        DO I=2,IM
          ZSMIN(K)=MIN(ZSMIN(K),ZS(I,K))
          ZSMAX(K)=MAX(ZSMAX(K),ZS(I,K))
        ENDDO
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DETERMINE SIGMA LAYERS BRACKETING HEIGHT LEVEL.
!  WITHIN SIGMA STRUCTURE, INTERPOLATE FIELDS LINEARLY IN HEIGHT
!  BETWEEN BRACKETING FULL SIGMA LAYERS.
      KD1=1
      DO K=1,KZZ
        IF(KM-1-KD1.GT.0)                                                 &
     &  KD1=KD1+ISRCHFGT(KM-1-KD1,ZSMAX(KD1+1),1,ZZ(K))-1
        KD2=KD1                                                           &
        IF(KM-1-KD2.GT.0)
     &  KD2=KD2+ISRCHFGT(KM-1-KD2,ZSMIN(KD1+1),1,ZZ(K))-1
        DO KD=KD1,KD2
          KU=KD+1
          DO I=1,IM
            IF(ZZ(K).GE.ZS(I,KD).AND.ZZ(K).LT.ZS(I,KU)) THEN
              WU=(ZZ(K)-ZS(I,KD))/(ZS(I,KU)-ZS(I,KD))
              UZZ(I,K)=US(I,KD)+WU*(US(I,KU)-US(I,KD))
              VZZ(I,K)=VS(I,KD)+WU*(VS(I,KU)-VS(I,KD))
              TZZ(I,K)=TS(I,KD)+WU*(TS(I,KU)-TS(I,KD))
            ENDIF
          ENDDO
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  EXTRAPOLATE SIGMA TO HEIGHT OUTSIDE MODEL DOMAIN
        DO I=1,IM
          IF(ZZ(K).LT.ZS(I,1)) THEN
            UZZ(I,K)=US(I,1)
            VZZ(I,K)=VS(I,1)
            TZZ(I,K)=TS(I,1)
          ELSEIF(ZZ(K).GE.ZS(I,KM)) THEN
            UZZ(I,K)=US(I,KM)
            VZZ(I,K)=VS(I,KM)
            TZZ(I,K)=TS(I,KM)
          ENDIF
        ENDDO
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE SPCOEF(L,N,X,F,S)                                      
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SPCOEF      COMPUTE 2ND DERIVATIVES FOR CUBIC SPLINES 
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: COMPUTE THE SECOND DERIVATIVES OF CUBIC SPLINE PROFILES     
!   IN PREPARATION FOR CUBIC SPLINE INTERPOLATIONS.                     
!   CUBIC SPLINES ARE PIECEWISE CUBIC POLYNOMIALS FITTING THE DATA      
!   WITH CONTINUOUS FIRST AND SECOND DERIVATIVES AT INTERIOR POINTS     
!   AND SECOND DERIVATIVES SET TO ZERO AT AND BEYOND THE END POINTS.    
!   THE COMPUTATIONS ARE DONE BY MARCHING UP THEN DOWN THE PROFILES.    
!   NOTE THE INNER DIMENSION OF THE DATA IS THE NUMBER OF PROFILES.     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!                                                                       
! USAGE:    CALL SPCOEF(L,N,X,F,S)                                      
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     L        - INTEGER NUMBER OF PROFILES                             
!     N        - INTEGER NUMBER OF POINTS IN EACH PROFILE               
!     X        - REAL (N) MONOTONICALLY INCREASING ABSCISSA VALUES      
!     F        - REAL (L,N) DATA VALUES                                 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     S        - REAL (L,N) 2ND DERIVATIVE OF F WITH RESPECT TO X       
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION X(L,N),F(L,N),S(L,N)                                      
      DIMENSION RHO(N-1)                                                
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  INITIALIZE END POINTS                                                
      RHO(1)=0.                                                         
      DO I=1,L                                                          
        S(I,1)=0.                                                       
        S(I,N)=0.                                                       
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  MARCH UP THE PROFILES                                                
        DO K=2,N-1                                                        
          HM1=X(I,K)-X(I,K-1)                                                 
          RH=1./(X(I,K+1)-X(I,K))                                             
          RHO(K)=-1./(HM1*(RHO(K-1)+2.)*RH+2.)                            
          D=6.*((F(I,K+1)-F(I,K))*RH-(F(I,K)-F(I,K-1))/HM1)*RH          
          S(I,K)=(HM1*S(I,K-1)*RH-D)*RHO(K)                             
        ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  MARCH DOWN THE PROFILES                                              
        DO K=N-1,2,-1                                                     
          S(I,K)=RHO(K)*S(I,K+1)+S(I,K)                                 
        ENDDO                                                             
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE SPFMAX(L,N,X,F,S,P,XP,FP)                              
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SPFMAX      FIND MAXIMUM VALUE USING CUBIC SPLINES    
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: COMPUTE THE MAXIMUM DATA VALUE OF CUBIC SPLINE PROFILES.    
!   CUBIC SPLINES ARE PIECEWISE CUBIC POLYNOMIALS FITTING THE DATA      
!   WITH CONTINUOUS FIRST AND SECOND DERIVATIVES AT INTERIOR POINTS     
!   AND SECOND DERIVATIVES SET TO ZERO AT AND BEYOND THE END POINTS.    
!   SUBPROGRAM SPCOEF MUST BE ALREADY CALLED TO COMPUTE 2ND DERIVATIVES.
!   NOTE THE INNER DIMENSION OF THE DATA IS THE NUMBER OF PROFILES.     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!                                                                       
! USAGE:    CALL SPFMAX(L,N,X,F,S,P,XP,FP)                              
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     L        - INTEGER NUMBER OF PROFILES                             
!     N        - INTEGER NUMBER OF POINTS IN EACH PROFILE               
!     X        - REAL (N) MONOTONICALLY INCREASING ABSCISSA VALUES      
!     F        - REAL (L,N) DATA VALUES                                 
!     S        - REAL (L,N) 2ND DERIVATIVE OF F (FROM SUBPROGRAM SPCOEF)
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     P        - REAL (L) POINT NUMBER                                  
!     XP       - REAL (L) ABSCISSA VALUES OF MAXIMUM VALUE              
!     FP       - REAL (L) MAXIMUM DATA VALUES                           
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION X(L,N),F(L,N),S(L,N),P(L),XP(L),FP(L)                     
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  FIND MAXIMUM GIVEN VALUE                                             
      DO I=1,L                                                          
        P(I)=1                                                          
        FP(I)=F(I,1)                                                    
        DO K=2,N                                                          
          IF(F(I,K).GT.FP(I)) THEN                                      
            P(I)=K                                                      
            FP(I)=F(I,K)                                                
          ENDIF                                                         
        ENDDO                                                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  DETERMINE MAXIMUM VALUE OF CUBIC SPLINE                              
        K1=NINT(P(I))                                                   
        KT=K1+SIGN(1,N+1-2*K1)                                          
        DX=X(I,K1)-X(I,KT)                                                  
        DF=F(I,K1)-F(I,KT)                                              
        S1=S(I,K1)                                                      
        ST=S(I,KT)                                                      
        DP=DF/DX+DX*(2*S1+ST)/6                                         
        K2=K1+SIGN(1.,DP)                                               
        IF(K2.GE.1.AND.K2.LE.N) THEN                                    
          X1=X(I,K1)                                                      
          X2=X(I,K2)                                                      
          XM=(X2+X1)/2                                                  
          DX=X2-X1                                                      
          F1=F(I,K1)                                                    
          F2=F(I,K2)                                                    
          DF=F2-F1                                                      
          S1=S(I,K1)                                                    
          S2=S(I,K2)                                                    
          SM=(S2+S1)/2                                                  
          DS=S2-S1                                                      
          IF(DS.NE.0.) THEN                                             
            XPA=XM-SM*DX/DS                                             
            XPB=SQRT((DX**2*(4*SM**2-S1*S2)/(3*DS)-2*DF)/DS)            
            XP(I)=XPA+XPB                                               
            SP=S1+DS*(XP(I)-X(I,K1))/DX                                   
            IF(SP.GT.0.) XP(I)=XPA-XPB                                  
          ELSEIF(S1.LT.0.) THEN                                         
            XP(I)=XM-DF/(DX*S1)                                         
          ELSE                                                          
            XP(I)=X1                                                    
          ENDIF                                                         
          DXP=XP(I)-X1                                                  
          P(I)=K1+DXP/DX                                                
          FP(I)=F1+DXP/DX*(DF-(DX-DXP)*(DXP*DS+DX*(2*S1+S2))/6)         
        ELSE                                                            
          P(I)=K1                                                       
          XP(I)=X(I,K1)                                                   
          FP(I)=F(I,K1)                                                 
        ENDIF                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE SPLINE(L,N,X,F,S,P,XP,FP,DP)                           
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
!  SUBPROGRAM:    SPLINE      INTERPOLATE DATA USING CUBIC SPLINES      
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: INTERPOLATE CUBIC SPLINE PROFILES TO GIVEN POINTS.          
!   CUBIC SPLINES ARE PIECEWISE CUBIC POLYNOMIALS FITTING THE DATA      
!   WITH CONTINUOUS FIRST AND SECOND DERIVATIVES AT INTERIOR POINTS     
!   AND SECOND DERIVATIVES SET TO ZERO AT AND BEYOND THE END POINTS.    
!   SUBPROGRAM SPCOEF MUST BE ALREADY CALLED TO COMPUTE 2ND DERIVATIVES.
!   NOTE THE INNER DIMENSION OF THE DATA IS THE NUMBER OF PROFILES.     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!                                                                       
! USAGE:    CALL SPLINE(L,N,X,F,S,P,XP,FP,DP)                           
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     L        - INTEGER NUMBER OF PROFILES                             
!     N        - INTEGER NUMBER OF POINTS IN EACH PROFILE               
!     X        - REAL (N) MONOTONICALLY INCREASING ABSCISSA VALUES      
!     F        - REAL (L,N) DATA VALUES                                 
!     S        - REAL (L,N) 2ND DERIVATIVE OF F (FROM SUBPROGRAM SPCOEF)
!     P        - REAL (L) POINT NUMBER OR 0 TO CALCULATE POINT NUMBER   
!     XP       - REAL (L) ABSCISSA VALUES TO WHICH TO INTERPOLATE       
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     P        - REAL (L) POINT NUMBER OR                               
!     FP       - REAL (L) INTERPOLATED DATA VALUES                      
!     DP       - REAL (L) 1ST DERIVATIVE OF F AT XP                     
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION X(N),F(L,N),S(L,N),P(L),XP(L),FP(L),DP(L)               
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  CALCULATE POINT NUMBER IF NECESSARY                                  
      DO I=1,L                                                          
        IF(P(I).LE.0.) THEN                                             
          K=1                                                           
          DOWHILE(K.LE.N.AND.XP(I).GT.X(K))                             
            K=K+1                                                       
          ENDDO                                                         
          P(I)=K-0.5                                                    
        ENDIF                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  EXTRAPOLATE OR INTERPOLATE CUBIC SPLINE                              
      DO I=1,L                                                          
        IF(P(I).LE.1.) THEN                                             
          P(I)=0.                                                       
          DX=X(2)-X(1)                                                  
          DP(I)=(F(I,2)-F(I,1))/DX-DX*S(I,2)/6.                         
          FP(I)=F(I,1)+(XP(I)-X(1))*DP(I)                               
        ELSEIF(P(I).GT.N) THEN                                          
          P(I)=N+1                                                      
          DX=X(N)-X(N-1)                                                
          DP(I)=(F(I,N)-F(I,N-1))/DX+DX*S(I,N-1)/6.                     
          FP(I)=F(I,N)+(XP(I)-X(N))*DP(I)                               
        ELSE                                                            
          KD=P(I)                                                       
          KU=KD+1                                                       
          DX=X(KU)-X(KD)                                                
          DD=XP(I)-X(KD)                                                
          DU=DX-DD                                                      
          P(I)=KD+DD/DX                                                 
          FU=F(I,KU)                                                    
          FD=F(I,KD)                                                    
          DF=FU-FD                                                      
          SU=S(I,KU)                                                    
          SD=S(I,KD)                                                    
          DS=SU-SD                                                      
          DP(I)=(DF+SU*DD**2/2-SD*DU**2/2-DS*DX**2/6)/DX                
          FP(I)=FD+DD/DX*(DF-DU*(DD*DS+DX*(SU+2*SD))/6)                 
        ENDIF                                                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE SUNDRY(IM,IX,KM,KSLP,KLI,K5Z,CLAT,SI,SL,KT,PT,           &
     &                  PS,OS,P,U,V,O,T,R,Q,QS,ZL,ZI,                     &
     &                  TPT,QPT,ZM,TM,OZ,CL,SUN)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    SUNDRY      COMPUTE SUNDRY FIELDS                      
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: COMPUTES SUNDRY FIELDS.                                     
!   THE CURRENT NSUN=26 SUNDRY FIELDS ARE:                              
!     1) SURFACE PRESSURE
!     2) SURFACE PRESSURE TENDENCY
!     3) COLUMN PRECIPITABLE WATER
!     4) COLUMN RELATIVE HUMIDITY
!     5) TROPOPAUSE HEIGHT
!     6) TROPOPAUSE TEMPERATURE
!     7) TROPOPAUSE PRESSURE
!     8) TROPOPAUSE ZONAL WIND
!     9) TROPOPAUSE MERIDIONAL WIND
!    10) TROPOPAUSE VERTICAL WIND SPEED SHEAR
!    11) SURFACE LIFTED INDEX
!    12) SURFACE CONVECTIVE AVAILABLE POTENTIAL ENERGY
!    13) SURFACE CONVECTIVE INHIBITIION
!    14) BEST LIFTED INDEX
!    15) BEST CONVECTIVE AVAILABLE POTENTIAL ENERGY
!    16) BEST CONVECTIVE INHIBITIION
!    17) MAXIMUM WIND HEIGHT
!    18) MAXIMUM WIND LEVEL TEMPERATURE
!    19) MAXIMUM WIND LEVEL PRESSURE
!    20) MAXIMUM WIND LEVEL ZONAL WIND
!    21) MAXIMUM WIND LEVEL MERIDIONAL WIND
!    22) SURFACE OROGRAPHY
!    23) SEA LEVEL PRESSURE
!    24) RELATIVE HUMIDITY IN SIGMA RANGE (0.44,1.00)
!    25) RELATIVE HUMIDITY IN SIGMA RANGE (0.72,0.94)
!    26) RELATIVE HUMIDITY IN SIGMA RANGE (0.44,0.72)
!    27) POTENTIAL TEMPERATURE AT SIGMA 0.9950
!    28) TEMPERATURE AT SIGMA 0.9950
!    29) PRESSURE VERTICAL VELOCITY AT SIGMA 0.9950
!    30) RELATIVE HUMIDITY AT SIGMA 0.9950
!    31) ZONAL WIND AT SIGMA 0.9950
!    32) MERIDIONAL WIND AT SIGMA 0.9950
!    33) TOTAL CLOUD WATER
!    34) TOTAL OZONE
!    35) 5-WAVE 500 MB GEOPOTENTIAL HEIGHT
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   SIG2TP       INTERPOLATE SIGMA TO TROPOPAUSE LEVEL                  
!   SIG2MW       INTERPOLATE SIGMA TO MAXWIND LEVEL                     
!   LIFTIX       COMPUTE BEST LIFTED INDEX                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  MCCALLA,IREDELL                                           
!   97-10-31  JUANG   FOR HYDRO AND NONHYDROSTATIC
!                                                                       
! USAGE:    CALL SUNDRY(IM,IX,KM,KSLP,KLI,K5Z,CLAT,SI,SL,KT,PT,             
!    &                  PS,OS,P,U,V,O,T,R,Q,QS,ZL,ZI,
!    &                  TPT,QPT,ZM,TM,OZ,CL,SUN)      
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM       - INTEGER NUMBER OF POINTS                               
!     IX       - INTEGER FIRST DIMENSION OF UPPER AIR DATA              
!     KM       - INTEGER NUMBER OF LEVELS                               
!     KSLP     - INTEGER (2) RELEVANT PRESSURE LEVELS FOR SLP           
!     KLI      - INTEGER RELEVANT PRESSURE LEVEL FOR LIFTED INDEX       
!     K5Z      - INTEGER RELEVANT PRESSURE LEVEL 5-WAVE HEIGHT
!     CLAT     - REAL (IM) COSINE OF LATITUDE                           
!     SI       - REAL (KM) SIGMA INTERFACES                             
!     SL       - REAL (KM) SIGMA VALUES                                 
!     KT       - INTEGER NUMBER OF PRESSURE THICKNESS LAYERS            
!     PT       - REAL PRESSURE THICKNESS IN KPA                         
!     ZS       - REAL (IM) SURFACE OROGRAPHY IN M                       
!     PS       - REAL (IM) SURFACE PRESSURE IN KPA                      
!     OS       - REAL (IM) SURFACE PRESSURE TENDENCY IN PA/S            
!     U        - REAL (IX,KM) ZONAL WIND IN M/S                         
!     V        - REAL (IX,KM) MERIDIONAL WIND IN M/S                    
!     O        - REAL (IX,KM) VERTICAL VELOCITY IN PA/S                 
!     T        - REAL (IX,KM) TEMPERATURE IN K                          
!     R        - REAL (IX,KM) RELATIVE HUMIDITY IN PERCENT              
!     Q        - REAL (IX,KM) SPECIFIC HUMIDITY IN KG/KG                
!     QS       - REAL (IX,KM) SATURATED SPECIFIC HUMIDITY IN KG/KG      
!     ZL       - REAL (IX,KM) HEIGHTS ON THE FULL LEVELS IN M
!     ZI       - REAL (IX,KM) HEIGHTS ON THE INTERFACES IN M
!     TPT      - REAL (IX,KT) TEMPERATURE IN K                          
!     QPT      - REAL (IX,KT) SPECIFIC HUMIDITY IN KG/KG                
!     ZM       - REAL (IX,*) HEIGHT ON PRESSURE SURFACE IN M            
!     TM       - REAL (IX,*) TEMPERATURE ON PRESSURE SURFACE IN K       
!     CL       - REAL (IX,KM) CLOUD WATER IN KG/KG
!     OZ       - REAL (IX,KM) OZONE IN KG/KG

!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUN      - REAL (IX,NSUN) SUNDRY FIELDS GIVEN ABOVE               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      PARAMETER(NSUN=35)                                                
      PARAMETER(NPS  = 1,NPST = 2,NTPW = 3,NTRH = 4,NTPZ = 5,            &
     &          NTPT = 6,NTPP = 7,NTPU = 8,NTPV = 9,NTPSH=10,            &
     &          NSLI =11,NSCAP=12,NSCIN=13,NBLI =14,NBCAP=15,            &
     &          NBCIN=16,NMWZ= 17,NMWT =18,NMWP =19,NMWU =20,            &
     &          NMWV =21,NZS  =22,NSLP =23,NRH1 =24,NRH2 =25,            &
     &          NRH3 =26,NS1TH=27,NS1T =28,NS1O =29,NS1R =30,            &
     &          NS1U =31,NS1V =32,NTCL =33,NTOZ =34,N5Z  =35)
      DIMENSION KSLP(2)                                                 
      DIMENSION SI(KM+1),SL(KM)                                         
      DIMENSION PS(IM),OS(IM),CLAT(IM)                           
      DIMENSION U(IX,KM),V(IX,KM),O(IX,KM),P(IX,KM)                     
      DIMENSION ZL(IX,KM),ZI(IX,KM)
      DIMENSION T(IX,KM),R(IX,KM),Q(IX,KM),QS(IX,KM)                    
      DIMENSION TPT(IX,KT),QPT(IX,KT)                                   
      DIMENSION ZM(IX,*),TM(IX,*)                                       
      DIMENSION OZ(IX,KM),CL(IX,KM)
      DIMENSION SUN(IX,NSUN)                                            
      DIMENSION WRK(IM)                                                 
      PARAMETER(G= 9.8000E+0 ,RD= 2.8705E+2 ,CP= 1.0046E+3 )            
      PARAMETER(ROCP=RD/CP,GOR=G/RD)                                             
      PARAMETER(PM1=1.E5,TM1=287.45,ZM1=113.,ZM2=5572.)                 
      PARAMETER(FSLP=G*(ZM2-ZM1)/(RD*TM1))                              
      PARAMETER(STRH1=0.44,STRH2=0.72,STRH3=0.44,                        &
     &          SBRH1=1.00,SBRH2=0.94,SBRH3=0.72)                       
      PARAMETER(SL1=0.9950)                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SURFACE OROGRAPHY, SURFACE PRESSURE AND SURFACE PRESSURE TENDENCY    
      DO I=1,IM                                                         
        SUN(I,NZS)=ZI(I,1)                                              
        SUN(I,NPS)=PS(I)*1.E3                                           
        SUN(I,NPST)=OS(I)                                               
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COLUMN PRECIPITABLE WATER AND RELATIVE HUMIDITY                      
      DO I=1,IM                                                         
        SUN(I,NTPW)=0.                                                  
        WRK(I)=0.                                                       
      ENDDO                                                             
      DO K=1,KM                                                       
        DS=SI(K)-SI(K+1)                                                
        DO I=1,IM                                                       
          SUN(I,NTPW)=SUN(I,NTPW)+Q(I,K)*DS                             
          WRK(I)=WRK(I)+QS(I,K)*DS                                      
        ENDDO                                                           
      ENDDO                                                             
      DO I=1,IM                                                         
        SUN(I,NTRH)=MIN(MAX(SUN(I,NTPW)/WRK(I),0.),1.)*100.             
        SUN(I,NTPW)=MAX(SUN(I,NTPW),0.)*PS(I)*1.E3/G                            
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SUNDRY TROPOPAUSE FIELDS                                             
      CALL SIG2TP(IM,IX,KM,CLAT,SI,SL,P,U,V,T,ZL,ZI,                     &
     &            SUN(1,NTPP),SUN(1,NTPU),SUN(1,NTPV),                   &
     &            SUN(1,NTPT),SUN(1,NTPZ),SUN(1,NTPSH))                             
      DO I=1,IM                                                         
        SUN(I,NTPP)=SUN(I,NTPP)*1.E3                                    
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  LIFTED INDEX                                                         
      IF(KLI.GT.0) THEN                                                 
        CALL LIFTIX(IM,IX,KT,PT,KM,SI,SL,PS,TM(1,KLI),TPT,QPT,T,Q,       &
     &              SUN(1,NSLI),SUN(1,NSCAP),SUN(1,NSCIN),               &
     &              SUN(1,NBLI),SUN(1,NBCAP),SUN(1,NBCIN))
      ELSE                                                              
        DO I=1,IM                                                       
          SUN(I,NSLI)=0.                                                
          SUN(I,NSCAP)=0.
          SUN(I,NSCIN)=0.
          SUN(I,NBLI)=0.                                                
          SUN(I,NBCAP)=0.
          SUN(I,NBCIN)=0.
        ENDDO                                                           
      ENDIF                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SUNDRY MAXWIND FIELDS                                                
      CALL SIG2MW(IM,IX,KM,P,U,V,T,ZL,                                   &
     &            SUN(1,NMWP),SUN(1,NMWU),SUN(1,NMWV),                   &
     &            SUN(1,NMWT),SUN(1,NMWZ))      
      DO I=1,IM                                                         
        SUN(I,NMWP)=SUN(I,NMWP)*1.E3                                    
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SEA LEVEL PRESSURE                                                   
      IF(KSLP(1).GT.0.AND.KSLP(2).GT.0) THEN                            
        K1=KSLP(1)                                                      
        K2=KSLP(2)                                                      
        DO I=1,IM                                                       
!          SUN(I,NSLP)=PM1*EXP(FSLP*ZM(I,K1)/(ZM(I,K2)-ZM(I,K1)))        
           TMEAN=0.5*ZL(I,1)*0.0065+T(I,1)
           SUN(I,NSLP)=P(I,1)*EXP(GOR*ZL(I,1)/TMEAN)*1.E3
        ENDDO                                                           
      ELSE                                                              
        DO I=1,IM                                                       
          SUN(I,NSLP)=0.                                                
        ENDDO                                                           
      ENDIF                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  AVERAGE RELATIVE HUMIDITY 1                                          
      DO I=1,IM                                                         
        SUN(I,NRH1)=0.                                                  
        WRK(I)=0.                                                       
      ENDDO                                                             
      DO K=1,KM                                                         
        DS=MIN(SI(K),SBRH1)-MAX(SI(K+1),STRH1)                          
        IF(DS.GT.0.) THEN                                               
          DO I=1,IM                                                     
            SUN(I,NRH1)=SUN(I,NRH1)+Q(I,K)*DS                           
            WRK(I)=WRK(I)+QS(I,K)*DS                                    
          ENDDO                                                         
        ENDIF                                                           
      ENDDO                                                             
      DO I=1,IM                                                         
        SUN(I,NRH1)=MIN(MAX(SUN(I,NRH1)/WRK(I),0.),1.)*100.             
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  AVERAGE RELATIVE HUMIDITY 2                                          
      DO I=1,IM                                                         
        SUN(I,NRH2)=0.                                                  
        WRK(I)=0.                                                       
      ENDDO                                                             
      DO K=1,KM                                                         
        DS=MIN(SI(K),SBRH2)-MAX(SI(K+1),STRH2)                          
        IF(DS.GT.0.) THEN                                               
          DO I=1,IM                                                     
            SUN(I,NRH2)=SUN(I,NRH2)+Q(I,K)*DS                           
            WRK(I)=WRK(I)+QS(I,K)*DS                                    
          ENDDO                                                         
        ENDIF                                                           
      ENDDO                                                             
      DO I=1,IM                                                         
        SUN(I,NRH2)=MIN(MAX(SUN(I,NRH2)/WRK(I),0.),1.)*100.             
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  AVERAGE RELATIVE HUMIDITY 3                                          
      DO I=1,IM                                                         
        SUN(I,NRH3)=0.                                                  
        WRK(I)=0.                                                       
      ENDDO                                                             
      DO K=1,KM                                                         
        DS=MIN(SI(K),SBRH3)-MAX(SI(K+1),STRH3)                          
        IF(DS.GT.0.) THEN                                               
          DO I=1,IM                                                     
            SUN(I,NRH3)=SUN(I,NRH3)+Q(I,K)*DS                           
            WRK(I)=WRK(I)+QS(I,K)*DS                                    
          ENDDO                                                         
        ENDIF                                                           
      ENDDO                                                             
      DO I=1,IM                                                         
        SUN(I,NRH3)=MIN(MAX(SUN(I,NRH3)/WRK(I),0.),1.)*100.             
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  BOTTOM SIGMA FIELDS INTERPOLATED FROM FIRST TWO MODEL LAYERS         
      F2=LOG(SL(1)/SL1)/LOG(SL(1)/SL(2))                                
      SL1K=(SL1*1.E-2)**(-ROCP)                                         
      DO I=1,IM                                                         
        SUN(I,NS1T)=T(I,1)+F2*(T(I,2)-T(I,1))                           
        SUN(I,NS1TH)=SUN(I,NS1T)*SL1K*PS(I)**(-ROCP)                    
        SUN(I,NS1O)=O(I,1)+F2*(O(I,2)-O(I,1))                           
        SUN(I,NS1R)=R(I,1)+F2*(R(I,2)-R(I,1))                           
        SUN(I,NS1U)=U(I,1)+F2*(U(I,2)-U(I,1))                           
        SUN(I,NS1V)=V(I,1)+F2*(V(I,2)-V(I,1))                           
      ENDDO                                                             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  TOTAL CLOUD WATER
        DO I=1,IM
          SUN(I,NTCL)=0.
        ENDDO
        DO K=1,KM
          DS=SI(K)-SI(K+1)
          DO I=1,IM
            SUN(I,NTCL)=SUN(I,NTCL)+CL(I,K)*DS
          ENDDO
        ENDDO
        DO I=1,IM
          SUN(I,NTCL)=MAX(SUN(I,NTCL),0.)*PS(I)*1.E3/G
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  TOTAL OZONE
        DO I=1,IM
          SUN(I,NTOZ)=0.
        ENDDO
        DO K=1,KM
          DS=SI(K)-SI(K+1)
          DO I=1,IM
            SUN(I,NTOZ)=SUN(I,NTOZ)+OZ(I,K)*DS
          ENDDO
        ENDDO
        DO I=1,IM
          SUN(I,NTOZ)=MAX(SUN(I,NTOZ),0.)*PS(I)*1.E3/G
!  CONVERT OZONE FROM KG/M2 TO DOBSON UNITS, WHICH GIVE THE DEPTH OF THE
!  OZONE LAYER IN 1E-5 M IF BROUGHT TO NATURAL TEMPERATURE AND PRESSURE.
          SUN(I,NTOZ)=SUN(I,NTOZ)/2.14E-5
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  5-WAVE HEIGHT
      IF(K5Z.GT.0) THEN
        DO I=1,IM
          SUN(I,N5Z)=ZM(I,K5Z)
        ENDDO
      ELSE
        DO I=1,IM
          SUN(I,N5Z)=0.
        ENDDO
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE SUNPRM(KO,PO,KT,KTT,PT,                                 &
     &                  IPUSUN,ITLSUN,IP1SUN,IP2SUN,KSLP,KLI,K5Z,L5Z)     
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    SUNPRM      SET PARAMETERS FOR SUNDRY FIELDS           
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31             
!                                                                       
! ABSTRACT: SETS PARAMETERS FOR THE SUNDRY FIELDS.                      
!   PARAMETERS RETURNED ARE PARAMETER INDICATOR, LEVEL TYPE INDICATOR   
!   AND TWO LEVEL NUMBERS.                                              
!   THE CURRENT NSUN=35 SUNDRY FIELDS ARE:                              
!     1) SURFACE PRESSURE
!     2) SURFACE PRESSURE TENDENCY
!     3) COLUMN PRECIPITABLE WATER
!     4) COLUMN RELATIVE HUMIDITY
!     5) TROPOPAUSE HEIGHT
!     6) TROPOPAUSE TEMPERATURE
!     7) TROPOPAUSE PRESSURE
!     8) TROPOPAUSE ZONAL WIND
!     9) TROPOPAUSE MERIDIONAL WIND
!    10) TROPOPAUSE VERTICAL WIND SPEED SHEAR
!    11) SURFACE LIFTED INDEX
!    12) SURFACE CONVECTIVE AVAILABLE POTENTIAL ENERGY
!    13) SURFACE CONVECTIVE INHIBITIION
!    14) BEST LIFTED INDEX
!    15) BEST CONVECTIVE AVAILABLE POTENTIAL ENERGY
!    16) BEST CONVECTIVE INHIBITIION
!    17) MAXIMUM WIND HEIGHT
!    18) MAXIMUM WIND LEVEL TEMPERATURE
!    19) MAXIMUM WIND LEVEL PRESSURE
!    20) MAXIMUM WIND LEVEL ZONAL WIND
!    21) MAXIMUM WIND LEVEL MERIDIONAL WIND
!    22) SURFACE OROGRAPHY
!    23) SEA LEVEL PRESSURE
!    24) RELATIVE HUMIDITY IN SIGMA RANGE (0.44,1.00)
!    25) RELATIVE HUMIDITY IN SIGMA RANGE (0.72,0.94)
!    26) RELATIVE HUMIDITY IN SIGMA RANGE (0.44,0.72)
!    27) POTENTIAL TEMPERATURE AT SIGMA 0.9950
!    28) TEMPERATURE AT SIGMA 0.9950
!    29) PRESSURE VERTICAL VELOCITY AT SIGMA 0.9950
!    30) RELATIVE HUMIDITY AT SIGMA 0.9950
!    31) ZONAL WIND AT SIGMA 0.9950
!    32) MERIDIONAL WIND AT SIGMA 0.9950
!    33) TOTAL CLOUD WATER
!    34) TOTAL OZONE
!    35) 5-WAVE 500 MB GEOPOTENTIAL HEIGHT
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   92-10-31  IREDELL                                                   
!   94-08-01  IREDELL                                                   
!                                                                       
! USAGE:    CALL SUNPRM(KO,PO,KT,KTT,PT,
!    &                  IPUSUN,ITLSUN,IP1SUN,IP2SUN,KSLP,KLI,K5Z,L5Z)     
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     KO       - INTEGER NUMBER OF PRESSURE LEVELS                      
!     PO       - REAL (KO) PRESSURE IN MILLIBARS                        
!     KT       - INTEGER NUMBER OF PRESSURE THICKNESS LAYERS
!     PT       - REAL PRESSURE THICKNESS IN KPA
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IPUSUN   - INTEGER (NSUN) PARAMETER INDICATORS                    
!     ITLSUN   - INTEGER (NSUN) LEVEL TYPE INDICATORS                   
!     IP1SUN   - INTEGER (NSUN) FIRST LEVEL NUMBERS                     
!     IP2SUN   - INTEGER (NSUN) SECOND LEVEL NUMBERS                    
!     KSLP     - INTEGER (2) RELEVANT PRESSURE LEVELS FOR SLP           
!     KLI      - INTEGER RELEVANT PRESSURE LEVEL FOR LIFTED INDEX       
!     K5Z      - INTEGER RELEVANT PRESSURE LEVEL 5-WAVE HEIGHT
!     L5Z      - INTEGER SUNDRY FIELD NUMBER FOR 5-WAVE HEIGHT
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   ISRCHEQ  - FIND FIRST VALUE IN AN ARRAY EQUAL TO TARGET VALUE       
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: CRAY FORTRAN                                              
!                                                                       
!$$$                                                                    
      DIMENSION PO(KO)                                                  
      PARAMETER(NSUN=35)                                                
      PARAMETER(NPS  = 1,NPST = 2,NTPW = 3,NTRH = 4,NTPZ = 5,            &
     &          NTPT = 6,NTPP = 7,NTPU = 8,NTPV = 9,NTPSH=10,            &
     &          NSLI =11,NSCAP=12,NSCIN=13,NBLI =14,NBCAP=15,            &
     &          NBCIN=16,NMWZ= 17,NMWT =18,NMWP =19,NMWU =20,            &
     &          NMWV =21,NZS  =22,NSLP =23,NRH1 =24,NRH2 =25,            &
     &          NRH3 =26,NS1TH=27,NS1T =28,NS1O =29,NS1R =30,            &
     &          NS1U =31,NS1V =32,NTCL =33,NTOZ =34,N5Z  =35)
      DIMENSION IPUSUN(NSUN),ITLSUN(NSUN)                               
      DIMENSION IP1SUN(NSUN),IP2SUN(NSUN)                               
      DIMENSION KSLP(2)                                                 
      DIMENSION IPUDEF(NSUN),ITLDEF(NSUN)                               
      DIMENSION IP1DEF(NSUN),IP2DEF(NSUN)                               
      DIMENSION PSLP(2)                                                 
      DATA IPUDEF/001,003,054,052,007,011,001,033,034,136,               &
     &            131,157,156,132,157,156,007,011,001,033,               &
     &            034,007,002,052,052,052,013,011,039,052,               &
     &            033,034,153,154,222/
      DATA ITLDEF/001,001,200,200,007,007,007,007,007,007,               &
     &            001,001,001,116,116,116,006,006,006,006,               &
     &            006,001,102,108,108,108,107,107,107,107,               &
     &            107,107,200,200,100/
      DATA IP1DEF/000,000,000,000,000,000,000,000,000,000,               &
     &            000,000,000,000,000,000,000,000,000,000,               &
     &            000,000,000,044,072,044,00000,00000,00000,00000,       &
     &            00000,00000,000,000,000/
      DATA IP2DEF/000,000,000,000,000,000,000,000,000,000,               &
     &            000,000,000,000,000,000,000,000,000,000,               &
     &            000,000,000,100,094,072,09950,09950,09950,09950,       &
     &            09950,09950,000,000,500/
      DATA PSLP/1000.,500./,PLI/500./                                   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      IPUSUN=IPUDEF        
      ITLSUN=ITLDEF       
      IP1SUN=IP1DEF      
      IP2SUN=IP2DEF                                                     
      KSLP(1)=MOD(ISRCHEQ(KO,PO,1,PSLP(1)),KO+1)                        
      KSLP(2)=MOD(ISRCHEQ(KO,PO,1,PSLP(2)),KO+1)                        
      KLI=MOD(ISRCHEQ(KO,PO,1,PLI),KO+1)                                
      IF(KSLP(1).EQ.0.OR.KSLP(2).EQ.0) IPUSUN(NSLP)=0                   
      IF(KLI.EQ.0) IPUSUN(NSLI)=0                                       
      IF(KLI.EQ.0) IPUSUN(NBLI)=0                                       
      IP1SUN(NBLI)=KTT*PT
      IP1SUN(NBCAP)=KTT*PT
      IP1SUN(NBCIN)=KTT*PT
!     IPUSUN(NTCL)=0
!     IPUSUN(NTOZ)=0
      P5Z=IP2SUN(N5Z)
      K5Z=MOD(ISRCHEQ(KO,PO,1,P5Z),KO+1)
      IF(K5Z.EQ.0) IPUSUN(N5Z)=0
      L5Z=N5Z
      ITLSUN(NBLI)=1
      IP1SUN(NBLI)=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN                                                            
      END                                                               
!-----------------------------------------------------------------------
      SUBROUTINE TOAWIPS(A,IJ,B,IJO,N00,N10,N11,N01,D00,D10,D11,D01)    
!                                                                       
      DIMENSION A(IJ),B(IJO)                                            
!                                                                       
      DIMENSION                                                          &
     &          N00(IJO),N10(IJO)                                        &
     &         ,N11(IJO),N01(IJO)                                        &
     &         ,D00(IJO),D10(IJO)                                        &
     &         ,D11(IJO),D01(IJO)                                       
      DIMENSION                                                          &
     &          A00(IJO),A10(IJO)                                        &
     &         ,A11(IJO),A01(IJO)                                        
!     print*, 'IJ = ', IJ, 'IJO = ',IJO
!                                                                       
!MIC$ DO ALL                                                            
!MIC$1 SHARED(A,B)                                                      
!MIC$1 SHARED(N00,N01,N10,N11)                                          
!MIC$1 SHARED(D00,D01,D10,D11)                                          
!MIC$1 PRIVATE(A00,A01,A10,A11,N)                                       
      DO 2000 N=1,IJO                                                   
      A00(N) = A(N00(N))                                                
      A10(N) = A(N10(N))                                                
      A11(N) = A(N11(N))                                                
      A01(N) = A(N01(N))                                                
2000  CONTINUE                                                          
      DO 3000 N=1,IJO                                                   
      B(N)   =    A00(N) * D00(N)                                        &
     &          + A10(N) * D10(N)                                        &
     &          + A11(N) * D11(N)                                        &
     &          + A01(N) * D01(N)                                       
3000  CONTINUE                                                          
!                                                                       
      RETURN                                                            
      END                                                               

      SUBROUTINE I2OINI(PROJ ,ORIENT ,TRUTH ,COTRU ,                     &
     &                  DELX ,DELY ,RLAT1 ,RLON1 ,RLAT2,RLON2,II, JJ,    &
     &                  PROJO,ORIENTO,TRUTHO,COTRUO,                     &
     &                  DELXO,DELYO,RLAT1O,RLON1O,RLAT2O,RLON2O,         &
     &                  N00,N10,N11,N01,D00,D10,D11,D01,IO,JO)          
!                                                                       
      DIMENSION                                                          &
     &          N00(IO*JO),N10(IO*JO)                                    &
     &         ,N11(IO*JO),N01(IO*JO)                                    &
     &         ,D00(IO*JO),D10(IO*JO)                                    &
     &         ,D11(IO*JO),D01(IO*JO)                                   
!                                                                       
      PRINT *,' DELX DELY DELXO DELYO ',DELX,DELY,DELXO,DELYO           
      CALL LL2XY(PROJ,ORIENT,TRUTH,COTRU,RLAT1,RLON1,X00,Y00)           
      PRINT *,' INPUT GRID RLAT1 RLON1 X00 Y00 ',                        &
     &                     RLAT1,RLON1,X00,Y00                          
!                                                                       
      CALL LL2XY(PROJ,ORIENT,TRUTH,COTRU,RLAT2,RLON2,X11,Y11)           
      PRINT *,' INPUT GRID RLAT2 RLON2 X11 Y11 ',                        &
     &                     RLAT2,RLON2,X11,Y11                          
      PRINT *,' INPUT DIMENSION II JJ ',II,JJ                           
      DELXII=(X11-X00)/(II-1)                                           
      DELYII=(Y11-Y00)/(JJ-1)                                           
      PRINT *,' DELXII DELYII ',DELXII,DELYII                           
!                                                                       
      CALL LL2XY(PROJO,ORIENTO,TRUTHO,COTRUO,RLAT1O,RLON1O,X00O,Y00O)   
      PRINT *,' OUTPUT GRID RLAT1  RLON1  X00  Y00 ',                    &
     &                      RLAT1O,RLON1O,X00O,Y00O                     
!                                                                       
      CALL LL2XY(PROJO,ORIENTO,TRUTHO,COTRUO,RLAT2O,RLON2O,X11O,Y11O)   
      PRINT *,' OUTPUT GRID RLAT2  RLON2  X11  Y11 ',                    &
     &                      RLAT2O,RLON2O,X11O,Y11O                     
      DELXOO=(X11O-X00O)/(IO-1)                                         
      DELYOO=(Y11O-Y00O)/(JO-1)                                         
      IF (PROJO.EQ.4) THEN
         DELXO=DELXOO
         DELYO=DELYOO
      ENDIF
      PRINT *,' DELXOO DELYOO ',DELXOO,DELYOO                           
!                                                                       
      N = 0                                                             
      DO 1000 J=1,JO                                                    
       YO=Y00O+(J-1)*DELYO                                              
       DO 1000 I=1,IO                                                   
        XO=X00O+(I-1)*DELXO                                             
        CALL XY2LL(PROJO,ORIENTO,TRUTHO,COTRUO,XO,YO,RLATO,RLONO)       
        CALL LL2XY(PROJ,ORIENT,TRUTH,COTRU,RLATO,RLONO,X,Y)             
        XLON=(X-X00)/DELX+1                                             
        XLAT=(Y-Y00)/DELY+1                                             
        LON=XLON                                                        
        LAT=XLAT                                                        
        IF( LON.LE.1 .OR. LON.GE.II ) THEN                              
           PRINT *,X,' OUT SIDE OF X DIMENSION AT I J ',I,J             
           PRINT *,' XO YO RLATO RLONO X Y ',XO,YO,RLATO,RLONO,X,Y      
           STOP                                                         
        ELSE IF( LAT.LE.1 .OR. LAT.GE.JJ ) THEN                         
           PRINT *,Y,' OUT SIDE OF Y DIMENSION AT I J ',I,J             
           PRINT *,' XO YO RLATO RLONO X Y ',XO,YO,RLATO,RLONO,X,Y      
           STOP                                                         
        ENDIF                                                           
        DLON1=XLON-LON                                                  
        DLAT1=XLAT-LAT                                                  
        DLON0=1-DLON1                                                   
        DLAT0=1-DLAT1                                                   
!                                                                       
      N = N + 1                                                         
      D00(N) = DLON0 * DLAT0                                            
      D10(N) = DLON1 * DLAT0                                            
      D11(N) = DLON1 * DLAT1                                            
      D01(N) = DLON0 * DLAT1                                            
      N00(N) = LON   + (LAT   -1)*II                                    
      N10(N) = LON+1 + (LAT   -1)*II                                    
      N11(N) = LON+1 + (LAT+1 -1)*II                                    
      N01(N) = LON   + (LAT+1 -1)*II                                    
!                                                                       
1000  CONTINUE                                                          
!                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE LL2XY(CPROJ,CORIENT,CTRUTH,CCOTRU,CLAT,CLON,CX,CY)     
!                                                                       
      PARAMETER(PI=3.14159,TWOPI=2.0*PI,HFPI=0.5*PI,QTPI=0.5*HFPI)      
      PARAMETER(RAD=PI/180.,RERTH=6371220.)                             
!                                                                       
! INPUT ARE ALL DEGREE AND OUTPUT TO GLOBAL X Y, NOT DOMAIN X Y         
!                                                                       
! IF PROJ=0  DO MERCATER PROJECTION                                     
! IF PROJ=1  DO NORTH POLAR PROJECTION                                  
! IF PROJ=-1 DO SOUTH POLAR PROJECTION                                  
! IF PROJ=2  DO NORTH LAMBERT PROJECTION                                
! IF PROJ=-2 DO SOUTH LAMBERT PROJECTION                                
!                                                                       
      NPROJ = CPROJ                                                     
      BLAT = CLAT                                                       
      BLON = CLON                                                       
!                                                                       
      IF( NPROJ.EQ.1 .OR. NPROJ.EQ.-1 ) THEN                            
! ++++++++++++++++++++++++++++++++++++++                                
! POLAR PROJECTION                                                      
! ++++++++++++++++++++++++++++++++++++++                                
        TRUTH  = CTRUTH * RAD                                           
        TRUTH  = NPROJ * TRUTH                                          
        ORIENT  = CORIENT * RAD                                         
        DLAMDA0 = ORIENT + HFPI                                         
        A2 =  RERTH * ( 1.0 + SIN(TRUTH) )                              
!                                                                       
! =========                                                             
        RSOA2 = TAN( (HFPI-BLAT*NPROJ)*0.5 )                            
        X2PY2 = ( RSOA2 * A2 ) ** 2.0                                   
        RLON = NPROJ * (BLON - DLAMDA0)                                 
        RLON = AMOD(RLON,TWOPI)                                         
        IF( RLON.LT.0. ) RLON=TWOPI+RLON                                
        YOX = TAN(RLON)                                                 
        X = SQRT( X2PY2/(1.+YOX*YOX) )                                  
        Y = SQRT( X2PY2 - X*X )                                         
        IF( RLON.GT.HFPI .AND. RLON.LT. PI+HFPI ) X = -X                
        IF( RLON.GT.PI .AND. RLON.LT. TWOPI ) Y = -Y                    
!                                                                       
      ELSE IF ( NPROJ.EQ.0 ) THEN                                       
!                                                                       
! ++++++++++++++++++++++++++++                                          
! DO MERCATER                                                           
! ++++++++++++++++++++++++++++                                          
        TRUTH  = CTRUTH * RAD                                           
        CENLON = CORIENT * RAD                                          
        CENLON = MOD(CENLON,TWOPI)                                      
        IF(CENLON.LT.0. E 0) CENLON = TWOPI + CENLON                    
        A2 =  RERTH * COS( TRUTH )                                      
        X0 = 0.0                                                        
        Y0 = A2 * LOG( ABS( TAN( QTPI + 0.5 * CENLAT ) ) )              
        DLAMDA0 = 0.0                                                   
!                                                                       
        BLON = MOD(BLON,TWOPI)                                          
        IF(BLON.LT.0. E 0) BLON = TWOPI + BLON                          
        X=A2*(BLON-CENLON)                                              
        Y=A2*LOG(TAN(BLAT/2.0+QTPI))                                    
!                                                                       
      ELSE IF( NPROJ.EQ.2 .OR. NPROJ.EQ.-2 ) THEN                       
!                                                                       
! ++++++++++++++++++++++++++++                                          
! DO LAMBERT                                                            
! ++++++++++++++++++++++++++++                                          
        IS=1                                                            
        IF( NPROJ.LT.0 ) IS=-1                                          
        TRUTH  = CTRUTH * RAD                                           
        COTRU  = CCOTRU * RAD                                           
        CENLON = CORIENT * RAD                                          
        CENLON = MOD(CENLON,TWOPI)                                      
        IF(CENLON.LT.0. E 0) CENLON = TWOPI + CENLON                    
        IF( CTRUTH.EQ.CCOTRU ) THEN                                     
          CONE= COS (HFPI-IS*TRUTH)                                     
        ELSE                                                            
          CONE=(LOG(COS(TRUTH))-LOG(COS(COTRU)))/                        &
     &         (LOG(TAN(QTPI-IS*TRUTH/2))-LOG(TAN(QTPI-IS*COTRU/2)))    
        ENDIF                                                           
        DLAMDA0 = 0.0                                                   
        R00=RERTH/CONE*COS(TRUTH)/(TAN(QTPI-IS*TRUTH/2))**CONE          
!                                                                       
        BLON = MOD(BLON,TWOPI)                                          
        IF(BLON.LT.0. E 0) BLON = TWOPI + BLON                          
        R=R00*(TAN((QTPI-BLAT/2)*IS))**CONE                             
        X=    R*SIN(CONE*(BLON-CENLON))                                 
        Y=-IS*R*COS(CONE*(BLON-CENLON))                                 
!                                                                       
      ELSE IF( NPROJ.EQ.4 ) THEN
        TRUTH  = CTRUTH * RAD
        CENLON = CORIENT * RAD
        CENLON = MOD(CENLON,TWOPI)
        IF(CENLON.LT.0. E 0) CENLON = TWOPI + CENLON
        CENLAT = CTRUTH * RAD
        CENLAT = MOD(CENLAT,TWOPI)
        IF(CENLAT.LT.0. E 0) CENLAT = TWOPI + CENLAT
        A2 =  RERTH * COS( TRUTH )
        X=A2*(BLON-CENLON)                                              
        Y=A2*(BLAT-CENLAT)                                              
!
      ENDIF                                                             
!                                                                       
      CX = X                                                            
      CY = Y                                                            
!                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE XY2LL(CPROJ,CORIENT,CTRUTH,CCOTRU,CX,CY,CLAT,CLON)     
!                                                                       
      PARAMETER(PI=3.14159,TWOPI=2.0*PI,HFPI=0.5*PI,QTPI=0.5*HFPI)      
      PARAMETER(RAD=PI/180.,RERTH=6371220.)                             
!                                                                       
! INPUT ARE ALL DEGREE AND OUTPUT TO GLOBAL X Y, NOT DOMAIN X Y         
!                                                                       
! IF PROJ=0  DO MERCATER PROJECTION                                     
! IF PROJ=1  DO NORTH POLAR PROJECTION                                  
! IF PROJ=-1 DO SOUTH POLAR PROJECTION                                  
! IF PROJ=2  DO NORTH LAMBERT PROJECTION                                
! IF PROJ=-2 DO SOUTH LAMBERT PROJECTION                                
!                                                                       
      NPROJ = CPROJ                                                     
      X = CX                                                            
      Y = CY                                                            
!                                                                       
      IF( NPROJ.EQ.1 .OR. NPROJ.EQ.-1 ) THEN                            
! ++++++++++++++++++++++++++++++++++++++                                
! POLAR PROJECTION                                                      
! ++++++++++++++++++++++++++++++++++++++                                
        TRUTH  = CTRUTH * RAD                                           
        TRUTH  = NPROJ * TRUTH                                          
        ORIENT  = CORIENT * RAD                                         
        DLAMDA0 = ORIENT + HFPI                                         
        A2 =  RERTH * ( 1.0 + SIN(TRUTH) )                              
!                                                                       
! =========                                                             
        IF( X.GT.0.0 ) THEN                                             
          BLON = ATAN(Y/X)                                              
        ELSE IF( X.LT.0.0 ) THEN                                        
          BLON = PI + ATAN(Y/X)                                         
        ELSE                                                            
          BLON = HFPI                                                   
          IF( Y.LT.0.0 ) BLON = BLON * 3.0                              
        ENDIF                                                           
        BLON = NPROJ * BLON + DLAMDA0                                   
        BLON = MOD(BLON,TWOPI)                                          
        IF(BLON.LT.0.0) BLON = TWOPI + BLON                             
        RSOA2 = SQRT( X*X + Y*Y )/A2                                    
        BLAT = HFPI - 2. * ATAN(RSOA2)                                  
        BLAT = NPROJ * BLAT                                             
!                                                                       
      ELSE IF ( NPROJ.EQ.0 ) THEN                                       
!                                                                       
! ++++++++++++++++++++++++++++                                          
! DO MERCATER                                                           
! ++++++++++++++++++++++++++++                                          
        TRUTH  = CTRUTH * RAD                                           
        CENLON = CORIENT * RAD                                          
        CENLON = MOD(CENLON,TWOPI)                                      
        IF(CENLON.LT.0. E 0) CENLON = TWOPI + CENLON                    
        A2 =  RERTH * COS( TRUTH )                                      
        DLAMDA0 = 0.0                                                   
!                                                                       
        BLON = X / A2 + CENLON                                          
        BLON = MOD(BLON,TWOPI)                                          
        IF(BLON.LT.0. E 0) BLON = TWOPI + BLON                          
        BLAT = 2.*(ATAN(EXP(Y/A2))-QTPI)                                
!                                                                       
      ELSE IF( NPROJ.EQ.2 .OR. NPROJ.EQ.-2 ) THEN                       
!                                                                       
! ++++++++++++++++++++++++++++                                          
! DO LAMBERT                                                            
! ++++++++++++++++++++++++++++                                          
        IS=1                                                            
        IF( NPROJ.LT.0 ) IS=-1                                          
        TRUTH  = CTRUTH * RAD                                           
        COTRU  = CCOTRU * RAD                                           
        CENLON = CORIENT * RAD                                          
        CENLON = MOD(CENLON,TWOPI)                                      
        IF(CENLON.LT.0. E 0) CENLON = TWOPI + CENLON                    
        IF( CTRUTH.EQ.CCOTRU ) THEN                                     
          CONE= COS (HFPI-IS*TRUTH)                                     
        ELSE                                                            
          CONE=(LOG(COS(TRUTH))-LOG(COS(COTRU)))/                        &
     &         (LOG(TAN(QTPI-IS*TRUTH/2))-LOG(TAN(QTPI-IS*COTRU/2)))    
        ENDIF                                                           
        DLAMDA0 = 0.0                                                   
        R00=RERTH/CONE*COS(TRUTH)/(TAN(QTPI-IS*TRUTH/2))**CONE          
!                                                                       
        R = SQRT( X*X + Y*Y )                                           
        BLON = CENLON + ASIN(X/R) / CONE                                
        BLON = MOD(BLON,TWOPI)                                          
        IF(BLON.LT.0. E 0) BLON = TWOPI + BLON                          
        BLAT = HFPI - 2 * IS * ATAN ( (R/R00)**(1./CONE) )              
!                                                                       
      ELSE IF( NPROJ.EQ.4 ) THEN
        TRUTH  = CTRUTH * RAD
        CENLON = CORIENT * RAD
        CENLON = MOD(CENLON,TWOPI)
        IF(CENLON.LT.0. E 0) CENLON = TWOPI + CENLON
        CENLAT = CTRUTH * RAD
        CENLAT = MOD(CENLAT,TWOPI)
        IF(CENLAT.LT.0. E 0) CENLAT = TWOPI + CENLAT
        A2 =  RERTH * COS( TRUTH )
        BLON = X / A2 + CENLON                                          
        BLON = MOD(BLON,TWOPI)                                          
        IF(BLON.LT.0. E 0) BLON = TWOPI + BLON                          
        BLAT = Y / A2 + CENLAT                                          
        BLAT = MOD(BLAT,TWOPI)                                          
        IF(BLAT.LT.0. E 0) BLAT = TWOPI + BLAT                          
!
      ENDIF
!                                                                       
! OUTPUT IN RADIANS                                                     
      CLAT = BLAT                                                       
      CLON = BLON                                                       
!                                                                       
      RETURN                                                            
      END                                                               
                                                                        
!-----------------------------------------------------------------------
!FPP$ NOCONCUR R                                                        
      SUBROUTINE GRIBIT(F,LBM,IDRT,IGRID,IM,JM,MXBIT,COLAT1,             &    
     &                  ILPDS,IPTV,ICEN,IGEN,IBMS,                       &
     &                  IPU,ITL,IL1,IL2,                                 &
     &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,                &
     &                  INA,INM,ICEN2,IDS,IENS,                          &
     &                  XLAT1,XLON1,XLAT2,XLON2,DELX,DELY,ORI,TRU,PROJ,  &
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
! USAGE:    CALL GRIBIT(F,LBM,IDRT,IGRID,IM,JM,MXBIT,COLAT1,                  
!    &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,      
!    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,               
!    &                  INA,INM,ICEN2,IDS,IENS,                         
!    &                  XLAT1,XLON1,DELX,DELY,ORI,TRU,PROJ,              
!    &                  GRIB,LGRIB,IERR)                                
!   INPUT ARGUMENT LIST:                                                
!     F        - REAL (IM*JM) FIELD DATA TO PACK INTO GRIB MESSAGE      
!     LBM      - LOGICAL (IM*JM) BITMAP TO USE IF IBMS=1                
!     IDRT     - INTEGER DATA REPRESENTATION TYPE                       
!                (0 FOR LATLON OR 4 FOR GAUSSIAN OR 5 FOR POLAR)        
!     IGRID    - grid number
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
!     ORI      - REAL ORIENTATION OF REGIONAL POLAR PROJECTION OR       
!     TRU      - TRUTH FOR REGIONAL MERCATER AND LAMBERT PROJECTION            
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
      INTEGER IBM(IM*JM*IBMS+1-IBMS),IPDS(100),IGDS(100),IBDS(100)      
      REAL FR(IM*JM)                                                    
      CHARACTER PDS(ILPDS)                                              
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  DETERMINE GRID PARAMETERS                                            
      PI=ACOS(-1.)                                                      
      NF=IM*JM                                                          
      IF(IDRT.EQ.0) THEN                                                
        IF(IM.EQ.144.AND.JM.EQ.73) THEN                                 
          IGRID=2                                                       
        ELSEIF(IM.EQ.360.AND.JM.EQ.181) THEN                            
          IGRID=3                                                       
!       ELSE                                                            
!         IGRID=255                                                     
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
! change to regional latlon        
        LAT1=NINT(180.E3/ACOS(-1.)*XLAT1)
        LON1=NINT(180.E3/ACOS(-1.)*XLON1)
        IGDS09=NINT(180.E3/ACOS(-1.)*XLAT2)
        IGDS10=NINT(180.E3/ACOS(-1.)*XLON2)
        IGDS11=(IGDS09-LAT1)/(JM-1)
        IGDS12=(IGDS10-LON1)/(IM-1)
        IGDS13=64
!
        IGDS14=0
        IGDS15=0
        IGDS16=0
        IGDS17=0
        IGDS18=0
      ELSEIF(IDRT.EQ.4) THEN                                            
        IF(IM.EQ.192.AND.JM.EQ.94) THEN                                 
          IGRID=98                                                      
        ELSEIF(IM.EQ.384.AND.JM.EQ.190) THEN                            
          IGRID=126                                                     
!       ELSE                                                            
!         IGRID=255                                                     
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
        IGDS14=0
        IGDS15=0
        IGDS16=0
        IGDS17=0
        IGDS18=0
      ELSEIF(IDRT.EQ.5) THEN    ! POLAR PROJECTION                      
!       print*,'POLAR PROJECTION'
!       print*,'PROJ=',PROJ
!       IGRID=255                                                       
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)                             
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)                             
        IRESFL=8                                                        
        IGDS09=NINT(ORI*1.E3)                                        
        IGDS10=DELX                                                     
        IGDS11=DELY                                                     
        IGDS12=0
        IF( NINT(PROJ).EQ.1  ) IGDS12=0         ! NORTH POLAR PROJ      
        IF( NINT(PROJ).EQ.-1 ) IGDS12=128       ! SOUTH POLAT PROJ      
        ISCAN=64                                                        
        IGDS13=ISCAN                                                    
        IGDS14=0
        IGDS15=0
        IGDS16=0
        IGDS17=0
        IGDS18=0
      ELSEIF(IDRT.EQ.1) THEN    ! MERCATER PROJECTION                   
!     print*,'MERCATER PROJECTION'
!     print*,'PROJ=',PROJ
!       IGRID=255                                                       
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)                             
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)                             
        IRESFL=8                                                        
        IGDS09=NINT(180.E3/ACOS(-1.) * XLAT2)                           
        IGDS10=NINT(180.E3/ACOS(-1.) * XLON2)                           
        IGDS11=DELX                                                     
        IGDS12=DELY                                                     
        IGDS13=NINT(TRU*1.E3)                                        
        ISCAN=64                                                        
        IGDS14=ISCAN                                                    
        IGDS15=0
        IGDS16=0
        IGDS17=0
        IGDS18=0
      ELSEIF(IDRT.EQ.3) THEN    ! LAMBERT PROJECTION                      
!     print*,'LAMBERT PROJECTION'
!     print*,'PROJ=',PROJ
!       IGRID=255                                                       
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)                             
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)                             
!      print*,'LAT1,LON1=',LAT1,LON1
        IRESFL=8                                                        
        IGDS09=NINT(ORI*1.E3)                                        
!      print*,'IGDS09=',IGDS09
        IGDS10=DELX                                                     
        IGDS11=DELY                                                     
!      print*,'IGDS10,IGDS11=',IGDS10,IGDS11
        IGDS12=0
        IF( NINT(PROJ).EQ.2  ) IGDS12=0         ! NORTH LAMBERT PROJ      
        IF( NINT(PROJ).EQ.-2 ) IGDS12=128       ! SOUTH LAMBERT PROJ      
!      print*,'IGDS12=',IGDS12
        ISCAN=64                                                        
        IGDS13=ISCAN                                                    
        IGDS14=0
        IGDS15=NINT(TRU*1.E3)
        IGDS16=IGDS15
!      print*,'IGDS15,IGDS16=',IGDS15,IGDS16
!       IGDS17=-90000    !lat of South Pole (MILLIDEGREES)
        IGDS17=0         !???????
        IGDS18=0         !lon of South Pole (MILLIDEGREES)
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
      IPDS(06)=1        ! GDS FLAG, USUALLY 1                           
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
      IPDS(23)=ICY      ! CENTURY                                       
      IPDS(24)=ICEN2    ! FORECAST SUBCENTER                            
      IPDS(25)=IDS      ! DECIMAL SCALING                               
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  FILL GDS AND BDS PARAMETERS                                          
      IGDS(01)=0        ! NUMBER OF VERTICAL COORDS                     
      IGDS(02)=IGRID    ! VERTICAL COORD FLAG                           
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
      IGDS(15)=IGDS15  
      IGDS(16)=IGDS16  
      IGDS(17)=IGDS17   
      IGDS(18)=IGDS18  
      IGDS(27)=0  
      IBDS(1:9)=0       ! BDS FLAGS                                     
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

! FR(i) is packed.
!     write(6,*) (FR(i),i=1,10)
!     write(6,*) 'NBIT= ',NBIT

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  CREATE PRODUCT DEFINITION SECTION                                    
      CALL W3FI68(IPDS,PDS)                                             
      IF(ICEN2.EQ.2.AND.ILPDS.GE.45) THEN                               
        ILAST=45                                                        
        CALL PDSENS(IENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)           
      ENDIF                                                             
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
        IDS(001)=0      ! PRESSURE (PA)
        IDS(002)=0      ! SEA-LEVEL PRESSURE (PA)
        IDS(003)=3      ! PRESSURE TENDENCY (PA/S)
                        !
                        !
        IDS(006)=0      ! GEOPOTENTIAL (M2/S2)
        IDS(007)=1      ! GEOPOTENTIAL HEIGHT (M)
        IDS(008)=1      ! GEOMETRIC HEIGHT (M)
        IDS(009)=1      ! STANDARD DEVIATION OF HEIGHT (M)
        IDS(010)=1      ! TOTAL OZONE (DOBSON)
        IDS(011)=1      ! TEMPERATURE (K)
        IDS(012)=1      ! VIRTUAL TEMPERATURE (K)
        IDS(013)=1      ! POTENTIAL TEMPERATURE (K)
        IDS(014)=1      ! PSEUDO-ADIABATIC POTENTIAL TEMPERATURE (K)
        IDS(015)=1      ! MAXIMUM TEMPERATURE (K)
        IDS(016)=1      ! MINIMUM TEMPERATURE (K)
        IDS(017)=1      ! DEWPOINT TEMPERATURE (K)
        IDS(018)=1      ! DEWPOINT DEPRESSION (K)
        IDS(019)=4      ! TEMPERATURE LAPSE RATE (K/M)
        IDS(020)=0      ! VISIBILITY (M)
        IDS(021)=0      ! RADAR SPECTRA 1 ()
        IDS(022)=0      ! RADAR SPECTRA 2 ()
        IDS(023)=0      ! RADAR SPECTRA 3 ()
        IDS(024)=0      ! HYDROSTATIC TEMPERATURE (K)
        IDS(025)=1      ! TEMPERATURE ANOMALY (K)
        IDS(026)=0      ! PRESSURE ANOMALY (PA)
        IDS(027)=1      ! GEOPOTENTIAL HEIGHT ANOMALY (M)
        IDS(028)=1      ! WAVE SPECTRA 1 ()
        IDS(029)=1      ! WAVE SPECTRA 2 ()
        IDS(030)=1      ! WAVE SPECTRA 3 ()
        IDS(031)=0      ! WIND DIRECTION (DEGREES)
        IDS(032)=1      ! WIND SPEED (M/S)
        IDS(033)=1      ! ZONAL WIND (M/S)
        IDS(034)=1      ! MERIDIONAL WIND (M/S)
        IDS(035)=-4     ! STREAMFUNCTION (M2/S)
        IDS(036)=-4     ! VELOCITY POTENTIAL (M2/S)
        IDS(037)=-1     ! MONTGOMERY STREAM FUNCTION (M2/S2)
        IDS(038)=8      ! SIGMA VERTICAL VELOCITY (1/S)
        IDS(039)=3      ! PRESSURE VERTICAL VELOCITY (PA/S)
        IDS(040)=4      ! GEOMETRIC VERTICAL VELOCITY (M/S)
        IDS(041)=6      ! ABSOLUTE VORTICITY (1/S)
        IDS(042)=6      ! ABSOLUTE DIVERGENCE (1/S)
        IDS(043)=6      ! RELATIVE VORTICITY (1/S)
        IDS(044)=6      ! RELATIVE DIVERGENCE (1/S)
        IDS(045)=4      ! VERTICAL U SHEAR (1/S)
        IDS(046)=4      ! VERTICAL V SHEAR (1/S)
        IDS(047)=0      ! DIRECTION OF CURRENT (DEGREES)
        IDS(048)=0      ! SPEED OF CURRENT (M/S)
        IDS(049)=0      ! U OF CURRENT (M/S)
        IDS(050)=0      ! V OF CURRENT (M/S)
        IDS(051)=5      ! SPECIFIC HUMIDITY (KG/KG)
        IDS(052)=0      ! RELATIVE HUMIDITY (PERCENT)
        IDS(053)=5      ! HUMIDITY MIXING RATIO (KG/KG)
        IDS(054)=1      ! PRECIPITABLE WATER (KG/M2)
        IDS(055)=0      ! VAPOR PRESSURE (PA)
        IDS(056)=0      ! SATURATION DEFICIT (PA)
        IDS(057)=1      ! EVAPORATION (KG/M2)
        IDS(058)=1      ! CLOUD ICE (KG/M2)
        IDS(059)=6      ! PRECIPITATION RATE (KG/M2/S)
        IDS(060)=0      ! THUNDERSTORM PROBABILITY (PERCENT)
        IDS(061)=1      ! TOTAL PRECIPITATION (KG/M2)
        IDS(062)=1      ! LARGE-SCALE PRECIPITATION (KG/M2)
        IDS(063)=1      ! CONVECTIVE PRECIPITATION (KG/M2)
        IDS(064)=6      ! WATER EQUIVALENT SNOWFALL RATE (KG/M2/S)
        IDS(065)=0      ! WATER EQUIVALENT OF SNOW DEPTH (KG/M2)
        IDS(066)=2      ! SNOW DEPTH (M)
        IDS(067)=2      ! MIXED-LAYER DEPTH (M)
        IDS(068)=2      ! TRANSIENT THERMOCLINE DEPTH (M)
        IDS(069)=2      ! MAIN THERMOCLINE DEPTH (M)
        IDS(070)=2      ! MAIN THERMOCLINE ANOMALY (M)
        IDS(071)=0      ! TOTAL CLOUD COVER (PERCENT)
        IDS(072)=0      ! CONVECTIVE CLOUD COVER (PERCENT)
        IDS(073)=0      ! LOW CLOUD COVER (PERCENT)
        IDS(074)=0      ! MIDDLE CLOUD COVER (PERCENT)
        IDS(075)=0      ! HIGH CLOUD COVER (PERCENT)
        IDS(076)=2      ! CLOUD WATER (KG/M2)
                        !
        IDS(078)=1      ! CONVECTIVE SNOW (KG/M2)
        IDS(079)=1      ! LARGE SCALE SNOW (KG/M2)
        IDS(080)=1      ! WATER TEMPERATURE (K)
        IDS(081)=0      ! SEA-LAND MASK ()
        IDS(082)=2      ! DEVIATION OF SEA LEVEL FROM MEAN (M)
        IDS(083)=5      ! ROUGHNESS (M)
        IDS(084)=1      ! ALBEDO (PERCENT)
        IDS(085)=1      ! SOIL TEMPERATURE (K)
        IDS(086)=0      ! SOIL WETNESS (KG/M2)
        IDS(087)=0      ! VEGETATION (PERCENT)
        IDS(088)=0      ! SALINITY (KG/KG)
        IDS(089)=4      ! DENSITY (KG/M3)
        IDS(090)=1      ! RUNOFF (KG/M2)
        IDS(091)=0      ! ICE CONCENTRATION ()
        IDS(092)=0      ! ICE THICKNESS (M)
        IDS(093)=0      ! DIRECTION OF ICE DRIFT (DEGREES)
        IDS(094)=0      ! SPEED OF ICE DRIFT (M/S)
        IDS(095)=0      ! U OF ICE DRIFT (M/S)
        IDS(096)=0      ! V OF ICE DRIFT (M/S)
        IDS(097)=0      ! ICE GROWTH (M)
        IDS(098)=0      ! ICE DIVERGENCE (1/S)
        IDS(099)=1      ! SNOW MELT (KG/M2)
        IDS(100)=2      ! SIG HEIGHT OF WAVES AND SWELL (M)
        IDS(101)=0      ! DIRECTION OF WIND WAVES (DEGREES)
        IDS(102)=2      ! SIG HEIGHT OF WIND WAVES (M)
        IDS(103)=2      ! MEAN PERIOD OF WIND WAVES (S)
        IDS(104)=0      ! DIRECTION OF SWELL WAVES (DEGREES)
        IDS(105)=2      ! SIG HEIGHT OF SWELL WAVES (M)
        IDS(106)=2      ! MEAN PERIOD OF SWELL WAVES (S)
        IDS(107)=0      ! PRIMARY WAVE DIRECTION (DEGREES)
        IDS(108)=2      ! PRIMARY WAVE MEAN PERIOD (S)
        IDS(109)=0      ! SECONDARY WAVE DIRECTION (DEGREES)
        IDS(110)=2      ! SECONDARY WAVE MEAN PERIOD (S)
        IDS(111)=0      ! NET SOLAR RADIATIVE FLUX AT SURFACE (W/M2)
        IDS(112)=0      ! NET LONGWAVE RADIATIVE FLUX AT SURFACE (W/M2)
        IDS(113)=0      ! NET SOLAR RADIATIVE FLUX AT TOP (W/M2)
        IDS(114)=0      ! NET LONGWAVE RADIATIVE FLUX AT TOP (W/M2)
        IDS(115)=0      ! NET LONGWAVE RADIATIVE FLUX (W/M2)
        IDS(116)=0      ! NET SOLAR RADIATIVE FLUX (W/M2)
        IDS(117)=0      ! TOTAL RADIATIVE FLUX (W/M2)
        IDS(118)=5      ! MAXIMUM SPECIFIC HUMIDITY (KG/KG)
        IDS(119)=5      ! MINIMUM SPECIFIC HUMIDITY (KG/KG)
                        !
                        !
                        !
        IDS(121)=0      ! LATENT HEAT FLUX (W/M2)
        IDS(122)=0      ! SENSIBLE HEAT FLUX (W/M2)
        IDS(123)=0      ! BOUNDARY LAYER DISSIPATION (W/M2)
        IDS(124)=3      ! U WIND STRESS (N/M2)
        IDS(125)=3      ! V WIND STRESS (N/M2)
        IDS(126)=3      ! WIND MIXING ENERGY (J)
        IDS(127)=3      ! IMMAGE DATA ()
        IDS(128)=0      ! MEAN SEA-LEVEL PRESSURE (STDATM) (PA)
        IDS(129)=0      ! MEAN SEA-LEVEL PRESSURE (MAPS) (PA)
        IDS(130)=0      ! MEAN SEA-LEVEL PRESSURE (ETA) (PA)
        IDS(131)=1      ! SURFACE LIFTED INDEX (K)
        IDS(132)=1      ! BEST LIFTED INDEX (K)
        IDS(133)=1      ! K INDEX (K)
        IDS(134)=1      ! SWEAT INDEX (K)
        IDS(135)=10     ! HORIZONTAL MOISTURE DIVERGENCE (KG/KG/S)
        IDS(136)=4      ! SPEED SHEAR (1/S)
        IDS(137)=3      ! 3-HR PRESSURE TENDENCY (PA/S)
        IDS(138)=6      ! BRUNT-VAISALA FREQUENCY SQUARED (1/S2)
        IDS(139)=11     ! POTENTIAL VORTICITY (MASS-WEIGHTED) (1/S/M)
        IDS(140)=0      ! RAIN MASK ()
        IDS(141)=0      ! FREEZING RAIN MASK ()
        IDS(142)=0      ! ICE PELLETS MASK ()
        IDS(143)=0      ! SNOW MASK ()
        IDS(144)=3      ! VOLUMETRIC SOIL MOISTURE CONTENT (FRACTION)
        IDS(145)=0      ! POTENTIAL EVAPORATION RATE (W/M2)
        IDS(146)=0      ! CLOUD WORKFUNCTION (J/KG)
        IDS(147)=3      ! U GRAVITY WAVE STRESS (N/M2)
        IDS(148)=3      ! V GRAVITY WAVE STRESS (N/M2)
        IDS(149)=10     ! POTENTIAL VORTICITY (M2/S/KG)
        IDS(150)=10     ! COVARIANCE BETWEEN V AND U (M2/S2)
        IDS(151)=10     ! COVARIANCE BETWEEN U AND T (K*M/S)
        IDS(152)=10     ! COVARIANCE BETWEEN V AND T (K*M/S)
        IDS(153)=6      ! CLOUD WATER MIXING RATIO (KG/KG)
        IDS(154)=9      ! OZONE MIXING RATIO (KG/KG)
        IDS(155)=0      ! GROUND HEAT FLUX (W/M2)
        IDS(156)=0      ! CONVECTIVE INHIBITION (J/KG)
        IDS(157)=0      ! CONVECTIVE APE (J/KG)
        IDS(158)=0      ! TURBULENT KE (J/KG)
        IDS(159)=0      ! CONDENSATION PRESSURE OF LIFTED PARCEL (PA)
        IDS(160)=0      ! CLEAR SKY UPWARD SOLAR FLUX (W/M2)
        IDS(161)=0      ! CLEAR SKY DOWNWARD SOLAR FLUX (W/M2)
        IDS(162)=0      ! CLEAR SKY UPWARD LONGWAVE FLUX (W/M2)
        IDS(163)=0      ! CLEAR SKY DOWNWARD LONGWAVE FLUX (W/M2)
        IDS(164)=0      ! CLOUD FORCING NET SOLAR FLUX (W/M2)
        IDS(165)=2      ! CLOUD FORCING NET LONGWAVE FLUX (W/M2)
        IDS(166)=0      ! VISIBLE BEAM DOWNWARD SOLAR FLUX (W/M2)
        IDS(167)=0      ! VISIBLE DIFFUSE DOWNWARD SOLAR FLUX (W/M2)
        IDS(168)=0      ! NEAR IR BEAM DOWNWARD SOLAR FLUX (W/M2)
        IDS(169)=0      ! NEAR IR DIFFUSE DOWNWARD SOLAR FLUX (W/M2)
                        !
                        !
        IDS(172)=3      ! MOMENTUM FLUX (N/M2)
        IDS(173)=0      ! MASS POINT MODEL SURFACE ()
        IDS(174)=0      ! VELOCITY POINT MODEL SURFACE ()
        IDS(175)=0      ! SIGMA LAYER NUMBER ()
        IDS(176)=2      ! LATITUDE (DEGREES)
        IDS(177)=2      ! EAST LONGITUDE (DEGREES)
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
        IDS(204)=0      ! DOWNWARD SOLAR RADIATIVE FLUX (W/M2)
        IDS(205)=0      ! DOWNWARD LONGWAVE RADIATIVE FLUX (W/M2)
                        !
        IDS(207)=0      ! MOISTURE AVAILABILITY (PERCENT)
        IDS(208)=0      ! EXCHANGE COEFFICIENT (KG/M2/S)
        IDS(209)=0      ! NUMBER OF MIXED LAYER NEXT TO SFC ()
                        !
        IDS(211)=0      ! UPWARD SOLAR RADIATIVE FLUX (W/M2)
        IDS(212)=0      ! UPWARD LONGWAVE RADIATIVE FLUX (W/M2)
        IDS(213)=0      ! NON-CONVECTIVE CLOUD COVER (PERCENT)
        IDS(214)=6      ! CONVECTIVE PRECIPITATION RATE (KG/M2/S)
        IDS(215)=7      ! TOTAL DIABATIC HEATING RATE (K/S)
        IDS(216)=7      ! TOTAL RADIATIVE HEATING RATE (K/S)
        IDS(217)=7      ! TOTAL DIABATIC NONRADIATIVE HEATING RATE (K/S)
        IDS(218)=2      ! PRECIPITATION INDEX (FRACTION)
        IDS(219)=1      ! STD DEV OF IR T OVER 1X1 DEG AREA (K)
        IDS(220)=4      ! NATURAL LOG OF SURFACE PRESSURE OVER 1 KPA ()
        IDS(221)=1      ! PLANETARY BOUNDARY LAYER HEIGHT (M)
        IDS(222)=1      ! 5-WAVE GEOPOTENTIAL HEIGHT (M)
        IDS(223)=1      ! PLANT CANOPY SURFACE WATER (KG/M2)
                        !
                        !
        IDS(226)=3      ! BLACKADARS MIXING LENGTH (M)
        IDS(227)=3      ! ASYMPTOTIC MIXING LENGTH (M)
        IDS(228)=1      ! POTENTIAL EVAPORATION (KG/M2)
        IDS(229)=0      ! SNOW PHASE-CHANGE HEAT FLUX (W/M2)
                        !
        IDS(231)=3      ! CONVECTIVE CLOUD MASS FLUX (PA/S)
        IDS(232)=0      ! DOWNWARD TOTAL RADIATION FLUX (W/M2)
        IDS(233)=0      ! UPWARD TOTAL RADIATION FLUX (W/M2)
        IDS(234)=1      ! BASEFLOW-GROUNDWATER RUNOFF (KG/M2)
        IDS(235)=1      ! STORM SURFACE RUNOFF (KG/M2)
                        !
        IDS(237)=6      ! TOTAL OZONE (KG/M2)
        IDS(238)=0      ! SNOW COVER (PERCENT)
        IDS(239)=1      ! SNOW TEMPERATURE (K)
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

!FPP$ NOCONCUR R
      SUBROUTINE PDSENS(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,MSGA)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    PDSENS.F    PACKS GRIB PDS EXTENSION 41- FOR ENSEMBLE
!   PRGMMR: ZOLTAN TOTH      ORG: W/NMC20    DATE: 95-03-14
!
! ABSTRACT: PACKS BRIB PDS EXTENSION STARTING ON BYTE 41 FOR ENSEMBLE
!       FORECAST PRODUCTS. FOR FORMAT OF PDS EXTENSION, SEE NMC OFFICE NOTE 38
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
!       TESTING GRIB EXTENSION 41- PACKER AND UNPACKER SUBROUTINES
!
      INTEGER KENS(5),KPROB(2),KCLUST(16),KMEMBR(80)
      DIMENSION XPROB(2)
      CHARACTER*1 MSGA(100)
      IF(ILAST.LT.41) THEN
      PRINT*,'WARNING - SUBROUTINE IS FOR PACKING BYTES 41 AND ABOVE'
          GO TO 333
          ENDIF
!       PACKING IS DONE IN FOUR SECTIONS ENDING AT BYTE IL
          IF(ILAST.GE.41) IL=45
          IF(ILAST.GE.46) IL=55
          IF(ILAST.GE.61) IL=76
          IF(ILAST.GE.77) IL=86
!       CHANGING THE NUMBER OF BYTES (FIRST THREE BYTES IN PDS)
          CALL SBYTE(MSGA, IL, 0,24)
!       PACKING FIRST SECTION (GENERAL INTORMATION SECTION)
      IF(IL.GE.45) CALL SBYTES(MSGA,KENS,40*8,8,0,5)
!       PACKING 2ND SECTION (PROBABILITY SECTION)
      IF(IL.GE.55) THEN
          CALL SBYTES(MSGA,KPROB,45*8,8,0,2)
          CALL W3FI01(LW)
          CALL W3FI76(XPROB(1),IEXP,IMANT,8*LW)
          CALL SBYTE(MSGA,IEXP,47*8,8)
          CALL SBYTE(MSGA,IMANT,48*8,24)
          CALL W3FI76(XPROB(2),IEXP,IMANT,8*LW)
          CALL SBYTE(MSGA,IEXP,51*8,8)
          CALL SBYTE(MSGA,IMANT,52*8,24)
      ENDIF
!       PACKING 3RD SECTION (CLUSTERING INFORMATION)
      IF(IL.GE.76) CALL SBYTES(MSGA,KCLUST,60*8,8,0,16)
!       PACKING 4TH SECTION (CLUSTER MEMBERSHIP)
      IF(IL.GE.86) CALL SBYTES(MSGA,KMEMBR,76*8,1,0,10)
!
333   CONTINUE
          RETURN
          END
      SUBROUTINE CTLHEAD(NN,IM,JM,KM,PROJ,TRU,ORI,                       &
     &                  CLAT,CLON,PI,PJ,DX,DY,                           &
     &                  IHR,IDAY,IMON,IYR,IFH,                           &
     &                  STRLON,DLON,STRLAT,DLAT,                         &
     &                  RLAT,IZ)                                        
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
      IF( PROJ.EQ.8 ) THEN
        WRITE(NN,123)
      ELSE
        WRITE(NN,103)
      ENDIF
      WRITE(NN,104)                                                     
      WRITE(NN,105)                                                     
      WRITE(NN,106)                                                     
 101  FORMAT('dset DATAFILE')                                           
 102  FORMAT('dtype grib')                                              
 103  FORMAT('options template')                                        
 104  FORMAT('index MAPFILE')                                           
 105  FORMAT('undef -9.99E+33')                                         
 106  FORMAT('title EXP1')                                              
 123  FORMAT('options yrev template')
!                                                                       
      IF( PROJ.EQ.0.0 ) THEN                                            
        WRITE(NN,108) IM,STRLON,DLON                                    
        WRITE(NN,110) JM                                                
        WRITE(NN,111) (RLAT(J),J=1,JM)                                  
      ELSE IF( PROJ.EQ.1.0 .OR. PROJ.EQ.-1.0 ) THEN
        WRITE(NN,107) IM,JM,PI,PJ,ORI,DX                                 
        IMP=360.*111/DX                                                 
        STRLONP=0.0                                                     
        DLONP=360./IMP                                                  
        JMP=IMP/4                                                       
        STRLATP=0.0                                                     
        DLATP=DLONP                                                     
        WRITE(NN,108) IMP,STRLONP,DLONP                                 
        WRITE(NN,109) JMP,STRLATP,DLATP                                 
      ELSE IF( PROJ.EQ.2.0 .OR. PROJ.EQ.-2.0 ) THEN
        WRITE(NN,117) IM,JM,CLAT,CLON,PI,PJ,TRU,TRU,ORI,DX,DY
        IMP=360.*111/DX
        STRLONP=0.0
        DLONP=360./IMP
        JMP=IMP/4
        STRLATP=0.0
        DLATP=DLONP
        WRITE(NN,108) IMP,STRLONP,DLONP
        WRITE(NN,109) JMP,STRLATP,DLATP
      ELSE
        WRITE(NN,108) IM,STRLON,DLON
        WRITE(NN,109) JM,STRLAT,DLAT
      ENDIF                                                             
 107  FORMAT('pdef',2I5,' nps',4G14.6)                                   
 108  FORMAT('xdef',I5,' linear',2G14.6)                                 
 109  FORMAT('ydef',I5,' linear',2G14.6)                                 
 110  FORMAT('ydef',I5,' levels')                                       
 111  FORMAT(5G14.6)                                                    
 117  FORMAT('pdef',2I5,' lcc',9G14.6)
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
      IF( IYR.LT.100 ) IYR=2050-MOD(2050-IYR,100)
      WRITE(NN,114) HOUR,DAY,MON(IMON),IYR,IFH                          
 112  FORMAT('zdef',I5,' levels')                                       
 113  FORMAT(10I8)                                                      
 114  FORMAT('tdef 99 linear ',A2,'Z',A2,A3,I4,I10,'hr')                
      WRITE(NN,115)                                                     
 115  FORMAT('vars TOTALNUM')                                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CTLEND(NN)                                             
      WRITE(NN,100)                                                     
 100  FORMAT('endvars')                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE CTLVAR(NN,NUMVAR,NUMSFC)                               
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
          WRITE(NNS,200) VARWRT,                                         &
     &                NUMS,NUMVARS,NUMSFCS,DSL(NUMVARS)                 
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
       VAR(118)='q-max' 
       VAR(119)='q-min' 
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
       VAR(153)='clwmr' 
       VAR(154)='o3mr-' 
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
		!     '                                                               
       VAR(191)='qci--'
       VAR(192)='qrs--'
       VAR(193)='q-c--'
       VAR(194)='q-i--'
       VAR(195)='q-r--'
       VAR(196)='q-s--'
       VAR(197)='q-g--'
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
!               !12345678901234567890123456789012345678901234567890     
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
       DSL(010)='TOTAL OZONE (DOBSON)                              '
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
       DSL(118)='MAXIMUM SPECFIC HUMIDITY(KG/KG)                   '    
       DSL(119)='MINIMUM SPECFIC HUMIDITY(KG/KG)                   '    
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
       DSL(153)='Cloud water mixing ratio [kg/kg]                  '    
       DSL(154)='Ozone mixing ratio [kg/kg]                        '    
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
       DSL(191)='mixing ratio of cloud water with ice (kg/kg)      '
       DSL(192)='mixing ratio of rain water with snow (kg/kg)      '
       DSL(193)='mixing ratio of cloud water (kg/kg)               '
       DSL(194)='mixing ratio of cloud ice (kg/kg)                 '
       DSL(195)='mixing ratio of rain (kg/kg)                      '
       DSL(196)='mixing ratio of snow (kg/kg)                      '
       DSL(197)='mixing ratio of groupel (kg/kg)                   '
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
      FUNCTION ISRCHEQ(N,A,IS,VAL)
      DIMENSION A(N)
      ISRCHEQ=N+1
      DO NN=IS,N
      IF( A(NN).EQ.VAL ) THEN
        ISRCHEQ=NN
        RETURN
      ENDIF
      ENDDO
      RETURN
      END
      FUNCTION ISRCHNE(N,A,IS,VAL)
      DIMENSION A(N)
      ISRCHNE=N+1
      DO NN=IS,N
      IF( A(NN).NE.VAL ) THEN
        ISRCHNE=NN
        RETURN
      ENDIF
      ENDDO
      RETURN
      END
      FUNCTION ISRCHFLT(N,A,IS,VAL)
      DIMENSION A(N)
      ISRCHFLT=N+1
      DO NN=IS,N
      IF( A(NN).LT.VAL ) THEN
        ISRCHFLT=NN
        RETURN
      ENDIF
      ENDDO
      RETURN
      END
      FUNCTION ISRCHFGT(N,A,IS,VAL)
      DIMENSION A(N)
      ISRCHFGT=N+1
      DO NN=IS,N 
      IF( A(NN).GT.VAL ) THEN
        ISRCHFGT=NN
        RETURN
      ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE SGL2FUL(S,F,L)
      REAL* 4  S(L)
      DIMENSION F(L)
      DO I=1,L
        F(I)=S(I)
      ENDDO
      RETURN
      END
      SUBROUTINE FUL2SGL(F,S,L)
      REAL* 4  S(L)
      DIMENSION F(L)
      DO I=1,L
        S(I)=F(I)
      ENDDO
      RETURN
      END

      SUBROUTINE WRYTE(N,LC,C)
      CHARACTER*1 C(LC)
      WRITE(N) (C(I),I=1,LC)
      RETURN
      END

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine calwxt1(lm,prec,t,q,p,pint,iwx)
!$$$  Subprogram documentation block
!
! Subprogram: calwxt1    Compute precipitation type
!   Prgmmr: Baldwin      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram computes precipitation type using
!           a decision tree approach that uses variables
!           such as integrated wet bulb temperature below freezing
!           and lowest layer temperature.
!           For more details, see Baldwin and Contorno preprint
!           from 13th Weather Analysis and Forecasting Conference
!           (or Baldwin et al. 10th NWP Conference preprint).
!
! Program history log:
!   1993-11-11  Baldwin
!   1994-09-30  Baldwin       set up new decision tree
!   1995-03-27  Iredell       modularized and vectorized
!   1999-10-18  Mark Iredell  fortran 90
!   2001-09-04  Juang and Du  adopted to the RSM model
!
! Usage:  call calwxt1(lm,prec,t,q,p,pint,iwx)
!   Input argument list:
!     lm           integer number of levels in this profile
!     prec         real precipitation (mm?)
!     t            real (lm) temperature (from top) (k)
!     q            real (lm) specific humidity (from top) (kg/kg)
!     p            real (lm) pressure (from top) (pa)
!     pint         real (lm+1) interface pressure (from top) (pa)
!   output arguments:
!     iwx          integer instantaneous weather type
!                  (the one's digit is for snow;
!                   the two's digit is for ice pellets;
!                   the four's digit is for freezing rain;
!                   the eight's digit is for rain.)
!
! Subprograms called:
!   (ftdp)       compute dewpoint temperature
!   (ftlcl)      compute lcl temperature
!   (fthe)       compute equivalent potential temperature
!   (ftma)       compute moist adiabat temperature
!   (stma)       compute moist adiabat temperature
!
! Remarks: Weather type is only computed where there is precipitation
!          (precip rate is used).
!          All profiles must start at the top and go down.
!          For efficiency, inline all function calls.
!
! Attributes:
!   Language: Fortran 90
!
!$$$

!     implicit none
      integer,intent(in):: lm
      real,intent(in):: prec,t(lm),q(lm),p(lm),pint(lm+1)
      integer,intent(out):: iwx
      integer lice,l,l150,lfrz,lwrm,lone
      real tcold,twarm,tdchk
      real psm150,tv,dz150,surfw,surfc
      real dz(lm)
!     real pv,pr,tdpd,tr,pk,tlcl,thelcl,twet(lm),qwet,
      real*8 pv,pr,tdpd,tr,pk,tlcl,thelcl,twet(lm),qwet,                 &
     &areap4,areas8
      real con_t0c,con_rd,con_rv,con_g,con_rog,con_eps,                  &
     &con_epsm1,con_fvirt

      con_t0c=2.7315e+2          ! temp at 0C
      con_rd=2.8705e+2           ! gas constant air
      con_rv=4.6150e+2           ! gas constant H2O
      con_g =9.8000e+0           ! gravity 
      con_rog=con_rd/con_g 
      con_eps=con_rd/con_rv
      con_epsm1=con_rd/con_rv-1.
      con_fvirt=con_rv/con_rd-1.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Initialize the weather type.
      iwx=0
!  If there is precipitation, find the coldest and warmest temperatures
!  in the saturated layer between 70 mb above ground and 500 mb.
!  Also find highest saturated layer in that range.
      if(prec.gt.0.) then
        tcold=t(lm)
        twarm=t(lm)
        lice=lm
        tdchk=2.
!  Try to find first 2 then 4 then 6 degree dewpoint depressions.
      do while(tcold.eq.t(lm).and.tdchk.le.6)
        do l=1,lm
          if(p(l).ge.500.e2.and.p(l).le.pint(lm+1)-70.e2) then
!         print*,'ha, ha.... we get here!'
            pv=p(l)*q(l)/(con_eps-con_epsm1*q(l))
            tdpd=t(l)-ftdp(pv)
            if(tdpd.lt.tdchk) then
              tcold=min(tcold,t(l))
              twarm=max(twarm,t(l))
              lice=min(lice,l)
            endif
          endif
        enddo
        tdchk=tdchk+2
      enddo
!  Decide weather type if there is no cold saturated air.
      if(tcold.gt.con_t0c-4) then
!  Decide freezing rain if the surface is cold.
        if(t(lm).lt.con_t0c) then
          iwx=iwx+4
!  Otherwise decide rain.
        else
          iwx=iwx+8
        endif
!  If there is cold saturated air, then find lowest 150 mb level
!  and the lowest freezing level and warm level.
      else
        psm150=pint(lm)-150.e2
        l150=1
        lfrz=1
        lwrm=1
        do l=1,lm
          if(pint(l+1).le.psm150) l150=l+1
          if(t(l).lt.con_t0c) lfrz=l+1
          if(t(l).ge.twarm) lwrm=l+1
        enddo
!  Compute layer thickness and wet bulb temperature where needed.
        lone=min(lice,l150,lfrz,lwrm)
        do l=lone,lm
          if(pint(l).gt.0) then
            tv=t(l)*(1+con_fvirt*q(l))
            dz(l)=con_rog*tv*log(pint(l+1)/pint(l))
          else
            dz(l)=0
          endif
          pv=p(l)*q(l)/(con_eps-con_epsm1*q(l))
          tdpd=t(l)-ftdp(pv)
          if(tdpd.gt.0.) then
            pr=p(l)
            tr=t(l)
            pk=fpkap(pr/1000.0)
            tlcl=ftlcl(tr,tdpd)
            thelcl=fthe(tlcl,pk*tlcl/tr)
!           call stma(thelcl,pk,twet(l),qwet)
            twet(l)=ftma(thelcl,pk,qwet)
          else
            twet(l)=t(l)
          endif
        enddo
!  Integrate area of twet above -4c below highest saturated layer.
        areap4=0
        do l=lice,lm
          if(twet(l).gt.con_t0c-4) then
            areap4=areap4+(twet(l)-(con_t0c-4))*dz(l)
          endif
        enddo
!  Decide snow if there is scarce warm air.
        if(areap4.lt.3000.) then
          iwx=iwx+1
        else
!  Otherwise integrate net area of twet w.r.t. 0c in lowest 150 mb.
          l=l150
          tv=t(l)*(1+con_fvirt*q(l))
          dz150=con_rog*tv*log(pint(l+1)/psm150)
          areas8=(twet(l)-con_t0c)*dz150
          do l=l150+1,lm
            areas8=areas8+(twet(l)-con_t0c)*dz(l)
          enddo
!  Integrate area of twet above 0c below the freezing level.
          surfw=0
          do l=lfrz,lm
            if(twet(l).gt.con_t0c) then
              surfw=surfw+(twet(l)-con_t0c)*dz(l)
            endif
          enddo
!  Integrate area of twet below 0c below the warmest saturated level.
          surfc=0
          do l=lwrm,lm
            if(twet(l).lt.con_t0c) then
              surfc=surfc+(twet(l)-con_t0c)*dz(l)
            endif
          enddo
!  Decide ice pellets if there is yet plenty of cold air.
      if(surfc.lt.-3000..or.(areas8.lt.-3000..and.surfw.lt.50.))         &
     &then
           iwx=iwx+2
          else
!  Otherwise decide freezing rain if the surface is cold.
            if(t(lm).lt.con_t0c) then
              iwx=iwx+4
!  Otherwise decide rain.
            else
              iwx=iwx+8
            endif
          endif
        endif
       endif
      endif
      
      return
      end

!     subroutine stma(the,pk,tma,qma)
!$$$     Subprogram Documentation Block
!
! Subprogram: stma         Compute moist adiabat temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute temperature and specific humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the LCL and pressure over 1e5 Pa to the kappa power.
!   Bilinear interpolations are done between values in a lookup table
!   computed in gtma. See documentation for stmaxg for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.01 Kelvin
!   and 5.e-6 kg/kg for temperature and humidity, respectively.
!   On the Cray, stma is about 35 times faster than exact calculation.
!   This subprogram should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-09-10  Jun Du              adopted to Regional Spectral Model
!
! Usage:  call stma(the,pk,tma,qma)
!
!   Input argument list:
!     the        Real(krealfp) equivalent potential temperature in Kelvin
!     pk         Real(krealfp) pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     tma        Real(krealfp) parcel temperature in Kelvin
!     qma        Real(krealfp) parcel specific humidity in kg/kg
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
!     implicit none
!     integer,parameter:: nxma=151,nyma=121
!     real*8 c1xma,c2xma,c1yma,c2yma,tbtma(nxma,nyma),
!    &tbqma(nxma,nyma)

!     real(krealfp),intent(in):: the,pk
!     real(krealfp),intent(out):: tma,qma
!     integer jx,jy
!     real(krealfp) xj,yj,ftx1,ftx2,qx1,qx2

!     real*8,intent(in):: the,pk
!     real*8,intent(out):: tma,qma
!     integer jx,jy
!     real*8 xj,yj,ftx1,ftx2,qx1,qx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
!     yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
!     jx=min(xj,nxma-1._krealfp)
!     jy=min(yj,nyma-1._krealfp)
!     xj=min(max(c1xma+c2xma*the,1.),real(nxma))
!     yj=min(max(c1yma+c2yma*pk,1.),real(nyma))
!     jx=min(xj,nxma-1.)
!     jy=min(yj,nyma-1.)
!     ftx1=tbtma(jx,jy)+(xj-jx)*(tbtma(jx+1,jy)-tbtma(jx,jy))
!     ftx2=tbtma(jx,jy+1)+(xj-jx)*(tbtma(jx+1,jy+1)-tbtma(jx,jy+1))
!     tma=ftx1+(yj-jy)*(ftx2-ftx1)
!     qx1=tbqma(jx,jy)+(xj-jx)*(tbqma(jx+1,jy)-tbqma(jx,jy))
!     qx2=tbqma(jx,jy+1)+(xj-jx)*(tbqma(jx+1,jy+1)-tbqma(jx,jy+1))
!     qma=qx1+(yj-jy)*(qx2-qx1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     return
!     end
      subroutine maxmin(a,len,k,k1,k2,ch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  maxmin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do print maximum and minimum of a given array.
!
! program history log:
!
! usage:    call  maxmin(a,len,k,k1,k2,ch)
!   input argument list:
!     a         - array for computing max and min (len,k)
!     len       - the first dimension of a
!     k         - the second dimension of a
!     k1        - lower limit of second dimension to print
!     k2        - upper limit to print
!     ch        - charcter string to print
!                 fpl and fml
!
!   output argument list:
!
!   input files: none
!
!   output files:
!     standard output
!
!   subprograms called:
!     intrinsic functions: amax1 amin1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      dimension a(len,k)
      character ch*(*)
!
      do 100 j=k1,k2
      aamax = a(1,j)
      aamin = a(1,j)
      do 10 m=1,len
      aamax = max( aamax, a(m,j) )
      if (aamax.eq.a(m,j)) mmax = m
      aamin = min( aamin, a(m,j) )
      if (aamin.eq.a(m,j)) mmin = m
10    continue
!     write(0,*)ch,' has max=',aamax,' min=',aamin,' at k=',j,mmax,mmin
      print   *,ch,' has max=',aamax,' min=',aamin,' at k=',j
100   continue
      return
      end
!
      subroutine expnd(origin,orislm,indsol,expand,imask,imax,jmax)
!
! written by shyh chen		ucsd,epcp,noaa
!
! input
!   origin  original value (imax,jmax)
!   orislm  original sea land mask (imax,jmax)
!   indsol  index for sea:0 or land:1
!   imask   working array for sea or land
!   imax    i dimension
!   jmax    j dimension
! output
!   expand  expanded value (imax,jmax)
!
      dimension orislm(imax,jmax),origin(imax,jmax),expand(imax,jmax)
      dimension temp(imax,jmax)
      dimension imask(imax,jmax),jmask(imax,jmax)
!
      do j=1,jmax
      do i=1,imax
         imask(i,j)=nint(orislm(i,j))
         if(imask(i,j).ge.2)imask(i,j)=0      ! get only land=1, or ocean=0
         expand(i,j)=origin(i,j)
         temp(i,j)=origin(i,j)
      enddo
      enddo
!
   25 continue
      ntot=0
      jmask=0
      do j=1,jmax
      do i=1,imax
       if(imask(i,j).ne.indsol)then    ! not over indsol
        ip1=min(i+1,imax)
        im1=max(i-1,1)
        jp1=min(j+1,jmax)
        jm1=max(j-1,1)
        tsum=0.
        np=0
        call acsum(imask(ip1,j),temp(ip1,j),tsum,indsol,np)
        call acsum(imask(im1,j),temp(im1,j),tsum,indsol,np)
        call acsum(imask(i,jp1),temp(i,jp1),tsum,indsol,np)
        call acsum(imask(i,jp1),temp(i,jp1),tsum,indsol,np)
        if(np.ge.1)then
          expand(i,j)=tsum/np
          jmask(i,j)=1
          ntot=ntot+1
        endif
       endif
      enddo
      enddo
      if(ntot.ge.1) then
        temp=expand
        imask=imask+(indsol-imask)*jmask
        go to 25
      endif
      return
      end
!
      subroutine acsum(imask,origin,tsum,indsol,np)
      if(imask.eq.indsol)then
        tsum=tsum+origin
        np=np+1
      endif
      return
      end
