#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define LNGRDS lngrdp
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define LNGRDS lngrd
#endif
!.................................................................
!....begin regional grid common....................
!
      common /rcomgd1/                                                   &   
     & flat(LNGRDS), flon(LNGRDS), 					 &
     &  fm2(LNGRDS), fm2x(LNGRDS), fm2y(LNGRDS)
!....
      common /rcomgd2/							 &
     &  slmsk(IGRD1S,JGRD1S),						 &
     & hprime(IGRD1S,JGRD1S,nmtvr),					 &
     &    swh(IGRD1S,levr,JGRD1S),hlw(IGRD1S,levr,JGRD1S),		 &
     & sfcnsw(IGRD1S,JGRD1S),   sfcdlw(IGRD1S,JGRD1S),			 &
     & sinlar(IGRD1S,JGRD1S),   coslar(IGRD1S,JGRD1S),			 &
     &                            coszer(IGRD1S,JGRD1S),		 &
     &    acv(IGRD1S,JGRD1S),     acvt(IGRD1S,JGRD1S),			 &
     &     cv(IGRD1S,JGRD1S),      cvt(IGRD1S,JGRD1S),			 &
     &    cvb(IGRD1S,JGRD1S),     acvb(IGRD1S,JGRD1S),			 &
     &  tsflw(IGRD1S,JGRD1S),     f10m(IGRD1S,JGRD1S),			 &
     &  sdec,cdec,slag,solhr,clstp
!....
      common /rcomgd3/ 				 			 &
     &  dusfc(IGRD1S,JGRD1S),    dvsfc(IGRD1S,JGRD1S),			 &
     &  dtsfc(IGRD1S,JGRD1S),    dqsfc(IGRD1S,JGRD1S),			 &
     & dlwsfc(IGRD1S,JGRD1S),   ulwsfc(IGRD1S,JGRD1S),			 &
     & geshem(IGRD1S,JGRD1S),     tsea(IGRD1S,JGRD1S),			 &
     &  dugwd(IGRD1S,JGRD1S),    dvgwd(IGRD1S,JGRD1S),			 &
     &   u10m(IGRD1S,JGRD1S),     v10m(IGRD1S,JGRD1S),			 &
     &    t2m(IGRD1S,JGRD1S),      q2m(IGRD1S,JGRD1S),			 &
     &  psurf(IGRD1S,JGRD1S),   psmean(IGRD1S,JGRD1S)
!....
      common /rcomgd4/							 &
     &    tg3(IGRD1S,JGRD1S),     zorl(IGRD1S,JGRD1S),			 &
     & sheleg(IGRD1S,JGRD1S),   bengsh(IGRD1S,JGRD1S),			 &
     &  gflux(IGRD1S,JGRD1S),    slrad(IGRD1S),				 &
     &    smc(IGRD1S,JGRD1S,lsoil),					 &
     &    stc(IGRD1S,JGRD1S,lsoil),					 &
     & canopy(IGRD1S,JGRD1S),   runoff(IGRD1S,JGRD1S),			 &
     & tmpmax(IGRD1S,JGRD1S),   tmpmin(IGRD1S,JGRD1S),			 &
     & spfhmax(IGRD1S,JGRD1S), spfhmin(IGRD1S,JGRD1S),			 &
     &     ep(IGRD1S,JGRD1S),   cldwrk(IGRD1S,JGRD1S),			 &
     &   hpbl(IGRD1S,JGRD1S),     pwat(IGRD1S,JGRD1S),			 &
     &  alvsf(IGRD1S,JGRD1S),    alvwf(IGRD1S,JGRD1S),			 &
     &  alnsf(IGRD1S,JGRD1S),    alnwf(IGRD1S,JGRD1S),			 &
     &  facsf(IGRD1S,JGRD1S),    facwf(IGRD1S,JGRD1S),                   &    
     &  vfrac(IGRD1S,JGRD1S),    vtype(IGRD1S,JGRD1S),                   &  
     &  stype(IGRD1S,JGRD1S),   uustar(IGRD1S,JGRD1S),                   &    
     &   ffmm(IGRD1S,JGRD1S),     ffhh(IGRD1S,JGRD1S)
!.... end of rcomgdn
