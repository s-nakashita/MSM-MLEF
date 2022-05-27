!.................................................................
!....parameters and variables for ozone ....................
!
#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define LNGRDS lngrdp
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define LNGRDS lngrd
#endif
!
      integer  lats18,lev46
      PARAMETER (lats18=18, lev46=46)
      COMMON/RCOMOZ/poz(lev46),						&
     &  ozprdin(lats18,lev46,36),					&
     &  ozdisin(lats18,lev46,36),					&
     &  JINDX1(LNGRDS),JINDX2(LNGRDS),  				& 
     &  DDY(LNGRDS),                    				& 
     &  OZPRDOUT(LNGRDS,lev46),						&
     &  OZDISOUT(LNGRDS,lev46),						&
     &  OZPRD(IGRD1S,JGRD1S,lev46),					&
     &  OZDIS(IGRD1S,JGRD1S,lev46)
!cc
