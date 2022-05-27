#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#endif
!.....begin commap................................................
      common/mapcom/  rlat(IGRD1S,JGRD1S),  rlon(IGRD1S,JGRD1S)		&
     &              ,  xm2(IGRD1S,JGRD1S),  xm2p(IGRD1S,JGRD1S)		&
     &              ,xm2px(IGRD1S,JGRD1S), xm2py(IGRD1S,JGRD1S)		&
     &              , gzdx(IGRD1S,JGRD1S),  gzdy(IGRD1S,JGRD1S)		&
     &              , corf(IGRD1S,JGRD1S),    xm(IGRD1S,JGRD1S)		&
     &              , xm2m
!.......sof commap................................................
