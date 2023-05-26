#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#endif
      common /rradia1/ fluxr(IGRD1S,JGRD1S,27)
      common /rradia2/ cvavg(IGRD1S,JGRD1S)
!.............................................................
