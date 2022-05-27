!
! ...... begin parltb .............
#ifdef MP
#define IGRD1S  igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S  igrd1
#define JGRD1S jgrd1
#endif
! for safety, we add 1 for both
      parameter(ibgd1=(IGRD1S-1)/bgf+2*border+1,                          &
     &          jbgd1=(JGRD1S-1)/bgf+2*border+1,                          &
     &          lngrdb=ibgd1*jbgd1)
      common/comparltb/ ib1,ib2,jb1,jb2,jbx,istr,ilen
! ...... end parltb .............
