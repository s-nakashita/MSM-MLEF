#include <rsparltb.h>
!.....begin comltb................................................
#ifdef MP
#define LNGRDS lngrdp
#else
#define LNGRDS lngrd
#endif
!.....begin comltb................................................
      common/rltbcom/                                                   &    
     &   rltb(LNGRDS),  sltb(LNGRDS)
      common/ltbcom/ 							&
     &   blat(lngrdb), blon(lngrdb)					&
     &  ,bcsln(lngrdb),bsnln(lngrdb)					&
     &  , wsltb(bgf,1-border:border)					&
     &  ,dwsltb(bgf,1-border:border)					&
     &  ,             qb(lngrdb)					&
     &  ,uub(lngrdb,levs),vvb(lngrdb,levs)				&
     &  ,teb(lngrdb,levs),rqb(lngrdb,levs,ntotal)			&
     &  , gzb(lngrdb)							&
     &  ,             qltb(lngrdb)					&
     &  ,uultb(lngrdb,levs),vvltb(lngrdb,levs)				&
     &  ,teltb(lngrdb,levs),rqltb(lngrdb,levs,ntotal)
      common/savbasecom/                                                &
     &    sgzb(LNGRDS), sqb(LNGRDS)                                     &
     &  ,suub(LNGRDS,levr),svvb(LNGRDS,levr)                            &
     &  ,steb(LNGRDS,levr),srqb(LNGRDS,levr,ntotal)
!.......sof comltb................................................
