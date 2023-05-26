!.....begin comver................................................
      common /rverfid/							&
     &   am(levr,levr),hm(levr,levr),tm(levr,levr),			&
     &   bm(levr,levr),cm(levr,levr),spdmax(levr),			&
     & si(levrp1),sl(levr),del(levr),rdel2(levrp1),rmsdot(levrm1),	&
     & ci(levrp1),cl(levr),tov(levr),   sv(levr),   rpi(levrm1),	&
     & p1(levr),p2(levr), h1(levr),   h2(levr),rpirec(levrm1)           &
#ifdef NONHYD
     &,cmn(levr+1,levr),                                                &
     & dmn(levr+1,levr),                                                &
     & emn(levr  ,levr+1),                                              &
     & fmn(levr  ,levr),                                                &
     & gmn(levr  ,levr+1),                                              &
     & hmn(levr  ,levr)                                                 &
#endif
     & ,tmprverfid
!.......sof comver................................................
