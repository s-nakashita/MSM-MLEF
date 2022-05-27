#ifdef MP
#define LNT22S lnt22p
#else
#define LNT22S lnt22
#endif
!c.................................................................
!c................begin (rscomfc)........................
!c....
!c    version with stacked transforms
!c....
       character*8 label(4)
#ifdef G2R
       common /rcomfgi/ label,idate(4),n1,n2
       common /rcomfg/ si(levs+1),sl(levs),                              &
     &             eps(lnuv),epsi(lnuv),                                 & 
     &              gz(LNT22S),	                                         & 
     &              zv(LNT22S,levs),                                     &
     &              du(LNT22S,levs),                                     & 
     &              te(LNT22S,levs),					 &
     &              rq(LNT22S,levs,ntotal),				 &
     &               q(LNT22S)
#endif
!c.................................................................
!c....
#ifdef C2R
       common /rcomfci/ label,idate(4),n1,n2
       common /rcomfc/ si(levs+1),sl(levs)				 &
     & ,  q(clngrd)							 &
     & , te(clngrd,levs)						 &
     & , rq(clngrd,levs,ntotal)						 &
     & , gz(clngrd)							 &
     & , du(clngrd,levs)						 &
     & , zv(clngrd,levs)						 &
     &,flat(clngrd), flon(clngrd)					 &
     &, fm2(clngrd), fm2x(clngrd), fm2y(clngrd)
#endif
!c....
!c.................................................................
