      subroutine transi(a,kmax)
!
!ccj
#include <paramodel.h>
!cc
      parameter(mwave=jcap,iromb=jromb)
!
      parameter(mwavep=mwave+1,                                         &
     &          mdim=(mwave+1)*(mwave+1)*2*iromb+                       &
     &               (mwave+1)*(mwave+2)*(1-iromb))
!
      common/comind/ indxnn(mdim),indxmm(mdim)
!
      dimension a(mdim,kmax)
      dimension b(mdim)
!
      do 1 k=1,kmax
      do 2 m=1,mdim
      b(indxnn(m))=a(m,k)
    2 continue
      do 3 m=1,mdim
      a(m,k)=b(m)
    3 continue
    1 continue
!
      return
      end
