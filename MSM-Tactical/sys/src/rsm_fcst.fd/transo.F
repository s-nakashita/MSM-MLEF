      subroutine transo(a,kmax)
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
      do 10 k=1,kmax
      do 11 m=1,mdim
      b(indxmm(m))=a(m,k)
   11 continue
      do 12 m=1,mdim
      a(m,k)=b(m)
   12 continue
   10 continue
!
      return
      end
