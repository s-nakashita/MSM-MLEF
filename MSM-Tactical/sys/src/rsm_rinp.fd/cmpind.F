      subroutine cmpind
!
!ccj
#include <paramodel.h>
!cc
      parameter(mwave=jcap,iromb=jromb)
!
      parameter(mwavep=mwave+1,                                          &  
     &          mdim=(mwave+1)*(mwave+1)*2*iromb+                        &
     &               (mwave+1)*(mwave+2)*(1-iromb))
!
      common/comind/ indxnn(mdim),indxmm(mdim)
!
!  indxnn(mdim) :  1-d index of converting input form spher coeff array
!                  to transposed form array
!  indxmm(mdim) :  1-d index of converting transposed form spher coeff
!                  array to input form spherical coeff array
!
      if(jromb.eq.0) then
      l=0
      do 10 m=1,mwavep
      nend=mwavep-m+1
      do 10 nn=1,nend
      n=nn+m-1
      l=l+2
      indx=(mwavep*(n-m)-(n-m)*(n-m-1)/2+m)*2-1
      indxnn(l-1)=indx
      indxnn(l  )=indx+1
   10 continue
!
      l=0
      do 20 nn=1,mwavep
      lln=mwavep-nn+1
      do 20 ll=1,lln
      n=ll+nn-1
      m=ll
      indx=(m*mwavep-(mwavep-n)-(m-1)*m/2)*2-1
      l=l+2
      indxmm(l-1)=indx
      indxmm(l  )=indx+1
   20 continue
      return
      endif
!
      if(jromb.eq.1) then
      l=0
      do 30 m=1,mwavep
      do 30 nn=1,mwavep
      n=nn+m-1
      indx=((n-m)*mwavep+m)*2-1
      l=l+2
      indxnn(l-1)=indx
      indxnn(l  )=indx+1
   30 continue
!
      l=0
      do 40 nn=1,mwavep
      do 40 ll=1,mwavep
      n=ll+nn-1
      m=ll
      indx=(mwavep*(m-1)+n-m+1)*2-1
      l=l+2
      indxmm(l-1)=indx
      indxmm(l  )=indx+1
   40 continue
      return
      endif
!
      end
