!fpp$ noconcur r
      subroutine sumtop(ap,top,qvv,klil,len0,lenh)
!ccj
#include <paramodel.h>
!cc
!c
      parameter ( lev2p1 = 2*levs+1 )
!c
      dimension     ap(2,0:len0,klil)
!c
      dimension    top(2,0:jcap,klil)
      dimension     ev(2,0:jcap,lev2p1)
      dimension     od(2,0:jcap,lev2p1)
!c
      dimension   qtop(2,0:jcap)
!c
      dimension    qvv(lnut2)
!c
!c
!c
      len=2*jcap1
      j=len+1
      do 10 l=0,jcap
      qtop(1,l) = qvv(j)
      qtop(2,l) = qvv(j+1)
      j=len+j
      len=len-2
   10 continue
!c
!c
!c
      do 24 k=1,klil
      do 22 l=0,jcap
            ev(1,l,k) = 0.0
            ev(2,l,k) = 0.0
            od(1,l,k) = 0.0
            od(2,l,k) = 0.0
   22 continue
   24 continue
!c
!c
      ltope=mod(jcap+1,2)
      ltopo=1-ltope
!c
      do k=1,klil
!c
!c      odd
        do l=ltopo,jcap,2
          od(1,l,k) = top(1,l,k) * qtop(1,l)
          od(2,l,k) = top(2,l,k) * qtop(2,l)
        enddo
!c
!c      even
        do l=ltope,jcap,2
          ev(1,l,k) = top(1,l,k) * qtop(1,l)
          ev(2,l,k) = top(2,l,k) * qtop(2,l)
        enddo
!c
      enddo
!c
!c
      do 250 k=1,klil
      do 240 l=0,jcap
            ap(1,l,k) = ap(1,l,k) + ev(1,l,k) + od(1,l,k)
            ap(2,l,k) = ap(2,l,k) + ev(2,l,k) + od(2,l,k)
!c
            ap(1,l+lenh,k) = ap(1,l+lenh,k) + ev(1,l,k) - od(1,l,k)
            ap(2,l+lenh,k) = ap(2,l+lenh,k) + ev(2,l,k) - od(2,l,k)
  240 continue
  250 continue
!c
      return
      end
