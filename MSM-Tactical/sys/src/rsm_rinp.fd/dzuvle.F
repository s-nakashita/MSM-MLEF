      subroutine dzuvle(d,z,u,v,utop,vtop)
      use physcons, rerth => con_rerth
!ccj
#include <paramodel.h>
!cc
      parameter (lnep=(jcap+2)*(jcap+3)/2)
      dimension  d(lnt22,levs),z(lnt22,levs)
      dimension  u(lnt22,levs),v(lnt22,levs)
      dimension  utop(2,jcap+1,levs)
      dimension  vtop(2,jcap+1,levs)
      dimension  e(lnep)
!
      save e, ifirst
!
!   array e =eps/n
!   array e =eps/n
!            eps/n=0. for n=l
!   array e =eps/n
!   array e =eps/n
!
      je(n,l) =((jcap+2)*(jcap+3)-(jcap+2-l)*(jcap+3-l))/2+n-l
!
      jc(n,l) = (jcap+1)*(jcap+2)-(jcap+1-l)*(jcap+2-l)+2*(n-l)
!
      data ifirst/1/
      if(ifirst.ne.1)go to 280
!
      do 220 l=0,jcap
             n=l
      e(je(n,l)+1)=0.
220   continue
      do 260 l=  0,jcap
      do 240 n=l+1,jcap+1
      rn=n
      rl=l
      a=(rn*rn-rl*rl)/(4.*rn*rn-1.)
      e(je(n,l)+1)=sqrt(a) / rn
240   continue
260   continue
      ifirst=0
280   continue
!
#ifdef ORIGIN_THREAD
!$doacross share(d,z,u,v,utop,vtop,e),
!$&        local(k,l,n,rl,rn,j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(d,z,u,v,utop,vtop,e)
!mic$1 private(k,l,n,rl,rn,j)
#endif
#ifdef OPENMP
!$omp parallel do private(k,l,n,rl,rn,j)
#endif
      do 10000 k=1,levs
             l=0
      do 320 n=0,jcap
!     u(l,n)=-i*l*d(l,n)/(n*(n+1))
!
      u(jc(n,l)+1,k)=0.0
      u(jc(n,l)+2,k)=0.0
!
      v(jc(n,l)+1,k)=0.0
      v(jc(n,l)+2,k)=0.0
320   continue
!
      do 440 l=1,jcap
          rl=l
      do 420 n=l,jcap
          rn=n
!     u(l,n)=-i*l*d(l,n)/(n*(n+1))
!
      u(jc(n,l)+2,k)=-rl*d(jc(n,l)+1,k)/(rn*(rn+1.))
      u(jc(n,l)+1,k)= rl*d(jc(n,l)+2,k)/(rn*(rn+1.))
!
      v(jc(n,l)+2,k)=-rl*z(jc(n,l)+1,k)/(rn*(rn+1.))
      v(jc(n,l)+1,k)= rl*z(jc(n,l)+2,k)/(rn*(rn+1.))
420   continue
440   continue
!
      do 540 l=  0,jcap-1
      do 520 n=l+1,jcap
      u(jc(n,l)+1,k)=u(jc(n,l)+1,k)-e(je(n,l)+1)*z(jc(n,l)-1,k)
      u(jc(n,l)+2,k)=u(jc(n,l)+2,k)-e(je(n,l)+1)*z(jc(n,l)  ,k)
!
      v(jc(n,l)+1,k)=v(jc(n,l)+1,k)+e(je(n,l)+1)*d(jc(n,l)-1,k)
      v(jc(n,l)+2,k)=v(jc(n,l)+2,k)+e(je(n,l)+1)*d(jc(n,l)  ,k)
520   continue
540   continue
!
      do 640 l=0,jcap-1
      do 620 n=l,jcap-1
      u(jc(n,l)+1,k)=u(jc(n,l)+1,k)+e(je(n+1,l)+1)*z(jc(n,l)+3,k)
      u(jc(n,l)+2,k)=u(jc(n,l)+2,k)+e(je(n+1,l)+1)*z(jc(n,l)+4,k)
!
      v(jc(n,l)+1,k)=v(jc(n,l)+1,k)-e(je(n+1,l)+1)*d(jc(n,l)+3,k)
      v(jc(n,l)+2,k)=v(jc(n,l)+2,k)-e(je(n+1,l)+1)*d(jc(n,l)+4,k)
620   continue
640   continue
!
      n=jcap+1
      do 740 l=0,jcap
      utop(1,l+1,k)=-e(je(n,l)+1)*z(jc(n,l)-1,k)
      utop(2,l+1,k)=-e(je(n,l)+1)*z(jc(n,l)  ,k)
!
      vtop(1,l+1,k)= e(je(n,l)+1)*d(jc(n,l)-1,k)
      vtop(2,l+1,k)= e(je(n,l)+1)*d(jc(n,l)  ,k)
740   continue
!
      do 820 j=1,lnt2
      u(j,k)=u(j,k)*rerth
      v(j,k)=v(j,k)*rerth
820   continue
!
      do 840 j=1,jcap1
      utop(1,j,k)=utop(1,j,k)*rerth
      utop(2,j,k)=utop(2,j,k)*rerth
      vtop(1,j,k)=vtop(1,j,k)*rerth
      vtop(2,j,k)=vtop(2,j,k)*rerth
840   continue
10000 continue
      return
      end
