      subroutine dellnp(q,dpdphs,dpdtop,dpdla)
      use physcons, rerth => con_rerth
!ccj
#include <paramodel.h>
!cc
!
      parameter (lnep=(jcap+2)*(jcap+3)/2)
!
! input q is in ibm triang. order
! output  is in ibm triang. order
!
      dimension         q(lnt22)
      dimension    dpdphs(lnt22)
      dimension    dpdtop(2,jcap1)
      dimension     dpdla(lnt22)
      dimension         e(lnep)
!
      save e, ifirst
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
        ie=je(n,l)+1
      e(ie)=0.
220   continue
      do 260 l=  0,jcap
      do 240 n=l+1,jcap+1
      rn=n
      rl=l
      a=(rn*rn-rl*rl)/(4.*rn*rn-1.)
        ie=je(n,l)+1
      e(ie)=sqrt(a)
240   continue
260   continue
      ifirst=0
280   continue
!
      do 340 l=0,jcap
          rl=l
      do 320 n=l,jcap
        icr=jc(n,l)+1
        ici=jc(n,l)+2
!     dpdla(l,n)= i*l*q(l,n)
!
      dpdla(ici)= rl*q(icr)
      dpdla(icr)=-rl*q(ici)
320   continue
340   continue
!
      do 440 l=0,jcap-1
      do 420 n=l,jcap-1
         ie=je(n+1,l)+1
        icr=jc(n,l)+1
        ici=jc(n,l)+2
         rn=n
      dpdphs(icr)=(rn+2.)*e(ie)*q(icr+2)
      dpdphs(ici)=(rn+2.)*e(ie)*q(ici+2)
420   continue
440   continue
!
             n=  jcap
      do 540 l=0,jcap
        icr=jc(n,l)+1
        ici=jc(n,l)+2
      dpdphs(icr)=0.0
      dpdphs(ici)=0.0
540   continue
!
      do 640 l=  0,jcap-1
      do 620 n=l+1,jcap
         ie=je(n,l)+1
        icr=jc(n,l)+1
        ici=jc(n,l)+2
         rn=n
      dpdphs(icr)=dpdphs(icr)+(1.-rn)*e(ie)*q(icr-2)
      dpdphs(ici)=dpdphs(ici)+(1.-rn)*e(ie)*q(ici-2)
620   continue
640   continue
!
          n=jcap+1
         rn=n
      do 740 l=0,jcap
         ie=je(n,l)+1
        icr=jc(n,l)+1
        ici=jc(n,l)+2
      dpdtop(1,l+1)=(1.-rn)*e(ie)*q(icr-2)
      dpdtop(2,l+1)=(1.-rn)*e(ie)*q(ici-2)
740   continue
!
      aa=1./rerth
      do 820 j=1,lnt2
       dpdla(j)= dpdla(j)*aa
      dpdphs(j)=dpdphs(j)*aa
820   continue
!
      do 840 j=1,jcap1
      dpdtop(1,j)=dpdtop(1,j)*aa
      dpdtop(2,j)=dpdtop(2,j)*aa
840   continue
!
      return
      end
