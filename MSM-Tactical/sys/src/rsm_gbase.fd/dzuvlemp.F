      subroutine dzuvlemp(d,z,u,v,utop,vtop,llstr,llens,lwvdef)
!
      use physcons, rerth => con_rerth
!ccj
#include <paramodel.h>
!
#ifdef MP
#define JCAP1S jcap1p
#define LEVSS levsp
#define LLN2S lln2p
#define LLN22S lln22p
#else
#define JCAP1S jcap1
#define LEVSS levs
#define LLN2S lnt2
#define LLN22S lnt22
#endif
      parameter (lnep=(jcap+2)*(jcap+3)/2)
      dimension  d(LLN22S,LEVSS),z(LLN22S,LEVSS)
      dimension  u(LLN22S,LEVSS),v(LLN22S,LEVSS)
      dimension  utop(2,JCAP1S,LEVSS)
      dimension  vtop(2,JCAP1S,LEVSS)
      dimension  lwvdef(jcap+1)
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
! ------------------ initial setting --------------------                       
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
! -------------------- end of initial setting ---------------
      lntx=0
      do 2000 ll=1,llens
        l=lwvdef(llstr+ll)
        lnt0=jc(l,l)-lntx
        lntx=lntx+jc(l+1,l+1)-jc(l,l)
!
#ifdef ORIGIN_THREAD
!$doacross share(d,z,u,v,utop,vtop,e,lnt0,l,ll),
!$&        local(k,n,rl,rn,j,jc0,je0)
#endif
#ifdef CRAY_THREAD
!mic$ do all                                                                    
!mic$1 shared(d,z,u,v,utop,vtop,e,lnt0,l,ll)
!mic$1 private(k,n,rl,rn,j,jc0,je0)
#endif
#ifdef OPENMP
!$omp parallel do private(k,n,rl,rn,j,jc0,je0)
#endif
      do 1000 k=1,LEVSS                                                        
!
      if(l.eq.0) then
        do 320 n=0,jcap
          jc0=jc(n,l)
          u(jc0+1,k)=0.0                                                        
          u(jc0+2,k)=0.0                                                        
!                                                                               
          v(jc0+1,k)=0.0 
          v(jc0+2,k)=0.0
320     continue  
      endif
!                                                                               
!----- l=1,jcap
! 
      if( l.ge.1 ) then
          rl=l                                                                  
        do 420 n=l,jcap                                                           
          rn=n                                                                  
          jc0=jc(n,l)-lnt0
!                                                                               
          u(jc0+2,k)=-rl*d(jc0+1,k)/(rn*(rn+1.))                            
          u(jc0+1,k)= rl*d(jc0+2,k)/(rn*(rn+1.))                            
!                                                                               
          v(jc0+2,k)=-rl*z(jc0+1,k)/(rn*(rn+1.))                            
          v(jc0+1,k)= rl*z(jc0+2,k)/(rn*(rn+1.))                            
420     continue                                                                  
      endif
!                                                                               
!----- l=  0,jcap-1
! 
      if( l.le.jcap-1 ) then
        do 520 n=l+1,jcap                                                         
          jc0=jc(n,l)-lnt0
          je0=je(n,l)
!
          u(jc0+1,k)=u(jc0+1,k)-e(je0+1)*z(jc0-1,k)                 
          u(jc0+2,k)=u(jc0+2,k)-e(je0+1)*z(jc0  ,k)                 
!                                                                               
          v(jc0+1,k)=v(jc0+1,k)+e(je0+1)*d(jc0-1,k)                 
          v(jc0+2,k)=v(jc0+2,k)+e(je0+1)*d(jc0  ,k)                 
520     continue                                                                  
        do 620 n=l,jcap-1                                                         
          jc0=jc(n,l)-lnt0
          je0=je(n+1,l)
!
          u(jc0+1,k)=u(jc0+1,k)+e(je0+1)*z(jc0+3,k)               
          u(jc0+2,k)=u(jc0+2,k)+e(je0+1)*z(jc0+4,k)               
!                                                                               
          v(jc0+1,k)=v(jc0+1,k)-e(je0+1)*d(jc0+3,k)               
          v(jc0+2,k)=v(jc0+2,k)-e(je0+1)*d(jc0+4,k)               
620     continue                                                                  
      endif
!                                                                               
      n=jcap+1                                                                  
!
!----- l=0,jcap
!     
      jc0=jc(n,l)-lnt0
      je0=je(n,l)
!
      utop(1,ll,k)=-e(je0+1)*z(jc0-1,k)                                
      utop(2,ll,k)=-e(je0+1)*z(jc0  ,k)                                
!                                                                               
      vtop(1,ll,k)= e(je0+1)*d(jc0-1,k)                                
      vtop(2,ll,k)= e(je0+1)*d(jc0  ,k)                                
!                                                                               
!----- l=0,jcap
!  
      utop(1,ll,k)=utop(1,ll,k)*rerth                                            
      utop(2,ll,k)=utop(2,ll,k)*rerth                                            
      vtop(1,ll,k)=vtop(1,ll,k)*rerth                                            
      vtop(2,ll,k)=vtop(2,ll,k)*rerth                                            
!
1000  continue                                                                  
!
2000  continue
!
      do k=1,LEVSS
        do j=1,LLN2S
          u(j,k)=u(j,k)*rerth 
          v(j,k)=v(j,k)*rerth
        enddo
      enddo
!                                                                               
      return                                                                    
      end                                                                       
