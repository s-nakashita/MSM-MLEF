#include <define.h>
      subroutine fl2imp(fp,fm,fln,qln,llstr,llens,lwvdef,lota,jcap      &  
#ifndef MP
     &           ,lnt2,lnt22                                            &   
#else
     &           ,jcapp,lln2p,lln22p                                    &
#endif
     &            )
!                                                                               
! parallelize at k level.                                                       
!                                                                               
!c#include <paramodel.h>
#ifdef MP
#define JCAPS jcapp
#define LLN2S lln2p
#define LLN22S lln22p
#else
#define JCAPS jcap
#define LLN2S lnt2
#define LLN22S lnt22
#endif
      dimension fp(2,0:JCAPS,lota), fm(2,0:JCAPS,lota),                 &       
     &          qln(LLN2S), fln(LLN22S,lota)
      dimension lwvdef(jcap+1)
!                                                                               
!     local scalars                                                             
!     -------------                                                             
!                                                                               
      integer n, l, k                                                           
!                                                                               
!     statement function                                                        
!     ------------------                                                        
!                                                                               
!     offset(n,l) is the offset in words                                        
!     to the (n,l)-element of a lower                                           
!     triangular matrix of complex numbers                                      
!     in an array containing the matrix                                         
!     packed in column-major order,                                             
!     where l and n range from 0 to jcap,                                       
!     inclusive                                                                 
!                                                                               
!          lower triangular matrix of complex numbers:                          
!                                                                               
!                     l -->                                                     
!                                                                               
!                   x                                                           
!               n   x x                                                         
!                   x x x                                                       
!               |   x x x x                                                     
!               v   x x x x x                                                   
!                   x x x x x x                                                 
!                                                                               
!          order of the matrix elements in memory:                              
!                                                                               
!          (0,0), (1,0), (2,0), ..., (jcap,0), (1,1), (2,1), (3,1), ...         
!                                                                               
      integer offset                                                            
      offset(n,l)=(jcap+1)*(jcap+2)-(jcap+1-l)*(jcap+2-l)+2*(n-l)
!                                                                               
!     ----------------------------------------------------------------          
!     compute the coefficients of the expansion in spherical harmonics          
!     of the field at each level                                                
!     ----------------------------------------------------------------          
!                                                                               
!
#ifdef CRAY_THREAD
!cfpp$ cncall                                                                   
#endif
!                                                                               
#ifdef ORIGIN_THREAD
!$doacross share(fp,fm,fln,qln,llstr,llens,lwvdef,jcap,lota)
!$& local(k,l,n,ll,nl,lntx,lnt0,lx)                                                                
#endif
#ifdef CRAY_THREAD
!mic$ doall shared(fp,fm,fln,qln,llstr,llens,lwvdef,jcap,lota)
!mic$1      private(k,l,n,ll,nl,lntx,lnt0,lx)
#endif
#ifdef OPENMP
!$omp parallel do private(k,l,n,ll,nl,lntx,lnt0,lx)
#endif
!                                                                               
      do k=1,lota  ! mj 5/8/1998                                                
!
        lntx=0
        do lx=1,llens
          l=lwvdef(llstr+lx)
          lnt0=offset(l,l)-lntx
          lntx=lntx+offset(l+1,l+1)-offset(l,l)
          ll=lx-1
!                                                                               
!        compute the even (n-l) expansion coefficients for each level           
!        ------------------------------------------------------------           
!                                                                               
!        real part                                                              
!                                                                               
          do n = l, jcap, 2                                                      
            nl=offset(n,l)-lnt0
            fln(nl+1,k) = fln(nl+1,k)                                   &
     &                   + fp(1,ll,k)*qln(nl+1)
          end do                                                                 
!                                                                               
!        imaginary part                                                         
!                                                                               
          do n = l, jcap, 2                                                      
            nl=offset(n,l)-lnt0
            fln(nl+2,k) = fln(nl+2,k)                                   &
     &                   + fp(2,ll,k)*qln(nl+2)
          end do                                                                 
!                                                                               
!        compute the odd (n-l) expansion coefficients for each level            
!        -----------------------------------------------------------            
!                                                                               
!        real part                                                              
!                                                                               
          do n = l+1, jcap, 2                                                  
            nl=offset(n,l)-lnt0
            fln(nl+1,k) = fln(nl+1,k)                                   &
     &                   + fm(1,ll,k)*qln(nl+1)
          end do                                                               
!                                                                               
!        imaginary part                                                         
!                                                                               
          do n = l+1, jcap, 2                                                  
            nl=offset(n,l)-lnt0
            fln(nl+2,k) = fln(nl+2,k)                                   &
     &                   + fm(2,ll,k)*qln(nl+2)
          end do                                                               
!                                                                               
        enddo                                                                   
      enddo     !mj 5/8/1998                                                    
!                                                                               
      return                                                                    
      end                                                                       
