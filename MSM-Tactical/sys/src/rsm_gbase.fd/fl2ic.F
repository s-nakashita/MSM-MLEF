      subroutine fl2ic(fp,fm,fln,qln,llstr,llens,lwvdef,lota)
#include <paramodel.h>
#ifdef MP
#define JCAPS jcapp
#define LLN2S lln2p
#define LLN22S lln22p
#else
#define JCAPS jcap
#define LLN2S lnt2
#define LLN22S lnt22
#endif
      parameter (len0p=JCAPS)                                                   
      parameter (len0m=JCAPS)                                                   
      dimension fp(2,0:len0p,lota), fm(2,0:len0m,lota),                 &
     &          qln(LLN2S), fln(LLN22S,lota)
      dimension lwvdef(jcap1)
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
      offset(n,l) = (jcap+1)*(jcap+2) - (jcap+1-l)*(jcap+2-l) + 2*(n-l)         
!                                                                               
!     ----------------------------------------------------------------          
!     compute the coefficients of the expansion in spherical harmonics          
!     of the field at each level                                                
!     ----------------------------------------------------------------          
!                                                                               
      lntx=0
      do 1000 lx=1,llens
        l=lwvdef(llstr+lx)
        lnt0=offset(l,l)-lntx
        lntx=lntx+offset(l+1,l+1)-offset(l,l)
        ll=lx-1
!
#ifdef CRA
         ls=l*((2*jcap+3)-l)                                                    
         lls=ls-lnt0
#endif
!                                                                               
!        compute the even (n-l) expansion coefficients for each level           
!        ------------------------------------------------------------           
!                                                                               
!        real part                                                              
!                                                                               
#ifdef CRA
         call sgerx1((jcap+2-l)/2,lota,1.,qln(lls+1),4,                 &        
     &               fp(1,ll,1),(len0p+1)*2,fln(lls+1,1),4,LLN22S)
#else
         do n = l, jcap, 2                                                      
            do k = 1, lota                                                      
               nl=offset(n,l)-lnt0
               fln(nl+1,k) = fln(nl+1,k)                                &
     &                      + fp(1,ll,k)*qln(nl+1)
            end do                                                              
         end do                                                                 
#endif
!                                                                               
!        imaginary part                                                         
!                                                                               
#ifdef CRA
         call sgerx1((jcap+2-l)/2,lota,1.,qln(lls+2),4,                 &        
     &               fp(2,ll,1),(len0p+1)*2,fln(lls+2,1),4,LLN22S) 
#else
         do n = l, jcap, 2                                                      
            do k = 1, lota                                                      
               nl=offset(n,l)-lnt0
               fln(nl+2,k) = fln(nl+2,k)                                &
     &                      + fp(2,ll,k)*qln(nl+2)
            end do                                                              
         end do                                                                 
#endif
!                                                                               
!        compute the odd (n-l) expansion coefficients for each level            
!        -----------------------------------------------------------            
#ifdef CRA
         if(l.lt.jcap) then                                                     
#endif
!                                                                               
!        real part                                                              
!                                                                               
#ifdef CRA
           call sgerx1((jcap+1-l)/2,lota,1.,qln(lls+3),4,               &         
     &                 fm(1,ll,1),(len0m+1)*2,fln(lls+3,1),4,LLN22S)               
#else
           do n = l+1, jcap, 2                                                  
              do k = 1, lota                                                    
                nl=offset(n,l)-lnt0
                fln(nl+1,k) = fln(nl+1,k)                               &
     &                      + fm(1,ll,k)*qln(nl+1)          
              end do                                                            
           end do                                                               
#endif
!                                                                               
!        imaginary part                                                         
!                                                                               
#ifdef CRA
           call sgerx1((jcap+1-l)/2,lota,1.,qln(lls+4),4,               &        
     &                 fm(2,ll,1),(len0m+1)*2,fln(lls+4,1),4,LLN22S)               
#else
           do n = l+1, jcap, 2                                                  
              do k = 1, lota                                                    
                nl=offset(n,l)-lnt0
                fln(nl+2,k) = fln(nl+2,k)                               &
     &                       + fm(2,ll,k)*qln(nl+2)
              end do                                                            
           end do                                                               
#endif
!                                                                               
#ifdef CRA
         endif                                                                  
#endif
!
 1000 continue
!                                                                               
      return                                                                    
      end                                                                       
