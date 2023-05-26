#include <define.h>
      subroutine sums2imp(fln,ap,qln,llstr,llens,lwvdef,levs1,jcap       &  
#ifndef MP
     &           ,lonf,lnt2,lnt22                                        &
#else
     &           ,lcapp,lln2p,lln22p                                     &
#endif
     &            )
!c#include <paramodel.h>
#ifdef MP
#define LLN2S lln2p
#define LLN22S lln22p
#define LCAPS lcapp
#else
#define LLN2S lnt2
#define LLN22S lnt22
#define LCAPS lonf
#endif
      dimension ap(2,0:LCAPS,levs1), qln(LLN2S), fln(LLN22S,levs1)
      dimension lwvdef(jcap+1)
!                                                                               
!     local scalars                                                             
!     -------------                                                             
!                                                                               
      integer i, n, l, k                                                        
      real    evenr, eveni                                                      
!                                                                               
!     statement functions                                                       
!     -------------------                                                       
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
      offset(n,l)=(jcap+1)*(jcap+2)-(jcap-l+1)*(jcap-l+2)+2*(n-l)
!                                                                               
!     ---                                                                       
!                                                                               
!     term(1,n,l,k) and term(2,n,l,k) are                                       
!     the real and imaginary part, resp.,                                       
!     of exp((0,1)*l*phi) times the (n,l) term                                  
!     in the expansion in spherical                                             
!     harmonics of the field at level k,                                        
!     where phi is the azimuthal angle                                          
!                                                                               
      term(i,n,l,k) = qln(offset(n,l)-lnt0+i)*fln(offset(n,l)-lnt0+i,k)
!                                                                               
!     zero the accumulators                                                     
!     ---------------------                                                     
!                                                                               
      len0=LCAPS
      lenh=len0/2
!
!     lens=l1e-l1s+l2e-l2s+1
      do k = 1, levs1 
         do l = 0, len0
            ap(1,l,k) = 0.                                                      
            ap(2,l,k) = 0.                                                      
         end do                                                                 
      end do                                                                    
!                                                                               
!     compute the even and odd (n-l) components                                 
!     of the fourier coefficients                                               
!     ---------------------------------------------------------                 
!                                                                               
!                                                                               
#ifdef CRA
      lntx=0
      do 1000 lx=1,llens
        l=lwvdef(llstr+lx)
        lnt0=offset(l,l)-lntx
        lntx=lntx+offset(l+1,l+1)-offset(l,l)
        ll=lx-1
!
        ls=l*((2*jcap+3)-l)                                                    
        lls=ls-lnt0
!                                                                               
!        compute the sum of the even (n-l) terms for each level                 
!        ------------------------------------------------------                 
!                                                                               
!        real part                                                              
!                                                                               
         call sgemvx1(levs1,(jcap+2-l)/2,1.,fln(lls+1,1),LLN22S,4,       &  
     &                qln(lls+1),4,1.,ap(1,ll,1),(len0+1)*2)
!                                                                               
!        imaginary part                                                         
!                                                                               
         call sgemvx1(levs1,(jcap+2-l)/2,1.,fln(lls+2,1),LLN22S,4,       & 
     &                qln(lls+2),4,1.,ap(2,ll,1),(len0+1)*2)
!                                                                               
!        compute the sum of the odd (n-l) terms for each level                  
!        -----------------------------------------------------                  
!                                                                               
!          real part                                                            
!                                                                               
         if(l.lt.jcap) then   
           call sgemvx1(levs1,(jcap+1-l)/2,1.,fln(lls+3,1),LLN22S,4,     &  
     &                qln(lls+3),4,1.,ap(1,lenh+ll,1),(len0+1)*2)
!                                                                               
!          imaginary part                                                       
!                                                                               
           call sgemvx1(levs1,(jcap+1-l)/2,1.,fln(lls+4,1),LLN22S,4,     &  
     &                qln(lls+4),4,1.,ap(2,lenh+ll,1),(len0+1)*2)
!                                                                               
         endif                                                                  
!
!     compute the fourier coefficients for each level                           
!     -----------------------------------------------                           
!                                                                               
        do k = 1, levs1  
          evenr = ap(1,ll,k)                                                   
          eveni = ap(2,ll,k)                                                   
          ap(1,ll,k) = ap(1,ll,k) + ap(1,lenh+ll,k)                              
          ap(2,ll,k) = ap(2,ll,k) + ap(2,lenh+ll,k)                              
          ap(1,lenh+ll,k) = evenr - ap(1,lenh+ll,k)                             
          ap(2,lenh+ll,k) = eveni - ap(2,lenh+ll,k)                             
        end do                                                                    
!
 1000 continue

#else

      do 1000 k = 1, levs1
!
        lntx=0
        do lx=1,llens
!
          l=lwvdef(llstr+lx)
          lnt0=offset(l,l)-lntx
          lntx=lntx+offset(l+1,l+1)-offset(l,l)
          ll=lx-1
!
!                                                                               
!        compute the sum of the even (n-l) terms for each level                 
!        ------------------------------------------------------                 
!                                                                               
!        real part                                                              
!                                                                               
          do n = l, jcap, 2                                                      
               ap(1,ll,k) = ap(1,ll,k) + term(1,n,l,k)
          enddo
!                                                                               
!        imaginary part                                                         
!                                                                               
          do n = l, jcap, 2                                                      
               ap(2,ll,k) = ap(2,ll,k) + term(2,n,l,k)
          enddo                                                                 
!                                                                               
!        compute the sum of the odd (n-l) terms for each level                  
!        -----------------------------------------------------                  
!                                                                               
!          real part                                                            
!                                                                               
          do n = l+1, jcap, 2                                                  
               ap(1,lenh+ll,k) = ap(1,lenh+ll,k) + term(1,n,l,k)
          enddo                                                               
!                                                                               
!          imaginary part                                                       
!                                                                               
          do n = l+1, jcap, 2                                                  
               ap(2,lenh+ll,k) = ap(2,lenh+ll,k) + term(2,n,l,k)
          enddo                                                               
!                                                                               
!     compute the fourier coefficients for each level                           
!     -----------------------------------------------                           
!                                                                               
          evenr = ap(1,ll,k)                                                   
          eveni = ap(2,ll,k)                                                   
          ap(1,ll,k) = ap(1,ll,k) + ap(1,lenh+ll,k)                              
          ap(2,ll,k) = ap(2,ll,k) + ap(2,lenh+ll,k)                              
          ap(1,lenh+ll,k) = evenr - ap(1,lenh+ll,k)                             
          ap(2,lenh+ll,k) = eveni - ap(2,lenh+ll,k)                             
!
        enddo                                                                    
!
 1000 continue
#endif
      return                                                                    
      end   
