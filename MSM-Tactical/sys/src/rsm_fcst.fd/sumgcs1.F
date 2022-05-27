      subroutine sumgcs1(ap,bp,flc,fls,qln,qlm,                          &  
     &                   igrd12s,lnwavs,iwav1s,jwav1,igrd1s,n)
! sum ap bp to flc fls                                                          
! cos and sin coefficients factor (qln,qlm) in y direction                      
! sum in y direction after doing fft in x direction                             
!                                                                               
      dimension ap(igrd12s,n), qln(jwav1), flc(lnwavs,n)                 &     
     &         ,bp(igrd12s,n), qlm(jwav1), fls(lnwavs,n)                       
!c                                                                              
!
#ifdef ORIGIN_THREAD
!$doacross local(sev,sod,cev,cod,i,j,k,iplus0)
#endif
#ifdef CRAY_THREAD
!mic$ do all private(sev,sod,cev,cod) autoscope                                 
#endif
#ifdef OPENMP
!$omp parallel do private(sev,sod,cev,cod,i,j,k,jplus0)
#endif
!
      do 2 k=1,n 
!                                                                               
      do i=1,iwav1s                                                         
!jun         cev =  ap(i,k) + ap(i+igrd1s,k)                                     
!jun         cod =  ap(i,k) - ap(i+igrd1s,k)                                     
!jun         sev =  bp(i,k) + bp(i+igrd1s,k)                                     
!jun         sod =  bp(i,k) - bp(i+igrd1s,k)                                     
         cev =  ap(i,k)
         cod =  ap(i,k)
         sev =  bp(i,k)
         sod =  bp(i,k)
!                                                                               
         jplus0 = (i-1)*jwav1
         flc(jplus0+1,k) = flc(jplus0+1,k) + qln(1) * cev
!                                                                               
         do j=2,jwav1,2
            flc(j+jplus0,k) = flc(j+jplus0,k) + qln(j) * cod
            fls(j+jplus0,k) = fls(j+jplus0,k) + qlm(j) * sev
            flc(j+1+jplus0,k) = flc(j+1+jplus0,k) + qln(j+1) * cev
            fls(j+1+jplus0,k) = fls(j+1+jplus0,k) + qlm(j+1) * sod
         enddo
      enddo
!c                                                                              
    2 continue 
      return  
      end    
