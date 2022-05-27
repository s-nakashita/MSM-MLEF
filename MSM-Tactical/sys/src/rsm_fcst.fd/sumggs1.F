      subroutine sumggs1(ap,fln,qln,                                     &  
     &                   lnwavs,iwav1s,jwav1,igrd1s,n)
!                                                                               
! sum coefficient of sine function at ap in y direction                         
! into fln after fft                                                            
!                                                                               
      dimension ap(igrd1s,n), qln(jwav1), fln(lnwavs,n)                       
!
#ifdef ORIGIN_THREAD
!$doacross local(sev,sod,i,j,k,iplus0)
#endif
#ifdef CRAY_THREAD
!mic$ do all private(sev,sod) autoscope
#endif
#ifdef OPENMP
!$omp parallel do private(sev,sod,i,j,k,jplus0)
#endif
!
      do 2 k=1,n                                                                
!                                                                               
      do i=1,iwav1s                                                         
!jun        sev =  ap(i,k) + ap(i+igrd1s,k)                                       
!jun        sod =  ap(i,k) - ap(i+igrd1s,k)                                       
        sev =  ap(i,k)
        sod =  ap(i,k)
!                                                                               
        jplus0 = (i-1)*jwav1
        do j=2,jwav1,2
          fln(j+jplus0,k) = fln(j+jplus0,k) + qln(j) * sev
          fln(j+1+jplus0,k) = fln(j+1+jplus0,k) + qln(j+1) * sod
        enddo
      enddo
!c                                                                              
    2 continue                                                                  
      return                                                                    
      end                                                                       
