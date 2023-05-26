      subroutine sumffs1(fln,ap,qln,                                     & 
     &                  lnwavs,iwav1s,jwav1,igrd1s,n)
!fpp$ noconcur r                                                                
!                                                                               
! sum coefficient (fln) of sine function in y direction into ap                 
! before doing fft in x direction                                               
!                                                                               
      dimension ap(igrd1s,n), qln(jwav1), fln(lnwavs,n)                       
!c                                                                              
      do 2 k=1,n                                                                
!                                                                               
        do i=1,iwav1s
          sev = 0.0
          sod = 0.0
          jplus0 = (i-1)*jwav1
          do j=2,jwav1,2
            sev = sev + qln(j) * fln(j+jplus0,k)
            sod = sod + qln(j+1) * fln(j+1+jplus0,k)
          enddo
!                                                                               
          ap(i        ,k) =  sev + sod
!jun          ap(i+ igrd1s,k) =  sev - sod
        enddo
!                                                                               
    2 continue                                                                  
      return                                                                    
      end                                                                       
