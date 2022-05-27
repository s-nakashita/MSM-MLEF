      subroutine sumfcs (flc,fls,ap,bp,qln,qlm,                          & 
     &                   igrd12s,lnwavs,iwav1s,jwav1,igrd1s,n)
!fpp$ noconcur r                                                                
! cos and sin coefficients (flc,fls) in x direction                             
! cos and sin coefficients factor (qln,qlm) in y direction                      
! sum in y direction before doing fft in x direction                            
!                                                                               
      dimension ap(igrd12s,n), qln(jwav1), flc(lnwavs,n)        
     &         ,bp(igrd12s,n), qlm(jwav1), fls(lnwavs,n)        
!c                                                               
      do 2 k=1,n                                                 
!                                                                
        do i=1,iwav1s                                            
          cev = qln(1) * flc((i-1)*jwav1+1,k)
          sev = 0.0
          cod = 0.0
          sod = 0.0
!c                                                               
          jplus0 = (i-1)*jwav1
          do j=2,jwav1,2
            cod = cod + qln(j) * flc(j+jplus0,k)
            sev = sev + qlm(j) * fls(j+jplus0,k)
            cev = cev + qln(j+1) * flc(j+1+jplus0,k)
            sod = sod + qlm(j+1) * fls(j+1+jplus0,k)
          enddo
!                                                               
          ap(       i,k) =  cev+cod
          ap(i+igrd1s,k) =  cev-cod
          bp(       i,k) =  sev+sod
          bp(i+igrd1s,k) =  sev-sod
        enddo
!                                                                
    2 continue 
      return  
      end    
