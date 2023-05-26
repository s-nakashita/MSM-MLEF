      subroutine sumfcs1(flc,fls,ap,qln,qlm,                             &
     &   kx,ky,llwavs,iwav1s,jwav1,igrd1,jgrd1,lotsynk,lot,              &
     &   ncpuss,lat1,lat2,latdons)
!fpp$ noconcur r                                                                
! cos and sin coefficients (flc,fls) in x direction                             
! cos and sin coefficients factor (qln,qlm) in y direction                      
! sum in y direction before doing fft in x direction                            
      dimension ap(igrd1,lotsynk,ncpuss), qln(jwav1,jgrd1),              &
     &     flc(llwavs,lot)        
      dimension                           qlm(jwav1,jgrd1),              &
     &     fls(llwavs,lot)        
!c                                                               
      do l=lat1,lat2
        lan=l-latdons
!
       do 2 k=1,lot                                               
!                                                                
        do i=1,iwav1s                                           
          cev = qln(1,l) * flc((i-1)*jwav1+1,k)
          sev = 0.0
          cod = 0.0
          sod = 0.0
!c                                                               
          jplus0 = (i-1)*jwav1
          do j=2,jwav1,2
            cod = cod + qln(j,l) * flc(j+jplus0,k)
            sev = sev + qlm(j,l) * fls(j+jplus0,k)
            cev = cev + qln(j+1,l) * flc(j+1+jplus0,k)
            sod = sod + qlm(j+1,l) * fls(j+1+jplus0,k)
          enddo
!                                                               
          ap(       i,k+kx-1,lan) =  cev+cod
          ap(       i,k+ky-1,lan) =  sev+sod
        enddo
!                                                                
    2 continue 
      enddo


      return  
      end    
