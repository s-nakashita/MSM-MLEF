      subroutine sumgcs2 (ap,flc,fls,qln,qlm,                            &
     &      kx,ky,igrd1,lnwavs,iwav1,jwav1,lot,                          &  
     &      lat1,lat2,latdons)
! sum ap bp to flc fls                                                          
! cos and sin coefficients factor (qln,qlm) in y direction                      
! sum in y direction after doing fft in x direction                             
!                                                                               
      dimension ap(igrd1,lotsyn,ncpuss),qln(jwav1,jgrd1),                &
     &   flc(lnwavs,n), qlm(jwav1,jgrd1), fls(lnwavs,n)                       
!
#ifdef OPENMP
!$omp parallel do private(lat,lan,k,i,jplus0,j)
#endif
!
      do lat=lat1,lat2
        lan=lat-latdons

      do 2 k=1,lot
!
      do i=1,iwav1                                                         
!
       jplus0 = (i-1)*jwav1
       do j=1,jwav1
         flc(j+jplus0,k)=flc(j+jplus0,k)+qln(j,lat)*ap(i,k+kx-1,lan)
         fls(j+jplus0,k)=fls(j+jplus0,k)+qlm(j,lat)*ap(i,k+ky-1,lan)
       enddo
      enddo
!c                
    2 continue 
      enddo

      return  
      end    
