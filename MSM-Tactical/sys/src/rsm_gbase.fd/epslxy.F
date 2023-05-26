      subroutine epslxy(lnwav,iwav1,jwav1,igrd,jgrd,                    &
     &                  epsx,epsy,snnp1,rnnp1,delx,dely)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:   epslxy                                                          
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06          
!                                                                               
! abstract:  prepare derivative and laplacian for regional wave field.          
!            for regional spectral model.                                       
!                                                                               
! program history log:                                                          
!                                                                               
! usage:   epslxy(epsx,epsy,snnp1,rnnp1,delx,dely)                              
!   input argument list:                                                        
!     delx	- grid spacing in x direction                                        
!     dely	- grid spacing in y direction                                        
!                                                                               
!   output argument list:                                                       
!     epsx	- coefficient for x derivative.                                      
!     epsy	- coefficient for y derivative.                                      
!     snnp1	- coefficient for laplacian operator.                               
!     rnnp1	- coefficient for inverse laplacian operator                        
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called: none                                                    
!	asin	- intrinsic function asin.                                               
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray ymp.                                                         
!                                                                               
!$$$                                                                            
      dimension epsx(lnwav), epsy(lnwav)                                &
     &       , snnp1(lnwav),rnnp1(lnwav)                                      
!                                                                               
      pi = asin(1.0d0) * 2.0d0
      pirlx = pi / (delx * igrd)                                               
      pirly = pi / (dely * jgrd)                                               
      nn = 0
      do 10 kk=1,iwav1                                                         
      k = kk - 1                                                                
      do 10 ll=1,jwav1                                                         
      l = ll - 1                                                                
      nn = nn + 1                                                              
      epsx(nn) = float(k)*pirlx                                                 
      epsy(nn) = float(l)*pirly                                                 
      snnp1(nn)= epsx(nn)*epsx(nn) + epsy(nn)*epsy(nn)                          
      rnnp1(nn)=0.0e0                                                      
      if( snnp1(nn) .ne. 0.0e0 ) rnnp1(nn)=1.0e0 / snnp1(nn)          
10    continue                                                                  
!cc   print *,' epsx ',(epsx(n),n=1, lnwav)                                    
!cc   print *,' epsy ',(epsy(n),n=1, lnwav)                                    
!cc   print *,' snnp1 ',(snnp1(n),n=1, lnwav)                                  
!cc   print *,' rnnp1 ',(rnnp1(n),n=1, lnwav)                                  
!cc   print *,' ----- end epslxy ----- '                                        
      return                                                                    
      end                                                                       
