      subroutine shalfo(a,km)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  shalfo                                                           
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06          
!                                                                               
! abstract:  transform model grid (i,j) to regular grid (2i,j/2) with           
!            north and south grid combined together as global model for         
!            regional model computation.                                        
!            the model grid is                                                  
!               (1,1),(2,1),.......(i,1),(1,j  ),(2,j  ),...,(i,j  )            
!               (1,2),(2,2),......,(i,2),(1,j-1),(2,j-1),...,(i,j-1)            
!                ...................................................            
!               (1,j/2),(2,j/2),..,(i,j/2),(1,j-j/2+1),...,(i,j-j/2+1)          
!            the regular grid is                                                
!               (1,1),(2,1),.......(i,1)                                        
!               (1,2),(2,2),.......(i,2)                                        
!               ........................                                        
!               (1,j),(2,j),.......(i,j)                                        
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call shalfo(a,k)                                                    
!   input argument list:                                                        
!     a         - model grid with dimension (i,j,k)                                    
!     k         - the third dimension of a                                             
!                                                                               
!   output argument list:                                                       
!     a         - regular output grid with dimension (i,j,k)                           
!                                                                               
!   common block:                                                               
!      /repro/                                                                  
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called: none                                                    
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray ymp.                                                         
!                                                                               
!$$$                                                                            
#include <paramodel.h>
!
! arrage j row for output (regional regular grid)
!
      dimension a(igrd1,jgrd1,km)
      dimension tmp(igrd1,jgrd1)
!
      do 10 k=1,km
      do 20 j=1,jgrd12
      jj = 2*j-1
      js = j
      jn = jgrd1+1 - j
      do 30 i=1,igrd1
      tmp(i,js) = a(i,jj  ,k)
      tmp(i,jn) = a(i,jj+1,k)
 30   continue
 20   continue
      do 40 j=1,jgrd1
      do 40 i=1,igrd1
      a(i,j,k) = tmp(i,j)
 40   continue
 10   continue
!
      return
      end
