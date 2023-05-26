      subroutine sumgrd(lngrd,grid1,grid2,km)                                         
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  sumgrd                                                           
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06          
!                                                                               
! abstract:  sum of two grid field into one grid field                          
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call sumgrd(grid1,grid2,km)                                         
!   input argument list:                                                        
!     grid1	- the first grid field with dimension of (lngrd,km)                 
!     grid2	- the second grid field with dimension of (lngrd,km)                
!     km 	- the second dimension of grid1 and grid2                             
!                                                                               
!   output argument list:                                                       
!     grid1	- the sum grid fields with dimension of (lngrd,km)                  
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
#include <define.h>
      dimension grid1(lngrd,km),grid2(lngrd,km)                               
!                                                                               
!
#ifdef ORIGIN_THREAD
!$doacross share(grid1,grid2,km),local(j,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all                                                                    
!mic$1 shared(grid1,grid2,km)                                                   
!mic$1 private(j,k)                                                             
#endif
#ifdef OPENMP
!$omp parallel do private(j,k)
#endif
!
      do 10 k=1,km                                                              
      do 10 j=1,lngrd                                                          
      grid1(j,k) = grid1(j,k)+grid2(j,k)                                        
10    continue                                                                  
      return                                                                    
      end                                                                       
