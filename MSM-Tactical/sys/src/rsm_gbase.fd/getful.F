      subroutine getful(grid,bgrd,sbgrd,ws,km)                                        
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  getful                                                           
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 94-02-06          
!                                                                               
! abstract:  obtained full regional grid field by adding base field to          
!            its perturbation.                                                  
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call getful(grid,bgrd,ws,km)                                        
!   input argument list:                                                        
!     grid	- perturbation grid values with dimension of (lngrd,km)             
!     bgrd	- base field values with dimension of (lngrdb,km)                    
!     ws	- interpolation coefficient for base field.                            
!     km	- the second dimension of grid and bgrd                                
!                                                                               
!   output argument list:                                                       
!     grid     - full field values of regional grids.                           
!    sbgrd     - interpolated base field values with dimension of (lngrd,km)
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called:                                                         
!	pint1                                                                         
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray ymp.                                                         
!                                                                               
!$$$                                                                            
#include <paramodel.h>
#include <rsparltb.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define IGRD1S igrd1p
#define LNGRDS  lngrdp
#else
#define IGRD1S igrd1
#define LNGRDS  lngrd
#endif
      dimension grid(LNGRDS,km)                                         &
     &         ,bgrd(lngrdb,km),ws(bgf,1-border:border)                 &
     &         ,sbgrd(LNGRDS,km)
      common /repro/ syn(IGRD1S,levr)                                         
#ifdef MP
      istr=lonstr(mype)
      ilen=lonlen(mype)
      jstr=latstr(mype)
      jlen=latlen(mype)
#else
      istr=1
      ilen=igrd1
      jstr=1
      jlen=jgrd1
#endif
      do j=1,jlen
        lat=jstr+j-1
        call pint1(bgrd,syn,ws,lat,km,IGRD1S,ib1,ib2,                   &
     &                 jb1,jb2,jbx,istr,ilen,                           &
     &                 bgf,border,jgrd1)
        jj=(j-1)*IGRD1S                                                    
        do k=1,km                                                               
         do i=1,ilen
           grid(i+jj,k) = grid(i+jj,k) + syn(i,k)                            
          sbgrd(i+jj,k) = syn(i,k)
         enddo                                                                  
        enddo                                                                   
      enddo                                                                     
      return                                                                    
      end                                                                       
