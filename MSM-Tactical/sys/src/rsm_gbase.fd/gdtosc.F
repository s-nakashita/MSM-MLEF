      subroutine gdtosc(grid,coef,km)                                             
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  gdtosc                                                           
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06          
!                                                                               
! abstract:  do regional grid to cos-cos wave transformation.                   
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call  gdtosc(grid,coef,km)                                            
!   input argument list:                                                        
!     grid	- grid field with dimension of (lngrd,km)                           
!     km	- the second dimension of grid and ff                                  
!                                                                               
!   output argument list:                                                       
!     coef     - wave coefficent with dimension of (lnwav,km)                    
!                                                                               
!   common block:                                                               
!	repro  - a temperory common block.                                            
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called:                                                         
!	ffasin  sumggc                                                                
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray ymp.                                                         
!                                                                               
!$$$                                                                            
#include <paramodel.h>
#include <rscompln.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define LNGRDS lngrdp
#define LNWAVS lnwavp
#else
#define LNGRDS lngrd
#define LNWAVS lnwav
#endif
      dimension grid(LNGRDS,km),coef(LNWAVS,km)            
#ifdef MP
      real, allocatable :: gridp1(:,:)
      real, allocatable :: gridp2(:,:,:)
      real, allocatable :: syn1(:,:)
      real, allocatable :: syn2(:,:,:)
      real, allocatable :: synpk1(:,:)   
      real, allocatable :: synpk2(:,:,:)
      real, allocatable :: coefpk1(:) 
      real, allocatable :: coefpk2(:,:)                      
#else
      real, allocatable :: syn(:,:)
#endif
!
#ifndef MP
      allocate (syn(igrd1,km))
      do k=1,km                                                                 
       do i=1,lnwav                                                            
        coef(i,k) = 0.0                                                           
       enddo                                                                    
      enddo                                                                     
      do j=1,jgrd1                                                          
        jlat=(j-1)*igrd1                                                    
        do k=1,km                                                               
         do i=1,igrd1                                                         
          ij=i+jlat                                                              
          syn(i,k) = grid(ij,k)                                                  
         enddo                                                                  
        enddo                                                                   
        call ffasinx1(syn,km,1,1,1)                                                 
        call sumggc1(syn,coef,gcosc(1,j),                               &
     &             lnwav,iwav1,jwav1,igrd1,km)                                     
      enddo 
      deallocate (syn)  
#else
      if(km.eq.1) then
        allocate (gridp1(igrd1p,jgrd1p))
        allocate (coefpk1(llwavp))
        allocate (synpk1(igrd1p,jgrd1))           
        allocate (syn1(igrd1,jgrd1p))      
        do i=1,llwavp                                                            
         coefpk1(i) = 0.0                                                           
        enddo                                                                    
        do j=1,latlen(mype)                                                          
          jlat=(j-1)*igrd1p
          do i=1,lonlen(mype)                                                        
            ij=i+jlat                                                              
            gridp1(i,j) = grid(ij,1)                                                  
          enddo                                                                  
        enddo 
        call rmpnx2x (gridp1,igrd1p,1,syn1,igrd1,1,jgrd1p,              &
     &                1,1,1)
        call ffasinx1(syn1,jgrd1p,1,1,1)                                                  
        call rmpny2nl(syn1,igrd1,jgrd1p,synpk1,igrd1p,jgrd1,            &
     &              1,1,1)
        call ffacosy1(synpk1,coefpk1,1,igrd1p,iwav1p,1,1)
        call rmpn2nn(coefpk1,llwavp,coef,lnwavp,1) 
        deallocate (syn1)
        deallocate (synpk1)
        deallocate (coefpk1)
        deallocate (gridp1)
!
      else if(km.eq.levr) then
!
        allocate (gridp2(igrd1p,levr,jgrd1p))           
        allocate (coefpk2(llwavp,levrp))           
        allocate (synpk2(igrd1p,levrp,jgrd1)) 
        allocate (syn2(igrd1,levrp,jgrd1p))
        do k=1,levrp                                                                 
         do i=1,llwavp                                                            
          coefpk2(i,k) = 0.0                                                           
         enddo                                                                    
        enddo                                                                     
        do j=1,latlen(mype)                                                           
          jlat=(j-1)*igrd1p
          do k=1,km                                                               
           do i=1,lonlen(mype)                                                        
            ij=i+jlat                                                              
            gridp2(i,k,j) = grid(ij,k)                                                  
           enddo                                                                  
          enddo                                                                   
        enddo 
        call rmpnx2nk(gridp2,igrd1p,levr,syn2,igrd1,levrp,              &
     &                jgrd1p,levr,levrp,1,1,1)
        call ffasinx1(syn2,jgrd1p,1,levrp,levrp)                                                  
        call rmpny2nl(syn2,igrd1,jgrd1p,synpk2,igrd1p,jgrd1,            &
     &              levrp,1,levrp)
!
        call ffacosy1(synpk2,coefpk2,1,igrd1p,iwav1p,levrp,levrp)
        call rmpnk2nn(coefpk2,llwavp,levrp,coef,lnwavp,levr,1)
        deallocate (syn2)
        deallocate (synpk2)
        deallocate (coefpk2) 	
        deallocate (gridp2) 
      else
        print *,' error:  km is not 1 or levr, gdtosc,km=',km
        call rmpabort
      endif              
#endif                                                                        
      return                                                                    
      end                                                                       
