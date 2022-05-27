      subroutine sctogd(coef,grid,km)                                             
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  sctogd                                                           
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 94-02-06          
!                                                                               
! abstract:  transform regional sin-cos wave coefficient                        
!            to regional grids.                                                 
!	     for regional spectral model.                                             
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call sctogd(coef,grid,km)                                             
!   input argument list:                                                        
!     coef	- wave coefficent with dimension of lnwav                             
!     km	- vertical dimension.                                                  
!                                                                               
!   output argument list:                                                       
!     grid     - grid field with dimension of lngrd                            
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called:                                                         
!	sumffc	- summation coefficient of cos wave in y direction                     
!	ffssin	- fast fourier coefficient transform of sin wave in                    
!                 x direction.                                                  
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray c90.                                                         
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
      dimension coef(LNWAVS,km),grid(LNGRDS,km)
#ifdef MP
      real, allocatable :: syn1(:,:)
      real, allocatable :: syn2(:,:,:)
      real, allocatable :: synpk1(:,:)   
      real, allocatable :: synpk2(:,:,:)
      real, allocatable :: coefpk1(:) 
      real, allocatable :: coefpk2(:,:)              
      real, allocatable :: gridp1(:,:)
      real, allocatable :: gridp2(:,:,:)
#else
      real, allocatable :: syn(:,:)
#endif
!
#ifndef MP
      allocate (syn(igrd1,km))
      do lat=1,jgrd1                                                          
        call sumffc1(coef,syn,ccosg(1,lat),                               &
     &             lnwav,iwav1,jwav1,igrd1,km)                                     
        call ffssinx1(syn,km,1,1,1 )                                                  
        jlat=(lat-1)*igrd1                                                    
        do k=1,km                                                               
         do i=1,igrd1                                                         
          j=i+jlat                                                              
          grid(j,k) = syn(i,k)                                                  
         enddo                                                                  
        enddo                                                                   
      enddo
      deallocate (syn)         
#else      
      if(km.eq.1) then
!      
        allocate (gridp1(igrd1p,jgrd1p))       
        allocate (coefpk1(llwavp))
        allocate (synpk1(igrd1p,jgrd1))           
        allocate (syn1(igrd1,jgrd1p))
        call rmpnn2n (coef,lnwavp,coefpk1,llwavp,1)
        call ffscosy1(coefpk1,synpk1,1,iwav1p,igrd1p,1,1)
!
        call rmpnl2ny(synpk1,igrd1p,jgrd1,syn1,igrd1,jgrd1p,             &   
     &                  1,1,1)
        call ffssinx1(syn1,jgrd1p,1,1,1 )                                                  
        call rmpx2nx(syn1,igrd1,1,gridp1,igrd1p,1,jgrd1p,                &
     &               1,1,1)
        do j=1,latlen(mype)                                                           
          jlat=(j-1)*igrd1p
           do i=1,lonlen(mype)                                                        
            ij=i+jlat                                                              
            grid(ij,1)=gridp1(i,j)
           enddo                                                                  
        enddo 
        deallocate (gridp1)	
        deallocate (syn1)
        deallocate (synpk1)
        deallocate (coefpk1)
!     
      else if(km.eq.levr) then
!      
        allocate (coefpk2(llwavp,levrp))           
        allocate (synpk2(igrd1p,levrp,jgrd1)) 
        allocate (syn2(igrd1,levrp,jgrd1p))
        allocate (gridp2(igrd1p,levr,jgrd1p))           

        call rmpnn2nk(coef,lnwavp,levr,coefpk2,llwavp,levrp,1)
        call ffscosy1(coefpk2,synpk2,1, iwav1p,igrd1p,levrp,levrp)
        call rmpnl2ny(synpk2,igrd1p,jgrd1,syn2,igrd1,jgrd1p,             &  
     &              levrp,1,levrp)
        call ffssinx1(syn2,jgrd1p,1,levrp,levrp )                                                  
        call rmpnk2nx(syn2,igrd1,levrp,gridp2,igrd1p,levr,               &
     &                jgrd1p,levrp,levr,1,1,1)
        do j=1,latlen(mype)
          jlat=(j-1)*igrd1p
          do k=1,km                                                               
           do i=1,lonlen(mype)                                                        
            ij=i+jlat                                                              
            grid(ij,k)=gridp2(i,k,j)
           enddo                                                                  
          enddo                                                                   
        enddo 
        deallocate (syn2)
        deallocate (synpk2)
        deallocate (coefpk2)  
        deallocate (gridp2)
      else
        print *,' error: km not = 1 or levr '
        call rmpabort
      endif
#endif
!
      return                                                                    
      end                                                                       
