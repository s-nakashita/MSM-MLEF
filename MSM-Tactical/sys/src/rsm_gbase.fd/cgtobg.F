      subroutine cgtobg(a,b,km)
!
! cubic interpolation
!
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  gg2rg                                                            
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06          
!                                                                               
! abstract:  do gaussian gloal grid to regional grid interpolation.             
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call gg2rg(a,b,km)                                                  
!   input argument list:                                                        
!     a         - gaussian grid from global field.                                     
!     km        - the second dimension of a and b                                      
!                                                                               
!   output argument list:                                                       
!     b         - regional grid.                                                       
!                                                                               
!   common block:                                                               
!       comg2r                                                                        
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
!ccj
#include <paramodel.h>
#include <rsparltb.h>
!cc
      dimension a(clngrd,km),b(lngrdb,km)
!
      common/comctobi/      n11(lngrdb)
      common/comctob/                                                   &
     &          xc0(lngrdb),xc1(lngrdb),xc2(lngrdb),xc3(lngrdb)         &
     &         ,yc0(lngrdb),yc1(lngrdb),yc2(lngrdb),yc3(lngrdb)         
!
      dimension a00(lngrdb),a01(lngrdb),a02(lngrdb),a03(lngrdb)         &
     &         ,a10(lngrdb),a11(lngrdb),a12(lngrdb),a13(lngrdb)         &
     &         ,a20(lngrdb),a21(lngrdb),a22(lngrdb),a23(lngrdb)         &
     &         ,a30(lngrdb),a31(lngrdb),a32(lngrdb),a33(lngrdb)
!
#ifdef ORIGIN_THREAD
!$doacross share(a,b,n11,lngrdb,km,xc0,xc1,xc2,xc3,yc0,yc1,yc2,yc3),
!$&        local(a00,a01,a02,a03,n,ij,k,a10,a11,a12,a13,
!$&              a20,a21,a22,a23,a30,a31,a32,a33)
#endif
#ifdef CRAY_THREAD
!mic$ do all                                                                    
!mic$1 shared(a,b)                                                              
!mic$1 shared(n11,lngrdb,km)                                                    
!mic$1 shared(xc0,xc1,xc2,xc3)                                                  
!mic$1 shared(yc0,yc1,yc2,yc3)                                                  
!mic$1 private(a00,a01,a02,a03,n,ij,k)                                          
!mic$1 private(a10,a11,a12,a13)                                                 
!mic$1 private(a20,a21,a22,a23)                                                 
!mic$1 private(a30,a31,a32,a33)                                                 
#endif
#ifdef OPENMP
!$omp parallel do private(n,ij,k), &
!$omp&            private(a00,a01,a02,a03), &
!$omp&            private(a10,a11,a12,a13), &
!$omp&            private(a20,a21,a22,a23), &
!$omp&            private(a30,a31,a32,a33)
#endif
      do 5000 k=1,km
      do 2000 n=1,lngrdb
      ij=n11(n)-cigrd1
      a00(n) = a(ij-1,k)
      a10(n) = a(ij  ,k)
      a20(n) = a(ij+1,k)
      a30(n) = a(ij+2,k)
      ij=n11(n)
      a01(n) = a(ij-1,k)
      a11(n) = a(ij  ,k)
      a21(n) = a(ij+1,k)
      a31(n) = a(ij+2,k)
      ij=n11(n)+cigrd1
      a02(n) = a(ij-1,k)
      a12(n) = a(ij  ,k)
      a22(n) = a(ij+1,k)
      a32(n) = a(ij+2,k)
      ij=n11(n)+cigrd1*2
      a03(n) = a(ij-1,k)
      a13(n) = a(ij  ,k)
      a23(n) = a(ij+1,k)
      a33(n) = a(ij+2,k)
2000  continue
      do 3000 n=1,lngrdb
        b(n,k) = xc3(n)*                                                &
     &       (yc3(n)*a33(n)+yc2(n)*a32(n)+yc1(n)*a31(n)+yc0(n)*a30(n))  &
     &         + xc2(n)*                                                &
     &       (yc3(n)*a23(n)+yc2(n)*a22(n)+yc1(n)*a21(n)+yc0(n)*a20(n))  &
     &         + xc1(n)*                                                &
     &       (yc3(n)*a13(n)+yc2(n)*a12(n)+yc1(n)*a11(n)+yc0(n)*a10(n))  &
     &         + xc0(n)*                                                &
     &       (yc3(n)*a03(n)+yc2(n)*a02(n)+yc1(n)*a01(n)+yc0(n)*a00(n))
3000  continue
!
5000  continue
!
      return
      end
