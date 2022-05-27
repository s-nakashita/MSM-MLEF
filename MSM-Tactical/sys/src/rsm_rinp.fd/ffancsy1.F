!fpp$ noconcur r
      subroutine ffancsy1(a, b,c,kx,ky,igrd1s,iwav1s,lotsynk,lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffancs
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of cos and sin waves in x direction.
!            for regional spectral model.
!
! program history log:
!
! usage:   call  ffancs (a,b, lot)
!   input argument list:
!     a   - grid values with dimension of jgrd1
!     b   - grid values with dimension of jgrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - coefficient values as cos wave
!     b     - coefficient values as sin wave
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
!$$$
#include <paramodel.h>
      parameter (lotmin=64,lotmax=64,ncpu=1)
      dimension a(igrd1s,lotsynk,jgrd1)
      dimension b(jwav1,iwav1s,lot)
      dimension c(jwav1,iwav1s,lot)
!
#ifdef FF
      parameter (jumph=(jgrd2+2)/2,jump=jumph*2)
      dimension table(42000)
#endif
#ifdef FT
      parameter (jump=jumpry)
      dimension work(jgrd2,lotmax,2)
      common /comrffti/ifax(20),jfax(20)
      common /comrfft/trigx(igrd2,2),trigy(jgrd2,2)
#endif
      dimension al(jump,lotmax)
!
! grid to coefficient ( for regional use )
! cosine and sine function together in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
      nlot=lotmax
#ifdef OPENMP
!!$omp parallel do private(k,j,l,i,lots,ll)
#endif
      do k=1,lot

      do 660 i=1,iwav1s,nlot
      lots = min0(nlot, iwav1s-i+1)
      do 560 j=i,i+lots-1
      do 510 l=2,jgrd
         al(l,j-i+1) = a(j,k+kx-1,l)+a(j,k+ky-1,l)
510   continue
             al(     1,j-i+1) = a(     j,k+kx-1,1)
             al(jgrd1,j-i+1) = a(j,k+kx-1,jgrd1)
      do 520 l=jgrd1+1,jgrd2
             ll=jgrd2+2-l
             al(l,j-i+1) = a(j,k+kx-1,ll)-a(j,k+ky-1,ll)
520   continue
      do 540 l=jgrd2+1,jump
             al(l,j-i+1) = 0.0
540   continue
560   continue
!
!     call -1 fft for analysis.
#ifdef FTC
      call rfftmlt(al,work,trigy,jfax,1,jump,jgrd2,lots,-1)
#endif
#ifdef FT9
      call fft99m (al,work,trigy,jfax,1,jump,jgrd2,lots,-1)
#endif
#ifdef FF
      scale=1./float(jgrd2)
#endif
#ifdef FFD
      call drcft(1,al,jump,al,jumph,jgrd2,lots,                          &        
     &           1,scale,table,22000,table(22001),20000)
      call drcft(0,al,jump,al,jumph,jgrd2,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call srcft(1,al,jump,al,jumph,jgrd2,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
      call srcft(0,al,jump,al,jumph,jgrd2,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
#endif
!
      do 640 j=i,i+lots-1
      do 620 l=1,jwav1
             b(l,j,k) =   al(2*l-1,j-i+1) 
             c(l,j,k) = - al(2*l  ,j-i+1) 
620   continue
640   continue
660   continue
!
      enddo

      return
      end
