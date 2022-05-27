!fpp$ noconcur r
      subroutine ffsncsy1(a, b, c,kx,ky,iwav1s,igrd1s,
     &    lotsyn,lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffsncs
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of cos and sin waves
!            in x direction.
!
! program history log:
!
! usage:   call  ffsncs (a,b, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1
!     b   - coefficient values with dimension of igrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - grid values as cos wave
!     b     - grid values as sin wave
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
      dimension a(jwav1,iwav1s,lot),b(jwav1,iwav1s,lot)
      dimension c(igrd1s,lotsyn,jgrd1)
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
!  coeficient to grid ( for regional use )
!  cosine and sine function together in x direction.
!
!  multiple fast fourier transform - synthesis.  isign=1
!
      nlot=lotmax
#ifdef OPENMP
!!$omp parallel do private(k,j,l,i,lots,ll)
#endif
      do k=1,lot
     
      do 460 l=1,iwav1s,nlot
      lots = min0(nlot, iwav1s-l+1)
      do 360 i=l,l+lots-1
      do 320 j=1,jwav1
             al(2*j-1,i-l+1) =   a(j,i,k) 
             al(2*j  ,i-l+1) = - b(j,i,k) 
320   continue
      do 340 j=jwav1*2+1,jump
             al(j,i-l+1) = 0.0
340   continue
360   continue
!
!     call 1 fft for systhesis.
#ifdef FTC
      call rfftmlt(al,work,trigy,jfax,1,jump,jgrd2,lots,1)
#endif
#ifdef FT9
      call fft99m (al,work,trigy,jfax,1,jump,jgrd2,lots,1)
#endif
#ifdef FF
      scale=1.0
#endif
#ifdef FFD
      call dcrft(1,al,jumph,al,jump,jgrd2,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
      call dcrft(0,al,jumph,al,jump,jgrd2,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call scrft(1,al,jumph,al,jump,jgrd2,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
      call scrft(0,al,jumph,al,jump,jgrd2,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
#endif
!
      do 440 i=l,l+lots-1
      do 420 j=2,jgrd
             jj=jgrd2+2-j
             c(i,kx+k-1,j) = ( al(j,i-l+1) + al(jj,i-l+1) )*0.5
             c(i,ky+k-1,j) = ( al(j,i-l+1) - al(jj,i-l+1) )*0.5
420   continue

             c(     i,kx+k-1,1) = al(     1,i-l+1)
             c(i,kx+k-1, jgrd1) = al(jgrd1,i-l+1)
             c(i,ky+k-1,     1) = 0.0
             c(i,ky+k-1, jgrd1) = 0.0
440   continue
460   continue
      enddo
!
      return
      end
