!fpp$ noconcur r
      subroutine ffsncsy (a, b, c,d,lot)
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
#ifdef MP
#define IWAV1S iwav1p
#define IGRD1S igrd1p
#else
#define IWAV1S iwav1
#define IGRD1S igrd1
#endif
      parameter (lotmin=64,lotmax=64,ncpu=1)
      dimension a(jwav1,IWAV1S,lot),b(jwav1,IWAV1S,lot)
      dimension c(IGRD1S,jgrd1,lot),d(IGRD1S,jgrd1,lot)
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
!$omp parallel do private(k,i,j,l,lots,ll,al,table,work)
#endif
      do k=1,lot
     
      do 460 i=1,IWAV1S,nlot
      lots = min0(nlot, IWAV1S-i+1)
      do 360 j=i,i+lots-1
      do 320 l=1,jwav1
             al(2*l-1,j-i+1) =   a(l,j,k) 
             al(2*l  ,j-i+1) = - b(l,j,k) 
320   continue
      do 340 l=jwav1*2+1,jump
             al(l,j-i+1) = 0.0
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
      call dcrft(1,al,jumph,al,jump,jgrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
      call dcrft(0,al,jumph,al,jump,jgrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call scrft(1,al,jumph,al,jump,igrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
      call scrft(0,al,jumph,al,jump,igrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
#endif
!
      do 440 j=i,i+lots-1
      do 420 l=2,jgrd
             ll=jgrd2+2-l
             c(j,l,k) = ( al(l,j-i+1) + al(ll,j-i+1) )*0.5
             d(j,l,k) = ( al(l,j-i+1) - al(ll,j-i+1) )*0.5
420   continue
             c(     j,1,k) = al(     1,j-i+1)
             c(j, jgrd1,k) = al(jgrd1,j-i+1)
             d(     j,1,k) = 0.0
             d(j,jgrd1,k)  = 0.0
440   continue
460   continue
!
      enddo
!
      return
      end
