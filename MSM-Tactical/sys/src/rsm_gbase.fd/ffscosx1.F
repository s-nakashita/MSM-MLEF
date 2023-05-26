!fpp$ noconcur r
      subroutine ffscosx1(a,jgrd1s,kx,lotsynk, lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffscos
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of cos wave in x direction.
!
! program history log:
!
! usage:   call  ffscos (a, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1_
!     lot - second dimension of a
!
!   output argument list:
!     a     - grid values
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
      dimension a(igrd1,lotsynk,jgrd1s)
!
#ifdef FF
      parameter (jumph=(igrd2+2)/2,jump=jumph*2)
      dimension table(42000)
#endif
#ifdef FT
      parameter (jump=jumpr)
      dimension work(igrd2,lotmax,2)
      common /comrffti/ifax(20),jfax(20)
      common /comrfft/trigx(igrd2,2),trigy(jgrd2,2)
#endif
      dimension al(jump,lotmax)
!
! coefficient to grid ( for regional use )
! cosine function only in x direction.
!
!  multiple fast fourier transform - synthesis.  isign=1
!
      nlot=lotmax
#ifdef OPENMP
!$omp parallel do private(k,i,j,l,lots,al,table,work)
#endif
      do k=1,lot

      do 460 i=1,jgrd1s,nlot
      lots = min0(nlot, jgrd1s-i+1)
      do 360 j=i,i+lots-1
      do 310 l=1,jump
             al(l,j-i+1) = 0.0
310   continue
      do 320 l=1,iwav1
             al(2*l-1,j-i+1) = a(l,k+kx-1,j) 
320   continue
360   continue
!
!     call 1 fft for systhesis.
#ifdef FTC
      call rfftmlt(al,work,trigx,ifax,1,jump,igrd2,lots,1)
#endif
#ifdef FT9
      call fft99m (al,work,trigx,ifax,1,jump,igrd2,lots,1)
#endif
#ifdef FF
      scale=1.0
#endif
#ifdef FFD
      call dcrft(1,al,jumph,al,jump,igrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
      call dcrft(0,al,jumph,al,jump,igrd2,lots,                         &
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
      do 420 l=1,igrd1
             a(l,kx+k-1,j) = al(l,j-i+1)
420   continue
440   continue
460   continue

       enddo
!
      return
      end
