!fpp$ noconcur r
      subroutine ffssiny (a, lot)
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffssin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of sin wave in x direction.
!
! program history log:
!
! usage:   call  ffssin (a, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1
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
      dimension a(igrd1,lot)
!
#ifdef FF
      parameter (jumph=(jgrd2+2)/2,jump=jumph*2)
      dimension table(42000)
#endif
#ifdef FT
      parameter (jump=jumpry)
      dimension work(igrd2,lotmax,2)
      common /comrffti/ifax(20),jfax(20)
      common /comrfft/trigx(igrd2,2),trigy(jgrd2,2)
#endif
      dimension al(jump,lotmax)
!
! coeficient to grid ( for regional use )
! sine function only in x direction
!
!  multiple fast fourier transform - synthesis.  isign=1
!
!
      nlot=lotmax
#ifdef OPENMP
!$omp parallel do private(i,j,l,ll,lots,al,table,work)
#endif
      do 460 i=1,lot,nlot
      lots = min0(nlot, lot-i+1)
      do 360 j=i,i+lots-1
      do 310 l=1,jump
             al(l,j-i+1) = 0.0
310   continue
      do 320 l=1,iwav1
             al(2*l,j-i+1) = - a(l,j) 
320   continue
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
      call scrft(1,al,jumph,al,jump,jgrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
      call scrft(0,al,jumph,al,jump,jgrd2,lots,                         &
     &           -1,scale,table,22000,table(22001),20000)
#endif
!
      do 440 j=i,i+lots-1
      do 420 l=1,igrd1
             a(l,j) = al(l,j-i+1)
420   continue
440   continue
460   continue
!
      return
      end
