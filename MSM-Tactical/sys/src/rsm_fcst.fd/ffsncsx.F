!fpp$ noconcur r
      subroutine ffsncsx (a, b, jgrd1s,lot)
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
      dimension a(igrd1,jgrd1s,lot)
      dimension b(igrd1,jgrd1s,lot)
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
!  coeficient to grid ( for regional use )
!  cosine and sine function together in x direction.
!
!  multiple fast fourier transform - synthesis.  isign=1
!
      nlot=lotmax
#ifdef OPENMP
!$omp parallel do private(k,i,j,l,ll,lots,al,table,work)
#endif

      do k=1,lot

      do 460 i=1,jgrd1s,nlot
      lots = min0(nlot, jgrd1s-i+1)
      do 360 j=i,i+lots-1
      do 320 l=1,iwav1
             al(2*l-1,j-i+1) =   a(l,j,k) 
             al(2*l  ,j-i+1) = - b(l,j,k) 
320   continue
      do 340 l=iwav1*2+1,jump
             al(l,j-i+1) = 0.0
340   continue
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
      do 420 l=2,igrd
             ll=igrd2+2-l
             a(l,j,k) = ( al(l,j-i+1) + al(ll,j-i+1) )*0.5
             b(l,j,k) = ( al(l,j-i+1) - al(ll,j-i+1) )*0.5
420   continue
             a(    1,j,k) = al(     1,j-i+1)
             a(igrd1,j,k) = al(igrd1,j-i+1)
             b(    1,j,k) = 0.0
             b(igrd1,j,k) = 0.0
440   continue
460   continue
      enddo
!
      return
      end
