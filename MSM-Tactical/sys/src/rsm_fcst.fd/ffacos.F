!fpp$ noconcur r
      subroutine ffacos (a, lot)
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffacos
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of cos wave in x direction.
!            for regional spectral model.
!
! program history log:
!
! usage:   call  ffacos (a, lot)
!   input argument list:
!     a   - grid values with dimension of igrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - coefficient values
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
! grid to coefficient ( for regional use )
! cosine function only in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
      nlot=lotmax
      do 660 i=1,lot,nlot
      lots = min0(nlot, lot-i+1)
      do 560 j=i,i+lots-1
      do 510 l=1,igrd1
             al(l,j-i+1) = a(l,j)
510   continue
      do 520 l=igrd1+1,igrd2
             ll=igrd2+2-l
             al(l,j-i+1) = a(ll,j)
520   continue
      do 540 l=igrd2+1,jump
             al(l,j-i+1) = 0.0
540   continue
560   continue
!
!     call -1 fft for analysis.
#ifdef FTC
      call rfftmlt(al,work,trigx,ifax,1,jump,igrd2,lots,-1)
#endif
#ifdef FT9
      call fft99m (al,work,trigx,ifax,1,jump,igrd2,lots,-1)
#endif
#ifdef FF
      scale=1./float(igrd2)
#endif
#ifdef FFD
      call drcft(1,al,jump,al,jumph,igrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
      call drcft(0,al,jump,al,jumph,igrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call srcft(1,al,jump,al,jumph,igrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
      call srcft(0,al,jump,al,jumph,igrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
#endif
!
      do 640 j=i,i+lots-1
      do 620 l=1,iwav1
             a(l,j) = al(2*l-1,j-i+1) 
620   continue
      do 630 l=iwav1+1,igrd1
             a(l,j) = 0.0
630   continue
640   continue
660   continue
!
      return
      end
