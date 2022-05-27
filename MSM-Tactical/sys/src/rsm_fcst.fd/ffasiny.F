!fpp$ noconcur r
      subroutine ffasiny (a, lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffasin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of sin wave in x direction.
!
! program history log:
!
! usage:   call  ffasin (a, lot)
!   input argument list:
!     a   - grid values with dimension of jgrd1
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
      dimension a(lot,jgrd1)
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
! grid to coefficient ( for regional use)
! sine function only in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
      nlot=lotmax
      do 660 i=1,lot,nlot
      lots = min0(nlot, lot-i+1)
      do 560 j=i,i+lots-1
      do 510 l=1,jgrd1
             al(l,j-i+1) = a(j,l)
510   continue
      do 520 l=jgrd1+1,jgrd2
             ll=jgrd2+2-l
             al(l,j-i+1) = - a(j,ll)
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
      call drcft(1,al,jump,al,jumph,jgrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
      call drcft(0,al,jump,al,jumph,jgrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call srcft(1,al,jump,al,jumph,jgrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
      call srcft(0,al,jump,al,jumph,jgrd2,lots,                         &
     &           1,scale,table,22000,table(22001),20000)
#endif
!
      do 640 j=i,i+lots-1
      do 620 l=1,jwav1
             a(j,l) = - al(2*l,j-i+1) 
620   continue
      do 630 l=jwav1+1,jgrd1
             a(j,l) = 0.0
630   continue
640   continue
660   continue
!
      return
      end
