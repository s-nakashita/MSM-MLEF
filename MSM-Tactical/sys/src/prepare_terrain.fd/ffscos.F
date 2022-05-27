      subroutine ffscos (a, igrd1, iwav1, lot)
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
!$$$
      use comrfft

      dimension a(igrd1,lot)

! coefficient to grid ( for regional use )
! cosine function only in x direction.
!
!  multiple fast fourier transform - synthesis.  isign=1
!
      igrd2 = (igrd1-1) * 2
      jump  = igrd2 + 3
      nlot=lotmax
      do 460 i=1,lot,nlot
      lots = min0(nlot, lot-i+1)
      do 360 j=i,i+lots-1
      do 310 l=1,jump
             al(l,j-i+1) = 0.0
310   continue
      do 320 l=1,iwav1
             al(2*l-1,j-i+1) = a(l,j) 
320   continue
360   continue
!
!     call 1 fft for systhesis.
      call fft99m (al,work,trigs,ifax,1,jump,igrd2,lots,1)
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
