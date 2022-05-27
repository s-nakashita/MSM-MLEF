      subroutine rprntb(a,jcap1,ch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rprntb
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  print the values by scaling.
!
! program history log:
!
! usage:    call rprntb(a,jcap1,ch)
!   input argument list:
!     a   - coefficent with dimension of (jcap1)
!     jcap1 - the dimensions of a
!     ch  - charcter string to indicate which variable is printed
!
!   output argument list:
!     fpl     - wave coefficent with dimension of (2,jcap1_,lot)
!     fml     - wave coefficent with dimension of (2,jcap1_,lot)
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
      dimension a(jcap1)
      character*(*) ch
      r = -1.e+20
      do 1 j=1,jcap1
      if (abs(a(j)).gt.r) r=abs(a(j))
1     continue
      print *,' vector of ',ch
      print 100, r
100   format (1h0, 'scale of vector  =', e12.5)
      if(r.eq.0.e0)return
      do 2 j=1,jcap1
      a(j) = a(j) / r
2     continue
      print 101, (a(j), j=1,jcap1)
101   format (1x, 18(f6.3, 1x))
      do 4 j=1,jcap1
      a(j) = a(j) * r
4     continue
      return
      end
