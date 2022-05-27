      subroutine poly(n,rad,p)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    poly        evaluates legendre polynomial.
!   prgmmr: joseph sela      org: w/nmc23    date: 88-04-01
!
! abstract: evaluates the unnormalized legendre polynomial
!   of specified degree at a given colatitude using a standard
!   recursion formula.  real arithmetic is used.
!
! program history log:
!   88-04-01  joseph sela
!
! usage:    call poly (n, rad, p)
!   input argument list:
!     n        - degree of legendre polynomial.
!     rad      - real colatitude in radians.
!
!   output argument list:
!     p        - real value of legendre polynomial.
!
! attributes:
!   language: fortran 200.
!   machine:  cyber 205.
!
!$$$
      x = cos(rad)
      y1 = 1.0
      y2=x
      do 1 i=2,n
      g=x*y2
      y3=g-y1+g-(g-y1)/float(i)
      y1=y2
      y2=y3
1     continue
      p=y3
      return
      end
