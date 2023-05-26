      subroutine epslon(eps,jcapx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    epslon      computes eps, a function of wave number.
!   prgmmr: joseph sela      org: w/nmc23    date: 88-04-01
!
! abstract: computes eps, a function of wave number.
!   eps is used in calculating legendre polys. and their derivatives.
!   eps is also used in computing winds from divergence and vorticity.
!
! program history log:
!   88-04-01  joseph sela
!
! usage:    call epslon (eps, jcap)
!   input argument list:
!     jcap     - index indicating the spectral truncation used.
!
!   output argument list:
!     eps      - array computed from sqrt((n**2-l**2)/(4*n**2-1)).
!
! attributes:
!   language: fortran 200.
!   machine:  cyber 205.
!
!$$$
!ccj
#include <paramodel.h>
!cc
      dimension eps(jcap1,jcap2)
      jcapx1 = jcapx + 1
      jcapx2 = jcapx + 2
      do 1 ll=1,jcapx1
      l = ll - 1
      do 1 inde=2,jcapx2
      n = l + inde - 1
      a = (n*n - l*l) / (4.0 * n*n - 1.0)
      eps(ll,inde)=sqrt(a)
1     continue
      do 2 ll=1,jcap1
      eps(ll,1) = 0.0e0
2     continue
      return
      end
