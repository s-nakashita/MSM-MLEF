      subroutine glats(lgghaf,colrad,wgt,wgtcs,rcs2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    glats       computes location of gaussian latitudes.
!   prgmmr: joseph sela      org: w/nmc23    date: 88-04-05
!
! abstract: computes the location of the gaussian latitudes for the
!   input lgghaf.  the latitudes are determined by finding
!   the zeros of the legendre polynomials.
!
! program history log:
!   88-04-05  joseph sela
!
! usage:    call glats (lgghaf, colrad, wgt, wgtcs, rcs2)
!   input argument list:
!     lgghaf   - number of gaussian latitudes in a hemisphere.
!
!   output argument list:
!     colrad   - array of colatitude of gaussian latitudes
!                in northern hemisphere.
!     wgt      - array of weights at each gaussian latitude
!                required for gaussian quadrature.
!     wgtcs    - array of gaussian weight/sin of colatitude squared.
!     rcs2     - array of reciprocal  of  sin of colatitude squared.
!
!   output files:
!     output   - printout file.
!
! attributes:
!   language: fortran 200.
!   machine:  cyber 205.
!
!$$$
#include <define.h>
!ccc  half precision colrad,wgt,wgtcs,rcs2
      real colrad,wgt,wgtcs,rcs2
      dimension colrad(lgghaf),wgt(lgghaf),wgtcs(lgghaf)
      dimension rcs2(lgghaf)
#ifdef CRA
      eps=1.e-12
#else
      eps=1.e-6
#endif
!     print 101
!101  format ('0 i   colat   colrad     wgt', 12x, 'wgtcs',
!ccc 1 10x, 'iter  res')
      si = 1.0
      l2=2*lgghaf
      rl2=l2
      scale = 2.0/(rl2*rl2)
      k1=l2-1
      pi = atan(si)*4.e+00
      dradz = pi / 360.
      rad = 0.0
      do 1000 k=1,lgghaf
      iter=0
      drad=dradz
1     call poly(l2,rad,p2)
2     p1 =p2
      iter=iter+1
      rad=rad+drad
      call poly(l2,rad,p2)
      if(sign(si,p1).eq.sign(si,p2)) go to 2
      if(drad.lt.eps)go to 3
      rad=rad-drad
      drad = drad * 0.25
      go to 1
3     continue
      colrad(k)=rad
      phi = rad * 180 / pi
      call poly(k1,rad,p1)
      x = cos(rad)
      w = scale * (1.0 - x*x)/ (p1*p1)
      wgt(k) = w
      sn = sin(rad)
      w=w/(sn*sn)
      wgtcs(k) = w
      rc=1./(sn*sn)
      rcs2(k) = rc
      call poly(l2,rad,p1)
!     print 102,k,phi,colrad(k),wgt(k),wgtcs(k),iter,p1
!102  format(1h ,i2,2x,f6.2,2x,f10.7,2x,e13.7,2x,e13.7,2x,i4,2x,d13.7)
1000  continue
      print 100,lgghaf
100   format(1h ,'shalom from 0.0e0 glats for ',i3)
      return
      end
