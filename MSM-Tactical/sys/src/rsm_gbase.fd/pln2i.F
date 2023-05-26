!fpp$ noconcur r
      subroutine pln2i(qlnt,qlnv,colrad,lat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pln2t       evaluates associated legendre functions.
!   prgmmr: joseph sela      org: w/nmc23    date: 88-10-25
!
! abstract: evaluates the required values of the normalized
!   associated legendre function at a prescribed colatitude.
!   a standard recursion relation is used with real arithmetic.
!
! program history log:
!   88-10-25  joseph sela
!
! usage:    call pln2t (qlnt, qlnv, colrad, lat)
!   input argument list:
!     colrad   - half precision colatitudes in radians for which
!                the associated legendre functions are to be
!                computed.
!     lat      - index which indicates the current latitude.
!
!   output argument list:
!     qlnt     - doubled scalar triangle of
!                half precision associated legendre functions.
!     qlnv     - doubled vector triangle of
!                half precision associated legendre functions.
!
! attributes:
!   language: fortran 200.
!   machine:  cyber 205.
!
!$$$
!ccj
#include <paramodel.h>
!cc
      dimension         qlnt(lnt2)
      dimension         qlnv(lnut2)
      dimension       colrad(latg2)
!c
      dimension            x(jcap1)
      dimension         dpln(lnut2)
!c
      common /pln2tii/ indxmv(lnut2)
      common /pln2ti/ deps(lnut2),rdeps(lnut2),                          &   
     &                dx(twoj1),y(jcap1)
!c
!cc   data ifir /0/
!cc         part between guards made into sr gpln2i.
!cc         7 dec 1990      m. rozwodoski
!c
      colr   = colrad(lat)
      sinlat = cos(colr)
      cos2   = 1.0 - sinlat * sinlat
      prod   = 1.0
      do 600 ll=1,jcap1
             x(ll) = 0.5*prod
!ccc         if (prod .lt. 1.0e-75)  prod=0.0
             prod = prod*cos2*y(ll)
  600 continue
      do 620 ll=1,jcap1
             x(ll) = sqrt(x(ll))
  620 continue
      do 640 ll=1,jcap1
             dpln(2*ll-1) = x(ll)
             dpln(2*ll  ) = x(ll)
  640 continue
      lplus = twoj1
      do 700 ll=1,twoj1
             dpln(ll+lplus) = dx(ll) * sinlat * dpln(ll)
  700 continue
      lp2 = 0
      lp1 =     twoj1
      lp0 = 2 * twoj1
      len =     twoj1 - 2
      do 740  n=3,jcap2
!dir$ ivdep
      do 720 ll=1,len
             dpln(ll+lp0) = (sinlat * dpln(ll+lp1)                       &    
     &               - deps(ll+lp1) * dpln(ll+lp2)) * rdeps(ll+lp0)
  720 continue
      lp2 = lp1
      lp1 = lp0
      lp0 = lp0 + len
      len = len - 2
  740 continue
!c
!c    transpose vector dpln array from cray order to ibm order.
      do 800 i=1,lnut2
             qlnv(indxmv(i)) = dpln(i)
  800 continue
!c
      lpv = 0
      lpt = 0
      len = twoj1
      do 860  n=1,jcap1
      do 840 ll=1,len
             qlnt(ll+lpt) = qlnv(ll+lpv)
  840 continue
      lpv = lpv + len + 2
      lpt = lpt + len
      len = len - 2
  860 continue
!c
      return
      end
