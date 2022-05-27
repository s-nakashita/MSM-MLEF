      subroutine rpln2i(qlnt,dqlnt,colrad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rpln2i
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare and compute some arraies for global tranformation
!            used in regional model
!
! program history log:
!
! usage:    call rpln2i(qlnt,dqlnt,colrad)
!   input argument list:
!     colrad  - co-latitude
!
!   output argument list:
!     qlnt      - transform coefficient for global waves
!     dqlnt     - transform coefficient for global waves derivative
!
!   common block:
! pln2ti  gozcom
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
#include <paramodel.h>
!c
      dimension         qlnt(lnt2)
      dimension        dqlnt(lnt2)
!c
! local arrays
      dimension         qlnv(lnut2)
      dimension            x(jcap1)
      dimension         dpln(lnut2)
!c
      common /pln2tii/ indxmv(lnut2)
      common /pln2ti/ deps(lnut2),rdeps(lnut2),                          & 
     &                dx(twoj1),y(jcap1)
      common /gozcom/ dxa(lnt2),dxb(lnt2)
!c
!cc   data ifir /0/
!cc         part between guards made into sr gpln2i.
!cc         7 dec 1990      m. rozwodoski
!c
      colr   = colrad
      sinlat = cos(colr)
      cos2   = 1.0 - sinlat * sinlat
      prod   = 1.0
      do 600 ll=1,jcap1
             x(ll) = 0.5*prod
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
!c    transpose vector dpln array from cra. order to ibm order.
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
      lp0 = 0
      lp1 = 2
      len = twoj1
      do 940  i=1,jcap1
      do 920 ll=1,len
             dqlnt(ll+lp0) = + qlnv(ll+lp1) * dxb(ll+lp0)
  920 continue
      lp1 = lp1 + len + 2
      lp0 = lp0 + len
      len = len - 2
  940 continue
             lend = lnt2 - 4
      do 960 ll=1,lend
             dqlnt(ll+2) = dqlnt(ll+2) + qlnt(ll) * dxa(ll+2)
  960 continue
!c
!c
      return
      end
