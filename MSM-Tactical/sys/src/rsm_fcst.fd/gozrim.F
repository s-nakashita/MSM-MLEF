!fpp$ noconcur r
      subroutine gozrim(qlnt,qlnv,qdert,epsi,lat,rcs2,wgtl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gozrmt      computes derivatives of legendres.
!   prgmmr: joseph sela      org: w/nmc23    date: 88-11-02
!
! abstract: computes derivatives of associated legendre functions
!   and for convenience, other required products of
!   legendres and factors involving wave number and latitude.
!   the resulting arrays are required for the application
!   of divergence and curl operators in msu22 and psu22.
!
! program history log:
!   88-11-02  joseph sela
!
! usage:    call gozrmt (qlnt,qlnv,qdert,eps,lat,qlnwct,rcs2,wgtl)
!   input argument list:
!     qlnt     - doubled scalar triangular
!                array of associated legendre functions at
!                a given latitude.
!                on input, values of qlnt are a subset of qlnv.
!     qlnv     - doubled vector triangular
!                array of associated legendre functions at
!                a given latitude.
!     eps      - array of function of wave number computed in epslon.
!                eps is used only during first call to gozrmt.
!     lat      - latitude index.
!     rcs2     - array of constants computed in glats (1/sin(lat)**2).
!     wgtl     - weight at gaussian latitude.
!                multiplier of output arrays qlnt, qdert, qlnwct.
!
!   output argument list:
!     qlnt     - doubled scalar triangular
!                array of qlnt*n*(n+1)*1/a**2   times wgtl.
!     qdert    - doubled scalar triangular
!                array of legendre derivatives  times wgtl.
!     qlnwct   - doubled scalar triangular
!                array of qlnt*l*rcs2(lat)/a    times wgtl.
!
! attributes:
!   language: fortran 200.
!   machine:  cyber 205.
!
!$$$
      use physcons, rerth => con_rerth
!ccj
#include <paramodel.h>
!cc
!ray       save
!c
      dimension          qlnt(lnt2)
      dimension          qlnv(lnut2)
      dimension         qdert(lnt2)
      dimension          epsi(jcap2,jcap1)
      dimension          rcs2(latg2)
!c
      common /gozcom/ dxa(lnt2),dxb(lnt2)
!c
!cc         part between guards made into sr ggozri.
!cc         7 dec 1990      m. rozwodoski
!c
!c    compute pln derivatives in ibm order.
      wcsa=rcs2(lat)*wgtl/rerth
!c
      lp0 = 0
      lp1 = 2
      len = twoj1
      do 640  i=1,jcap1
      do 620 ll=1,len
             qdert(ll+lp0) = qlnv(ll+lp1) * dxb(ll+lp0)
  620 continue
      lp1 = lp1 + len + 2
      lp0 = lp0 + len
      len = len - 2
  640 continue
!c
                  lend = lnt2 - 4
      do 720 ll=1,lend
             qdert(ll+2) = qdert(ll+2) + qlnt(ll) * dxa(ll+2)
  720 continue
!c
      do 760 ll=1,lnt2
             qdert(ll) = qdert(ll) * wcsa
  760 continue
!c
!c
!c
      return
      end
