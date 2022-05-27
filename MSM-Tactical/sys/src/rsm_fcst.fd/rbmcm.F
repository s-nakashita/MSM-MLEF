! --------------------- rver ---------------------------
! modified by hann-ming henry juang for rsm.
! ------------------------------------------------------
      subroutine rbmcm(tov,p1,p2,h1,h2,del,ci,bm,cm,sv,am)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rbmcm
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare some constant arraies for dynamic computation and semi-
!            implicit computation.
!
! program history log:
!
! usage:    call rbmcm(tov,p1,p2,h1,h2,del,ci,bm,cm,dt,sv,am)
!   input argument list:
!     tov    - base constant temperature.
!     p1     - inverse of rpi
!     p2     - rpi
!     del    - sigma layer thickness
!     dt     - time step
!     ci     - sigma interface values
!     sv     - negative of del time mean of map factor
!     am     - matrix to compute geopotential height
!
!   output argument list:
!     h1     - vector for temperature equation
!     h2     - as h1
!     bm     - for semi-implicit computation
!     cm     - for semi-implicit computation
!
!   common block :
!     rscommap
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use physcons, rocp => con_rocp, rd => con_rd
!ccj
#include <paramodel.h>
#include <rscommap.h>
!cc
      dimension x1(levr),x2(levr)
      dimension bm(levr,levr), cm(levr,levr),                            &   
     &  tov(levr), p1(levr), p2(levr), h1(levr), h2(levr),               &
     &  del(levr), ci(levrp1), sv(levr), am(levr,levr)
!      print *,' ---- start bmcm ---- '
      rk = rocp
      do 1 le=1,levrm1
      h1(le) = ( p1(le) * tov(le+1) - tov(le))/(del(le)+del(le+1))
1     continue
      h1(levr) = 0.e0
      h2( 1 ) = 0.e0
      do 2 le=2,levr
      h2(le) = ( tov(le) - p2(le) * tov(le-1))/(del(le)+del(le-1))
2     continue
      do 7 k=1,levr
      x1(k) = rk*tov(k)+ci(k+1)*h1(k)+ci(k)*h2(k)
      x2(k) = h1(k)+h2(k)
7     continue
      do 33 j=1,levr
      do 33 k=1,levr
      bm(k,j) = -x1(k)*del(j)
33    continue
      do 34 k=1,levr
      do 34 j=1,levr
      kk = k
      if(j.gt.kk) go to 34
      bm(k,j) = bm(k,j) + x2(k)*del(j)
34    continue
      do 3 k=1,levr
      bm(k,k) = bm(k,k) - h2(k) * del(k)
3     continue
      do 5 j=1,levr
      do 5 i=1,levr
      bm(i,j) = bm(i,j) * xm2m
5     continue
      do 4 i=1,levr
      do 4 j=1,levr
      cm(i,j) = 0.e0
      do 44 k=1,levr
      cm(i,j) = cm(i,j) + am(i,k) * bm(k,j)
44    continue
      cm(i,j) = cm(i,j)+rd*tov(i)*sv(j)
4     continue
!cc   print *,' ---- end bmcm ---- '
      return
      end
