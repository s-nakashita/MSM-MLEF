      subroutine rsetsig(levr, levrm1, levrp1,                           &  
     &                   ci, si, del, sl, cl, rpi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsetsig
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare sigma layer and its related arraies for computation.
!
! program history log:
!
! usage:    call rsetsig(ci, si, del, sl, cl, rpi)
!   input argument list:
!     si  - sigma interface values
!     sl  - sigma layer values
!
!   output argument list:
!     ci     - 1 - sigma interface
!     cl     - 1 - sigma layer
!     del     - igma layer thickness
!     rpi     - (sl(k+1)/sl(k))**(r/cp)
!
!   common block:
! rscommap  rscompln
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
      use physcons, rocp => con_rocp
#include <define.h>
!ccj
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
!cc
! ===== modified to run regional spectral model =====
! sl and si are given
      dimension ci(levrp1), si(levrp1),                                  &  
     & del(levr), sl(levr), cl(levr), rpi(levrm1)
!
#ifdef MP
      if (mype .eq. master) then
#endif
      print *,' ----- begin rsetsig ----'
#ifdef MP
      endif
#endif
!
      do 10 k=1,levr
      del(k)=si(k)-si(k+1)
      cl(k)=1.0e0-sl(k)
10    continue
      do 20 k=1,levrp1
      ci(k)=1.0e0-si(k)
20    continue
      sumdel=0.e0
      do 30 k=1,levr
      sumdel=sumdel+del(k)
30    continue
!
      rk = rocp
! compute pi ratios for temp. matrix.
      do 40 le=1,levrm1
      base = sl(le+1)/sl(le)
40    rpi(le) = base**rk
!
      do 50 le=1,levrp1
#ifdef MP
      if (mype .eq. master) then
#endif
      print 100, le, ci(le), si(le)
100   format (1h , 'level=', i2, 2x, 'ci=', f6.3, 2x, 'si=', f6.3)
#ifdef MP
      endif
#endif
50    continue
      print 97
97    format (1h0)
      do 60 le=1,levr
#ifdef MP
      if (mype .eq. master) then
#endif
      print 101, le, cl(le), sl(le), del(le)
101   format (1h , 'layer=', i2, 2x, 'cl=', f6.3, 2x, 'sl=', f6.3, 2x,   &  
     & 'del=', f6.3)
#ifdef MP
      endif
#endif
60    continue
#ifdef MP
      if (mype .eq. master) then
#endif
      print 102, (rpi(le), le=1,levrm1)
102   format (1h0, 'rpi=', (18(1x,f6.3)) )
      print 99,sumdel
99    format (1h0,' ---- end rsetsig ---- sumdel(should be 1)=',e13.4)
#ifdef MP
      endif
#endif
      return
      end
