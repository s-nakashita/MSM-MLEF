      subroutine ramtm(del,rpi,sv,p1,p2,am,hm,tm,si)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ramtm
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare some constant arraies for dynamic computation and semi-
!            implicit computation.
!
! program history log:
!
! usage:    call ramtm(del,rpi,sv,p1,p2,am,hm,tm,si)
!   input argument list:
!     del - sigma layer thickness
!     rpi - (sl(k+1)/sl(k))**(r/cp)
!     si  - sigma layer values
!
!   output argument list:
!     sv     - negative of del time mean of map factor
!     p1     - inverse of rpi
!     p2     - rpi
!     am     - matrix to compute geopotential height
!     hm     - am
!     tm     - am
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! rprnta
! rprntb
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use physcons, cp => con_cp, rd => con_rd
!ccj
#include <paramodel.h>
#include <rscommap.h>
!cc
      dimension del(levr),rpi(levrm1),sv(levr),si(levrp1),               & 
     & p1(levr),p2(levr),                                                &
     & am(levr,levr),hm(levr,levr),tm(levr,levr)
!cc   print *,' ----- start amhmtm ------ '
!
! .... revised at (may 1, 1990) hann-ming juang
      do 10 le=1,levr
      sv(le) = -del(le) * xm2m
10    continue
! .... rpi(k) = ( sl(k+1) / sl(k) ) ** kapa
      do 20 le=1,levrm1
      p1(le) = 1.e0 / rpi(le)
      p2(le+1) = rpi(le)
20    continue
      p1(levr) = 0.e0
      p2( 1 ) = 0.e0
!        print *,' xm2m ',xm2m
!        print *,' del ',del
!     call rprntb(sv,levr,'sv')
!     call rprntb(p1,levr,'p1')
!     call rprntb(p2,levr,'p2')
!
      cphf = 0.5e0 * cp
      do 110 k=1,levr
      do 110 j=1,levr
      am(k,j) = rd * del(j)
110   continue
      do 120 k=1,levr
      do 120 j=1,levrm1
      am(k,j) = am(k,j) - cphf * ( 1. - p2(j+1) ) * si(j+1)
120   continue
      do 125 k=1,levr
      do 125 j=1,levrm1
      if( j .lt. k )                                                     & 
     &    am(k,j) = am(k,j) + cphf * ( 1. - p2(j+1) )
125   continue
      do 130 k=1,levr
      do 130 j=2,levr
      am(k,j) = am(k,j) - cphf * ( p1(j-1) - 1. ) * si(j)
130   continue
      do 135 k=1,levr
      do 135 j=2,levr
      if( j .le. k )                                                     & 
     &   am(k,j) = am(k,j) + cphf * ( p1(j-1) - 1. )
135   continue
!     call rprnta(am,levr,'am (my own)')
!...  store am in tm and divide am
      do 210 i=1,levr
      do 210 j=1,levr
      tm(i,j) = am(i,j)
      hm(i,j) = am(i,j)
210   continue
!cc   print 333
!333  format(1h0,'shalom  amhmtm')
      return
      end
