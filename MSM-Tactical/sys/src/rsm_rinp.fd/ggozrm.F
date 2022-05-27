      subroutine ggozrm(epsi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ggozrm      sets common for subroutine gozrmt.
!   prgmmr: joseph sela      org: w/nmc23    date: 91-03-14
!
! abstract: initializes the constant variables and arrays
!   of a common for subroutine gozrmt.
!
! program history log:
!   91-03-14  joseph sela
!
! usage:    call ggozrm ( eps )
!
! remarks: call subroutine once before calls to gozrmt.
!          refer to gozrmt for additional documentation.
!
! attributes:
!   language: fortran, cft77.
!   machine:  cray y-mp.
!
!$$$
!ccj
#include <paramodel.h>
!cc
!c
      dimension          epsi(jcap2,jcap1)
!c
      dimension         dxint(lnt2)
      dimension            dx(twoj1,jcap2)
      dimension          deps(twoj1,jcap2)
!c
      common /gozcom/ dxa(lnt2),dxb(lnt2)
!c
      do 200 ll=1,twoj1
             dxint(2*ll-1) = ll
             dxint(2*ll  ) = ll
  200 continue
      lp = 0
      do 240  i=1,jcap2
      do 220 ll=1,twoj1
             dx(ll,i) = dxint(ll+lp)
  220 continue
      lp = lp + 2
  240 continue
      do 280  i=1,jcap2
      do 260 ll=1,jcap1
             deps(2*ll-1,i) = epsi(i,ll)
             deps(2*ll  ,i) = epsi(i,ll)
  260 continue
  280 continue
      do 300 ll=1,twoj1
             dxa(ll) = 0.0
  300 continue
      lp1 = twoj1
      len = twoj1 - 2
      do 340  i=1,jcap
      do 320 ll=1,len
             dxa(ll+lp1) =  dx(ll,i+1) * deps(ll,i+1)
             dxb(ll+lp1) = -dx(ll,i  ) * deps(ll,i+2)
  320 continue
      lp1 = lp1 + len
      len = len - 2
  340 continue
      do 380  i=1,jcap2
      do 360 ll=1,twoj1
             dx(ll,i) = dx(ll,i) - 1.e0
  360 continue
  380 continue
      do 400 ll=1,twoj1
             dxb(ll) = -dx(ll,1) * deps(ll,2)
  400 continue
!c
!c    transpose scalar arrays dxa, dxb, dxc, dxd
!c    from cray order to ibm order.
      call transo (dxa, 2)
!c
      return
      end
