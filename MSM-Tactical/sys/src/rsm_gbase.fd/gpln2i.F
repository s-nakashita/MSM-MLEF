      subroutine gpln2i
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gpln2t      sets common for subroutine pln2t.
!   prgmmr: joseph sela      org: w/nmc23    date: 91-03-14
!
! abstract: initializes the constant variables and arrays
!   of a common for subroutine pln2t.
!
! program history log:
!   91-03-14  joseph sela
!
! usage:    call gpln2t
!
! remarks: call subroutine once before calls to pln2t.
!          refer to pln2t for additional documentation.
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
      dimension       x(jcap1)
!c
      common /pln2tii/ indxmv(lnut2)
      common /pln2ti/ deps(lnut2),rdeps(lnut2),                         &
     &                dx(twoj1),y(jcap1)
!c
!cc   data ifir /0/
!c
!cc   if  (ifir .eq. 1)  go to 500
!cc        ifir = 1
      do 200 ll=1,jcap1
             rdeps(ll) = 0.0
  200 continue
      lplus = jcap1
      len   = jcap1
      do 240 inde=2,jcap2
      do 220   ll=1,len
             l = ll - 1
             n = l + inde - 1
             rdeps(ll+lplus) = (n*n - l*l) / (4.0 * n*n - 1.0)
  220 continue
      lplus = lplus + len
      len = len - 1
  240 continue
      do 260 i=jcap2,lnut
             rdeps(i) = sqrt(rdeps(i))
  260 continue
      do 300 i=1,lnut
             deps(2*i-1) = rdeps(i)
             deps(2*i  ) = rdeps(i)
  300 continue
      ibegin = twoj1 + 1
      do 320 i=ibegin,lnut2
             rdeps(i) = 1.0/deps(i)
  320 continue
      do 400 ll=1,jcap1
             x(ll) = ll*2+1
  400 continue
      do 420 ll=1,jcap1
             y(ll) = x(ll)/(x(ll)-1.)
  420 continue
      do 440 ll=1,jcap1
             x(ll) = sqrt(x(ll))
  440 continue
      do 460 ll=1,jcap1
             dx(2*ll-1) = x(ll)
             dx(2*ll  ) = x(ll)
  460 continue
! 500 continue
!c
!c    set index array for transposing vector array
!c    from cray order to ibm order.
      l=0
      do 640 nn=1,jcap2
      lln=min0(jcap2-nn+1,jcap1)
      do 620 ll=1,lln
      indx=((jcap+3)*(ll-1)-(ll-1)*ll/2+nn)*2
      l=l+2
      indxmv(l-1)=indx-1
      indxmv(l  )=indx
  620 continue
  640 continue
!c
      return
      end
