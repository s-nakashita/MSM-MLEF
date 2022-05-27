      subroutine cdefghm(cmn,dmn,emn,fmn,gmn,hmn,                        &     
     &                   si,sl,tov,del,h1,h2)
!j   &                   si,sl,tov,del,rdel2,h1,h2)
!
      use physcons, rocp => con_rocp, rd => con_rd, g => con_g,          &
     &              cp => con_cp, cv => con_cv
!ccj
#include <paramodel.h>
#ifdef NONHYD
#include <rscommap.h>
!cc
      dimension                                                          &
     & tov(levr),si(levr+1),sl(levr),                                    &
!j   & tov(levr),rdel2(levr+1),si(levr+1),sl(levr),                      &
     & del(levr),h1(levr),h2(levr),                                      &
     & cmn(levr+1,levr),                                                 &
     & dmn(levr+1,levr),                                                 &
     & emn(levr  ,levr+1),                                               &
     & fmn(levr  ,levr),                                                 &
     & gmn(levr  ,levr+1),                                               &
     & hmn(levr  ,levr)
!
!cj      print *,' tov ',(tov(k),k=1,levr)
      do i=1,levr+1
        do j=1,levr
          cmn(i,j)=0.0
          dmn(i,j)=0.0
        enddo
      enddo
      do k=2,levr
         cmn(k,k  )= 0.5 * g / tov(k)
         cmn(k,k-1)= 0.5 * g / tov(k-1)
         dmn(k,k  )= - 2.0 * g * si(k) / (del(k)+del(k-1))
         dmn(k,k-1)=   2.0 * g * si(k) / (del(k)+del(k-1))
!j       dmn(k,k  )= - 2.0 * g * si(k) * rdel2(k)
!j       dmn(k,k-1)=   2.0 * g * si(k) * rdel2(k)
!cj         print *,' k cmnk cmnk-1 dmnk dmnk-1 '
!cj     1          ,k,cmn(k,k),cmn(k,k-1),dmn(k,k),dmn(k,k-1)
      enddo
! test
!     cmn(levr+1,levr)= g / tov(levr)
!
!cj      call prtmtx(cmn,levr+1,levr,' cmn ')
!cj      call prtmtx(dmn,levr+1,levr,' dmn ')
!
      xk = rocp
      gama = cp / cv
      do i=1,levr
        do j=1,levr+1
          emn(i,j)=0.0
          gmn(i,j)=0.0
        enddo
      enddo
      gocp= g / cp
      gord= g / rd
      do k=1,levr
        gordt = gord / tov(k)
        gmsod = gama* sl(k) / del(k)
        emn(k,k  )= gordt * ( 0.5*si(k  )/sl(k) + gmsod )
        emn(k,k+1)= gordt * ( 0.5*si(k+1)/sl(k) - gmsod )
        gmn(k,k  )= emn(k,k  )/gordt*gocp-h2(k)*si(k  )*gordt
        gmn(k,k+1)= emn(k,k+1)/gordt*gocp-h1(k)*si(k+1)*gordt
!cj         print *,' k emnk emnk-1 gmnk gmnk-1 '
!cj     1          ,k,emn(k,k),emn(k,k+1),gmn(k,k),gmn(k,k+1)
      enddo
!cj      call prtmtx(emn,levr,levr+1,' emn ')
!cj      call prtmtx(gmn,levr,levr+1,' gmn ')
!
#ifdef INTDTB
      do i=1,levr
        do j=1,levr
          hmn(i,j)= xk*tov(i)+h1(i)*(1.-si(i+1))+h2(i)*(1.-si(i))
          hmn(i,j)= - xm2m * hmn(i,j) * del(j)
        enddo
      enddo
      do i=1,levr
        do j=1,i
          hmn(i,j)=hmn(i,j)+xm2m*h1(i)*del(j)
        enddo
      enddo
      do i=2,levr
        do j=1,i-1
          hmn(i,j)=hmn(i,j)+xm2m*h2(i)*del(j)
        enddo
      enddo
#else
      do i=1,levr
        do j=1,levr
          hmn(i,j) = 0.0
        enddo
      enddo
#endif
      do i=1,levr
        do j=1,levr
          fmn(i,j)= - xm2m * del(j)
        enddo
      enddo
      do k=1,levr
        fmn(k,k)= xm2m * gama + fmn(k,k)
        hmn(k,k)= xk * tov(k) * xm2m * gama + hmn(k,k)
      enddo
!cj      call prtmtx(fmn,levr,levr,' fmn ')
!cj      call prtmtx(hmn,levr,levr,' hmn ')
!
#endif
      return
      end
