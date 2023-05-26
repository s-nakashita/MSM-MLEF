      subroutine nsicdif
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!::::::: nonhydrostatic version of semi-implicit time scheme ::::::::
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
      use physcons, rd => con_rd
!
      parameter(alpha=0.8)
!ccj
#include <paramodel.h>
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#ifdef NONHYD
#include <rscomspc.h>
#include <rscomver.h>
#include <rscomio.h>
!cc
!
! this code is modified to have internal coordinate sureface pressure
! and external coordinate hydrostatic temperature for mesoscale
! spectral model (msm)
!
      common /repron/                                                    &   
     &  tmp(LNWAVS,levr+1),tmp1(LNWAVS,levr+1)                           &
     &, dim(LNWAVS,levr)
      common/nsiccom/                                                    &
     &          dcm(LNWAVS,levr,levr),                                   &
     &          wcm(levr,levr+1),                                        &
     &          x3minv(levr+1,levr+1),                                   &
     &          x4m   (levr+1,levr  ),                                   &
     &          tor(levr),dtx2
      save dtsave
!
      data dtsave/0.0/
!     print *,' ------ into nsicdif -------- deltim= ',deltim
!
!
      if( dtsave.ne.deltim ) then
        dtsave = deltim
        dtx2 = 2. * alpha * deltim
!j      print *,' alpha= ',alpha
        do 1 j=1,levr
        tor(j)=rd*tov(j)
  1     continue
        call nsicini(tor,cm,cmn,dmn,emn,fmn,                             &
     &      gmn,hmn,dcm,wcm,x3minv,x4m,snnp1,dtx2)
      endif
!
!----------------- transform u v to di ze before semi-implicit ---
      call uv2dz(LNWAVS,  x,  w,tmp,tmp1,epsx,epsy,levr)
!mic$ do all
!mic$1 shared(tmp,tmp1,x,w)
!mic$1 private(i)
      do 20 k=1,levr
      do 20 i=1,LNWAVS
      x (i,k) = tmp (i,k)
      w (i,k) = tmp1(i,k)
20    continue
      call uv2dz(LNWAVS, uu, vv, di,tmp,epsx,epsy,levr)
      call uv2dz(LNWAVS,uum,vvm,dim,tmp,epsx,epsy,levr)
!
      do i=1,LNWAVS
        z(i) = dtx2 * z(i) - q(i) + qm(i)   ! q1
      enddo
      do k=1,levr
        do i=1,LNWAVS
          x(i,k) = dtx2 * x(i,k) - di(i,k) + dim(i,k)   ! d1
#ifdef INTDTB
          y(i,k) = dtx2 * y(i,k) - te(i,k) + tem(i,k)   ! t1
#endif
          p(i,k) = dtx2 * p(i,k) - pn(i,k) + pnm(i,k)   ! p1
          t(i,k) = dtx2 * t(i,k) - tn(i,k) + tnm(i,k)   ! h1
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          o(i,k) = dtx2 * o(i,k) - on(i,k) + onm(i,k)    ! w1
        enddo
      enddo
!
#ifdef INTDTB
! d2
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr
          do i=1,LNWAVS
            tmp(i,k) = tmp(i,k) + dtx2*snnp1(i)*am(k,kk)*y(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          x(i,k)=x(i,k)+tmp(i,k)+dtx2*snnp1(i)*tor(k)*(z(i)+p(i,k))
        enddo
      enddo
#else
      do k=1,levr
        do i=1,LNWAVS
          x(i,k)=x(i,k)         +dtx2*snnp1(i)*tor(k)*(z(i)+p(i,k))
        enddo
      enddo
#endif
! w2
      do k=1,levr+1
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr+1
          do i=1,LNWAVS
            tmp(i,k) = tmp(i,k) +                                        &   
     &                 (cmn(k,kk)*t(i,kk)+dmn(k,kk)*p(i,kk))
          enddo
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          o(i,k)=o(i,k)+dtx2*tmp(i,k)
        enddo
      enddo
! d bar
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr+1
        do k=1,levr
          do i=1,LNWAVS
            sndt2 = snnp1(i)*dtx2*dtx2
            tmp(i,k) = tmp(i,k) + sndt2*wcm(k,kk)*o(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          x(i,k) = x(i,k) + tmp(i,k)
        enddo
      enddo
!
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr
          do i=1,LNWAVS
            tmp(i,k) = tmp(i,k) + dcm(i,k,kk)*x(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          x(i,k) = tmp(i,k)
        enddo
      enddo
! finish d bar in x
! w bar
      do k=1,levr+1
        do i=1,LNWAVS
          tmp1(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr+1
          do i=1,LNWAVS
            tmp1(i,k) = tmp1(i,k) + x4m(k,kk) * x(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          o(i,k) = o(i,k) - tmp1(i,k)
        enddo
      enddo
!
      do k=1,levr+1
        do i=1,LNWAVS
          tmp1(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr+1
        do k=1,levr+1
          do i=1,LNWAVS
            tmp1(i,k) = tmp1(i,k) + x3minv(k,kk) * o(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          o(i,k) = tmp1(i,k)
        enddo
      enddo
! qs bar
      do i=1,LNWAVS
        tmp(i,1) = 0.0
      enddo
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,1) = tmp(i,1) + dtx2 * sv(k) * x(i,k)
        enddo
      enddo
      do i=1,LNWAVS
        z(i) = z(i) + tmp(i,1)
      enddo

#ifdef INTDTB
! t bar
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr
          do i=1,LNWAVS
            tmp(i,k) = tmp(i,k) + dtx2*bm(k,kk)*x(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          y(i,k) = y(i,k) + tmp(i,k)
        enddo
      enddo
#endif
! p bar
! h bar
      do k=1,levr
        do i=1,LNWAVS
          tmp (i,k) = 0.0
          tmp1(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr+1
        do k=1,levr
          do i=1,LNWAVS
            tmp (i,k) = tmp (i,k) + emn(k,kk)*o(i,kk)
            tmp1(i,k) = tmp1(i,k) + gmn(k,kk)*o(i,kk)
          enddo
        enddo
      enddo
      do kk=1,levr
        do k=1,levr
          do i=1,LNWAVS
            tmp (i,k) = tmp (i,k) - fmn(k,kk)*x(i,kk)
            tmp1(i,k) = tmp1(i,k) - hmn(k,kk)*x(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          p(i,k) = p(i,k) + dtx2 * tmp (i,k)
          t(i,k) = t(i,k) + dtx2 * tmp1(i,k)
        enddo
      enddo
! change bar to tendency
      do i=1,LNWAVS
        z(i) = (z(i) + q(i) - qm(i))/dtx2   ! q
      enddo
      do k=1,levr
        do i=1,LNWAVS
          x(i,k) = (x(i,k) + di(i,k) - dim(i,k))/dtx2   ! d
#ifdef INTDTB
          y(i,k) = (y(i,k) + te(i,k) - tem(i,k))/dtx2   ! t
#endif
          p(i,k) = (p(i,k) + pn(i,k) - pnm(i,k))/dtx2   ! p
          t(i,k) = (t(i,k) + tn(i,k) - tnm(i,k))/dtx2   ! h
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          o(i,k) = (o(i,k) + on(i,k) - onm(i,k))/dtx2        ! w
        enddo
      enddo
!
!-------------------change dz forcing to uv---------
      call dz2uv(LNWAVS,x,w,tmp,tmp1,epsx,epsy,rnnp1,levr)
!mic$ do all
!mic$1 shared(tmp,tmp1,x,w)
!mic$1 private(i)
      do 870 k=1,levr
      do 870 i=1,LNWAVS
      x(i,k) = tmp (i,k)
      w(i,k) = tmp1(i,k)
 870  continue
!      print *,' ------ end nsicdif -------- deltim= ',deltim
!
#endif
      return
      end
