
      subroutine flxco2(swc,u1,du,nu,swh,w1,dw,nw,tbl,dflx              &
     &,                                               l, imax)
!fpp$ noconcur r
!********************************************************************
!  compute the absorption due to co2. ref: chou (j. climate, 1990,
!     209-217)
!     updated sep. 1999 based on nasa/tm-1999-104606, vol 15.
!  the effect of co2 absorption below the cloud top is neglected.
!  input variables:
!     swc         : column amount of co2
!     swh         : column amount of water vapor
!     u1,du,w1,dw : coefficients
!     tbl         : look up co2 absorption table
!     nu,nw       : table dimensions
!  output variables:
!     dflx        : additional flux reduction due to co2 for clear sky
!
!********************************************************************
!
      use machine , only : kind_rad
      implicit none
!
      integer imax, l, nu, nw
      real (kind=kind_rad) swc(imax,l+1),   swh(imax,l+1)               &
     &,                     dflx(imax,l+1),  tbl(nu,nw)
      real (kind=kind_rad) u1, du, w1, dw, x1, y1, clog, wlog, dc, dd   &
     &,                     x2, y2
      integer i, k, ic, iw, ic1, iw1
!
! ... table look-up for the reduction of clear-sky solar
!
      x1 = u1 - 0.5*du
      y1 = w1 - 0.5*dw
      do k=2,l+1
        do i=1,imax
            clog = swc(i,k)
            wlog = swh(i,k)
            ic = int( (clog - x1)/du + 1.0)
            iw = int( (wlog - y1)/dw + 1.0)
            ic = max(2, min(nu, ic))
            iw = max(2, min(nw, iw))
            ic1 = ic - 1
            iw1 = iw - 1
            dc = clog - float(ic-2)*du - u1
            dd = wlog - float(iw-2)*dw - w1
            x2 = tbl(ic1,iw1) + (tbl(ic1,iw)-tbl(ic1,iw1))/dw * dd
            y2 = x2 + (tbl(ic,iw1) - tbl(ic1,iw1))/du * dc
            dflx(i,k) = dflx(i,k) + y2
        enddo
      enddo
!
      return
      end
