!fpp$ noconcur r
#include <define.h>
      subroutine dcyc2t3(im,ix,levs,solhr,slag,                         &
     &                   sinlab,coslab,sdec,cdec,                       &
     &                   xlon,czmn,sfcdlw,sfcnsw,tf,tsea,tsflw,swh,hlw, &
     &                   dlwsfc,ulwsfc,slrad,tau,xmu)
!
      use machine     , only : kind_phys
      use physcons, pi => con_pi, sbc => con_sbc
      implicit none
!
      integer              levs,im,ix
      real(kind=kind_phys) cdec,cnwatt,hsigma,sdec,slag,solhr
      real(kind=kind_phys) sinlab(ix)  , coslab(ix), xlon(ix),          &
     &                     czmn(ix),     sfcdlw(ix), sfcnsw(ix),        &
     &                     tf(ix),       tsea(ix),   tsflw(ix),         &
     &                     dlwsfc(ix),   ulwsfc(ix), slrad(ix),         &
     &                     xmu(ix)
      real(kind=kind_phys) swh(ix,levs), hlw(ix,levs), tau(ix,levs)
!     parameter           (hsigma=sbc,cnwatt=-con_jcal*1.e4/60.)
      integer              i, k
      real(kind=kind_phys) cns,ss,cc,ch,sdlw, tem
!-----------------------------------------------------------------------
!  compute cosine of solar zenith angle for both hemispheres.
      cns = pi*(solhr-12.)/12.+slag
      do i=1,im
        ss     = sinlab(i) * sdec
        cc     = coslab(i) * cdec
        ch     = cc * cos(xlon(i)+cns)
        xmu(i) = ch + ss
      enddo
!       xmu=(sinlab*sdec)                                                &
!    &        +(coslab*cdec)*cos(xlon+cns)
!c    do i=1,lon2
!  normalize by average value over radiation period for daytime.
      do i=1,im
        if(xmu(i).gt.0.01.and.czmn(i).gt.0.01) then
          xmu(i) = xmu(i) / czmn(i)
        else
          xmu(i) = 0.
        endif
!  adjust longwave flux at surface to account for t changes in layer 1.
!       sdlw      = sfcdlw(i)*(tf(i)/tsflw(i))**4
        tem       = tf(i) / tsflw(i)
        tem       = tem * tem
        dlwsfc(i) = sfcdlw(i) * tem * tem
!  return net surface radiative flux.
!       slrad(i)  = sfcnsw(i)*xmu(i) + sdlw
        slrad(i)  = sfcnsw(i)*xmu(i) - dlwsfc(i)
!  return downward and upward longwave flux at ground, respectively.
!       dlwsfc(i) = sdlw*cnwatt
!       ulwsfc(i) = hsigma*tsea(i)**4
        tem       = tsea(i) * tsea(i)
        ulwsfc(i) = sbc * tem * tem
      enddo
!  add radiative heating to temperature tendency
      do k=1,levs
        do i=1,im
          tau(i,k) = tau(i,k) + swh(i,k)*xmu(i) + hlw(i,k)
        enddo
      enddo
      return
      end
