
      subroutine qsatcn(tt,p,q,dqdt,lprnt)

      use machine , only : kind_phys
      use funcphys , only : fpvs
      use physcons, rv => con_rv, cvap => con_cvap, cliq => con_cliq     &   
     &,             csol => con_csol, ttp => con_ttp, hvap => con_hvap   &
     &,             hfus => con_hfus, eps => con_eps, epsm1 => con_epsm1
      implicit none
!     include 'constant.h'
!
      real(kind=kind_phys) tt, p, q, dqdt
!
      real(kind=kind_phys) rvi, facw, faci, hsub, tmix, den
      real(kind=kind_phys) zero,one,one_m10
      parameter (rvi=1.0/rv)
      parameter (facw=cvap-cliq, faci=cvap-csol)
      parameter (hsub=hvap+hfus, tmix=ttp-20.0, den=1.0/(ttp-tmix))
      parameter (zero=0.,one=1.,one_m10=1.e-10)
      logical lprnt
!
!cfpp$ noconcur r
      real(kind=kind_phys) es, d, hlorv, w
!
!     es    = 10.0 * fpvs(tt)                ! fpvs is in centibars!
      es    = 0.01 * fpvs(tt)                ! fpvs is in pascals!
      d     = 1.0 / max(p+epsm1*es,one_m10)
!
      q     = min(eps*es*d, one)
!
      w     = max(zero, min(one, (tt - tmix)*den))
      hlorv = ( w      * (hvap + facw * (tt-ttp))                        &   
     &       + (1.0-w) * (hsub + faci * (tt-ttp)) ) * rvi
      dqdt  = p * q * hlorv *  d / (tt*tt)
!
      return
      end
