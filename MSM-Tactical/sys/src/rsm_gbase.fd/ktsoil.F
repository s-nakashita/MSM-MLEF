      function ktsoil(theta,ktype)
!
      use machine     , only : kind_phys
      use module_progtm , only : tsat, dfkt
      implicit none
      integer              ktype,kw
      real(kind=kind_phys) ktsoil, theta, w
!
      w = (theta / tsat(ktype)) * 20. + 1.
      kw = w
      kw = min(kw,21)
      kw = max(kw,1)
      ktsoil = dfkt(kw,ktype)                                           &
     &         + (w - kw) * (dfkt(kw+1,ktype) - dfkt(kw,ktype))
      return
      end
