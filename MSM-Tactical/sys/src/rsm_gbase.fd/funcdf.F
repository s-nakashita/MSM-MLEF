      function funcdf(theta,ktype)
!
      use machine     , only : kind_phys
      use module_progtm , only : tsat, dfk
      implicit none
      integer              ktype,kw
      real(kind=kind_phys) funcdf,theta,w
!
      w = (theta / tsat(ktype)) * 20. + 1.
      kw = w
      kw = min(kw,21)
      kw = max(kw,1)
      funcdf = dfk(kw,ktype)                                            &
     &         + (w - kw) * (dfk(kw+1,ktype) - dfk(kw,ktype))
      return
      end
