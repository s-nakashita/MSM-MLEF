      function funckt(theta,ktype)
!
      use machine     , only : kind_phys
      use module_progtm , only : tsat, ktk
      implicit none
      integer             ktype,kw
      real(kind=kind_phys) funckt,theta,w
!
      w = (theta / tsat(ktype)) * 20. + 1.
      kw = w
      kw = min(kw,21)
      kw = max(kw,1)
      funckt = ktk(kw,ktype)                                            &
     &         + (w - kw) * (ktk(kw+1,ktype) - ktk(kw,ktype))
      return
      end
