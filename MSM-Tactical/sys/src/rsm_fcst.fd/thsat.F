      function thsat(ktype)
!
      use machine     , only : kind_phys
      use module_progtm , only : tsat
      implicit none
      integer             ktype
      real(kind=kind_phys) thsat
!
      thsat = tsat(ktype)
      return
      end
