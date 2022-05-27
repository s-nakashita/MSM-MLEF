      subroutine crtwrk(pl, acr)
      use machine , only : kind_phys
      use module_ras , only : ac, ad
      implicit none
!
      real(kind=kind_phys) pl, acr
      integer iwk
!
      iwk = pl * 0.02 - 0.999999999
      iwk = max(1, min(iwk,16))
      acr = ac(iwk) + pl * ad(iwk)
!
      return
      end
