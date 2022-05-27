      module machine

      implicit none
      save
!  machine dependant constants
      integer kind_io4,kind_io8,kind_phys,kind_rad
      parameter (kind_rad = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
      parameter (kind_phys = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
      parameter (kind_io4 = 4)
      parameter (kind_io8 = 8)

      end module machine
