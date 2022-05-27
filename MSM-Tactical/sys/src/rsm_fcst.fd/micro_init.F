      subroutine micro_init(lm,len,f_ice,f_rain,f_rimef,dt,fhour,me      &                       
     &,                     first)
!
!     this subroutine initializes the necessary constants and
!     tables for brad ferrier cloud microphysics package
!
      use machine , only : kind_phys
      use module_microphysics , only : gsmconst
      implicit none
!
      logical first
      integer lm, len, me
      real (kind=kind_phys) f_ice(len,lm), f_rain(len,lm),               &
     &                      f_rimef(len,lm), dt, fhour
!
      if (fhour .lt. 0.1) then
        f_ice   = 0.              ! initialize ice fraction array (real)
        f_rain  = 0.              ! initialize rain fraction array (real)
        f_rimef = 1.              ! initialize rime factor array (real)
      endif
      call gsmconst (dt,me,first) ! initialize lookup tables & constants
!
      return
      end
