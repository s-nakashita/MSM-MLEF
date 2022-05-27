      subroutine e2spec(emiss,avephi,fxosp,dtsp                         &
     &,                 lp1, imax, t1, t2, t4)
!fpp$ noconcur r
!
!     subroutine e2spec computes the exchange terms in the flux equation
!  for longwave radiation for 2 terms used for nearby layer compu-
!  tations. the method is a table lookup on a pre-
!  computed e2 function (defined in ref. (4)).
!     calculations are done in the frequency range:
!        0-560,1200-2200 cm-1
!  motivation for these calculations is in references (1) and (4).
!       inputs:                    (module blocks)
!     t1,t2,t4,                  tabcom
!     avephi                           tfcom
!     fxosp,dtsp                argument list
!       outputs:
!     emiss                            tfcom
!
!        called by :     fst88
!        calls     :
!
!
      use machine , only : kind_rad
      use hcon
      implicit none
!
      integer lp1, imax
      real (kind=kind_rad) avephi(imax,lp1), emiss(imax,lp1)
!---variables in the argument list
      real (kind=kind_rad) t1(5040),      t2(5040), t4(5040)            &
     &,                     fxosp(imax,2), dtsp(imax,2)
      real (kind=kind_rad) tmp3, fyo, du
      integer k, i, ival
!
!---first we obtain the emissivities as a function of temperature
!   (index fxo) and water amount (index fyo). this part of the code
!   thus generates the e2 function.
!
      do  k=1,2
        do  i=1,imax
          tmp3       = log10(avephi(i,k)) + h16e1
          fyo        = aint(tmp3*ten)
          du         = tmp3 - hp1*fyo
          ival       = h28e1*fyo + fxosp(i,k)
          emiss(i,k) = t1(ival) + du*t2(ival) + dtsp(i,k)*t4(ival)
      enddo
      enddo
!
      return
      end
