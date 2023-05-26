      subroutine e290(emissb,emiss,avephi,klen,fxoe2,dte2               &
     &,               l, lp1, imax, t1, t2, t4)
!fpp$ noconcur r
!
!     subroutine e290 computes the exchange terms in the flux equation
!  for longwave radiation for all terms except the exchange with the
!  top of the atmosphere. the method is a table lookup on a pre-
!  computed e2 function (defined in ref. (4)).
!     calculations are done in the frequency range:
!       1) 0-560,1200-2200 cm-1   for q(approx)
!  motivation for these calculations is in references (1) and (4).
!       inputs:                    (module blocks)
!     t1,t2,t4,                  tabcom
!     avephi                           tfcom
!     fxoe2,dte2,klen           argument list
!       outputs:
!     emiss,emissb                     tfcom
!
!        called by :     fst88
!        calls     :
!
!
      use machine , only : kind_rad
      use hcon
      implicit none
!
      integer l, lp1, imax, klen
      real (kind=kind_rad) emissb(imax,lp1), emiss(imax,lp1)            &
     &,                     avephi(imax,lp1)                            &
     &,                     dt(imax,lp1),     fyo(imax,lp1)             &
     &,                     du(imax,lp1)
      integer               ival(imax,lp1)
!---variables in the argument list
      real (kind=kind_rad) t1(5040),        t2(5040),   t4(5040)        &
     &,                     fxoe2(imax,lp1), dte2(imax,lp1)
!
      real (kind=kind_rad) tmp3
      integer lp2,i, k, k1, item
!
!---first we obtain the emissivities as a function of temperature
!   (index fxo) and water amount (index fyo). this part of the code
!   thus generates the e2 function.
!
!---calculations for varying kp (from kp=k+1 to lp1, including special
!   case: results are in emiss

      lp2 = l + 2
      do k=1,lp2-klen
        k1 = k + klen - 1
        do i=1,imax
          tmp3        = log10(avephi(i,k1)) + h16e1
          fyo(i,k)    = aint(tmp3*ten)
          du(i,k)     = tmp3  - hp1*fyo(i,k)
          fyo(i,k)    = h28e1 * fyo(i,k)
          item        = fyo(i,k) + fxoe2(i,k1)
          emiss(i,k1) = t1(item) + du(i,k)    * t2(item)                &
     &                           + dte2(i,k1) * t4(item)
        enddo
      enddo

!---the special case emiss(i,l) (layer kp) is obtained now
!   by averaging the values for l and lp1:

      do i=1,imax
        emiss(i,l) = haf*(emiss(i,l) + emiss(i,lp1))
      enddo

!---note that emiss(i,lp1) is not useful after this point.
!
!---calculations for kp=klen and varying k; results are in emissb.
!  in this case, the temperature index is unchanged, always being
!  fxo(i,klen-1); the water index changes, but is symmetrical with
!  that for the varying kp case.note that the special case is not
!  involved here.
!     (fixed level) k varies from (klen+1) to lp1; results are in
!   emissb(i,(klen) to l)
!
      do k=1,lp1-klen
        do i=1,imax
          dt(i,k)   = dte2(i,klen-1)
          ival(i,k) = fyo(i,k) + fxoe2(i,klen-1)
        enddo
      enddo
!
      do k=1,lp1-klen
        k1 = k + klen - 1
        do i=1,imax
          item = ival(i,k)
          emissb(i,k1) = t1(item) + du(i,k) * t2(item)                  &
     &                            + dt(i,k) * t4(item)
        enddo
      enddo
!
      return
      end
