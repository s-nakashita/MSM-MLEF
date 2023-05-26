!
      subroutine e1e290(g1,g2,g3,g4,g5,emiss,fxoe1,dte1,fxoe2,dte2,     &
     &                  avephi, l, lp1, imax, em1v, em1vw, t1, t2, t4)
!fpp$ noconcur r
!
!     subroutine e1e290 computes the exchange terms in the flux equation
!  for longwave radiation for all terms except the exchange with the
!  top of the atmosphere. the method is a table lookup on a pre-
!  computed e2 function (defined in ref. (4)).
!      the e1 function  calculations (formerly done in subroutine
!  e1v88 compute the flux resulting from the exchange of photons
!  between a layer and the top of the atmosphere.  the method is a
!  table lookup on a pre-computed e1 function.
!     calculations are done in two frequency ranges:
!       1) 0-560,1200-2200 cm-1   for q(approx)
!       2) 160-560 cm-1           for q(approx,cts).
!  motivation for these calculations is in references (1) and (4).
!       inputs:                    (module blocks)
!     em1v,em1vw,t1,t2,t4              tabcom
!     avephi                           tfcom
!     temp                             radisw
!     t                                kdacom
!     fxoe1,dte1                argument list
!     fxoe2,dte2                argument list
!       outputs:
!     emiss                            tfcom
!     g1,g2,g3                  argument list,for 1st freq. range
!     g4,g5                     argument list,for 2nd freq. range
!
!        called by :     fst88
!        calls     :
!
!
      use machine , only : kind_rad
      use hcon
      implicit none
!
      integer  l, lp1, imax
      real (kind=kind_rad) avephi(imax,lp1), emiss(imax,lp1)
!
      integer   it1(imax,3*l+2)
      real (kind=kind_rad) fyo(imax,lp1), du(imax,lp1),                 &
     &                      ww1(imax,lp1), ww2(imax,lp1)
!---variables in the argument list
      real (kind=kind_rad) t1(5040),        t2(5040), t4(5040)          &
     &,                     em1v(5040),      em1vw(5040)                &
     &,                     fxoe1(imax,lp1), dte1(imax,lp1)             &
     &,                     fxoe2(imax,lp1), dte2(imax,lp1),            &
     &                      g1(imax,lp1),    g2(imax,l)                 &
     &,                     g3(imax,lp1),    g4(imax,lp1), g5(imax,l)
!
      real (kind=kind_rad) tmp3, tem1, tem2, tem3, tem4
      integer ll, llp1, kp, i, ival, k1, k2, item
!
!---first we obtain the emissivities as a function of temperature
!   (index fxo) and water amount (index fyo). this part of the code
!   thus generates the e2 function. the fxo indices have been
!   obtained in fst88, for convenience.
!
!---this subroutine evaluates the k=1 case only--
!
!---this loop replaces loops going fromi=1,imax and kp=2,lp1 plus
!   the special case for the lp1th layer.
!     lp2  = l + 2
      ll   = l + l
      llp1 = ll + 1
!
      do kp=1,lp1
        do i=1,imax
          tmp3        = log10(avephi(i,kp)) + h16e1
          fyo(i,kp)   = aint(tmp3*ten)
          du(i,kp)    = tmp3 - hp1*fyo(i,kp)
          fyo(i,kp)   = h28e1 * fyo(i,kp)
          ival        = fyo(i,kp) + fxoe2(i,kp)
          emiss(i,kp) = t1(ival ) + du(i,kp)   * t2(ival)               &
     &                            + dte2(i,kp) * t4(ival)
        enddo
      enddo
!
!---the special case emiss(i,l) (layer kp) is obtained now
!   by averaging the values for l and lp1:
      do i=1,imax
        emiss(i,l) = haf*(emiss(i,l) + emiss(i,lp1))
      enddo
!
!   calculations for the kp=1 layer are not performed, as
!   the radiation code assumes that the top flux layer (above the
!   top data level) is isothermal, and hence contributes nothing
!   to the fluxes at other levels.
!
!***the following is the calculation for the e1 function, formerly
!    done in subroutine e1v88. the move to e1e288 is due to the
!    savings in obtaining index values (the temp. indices have
!    been obtained in fst88, while the u-indices are obtained
!    in the e2 calcs.,with k=1).
!
!   for terms involving top layer, du is not known; in fact, we
!   use index 2 to repersent index 1 in prev. code. this means that
!    the it1 index 1 and llp1 has to be calculated separately. the
!   index llp2 gives the same value as 1; it can be omitted.
!
      do i=1,imax
        it1(i,1) = fxoe1(i,1)
        ww1(i,1) = ten - dte1(i,1)
        ww2(i,1) = hp1
      enddo
!dir$ ivdep
      do kp=1,l
        k1 = kp + 1
        k2 = kp + lp1
        do i=1,imax
          it1(i,k1) = fyo(i,kp) + fxoe1(i,k1)
          it1(i,k2) = fyo(i,kp) + fxoe1(i,kp)
          ww1(i,k1) = ten       - dte1(i,k1)
          ww2(i,k1) = hp1       - du(i,kp)
        enddo
      enddo
      do kp=1,l
        do i=1,imax
          it1(i,kp+llp1) = fyo(i,kp) + fxoe1(i,1)
        enddo
      enddo
!
!  g3(i,1) has the same values as g1 (and did all along)
      do i=1,imax
        tem1 = ww1(i,1) * ww2(i,1)
        tem2 = ww2(i,1) * dte1(i,1) 
        item = it1(i,1)
        g1(i,1) = tem1 * em1v(item)  + tem2 * em1v(item+1)
        g4(i,1) = tem1 * em1vw(item) + tem2 * em1vw(item+1)
        g3(i,1) = g1(i,1)
      enddo
      do kp=1,l
        k1 = kp + 1
        do i=1,imax
          tem1 = ww1(i,k1)  * ww2(i,k1)
          tem2 = ww2(i,k1)  * dte1(i,k1) 
          tem3 = ww1(i,k1)  * du(i,kp) 
          tem4 = dte1(i,k1) * du(i,kp)
          item = it1(i,k1)
!
          g1(i,k1) = tem1 * em1v(item)     + tem2 * em1v(item+1)+       &
     &               tem3 * em1v(item+28)  + tem4 * em1v(item+29)
          g4(i,k1) = tem1 * em1vw(item)    + tem2 * em1vw(item+1)+      &
     &               tem3 * em1vw(item+28) + tem4 * em1vw(item+29)
!
          tem1 = ww1(i,kp)  * ww2(i,k1) 
          tem2 = ww2(i,k1)  * dte1(i,kp)
          tem3 = ww1(i,kp)  * du(i,kp)
          tem4 = dte1(i,kp) * du(i,kp)
          item = it1(i,lp1+kp)
!
          g2(i,kp) = tem1 * em1v(item)     + tem2 * em1v(item+1)+       &
     &               tem3 * em1v(item+28)  + tem4 * em1v(item+29)
          g5(i,kp) = tem1 * em1vw(item)    + tem2 * em1vw(item+1)+      &
     &               tem3 * em1vw(item+28) + tem4 * em1vw(item+29)
        enddo
      enddo
      do kp=2,lp1
        do i=1,imax
          item = it1(i,ll+kp)
          g3(i,kp) = ww1(i,1)  * ww2(i,kp)  * em1v(item)+               &
     &               ww2(i,kp) * dte1(i,1)  * em1v(item+1)+             &
     &               ww1(i,1)  * du(i,kp-1) * em1v(item+28)+            &
     &               dte1(i,1) * du(i,kp-1) * em1v(item+29)
        enddo
      enddo
!
      return
      end
