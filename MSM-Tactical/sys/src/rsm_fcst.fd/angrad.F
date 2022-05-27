      subroutine angrad( pres, alm, stla, ctl2, al2                     &
     &,                  pi, tla, prb, wfn, ufn)
      use machine , only : kind_phys
      use module_ras , only : refp, refr, tlac, plac, tlbpl, drdp
      implicit none

      real(kind=kind_phys) pres, stla, ctl2, pi,  pifac                 &
     &,                    alm,  al2,  tla,  tem, tem1                  &
     &,                    prb,  acr,  wfn,  ufn
!
      integer i
!
      pifac = pi / 180.0
!     print *,' pres=',pres
      if (tla .lt. 0.0) then
          if (pres .le. plac(1)) then
            tla = tlac(1)
          elseif (pres .le. plac(2)) then
            tla = tlac(2) + (pres-plac(2))*tlbpl(1)
          elseif (pres .le. plac(3)) then
            tla = tlac(3) + (pres-plac(3))*tlbpl(2)
          elseif (pres .le. plac(4)) then
            tla = tlac(4) + (pres-plac(4))*tlbpl(3)
          elseif (pres .le. plac(5)) then
            tla = tlac(5) + (pres-plac(5))*tlbpl(4)
          elseif (pres .le. plac(6)) then
            tla = tlac(6) + (pres-plac(6))*tlbpl(5)
          elseif (pres .le. plac(7)) then
            tla = tlac(7) + (pres-plac(7))*tlbpl(6)
          elseif (pres .le. plac(8)) then
            tla = tlac(8) + (pres-plac(8))*tlbpl(7)
          else
            tla = tlac(8)
          endif
!         tla = tla * 1.5

          stla = sin(tla*pifac)
          tem1 = cos(tla*pifac)
          ctl2 = tem1 * tem1

      else
          stla = sin(tla*pifac)
          tem1 = cos(tla*pifac)
          ctl2 = tem1 * tem1

      endif
        if (pres .ge. refp(1)) then
          tem = refr(1)
        elseif (pres .ge. refp(2)) then
          tem = refr(1) + (pres-refp(1)) * drdp(1)
        elseif (pres .ge. refp(3)) then
          tem = refr(2) + (pres-refp(2)) * drdp(2)
        elseif (pres .ge. refp(4)) then
          tem = refr(3) + (pres-refp(3)) * drdp(3)
        elseif (pres .ge. refp(5)) then
          tem = refr(4) + (pres-refp(4)) * drdp(4)
        elseif (pres .ge. refp(6)) then
          tem = refr(5) + (pres-refp(5)) * drdp(5)
        else
          tem = refr(6)
        endif
        al2 = max(alm, 2.0e-4/tem)
!
      return
      end
