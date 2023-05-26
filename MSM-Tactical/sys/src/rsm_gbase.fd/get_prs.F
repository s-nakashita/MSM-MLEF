      subroutine get_prs(im,ix,levs,rkap,cp,nu,                         &
     &                   t,q,prsi,prki,prsl,prkl,phii,phil,del)
!
      use machine , only : kind_phys
      implicit none
!
      integer im, ix, levs
      real(kind=kind_phys) rkap, cp, nu
      real(kind=kind_phys) prsi(ix,levs+1), prki(ix,levs+1)             &
     &,                    phii(ix,levs+1), phil(ix,levs)               &
     &,                    prsl(ix,levs),   prkl(ix,levs)               &
     &,                    del(ix,levs),    t(ix,levs),   q(ix,levs)
      real(kind=kind_phys) rkapi, rkapp1, tem, dphib, dphit
      integer i, l
!
      rkapi  = 1.0 / rkap
      rkapp1 = 1.0 + rkap
!                                    pressure is in centibars!!!!
      do l=1,levs
        do i=1,im
          del(i,l) = prsi(i,l) - prsi(i,l+1)
        enddo
      enddo
      if (prki(1,1) .le. 0.0) then
        do i=1,im
          prki(i,1) = (prsi(i,1)*0.01) ** rkap
        enddo
        do l=1,levs
          do i=1,im
            prki(i,l+1) = (prsi(i,l+1)*0.01) ** rkap
            tem         = rkapp1 * del(i,l)
            prkl(i,l)   = (prki(i,l)*prsi(i,l)-prki(i,l+1)*prsi(i,l+1))  &
     &                  / tem
          enddo
        enddo
      elseif (prkl(1,1) .le. 0.0) then
        do l=1,levs
          do i=1,im
            tem         = rkapp1 * del(i,l)
            prkl(i,l)   = (prki(i,l)*prsi(i,l)-prki(i,l+1)*prsi(i,l+1))  &
     &                  / tem
          enddo
        enddo
      endif
      if (prsl(1,1) .le. 0.0) then
        do l=1,levs
          do i=1,im
            prsl(i,l)   = 100.0 * prkl(i,l) ** rkapi
          enddo
        enddo
      endif
!hmhj if (phil(1,levs) .le. 0.0) then ! if geopotential is not given, calculate
        do i=1,im
          phii(i,1)   = 0.0           ! ignoring topography height here
        enddo
        do l=1,levs
          do i=1,im
            tem         = cp * t(i,l) * (1.0 + nu * max(q(i,l),0.0))     & 
     &                  / prkl(i,l)
            dphib       = (prki(i,l) - prkl(i,l)) * tem
            dphit       = (prkl(i,l  ) - prki(i,l+1)) * tem
            phil(i,l)   = phii(i,l) + dphib
            phii(i,l+1) = phil(i,l) + dphit
          enddo
        enddo
!hmhj endif
!
      return
      end
