      subroutine get_phi(im,ix,levs,cp,nu,t,q,prki,prkl,phii,phil)
!
      use machine , only : kind_phys
      implicit none
!
      integer im, ix, levs
      real(kind=kind_phys) cp, nu
      real(kind=kind_phys) prki(ix,levs+1), prkl(ix,levs)               &
     &,                    phii(ix,levs+1), phil(ix,levs)               &
     &,                    t(ix,levs),      q(ix,levs)
      real(kind=kind_phys) tem, dphib, dphit
      integer i, l
!
      do i=1,im
        phii(i,1)   = 0.0           ! ignoring topography height here
      enddo
      do l=1,levs
        do i=1,im
          tem         = cp * t(i,l) * (1.0 + nu * max(q(i,l),0.0))      &
     &                / prkl(i,l)
          dphib       = (prki(i,l) - prkl(i,l)) * tem
          dphit       = (prkl(i,l  ) - prki(i,l+1)) * tem
          phil(i,l)   = phii(i,l) + dphib
          phii(i,l+1) = phil(i,l) + dphit
        enddo
      enddo
!
      return
      end
