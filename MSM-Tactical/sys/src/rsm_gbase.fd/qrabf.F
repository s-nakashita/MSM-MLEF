      subroutine qrabf(qrp,qraf,qrbf)
      use machine , only : kind_phys
      use module_ras , only : nqrp,c1xqrp,c2xqrp,tbqrp,tbqra,tbqrb
      implicit none
!
      real(kind=kind_phys) qrp, qraf, qrbf, xj, real_nqrp, one
      parameter (one=1.)
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      real_nqrp=real(nqrp)
      xj   = min(max(c1xqrp+c2xqrp*qrp,one),real_nqrp)
      jx   = min(xj,nqrp-one)
      xj   = xj - jx
      qraf = tbqra(jx)  + xj * (tbqra(jx+1)-tbqra(jx))
      qrbf = tbqrb(jx)  + xj * (tbqrb(jx+1)-tbqrb(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
