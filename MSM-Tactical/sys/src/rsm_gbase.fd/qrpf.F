      function qrpf(qrp)
!
      use machine , only : kind_phys
      use module_ras , only : nqrp,c1xqrp,c2xqrp,tbqrp,tbqra,tbqrb
      implicit none

      real(kind=kind_phys) qrp, qrpf, xj, real_nqrp, one
      parameter (one=1.)
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      real_nqrp=real(nqrp)
      xj   = min(max(c1xqrp+c2xqrp*qrp,one),real_nqrp)
!     xj   = min(max(c1xqrp+c2xqrp*qrp,one),float(nqrp))
      jx   = min(xj,nqrp-one)
      qrpf = tbqrp(jx)  + (xj-jx) * (tbqrp(jx+1)-tbqrp(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
