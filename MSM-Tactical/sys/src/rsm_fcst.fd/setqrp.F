      subroutine setqrp
      use machine , only : kind_phys
      use module_ras , only : nqrp,c1xqrp,c2xqrp,tbqrp,tbqra,tbqrb
      implicit none

      real(kind=kind_phys) tem2,tem1,x,xinc,xmax,xmin
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!fpp$ noconcur r
!     xmin=1.0e-6
      xmin=0.0
      xmax=5.0
      xinc=(xmax-xmin)/(nqrp-1)
      c1xqrp=1.-xmin/xinc
      c2xqrp=1./xinc
      tem1 = 0.001 ** 0.2046
      tem2 = 0.001 ** 0.525
      do jx=1,nqrp
        x         = xmin + (jx-1)*xinc
        tbqrp(jx) =        x ** 0.1364
        tbqra(jx) = tem1 * x ** 0.2046
        tbqrb(jx) = tem2 * x ** 0.525
      enddo    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
