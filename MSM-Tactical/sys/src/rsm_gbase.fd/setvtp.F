      subroutine setvtp
      use machine , only : kind_phys
      use module_ras , only : nvtp,c1xvtp,c2xvtp,tbvtp
      implicit none

      real(kind=kind_phys) vtpexp,xinc,x,xmax,xmin
      integer jx
      parameter(vtpexp=-0.3636)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!fpp$ noconcur r
      xmin=0.05
      xmax=1.5
      xinc=(xmax-xmin)/(nvtp-1)
      c1xvtp=1.-xmin/xinc
      c2xvtp=1./xinc
      do jx=1,nvtp
        x         = xmin + (jx-1)*xinc
        tbvtp(jx) =        x ** vtpexp
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
