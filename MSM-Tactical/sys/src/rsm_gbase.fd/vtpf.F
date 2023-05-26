      function vtpf(ror)
!
      use machine , only : kind_phys
      use module_ras , only : nvtp,c1xvtp,c2xvtp,tbvtp
      implicit none
      real(kind=kind_phys) ror, vtpf, xj, real_nvtp, one
      parameter (one=1.)
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      real_nvtp=real(nvtp)
      xj   = min(max(c1xvtp+c2xvtp*ror,one),real_nvtp)
      jx   = min(xj,nvtp-one)
      vtpf = tbvtp(jx)  + (xj-jx) * (tbvtp(jx+1)-tbvtp(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
