#include <define.h>
      subroutine setras( dt, nsphys, fh, rannum, dtf, seed0,             &    
     &                   krsize, nrnd, ncrnd, me, ii, jj)
!
      use machine , only : kind_phys
      use module_ras , only : krmin, krmax, afc, set_ras_afc
      implicit none

      integer              krsize, nsphys, ncrnd, me, nrnd(krsize)       &
     &,                    ii, jj
      real(kind=kind_phys) dtf, dt, fh, seed0,  rannum(200,nsphys)
      integer              iseed
!
!     compute ncrnd and the random numbers
!
!     ncrnd   = 42 * (dt/3600) + 0.50001

!     ncrnd   = (krmax-krmin+1) * (dtf/1200) + 0.50001
!     ncrnd   = min(28, (krmax-krmin+1)) * (dtf/1200) + 0.50001
      ncrnd   = min(20, (krmax(ii,jj)-krmin(ii,jj)+1)) * (dtf/1200)      &  
     &        + 0.50001
      if (dt .gt. dtf) ncrnd = (5*ncrnd) / 4
      ncrnd   = max(ncrnd, 1)

!        print *,' ncrnd=',ncrnd,' dt=',dt,' dtf=',dtf
!    *,          ' rasalf=',rasalf
!
      iseed = fh*3600.0 + 0.0001
      iseed = max(iseed, 1)
!
      nrnd = mod(100.0*sqrt(float(iseed)),1.0e9) + 1 + seed0
#ifdef IBMSP
      call random_seed(generator=2)
#else
      call random_seed
#endif
      call random_seed(put=nrnd)
      call random_number(rannum)
!
      call set_ras_afc(dt)
!
      return
      end
