!
      subroutine rbgtsic
#include <paramodel.h>
#include <rscomspc.h>
#include <rscombgt.h>
#ifdef A
      do k=1,levr
      do i=1,lnwav
#endif
#ifdef T
      tek(i,k)=y(i,k)-tek(i,k)
#endif
#ifdef U
      uuk(i,k)=x(i,k)-uuk(i,k)
#endif
#ifdef V
      vvk(i,k)=w(i,k)-vvk(i,k)
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef P
      do i=1,lnwav
      psk(i)=z(i)-psk(i)
      enddo
#endif
#ifdef T
      call cctogd(tek,wt(1,1,12),levr)
#endif
#ifdef U
      call sctogd(uuk,wu(1,1,8),levr)
#endif
#ifdef V
      call cstogd(vvk,wv(1,1,8),levr)
#endif
#ifdef P
      call cctogd(psk,wp(1,1,3),1)
#endif
      return
      end
