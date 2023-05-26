      subroutine rbgtdif
#include <paramodel.h>
#include <rscomspc.h>
#include <rscombgt.h>
#ifdef A
      do k=1,levr
      do i=1,lnwav
#endif
#ifdef T
      tek(i,k)=( y(i,k)-tek(i,k))*rdt2
#endif
#ifdef Q
      rqk(i,k)=(rt(i,k)-rqk(i,k))*rdt2
#endif
#ifdef U
      uuk(i,k)=( x(i,k)-uuk(i,k))*rdt2
#endif
#ifdef V
      vvk(i,k)=( w(i,k)-vvk(i,k))*rdt2
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef T
      call cctogd(tek,wt(1,1,5),levr)
#endif
#ifdef Q
      call cctogd(rqk,wq(1,1,4),levr)
#endif
#ifdef U
      call sctogd(uuk,wu(1,1,6),levr)
#endif
#ifdef V
      call cstogd(vvk,wv(1,1,6),levr)
#endif
#ifdef A
      do k=1,levr
      do i=1,lnwav
#endif
#ifdef T
      tek(i,k)=tem(i,k)
#endif
#ifdef Q
      rqk(i,k)=rm (i,k)
#endif
#ifdef U
      uuk(i,k)=uum(i,k)
#endif
#ifdef V
      vvk(i,k)=vvm(i,k)
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef P
      do i=1,lnwav
      psk(i)=qm(i)
      enddo
#endif
      return
      end
