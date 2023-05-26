      subroutine rbgtlpa
#include <paramodel.h>
#include <rscomspc.h>
#include <rscombgt.h>
#ifdef A
      do k=1,levr
      do i=1,lnwav
#endif
#ifdef T
      tek(i,k)=y(i,k)
#endif
#ifdef U
      uuk(i,k)=x(i,k)
#endif
#ifdef V
      vvk(i,k)=w(i,k)
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef P
      do i=1,lnwav
      psk(i)=z(i)
      enddo
#endif
      return
      end

