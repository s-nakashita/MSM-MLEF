      subroutine rbgtzero(bgt,kk,nn)
#include <paramodel.h>
#ifdef A
      dimension bgt(lngrd,kk,nn)
      print *,' enter rbgtzero k=',kk,' n=',nn
      do n=1,nn
        do k=1,kk
          do i=1,lngrd
            bgt(i,k,n)=0.0
          enddo
        enddo
      enddo
      return
#endif
      end
