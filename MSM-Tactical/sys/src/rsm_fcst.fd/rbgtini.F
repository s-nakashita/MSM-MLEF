      subroutine rbgtini(junit1,bgt,kk,nn)
#include <paramodel.h>
#ifdef A
      dimension bgt(lngrd)
#endif
!
!     initialized budget for restart
!
#ifdef A
      print *,' enter rbgtini k=',kk,' n=',nn,' junit1=',junit1
      do i=1,lngrd
        bgt(i)=0.0
      enddo
      do n=1,nn
        do k=1,kk
          write(junit1) bgt
        enddo
      enddo
      return
#endif
      end
