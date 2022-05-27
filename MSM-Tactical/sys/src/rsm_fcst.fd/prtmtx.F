      subroutine prtmtx(a,im,jm,ch)
#include <paramodel.h>
#ifdef NONHYD
      character ch*(*)
      dimension a(im,jm),b(levr+1,levr+1)
      fmax=abs(a(1,1))
      do j=1,jm
        do i=1,im
          fmax=max(fmax,abs(a(i,j)))
        enddo
      enddo
      if (fmax.eq.0.0) then
        print *,' this matrix is all zero.'
        return
      endif
      do j=1,jm
        do i=1,im
          b(i,j)=a(i,j)/fmax*100.
          if( b(i,j).eq.0.0 ) b(i,j)=111.0
        enddo
      enddo
      print *,' matrix ',ch,' abs max =',fmax
      print *,' matrix nornalized to its abs max then * 100 '
      print *,' and 111 represents zero values '
      do i=1,im
        write(*,100) (nint(b(i,j)),j=1,jm)
 100    format( 20 i4 )
      enddo
#endif
      return
      end
