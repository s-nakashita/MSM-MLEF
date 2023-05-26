      subroutine fl2i(fp,fm,fln,qln,levs1)
!ccj
#include <paramodel.h>
!cc
      parameter (len0p=jcap)
      parameter (len0m=jcap)
      dimension fp(2,0:len0p,levs1), fm(2,0:len0m,levs1),               &
     &          qln(2*lnt), fln(lnt22,levs1)
!
!     local scalars
!     -------------
!
      integer n, l, k
!
!     statement function
!     ------------------
!
!     offset(n,l) is the offset in words
!     to the (n,l)-element of a lower
!     triangular matrix of complex numbers
!     in an array containing the matrix
!     packed in column-major order,
!     where l and n range from 0 to jcap,
!     inclusive
!
!          lower triangular matrix of complex numbers:
!
!                     l -->
!
!                   x
!               n   x x
!                   x x x
!               |   x x x x
!               v   x x x x x
!                   x x x x x x
!
!          order of the matrix elements in memory:
!
!          (0,0), (1,0), (2,0), ..., (jcap,0), (1,1), (2,1), (3,1), ...
!
      integer offset
      offset(n,l) = (jcap+1)*(jcap+2) - (jcap+1-l)*(jcap+2-l) + 2*(n-l)
!
!     ----------------------------------------------------------------
!     compute the coefficients of the expansion in spherical harmonics
!     of the field at each level
!     ----------------------------------------------------------------
!
!fpp$ cncall
      do l = 0, jcap
!ccccccccccccccccccccccccccccccccc
#ifdef CRA
         ls=l*((2*jcap+3)-l)
#endif
!
!        compute the even (n-l) expansion coefficients for each level
!        ------------------------------------------------------------
!
!        real part
!
#ifdef CRA
         call sgerx1((jcap+2-l)/2,levs1,1.,qln(ls+1),4,                 &
     &               fp(1,l,1),(len0p+1)*2,fln(ls+1,1),4,lnt22)
#else
         do n = l, jcap, 2
            do k = 1, levs1
               fln(offset(n,l)+1,k) = fln(offset(n,l)+1,k)              &
     &                                + fp(1,l,k)*qln(offset(n,l)+1)
            end do
         end do
#endif
!
!        imaginary part
!
#ifdef CRA
         call sgerx1((jcap+2-l)/2,levs1,1.,qln(ls+2),4,                 &
     &               fp(2,l,1),(len0p+1)*2,fln(ls+2,1),4,lnt22)
#else
         do n = l, jcap, 2
            do k = 1, levs1
               fln(offset(n,l)+2,k) = fln(offset(n,l)+2,k)              &
     &                                + fp(2,l,k)*qln(offset(n,l)+2)
            end do
         end do
#endif
!
!        compute the odd (n-l) expansion coefficients for each level
!        -----------------------------------------------------------
#ifdef CRA
         if(l.lt.jcap) then
#endif
!
!        real part
!
#ifdef CRA
           call sgerx1((jcap+1-l)/2,levs1,1.,qln(ls+3),4,                &
     &                 fm(1,l,1),(len0m+1)*2,fln(ls+3,1),4,lnt22)
#else
           do n = l+1, jcap, 2
              do k = 1, levs1
                 fln(offset(n,l)+1,k) = fln(offset(n,l)+1,k)             &
     &                                  + fm(1,l,k)*qln(offset(n,l)+1)
              end do
           end do
#endif
!
!        imaginary part
!
#ifdef CRA
           call sgerx1((jcap+1-l)/2,levs1,1.,qln(ls+4),4,               &
     &                 fm(2,l,1),(len0m+1)*2,fln(ls+4,1),4,lnt22)
#else
           do n = l+1, jcap, 2
              do k = 1, levs1
                 fln(offset(n,l)+2,k) = fln(offset(n,l)+2,k)            &
     &                                  + fm(2,l,k)*qln(offset(n,l)+2)
              end do
           end do
#endif
!
#ifdef CRA
         endif
#endif
!ccccccccccccccccccccccccccccccccccccccc
      end do
!
      return
      end
