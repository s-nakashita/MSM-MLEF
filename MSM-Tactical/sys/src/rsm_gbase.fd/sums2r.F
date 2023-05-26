!fpp$ noconcur r
      subroutine sums2r(fln,ap,qln,levs1)
!ccj
#include <paramodel.h>
!cc
      parameter (len0=lonr)
      parameter (lenh=lonr/2)
      dimension ap(2,0:len0,levs1), qln(2*lnt), fln(lnt22,levs1)
!
!     local scalars
!     -------------
!
      integer i, n, l, k
      real    evenr, eveni
!
!     statement functions
!     -------------------
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
      offset(n,l) = (jcap+1)*(jcap+2) - (jcap-l+1)*(jcap-l+2) + 2*(n-l)
!
!     ---
!
!     term(1,n,l,k) and term(2,n,l,k) are
!     the real and imaginary part, resp.,
!     of exp((0,1)*l*phi) times the (n,l) term
!     in the expansion in spherical
!     harmonics of the field at level k,
!     where phi is the azimuthal angle
!
      term(i,n,l,k) = qln(offset(n,l)+i)*fln(offset(n,l)+i,k)
!
!     zero the accumulators
!     ---------------------
!
      do k = 1, levs1
         do l = 0, jcap
            ap(1,l,k) = 0.
            ap(2,l,k) = 0.
            ap(1,lenh+l,k) = 0.
            ap(2,lenh+l,k) = 0.
         end do
      end do
!
!     compute the even and odd (n-l) components
!     of the fourier coefficients
!     ---------------------------------------------------------
!
!fpp$ cncall
      do l = 0, jcap
!cccccccccccccccccccccccccccccccccc
#ifdef CRA
         ls=l*((2*jcap+3)-l)
#endif
!
!        compute the sum of the even (n-l) terms for each level
!        ------------------------------------------------------
!
!        real part
!
#ifdef CRA
         call sgemvx1(levs1,(jcap+2-l)/2,1.,fln(ls+1,1),lnt22,4,         &  
     &                qln(ls+1),4,1.,ap(1,l,1),(len0+1)*2)
#else
         do n = l, jcap, 2
            do k = 1, levs1
               ap(1,l,k) = ap(1,l,k) + term(1,n,l,k)
            end do
         end do
#endif
!
!        imaginary part
!
#ifdef CRA
         call sgemvx1(levs1,(jcap+2-l)/2,1.,fln(ls+2,1),lnt22,4,         &  
     &                qln(ls+2),4,1.,ap(2,l,1),(len0+1)*2)
#else
         do n = l, jcap, 2
            do k = 1, levs1
               ap(2,l,k) = ap(2,l,k) + term(2,n,l,k)
            end do
         end do
#endif
!
!        compute the sum of the odd (n-l) terms for each level
!        -----------------------------------------------------
#ifdef CRA
         if(l.lt.jcap) then
#endif
!
!        real part
!
#ifdef CRA
           call sgemvx1(levs1,(jcap+1-l)/2,1.,fln(ls+3,1),lnt22,4,       &  
     &                  qln(ls+3),4,1.,ap(1,lenh+l,1),(len0+1)*2)
#else
           do n = l+1, jcap, 2
              do k = 1, levs1
                 ap(1,lenh+l,k) = ap(1,lenh+l,k) + term(1,n,l,k)
              end do
           end do
#endif
!
!        imaginary part
!
#ifdef CRA
           call sgemvx1(levs1,(jcap+1-l)/2,1.,fln(ls+4,1),lnt22,4,       &
     &                  qln(ls+4),4,1.,ap(2,lenh+l,1),(len0+1)*2)
#else
           do n = l+1, jcap, 2
              do k = 1, levs1
                 ap(2,lenh+l,k) = ap(2,lenh+l,k) + term(2,n,l,k)
              end do
           end do
#endif
!
#ifdef CRA
         endif
#endif
!cccccccccccccccccccccccccccccccccccc
      end do
!
!
!     compute the fourier coefficients for each level
!     -----------------------------------------------
!
      do k = 1, levs1
         do l = 0, jcap
            evenr = ap(1,l,k)
            eveni = ap(2,l,k)
            ap(1,l,k) = ap(1,l,k) + ap(1,lenh+l,k)
            ap(2,l,k) = ap(2,l,k) + ap(2,lenh+l,k)
            ap(1,lenh+l,k) = evenr - ap(1,lenh+l,k)
            ap(2,lenh+l,k) = eveni - ap(2,lenh+l,k)
         end do
      end do
!
      return
      end
