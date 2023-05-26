!**********************************************************************
      subroutine setindxoz(lngrd,lats18,xlat,ddy,jindx1,jindx2)
      use machine , only : kind_phys
!c
!
      integer lngrd,lats18,j
      real(kind=kind_phys) blat,dphi,pi,dphii,elat,colrad
      parameter (blat =-85.0, dphi=10.0)
      real(kind=kind_phys) gaul(lngrd),xlat(lngrd)
      integer jindx1(lngrd),jindx2(lngrd)
      real(kind=kind_phys) ddy(lngrd)
!
         pi=acos(-1.)
         do j=1,lngrd
          gaul(j) = xlat(j)*180.0/pi
         enddo
!
         dphii = 1.0 / dphi
         elat  = blat + (lats18-1)*dphi
!
         do j=1,lngrd
            ddy(j)     = (gaul(j) - blat) * dphii + 1.0
!
            jindx1(j)  =  ddy(j)
!
            jindx2(j)  = jindx1(j) + 1
!
            ddy(j)     = ddy(j) - jindx1(j)
!
            if (gaul(j) .le. blat) then
               jindx1(j) = 1
               jindx2(j) = 1
            endif
            if (gaul(j) .ge. elat) then
               jindx1(j) = lats18
               jindx2(j) = lats18
            endif
         enddo
!
      return
      end
