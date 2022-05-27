      subroutine bilinear(gg,ijmd,rr,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
!
! ----- bilinear interpolation
!
      dimension gg(ijmd),rr(lngrd)					&
               ,n00(lngrd),n01(lngrd),n10(lngrd),n11(lngrd)		&
               ,d00(lngrd),d01(lngrd),d10(lngrd),d11(lngrd)
!
      do n = 1,lngrd
        rr(n)   = gg(n00(n)) * d00(n)					&
                + gg(n10(n)) * d10(n)					&
                + gg(n11(n)) * d11(n)					&
                + gg(n01(n)) * d01(n)
      enddo
!
      return
      end
