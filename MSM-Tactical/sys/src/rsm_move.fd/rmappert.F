      subroutine rmappert(im,jm,ijm,gro,grb,grn,map,imap,fmap)
!
      use machine , only : kind_io4
!
      integer  map(ijm),imap(ijm)
      real(kind=kind_io4) gro(ijm),grb(ijm),grn(ijm),fmap(ijm,4)
      real(kind=kind_io4) a00,a10,a01,a11

!     print *,' enter rmappert '
      do i=1,ijm
         if( map(i).eq.1 ) then
            i00 = imap(i)
            i10 = i00 + 1
            i01 = i00 + im
            i11 = i00 + im + 1
            a00 = fmap(i,1)*( gro(i00) - grb(i00) )
            a10 = fmap(i,2)*( gro(i10) - grb(i10) )
            a01 = fmap(i,3)*( gro(i01) - grb(i01) )
            a11 = fmap(i,4)*( gro(i11) - grb(i11) )
            grn(i) = grn(i) + a00 + a01 + a10 + a11
         endif
      enddo
!     print *,' end of rmappert '

      return
      end
