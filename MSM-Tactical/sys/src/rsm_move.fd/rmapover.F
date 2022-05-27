      subroutine rmapover(gro,grn,map,imap,ijm,km)
!
      use machine , only : kind_io4
      integer  map(ijm),imap(ijm)
      real(kind=kind_io4) gro(ijm,km),grn(ijm,km)

!     print *,' enter rmapover '
      do i=1,ijm
         if( map(i).eq.1 ) then
            do k=1,km
              grn(i,k) = gro(imap(i),k)
            enddo
         endif
      enddo
!     print *,' end of rmapover '

      return
      end
