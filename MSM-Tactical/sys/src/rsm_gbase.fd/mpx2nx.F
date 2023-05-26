      subroutine mpx2nx(lonfx,a,lonf2,lota,b,lonf2p,lotb,latg2p,         &
     &                  leva,levb,nlev)
!$$$  subprogram documentation block
!
! subprogram:   mpx2nx
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:   transpose (if,k,jp) to (ip,k,jp)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call mpx2nx(a,lonf2,lota,b,lonf2p,lotb,latg2p,
!        &                  leva,levb,nlev)
!
!    input argument lists:
!       a       - real (lonf2,lota,latg2p) partial field in j k
!       lonf2p  - integer partial longitude grid x 2
!       latg2p  - integer partial latitude grid / 2
!       lonf2   - integer total longitude grid x 2
!       leva    - integer starting vertical layer for a
!       levb    - integer starting vertical layer for b
!       nlev    - integer total set of fields
!
!    output argument list:
!       b       - real (lonf2p,lotb,latg2p) partial field in i j
!
! subprograms called:
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
      implicit none
#include <npesi.h>
#include <comrmpi.h>
#include <commpi.h>
      integer lonfx,lonf2,lonf2p,latg2p,lota,lotb,leva,levb,nlev         &                     
     &       ,i,j,n,ioff,noffa,noffb,lonf2d,lonfp,lonff
      real    a,b
      dimension a(lonf2,lota,latg2p),b(lonf2p,lotb,latg2p)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do j=1,latg2p
          do n=1,lotb
            do i=1,lonf2
              b(i,n,j) = a(i,n,j)
            enddo
          enddo
        enddo
        return
      endif
!
! otherwise
! cut the need and through away the remain.
      lonff=lonfx
      lonfp=glonlen(mype)
      ioff=glonstr(mype)-1
      do j=1,glatlen(mype)
        do n=1,nlev
          noffa=leva-1+n
          noffb=levb-1+n
          do i=1,lonfp
            b(i      ,noffb,j)=a(i+ioff      ,noffa,j)
            b(i+lonfp,noffb,j)=a(i+ioff+lonff,noffa,j)
          enddo
        enddo
      enddo
!
#endif
      return
      end
