      subroutine rmpsp2f(a,lnwavp,b,lnwav,ntotal)
!$$$  subprogram documentation block
!
! subprogram:   rmpsp2f
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: transpose (lnpp,kf) to (lnf,kf)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call rmpsf2p(a,lnwav,b,lnwavp,ntotal)
!
!    input argument lists:
!       a       - real (lnwavp,ntotal) sub partial field
!       lnwav     - integer total spectral grid
!       lnwavp   - integer sub partial spectral grid
!       ntotal  - integer total set of fields
!
!    output argument list:
!       b       - real (lnwav,ntotal) total field
!
! subprograms called:
!   mpi_gatherv  - gather all pe in the same comm
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
      integer lnwav,lnwavp,ntotal,n,m,k,mk,ierr
      real    a,b
      dimension a(lnwavp,ntotal),b(lnwav,ntotal)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(lnwavp*ntotal))
      allocate(tmprcv(lnwav*ntotal))
      allocate(len(0:npes-1))
      allocate(loc(0:npes-1))
!
!
      if( mype.eq.master ) then
        mk=0
        do n=0,npes-1
          loc(n)=mk
          do k=1,ntotal
            do m=1,lntlen(n)
              mk=mk+1
            enddo
          enddo
          len(n)=ntotal*lntlen(n)
        enddo
      else
        len(mype)=ntotal*lntlen(mype)
      endif
!
      mk=0
      do k=1,ntotal
        do m=1,lntlen(mype)
          mk=mk+1
          tmpsnd(mk)=a(m,k)
        enddo
      enddo
!
!      call maxmin(tmpsnd, len(mype),1,1,1,'in rmpsp2f, tmpsnd')
!
      call mpi_gatherv(tmpsnd,len(mype),MPIREAL,                         & 
     &     tmprcv,len(0),loc(0),MPIREAL,0,mpi_comm_world,ierr)
!
      if( mype.eq.0 ) then
        mk=0
        do n=0,npes-1
          do k=1,ntotal
            do m=1,lntlen(n)
              mk=mk+1
              b(m+lntstr(n),k)=tmprcv(mk)
            enddo
          enddo
        enddo
!jw
      else
        b(1:lnwav,1:ntotal)=0.
      endif
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
!
      call rmpsynall
#endif
      return
      end
