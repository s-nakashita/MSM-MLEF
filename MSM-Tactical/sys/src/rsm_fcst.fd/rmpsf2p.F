      subroutine rmpsf2p(a,lnwav,b,llwavp,ntotal)
!$$$  subprogram documentation block
!
! subprogram: 	rmpsf2p
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: transpose (lnf,kf) to (lnpp,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpsf2p(a,lnwav,b,llwavp,ntotal)
!
!    input argument lists:
!	a	- real (lnwav,ntotal) total field
!	lnwav	- integer total spectral grid
!	llwavp	- integer sub partial spectral grid
!	ntotal	- integer total set of fields
!
!    output argument list:
!	b	- real (llwavp,ntotal) sub partial field
! 
! subprograms called:
!   mpi_scatterv  - send to all pe in the same comm
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
      integer lnwav,llwavp,ntotal ,n,m,k,mk,ierr
      real    a,b
      dimension a(lnwav,ntotal),b(llwavp,ntotal)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(lnwav*ntotal))
      allocate(tmprcv(llwavp*ntotal))
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
              tmpsnd(mk)=a(m+lntstr(n),k)
            enddo
          enddo
          len(n)=ntotal*lntlen(n)
        enddo
      else
        len(mype)=ntotal*lntlen(mype)
      endif
!
      call mpi_scatterv(tmpsnd,len(0),loc(0),MPIREAL,                    &  
     &     tmprcv,len(mype),MPIREAL,0,mpi_comm_world,ierr)
!
      mk=0
      do k=1,ntotal
        do m=1,lntlen(mype)
          mk=mk+1
          b(m,k)=tmprcv(mk)
        enddo
      enddo
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
!
#endif
      return
      end
