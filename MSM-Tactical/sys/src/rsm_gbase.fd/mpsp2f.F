      subroutine mpsp2f(jcap,lwvdef,a,lntpp,b,lnt,ntotal)
!$$$  subprogram documentation block
!
! subprogram:   mpsp2f
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: transpose (lnpp,kf) to (lnf,kf)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call mpsf2p(a,lnt,b,lntpp,ntotal)
!
!    input argument lists:
!       a       - real (lntpp,ntotal) sub partial field
!       lnt     - integer total spectral grid
!       lntpp   - integer sub partial spectral grid
!       ntotal  - integer total set of fields
!
!    output argument list:
!       b       - real (lnt,ntotal) total field
!
! subprograms called:
!   spcshflo	 - shafle spectral for output
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
#include <commpi.h>
      integer jcap,lnt,lntpp,ntotal                                      &                   
     &       ,n,m,k,mk,ierr
      real    a,b
      dimension a(lntpp,ntotal),b(lnt,ntotal)
      integer   lwvdef
      dimension lwvdef(jcap+1)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(lntpp*ntotal))
      allocate(tmprcv(lnt*ntotal))
      allocate(len(0:npes-1))
      allocate(loc(0:npes-1))
!
!
      if( mype.eq.master ) then
        mk=0
        do n=0,npes-1
          loc(n)=mk
          do k=1,ntotal
            do m=1,glntlen(n)*2
              mk=mk+1
            enddo
          enddo
          len(n)=ntotal*glntlen(n)*2
        enddo
      else
        len(mype)=ntotal*glntlen(mype)*2
      endif
!
      mk=0
      do k=1,ntotal
        do m=1,glntlen(mype)*2
          mk=mk+1
          tmpsnd(mk)=a(m,k)
        enddo
      enddo
!
      call mpi_gatherv(tmpsnd,len(mype),MPIREAL,                         &                      
     &     tmprcv,len(0),loc(0),MPIREAL,0,mpi_comm_world,ierr)
!
      if( mype.eq.0 ) then
        mk=0
        do n=0,npes-1
          do k=1,ntotal
            do m=1,glntlen(n)*2
              mk=mk+1
              b(m+glntstr(n)*2,k)=tmprcv(mk)
            enddo
          enddo
        enddo
!jw  initialization b for all other processors
      else
         b=0.
      endif
!
      if( mype.eq.master ) call spcshflo(b,lnt,ntotal,jcap,lwvdef)
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
!
#endif
      return
      end
