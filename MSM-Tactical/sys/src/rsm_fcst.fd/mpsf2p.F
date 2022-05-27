      subroutine mpsf2p(jcap,lwvdef,a,lnt,b,lntpp,ntotal)
!$$$  subprogram documentation block
!
! subprogram: 	mpsf2p
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: transpose (lnf,kf) to (lnpp,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call mpsf2p(a,lnt,b,lntpp,ntotal)
!
!    input argument lists:
!	a	- real (lnt,ntotal) total field
!	lnt	- integer total spectral grid
!	lntpp	- integer sub partial spectral grid
!	ntotal	- integer total set of fields
!
!    output argument list:
!	b	- real (lntpp,ntotal) sub partial field
! 
! subprograms called:
!   spcshfli		- shafle spectral for balance from input
!   spcshflo		- shafle spectral for output
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
#include <commpi.h>
      integer jcap,lnt,lntpp,ntotal                                      &                            
     &       ,n,m,k,mk,ierr
      real    a,b
      dimension a(lnt,ntotal),b(lntpp,ntotal)
      integer   lwvdef
      dimension lwvdef(jcap+1)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(lnt*ntotal))
      allocate(tmprcv(lntpp*ntotal))
      allocate(len(0:npes-1))
      allocate(loc(0:npes-1))
!
!
      if( mype.eq.master ) then
        call spcshfli(a,lnt,ntotal,jcap,lwvdef)
        mk=0
        do n=0,npes-1
          loc(n)=mk
          do k=1,ntotal
            do m=1,glntlen(n)*2
              mk=mk+1
              tmpsnd(mk)=a(m+glntstr(n)*2,k)
            enddo
          enddo
          len(n)=ntotal*glntlen(n)*2
        enddo
      else
        len(mype)=ntotal*glntlen(mype)*2
      endif
!
      call mpi_scatterv(tmpsnd,len(0),loc(0),MPIREAL,                    &                   
     &     tmprcv,len(mype),MPIREAL,0,mpi_comm_world,ierr)
!
      mk=0
      do k=1,ntotal
        do m=1,glntlen(mype)*2
          mk=mk+1
          b(m,k)=tmprcv(mk)
        enddo
      enddo
!
      if( mype.eq.master ) call spcshflo(a,lnt,ntotal,jcap,lwvdef)
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
!
#endif
      return
      end
