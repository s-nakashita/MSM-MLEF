      subroutine mpnn2n(a,lnt2p,b,lln2p,nvar)
!$$$  subprogram documentation block
!
! subprogram:   mpnn2n
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (lnt2p,nvar) to (lln2p,nvar)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call mpnn2n(a,lnt2p,b,lln2p,nvar)
!
!    input argument lists:
!       a       - real (lnt2p,nvar) sub partial field
!       lnt2p    - integer sub partial spectral grid
!       lln2p    - integer partial spectral grid
!       nvar    - integer total set of fields
!
!    output argument list:
!       b       - real (lln2p,nvar) partial field
!
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
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
      integer lnt2p,lln2p,nvar,status,lntstr0                            &                           
     &       ,ii,ipe,ipe0,n,m,mn,len,ierr
      real    a,b
      dimension a(lnt2p,nvar),b(lln2p,nvar)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do n=1,nvar
          do m=1,lnt2p
            b(m,n)=a(m,n)
          enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(lnt2p*nvar*ncol))
      allocate(tmprcv(lnt2p*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
      ipe0=int(mype/ncol)*ncol
!
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        do n=1,nvar
          do m=1,glntlen(mype)*2
            mn=mn+1
            tmpsnd(mn)=a(m,n)
          enddo
        enddo
        lensnd(ii)=mn-locsnd(ii)
      enddo
!
      mn=0
      do ii=1,ncol
        locrcv(ii)=mn
        ipe=(ii-1)+ipe0
        lenrcv(ii)=glntlen(ipe)*2*nvar
        mn=mn+lenrcv(ii)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &                    
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
! received
      mn=0
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        lntstr0=glntstr(ipe)*2-glnpstr(ipe0)*2
        do n=1,nvar
          do m=1,glntlen(ipe)*2
            mn=mn+1
            b(m+lntstr0,n)=tmprcv(mn)
          enddo
        enddo
      enddo
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(lensnd)
      deallocate(lenrcv)
      deallocate(locsnd)
      deallocate(locrcv)
!
#endif
      return
      end
