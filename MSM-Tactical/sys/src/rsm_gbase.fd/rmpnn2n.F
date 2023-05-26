      subroutine rmpnn2n(a,lnwavp,b,llwavp,nvar)
!$$$  subprogram documentation block
!
! subprogram:   rmpnn2n
!
! prgmmr: hann-ming henry juang    org:w/np51   date:02-07-01
!
! abstract:  transpose (lnwavp,nvar) to (llwavp,nvar)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call rmpnn2n(a,lnwavp,b,llwavp,nvar)
!
!    input argument lists:
!       a       - real (lnwavp,nvar) partial field
!       lnwavp    - integer sub partial spectral grid
!       llwavp   - integer partial spectral grid
!       nvar    - integer total set of fields
!
!    output argument list:
!       b       - real (llwavp,nvar) partial field
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
      integer lnwavp,llwavp,nvar,status,lntstr0                          & 
     &       ,ii,ipe,ipe0,n,m,mn,len,ierr
      real    a,b
      dimension a(lnwavp,nvar),b(llwavp,nvar)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do n=1,nvar
            do m=1,lnwavp
              b(m,n)=a(m,n)
            enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(lnwavp*nvar*ncol))
      allocate(tmprcv(lnwavp*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
!
      ipe0=int(mype/ncol)*ncol
! cuting  k
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        do n=1,nvar
          do m=1,lntlen(mype)
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
        lenrcv(ii)=lntlen(ipe)*nvar
        mn=mn+lenrcv(ii)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   & 
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
      if(ierr.ne.0) then
        print *,' error after alltoallv ',ierr
      endif
! restore  n
      mn=0
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        lntstr0=lntstr(ipe)-lnpstr(ipe0)
        do n=1,nvar
          do m=1+lntstr0,lntlen(ipe)+lntstr0
            mn=mn+1
            b(m,n)=tmprcv(mn)
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
