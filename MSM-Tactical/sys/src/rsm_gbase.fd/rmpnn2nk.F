      subroutine rmpnn2nk(a,lnwavp,levr,b,llwavp,levrp,nvar)
!$$$  subprogram documentation block
!
! subprogram:   rmpnn2nk
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (lnwavp,levr,nvar) to (llwavp,levrp,nvar)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call rmpnn2nk(a,lnwavp,levr,b,llwavp,levrp,nvar)
!
!    input argument lists:
!       a       - real (lnwavp,levr,nvar) partial field
!       lnwavp    - integer sub partial spectral grid
!       levrp   - integer partial vertical grid
!       llwavp   - integer partial spectral grid
!       levr    - integer full vertical grid
!       nvar    - integer total set of fields
!
!    output argument list:
!       b       - real (llwavp,levrp,nvar) partial field
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
      use cnttime
      implicit none
#include <npesi.h>
#include <comrmpi.h>
      integer lnwavp,levr,llwavp,levrp,nvar,status,lntstr0               &  
     &       ,ii,ipe,ipe0,n,k,m,mn,len,ierr
      real    a,b
      dimension a(lnwavp,levr,nvar),b(llwavp,levrp,nvar)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do n=1,nvar
          do k=1,levr
            do m=1,lnwavp
              b(m,k,n)=a(m,k,n)
            enddo
          enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(lnwavp*levrp*nvar*ncol))
      allocate(tmprcv(lnwavp*levrp*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
!
#ifdef CNT_5
      call cnttime_on("rmpnn2nk_before")
#endif
      ipe0=int(mype/ncol)*ncol
! cuting  k
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        ipe=(ii-1)+ipe0
        do n=1,nvar
          do k=levstr(ipe),levlen(ipe)+levstr(ipe)-1
            do m=1,lntlen(mype)
              mn=mn+1
              tmpsnd(mn)=a(m,k,n)
            enddo
          enddo
        enddo
        lensnd(ii)=mn-locsnd(ii)
      enddo
!
      mn=0
      do ii=1,ncol
        locrcv(ii)=mn
        ipe=(ii-1)+ipe0
        lenrcv(ii)=levlen(mype)*lntlen(ipe)*nvar
        mn=mn+lenrcv(ii)
      enddo
!
#ifdef CNT_5
      call cnttime_off("rmpnn2nk_before")
      call rmpsynrow
      call cnttime_on("rmpnn2nk_call")
#endif
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
#ifdef CNT_5
      call cnttime_off("rmpnn2nk_call")
      call cnttime_on("rmpnn2nk_after")
#endif
      if(ierr.ne.0) then
        print *,' error after alltoallv ',ierr
      endif
! restore  n
      mn=0
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        lntstr0=lntstr(ipe)-lnpstr(ipe0)
        do n=1,nvar
          do k=1,levlen(mype)
            do m=1+lntstr0,lntlen(ipe)+lntstr0
              mn=mn+1
              b(m,k,n)=tmprcv(mn)
            enddo
          enddo
        enddo
      enddo
#ifdef CNT_5
      call cnttime_off("rmpnn2nk_after")
#endif
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
