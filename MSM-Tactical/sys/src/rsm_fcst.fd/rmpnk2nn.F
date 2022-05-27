      subroutine rmpnk2nn(a,llwavp,levrp,b,lnwavp,levr,nvar)
!$$$  subprogram documentation block
!
! subprogram: 	rmpnk2nn
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (llwavp,levrp,nvar) to (lnwavp,levr,nvar)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpnk2nn(a,llwavp,levrp,b,lnwavp,levr,nvar)
!
!    input argument lists:
!	a	- real (llwavp,levrp,nvar) partial field 
!	llwavp	- integer partial spectral grid
!	levrp	- integer partial vertical grid
!	lnwavp	- integer sub partial spectral grid
!	levr	- integer full vertical grid
!	nvar	- integer total set of fields
!
!    output argument list:
!	b	- real (llwavp,levrp,nvar) partial field 
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
      integer lnwavp,levr,llwavp,levrp,nvar,status,lntstr0                &
     &       ,ii,ipe,ipe0,n,k,m,mn,len,ierr
      real    a,b
      dimension a(llwavp,levrp,nvar),b(lnwavp,levr,nvar)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do n=1,nvar
          do k=1,levr
            do m=1,llwavp
              b(m,k,n) = a(m,k,n)
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
      ipe0=int(mype/ncol)*ncol
! cut in n
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        ipe=(ii-1)+ipe0
        lntstr0=lntstr(ipe)-lnpstr(ipe0)
        do n=1,nvar
          do k=1,levlen(mype)
            do m=1+lntstr0,lntlen(ipe)+lntstr0
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
        lenrcv(ii)=levlen(ipe)*lntlen(mype)*nvar
        mn=mn+lenrcv(ii)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                    &
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                    &
     &                   comm_row,ierr)
!
! restore to k
      mn=0
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        do n=1,nvar
          do k=levstr(ipe),levlen(ipe)+levstr(ipe)-1
            do m=1,lntlen(mype)
              mn=mn+1
              b(m,k,n)=tmprcv(mn)
            enddo
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
