      subroutine mpnn2nk(a,lnt2p,levs,b,lln2p,levsp,nvar)
!$$$  subprogram documentation block
!
! subprogram:   mpnn2nk
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (lnt2p,levs,nvar) to (lln2p,levsp,nvar)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call mpnn2nk(a,lnt2p,levs,b,lln2p,levsp,nvar)
!
!    input argument lists:
!       a       - real (lnt2p,levs,nvar) partial field
!       lnt2p    - integer sub partial spectral grid
!       levsp   - integer partial vertical grid
!       lln2p   - integer partial spectral grid
!       levs    - integer full vertical grid
!       nvar    - integer total set of fields
!
!    output argument list:
!       b       - real (lln2p,levsp,nvar) partial field
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
      integer lnt2p,levs,lln2p,levsp,nvar,status,lntstr0                 &                   
     &       ,ii,ipe,ipe0,n,k,m,mn,len,ierr
      real    a,b
      dimension a(lnt2p,levs,nvar),b(lln2p,levsp,nvar)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do n=1,nvar
          do k=1,levs
            do m=1,lnt2p
              b(m,k,n)=a(m,k,n)
            enddo
          enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(lnt2p*levsp*nvar*ncol))
      allocate(tmprcv(lnt2p*levsp*nvar*ncol))
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
        ipe=(ii-1)+ipe0
        do n=1,nvar
          do k=1,glevlen(ipe)
            do m=1,glntlen(mype)*2
              mn=mn+1
              tmpsnd(mn)=a(m,k+glevstr(ipe)-1,n)
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
        lenrcv(ii)=glevlen(mype)*glntlen(ipe)*2*nvar
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
        lntstr0=glntstr(ipe)*2-glnpstr(ipe0)*2
        do n=1,nvar
          do k=1,glevlen(mype)
            do m=1,glntlen(ipe)*2
              mn=mn+1
              b(m+lntstr0,k,n)=tmprcv(mn)
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
