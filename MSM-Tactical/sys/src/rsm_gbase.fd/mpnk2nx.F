      subroutine mpnk2nx(lonfx,a,lonf2,lota,                             &
     &                   b,lonf2p,lotb,latg2p,levsp,levs,                &
     &                   leva,levb,nvar)
!$$$  subprogram documentation block
!
! subprogram: 	mpnk2nx
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (if,kp) to (ip,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call mpnk2nx(a,lonf2,lota,
!        &                   b,lonf2p,lotb,latg2p,levsp,levs,
!        &                   leva,levb,nvar)
!
!    input argument lists:
!	a	- real (lonf2,lota,latg2p) partial field in j k
!	lonf2	- integer total longitude grid x 2
!	lota	- integer total field of nvar*levsp
!	lonf2p	- integer partial longitude grid x 2
!	lotb	- integer total field of nvar*levs
!	latg2p	- integer partial latitude grid / 2
!	levsp	- integer partial vertical grid 
!	levs	- integer total vertical grid 
!	leva	- integer start vertical grid for a
!	levb	- integer start vertical grid for b
!	nvar	- integer total set of fields
!
!    output argument list:
!	b	- real (lonf2p,lotb,latg2p) partial field in i j
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
      integer lonfx,lonf2,levsp,latg2p,lonf2p,levs,nvar,status           &                     
     &       ,lota,lotb,leva,levb,ioff,koff,lonff,lonf2d                 &
     &       ,i,j,k,ii,ipe,ipe0,n,m,mn,len,kp,kf,ierr                    &
     &       ,latlon,lonfp
      real    a,b
      dimension a(lonf2,lota,latg2p),b(lonf2p,lotb,latg2p)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do j=1,latg2p
          do k=1,lota
            do i=1,lonf2
              b(i,k,j) = a(i,k,j)
            enddo
          enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(lonf2p*latg2p*levsp*nvar*ncol))
      allocate(tmprcv(lonf2p*latg2p*levsp*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
!
      ipe0=int(mype/ncol)*ncol
! cut in x
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        ipe=(ii-1)+ipe0
        lonff=lonfx
        lonfp=glonlen(ipe)
        ioff=glonstr(ipe)-1
        do n=1,nvar
          kp=(n-1)*levsp+leva-1
          do j=1,glatlen(mype)
            do k=1,glevlen(mype)
              do i=1,lonfp
                mn=mn+1
                tmpsnd(mn) = a(i+ioff     ,k+kp,j)
                mn=mn+1
                tmpsnd(mn) = a(i+ioff+lonff,k+kp,j)
              enddo
            enddo
          enddo
        enddo
        lensnd(ii)=mn-locsnd(ii)
      enddo
!
      latlon=glatlen(mype)*glonlen(mype)
      mn=0
      do ii=1,ncol
        locrcv(ii)=mn
        ipe=(ii-1)+ipe0
        lenrcv(ii)=glevlen(ipe)*latlon*2*nvar
        mn=mn+lenrcv(ii)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &                           
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
! restore to k
      mn=0
      lonfp=glonlen(mype)
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        koff=glevstr(ipe)-1
        do n=1,nvar
          kf=(n-1)*levs+levb-1+koff
          do j=1,glatlen(mype)
            do k=1,glevlen(ipe)
              do i=1,lonfp
                mn=mn+1
                b(i      ,k+kf,j)=tmprcv(mn)
                mn=mn+1
                b(i+lonfp,k+kf,j)=tmprcv(mn)
              enddo
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
