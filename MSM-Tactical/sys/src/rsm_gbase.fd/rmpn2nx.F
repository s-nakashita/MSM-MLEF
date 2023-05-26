      subroutine rmpn2nx(a,igrd1,b,igrd1p,jgrd1p,nvar)
!$$$  subprogram documentation block
!
! subprogram: 	rmpn2nx
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (if,kp) to (ip,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpnk2n(igrd1,a,igrd12,
!        &                   b,igrd12p,jgrd12p,
!        &                   nvar)
!
!    input argument lists:
!	a	- real (igrd12,jgrd12p) partial field in j k
!	igrd12	- integer total longitude grid x 2
!	igrd12p	- integer partial longitude grid x 2
!	jgrd12p	- integer partial latitude grid / 2
!	nvar	- integer total set of fields
!
!    output argument list:
!	b	- real (igrd12p,jgrd12p) partial field in i j
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
      integer igrd1,jgrd1p,igrd1p,nvar,status                            & 
     &       ,ioff,koff,lonf2d                                           &
     &       ,i,j,k,ii,ipe,ipe0,n,m,mn,len,kp,kf,ierr                    &
     &       ,latlon,lonfp,ioffi
      real    a,b
      dimension a(igrd1,jgrd1p),b(igrd1p,jgrd1p)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do j=1,jgrd1p
            do i=1,igrd1
              b(i,j) = a(i,j)
            enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(igrd1p*jgrd1p*nvar*ncol))
      allocate(tmprcv(igrd1p*jgrd1p*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
!
      ipe0=int(mype/ncol)*ncol
!      print*,'ipe0=',ipe0
! cut in x
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        ipe=(ii-1)+ipe0
        lonfp=lonlen(ipe)
        ioff=lonstr(ipe)-1
        do n=1,nvar
          do j=1,latlen(mype)
              do i=1+ioff,lonfp+ioff
                mn=mn+1
                tmpsnd(mn) = a(i,j)
              enddo
          enddo
        enddo
        lensnd(ii)=mn-locsnd(ii)
      enddo
!
      latlon=latlen(mype)*lonlen(mype)
!
      mn=0
      do ii=1,ncol
        locrcv(ii)=mn
        ipe=(ii-1)+ipe0
        lenrcv(ii)=latlon*nvar
        mn=mn+lenrcv(ii)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                    &
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                    &
     &                   comm_row,ierr)
! restore to k
      mn=0
      lonfp=lonlen(mype)
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        do n=1,nvar
          do j=1,latlen(mype)
              do i=1,lonfp
                mn=mn+1
                b(i,j)=tmprcv(mn)
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
