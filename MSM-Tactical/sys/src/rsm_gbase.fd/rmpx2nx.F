      subroutine rmpx2nx(a,igrd1,lota,                                   & 
     &                   b,igrd1p,lotb,jgrd1p,                           &
     &                   leva,levb,nvar)
!$$$  subprogram documentation block
!
! subprogram: 	rmpnk2nx
!            
!   prgmmr:  yifeng cui              org: sdsc       date: 02-07-05
!
! abstract:  transpose (if,k,kp) to (ip,k,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpx2nx(a,igrd1,lota,
!        &                   b,igrd1p,lotb,jgrd1p,
!        &                   leva,levb,nvar)
!
!    input argument lists:
!	a	- real (igrd12,lota,jgrd12p) partial field in j k
!	igrd12	- integer total longitude grid x 2
!	lota	- integer total field of nvar*levsp
!	igrd12p	- integer partial longitude grid x 2
!	lotb	- integer total field of nvar*levs
!	jgrd12p	- integer partial latitude grid / 2
!	leva	- integer start vertical grid for a
!	levb	- integer start vertical grid for b
!	nvar	- integer total set of fields
!
!    output argument list:
!	b	- real (igrd12p,lotb,jgrd12p) partial field in i j
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
      integer igrd1,jgrd1p,igrd1p,nvar,                                  &
     &       lota,lotb,leva,levb,lonf2d,noffa,                           &
     &       i,j,n,ii,ipe,ipe0,len,ierr,mn,noffb,                        &
     &       latlon,lonfp,ioff
      real    a,b
      dimension a(igrd1,lota,jgrd1p),b(igrd1p,lotb,jgrd1p)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do j=1,jgrd1p
          do n=1,lota
            do i=1,igrd1
              b(i,n,j) = a(i,n,j)
            enddo
          enddo
        enddo
        return
      endif
!
! otherwise
! cut the need and through away the remain.
!
!      if (mype.eq.0) then
!      print *,'in rmpx2nx,lonstr=',lonstr(mype),
!     &  'a=',(a(i+lonstr(mype),leva,1),i=1,5)
!      endif

      ioff=lonstr(mype)-1
      lonfp=lonlen(mype)
      do j=1,latlen(mype)
        do n=1,nvar
          noffa=leva-1+n
          noffb=levb-1+n   
          do i=1,lonfp
            b(i      ,noffb,j)=a(i+ioff      ,noffa,j)
          enddo
        enddo
      enddo
!
!      if (mype.eq.0) then
!      print *,'in rmpx2nx,after=',
!     &  'b=',(b(i,levb,1),i=1,5)
!      endif
#endif
      return
      end
