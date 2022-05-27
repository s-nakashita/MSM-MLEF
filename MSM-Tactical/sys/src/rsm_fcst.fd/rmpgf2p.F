      subroutine rmpgf2p(a,igrd1,jgrd1,                                  &
     &                   b,igrd1p,jgrd1p,ntotal)
!$$$  subprogram documentation block
!
! subprogram: 	rmpgf2p
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: transpose (if,jf,kf) to (ip,jp,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call mpgf2p(a,igrd12,jgrd12,b,igrd12p,jgrd12p,ntotal)
!
!    input argument lists:
!	a	- real (igrd12,jgrd12,ntotal) full field 
!	igrd12	- integer total grid in longitude time 2
!	jgrd12	- integer total grid in latitude divide 2
!	ntotal	- integer total set of fields
!
!    output argument list:
!	b	- real (igrd12p,jgrd12p,ntotal) partial field for each pe 
!	igrd12p	- integer partial grid in longitude time 2
!	jgrd12p	- integer partial grid in latitude divide 2
! 
! subprograms called:
!   mpi_scatterv	- scatter message from master pe to all pe
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
!
#include <define.h>
#ifdef MP
      implicit none
#include <npesi.h>
#include <comrmpi.h>
      integer igrd1,jgrd1,igrd1p,jgrd1p,ntotal                            & 
     &       ,ii,jj,n,mk,i,j,k,ierr,lonlend,lonstrd,lonff
      real    a,b
      dimension a(igrd1,jgrd1,ntotal),b(igrd1p,jgrd1p,ntotal)
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(igrd1 *jgrd1 *ntotal))
      allocate(tmprcv(igrd1p*jgrd1p*ntotal))
      allocate(len(0:npes-1))
      allocate(loc(0:npes-1))
!
!
      if( mype.eq.master ) then
        mk=0
        do n=0,npes-1
          loc(n)=mk
          lonlend=lonlen(n)
          lonstrd=lonstr(n)-1
          do k=1,ntotal
            do j=1,latlen(n)
              jj=j+latstr(n)-1
              do i=1,lonlend
                ii=i+lonstrd
                mk=mk+1
                tmpsnd(mk)=a(ii,jj,k)
              enddo
            enddo
          enddo
          len(n)=mk-loc(n)
        enddo
      else
        len(mype)=ntotal*lonlen(mype)*latlen(mype)
      endif
!
      call mpi_scatterv(tmpsnd,len(0),loc(0),MPIREAL,                    & 
     &     tmprcv,len(mype),MPIREAL,0,mpi_comm_world,ierr)
!
      mk=0
      lonlend=lonlen(mype)
      do k=1,ntotal
        do j=1,latlen(mype)
          do i=1,lonlend
            mk=mk+1
            b(i        ,j,k)=tmprcv(mk)
          enddo
          do i=lonlend+1,igrd1p
            b(i,j,k)=tmprcv(mk)
          enddo
        enddo
        do j=latlen(mype)+1,jgrd1p
          do i=1,igrd1p
            b(i,j,k)=tmprcv(mk)
          enddo
        enddo
      enddo
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
!
#endif
      return
      end
