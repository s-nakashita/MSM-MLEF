      subroutine rmpgp2f(a,igrd1p,jgrd1p,b,igrd1,jgrd1,ntotal)
!$$$  subprogram documentation block
!
! subprogram: 	rmpgp2f
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (ip,jp,kf) to (if,jf,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpgp2f(a,igrd1p,jgrd1p,b,igrd1,jgrd1,ntotal)
!
!    input argument lists:
!	b	- real (igrd1p,jgrd1p,ntotal) partial field for each pe 
!	igrd1p	- integer partial grid in longitude time 2
!	jgrd1p	- integer partial grid in latitude divide 2
!	ntotal	- integer total set of fields
!
!    output argument list:
!	a	- real (igrd1,jgrd1,ntotal) full field 
!	igrd1	- integer total grid in longitude time 2
!	jgrd1	- integer total grid in latitude divide 2
! 
! subprograms called:
!   mpi_gatherv	- gather message from all pe to master
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
      integer igrd1p,jgrd1p,igrd1,jgrd1,ntotal                           & 
     &       ,ii,jj,n,i,j,k,mk,ierr,lonlend,lonstrd,lonff
      real    a,b
      dimension a(igrd1p,jgrd1p,ntotal),b(igrd1,jgrd1,ntotal)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(igrd1p*jgrd1p*ntotal))
      allocate(tmprcv(igrd1 *jgrd1 *ntotal))
      allocate(len(0:npes-1))
      allocate(loc(0:npes-1))
!
       if( mype.eq.master ) then
        mk=0
        do n=0,npes-1
          loc(n)=mk
          len(n)=ntotal*lonlen(n)*latlen(n)
          mk=loc(n)+len(n)
        enddo
       endif
!      
      lonlend=lonlen(mype)
      mk=0
      do k=1,ntotal
        do j=1,latlen(mype)
          do i=1,lonlend
            mk=mk+1
            tmpsnd(mk)=a(i,j,k)
          enddo
        enddo
      enddo
      len(mype)=mk
!
      call mpi_gatherv(tmpsnd,len(mype),MPIREAL,                         &  
     &     tmprcv,len(0),loc(0),MPIREAL,0,mpi_comm_world,ierr)
!
      if( mype.eq.0 ) then
        mk=0
        do n=0,npes-1
          lonlend=lonlen(n)
          lonstrd=lonstr(n)-1
          do k=1,ntotal
            do j=1,latlen(n)
              jj=j+latstr(n)-1
              do i=1,lonlend
                ii=i+lonstrd
                mk=mk+1
                b(ii,jj,k)=tmprcv(mk)
              enddo
            enddo
          enddo
        enddo
!jw initialize for all other processors
      else
        b=0.
      endif
!      print*,'finish routine rmpgp2f'
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
!
#endif
      return
      end
