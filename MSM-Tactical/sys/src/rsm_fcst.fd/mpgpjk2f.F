      subroutine mpgpjk2f(lonfx,a,lonf2x,latg2p,b,lonf2,latg2,           & 
     &                    kp,kf,latdef,latgx)
!$$$  subprogram documentation block
!
! subprogram: 	mpgp2f
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (ip,jp,kf) to (if,jf,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call mpgp2f(a,lonf2p,latg2p,b,lonf2,latg2,ntotal)
!
!    input argument lists:
!	b	- real (lonf2p,latg2p,ntotal) partial field for each pe 
!	lonf2p	- integer partial grid in longitude time 2
!	latg2p	- integer partial grid in latitude divide 2
!	ntotal	- integer total set of fields
!
!    output argument list:
!	a	- real (lonf2,latg2,ntotal) full field 
!	lonf2	- integer total grid in longitude time 2
!	latg2	- integer total grid in latitude divide 2
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
#include <commpi.h>
!
      integer lonfx,lonf2x,latg2p,lonf2,latg2,kp,kf                      &                        
     &       ,ii,jj,kk,n,i,j,k,mk,ierr
      real    a,b
      dimension a(lonf2x,latg2p,kp),b(lonf2,latg2,kf)
      integer   latdef,latgx
      dimension latdef(latgx/2)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::len(:),loc(:)
!
      allocate(tmpsnd(lonf2x*latg2p*kp))
      allocate(tmprcv(lonf2 *latg2 *kf))
      allocate(len(0:npes-1))
      allocate(loc(0:npes-1))
!
!
      if( mype.eq.master ) then
        mk=0
        do n=0,npes-1
          loc(n)=mk
          len(n)=glevlen(n)*2*glatlen(n)*lonfx*2
          mk=loc(n)+len(n)
        enddo
      endif
!
!
      mk=0
      do k=1,glevlen(mype)
        do j=1,glatlen(mype)
          do i=1,lonfx*2
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
          do k=1,glevlen(n)
            kk=k+glevstr(n)
            do j=1,glatlen(n)
              jj=j+glatstr(n)-1
              jj=latdef(jj)
              do i=1,lonfx*2
                mk=mk+1
                b(i,jj,kk)=tmprcv(mk)
              enddo
            enddo
          enddo
        enddo
!jw initialization for all other processors
      else
        b=0.
      endif
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(len)
      deallocate(loc)
#endif
!
      return
      end
