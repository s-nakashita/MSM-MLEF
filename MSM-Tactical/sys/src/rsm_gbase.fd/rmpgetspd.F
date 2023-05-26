      subroutine rmpgetspd(levr,spdmax)
!$$$  subprogram documentation block
!
! subprogram: 	rmpgetspd
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  get spdmax from all pe
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpgetspd(spdmax)
!
!    input argument lists:
!	spdmax	- real (levs) array of maximal speed for each pe
!
!    output argument list:
!	spdmax	- real (levs) array of maximal speed from all pe for
!                 master pe.
! 
! subprograms called:
!   mpi_allreduce - to gather message from all pe  to master
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
      integer k,ierr,levr
      real    spdmax
      dimension spdmax(levr)
      real(kind=kind_mpi),allocatable::spdsnt(:),spdrcv(:)
!
      allocate(spdsnt(levr))
      allocate(spdrcv(levr))
!
      do k=1,levr
        spdsnt(k)=spdmax(k)
      enddo
      call mpi_allreduce(spdsnt,spdrcv,levr,MPIREAL,                     & 
     &                   mpi_max,mpi_comm_world,ierr)
      do k=1,levr
        spdmax(k)=spdrcv(k)
      enddo
!
      deallocate(spdsnt)
      deallocate(spdrcv)
!
#endif
      return
      end
