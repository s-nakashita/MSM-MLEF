      subroutine rmpsyncol
!$$$  subprogram documentation block
!
! subprogram: 	rmpsyncol
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  set mpi barrier for comm_column nodes
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpsyncol
!
! subprograms called:
!   mpi_barrier  - set barrier for comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!ccc
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      call mpi_barrier(comm_column,ierr)
      if( ierr.ne.0 ) then
        print *,'PE',mype,':****** Error stop in mpsyncol ****** '
        print *,'PE',mype,':error code from mpi_barrier = ',ierr
        call rmpabort
      endif
#endif
      return
      end
