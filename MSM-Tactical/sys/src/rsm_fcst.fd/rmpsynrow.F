      subroutine rmpsynrow
!$$$  subprogram documentation block
!
! subprogram: 	rmpsynrow
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: set mpi barrier for comm_row nodes
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpsynrow
!
! subprograms called:
!   mpi_barrier  - set barrier for comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!cc
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      call mpi_barrier(comm_row,ierr)
      if( ierr.ne.0 ) then
        print *,'PE',mype,':***** Error stop in mpsynrow ******** '
        print *,'PE',mype,':error code from mpi_barrier = ',ierr
        call rmpabort
      endif
#endif
      return
      end
