      subroutine rmpabort
!$$$  subprogram documentation block
!
! subprogram: 	rmpabort
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: abort the mpi by any pe.
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpfine
!
!    input argument lists:
!
!    output argument list:
! 
! subprograms called:
!   mpi_abort	- to abort mpi
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <npesi.h>
#include <comrmpi.h>
      call mpi_abort(mpi_comm_world,1,info)
      if( info.ne.0 ) then
        print *,'PE',mype,': ********* Error stop in rmpabort ****** '
        print *,'PE',mype,': error code from mpi_abort =',info
        call abort 
      endif
      return
      end
