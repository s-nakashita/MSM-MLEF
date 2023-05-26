      subroutine rmpfine(endwtime)
!$$$  subprogram documentation block
!
! subprogram: 	rmpfine
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: finalizing the mpi by each pe.
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
!   mpi_finalize	- to end of mpi
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      endwtime=mpi_wtime()
      call mpi_finalize(info)
      if( info.ne.0 ) then
        print *,'PE',mype,': ********* Error stop in rmpfine ******* '
        print *,'PE',mype,': error code from mpi_finalize =',info
        call rmpabort 
      endif
#endif
      return
      end
