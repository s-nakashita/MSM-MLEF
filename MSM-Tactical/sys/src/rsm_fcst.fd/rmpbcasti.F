      subroutine rmpbcasti(n,len)
!$$$  subprogram documentation block
!
! subprogram: 	rmpbcasti
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  broadcast integer array to all pes
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	rmpbcasti(n,len)
!
!    input argument lists:
!	n	- integer (len) array from master pe 
!	len	- integer length of array n
!
!    output argument list:
!	n	- integer (len) array from master pe 
! 
! subprograms called:
!   mpi_bcast	- to broadcast to all pe in the comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      integer n,len
      dimension n(len)
!
      call mpi_bcast(n,len,mpi_integer,0,mpi_comm_world,ierr)
!
#endif
      return
      end
