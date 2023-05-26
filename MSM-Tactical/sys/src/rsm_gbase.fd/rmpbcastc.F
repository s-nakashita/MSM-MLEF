      subroutine rmpbcastc(n,len)
!$$$  subprogram documentation block
!
! subprogram: 	rmpbcastc
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: broadcast character array to all pes
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	rmpbcastc(n,len)
!
!    input argument lists:
!	n	- character (len) charater array from master pe 
!	len	- integer length of array n
!
!    output argument list:
!	n	- character (len) charater array from master pe 
! 
! subprograms called:
!   mpi_bcast	- to broadcast to all pe in the comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
!
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      integer len,ierr
      character*1 n(len)
!
!      print *,'in rmpbcastc now'
      call mpi_bcast(n,len,mpi_character,0,mpi_comm_world,ierr)
!
#endif
      return
      end
