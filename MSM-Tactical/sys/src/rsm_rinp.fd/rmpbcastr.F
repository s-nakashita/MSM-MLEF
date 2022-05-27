      subroutine rmpbcastr(a,len)
!$$$  subprogram documentation block
!
! subprogram: 	rmpbcastr
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: broadcast real array to all pes
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	rmpbcastr(a,len)
!
!    input argument lists:
!	a	- real (len) array from master pe 
!	len	- integer length of array a
!
!    output argument list:
!	a	- real (len) array from master pe 
! 
! subprograms called:
!   mpi_bcast	- to broadcast to all pe in the comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <npesi.h>
#include <comrmpi.h>
      integer len,i
      real a
      dimension a(len)
!
      real(kind=kind_mpi),allocatable::tmpa(:)
!
      allocate(tmpa(len))
!
      if( mype.eq.master ) then
        do i=1,len
          tmpa(i) = a(i)
        enddo
      endif
!      call MPI_Bcast(tmpa,len,MPIREAL,0,mpi_comm_world,ierr)
      call mpi_bcast(tmpa,len,MPIREAL,0,mpi_comm_world,ierr)
      do i=1,len
        a(i) = tmpa(i)
      enddo
!           print *, 'processor ',mype,' got ',tmpa
!      do i=1,16
!          print *, 'con(',i,')=',a(i)
!      enddo
!
      deallocate(tmpa)
!
      return
      end
