      subroutine mptiming(index,name,time,io)
!$$$  subprogram documentation block
!
! subprogram: 	mptiming
!            
! prgmmr: hann-ming henry juang    org:w/np23   date:06-11-2003
!
! abstract: accumulated timing for each routines
!
! program history log:
!
! usage:	call mptiming(index,name,time,io)
!
!  index	routine index, initial is 0
!  name		routine name
!  time		routine initial time
!  io		0 for enter the routine, 1 to exit, 2 for all output
!
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
      include 'mpif.h'
!
      implicit none
!
      character*12     routine_name
      integer          routine_call
      double precision routine_time
      common /comrmpi_time/ index_all,index_dummy,                       &            
     &                      routine_name(100),                           &
     &                      routine_call(100),                           &
     &                      routine_time(100)
      character*(*) name
      integer       index,io
      double precision time

      if( index.le.0 ) then
        if( index.eq.-1 ) index_all=0
        index_all = index_all+1
        index         = index_all
        routine_name(index) = name
        routine_call(index) = 0
        routine_time(index) = 0.0
      endif
      if( io.eq.0 ) then
        time = mpi_wtime()
        return
      else
        routine_call(index) = routine_call(index) + 1
        routine_time(index) = routine_time(index) +                      &                      
      &                            mpi_wtime() - time
        if( io.eq.2 ) then
          time=0.0
          do i=1,index_all
            write(*,*) routine_name(i),' is called ',routine_call(i),    &   
     &            ' times with total time of ',routine_time(i),' sec.'
            time = time + routine_time(i)
          enddo
          write(*,*) ' Total MPI routine cost ',time,' sec.'
        endif
      endif
!
#endif
      return
      end
