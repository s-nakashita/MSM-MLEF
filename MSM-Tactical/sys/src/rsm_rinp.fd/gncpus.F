!-----------------------------------------------------------------------
      subroutine gncpus(ncpus)
!$$$  subprogram documentation block
!
! subprogram: gncpus         gets environment number of cpus
!   prgmmr: iredell          org: w/nmc23     date: 94-08-19
!
! abstract: gets and returns the environment variable ncpus,
!   designating the number of processors over which to parallelize.
!
! program history log:
!   94-08-19  iredell
!
! usage:    call gncpus(ncpus)
!   output arguments:
!     ncpus        integer number of cpus
!
! subprograms called:
!   getenv       get environment variable
!
! attributes:
!   language: cray fortran
!
!$$$
#include <define.h>
#ifdef CRA
      integer getenv
      character*8 cncpus
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!ccj
#include <ncpu.h>
!cc
#ifdef CRA
           ncpus=1
           iret=getenv('ncpus',cncpus)
           if(iret.eq.1) then
             read(cncpus,'(bn,i8)',iostat=ios) ncpus
             ncpus=max(ncpus,1)
           endif
#else
           ncpus=ncpu
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
