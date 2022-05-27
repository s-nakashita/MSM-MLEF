!-----------------------------------------------------------------------
      subroutine gncpus(ncpusi)
!$$$  subprogram documentation block
!
! subprogram: gncpus         gets environment number of cpus
!   prgmmr: iredell          org: w/nmc23     date: 94-08-19
!
! abstract: gets and returns the environment variable ncpusi,
!   designating the number of processors over which to parallelize.
!
! program history log:
!   94-08-19  iredell
!
! usage:    call gncpus(ncpusi)
!   output arguments:
!     ncpusi        integer number of cpus
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
           ncpusi=1
           iret=getenv('ncpus',cncpus)
           if(iret.eq.1) then
             read(cncpus,'(bn,i8)',iostat=ios) ncpusi
             ncpusi=max(ncpusi,1)
           endif
#else
           ncpusi=ncpu
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
