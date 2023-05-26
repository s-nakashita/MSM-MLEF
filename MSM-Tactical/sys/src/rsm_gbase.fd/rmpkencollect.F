      subroutine rmpkencollect(itnum,nvrken,nptken,nstken,svdata)
!
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
!
      integer itnum,nvrken,nptken,nstken,ier,ierr
      dimension svdata(nvrken,nptken,nstken)
      real, allocatable  ::  sv2(:,:,:)
!
!j      call mpi_comm_rank(mpi_comm_world,nrank,ier)
!
      allocate (sv2(nvrken,nptken,itnum))
!initial
      sv2=0.
!
!j      call mpi_barrier(mpi_comm_world,ier)
!
!j    write(0,*) ' about to call mpi_reduce in kencollect'
!
      call mpi_reduce(svdata, sv2, nvrken*nptken*itnum,                  &
     &  mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
!
!j      call mpi_barrier(mpi_comm_world,ier)
!
!j    write(0,*) ' called  mpi_reduce in kencollect'
!
      svdata(:,:,1:itnum)=sv2(:,:,1:itnum)
      deallocate (sv2)
!
#endif
      return
      end
