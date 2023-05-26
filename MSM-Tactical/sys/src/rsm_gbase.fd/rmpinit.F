      subroutine rmpinit(ncoli,nrowi,strwtime)
!$$$  subprogram documentation block
!
! subprogram: 	rmpinit
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: initializing the mpi by each pe.
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpinit
!
! subprograms called:
!   mpi_init		- to initial mpi call
!   mpi_comm_size	- to get total number of pe for the comm
!   mpi_comm_rank	- to get my own pe number 
!   mpi_comm_split	- to split entire comm into sub comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      call mpi_init(info)
      strwtime=mpi_wtime()
      call mpi_comm_size(mpi_comm_world,npes,ierr)
      call mpi_comm_rank(mpi_comm_world,mype,jerr)
      if( ierr.ne.0 .or. jerr.ne.0 .or.                                  &  
     &    info.ne.0 .or. npes.ne.npesi ) then
        print *,'PE',mype,':********* Error stop in rmpinit ********* '
        print *,'PE',mype,':error code from mpi_init = ',info
        print *,'PE',mype,':error code from mpi_comm_size = ',ierr
        print *,'PE',mype,':error code from mpi_comm_rank = ',jerr
        print *,'PE',mype,':npes preset in code = ',npesi
        print *,'PE',mype,':npes request from environment = ',npes
        print *,'PE',mype,':******* End of output for rmpinit ******* '
        call rmpabort
      else
        master=0
        msgtag=0
        ncol=ncoli
        nrow=nrowi
        myrow=mype/ncol
        mycol=mod(mype,ncol)
        call mpi_comm_split(mpi_comm_world,myrow,mycol,comm_row,ierr)
        call mpi_comm_split(mpi_comm_world,mycol,myrow,comm_column,jerr)
        if( ierr.ne.0 .or. jerr.ne.0 ) then
          print *,'PE',mype,':error code for doing comm_row = ',ierr
          print *,'PE',mype,':error code for doing comm_column = ',jerr
          call rmpabort
        else
          print *,'PE',mype,' npes ncol nrow mycol myrow',                 &
     &                        npes,ncol,nrow,mycol,myrow
        endif
      endif
#endif
      return
      end

!     function mpi_wtime( )
!     real*8 mpi_wtime
!     mpi_wtime = timef()
!     return
!     end
