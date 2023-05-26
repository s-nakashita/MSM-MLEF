      subroutine rmpn2nn(a,llwavp,b,lnwavp,nvar)
!$$$  subprogram documentation block
!
! subprogram: 	rmpn2nn
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (llwavp,nvar) to (lnwavp,nvar)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpnk2nn(a,llwavp,b,lnwavp,nvar)
!
!    input argument lists:
!	a	- real (llwavp,nvar) partial field 
!	llwavp	- integer partial spectral grid
!	lnwavp	- integer sub partial spectral grid
!	nvar	- integer total set of fields
!
!    output argument list:
!	b	- real (llwavp,levrp,nvar) partial field 
! 
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
      implicit none
#include <npesi.h>
#include <comrmpi.h>
      integer lnwavp,llwavp,nvar,status,lntstr0                          &  
     &       ,ii,ipe,ipe0,n,k,m,mn,len,ierr
      real    a,b
      dimension a(llwavp,nvar),b(lnwavp,nvar)
!
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do n=1,nvar
            do m=1,llwavp
              b(m,n) = a(m,n)
            enddo
        enddo
        return
      endif
!  
      ipe0=int(mype/ncol)*ncol
      lntstr0=lntstr(mype)-lnpstr(ipe0)
! cut in n
       do n=1,nvar
         do m=1,lntlen(mype)
           b(m,n)=a(m+lntstr0,n)
         enddo
      enddo
!
#endif
      return
      end
