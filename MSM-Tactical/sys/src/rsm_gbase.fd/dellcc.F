      subroutine dellcc(lnwav,qt,qtx,qty,epsx,epsy,km)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  dellcc
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  first derivative of coefficent for cos-cos wave.
!            for regional spectral model.
!
! program history log:
!
! usage:    call dellcc(qt,qtx,qty,epsx,epsy)
!   input argument list:
!     qt  - wave coefficent with dimension of LNWAVS
!     epsx  - coefficient for x derivative.
!     epsy  - coefficient for y derivative.
!
!   output argument list:
!     qtx     - x derivative of wave coefficient
!     qty     - y derivative of wave coefficient
!
!   input files: none
!
!   output files: none
!
!   subprograms called: none
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
!$$$
#include <define.h>
      dimension epsx(lnwav), epsy(lnwav)
      dimension qt  (lnwav,km),qtx(lnwav,km),qty(lnwav,km)
#ifdef ORIGIN_THREAD
!$doacross share(qt,qtx,qty,epsx,epsy),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(qt,qtx,qty,epsx,epsy)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j,k)
#endif
      do k=1,km
        do j=1,lnwav
          qtx(j,k)= -qt(j,k)*epsx(j)
          qty(j,k)= -qt(j,k)*epsy(j)
        enddo
      enddo
      return
      end
