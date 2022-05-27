      subroutine dz2uv(lnwav,di,ze,uu,vv,epsx,epsy,rnnp1,levr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  dz2uv
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  transform coefficients of divergence and vorticity to
!            wind in x and y directions.
!            for regional spectral model.
!
! program history log:
!
! usage:    call dz2uv(di,ze,uu,vv,epsx,epsy,rnnp1)
!   input argument list:
!     di        - wave coefficent of divergence with dimension of lnwav
!     ze        - wave coefficent of vorticity  with dimension of lnwav
!     epsx      - coefficient for x derivative.
!     epsy      - coefficient for y derivative.
!     rnnp1     - coefficient for inverse of laplacian.
!
!   output argument list:
!     uu        - wave coefficent of x-wind with dimension of lnwav
!     vv        - wave coefficent of y-wind with dimension of lnwav
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
!   machine:  cray ymp.
!
!$$$
#include <define.h>
      dimension epsx(lnwav),epsy(lnwav),rnnp1(lnwav)
      dimension di(lnwav,levr),ze(lnwav,levr)                           &
     &         ,uu(lnwav,levr),vv(lnwav,levr)
!
#ifdef ORIGIN_THREAD
!$doacross share(uu,vv,di,ze,epsx,epsy,rnnp1),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uu,vv,di,ze,epsx,epsy,rnnp1)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j,k)
#endif
      do k=1,levr
        do j=1,lnwav
          uu(j,k) = - epsx(j)*di(j,k) - epsy(j)*ze(j,k)
          vv(j,k) = - epsy(j)*di(j,k) + epsx(j)*ze(j,k)
          uu(j,k) = - uu(j,k)*rnnp1(j)
          vv(j,k) = - vv(j,k)*rnnp1(j)
        enddo
      enddo
      return
      end
