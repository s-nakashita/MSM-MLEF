      subroutine uv2dz(lnwav,uu,vv,di,ze,epsx,epsy,levr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  uv2dz
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  transform coefficients of wind in x and y directions to
!           divergence and vorticity.
!
! program history log:
!
! usage:    call uv2dz(uu,vv,di,ze,epsx,epsy)
!   input argument list:
!     uu  - wave coefficent of x-wind with dimension of lnwav
!     vv  - wave coefficent of y-wind with dimension of lnwav
!     epsx  - coefficient for x derivative.
!     epsy  - coefficient for y derivative.
!
!   output argument list:
!     di  - wave coefficent of divergence with dimension of lnwav
!     ze  - wave coefficent of vorticity  with dimension of lnwav
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
      dimension epsx(lnwav),epsy(lnwav)
      dimension di(lnwav,levr),ze(lnwav,levr)                           &
     &         ,uu(lnwav,levr),vv(lnwav,levr)
!
#ifdef ORIGIN_THREAD
!$doacross share(uu,vv,di,ze,epsx,epsy),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uu,vv,di,ze,epsx,epsy)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j)
#endif
      do k=1,levr
        do j=1,lnwav
          di(j,k) =   epsx(j)*uu(j,k) + epsy(j)*vv(j,k)
          ze(j,k) =   epsy(j)*uu(j,k) - epsx(j)*vv(j,k)
        enddo
      enddo
      return
      end
