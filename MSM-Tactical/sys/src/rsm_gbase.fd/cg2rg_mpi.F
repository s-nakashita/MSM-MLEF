      subroutine cg2rg_mpi(a,b,km)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gg2rg
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do gaussian gloal grid to regional grid interpolation.
!
! program history log:
!
! usage:    call gg2rg(a,b,km)
!   input argument list:
!     a   - gaussian grid from global field.
!     km  - the second dimension of a and b
!
!   output argument list:
!     b   - regional grid.
!
!   common block:
! comg2r
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
!ccj
#include <paramodel.h>
#ifdef MP
#define LNGRDS lngrdp
#else
#define LNGRDS lngrd
#endif
!cc
!
      dimension a(clngrd,km),b(LNGRDS,km)
!
      common/comc2r/                                                    &
     &          n00(LNGRDS),n10(LNGRDS)                                   &
     &         ,n11(LNGRDS),n01(LNGRDS)                                   &
     &         ,d00(LNGRDS),d10(LNGRDS)                                   &
     &         ,d11(LNGRDS),d01(LNGRDS)
      dimension                                                         &
     &          a00(LNGRDS),a10(LNGRDS)                                   &
     &         ,a11(LNGRDS),a01(LNGRDS)
!
#ifdef ORIGIN_THREAD
!$doacross share(a,b,n00,n01,n10,n11,d00,d01,d10,d11,km),
!$&        local(a00,a01,a10,a11,n,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(a,b)
!mic$1 shared(n00,n01,n10,n11)
!mic$1 shared(d00,d01,d10,d11)
!mic$1 shared(km)
!mic$1 private(a00,a01,a10,a11,n,k)
#endif
#ifdef OPENMP
!$omp parallel do private(a00,a01,a10,a11,n,k)
#endif
      do 5000 k=1,km
      do 2000 n=1,LNGRDS
      a00(n) = a(n00(n),k)
      a10(n) = a(n10(n),k)
      a11(n) = a(n11(n),k)
      a01(n) = a(n01(n),k)
2000  continue
      do 3000 n=1,LNGRDS
      b(n,k) =    a00(n) * d00(n)                                       &
     &          + a10(n) * d10(n)                                       &
     &          + a11(n) * d11(n)                                       &
     &          + a01(n) * d01(n)
3000  continue
!
5000  continue
!
      return
      end
