      subroutine rgetcd(cm,dm,snnp1,dt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rgetcd
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare constant matix for semi-implicit computation
!
! program history log:
!
! usage:    call rgetcd(cm,dm,snnp1,dt)
!   input argument list:
!     cm  - matrix for preparing dm for semi-implicit

!     snnp1 - laplacian operator for regional spectral coefficient
!     dt  - time step

!
!   output argument list:
!     dm  - matrix for semi-implicit

!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!   matinv

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
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
      dimension cm(levr,levr)
      dimension snnp1(LNWAVS),dm(LNWAVS,levr,levr)
      dimension rim(levr,levr),sn1dt2(LNWAVS),                           & 
     & ddd(LNWAVS),ppp(LNWAVS),rrr(LNWAVS)
!.................................................................
#ifdef ORIGIN_THREAD
!$doacross share(rim),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rim)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(k,j)
#endif
      do 10 k=1,levr
      do 10 j=1,levr
      rim(j,k)=0.e0
 10   continue
      do 20 k=1,levr
      rim(k,k) = 1.e0
 20   continue
      do 2000 nn=1,LNWAVS
      sn1dt2(nn) = snnp1(nn)*dt*dt
2000  continue
!
#ifdef ORIGIN_THREAD
!$doacross share(dm,nn,rim,sn1dt2,cm),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(dm,nn,rim,sn1dt2,cm)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j,i,nn)
#endif
      do 50 j=1,levr
      do 50 i=1,levr
      do 50 nn=1,LNWAVS
      dm(nn,i,j) = rim(i,j) - sn1dt2(nn)*cm(i,j)
 50   continue
      call matinv(dm,LNWAVS,levr,ddd,ppp,rrr)
!..
!     print 100,dt
100   format(1h ,'completed getcd dt=',f7.1)
      return
      end
