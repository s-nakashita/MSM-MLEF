      subroutine rfilt1(filta,filtb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rfilt1
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional time filter of the first pass
!
! program history log:
!
! usage:    call rfilt1(tem,te,usm,us,vsm,vs,rm,rq,filta,filtb)
!   input argument list:
!     tem       - wave perturbation for virtual temperature at time n-1
!     te        - wave perturbation for virtual temperature at time n
!     usm       - wave perturbation for x-direction wind at time n-1
!     us        - wave perturbation for x-direction wind at time n
!     vsm       - wave perturbation for y-direction wind at time n-1
!     vs        - wave perturbation for y-direction wind at time n
!      rm       - wave perturbation for specific humidity at time n-1
!      rq       - wave perturbation for specific humidity at time n
!     filta     - filter coefficient for time n
!     filtb     - filter coefficient for time n-1
!
!   output argument list:
!     tem       - filtered wave perturbation for virtual temperature 
!                 at time n and stored at time n-1 for next time step
!     usm       - filtered wave perturbation for x-direction wind 
!                 at time n and stored at time n-1 for next time step
!     vsm       - filtered wave perturbation for y-direction wind
!                 at time n and stored at time n-1 for next time step
!      rq       - filtered wave perturbation for specific humidity
!                 at time n and stored at time n for rloopb
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
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#include <rscomspc.h>
!cc
!.....
#ifdef ORIGIN_THREAD
!$doacross share(tem,uum,vvm,te ,uu ,vv ,filta,filtb),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(tem,uum,vvm)
!mic$1 shared(te ,uu ,vv)
!mic$1 shared(filta,filtb)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j,k)
#endif
      do k=1,levr
        do j=1,LNWAVS
          tem(j,k)=filtb*tem(j,k)+filta*te(j,k)
          uum(j,k)=filtb*uum(j,k)+filta*uu(j,k)
          vvm(j,k)=filtb*vvm(j,k)+filta*vv(j,k)
#ifdef NONHYD
          pnm(j,k)=filtb*pnm(j,k)+filta*pn(j,k)
          tnm(j,k)=filtb*tnm(j,k)+filta*tn(j,k)
          onm(j,k)=filtb*onm(j,k)+filta*on(j,k)
#endif
        enddo
      enddo
#ifdef NONHYD
      k=levr+1
      do j=1,LNWAVS
      onm(j,k)=filtb*onm(j,k)+filta*on(j,k)
      enddo
#endif
#ifdef ORIGIN_THREAD
!$doacross share(rm,rq,filta,filtb),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rm)
!mic$1 shared(rq)
!mic$1 shared(filta,filtb)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(nc,j,k)
#endif
      do k=1,levr
      do nc=1,ntotal
        do j=1,LNWAVS
          rm(j,k,nc)=filtb* rm(j,k,nc)+filta*rq(j,k,nc)
        enddo
      enddo
      enddo
      return
      end
