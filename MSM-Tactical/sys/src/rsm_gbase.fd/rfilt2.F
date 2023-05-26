      subroutine rfilt2(filtb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rfilt2
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional time filter of the second pass
!
! program history log:
!
! usage:    call rfilt2(tem,te,y,usm,us,x,vsm,vs,w,rm,rq,rt,filtb)
!   input argument list:
!     tem       - wave perturbation for virtual temperature at time n-1
!     te        - wave perturbation for virtual temperature at time n
!     y         - wave perturbation for virtual temperature at time n+1
!     usm       - wave perturbation for x-direction wind at time n-1
!     us        - wave perturbation for x-direction wind at time n
!     x         - wave perturbation for x-direction wind at time n+1
!     vsm       - wave perturbation for y-direction wind at time n-1
!     vs        - wave perturbation for y-direction wind at time n
!     w         - wave perturbation for y-direction wind at time n+1
!      rm       - wave perturbation for specific humidity at time n-1
!      rq       - wave perturbation for specific humidity at time n
!      rt       - wave perturbation for specific humidity at time n+1
!     filtb     - filter coefficient for time n-1
!
!   output argument list:
!     tem       - filtered wave perturbation for virtual temperature 
!                 at time n and stored at time n-1 for next time step
!     te        - update wave perturbation for virtual temperature 
!                 at time n+1 and stored at time n for next time step
!     usm       - filtered wave perturbation for x-direction wind 
!                 at time n and stored at time n-1 for next time step
!     us        - filtered wave perturbation for x-direction wind 
!                 at time n+1 and stored at time n for next time step
!     vsm       - filtered wave perturbation for y-direction wind
!                 at time n and stored at time n-1 for next time step
!     vs        - filtered wave perturbation for y-direction wind
!                 at time n+1 and stored at time n for next time step
!      rm       - filtered wave perturbation for specific humidity
!                 at time n and stored at time n-1 for next step
!      rq       - filtered wave perturbation for specific humidity
!                 at time n+1 and stored at time n for next step
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
!.......................................
#ifdef ORIGIN_THREAD
!$doacross share(tem,uum,vvm,te,uu,vv,filtb),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(tem,uum,vvm)
!mic$1 shared(te ,uu ,vv)
!mic$1 shared(filtb)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(k,j)
#endif

      do k=1,levr
        do j=1,LNWAVS
          tem(j,k)=tem(j,k)+filtb*te(j,k)
          uum(j,k)=uum(j,k)+filtb*uu(j,k)
          vvm(j,k)=vvm(j,k)+filtb*vv(j,k)
#ifdef NONHYD
          pnm(j,k)=pnm(j,k)+filtb*pn(j,k)
          tnm(j,k)=tnm(j,k)+filtb*tn(j,k)
          onm(j,k)=onm(j,k)+filtb*on(j,k)
#endif
        enddo
      enddo
#ifdef NONHYD
      k=levr+1
      do j=1,LNWAVS
      onm(j,k)=onm(j,k)+filtb*on(j,k)
      enddo
#endif
#ifdef ORIGIN_THREAD
!$doacross share(rm,rq,filtb),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rm)
!mic$1 shared(rq)
!mic$1 shared(filtb)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(nc,j,k)
#endif
      do k=1,levr
      do nc=1,ntotal
        do j=1,LNWAVS
          rm(j,k,nc)= rm(j,k,nc)+filtb*rq(j,k,nc)
        enddo
      enddo
      enddo
!..........
      return
      end
