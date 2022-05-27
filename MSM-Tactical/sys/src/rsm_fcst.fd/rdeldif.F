      subroutine rdeldif(sl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rdeldif
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  horizontal diffusion of regional perturbation waves
!            for regional spectral model.
!
! program history log:
!
! usage:    call rdeldif
!   input argument list:
!
!   output argument list:
!
!   common block :
!       rscomf
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
!.....
!ccj
#include <paramodel.h>
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#include <rscomspc.h>
#include <rscomio.h>
!cc
      dimension xkm(LNWAVS),xkh(LNWAVS)
      dimension rkm(LNWAVS),rkh(LNWAVS)
      dimension rtrd(levr),sl(levr)
!x    dimension tt(LNWAVS),ct(levr),cq(levr)
      save ddt,xkm,xkh,rkm,rkh,rtrd
!.................................................................
!
      data ddt/0/
      if(ddt.ne.deltim) then
         ddt = deltim
!
         coefmax = (epsxmax**2+epsymax**2)**2
         do 20 i=1,LNWAVS
         coef=(epsx(i)**2+epsy(i)**2)**2
         coef = coef/coefmax
         xkh(i) = coef / difhsec
         xkm(i) = coef / difmsec
         rkm(i) = 1.0 / ( 1.0 + 2.0 * ddt * xkm(i) )
         rkh(i) = 1.0 / ( 1.0 + 2.0 * ddt * xkh(i) )
 20      continue
!
#ifdef CHGTLEV
!  sigma level at which to begin rayleigh damping
        slrd0=0.012
!  reciprocal of time scale per scale height
!  above beginning sigma level for rayleigh damping
!       rtrd1=1./(5.*86400.)
        rtrd1=1./(1.*3600.)
!
        do k=1,levr
          if(sl(k).lt.slrd0) then
            rtrd(k)=rtrd1*log(slrd0/sl(k))
          else
            rtrd(k)=0
          endif
        enddo
#endif
!
!x       do 40 k=1,levr
!x       ku = min0 ( k+1, levr )
!x       kd = max0 ( k-1, 1     )
!x       sldsl = sl(k) / (sl(ku) - sl(kd)) / 4.0
!x       ct(k) = ( te(1,ku) - te(1,kd) ) * sldsl
!x       cq(k) = ( rq(1,ku) - rq(1,kd) ) * sldsl
!40      continue
!
      endif
!
! time splitting implicit diffusion
!
#ifdef ORIGIN_THREAD
!$doacross share(x,w,y,rkm,rkh),local(i,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(x,w,y)
!mic$1 shared(rkm,rkh)
!mic$1 private(i,k)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k,rkmik)
#endif
        do 1100 k=1,levr
        do 1100 i=1,LNWAVS
#ifdef CHGTLEV
        rkmik = 1.0 / ( 1.0 + 2.0 * ddt *xkm(i) + ddt*rtrd(k) )
        x (i,k) = x (i,k) * rkmik
        w (i,k) = w (i,k) * rkmik
#else
        x (i,k) = x (i,k) * rkm(i)
        w (i,k) = w (i,k) * rkm(i)
#endif
        y (i,k) = y (i,k) * rkh(i)
#ifdef NONHYD
        p (i,k) = p (i,k) * rkh(i)
        t (i,k) = t (i,k) * rkh(i)
        o (i,k) = o (i,k) * rkm(i)
#endif
1100    continue
#ifdef NONHYD
        k=levr+1
        do i=1,LNWAVS
        o (i,k) = o (i,k) * rkm(i)
        enddo
#endif
!
#ifdef ORIGIN_THREAD
!$doacross share(rt,rkh),local(i,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rt)
!mic$1 shared(rkh)
!mic$1 private(i,k)
#endif
#ifdef OPENMP
!$omp parallel do private(nc,i,k)
#endif
        do 1200 nc=1,ntotal
        do 1200 k=1,levr
        do 1200 i=1,LNWAVS
        rt(i,k,nc) = rt(i,k,nc) * rkh(i)
1200    continue
!
      return
      end
