!ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rimpadj
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rimpadj
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional semi-implicit adjustment after rloopb
!
! program history log:
!
! usage:    call rimpadj
!   input argument list:
!
!   output argument list:
!
!   common block:
! rscomf  /repro/
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! rgetcd  uv2dz  dz2uv
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use physcons, rd => con_rd
!ccj
#include <paramodel.h>
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#include <rscomfi.h>
#include <rscomspc.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscompln.h>
#include <rscomrad.h>
#include <rscomloc.h>
!cc
!.....
!.....implicit adjustment for model physics
!     di te z are changes of model physics for dx dy dz
!.....
!     dimension
      common /reprob/                                                    &  
     &     vln(LNWAVS,levr),                                             &
     &      ze(LNWAVS,levr)
!
      save tor,dm,dt
!    
      dimension tor(levr),dm(LNWAVS,levr,levr)
!
      data dt/-1.0e0/
!---------------------------------------------------
      if( dt.ne.deltim ) then
         dt = deltim
         print *,' ---- initiate rimpadj with new dt = ',dt
         call rgetcd(cm,dm,snnp1,dt)
         do 1 j=1,levr
         tor(j)=rd*tov(j)
  1      continue
      endif
!....< translate u v to di ze, then do implicit scheme to changes >
      call uv2dz(LNWAVS,uu,vv,di,ze,epsx,epsy,levr)
!------------------- do adjustment of semi-implicit ------
#ifdef ORIGIN_THREAD
!$doacross share(vln),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(vln)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(j,i,k)
#endif
      do 21 j=1,levr
      do 21 i=1,LNWAVS
      vln(i,j) = 0.e0
21    continue
      do 22 k=1,levr
      do 22 j=1,levr
      do 22 i=1,LNWAVS
      vln(i,j) = vln(i,j) + am(j,k) * te(i,k)
22    continue
!
#ifdef ORIGIN_THREAD
!$doacross share(vln,tor,z,snnp1,di,dt),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(vln,tor,z,snnp1,di,dt)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,j)
#endif
      do 24 j=1,levr
      do 24 i=1,LNWAVS
      vln(i,j) = vln(i,j) + tor(j) * z(i)
      vln(i,j) = vln(i,j) * snnp1(i) * dt
      vln(i,j) = vln(i,j) + di(i,j)
24    continue
!-----------------------  new dx by ivert matrix ----
!-------------------------------- div ------
#ifdef ORIGIN_THREAD
!$doacross share(di),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(di)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(k,j,i)
#endif
      do 300 k=1,levr
      do 300 i=1,LNWAVS
      di(i,k) = 0.0e0
300   continue
      do 350 k=1,levr
      do 350 j=1,levr
      do 350 i=1,LNWAVS
      di(i,j) = di(i,j) + dm(i,j,k) * vln(i,k)
350   continue
!-------------------------------- temp ------
#ifdef ORIGIN_THREAD
!$doacross share(vln),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(vln)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(j,i)
#endif
      do 510 j=1,levr
      do 510 i=1,LNWAVS
      vln(i,j) = 0.e0
510   continue
      do 520 k=1,levr
      do 520 j=1,levr
      do 520 i=1,LNWAVS
      vln(i,j) = vln(i,j) + bm(j,k) * di(i,k)
520   continue
#ifdef ORIGIN_THREAD
!$doacross share(vln,te,dt),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(vln,te,dt)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,j)
#endif
      do 530 j=1,levr
      do 530 i=1,LNWAVS
      te(i,j)= te(i,j) + vln(i,j) * dt
530   continue
!------------------------------ ln(ps) -----
      do 400 i=1,LNWAVS
      vln(i,1) = 0.e0
400   continue
      do 450 k=1,levr
      do 450 i=1,LNWAVS
      vln(i,1) = vln(i,1) + sv(k) * di(i,k)
450   continue
      do 470 i=1,LNWAVS
      z(i) = z(i) + vln(i,1) * dt
470   continue
      call dz2uv(LNWAVS,di,ze,uu,vv,epsx,epsy,rnnp1,levr)
! =================================================
      return
      end
