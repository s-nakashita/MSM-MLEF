      subroutine rsicdif
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsicdif
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional semi-implicit integration.
!
! program history log:
!
! usage:    call rsicdif
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
!.....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....use u v as input, update u v tendency by div vor tendency.....
!.....
      use physcons, rd => con_rd
!ccj
#include <paramodel.h>
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#include <rscomspc.h>
#include <rscomver.h>
#include <rscomio.h>
!cc
      save tor,dm,dt
      dimension tor(levr),dm(LNWAVS,levr,levr)
      common /reprosi/                                                   & 
     &  eln(LNWAVS,levr),qdtz(LNWAVS)                                    &
     &, uln(LNWAVS,levr),vln(LNWAVS,levr)                                &
     &, dim(LNWAVS,levr)
!
      data dt/-1.0e0/
!---------------------------------------------------
      if( dt.ne.deltim ) then
         dt = deltim
!        print *,' ---- initiate sicdif with new dt = ',dt
         call rgetcd(cm,dm,snnp1,dt)
         do 1 j=1,levr
         tor(j)=rd*tov(j)
  1      continue
      endif
!----------------- transform u v to di ze before semi-implicit ---
      call uv2dz(LNWAVS,  x,  w,uln,vln,epsx,epsy,levr)
      call uv2dz(LNWAVS, uu, vv, di,eln,epsx,epsy,levr)
      call uv2dz(LNWAVS,uum,vvm,dim,eln,epsx,epsy,levr)
#ifdef ORIGIN_THREAD
!$doacross share(uln,vln,x,w),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uln,vln,x,w)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 20 k=1,levr
      do 20 i=1,LNWAVS
      x (i,k) = uln (i,k)
      w (i,k) = vln (i,k)
20    continue
!-------------------------------------------------------
      do 100 i=1,LNWAVS
      qdtz(i) = qm(i) - q(i) + dt * z(i)
100   continue
#ifdef ORIGIN_THREAD
!$doacross share(eln,tem,te,dt,y),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(eln,tem,te,dt,y)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(k,i)
#endif
      do 150 k=1,levr
      do 150 i=1,LNWAVS
      eln(i,k)=  tem(i,k) - te(i,k) + dt * y(i,k)
150   continue
!---------------------------------------------------
#ifdef ORIGIN_THREAD
!$doacross share(uln),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uln)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,j,k)
#endif
      do 200 k=1,levr
      do 200 i=1,LNWAVS
      uln(i,k) = 0.e0
200   continue
      do 220 k=1,levr
      do 220 j=1,levr
      do 220 i=1,LNWAVS
      uln(i,j) = uln(i,j) + am(j,k) * eln(i,k)
220   continue
#ifdef ORIGIN_THREAD
!$doacross share(uln,vln,dim,tor,qdtz,snnp1,x,di,dt),
!$&        local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uln,vln,dim,tor,qdtz,snnp1,x,di,dt)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 240 k=1,levr
      do 240 i=1,LNWAVS
      vln(i,k) = uln(i,k) + tor(k) * qdtz(i)
      vln(i,k) = vln(i,k) * snnp1(i) + x(i,k)
      vln(i,k) = dim(i,k) - di(i,k) + dt * vln(i,k)
240   continue
!----------------------- vln = d bar bar = ((d+)+(d-))/2 ----
#ifdef ORIGIN_THREAD
!$doacross share(uln),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uln)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 300 k=1,levr
      do 300 i=1,LNWAVS
      uln(i,k) = 0.0e0
300   continue
      do 350 k=1,levr
      do 350 j=1,levr
      do 350 i=1,LNWAVS
      uln(i,j) = uln(i,j) + dm(i,j,k) * vln(i,k)
350   continue
!------------------------------ ln(ps) -----
      do 400 i=1,LNWAVS
      vln(i,1) = 0.e0
400   continue
      do 450 k=1,levr
      do 450 i=1,LNWAVS
      vln(i,1) = vln(i,1) + sv(k) * uln(i,k)
450   continue
      do 470 i=1,LNWAVS
      z(i) = z(i) + vln(i,1)
470   continue
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
!$omp parallel do private(i,k)
#endif
      do 500 k=1,levr
      do 500 i=1,LNWAVS
      vln(i,k) = 0.e0
500   continue
      do 520 k=1,levr
      do 520 j=1,levr
      do 520 i=1,LNWAVS
      vln(i,j) = vln(i,j) + bm(j,k) * uln(i,k)
520   continue
#ifdef ORIGIN_THREAD
!$doacross share(uln,vln,dim,di,x,y,dt),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uln,vln,dim,di,x,y,dt)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 530 k=1,levr
      do 530 i=1,LNWAVS
      y(i,k)= y(i,k) + vln(i,k)
!-------------------------------- div ------
      x(i,k)= (uln(i,k) - dim(i,k) + di(i,k) ) / dt
530   continue
!-------------------change dz forcing to uv---------
      call dz2uv(LNWAVS,x,w,uln,vln,epsx,epsy,rnnp1,levr)
#ifdef ORIGIN_THREAD
!$doacross share(uln,vln,x,w),local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uln,vln,x,w)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 870 k=1,levr
      do 870 i=1,LNWAVS
      x(i,k) = uln(i,k)
      w(i,k) = vln(i,k)
 870  continue
! =================================================
      return
      end
