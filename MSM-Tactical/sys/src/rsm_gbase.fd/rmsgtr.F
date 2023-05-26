      subroutine rmsgtr(q,x,y,w,del,r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rmsgtr
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  compute horizontal mean for regional perturbation waves.
!
! program history log:
!
! usage:    call rmsgtr(q,x,y,w,del,r)
!   input argument list:
!     q   - wave coefficent of perturbation log surface pressure
!     x   - wave coefficent of perturbation x-direction wind
!     y   - wave coefficent of perturbation virtual temperature
!     w   - wave coefficent of perturbation y-direction wind
!     del   - sigma layer thickness
!     r   - wave coefficent of perturbation specific humidity
!
!   output argument list:
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! rbartr
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      real,allocatable::qf(:),xf(:,:),yf(:,:),wf(:,:),rf(:,:)
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
!cc
      dimension q(LNWAVS),x(LNWAVS,levr),y(LNWAVS,levr)
      dimension w(LNWAVS,levr),r(LNWAVS,levr)
      dimension rx(levr),ry(levr),rw(levr),del(levr),rr(levr)
!
#ifdef MP
      allocate(qf(lnwav))
      allocate(xf(lnwav,levr))
      allocate(yf(lnwav,levr))
      allocate(wf(lnwav,levr))
      allocate(rf(lnwav,levr))
!
      call rmpsp2f(q,lnwavp,qf,lnwav,1)
      call rmpsp2f(x,lnwavp,xf,lnwav,levr)
      call rmpsp2f(y,lnwavp,yf,lnwav,levr)
      call rmpsp2f(w,lnwavp,wf,lnwav,levr)
      call rmpsp2f(r,lnwavp,rf,lnwav,levr)
!
      if( mype.eq.master ) then 
!        call maxmin(qf,lnwav,1,1,1,'in rmsgtr,q')
!        call maxmin(xf,lnwav,levr,1,levr,'in rmsgtr,xf')
!        call maxmin(yf,lnwav,levr,1,levr,'in rmsgtr,yf')
!        call maxmin(wf,lnwav,levr,1,levr,'in rmsgtr,wf')
!        call maxmin(rf,lnwav,levr,1,levr,'in rmsgtr,rf')

#define QS qf
#define XS xf
#define YS yf
#define WS wf
#define RS rf
#else
#define QS q
#define XS x
#define YS y
#define WS w
#define RS r
#endif
      vr=0.e0
      vx=0.e0
      vy=0.e0
      vw=0.e0
      do 10 k=1,levr
      call rbartr(lnwav,XS(1,k),XS(1,k),rx(k))
      call rbartr(lnwav,YS(1,k),YS(1,k),ry(k))
      call rbartr(lnwav,WS(1,k),WS(1,k),rw(k))
      vx=vx+rx(k)*del(k)
      vy=vy+ry(k)*del(k)
      vw=vw+rw(k)*del(k)
10    continue
      do 20 k=1,levr
      call rbartr(lnwav,RS(1,k),RS(1,k),rr(k))
      vr=vr+rr(k)*del(k)
20    continue
      call rbartr(lnwav,QS,QS,rq)
!     print 50
!50   format(1h ,'begin rmsgt')
      print 100,vx,vw,vy,vr,rq
100   format(1h0,' u  v  temp  mixratio ln(ps)'/5(1x,e10.3),' total ')
 200  format(1h ,4(1x,e10.3),' layer=',i3)
      do 40 k=1,levr
      if(k.gt.levr)go to 35
      print 200,rx(k),rw(k),ry(k),rr(k),k
      go to 39
35    continue
      print 200,rx(k),rw(k),ry(k)
39    continue
40    continue
!     print 300
!300  format(1h ,'end rmsgt')
#undef QS
#undef XS
#undef YS
#undef WS
#undef RS
#ifdef MP
      endif
      deallocate(qf)
      deallocate(xf)
      deallocate(yf)
      deallocate(wf)
      deallocate(rf)
#endif
      return
      end
