      subroutine rupdate
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rupdate
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  update prognostic variables from forcing (or tendency).
!
! program history log:
!
! usage:    call rupdate
!   input argument list:
!
!   output argument list:
!
!   common block:
! rscomf
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
#include <npesi.h>
#include <comrmpi.h>
#else
#define LNWAVS lnwav
#endif
#include <rscomspc.h>
#include <rscomio.h>
!
      dimension bc_mask(lnwav),bc_maskp(LNWAVS)
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,'enter rupdate,fhour=',fhour
#ifdef MP
      endif
#endif
!cc
!
!------------------------------ ln (ps) -------------
      do 100 i=1,LNWAVS
      z(i) = qm(i) + dt2 * z(i)
100   continue
#ifdef MP
      if( mype.eq.master ) then
#endif
#ifdef MBC
! mean bias correction
!      print *,'mype=',mype,'lnwavs=',LNWAVS,                       &
!     &    'jwav1=',jwav1
       z(1)=0.
!      remkh=mod(fhour,6.0)
!      if ( remkh .eq. 0) then
!        print *,'mype=',mype,' fhour=',fhour,'rem=',remkh
!        print *,'wave0, k=1,z(0)=',z(1)
!        print *,'wave0i,k=1,z(1)=',z(2)
!        print *,'wave1r,k=1,z(2)=',z(3)
!        print *,'wave1i,k=1,z(3)=',z(1+jwav1)
!        print *,'wave2r,k=1,z(4)=',z(2+jwav1)
!        print *,'wave2i,k=1,z(5)=',z(3+jwav1)
!        print *,'wave3r,k=1,z(6)=',z(1+jwav1*2)
!        print *,'wave3i,k=1,z(7)=',z(2+jwav1*2)
!        print *,'wave4r,k=1,z(8)=',z(3+jwav1*2)
!      endif
#endif
#ifdef MP
      endif
#endif
!
!-----------------------------------------------
#ifdef ORIGIN_THREAD
!$doacross share(tem,uum,vvm,dt2,y,x,w),local(i,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(tem,uum,vvm,dt2)
!mic$1 shared(  y,  x,  w)
!mic$1 private(i,k)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 200 k=1,levr
      do 201 i=1,LNWAVS
!-------------------------------------- temp ------
      y(i,k)=  tem(i,k) + dt2 * y(i,k)
!-------------------------------------- uu ---------
      x(i,k) = uum(i,k) + dt2 * x(i,k)
! ------------------------------------- vv ---------
      w(i,k) = vvm(i,k) + dt2 * w(i,k)
! --------------------------------------------------
#ifdef NONHYD
      p(i,k) = pnm(i,k) + dt2 * p(i,k)
      t(i,k) = tnm(i,k) + dt2 * t(i,k)
#endif
201   continue
#ifdef MP
      if( mype.eq.master ) then
#endif
#ifdef MBC
!      print *,'mype=',mype,'lev=',levr,'lnwavs=',LNWAVS,            &
!     &    'jwav1=',jwav1
        y(1,k)=0.
        x(1,k)=0.
        w(1,k)=0.
!      remkh=mod(fhour,6.0)
!      if ( remkh .eq. 0) then
!        print *,'mype=',mype,' fhour=',fhour,'rem=',remkh
!        print *,'wave0, k=',k,'y(0,k)=',y(1,k)
!        print *,'wave0i,k=',k,'y(1,k)=',y(2,k)
!        print *,'wave1r,k=',k,'y(2,k)=',y(3,k)
!        print *,'wave1i,k=',k,'y(3,k)=',y(1+jwav1,k)
!        print *,'wave2r,k=',k,'y(4,k)=',y(2+jwav1,k)
!        print *,'wave2i,k=',k,'y(5,k)=',y(3+jwav1,k)
!        print *,'wave3r,k=',k,'y(6,k)=',y(1+jwav1*2,k)
!        print *,'wave3i,k=',k,'y(7,k)=',y(2+jwav1*2,k)
!        print *,'wave4r,k=',k,'y(8,k)=',y(3+jwav1*2,k)

!        print *,'mype=',mype,' fhour=',fhour,'rem=',remkh
!        print *,'wave0, k=',k,'x(0,k)=',x(1,k)
!        print *,'wave0i,k=',k,'x(1,k)=',x(2,k)
!        print *,'wave1r,k=',k,'x(2,k)=',x(3,k)
!        print *,'wave1i,k=',k,'x(3,k)=',x(1+jwav1,k)
!        print *,'wave2r,k=',k,'x(4,k)=',x(2+jwav1,k)
!        print *,'wave2i,k=',k,'x(5,k)=',x(3+jwav1,k)
!        print *,'wave3r,k=',k,'x(6,k)=',x(1+jwav1*2,k)
!        print *,'wave3i,k=',k,'x(7,k)=',x(2+jwav1*2,k)
!        print *,'wave4r,k=',k,'x(8,k)=',x(3+jwav1*2,k)

!        print *,'mype=',mype,' fhour=',fhour,'rem=',remkh
!        print *,'wave0, k=',k,'w(0,k)=',w(1,k)
!        print *,'wave0i,k=',k,'w(1,k)=',w(2,k)
!        print *,'wave1r,k=',k,'w(2,k)=',w(3,k)
!        print *,'wave1i,k=',k,'w(3,k)=',w(1+jwav1,k)
!        print *,'wave2r,k=',k,'w(4,k)=',w(2+jwav1,k)
!        print *,'wave2i,k=',k,'w(5,k)=',w(3+jwav1,k)
!        print *,'wave3r,k=',k,'w(6,k)=',w(1+jwav1*2,k)
!        print *,'wave3i,k=',k,'w(7,k)=',w(2+jwav1*2,k)
!        print *,'wave4r,k=',k,'w(8,k)=',w(3+jwav1*2,k)

!      endif
#endif
#ifdef MP
      endif
#endif
200   continue
#ifdef NONHYD
      do k=1,levr+1
      do i=1,LNWAVS
      o(i,k) = onm(i,k) + dt2 * o(i,k)
      enddo
      enddo
#endif
#ifdef CHGTLEV
! hmhj 04272007 force zero perturbations to u,v on model top
      k = levr
      do i=1,lnwavp
!-------------------------------------- uu ---------
      uum(i,k) = 0.0
      uu(i,k) = 0.0
      x(i,k) = 0.0
! ------------------------------------- vv ---------
      vvm(i,k) = 0.0
      vv(i,k) = 0.0
      w(i,k) = 0.0
      enddo
!-----------------------------------------------
#endif
!-----------------------------------------------
#ifdef ORIGIN_THREAD
!$doacross share(rm,dt2,rt),local(i,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rm,dt2)
!mic$1 shared(rt)
!mic$1 private(i,k)
#endif
#ifdef OPENMP
!$omp parallel do private(nc,i,k)
#endif
      do 202 nc=1,ntotal
      do 202 k=1,levr
      do 205 i=1,LNWAVS
! ------------------------------------- mixing ratio
      rt(i,k,nc) = rm(i,k,nc) + dt2 * rt(i,k,nc)
205   continue
#ifdef MP
      if( mype.eq.master ) then
#endif
#ifdef MBC
!      print *,'mype=',mype,'lev=',levr,'lnwavs=',LNWAVS,                 &
!     &    'jwav1=',jwav1
      if (nc .eq. 1) then
         rt(1,k,nc)=0.
      endif
!      remkh=mod(fhour,6.0)
!      if ( remkh .eq. 0) then
!      print *,'mype=',mype,' fhour=',fhour,'rem=',remkh
!      print *,'wave0, nc=',nc,'k=',k,'rt(0,k)=',rt(1,k,nc)
!      print *,'wave0i,nc=',nc,'k=',k,'rt(1,k)=',rt(2,k,nc)
!      print *,'wave1r,nc=',nc,'k=',k,'rt(2,k)=',rt(3,k,nc)
!      print *,'wave1i,nc=',nc,'k=',k,'rt(3,k)=',rt(1+jwav1,k,nc)
!      print *,'wave2r,nc=',nc,'k=',k,'rt(4,k)=',rt(2+jwav1,k,nc)
!      print *,'wave2i,nc=',nc,'k=',k,'rt(5,k)=',rt(3+jwav1,k,nc)
!      print *,'wave3r,nc=',nc,'k=',k,'rt(6,k)=',rt(1+jwav1*2,k,nc)
!      print *,'wave3i,nc=',nc,'k=',k,'rt(7,k)=',rt(2+jwav1*2,k,nc)
!      print *,'wave4r,nc=',nc,'k=',k,'rt(8,k)=',rt(3+jwav1*2,k,nc)
!      endif
#endif
#ifdef MP
      endif
#endif
202   continue
!
#ifdef LBC
#ifdef MP
      if ( mype.eq.master) then
#endif
      bc_mask=1.0
      do j=1,jwav1
         bc_mask(j)=0.0
      enddo
      do i=1,iwav1
         bc_mask(1+(i-1)*jwav1)=0.0
      enddo
#ifdef MP
      endif
      call rmpsf2p(bc_mask,lnwav,bc_maskp,lnwavp,1)
#else
      bc_maskp = bc_mask
#endif
      do i=1,LNWAVS
         z(i)=z(i)*bc_maskp(i)
      enddo
      do k=1,levr
         do i=1,LNWAVS
            x(i,k)=x(i,k)*bc_maskp(i)
            y(i,k)=y(i,k)*bc_maskp(i)
            w(i,k)=w(i,k)*bc_maskp(i)
            rt(i,k,1)=rt(i,k,1)*bc_maskp(i)
         enddo
      enddo
#endif

! ----------------------------------------------------
      return
      end
