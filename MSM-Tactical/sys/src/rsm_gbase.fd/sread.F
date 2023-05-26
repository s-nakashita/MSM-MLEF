#include <define.h>
      subroutine sread(n,label,fhour,idate,                              & 
     &                 flat,flon,fm2,fm2x,fm2y,sl,si,                    &
     &                 fgz,fq,fte,fuu,fvv,frq                            &
#ifdef NONHYD
     &                     ,fpp,ftt,fww,tov                              &
#endif
     &                 )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sread
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  read regional sigma surface file and base field to obtain
!            regional perturbation in wave coefficient.
!
! program history log:
!
! usage:    call sread(n,fhour,idate,flat,flon,fm2,fm2x,fm2y,sl,si,
!          1           fgz,fq,fte,fuu,fvv,frq)
!   input argument list:
!     n         - file input unit number
!
!   output argument list:
!     fhour     - forecast hour
!     idate     - integrer date of hour month day year.
!     flat      - latitudes
!     flon      - longitudes
!     fm2       - square of map factor
!     fm2x      - d (fm2) / dx
!     fm2y      - d (fm2) / dy
!     sl        - sigma layers
!     si        - sigma interfaces
!     fgz       - perturbation wave coefficient of terrain height
!     fq        - perturbation wave coefficient of log surface pressure (cb)
!     fte       - perturbation wave coefficient of virtual temperature
!     fuu       - perturbation wave coefficient of x-direction wind 
!     fvv       - perturbation wave coefficient of y-direction wind
!     frq       - perturbation wave coefficient of specific humidity
!
!   input files: 
!       n
!
!   output files: none
!
!   subprograms called:
!       maxmin  getptb  gdtocc gdtocs gdtosc 
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
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      real, allocatable :: full(:,:),fulv(:,:),fullk1(:)
      real, allocatable :: fulltmp(:,:),fullk1tmp(:)
#endif
      real, allocatable :: grid(:,:),grdv(:,:),gridk1(:)
#ifdef NONHYD
      real, allocatable :: tbs(:,:)
#endif
#include <rscomltb.h>
#ifdef MP
#define LNGRDS lngrdp
#define LNWAVS lnwavp
#else
#define LNGRDS lngrd
#define LNWAVS lnwav
#endif
!cc
      parameter(levmax=100)
      character*8 label(4)
      real(kind=kind_io4), allocatable :: tmps(:)
      real(kind=kind_io4) fhours,sis(levr+1),sls(levr)
      real(kind=kind_io4) dummy(2*levmax+1-levrp1-levr)                  &  
     &         ,ext(512-(6+2*levmax))
      dimension idate(4),si(levrp1),sl(levr),                            &
     & flat(LNGRDS), flon(LNGRDS),                                       &
     &  fm2(LNGRDS), fm2x(LNGRDS), fm2y(LNGRDS),                         &
     & fgz(LNWAVS), fq(LNWAVS),fte(LNWAVS,levr),                         &
     & fuu(LNWAVS,levr),fvv(LNWAVS,levr),frq(LNWAVS,levr,ntotal)         &
#ifdef NONHYD
     &,fpp(LNWAVS,levr),ftt(LNWAVS,levr),fww(LNWAVS,levr+1)              &
     &,tov(levr)
      real*4 qs4(lngrd),sl4(levr),tempik                                 &
#endif
     &  ,sreadtmp1(2)
!cjun
      dimension tmp(lngrd,levr),tmpk1(lngrd)
! zyf test
      dimension zyftmp(LNWAVS,levr),zyfgrid(LNGRDS,levr+1)
      dimension zyffull(lngrd,levr+1)
      character cfhour*16,cform*40
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,' ---- begin sread -----'
#ifdef MP
      endif
#endif
!
#ifdef MP
      allocate (full(lngrd,levr+1))
      allocate (fulv(lngrd,levr))
!      allocate (fulltmp(lngrd,levr))
      allocate (fullk1(lngrd))
!      allocate (fullk1tmp(lngrd))
#endif
      allocate (grid(LNGRDS,levr+1))
      allocate (grdv(LNGRDS,levr))
      allocate (gridk1(LNGRDS))
      allocate (tmps(lngrd))
#ifdef NONHYD
      allocate (tbs(lngrd,levr))
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      rewind n
      read(n) label
      print 3000,label,n
3000  format(1h0,'sread label  ',4a10,' n=',i3)
!
      read(n) fhours,idate,(sis(k),k=1,levrp1),(sls(k),k=1,levr)         &  
     &         ,dummy,ext
      fhour=fhours
      call sgl2ful(sis,si,levr+1)
      call sgl2ful(sls,sl,levr)
      print *,' fhour idate si sl ',fhour,idate,si,sl
      print *,' lngrd=',lngrd,'lngrdb=',lngrdb
#ifdef MP
      endif
      call rmpbcastc(label,32)
      call rmpbcastr(fhour,1)
      call rmpbcasti(idate,4)
      call rmpbcastr(si,levrp1)
      call rmpbcastr(sl,levr)
#endif
!-------------
#ifdef MP
#define GRIDS full
#define GRDVS fulv
#define GRIDK1S fullk1
#else
#define GRIDS grid
#define GRDVS grdv
#define GRIDK1S gridk1
#endif
!-----------------------------------------gz
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDK1S,lngrd)
!     call maxmin(GRIDK1S,lngrd,1,1,1,'sread gz')
#ifdef MP
      endif
      call rmpgf2p(GRIDK1S,igrd1,jgrd1,gridk1,igrd1p,jgrd1p,1)
!      call rmpgp2f(gridk1,igrd1p,jgrd1p,fullk1tmp,igrd1,jgrd1,1)
!      if( mype.eq.master ) then
!        call maxmin(fullk1tmp,lngrd,1,1,1,'ptb gz grd 1')
!        open ( 93,file='test_lev1',form='unformatted')
!        write(93) fullk1tmp
!        close(93)
!      endif
#endif
      call getptb(gridk1,gzb,sgzb,wsltb,1)
!#ifdef MP
!      tmp=0.
!      call rmpgp2f(gridk1,igrd1p,jgrd1p,tmp,igrd1,jgrd1,1)
!      if (mype.eq.master) then
!        call maxmin(tmp,lngrd,1,1,1,'ptb gz grd')
!      endif
!#endif
      call gdtocc(gridk1,fgz,1)
!---------------------------------------- q
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDK1S,lngrd)
!     call maxmin(GRIDK1S,lngrd,1,1,1,'sread q')
#ifdef NONHYD
      do i=1,lngrd
        qs4(i) = GRIDK1S(i)
      enddo
#endif
#ifdef MP
      endif
      call rmpgf2p(GRIDK1S,igrd1,jgrd1,gridk1,igrd1p,jgrd1p,1)
#endif
      call getptb(gridk1,qb,sqb,wsltb,1)
      call gdtocc(gridk1,fq,1)
!cjun
!--------------------------------------- te
#ifdef MP
      if( mype.eq.master ) then
#endif
      do 100 k=1,levr
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDS(1,k),lngrd)
100   continue
!     call maxmin(GRIDS,lngrd,k,1,k,'sread te')
#ifdef NONHYD
      do k=1,levr
        tov(k)=0.
      do i=1,lngrd
        tbs(i,k) = GRIDS(i,k)
        tov(k)=tov(k)+tbs(i,k)
      enddo
        tov(k)=tov(k)/float(lngrd)
      enddo
#endif
#ifdef MP
      endif
#ifdef NONHYD
      call rmpbcastr(tov,levr)
#endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,grid,igrd1p,jgrd1p,levr)
#endif
      call getptb(grid,teb,steb,wsltb,levr)
      call gdtocc(grid,fte,levr)

!---------------------------------------- uu vv
#ifdef MP
      if( mype.eq.master ) then
#endif
      do 110 k=1,levr
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDS(1,k),lngrd)
      read(n) (tmps(i),i=1,lngrd)
       call sgl2ful(tmps,GRDVS(1,k),lngrd)
110   continue
!     call maxmin(GRIDS,lngrd,k,1,k,'sread uu')
!     call maxmin(GRIDS,lngrd,k,1,k,'sread vv')
#ifdef MP
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,grid,igrd1p,jgrd1p,levr)
      call rmpgf2p(GRDVS,igrd1,jgrd1,grdv,igrd1p,jgrd1p,levr)
#endif
      call getptb(grid,uub,suub,wsltb,levr)
      call gdtosc(grid,fuu,levr)
      call getptb(grdv,vvb,svvb,wsltb,levr)
      call gdtocs(grdv,fvv,levr)
!-------------------------------------------- rq
      do 121 nc = 1,ntotal
#ifdef MP
      if( mype.eq.master ) then
#endif
      do 120 k=1,levr
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDS(1,k),lngrd)
120   continue
#ifdef MP
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,grid,igrd1p,jgrd1p,levr)
#endif 
      call getptb(grid,rqb(1,1,nc),srqb(1,1,nc),wsltb,levr)
      call gdtocc(grid,frq(1,1,nc),levr)
121   continue
!
#ifdef NONHYD
!c--------------------------------------- pn
#ifdef MP
      if( mype.eq.master ) then
#endif
      do k=1,levr
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDS(1,k),lngrd)
      enddo
      do k=1,levr
        sl4(k) = log( sl(k) )
      do i=1,lngrd
        tempik = qs4(i) + sl4(k)
        GRIDS(i,k) = GRIDS(i,k) - tempik
      enddo
      enddo
#ifdef MP
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,grid,igrd1p,jgrd1p,levr)
#endif
      call gdtocc(grid,fpp,levr)
!--------------------------------------- tn
#ifdef MP
      if( mype.eq.master ) then
#endif
      do k=1,levr
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDS(1,k),lngrd)
      enddo
      do k=1,levr
      do i=1,lngrd
        GRIDS(i,k) = GRIDS(i,k) - tbs(i,k)
      enddo
      enddo
#ifdef MP
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,grid,igrd1p,jgrd1p,levr)
#endif
      call gdtocc(grid,ftt,levr)
!--------------------------------------- wn
#ifdef MP
      if( mype.eq.master ) then
#endif
      do k=1,levr+1
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,GRIDS(1,k),lngrd)
      enddo
#ifdef MP
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,grid,igrd1p,jgrd1p,levr+1)
#endif
      call gdtocc(grid,fww,levr)
      call gdtocc(grid(1,levr+1),fww(1,levr+1),1)
!
#endif
!-------------------------------------------- fm2
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
#ifdef MP
      call sgl2ful(tmps,GRIDS, lngrd)
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,fm2,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,fm2, lngrd)
#endif
!-------------------------------------------- fm2x
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
#ifdef MP
      call sgl2ful(tmps,GRIDS,lngrd)
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,fm2x,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,fm2x, lngrd)
#endif
!-------------------------------------------- fm2y
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
#ifdef MP
      call sgl2ful(tmps,GRIDS,lngrd)
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,fm2y,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,fm2y, lngrd)
#endif
!-------------------------------------------- flat
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
#ifdef MP
      call sgl2ful(tmps,GRIDS,lngrd)
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,flat,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,flat, lngrd)
#endif
!-------------------------------------------- flon
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,lngrd)
#ifdef MP
      call sgl2ful(tmps,GRIDS,lngrd)
      endif
      call rmpgf2p(GRIDS,igrd1,jgrd1,flon,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,flon, lngrd)
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,' ------ end sread ----- '
#ifdef MP
      endif
#endif
!
#ifdef MP
      deallocate (full)
      deallocate (fulv)
      deallocate (fullk1)
#endif
      deallocate (grid)
      deallocate (grdv)
      deallocate (gridk1)
      deallocate (tmps)
#ifdef NONHYD
      deallocate (tbs)
#endif
!
      return
      end
