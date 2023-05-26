#include <define.h>
!=============================================================
      subroutine bwrite(n,label,fhour,idate,                             &  
     &                  flat,flon,fm2,fm2x,fm2y,sl,si,                   &
     &                  sgz,sq,ste,suu,svv,srq                           &
     &                                        )
!
!   input files:
!   n
!
!   output files: none
!
!   subprograms called:
!   maxmin  getful  cctogd  cstogd sctogd
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
      use physcons , only : pi=>con_pi
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      real, allocatable :: full(:,:),fulv(:,:),fullk1(:)
#endif
#ifdef MP
#define LNGRDS lngrdp
#define LNWAVS lnwavp
#else
#define LNGRDS lngrd
#define LNWAVS lnwav
#endif
      real, allocatable ::grid(:,:),grdv(:,:),gridk1(:)
#include <rscomltb.h>
#include <rscomloc.h>
!cc
      parameter(levmax=100)
      character*8 label(4)
      real(kind=kind_io4), allocatable :: tmps(:)
      real(kind=kind_io4) fhours,sis(levr+1),sls(levr)
      real(kind=kind_io4) dummy(2*levmax+1-levrp1-levr)                  &  
     &         ,ext(512-(6+2*levmax))
      dimension idate(4),si(levrp1),sl(levr),                            &
     & flat(LNGRDS), flon(LNGRDS),                                       &
     &   fm2(LNGRDS), fm2x(LNGRDS), fm2y(LNGRDS),                        &
     & sgz(LNGRDS), sq(LNGRDS),ste(LNGRDS,levr),                         &
     & suu(LNGRDS,levr),svv(LNGRDS,levr),srq(LNGRDS,levr,ntotal)         &
     &  ,bwritetmp1(2)

!cjun
      dimension tmp(lngrd,levr),tmpk1(lngrd)
      character cfhour*16,cform*40

!
#ifdef MP
      allocate (full(lngrd,levr+1))
      allocate (fulv(lngrd,levr))
      allocate (fullk1(lngrd))
#endif
      allocate (grid(LNGRDS,levr+1))
      allocate (grdv(LNGRDS,levr))
      allocate (gridk1(LNGRDS))
      allocate (tmps(lngrd))
!c
#ifdef MP
      if( mype.eq.master ) then
#endif
      label(1)='ncep'
      label(2)='rsm'
      label(3)='mpi'
      label(4)='version'
!c
      write(n) label
      print 3000,label,n
3000  format(1h0,'bwrite label  ',4a10,' n=',i3)
!
      do i=1,2*levmax+1-levrp1-levr
      dummy(i)=0.0
      enddo
      do i=1,512-(6+2*levmax)
      ext(i)=0.0
      enddo
!
      ext(1)=iwav1
      ext(2)=jwav1
      ext(3)=igrd1
      ext(4)=jgrd1
      ext(5)=levr
      ext(6)=2+levr*4+5
      ext(7)=rproj             !projection flag
      ext(8)=rtruth
      ext(9)=rorient
      ext(10)=rcenlat
      ext(11)=rcenlon
      ext(12)=rlftgrd
      ext(13)=rbtmgrd
      ext(14)=rdelx
      ext(15)=rdely
      ext(16)=0.0
!
      fhours=fhour
      call ful2sgl(si,sis,levrp1)
      call ful2sgl(sl,sls,levr)
      write(n) fhours,idate,(sis(k),k=1,levrp1),(sls(k),k=1,levr)        &
     &         ,dummy,ext
!
      print *,' fhour idate si sl ',fhour,idate,si,sl
      print *,' other model experimental constants: '                    & 
     &       ,' wavei wavej gridi gridj xlayers totfld '                 &
     &         ,(ext(i),i=1,6)                                           &
     &       ,' proj truth orient cenlat cenlon '                        &
     &         ,(ext(i),i=7,11)                                          &
     &       ,' grdleft grdbotm delx dely '                              &
     &         ,(ext(i),i=12,15)
#ifdef MP
      endif
#endif
#ifdef MP
#define GRIDS full
#define GRDVS fulv
#define GRIDK1S fullk1
#else
#define GRIDS grid
#define GRDVS grdv
#define GRIDK1S gridk1
#endif
!------------------------------------------ gz
      gridk1 = sgz
#ifdef MP
      call rmpgp2f(gridk1,igrd1p,jgrd1p,                                 &  
     &             GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDK1S,lngrd,1,1,1,'bwrite gz grid')
      call ful2sgl(GRIDK1S,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      endif
#endif
!-------------------------------------------- q
      gridk1 = sq
#ifdef MP
      call rmpgp2f(gridk1,igrd1p,jgrd1p,GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call ful2sgl(GRIDK1S,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      endif
#endif
!---------------------------------------------- te
!#ifdef MP
!      tmp=0.
!      call rmpsp2f(fte,lnwavp,tmp,lnwav,levr)
!      if (mype.eq.master) then
!        call maxmin(tmp,lnwav,levr,1,levr,'before bwrite te sp')
!      endif
!#endif
      grid = ste
#ifdef MP
      call rmpgp2f(grid,igrd1p,jgrd1p,GRIDS,igrd1,jgrd1,levr)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDS,lngrd,levr,1,levr,'bwrite te grid')
      do 300 k=1,levr
      call ful2sgl(GRIDS(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
300   continue
#ifdef MP
      endif
#endif
!------------------------------------------------ uu vv

      grid = suu
      grdv = svv
#ifdef MP
      call rmpgp2f(grid,igrd1p,jgrd1p,GRIDS,igrd1,jgrd1,levr)
      call rmpgp2f(grdv,igrd1p,jgrd1p,GRDVS,igrd1,jgrd1,levr)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDS,lngrd,levr,1,levr,'bwrite uu grid')
      call maxmin(GRDVS,lngrd,levr,1,levr,'bwrite vv grid')
      do 310 k=1,levr
      call ful2sgl(GRIDS(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      call ful2sgl(GRDVS(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
310   continue
#ifdef MP
      endif
#endif
!------------------------------------------------- rq
!ccj
      do nc = 1,ntotal
      grid = srq(:,:,nc)
#ifdef MP
         call rmpgp2f(grid,igrd1p,jgrd1p,GRIDS,igrd1,jgrd1,levr)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDS,lngrd,levr,1,levr,'bwrite rq grid')
      do 320 k=1,levr
      call ful2sgl(GRIDS(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
320   continue
#ifdef MP
      endif
#endif
      enddo
!cc
!-------------------------------------------------- fm2
#ifdef MP
      call rmpgp2f(fm2,igrd1p,jgrd1p,GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDK1S,lngrd,1,1,1,'bwrite fm2 grid')
      call ful2sgl(GRIDK1S, tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      endif
#endif
!-------------------------------------------------- fm2x
#ifdef MP
      call rmpgp2f(fm2x,igrd1p,jgrd1p,GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDK1S,lngrd,1,1,1,'bwrite fm2x grid')
      call ful2sgl(GRIDK1S, tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      endif
#endif
!-------------------------------------------------- fm2y
#ifdef MP
      call rmpgp2f(fm2y,igrd1p,jgrd1p,GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDK1S,lngrd,1,1,1,'bwrite fm2y grid')
      call ful2sgl(GRIDK1S, tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      endif
#endif
!-------------------------------------------------- flat
#ifdef MP
      call rmpgp2f(flat,igrd1p,jgrd1p,GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDK1S,lngrd,1,1,1,'bwrite flat grid')
      call ful2sgl(GRIDK1S, tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      endif
#endif
!-------------------------------------------------- flon
#ifdef MP
      call rmpgp2f(flon,igrd1p,jgrd1p,GRIDK1S,igrd1,jgrd1,1)
      if( mype.eq.master ) then
#endif
      call maxmin(GRIDK1S,lngrd,1,1,1,'bwrite flon grid')
      ! add 20220720
      if(maxval(GRIDK1S).gt.2.0e0*pi) then
        GRIDK1S = GRIDK1S - 2.0e0*pi
      end if
      call ful2sgl(GRIDK1S, tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
#ifdef MP
      print *,' ------ end bwrite ----- '
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
!
      return
      end
