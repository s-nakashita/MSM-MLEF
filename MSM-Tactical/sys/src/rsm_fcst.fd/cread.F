#include <define.h>
      subroutine cread(n,ijmaxf,ijmaxp,kmax,                            &
#ifdef MP
     &                 mype,master,                                     &
#ifdef G2R
     &                 jcap,lwvdef,                                     &
#endif
#endif
     &                 label,fhour,idate,sl,si,                         &
     &                 gz,q,te,du,zv,rq                                 &
#ifdef C2R
     &                ,fm2,fm2x,fm2y,flat,flon                          &
#endif
     &                ,ntotal,ntoz,ncld,ncldb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rread
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  read regional sigma surface file.
!
! program history log:
!
! usage:    call rread(n,fhour,idate,flat,flon,fm2,fm2x,fm2y,sl,si,
!          1           gz,q,te,uu,vv,rq)
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
!     gz        - terrain height
!     q         - log surface pressure (cb)
!     te        - virtual temperature
!     uu        - x-direction wind 
!     vv        - y-direction wind
!     rq        - specific humidity
!
!   input files: 
!       n
!
!   output files: none
!
!   subprograms called:
!       maxmin  
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
      character*8 label(4)
      dimension idate(4),si(kmax+1),sl(kmax),                           &
#ifdef C2R
     & fm2(ijmaxp),fm2x(ijmaxp),fm2y(ijmaxp),flat(ijmaxp),flon(ijmaxp), &
#endif
     & gz(ijmaxp), q(ijmaxp),te(ijmaxp,kmax),                           &
     & du(ijmaxp,kmax),zv(ijmaxp,kmax),rq(ijmaxp,kmax,ntotal)
      real(kind=kind_io4), allocatable :: tmps(:)
      integer   ijmaxf1
#ifdef MP
      integer   mype,master
      real, allocatable :: coef(:),coefk(:,:),coefv(:,:),coefq(:,:,:)
#ifdef G2R
      integer   jcap,lwvdef
      dimension lwvdef(jcap+1)
#endif
#endif
      real(kind=kind_io4) fhours,sis(kmax+1),sls(kmax)
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,' ---- begin cread -----'
#ifdef G2R
      ijmaxf1=ijmaxf-1
#else
      ijmaxf1=ijmaxf
#endif
#ifdef MP
      endif
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      rewind n
      read(n) label
      write(6,*) label
      read(n) fhours,(idate(i),i=1,4),                                  &
     &           (sis(k),k=1,kmax+1),(sls(k),k=1,kmax)
      fhour=fhours
      call sgl2ful(sis,si,kmax+1)
      call sgl2ful(sls,sl,kmax)
      write(6,210) (idate(i),i=1,4),fhour,                              &
     &           (si(k),k=1,kmax+1),(sl(k),k=1,kmax)
210   format(5x,' base field input date and fcst hour ',                &
     &       4i5,f7.1/(2x,g13.6))
#ifdef MP
      endif
      call rmpbcastc(label,4)
      call rmpbcastr(fhour,1)
      call rmpbcasti(idate,4)
      call rmpbcastr(si,kmax+1)
      call rmpbcastr(sl,kmax)
#endif
!
      allocate (tmps(ijmaxf))
#ifdef MP
      allocate (coef(ijmaxf))
      allocate (coefk(ijmaxf,kmax))
      allocate (coefv(ijmaxf,kmax))
      allocate (coefq(ijmaxf,kmax,ntotal))
#endif
!-----------------------------------------------------gz
#define GZS gz
#ifdef MP
      if( mype.eq.master ) then
#ifdef G2R
#undef GZS
#define GZS coef
#endif
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,GZS,ijmaxf1)
!     call maxmin(GZS,ijmaxf1,1,1,1,'global read gz')
#ifdef MP
      endif
#ifdef G2R
      call mpsf2p(jcap,lwvdef,coef,ijmaxf,gz,ijmaxp,1)
#else
      call rmpbcastr(GZS,ijmaxf)
#endif
#endif
!------------------------------------------------------q
#define QS q
#ifdef MP
      if( mype.eq.master ) then
#ifdef G2R
#undef QS
#define QS coef
#endif
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,QS,ijmaxf1)
!     call maxmin(QS,ijmaxf1,1,1,1,'global read q')
#ifdef MP
      endif
#ifdef G2R
      call mpsf2p(jcap,lwvdef,coef,ijmaxf,q,ijmaxp,1)
#else
      call rmpbcastr(QS,ijmaxf)
#endif
#endif
!------------------------------------------------------te
#define TES te
#ifdef MP
      if( mype.eq.master ) then
#ifdef G2R
#undef TES
#define TES coefk
#endif
#endif
      do k=1,kmax
        read(n) (tmps(i),i=1,ijmaxf1)
        call sgl2ful(tmps,TES(1,k),ijmaxf1)
!     call maxmin(TES(1,k),ijmaxf1,1,1,1,'global read ts')
      enddo
#ifdef MP
      endif
#ifdef G2R
      call mpsf2p(jcap,lwvdef,coefk,ijmaxf,te,ijmaxp,kmax)
#else
      call rmpbcastr(TES,ijmaxf*kmax)
#endif
#endif
!------------------------------------------------------du zv
#define DUS du
#define ZVS zv
#ifdef MP
      if( mype.eq.master ) then
#ifdef G2R
#undef DUS
#undef ZVS
#define DUS coefk
#define ZVS coefv
#endif
#endif
      do k=1,kmax
        read(n) (tmps(i),i=1,ijmaxf1)
        call sgl2ful(tmps,DUS(1,k),ijmaxf1)
        read(n) (tmps(i),i=1,ijmaxf1)
        call sgl2ful(tmps,ZVS(1,k),ijmaxf1)
!       call maxmin(DUS(1,k),ijmaxf1,1,1,1,'global read uu')
!       call maxmin(ZVS(1,k),ijmaxf1,1,1,1,'global read vv')
      enddo
#ifdef MP
      endif
#ifdef G2R
      call mpsf2p(jcap,lwvdef,coefk,ijmaxf,du,ijmaxp,kmax)
      call mpsf2p(jcap,lwvdef,coefv,ijmaxf,zv,ijmaxp,kmax)
#else
      call rmpbcastr(DUS,ijmaxf*kmax)
      call rmpbcastr(ZVS,ijmaxf*kmax)
#endif
#endif
!------------------------------------------------rq
#define RQS rq
#ifdef MP
      if( mype.eq.master ) then
#ifdef G2R
#undef RQS
#define RQS coefq
#endif
#endif
      do k=1,kmax
        read(n) (tmps(i),i=1,ijmaxf1)
        call sgl2ful(tmps,RQS(1,k,1),ijmaxf1)
!       call maxmin(RQS(1,k,1),ijmaxf1,1,1,1,'global read rq')
      enddo
#ifdef MP
      endif
#ifdef G2R
      call mpsf2p(jcap,lwvdef,coefq(1,1,1),ijmaxf,                       &    
     &            rq(1,1,1),ijmaxp,kmax)
#endif
#endif
!------------------------------------------------oz
       if(ntoz.eq.1) then
         do kt = 1,ntoz
           ktrace = 1+kt
#ifdef MP
           if( mype.eq.master ) then
#endif
           do k=1,kmax
             read(n)(tmps(i),i=1,ijmaxf1)
             call sgl2ful(tmps,RQS(1,k,ktrace),ijmaxf1)
!            call maxmin(RQS(1,k,ktrace),ijmaxf1,1,1,1,'global read rq')
           enddo
#ifdef MP
           endif
#ifdef G2R
           call mpsf2p(jcap,lwvdef,coefq(1,1,ktrace),ijmaxf,            &    
     &                 rq(1,1,ktrace),ijmaxp,kmax)
#endif
#endif
         enddo
       endif
!------------------------------------------------cld
       if(ncldb.ge.1) then
         do kc = 1,ncldb
           kcloud = 1+ntoz+kc
#ifdef MP
           if( mype.eq.master ) then
#endif
           do k=1,kmax
             read(n)(tmps(i),i=1,ijmaxf1)
             call sgl2ful(tmps,RQS(1,k,kcloud),ijmaxf1)
!            call maxmin(RQS(1,k,kcloud),ijmaxf1,1,1,1,'global read rq')
           enddo
#ifdef MP
           endif
#ifdef G2R
           call mpsf2p(jcap,lwvdef,coefq(1,1,kcloud),ijmaxf,             &   
     &                 rq(1,1,kcloud),ijmaxp,kmax)
#endif
#endif
         enddo
       endif
#ifdef MP
#ifdef C2R
       call rmpbcastr(RQS,ijmaxf*kmax*ntotal)
#endif
#endif

       !if(ncldb.eq.0.and.ncld.ge.1) then
       if(ncld.gt.ncldb) then
         do kc = ncldb+1,ncld
           kcloud = 1+ntoz+kc
           do k=1,kmax
           do i=1,ijmaxp
             rq(i,k,kcloud)=0.
           enddo
           enddo
         enddo
       endif

!cc

#ifdef MP
      if( mype.eq.master ) then
#endif
#ifdef G2R
      print*,' - end of reading base field from global file unit=',n
#endif
#ifdef C2R
      print*,' - end of reading base field from c file unit=',n
#endif
#ifdef MP
      endif
#endif
!
#ifdef C2R
#ifdef N2R
#ifdef MP
      if( mype.eq.master ) then
      do k=1,kmax
        read(n) (tmps(i),i=1,ijmaxf1)
      enddo
      do k=1,kmax
        read(n) (tmps(i),i=1,ijmaxf1)
      enddo
      do k=1,kmax+1
        read(n) (tmps(i),i=1,ijmaxf1)
      enddo
      endif
#endif
#endif
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,fm2,ijmaxf1)
!     call maxmin(fm2,ijmaxf1,1,1,1,'global read fm2')
#ifdef MP
      endif
      call rmpbcastr(fm2,ijmaxf)
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,fm2x,ijmaxf1)
!     call maxmin(fm2x,ijmaxf1,1,1,1,'global read fm2x')
#ifdef MP
      endif
      call rmpbcastr(fm2x,ijmaxf)
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,fm2y,ijmaxf1)
!     call maxmin(fm2y,ijmaxf1,1,1,1,'global read fm2y')
#ifdef MP
      endif
      call rmpbcastr(fm2y,ijmaxf)
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,flat,ijmaxf1)
!     call maxmin(flat,ijmaxf1,1,1,1,'global read flat')
#ifdef MP
      endif
      call rmpbcastr(flat,ijmaxf)
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(n) (tmps(i),i=1,ijmaxf1)
      call sgl2ful(tmps,flon,ijmaxf1)
!     call maxmin(flon,ijmaxf1,1,1,1,'global read flon')
#ifdef MP
      endif
      call rmpbcastr(flon,ijmaxf)
      if( mype.eq.master ) then
      print*,' - end of reading base field from regional file unit=',n
      endif
#endif
#endif
!
      deallocate (tmps)
#ifdef MP
      deallocate (coef)
      deallocate (coefk)
      deallocate (coefv)
      deallocate (coefq)
#endif
!
      return
      end
