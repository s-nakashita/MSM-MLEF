      subroutine rsmltb(secnow,secnext,ggz,gq,gte,gdu,gzv,grq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsmltb
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do the global to regional grid for base field of regional and
!            compute the tendency of base field.
!
! program history log:
!
! usage:    call rsmltb(secnow,secnext,ggz,gq,gte,gdi,gze,grq)
!   input argument list:
!     secnow    - current time in seconds
!     secnext   - next base field time in second
!     ggz       - global terrain coefficient
!     gq        - global log surface pressure coefficient
!     gte       - global virtual temperature coefficient
!     gdi       - global divergence coefficient
!     gze       - global vorticity coefficient
!     grq       - global specific humidity coefficient
!
!   output argument list:
!
!   common block:
!       rscomf  rscomltb
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!       gsm2rsm  
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....use u v as input, update u v tendency ....
!.....
!....
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
#include <rscomltb.h>
!cc
#ifdef MP
#define LNT22S lnt22p
#else
#define LNT22S lnt22
#endif
!
#ifdef G2R
      dimension ggz(LNT22S),gq(LNT22S),gte(LNT22S,levs)                  &  
     & ,gdu(LNT22S,levs),gzv(LNT22S,levs),grq(LNT22S,levs,ntotal)
#endif
#ifdef C2R
      dimension ggz(clngrd),gq(clngrd),gte(clngrd,levr)                  &
     & ,gdu(clngrd,levr),gzv(clngrd,levr),grq(clngrd,levr,ntotal)
#endif
      dimension gzbt(lngrdb)
!....
#ifdef MP
        if (mype.eq.master) then
#endif
      print *,' ------ into rsmltb ----- '
!                                                         
#ifdef MP
        endif
#endif
!
      do 100 i=1,lngrdb
      gzbt (i)=gzb (i)
 100  continue
!
#ifdef G2R
         call gsm2bgd(ggz,gq,gte,gdu,gzv,grq)
#endif
#ifdef C2R
         call rsm2bgd(ggz,gq,gte,gdu,gzv,grq)
#endif
!.....
         rdt = 1.0 / ( secnext - secnow )
#ifdef MP
        if (mype.eq.master) then
#endif
!         print *,' in rsmltb secnow secnext rdt '
!     1                      ,secnow,secnext,rdt
#ifdef MP
        endif
#endif
         do 225 i=1,lngrdb
         gzb (i)=gzbt (i)
         qltb (i) = (qltb (i)-qb (i))*rdt
 225     continue
!
#ifdef ORIGIN_THREAD
!$doacross share(teb,uub,vvb,teltb,uultb,vvltb,rdt,lngrdb),
!$&        local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(teb,uub,vvb)
!mic$1 shared(teltb,uultb,vvltb)
!mic$1 shared(rdt,lngrdb)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
         do 235 k=1,levr
         do 235 i=1,lngrdb
         teltb(i,k) = (teltb(i,k)-teb(i,k))*rdt
         uultb(i,k) = (uultb(i,k)-uub(i,k))*rdt
         vvltb(i,k) = (vvltb(i,k)-vvb(i,k))*rdt
 235     continue
!
#ifdef ORIGIN_THREAD
!$doacross share(rqb,rqltb,rdt,lngrdb),
!$&        local(i)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rqb)
!mic$1 shared(rqltb)
!mic$1 shared(rdt,lngrdb)
!mic$1 private(i)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k,nc)
#endif
         do 240 k=1,levr
         do 240 nc=1,ntotal
         do 240 i=1,lngrdb
         rqltb(i,k,nc) = (rqltb(i,k,nc)-rqb(i,k,nc))*rdt
 240     continue
!
      return
      end
