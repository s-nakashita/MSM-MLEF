      subroutine rlatbnd
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rlatbnd
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  update the base field and its tendency.
!
! program history log:
!
! usage:    call rlatbnd
!
!   output argument list:
!
!   common block:
! rscomf  rscomltb
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
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
#include <rscomio.h>
#include <rscomltb.h>
!....
!
!  ----------- update global base field data at n
      do 900 i=1,lngrdb
      qb(i) = qb(i)+qltb(i)*dtltb
900   continue
!
#ifdef ORIGIN_THREAD
!$doacross share(teb,uub,vvb,teltb,uultb,vvltb,dtltb),
!$&        local(i,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(teb,uub,vvb)
!mic$1 shared(teltb,uultb,vvltb)
!mic$1 shared(dtltb)
!mic$1 private(i,k)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k)
#endif
      do 910 k=1,levr
      do 910 i=1,lngrdb
      teb(i,k) = teb(i,k)+teltb(i,k)*dtltb
      uub(i,k) = uub(i,k)+uultb(i,k)*dtltb
      vvb(i,k) = vvb(i,k)+vvltb(i,k)*dtltb
910   continue
!c
#ifdef ORIGIN_THREAD
!$doacross share(rqb,rqltb,dtltb),
!$&        local(i,k,nc)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rqb)
!mic$1 shared(rqltb)
!mic$1 shared(dtltb)
!mic$1 private(i,k,nc)
#endif
#ifdef OPENMP
!$omp parallel do private(i,k,nc)
#endif
      do 920 k=1,levr
      do 920 nc=1,ntotal
      do 920 i=1,lngrdb
      rqb(i,k,nc) = rqb(i,k,nc)+rqltb(i,k,nc)*dtltb
920   continue
!
      return
      end
