      subroutine rdfini(kstep,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rdfini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional digital filter forward initialization
!
! program history log:
!
! usage:    call rdfini(icall,hrini)
!   input argument list:
!     icall     - index to have inital call or following calls.
!     hrini     - initialization period, it will be no initialization
!                 if hrini=0
!
!   output argument list:
!
!   common block:
!     rscomf  rscomltb /inicom/
!
!   input files: none
!
!   output files: none
!      nrinit : for scratch use as temperory file.
!
!   subprograms called: 
!      rfixio
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
#include <npesi.h>
#include <comrmpi.h>
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
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
!.....
      common/rinicom/    totsum                                          &   
     &                    , qs(LNWAVS)                                   &
     &  ,tes(LNWAVS,levr),rqs(LNWAVS,levr,ntotal)                        &
     &  ,uus(LNWAVS,levr),vvs(LNWAVS,levr)
!
      if(kstep.lt.-nstep) then
        totsum=0.0
        print *,' initial rdfini '
        do k=1,levr
          do i=1,LNWAVS
            uus(i,k) = 0.0
            vvs(i,k) = 0.0
            tes(i,k) = 0.0
          enddo
        enddo
        do nc=1,ntotal
        do k=1,levr
          do i=1,LNWAVS
            rqs(i,k,nc) = 0.0
          enddo
        enddo
        enddo
        do 12 i=1,LNWAVS
          qs(i) = 0.0
  12    continue
!
      elseif(kstep.le.nstep) then
        if(kstep.ne.0) then
          sx= acos(-1.)*kstep/nstep
          wx= acos(-1.)*kstep/(nstep+1)
          digfil= sin(wx)/wx * sin(sx)/sx
        else
          digfil = 1.0
        endif
        totsum = totsum + digfil
        do k=1,levr
          do j=1,LNWAVS
            uus(j,k) = uus(j,k) + digfil*uu(j,k)
            vvs(j,k) = vvs(j,k) + digfil*vv(j,k)
            tes(j,k) = tes(j,k) + digfil*te(j,k)
          enddo
        enddo
        do nc=1,ntotal
        do k=1,levr
          do j=1,LNWAVS
            rqs(j,k,nc) = rqs(j,k,nc) + digfil*rq(j,k,nc)
          enddo
        enddo
        enddo
        do 120 j=1,LNWAVS
          qs(j) = qs(j) + digfil*q(j)
 120    continue
      else
        do k=1,levr
          do j=1,LNWAVS
            uu (j,k) = uus(j,k) / totsum
            vv (j,k) = vvs(j,k) / totsum
            te (j,k) = tes(j,k) / totsum
          enddo
        enddo
        do nc=1,ntotal
        do k=1,levr
          do j=1,LNWAVS
            rq (j,k,nc) = rqs(j,k,nc) / totsum
          enddo
        enddo
        enddo
        do 220 j=1,LNWAVS
          q (j) = qs(j) / totsum
 220    continue
      endif
!................................................
!
      return
      end
