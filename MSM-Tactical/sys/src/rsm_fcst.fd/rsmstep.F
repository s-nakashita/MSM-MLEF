      subroutine rsmstep
!ccj
        use cnttime
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
#include <rscomgpd.h>
!
! flag for 4th-order numerical perturbation diffusion on sigma surface
!    ndflg=1: on
!    ndflg=0: off
!
!testjun
      dimension tmp(lnwav,levrp1),tmpsk(lnwav,levr)
!
      parameter(ndflg=1)
!
!cc..........................................
#ifdef MP
        if (mype.eq.master) then
#endif
      print *,' enter rsmstep for fhour=',fhour
#ifdef MP
        endif
#endif
#ifdef RKN
      isave=0
      if(lsken) isave=1
#endif
!jun merge rloopr into rloopa
!      if(lsphy) call rloopr
!.....
#ifdef A
      if(lsbgt) call rbgt000
#endif
!
#ifdef MP
#ifdef CNT_2 
      call cnttime_on("rloopmp")
#endif
      call rloopmp
#ifdef CNT_2 
      call cnttime_off("rloopmp")
#endif
#else
      call rloopsp
#endif
!.....
#ifdef A
      if(lsbgt) call rbgtlpa
#endif
!

#ifdef NONHYD
      if(lsimp) call nsicdif
#else
      if(lsimp) call rsicdif
#endif
!.....
#ifdef A
      if(lsbgt) call rbgtsic
#endif
!....
      call rlatbnd
!.....
      call rupdate

!.....
!      if(lsdmp) call rloopd
!.....
#ifdef A
      if(lsbgt) call rbgtlpd
#endif
!
!testjun
      if(ndflg.eq.1) call rdeldif(sl)
!
#ifdef A
      if(lsbgt) call rbgtdif
#endif
!.....
      do 500 j=1,LNWAVS
       qm(j)=q(j)
       q(j)=z(j)
 500  continue
!.....
      if(lsftr) call rfilt1(filta,filtb)
!....
! merge rloopb into rloopa
!      if(lsphy) call rloopb
!
#ifdef A
      if(lsbgt) call rbgtlpb
#endif
!.....
#ifdef ORIGIN_THREAD
!$doacross share(uu,vv,te,x,w,y),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uu,vv,te)
!mic$1 shared( x, w, y)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j)
#endif
      do k=1,levr
        do j=1,LNWAVS
          uu(j,k)=x(j,k)
          vv(j,k)=w(j,k)
          te(j,k)=y(j,k)
#ifdef NONHYD
          pn(j,k)=p(j,k)
          tn(j,k)=t(j,k)
          on(j,k)=o(j,k)
#endif
        enddo
      enddo
#ifdef NONHYD
      k=levr+1
      do j=1,LNWAVS
        on(j,k)=o(j,k)
      enddo
#endif

#ifdef ORIGIN_THREAD
!$doacross share(rq,rt),local(j)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(rq)
!mic$1 shared(rt)
!mic$1 private(j)
#endif
#ifdef OPENMP
!$omp parallel do private(j,nc,k)
#endif
      do nc=1,ntotal
      do k=1,levr
        do j=1,LNWAVS
          rq(j,k,nc)=rt(j,k,nc)
        enddo
      enddo
      enddo
      if(lsftr) call rfilt2(filtb)
!
#ifdef RKN
      if(lsken.and..not.lsout) then
        write(0,*)' gwvx will increment itnum from',                     &  
     &  itnum,' to',itnum+1,' fhour= ',fhour
        itnum=itnum+1
      endif
#endif
!
      return
      end
