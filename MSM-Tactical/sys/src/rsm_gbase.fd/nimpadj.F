      subroutine nimpadj
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c::::::: nonhydrostatic version of semi-implicit adjustment  ::::::::
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     parameter(alpha=0.7)
c
cccj
#include <paramodel.h>
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#ifdef NONHYD
#include <rscomfi.h>
#include <rscomspc.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscompln.h>
#include <rscomrad.h>
#include <rscomloc.h>
ccc
c
      common /repro/
     1  tmp(LNWAVS,levr+1),tmp1(LNWAVS,levr+1)
     3, dim(LNWAVS,levr)
      common/nsiccom/
     1          dcm(LNWAVS,levr,levr),
     1          wcm(levr,levr+1),
     1          x3minv(levr+1,levr+1),
     1          x4m   (levr+1,levr  ),
     2          tor(levr),dtx2
c
c all common arraies are defined in rsicdif
c all input and output are changes of prognostic variables
c input/output  uu vv te z pn tn on
c
c----------------- transform u v to di ze before semi-implicit ---
      call uv2dz(LNWAVS,uu,vv,tmp,tmp1,epsx,epsy,levr)
cmic$ do all
cmic$1 shared(tmp,tmp1,uu,vv)
cmic$1 private(i)
      do 20 k=1,levr
      do 20 i=1,LNWAVS
      uu (i,k) = tmp (i,k)	! div
      vv (i,k) = tmp1(i,k)	! vor
20    continue
c
c delta d2
cx    do k=1,levr
cx      do i=1,LNWAVS
cx        tmp(i,k) = 0.0
cx      enddo
cx    enddo
cx    do kk=1,levr
cx      do k=1,levr
cx        do i=1,LNWAVS
cx          tmp(i,k) = tmp(i,k) + dtx2*snnp1(i)*am(k,kk)*te(i,kk)
cx        enddo
cx      enddo
cx    enddo
      do k=1,levr
        do i=1,LNWAVS
cx        uu(i,k)=uu(i,k)+tmp(i,k)+dtx2*snnp1(i)*tor(k)*(z(i)+pn(i,k))
          uu(i,k)=uu(i,k)+         dtx2*snnp1(i)*tor(k)*(z(i)+pn(i,k))
        enddo
      enddo
c delta w2
      do k=1,levr+1
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr+1
          do i=1,LNWAVS
            tmp(i,k) = tmp(i,k) +
     1                 (cmn(k,kk)*tn(i,kk)+dmn(k,kk)*pn(i,kk))
          enddo
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          on(i,k)=on(i,k)+dtx2*tmp(i,k)
        enddo
      enddo
c delta d
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr+1
        do k=1,levr
          do i=1,LNWAVS
            sndt2=snnp1(i)*dtx2*dtx2
            tmp(i,k) = tmp(i,k) + sndt2*wcm(k,kk)*on(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          uu(i,k) = uu(i,k) + tmp(i,k)
        enddo
      enddo
c
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr
          do i=1,LNWAVS
            tmp(i,k) = tmp(i,k) + dcm(i,k,kk)*uu(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          uu(i,k) = tmp(i,k)
        enddo
      enddo
c finish delta d in uu
c delta w 
      do k=1,levr+1
        do i=1,LNWAVS
          tmp1(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr
        do k=1,levr+1
          do i=1,LNWAVS
            tmp1(i,k) = tmp1(i,k) + x4m(k,kk) * uu(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          on(i,k) = on(i,k) - tmp1(i,k)
        enddo
      enddo
c
      do k=1,levr+1
        do i=1,LNWAVS
          tmp1(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr+1
        do k=1,levr+1
          do i=1,LNWAVS
            tmp1(i,k) = tmp1(i,k) + x3minv(k,kk) * on(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr+1
        do i=1,LNWAVS
          on(i,k) = tmp1(i,k)
        enddo
      enddo
c delta qs
      do i=1,LNWAVS
        tmp(i,1) = 0.0
      enddo
      do k=1,levr
        do i=1,LNWAVS
          tmp(i,1) = tmp(i,1) + dtx2 * sv(k) * uu(i,k)
        enddo
      enddo
      do i=1,LNWAVS
        z(i) = z(i) + tmp(i,1)
      enddo
c delta ts
cx    do k=1,levr
cx      do i=1,LNWAVS
cx        tmp(i,k) = 0.0
cx      enddo
cx    enddo
cx    do kk=1,levr
cx      do k=1,levr
cx        do i=1,LNWAVS
cx          tmp(i,k) = tmp(i,k) + dtx2*bm(k,kk)*uu(i,kk)
cx        enddo
cx      enddo
cx    enddo
cx    do k=1,levr
cx      do i=1,LNWAVS
cx        te(i,k) = te(i,k) + tmp(i,k)
cx      enddo
cx    enddo
c delta p
c delta t 
      do k=1,levr
        do i=1,LNWAVS
          tmp (i,k) = 0.0
          tmp1(i,k) = 0.0
        enddo
      enddo
      do kk=1,levr+1
        do k=1,levr
          do i=1,LNWAVS
            tmp (i,k) = tmp (i,k) + emn(k,kk)*on(i,kk)
            tmp1(i,k) = tmp1(i,k) + gmn(k,kk)*on(i,kk)
          enddo
        enddo
      enddo
      do kk=1,levr
        do k=1,levr
          do i=1,LNWAVS
            tmp (i,k) = tmp (i,k) - fmn(k,kk)*uu(i,kk)
            tmp1(i,k) = tmp1(i,k) - hmn(k,kk)*uu(i,kk)
          enddo
        enddo
      enddo
      do k=1,levr
        do i=1,LNWAVS
          pn(i,k) = pn(i,k) + dtx2 * tmp (i,k)
          tn(i,k) = tn(i,k) + dtx2 * tmp1(i,k)
        enddo
      enddo
c
c-------------------change dz forcing to uv---------
      call dz2uv(LNWAVS,uu,vv,tmp,tmp1,epsx,epsy,rnnp1,levr)
cmic$ do all
cmic$1 shared(tmp,tmp1,uu,vv)
cmic$1 private(i)
      do 870 k=1,levr
      do 870 i=1,LNWAVS
      uu(i,k) = tmp (i,k)
      vv(i,k) = tmp1(i,k)
 870  continue
#endif
c
      return
      end
