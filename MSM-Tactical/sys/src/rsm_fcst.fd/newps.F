      subroutine newps(idimt,kdimi,t,ps,hold,hnew,psnew,si)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  newps
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  compute a new surface pressure with new terrain height.
!
! program history log:
!
! usage:    call newps(idimt,kdimi,t,ps,hold,hnew,psnew,si)
!   input argument list:
!     idimt     - wave coefficent with dimension of (lonf22,lot)
!     kdimt     - the second dimension of anl and alf as well as 
!     t         - virtual temperature
!     ps        - surface pressure on surface hold
!     hold      - terrain height for ps. 
!     hnew      - terrain height for psnew.
!     si        -  sigma level (interface) values.
!
!   output argument list:
!     psnew     - surface pressure  on surface hnew
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
!
      use physcons, grav => con_g, rd => con_rd
!ccj
#include <paramodel.h>
!cc
         dimension t(idimt,kdimi),ps(idimt),psnew(idimt)
         dimension hold(idimt),hnew(idimt),si(kdimi+1)
         dimension tau(igrd1o*jgrd1o,levri)
         dimension rlsig(levri+1)
         dimension h(igrd1o*jgrd1o,levri+1)
         dimension a(igrd1o*jgrd1o)
         dimension absa(igrd1o*jgrd1o)
!--------
!-------- compute interface heights.
!--------
         print *,' ------ into newpsfc ---'
         rog = rd/grav
         do 5 k=1,kdimi
         rlsig(k) = - log(si(k))
         do 5 i=1,idimt
         tau(i,k) = t(i,k) * rog
5        continue
         rlsig(kdimi+1) = - log ( .005 )
         do 10 i=1,idimt
         psnew(i)= - ps(i)
         h(i,1)=hold(i)
10       continue
         do 100 k=1,kdimi
         do 100 i=1,idimt
           h(i,k+1)=h(i,k)+tau(i,k)*(rlsig(k+1)-rlsig(k))
100      continue
!--------
!-------- loop over layers, testing to see if new sfc pressure is
!-------- in layer, and obtaining it, if so.
!--------
         eps=1.e-1
         do 200 k=1,kdimi
!----------
!---------- compute lapse rate
!----------
           kp=min(k+1,kdimi)
           km=max(1,k-1)
           kppz=min(k+2,kdimi)
           kpz=kppz-1
           kmmz=max(1,k-2)
           kmz=kmmz+1
           do 2020 iq=1,idimt
           a(iq)=2.0e0*(tau(iq,kp)-tau(iq,km))/                       &
     &          (rlsig(kpz)+rlsig(kppz)-rlsig(kmz)-rlsig(kmmz))
           absa(iq)=abs(a(iq))
2020       continue
           do 2030 i=1,idimt
           if(hnew(i).ge.h(i,k).and.hnew(i).le.h(i,k+1).and.            &
     &        absa(i).gt.eps) psnew(i)=rlsig(k)                         &
     &                                -ps(i)+(sqrt(tau(i,k)**2+         &
     &        2.0e0*a(i)*(hnew(i)-h(i,k))) - tau(i,k) )/a(i)
           if(hnew(i).ge.h(i,k).and.hnew(i).le.h(i,k+1).and.            &
     &        absa(i).le.eps) psnew(i)=rlsig(k)                         &
     &                                -ps(i)+(hnew(i)-h(i,k))/tau(i,k)
2030       continue
200      continue
!--------
!-------- do points which fall below first layer (use fixed lapse rate
!--------  of 6.5 deg per km.
!--------
         gamma=6.5e-3
         gascon=rd
         g=grav
         c=gascon*gamma/g
         do 8010 iq=1,idimt
         a(iq)=tau(iq,1)*                                               &
     &       (1.e0-exp(c*(rlsig(1)-rlsig(2))))/(rlsig(1)-rlsig(2))
8010     continue
         do 8050 i=1,idimt
         if( hnew(i).le.h(i,1) ) psnew(i)=rlsig(1)                      &
     &                                   -ps(i)+(sqrt(tau(i,1)**2+      &
     &     2.e0*a(i)*(hnew(i)-h(i,1))) - tau(i,1))/a(i)
8050     continue
         icount = 0
         do 8100 i=1,idimt
         psnew(i) = - psnew(i)
!cc      if( psnew(i).eq.ps(i) ) icount = icount + 1
         if( psnew(i).eq.ps(i) ) then
              icount = icount + 1
         else
!         print *,'  gz gzold ps psnew ',hold(i),hnew(i),ps(i),psnew(i)
         endif
8100     continue
!
         write(6,300)icount
300      format(' number of surface pressure points not updated=',i6)
!
      return
      end
