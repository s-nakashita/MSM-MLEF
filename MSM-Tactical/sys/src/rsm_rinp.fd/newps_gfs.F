      subroutine newps_gfs(idimt,kdimi,t,ps,z,hold,hnew,psnew,si)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  newps_gfs
!
! abstract:  compute a new surface pressure with new terrain height.
!
! program history log:
!
! usage:    call newps_gfs(idimt,kdimi,t,ps,hold,hnew,psnew,si)
!   input argument list:
!     idimt     - wave coefficent with dimension of (lonf22,lot)
!     kdimt     - the second dimension of anl and alf as well as 
!     t         - virtual temperature
!     ps        - surface pressure on surface hold
!     z         - geopotential height on full levels
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
         dimension z(idimt,kdimi)
         dimension hold(idimt),hnew(idimt),si(idimt,kdimi+1)
         dimension tau(igrd1o*jgrd1o,levri)
         dimension rlsig(idimt,levri+1)
         dimension h(igrd1o*jgrd1o,levri+1)
         dimension a(igrd1o*jgrd1o)
         dimension absa(igrd1o*jgrd1o)
!--------
!-------- compute interface heights.
!--------
         print *,' ------ into newpsfc gfs ---'
         rog = rd/grav
         do 5 i=1,idimt
         do 5 k=1,kdimi
         rlsig(i,k) = - log(si(i,k))
         tau(i,k) = t(i,k) * rog
5        continue
         rlsig(:,kdimi+1) = - log ( .005 )
         do 10 i=1,idimt
         psnew(i)= ps(i)
         h(i,1)=hold(i)
10       continue
         print *,'psnew=',psnew(128),'hnew=',hnew(128),'hold=',hold(128)
         do 50 i=1,idimt
         do 100 k=1,kdimi-1
           h(i,k+1)=0.5*(z(i,k)+z(i,k+1))
100      continue
           h(i,kdimi+1)=z(i,kdimi)
50       continue
         print *,'h(128,k)=',(h(128,k),k=1,kdimi)
         print *,'rlsig(128,k)=',(rlsig(128,k),k=1,kdimi)
         print *,'tau(128,k)=',(tau(128,k),k=1,kdimi)
!--------
!-------- loop over layers, testing to see if new sfc pressure is
!-------- in layer, and obtaining it, if so.
!--------
         gamma=6.5e-3
         !gamma=5.0e-3
         !gamma=1.0e-2
         gascon=rd
         g=grav
         c=g/gascon/gamma
         do 2030 i=1,idimt
           do 200 k=1,kdimi
           if(hnew(i).ge.h(i,k).and.hnew(i).le.h(i,k+1)) then
              psnew(i)=ps(i)-rlsig(i,k)+ &
        &  log(1.e0 - gamma*(hnew(i)-h(i,k))/t(i,k))*c
           endif
200        continue
           if(hnew(i).le.h(i,1)) then
              psnew(i)=ps(i)-rlsig(i,1)+ &
        &  log(1.e0 - gamma*(hnew(i)-h(i,1))/t(i,1))*c
           end if
2030     continue
         print *,'psnew(128,k)=',psnew(128)
         icount = 0
         do 8100 i=1,idimt
         if( psnew(i).gt.5.) print *,'psnew=',psnew(i),'j=',              &
     &     (i-1)/igrd1+1, 'i=',mod(i,igrd1)
         if( psnew(i).eq.ps(i) ) then
              icount = icount + 1
         else if( hnew(i).gt.4000. ) then
         print *,'  gzold gznew psold psnew ',hold(i),hnew(i),ps(i),psnew(i)
         endif
8100     continue
!
         write(6,300)icount
300      format(' number of surface pressure points not updated=',i6)
!
      return
      end
