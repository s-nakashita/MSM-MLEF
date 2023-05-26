#include <define.h>
      subroutine rfidi(ug,vg,tg,rqg,dlam,dphi,udx,udy,vdx,vdy,           &    
     &                 tdx,tdy,rqdx,rqdy,dlax,spdlat,                    &
     &       p1,p2,am,xm2,xm2px,xm2py,corf,gzdx,gzdy,del,rdel2,          &
     &       lonlens,igrd1s,levr,levrm1,levrp1,ntotal                    &
#ifdef T
     &           ,at,bt,ct                                               &
#endif
#ifdef Q
     &           ,aq,bq                                                  &
#endif
#ifdef U
     &           ,au,bu,fu,pu                                            &
#endif
#ifdef V
     &           ,av,bv,fv,pv                                            &
#endif
#ifdef P
     &           ,ap,bp                                                  &
#endif
     &           )
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rfidi
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional dynamic computation (full tendency)
!
! program history log:
!
! usage:    call rfidi(ug,vg,tg,rqg,dlam,dphi,udx,udy,vdx,vdy,
!          1           tdx,tdy,rqdx,rqdy,dlax,
!          2           p1,p2,am,xm2,xm2px,xm2py,corf,gzdx,gzdy,del,rdel2)
!
!   input argument list:
!     ug        - full grid field of u
!     vg        - full grid field of v
!     tg        - full grid field of virtual temperaturev
!     rqg       - full grid field of specific humidity
!     dlam      - x-derivative of log (surface pressure) 
!     dphi      - y-derivative of log (surface pressure) 
!     udx       - x-derivative of u
!     udy       - y-derivative of u
!     vdx       - x-derivative of v
!     vdy       - y-derivative of v
!     tdx       - x-derivative of virtual temperature
!     tdy       - y-derivative of virtual temperature
!     rqdx      - x-derivative of specific humidity
!     rqdy      - y-derivative of specific humidity
!     p1        - 
!     p2        -
!     am        - hydrostatic vector to obtain geopotential height
!     xm2       - map facter square
!     xm2px     - x-derivative of map factor square
!     xm2py     - y-derivative of map factor square
!     corf      - coriolis force
!     gzdx      - x-derivative of terrain height
!     gzdy      - y-derivative of terrain height
!     del       - sigma layer thickness
!     rdel2     - inverse of sigma layer thickness
!
!   output argument list:
!     ug        - full tendency grid field of u
!     vg        - full tendency grid field of v
!     tg        - full tendency grid field of virtual temperaturev
!     rqg       - full tendency grid field of specific humidity
!     dlax      - full tendency grid field of log surface pressure
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
      use physcons, rocp => con_rocp, rd => con_rd
!.....
!.....developed by hann-ming henry juang (dec 1990, version 0)......
!..... this version of total forcing computation including
!..... linear and nonlinear in equations.
!.....
#ifdef T
      dimension at(igrd1s,levr)
      dimension bt(igrd1s,levr)
      dimension ct(igrd1s,levr)
#endif
#ifdef Q
      dimension aq(igrd1s,levr,ntotal)
      dimension bq(igrd1s,levr,ntotal)
#endif
#ifdef U
      dimension au(igrd1s,levr)
      dimension bu(igrd1s,levr)
      dimension fu(igrd1s,levr)
      dimension pu(igrd1s,levr)
#endif
#ifdef V
      dimension av(igrd1s,levr)
      dimension bv(igrd1s,levr)
      dimension fv(igrd1s,levr)
      dimension pv(igrd1s,levr)
#endif
#ifdef P
      dimension ap(igrd1s)
      dimension bp(igrd1s)
#endif
!
      dimension   spdlat(levr),                                          &  
     &   tg(igrd1s,levr),tdx(igrd1s,levr),tdy(igrd1s,levr),              &
     &   ug(igrd1s,levr),udx(igrd1s,levr),udy(igrd1s,levr),              &
     &   vg(igrd1s,levr),vdx(igrd1s,levr),vdy(igrd1s,levr),              &
     &   rqg(igrd1s,levr,ntotal),rqdx(igrd1s,levr,ntotal),               &
     &   rqdy(igrd1s,levr,ntotal),                                       &
     &   dphi(igrd1s),dlam(igrd1s)
      dimension  dlax(igrd1s),                                           &
     &    b(igrd1s,levr),  a(igrd1s,levr),                               &
     &  tau(igrd1s,levr),rtg(igrd1s,levr,ntotal)
      dimension  p1(levr),p2(levr),                                      &
     &  am(levr,levr),xm2(igrd1s),xm2px(igrd1s),                         &
     &  xm2py(igrd1s),corf(igrd1s),del(levr),                            &
     &  gzdx(igrd1s),gzdy(igrd1s),rdel2(levrp1)
!
      dimension dg(igrd1s,levr), ek(igrd1s,levr),spd(igrd1s,levr),       &
     &     cg (igrd1s,levr), db(igrd1s,levr),cb(igrd1s,levr),            &
     &     dot(igrd1s,levrp1),dup(igrd1s,levr),dvp(igrd1s,levr),         &
     &     dum(igrd1s,levr ),dvm(igrd1s,levr)
!
      dot=0.e0
      dvm=0.e0
      dum=0.e0
      dvp=0.e0
      dup=0.e0
      a=0
      b=0
      tau=0
      rtg=0
!
      do 20 k=1,levr
      do 20 j=1,lonlens
      dup(j,k) = 0.0e0
      dvp(j,k) = 0.0e0
      dg(j,k)=udx(j,k)+vdy(j,k)
      ek(j,k)=(ug(j,k)*ug(j,k)+vg(j,k)*vg(j,k))*0.5e0
!      spd(j,k)=sqrt(2.*ek(j,k))*xm2(j)*sqrt(xm2(j))
      spd(j,k)=sqrt(2.*ek(j,k))*xm2(j)
  20  continue
      do k=1,levr
      spdlat(k) = 0.0e0
      do j=1,lonlens
        spdlat(k) = max( spdlat(k),spd(j,k))
      enddo
      enddo
      do 50  k=1,levr
      do 50 kk=1,levr
      do 50 j=1,lonlens
      dvp(j,k) = dvp(j,k) + tdx(j,kk) * am(k,kk)
      dup(j,k) = dup(j,k) + tdy(j,kk) * am(k,kk)
  50  continue
!
! a=dv/dt  b=du/dt
      do 100 k=1,levr
      do 100 j=1,lonlens
      a(j,k)= ug(j,k) * vdx(j,k) + vg(j,k) * vdy(j,k)
      b(j,k)= ug(j,k) * udx(j,k) + vg(j,k) * udy(j,k)
      a(j,k)=-a(j,k)*xm2(j)-ek(j,k)*xm2py(j)
      b(j,k)=-b(j,k)*xm2(j)-ek(j,k)*xm2px(j)
#ifdef U
      au(j,k)=b(j,k)
#endif
#ifdef V
      av(j,k)=a(j,k)
#endif
       a(j,k)=a(j,k)-gzdy(j)-dup(j,k)- rd*tg(j,k)*dphi(j)
       b(j,k)=b(j,k)-gzdx(j)-dvp(j,k)- rd*tg(j,k)*dlam(j)
#ifdef U
      pu(j,k)=b(j,k)-au(j,k)
#endif
#ifdef V
      pv(j,k)=a(j,k)-av(j,k)
#endif
       a(j,k)=a(j,k) - corf(j)*ug(j,k)
       b(j,k)=b(j,k) + corf(j)*vg(j,k)
#ifdef U
      fu(j,k)=b(j,k)-au(j,k)-pu(j,k)
#endif
#ifdef V
      fv(j,k)=a(j,k)-av(j,k)-pv(j,k)
#endif
  100 continue

      do 120 k=1,levr
      do 120 j=1,lonlens
      tau(j,k)= ug(j,k) * tdx(j,k) + vg(j,k) * tdy(j,k)
      tau(j,k)=-tau(j,k)*xm2(j)
#ifdef T
       at(j,k)=tau(j,k)
#endif
  120 continue
!
      do 140 nc=1,ntotal
      do 140 k=1,levr
      do 140 j=1,lonlens
      rtg(j,k,nc)= ug(j,k) * rqdx(j,k,nc) + vg(j,k) * rqdy(j,k,nc)
      rtg(j,k,nc)=-rtg(j,k,nc)*xm2(j)
#ifdef Q
       aq(j,k,nc)=rtg(j,k,nc)
#endif
  140 continue
!
      do 180 k=1,levr
      do 180 j=1,lonlens
      cg(j,k)=ug(j,k)*dlam(j)+ vg(j,k)*dphi(j)
  180 continue
!
      rk= rocp
      do 190 j=1,lonlens
      db(j,1)=del(1)*dg(j,1)
      cb(j,1)=del(1)*cg(j,1)
  190 continue
      do 210 k=1,levrm1
      do 210 j=1,lonlens
      db(j,k+1)=db(j,k)+del(k+1)*dg(j,k+1)
      cb(j,k+1)=cb(j,k)+del(k+1)*cg(j,k+1)
  210 continue
!
!   store forcing of lnp in dlax
!
      do 220 j=1,lonlens
      dlax(j)= -(cb(j,levr)+db(j,levr))*xm2(j)
#ifdef P
      ap(j)=-cb(j,levr)*xm2(j)
      bp(j)=-db(j,levr)*xm2(j)
#endif
  220 continue
!
!   sigma dot computed only at interior interfaces.
!
      do 230 j=1,lonlens
      dot(j,1)=0.e0
      dvm(j,1)=0.e0
      dum(j,1)=0.e0
      dot(j,levrp1)=0.e0
      dvp(j,levr )=0.e0
      dup(j,levr )=0.e0
  230 continue
!
! dot is sigma dot prime
      do 240 k=1,levrm1
      do 240 j=1,lonlens
      dot(j,k+1)=dot(j,k)+                                               &  
     &                 del(k)*(db(j,levr)+cb(j,levr)-                    &
     &                 dg(j,k)-cg(j,k))*xm2(j)
  240 continue
!      call maxmin(dot,lonlens,levrm1,1,levrm1,'in rfidi,dot')
!
      do 250 k=1,levr
      do 250 j=1,lonlens
#ifdef T
       ct(j,k)=tau(j,k)
#endif
      tau(j,k)=tau(j,k)                                                  & 
     & +rk*tg(j,k)*xm2(j)*(cg(j,k)-cb(j,levr)-db(j,levr))
#ifdef T
       ct(j,k)=tau(j,k)-ct(j,k)
#endif
  250 continue
!
      do 260 k=1,levrm1
      do 260 j=1,lonlens
      dvp(j,k  )=vg(j,k+1)-vg(j,k)
      dup(j,k  )=ug(j,k+1)-ug(j,k)
      dvm(j,k+1)=vg(j,k+1)-vg(j,k)
      dum(j,k+1)=ug(j,k+1)-ug(j,k)
  260 continue
!ccccccccccccccccccccccccccccccccc

      do 270 k=1,levr
      do 270 j=1,lonlens
      vg(j,k)=a(j,k)                                                     &  
     &           -( dot(j,k+1)*dvp(j,k)                                  &
     &             +dot(j,k  )*dvm(j,k) )*rdel2(k)
      ug(j,k)=b(j,k)                                                     &
     &           -( dot(j,k+1)*dup(j,k)                                  &
     &             +dot(j,k  )*dum(j,k) )*rdel2(k)
#ifdef U
      bu(j,k)=ug(j,k)-b(j,k)
#endif
#ifdef V
      bv(j,k)=vg(j,k)-a(j,k)
#endif
  270 continue

      do 280 k=1,levrm1
      do 280 j=1,lonlens
      dup(j,k  )=p1(k  )*tg(j,k+1)-tg(j,k)
      dum(j,k+1)=tg(j,k+1)-p2(k+1)*tg(j,k)
  280 continue
!
      do 290 k=1,levr
      do 290 j=1,lonlens
      tg (j,k)=tau(j,k)                                                  &  
     &               -( dot(j,k+1)*dup(j,k)                              &
     &                 +dot(j,k  )*dum(j,k) )*rdel2(k)
#ifdef T
       bt(j,k)=tg(j,k)-tau(j,k)
#endif
  290 continue
!
      do nc=1,ntotal
      do 300 k=1,levrm1
      do 300 j=1,lonlens
      dup(j,k  )=rqg(j,k+1,nc)-rqg(j,k,nc)
      dum(j,k+1)=rqg(j,k+1,nc)-rqg(j,k,nc)
  300 continue
      do 310 j=1,lonlens
      dup(j,levr)=0.e0
  310 continue
      do 320 k=1,levr
      do 320 j=1,lonlens
      rqg(j,k,nc)=rtg(j,k,nc)                                            &  
     &               -( dot(j,k+1)*dup(j,k)                              &
     &                 +dot(j,k  )*dum(j,k) )*rdel2(k)
#ifdef Q
       bq(j,k,nc)=rqg(j,k,nc)-rtg(j,k,nc)
#endif
  320 continue
      enddo
!
      return
      end
