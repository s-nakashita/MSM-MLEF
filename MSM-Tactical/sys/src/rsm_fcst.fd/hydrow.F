      subroutine hydrow(ug,vg,tg,dlam,dphi,udx,udy,vdx,vdy,              &    
     &                 tdx,tdy,ww,                                       &
     &                 si,sl,p1,p2,am,xm2,                               &
     &                 gzdx,gzdy,del,rdel2,                              &
     &                 lonlens,igrd1s,levr,levrp1,levrm1)
!.....
!.....developed by hann-ming henry juang (dec 1990, version 0)......
!..... this version of total forcing computation including
!..... linear and nonlinear in equations.
!.....
!
      use physcons, rocp => con_rocp, rd => con_rd, g => con_g
!
      dimension                                                          &
     &   tg(igrd1s,levr),tdx(igrd1s,levr),tdy(igrd1s,levr),              &
     &   ug(igrd1s,levr),udx(igrd1s,levr),udy(igrd1s,levr),              &
     &   vg(igrd1s,levr),vdx(igrd1s,levr),vdy(igrd1s,levr),              &
     & dphi(igrd1s),dlam(igrd1s),ww(igrd1s,levrp1)
      dimension  tau(igrd1s,levr)
      dimension  si(levr+1),sl(levr)
      dimension  p1(levr),p2(levr),                                      &
     &  am(levr,levr),xm2(igrd1s),del(levr),                             &
     &  gzdx(igrd1s),gzdy(igrd1s),rdel2(levrp1)
!
      dimension dg(igrd1s,levr),                                         &   
     &     cg (igrd1s,levr), db(igrd1s,levr),cb(igrd1s,levr),            &
     &     dot(igrd1s,levrp1),dup(igrd1s,levr),                          &
     &     dum(igrd1s,levr )
!
      do 180 k=1,levr
      do 180 j=1,lonlens
      cg(j,k)=ug(j,k)*dlam(j)+vg(j,k)*dphi(j)
      dg(j,k)=udx(j,k)+vdy(j,k)
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
!   sigma dot computed only at interior interfaces.
!
      do j=1,lonlens
        dum(j,1)=0.e0
        dot(j,1)=0.e0
        dot(j,levrp1)=0.e0
        dup(j,levr  )=0.e0
      enddo
!
! dot is sigma dot prime (=-sigma dot)
      do 240 k=1,levrm1
      do 240 j=1,lonlens
      dot(j,k+1)=dot(j,k)+                                               &   
     &                 del(k)*(db(j,levr)+cb(j,levr)-                    &
     &                 dg(j,k)-cg(j,k))*xm2(j)
  240 continue
!      call maxmin(dot,lonlens,levrm1,1,levrm1,'in rfidi,dot,hydrow')
!
      do 250 k=1,levr
      do 250 j=1,lonlens
      tau(j,k)= ug(j,k) * tdx(j,k) + vg(j,k) * tdy(j,k)
      tau(j,k)=-tau(j,k)*xm2(j)
      tau(j,k)=tau(j,k)                                                  &
     & +rk*tg(j,k)*xm2(j)*(cg(j,k)-cb(j,levr)-db(j,levr))
  250 continue
!      call maxmin(tau,lonlens,levrm1,1,levrm1,'in rfidi,tau,hydrow')
!
      do 280 k=1,levrm1
      do 280 j=1,lonlens
      dup(j,k  )=p1(k  )*tg(j,k+1)-tg(j,k)
      dum(j,k+1)=tg(j,k+1)-p2(k+1)*tg(j,k)
  280 continue
!
      do 290 k=1,levr
      do 290 j=1,lonlens
      tau(j,k)=tau(j,k)-                                                 &
     &               (dot(j,k+1)*dup(j,k)+                               &
     &                dot(j,k  )*dum(j,k))*rdel2(k)
  290 continue
!
      do 300 k=1,levr
      do 300 j=1,lonlens
      dup(j,k) = gzdx(j)
      dum(j,k) = gzdy(j)
  300 continue
      do 320 kk=1,levr
      do 320  k=1,levr
      do 320 j=1,lonlens
      dup(j,k) = dup(j,k) + tdx(j,kk) * am(k,kk)
      dum(j,k) = dum(j,k) + tdy(j,kk) * am(k,kk)
  320 continue
      do 350 k=1,levr
      do 350 j=1,lonlens
      dum(j,k)=ug(j,k)*dup(j,k)+vg(j,k)*dum(j,k)
 350  continue
!
      do 395  k=1,levr
      do 395 j=1,lonlens
      dup(j,k)=0.0
 395  continue
      do 400 kk=1,levr
      do 400  k=1,levr
      do 400 j=1,lonlens
      dup(j,k) = dup(j,k) + tau(j,kk) * am(k,kk)
 400  continue
!
      do 450 k=1,levr
      do 450 j=1,lonlens
      dup(j,k)=dup(j,k)+xm2(j)*dum(j,k)
 450  continue
!      call maxmin(tau,lonlens,levrm1,1,levrm1,'in rfidi,dup,hydrow')
!
      do 500 k=2,levr
      do 500 j=1,lonlens
      ww(j,k)= rd * dot(j,k) / si(k)
      ww(j,k)= ww(j,k)*(tg(j,k)+tg(j,k-1))+(dup(j,k)+dup(j,k-1))
      ww(j,k)=0.5*ww(j,k)/ g
 500  continue
!
      k=levr
      do 550 j=1,lonlens
      ww(j,  1)=xm2(j)*dum(j,1)/ g
!     ww(j,k+1)=xm2(j)*dum(j,k)/ g
      ww(j,k+1)=dup(j,k)/ g
 550  continue
!testjun

!     ww=0.
!
!      call maxmin(ww,lonlens,levr+1,1,levr+1,' ww at 0 ')
!
      return
      end
