#include <define.h>
      subroutine nfidi(ug,vg,tg,rqg,dlam,dphi,udx,udy,vdx,vdy,          &
     &                 tdx,tdy,rqdx,rqdy,dlax,spdlat,                   &
     &                 png,tng,wng,		                        &! non
     &                 pndx,tndx,wndx,	                                &! non
     &                 pndy,tndy,wndy,	                                &! non
     &                 wngtopm,deltim,                                  &
     &                 dtbdt,                                           &
     &                 si,sl,p1,p2,am,xm2,xm2px,xm2py,                  &
     &                 corf,corf2,gzsdx,gzsdy,del,rdel2,                &
     &                 lonlens,igrd1s,levr,ntotal)
!
      use physcons, rd => con_rd, rocp => con_rocp, g => con_g ,        &
     &              cp => con_cp, cv => con_cv, rerth => con_rerth
!.....
! this dynamical code s using base field temperature tendency as
! the coordinate 'hydrostatic' temperature, but not surface pressure
! in this case, coordinate surface pressure is determined internally and
! coordinate temperature is determined externally as the same as base field.
!.....
!
      dimension dtbdt(igrd1s,levr)
      dimension   spdlat(levr),spd(igrd1s,levr),                        &
     &   tg(igrd1s,levr),tdx(igrd1s,levr),tdy(igrd1s,levr),             &
     &   ug(igrd1s,levr),udx(igrd1s,levr),udy(igrd1s,levr),             &
     &   vg(igrd1s,levr),vdx(igrd1s,levr),vdy(igrd1s,levr),             &
     &  rqg(igrd1s,levr,ntotal),                                        &
     & rqdx(igrd1s,levr,ntotal),                                        &
     & rqdy(igrd1s,levr,ntotal),                                        &
     &  png(igrd1s,levr),pndx(igrd1s,levr),pndy(igrd1s,levr),           &
     &  tng(igrd1s,levr),tndx(igrd1s,levr),tndy(igrd1s,levr),           &
     &  wng(igrd1s,levr+1),                                             &
     & wndx(igrd1s,levr+1),wndy(igrd1s,levr+1),                         &
     & dphi(igrd1s),dlam(igrd1s),wngtopm(igrd1s)                      
      dimension  dlax(igrd1s),                                          &
     &    b(igrd1s,levr),  a(igrd1s,levr),                              &
     &    p(igrd1s,levr),  t(igrd1s,levr), w(igrd1s,levr+1),            &
     &  tau(igrd1s,levr),rtg(igrd1s,levr,ntotal)                       
      dimension  p1(levr),p2(levr),si(levr+1),sl(levr),                 &
     &  am(levr,levr),xm2(igrd1s),xm2px(igrd1s),                        &
     &  xm2py(igrd1s),corf(igrd1s),corf2(igrd1s),del(levr),             &
     &  gzsdx(igrd1s),gzsdy(igrd1s),rdel2(levr+1)  
!
      dimension dg(igrd1s,levr), ek(igrd1s,levr),                       &
     &     cg (igrd1s,levr), db(igrd1s,levr),cb(igrd1s,levr),           &
     &     dgzdx(igrd1s,levr ),dgzdy(igrd1s,levr),                      &
#ifdef INTDTB
     &      dot(igrd1s,levr+1),                                         &
#endif
     &     sdot(igrd1s,levr+1),	                                        &! non
     &     dup(igrd1s,levr),dvp(igrd1s,levr),                           &
     &     dum(igrd1s,levr ),dvm(igrd1s,levr)
      real s2,umean,vmean,wmean
!
!       print *,'in nfidi, start'
      do k=1,levr
        do j=1,lonlens
          dgzdx(j,k) = gzsdx(j)
          dgzdy(j,k) = gzsdy(j)
          dg(j,k)=udx(j,k)+vdy(j,k)
          ek(j,k)=(ug(j,k)*ug(j,k)+vg(j,k)*vg(j,k))*0.5e0
          spd(j,k)=sqrt(2.*ek(j,k))*xm2(j)
!hmhj     spd(j,k)=sqrt(2.*ek(j,k))*xm2(j)*sqrt(xm2(j))
        enddo
      enddo
      do k=1,levr
        spdlat(k) = 0.0e0
        do j=1,lonlens
          spdlat(k) = max( spdlat(k),spd(j,k))
        enddo
      enddo
      do k=1,levr
        do kk=1,levr
          do j=1,lonlens
            dgzdx(j,k) = dgzdx(j,k) + tdx(j,kk) * am(k,kk)
            dgzdy(j,k) = dgzdy(j,k) + tdy(j,kk) * am(k,kk)
          enddo
        enddo
      enddo
!
! add nonhydrostatic effect to wind
!
      do j=1,lonlens
        dum(j,1)=0.e0
        dup(j,levr )=0.e0
      enddo
      do k=1,levr-1
        do j=1,lonlens
          dup(j,k  )=-si(k+1)*(png(j,k+1)-png(j,k))
          dum(j,k+1)=-si(k+1)*(png(j,k+1)-png(j,k))
        enddo
      enddo
      do k=1,levr
        do j=1,lonlens
          dvp(j,k)=(1.+tng(j,k)/tg(j,k))*                               &
     &             (1.+(dup(j,k)+dum(j,k))*rdel2(k))
        enddo
      enddo
!
! a=dv/dt  b=du/dt  nonhydrostatic effect adds to a and b
      do k=1,levr
        do j=1,lonlens
          a(j,k)= ug(j,k) * vdx(j,k) + vg(j,k) * vdy(j,k)
          b(j,k)= ug(j,k) * udx(j,k) + vg(j,k) * udy(j,k)
          a(j,k)=-a(j,k)*xm2(j)-ek(j,k)*xm2py(j)
          b(j,k)=-b(j,k)*xm2(j)-ek(j,k)*xm2px(j)
          a(j,k)=a(j,k) - dgzdy(j,k)*dvp(j,k)
          b(j,k)=b(j,k) - dgzdx(j,k)*dvp(j,k)
          a(j,k)=a(j,k) - rd*(tg(j,k)+tng(j,k))*(dphi(j)+pndy(j,k))
          b(j,k)=b(j,k) - rd*(tg(j,k)+tng(j,k))*(dlam(j)+pndx(j,k))
          wmean=0.5*(wng(j,k)+wng(j,k+1))
          a(j,k)=a(j,k) - corf(j)*ug(j,k)
          b(j,k)=b(j,k) + corf(j)*vg(j,k) - corf2(j)*wmean
          a(j,k)=a(j,k) - vg(j,k)*wmean/rerth
          b(j,k)=b(j,k) - ug(j,k)*wmean/rerth
        enddo
      enddo
!
! ---------- do hydrostatic base first ( tau and dlax ) ----
!
      do k=1,levr
        do j=1,lonlens
        cg(j,k)=ug(j,k)*dlam(j)+vg(j,k)*dphi(j)
        enddo
      enddo
!
      do j=1,lonlens
        db(j,1)=del(1)*dg(j,1)
        cb(j,1)=del(1)*cg(j,1)
      enddo
      do k=1,levr-1
        do j=1,lonlens
          db(j,k+1)=db(j,k)+del(k+1)*dg(j,k+1)
          cb(j,k+1)=cb(j,k)+del(k+1)*cg(j,k+1)
        enddo
      enddo
!
!   store forcing of lnp in dlax
!
      do j=1,lonlens
        dlax(j)= -(cb(j,levr)+db(j,levr))*xm2(j)
      enddo
!
!   sigma dot computed only at interior interfaces. dot=-(sigma dot)
!
      do j=1,lonlens
        dvm(j,1)=0.e0
        dum(j,1)=0.e0
        dvp(j,levr )=0.e0
        dup(j,levr )=0.e0
      enddo
!
#ifdef INTDTB
      do j=1,lonlens
        dot(j,1)=0.e0
        dot(j,levr+1)=0.e0
      enddo
! dot is sigma dot prime
      do k=1,levr-1
        do j=1,lonlens
        dot(j,k+1)=dot(j,k)+                                            &
     &                 del(k)*(db(j,levr)+cb(j,levr)-                   &
     &                 dg(j,k)-cg(j,k))*xm2(j)
        enddo
      enddo
!
      rk= rocp
      do k=1,levr
        do j=1,lonlens
          tau(j,k)= ug(j,k) * tdx(j,k) + vg(j,k) * tdy(j,k)
          tau(j,k)=-tau(j,k)*xm2(j)
          tau(j,k)=tau(j,k)                                             &
     &          +rk*tg(j,k)*xm2(j)*                                     &
     &          (cg(j,k)-cb(j,levr)-db(j,levr))
!t--------next should be consistent with nsicdf and ct options here
!t        tau(j,k)=tau(j,k)
!t   &          -rk*tg(j,k)*
!t   &          (dot(j,k+1)+dot(j,k))/2.0/sl(k)
        enddo
      enddo
!
      do k=1,levr-1
        do j=1,lonlens
        dup(j,k  )=p1(k  )*tg(j,k+1)-tg(j,k)
        dum(j,k+1)=tg(j,k+1)-p2(k+1)*tg(j,k)
!t      dup(j,k  )=tg(j,k+1)-tg(j,k)
!t      dum(j,k+1)=tg(j,k+1)-tg(j,k)
        enddo
      enddo
!
      do k=1,levr
        do j=1,lonlens
          tau(j,k)=tau(j,k)-                                            &
     &               (dot(j,k+1)*dup(j,k)+                              &
     &                dot(j,k  )*dum(j,k))*rdel2(k)
        enddo
      enddo
!
#else
      do k=1,levr
        do j=1,lonlens
          tau(j,k)=dtbdt(j,k)
        enddo
      enddo
#endif
!
! -------------------------------------------------------------------
!
! add nonhydrostatic prognostic after here
!
      do k=2,levr
        do j=1,lonlens
          ek(j,k)=(1.+0.5*(tng(j,k)/tg(j,k)+tng(j,k-1)/tg(j,k-1)))*      &   
     &           (1.-si(k)*(png(j,k)-png(j,k-1))*rdel2(k-1)*2.0)
        enddo
      enddo
!
! wng is full field input and make tng as full
!
      do k=1,levr
        do j=1,lonlens
          tng (j,k) = tng (j,k) + tg (j,k)
          tndx(j,k) = tndx(j,k) + tdx(j,k)
          tndy(j,k) = tndy(j,k) + tdy(j,k)
        enddo
      enddo
!      call maxmin(tg,lonlens,levr,1,levr,'in nfidi start, tg')
!      call maxmin(tng,lonlens,levr,1,levr,'in nfidi start, t tdcy')
!
! nonhydrostatic s dot      sdot=-(s dot)
!
      do k=1,levr
        do j=1,lonlens
          dup(j,k) = 0.0
          dvp(j,k)=xm2(j)*(ug(j,k)*dgzdx(j,k)+vg(j,k)*dgzdy(j,k))
        enddo
      enddo
      do k=1,levr
        do kk=1,levr
          do j=1,lonlens
            dup(j,k) = dup(j,k) + tau(j,kk) * am(k,kk)	! d gz / dt
          enddo
        enddo
      enddo
      do k=1,levr
        do j=1,lonlens
          dum(j,k)=dup(j,k)+dvp(j,k)
        enddo
      enddo
      do k=2,levr
        do j=1,lonlens
          dvp(j,k) = 0.5 * ( tg(j,k) + tg(j,k-1) )
          sdot(j,k) = - g * wng(j,k) + 0.5 * ( dum(j,k) + dum(j,k-1) )
          sdot(j,k) = - sdot(j,k) * si(k) / rd / dvp(j,k) 
        enddo
      enddo
!      call maxmin(wng,lonlens,levr,1,levr,'nfidi, wng')
!
      k=levr
      do j=1,lonlens
! bottom level condition
        sdot(j,1)=0.e0
        wng(j,1)=ug(j,1)*gzsdx(j)+vg(j,1)*gzsdy(j)
        wng(j,1)=wng(j,1)*xm2(j)/ g
! top level condition
        sdot(j,k+1)=0.e0
        wng(j,levr+1)=dum(j,levr)/ g
      enddo
!      call maxmin(sdot,lonlens,levr+1,1,levr+1,'nfidi, sdot')
!  ----------------------------------------------------------
      do j=1,lonlens
        dvm(j,1)=0.e0
        dum(j,1)=0.e0
        dvp(j,levr )=0.e0
        dup(j,levr )=0.e0
      enddo
!
! rq nonhydrostatic forcing
!
      do nc=1,ntotal
        do k=1,levr-1
          do j=1,lonlens
            dup(j,k  )=rqg(j,k+1,nc)-rqg(j,k,nc)
            dum(j,k+1)=rqg(j,k+1,nc)-rqg(j,k,nc)
          enddo
        enddo
        do k=1,levr
          do j=1,lonlens
            rtg(j,k,nc)= ug(j,k)*rqdx(j,k,nc)+vg(j,k)*rqdy(j,k,nc)
            rtg(j,k,nc)=-rtg(j,k,nc)*xm2(j)
            rqg(j,k,nc)=rtg(j,k,nc)-(sdot(j,k+1)*dup(j,k)+              &
     &                sdot(j,k  )*dum(j,k))*rdel2(k)
          enddo
        enddo
      enddo
!
! v and u nonhydrostatic forcing
!
      do k=1,levr-1
        do j=1,lonlens
          dvp(j,k  )=vg(j,k+1)-vg(j,k)
          dup(j,k  )=ug(j,k+1)-ug(j,k)
          dvm(j,k+1)=vg(j,k+1)-vg(j,k)
          dum(j,k+1)=ug(j,k+1)-ug(j,k)
        enddo
      enddo
      do k=1,levr
        do j=1,lonlens
          a (j,k)=a(j,k)                                                &
     &           -( sdot(j,k+1)*dvp(j,k)                                &
     &             +sdot(j,k  )*dvm(j,k) )*rdel2(k)
          b (j,k)=b(j,k)                                                &
     &           -( sdot(j,k+1)*dup(j,k)                                &
     &             +sdot(j,k  )*dum(j,k) )*rdel2(k)
        enddo
      enddo
!
! p nonhydrostatic forcing
!
      do k=1,levr-1
        do j=1,lonlens
          dvp(j,k  )=-si(k+1)*(vg(j,k+1)-vg(j,k))
          dup(j,k  )=-si(k+1)*(ug(j,k+1)-ug(j,k))
          dvm(j,k+1)=-si(k+1)*(vg(j,k+1)-vg(j,k))
          dum(j,k+1)=-si(k+1)*(ug(j,k+1)-ug(j,k))
        enddo
      enddo
      gama= cp / cv
      rk= rocp
      do k=1,levr
        do j=1,lonlens
          p(j,k)=(dup(j,k)+dum(j,k))*rdel2(k)*dgzdx(j,k)
!          print *,'in nfidi,1,p(j,k)=',p(j,k),'dup=',dup(j,k),'dum=',   &
!     &     dum(j,k),'rdel2=',rdel2(k),'dgzdx=',dgzdx(j,k)

          p(j,k)= p(j,k)+(dvp(j,k)+dvm(j,k))*rdel2(k)*dgzdy(j,k)
!          print *,'in nfidi,2,p(j,k)=',p(j,k),'dvp=',dvp(j,k),'dvm=',   &
!     &     dvm(j,k),'rdel2=',rdel2(k),'dgzdy=',dgzdy(j,k)

          p(j,k)= p(j,k) / rd / tg(j,k) + dg(j,k)
!          print *,'in nfidi,3,p(j,k)=',p(j,k),'dg=',dg(j,k)

          p(j,k)= p(j,k)*xm2(j)
!          print *,'in nfidi,4,p(j,k)=',p(j,k),'xm2=',xm2(j)

          p(j,k)= p(j,k)-                                               &
     &        g * sl(k)*(wng(j,k+1)-wng(j,k))/                          &
     &                   ( si(  k+1)- si(  k)) / rd /tg(j,k)
!          print *,'in nfidi,5,p(j,k)=',p(j,k),'wng=',wng(j,k),'wng=',    &
!     &     wng(j,k+1),'sl=',sl(k),'si=',si(k+1),si(k),'tg=',tg(j,k),     &
!     &     'rd=',rd,'g=',g

          p(j,k)= -gama*p(j,k)+0.5*(sdot(j,k+1)+sdot(j,k))/sl(k)
!          print *,'in nfidi,6,p(j,k)=',p(j,k),'sdot=',sdot(j,k+1),       &
!     &     sdot(j,k),'sl=',sl(k),'gama=',gama

          t(j,k)= rk*tng(j,k)*p(j,k)   ! pass full p without s to t
!          print *,'in nfidi,7,t(j,k)=',t(j,k),'tng=',tng(j,k+1),          &
!     &     'p=',p(j,k),'rk=',rk


!          call rmpsynall
!          call rmpabort
        enddo
      enddo
!      call maxmin(p,lonlens,levr,1,levr,'in nfidi, p tdcy')
!      call maxmin(t,lonlens,levr,1,levr,'in nfidi, t tdcy')
      do k=1,levr-1
        do j=1,lonlens
          dvp(j,k  )=png(j,k+1)-png(j,k)
          dvm(j,k+1)=png(j,k+1)-png(j,k)
        enddo
      enddo
      do k=1,levr
        do j=1,lonlens
          p(j,k)=p(j,k)-xm2(j)*                                         &
     &        (ug(j,k)*pndx(j,k)+vg(j,k)*pndy(j,k)+cg(j,k))
          p(j,k)=p(j,k)-                                                &
     &               (sdot(j,k+1)*dvp(j,k)+                             &
     &                sdot(j,k  )*dvm(j,k))*rdel2(k)
        enddo
      enddo
!
! t nonhydrostatic forcing
!
      do k=1,levr-1
        do j=1,lonlens
          dup(j,k  )=p1(k  )*tng(j,k+1)-tng(j,k)
          dum(j,k+1)=tng(j,k+1)-p2(k+1)*tng(j,k)
!t        dup(j,k  )=tng(j,k+1)-tng(j,k)
!t        dum(j,k+1)=tng(j,k+1)-tng(j,k)
        enddo
      enddo
      do k=1,levr
        do j=1,lonlens
          t(j,k)= t(j,k)-xm2(j)*( ug(j,k) * tndx(j,k)+                  &
     &                vg(j,k) * tndy(j,k) )
          t(j,k)=t(j,k)-(sdot(j,k+1)*dup(j,k)+                          &
     &                sdot(j,k  )*dum(j,k))*rdel2(k)
        enddo
      enddo
!
! w nonhydrostatic forcing
!
      do k=1,levr
        do j=1,lonlens
          dvm(j,k)= - 0.5 * ( sdot(j,k)+sdot(j,k+1) ) *                 &
     &            ( wng(j,k+1)-wng(j,k) )/( si(k+1) - si(k) )
        enddo
      enddo
      do k=2,levr
        do j=1,lonlens
          umean= 0.5 * (ug(j,k)+ug(j,k-1))
          vmean= 0.5 * (vg(j,k)+vg(j,k-1))
          s2=umean*umean+vmean*vmean
          dup(j,k)= umean * wndx(j,k)
          dvp(j,k)= vmean * wndy(j,k)
          dup(j,k)= dup(j,k)+dvp(j,k)
          dvp(j,k)= 0.5 * ( dvm(j,k)+dvm(j,k-1) )
          w(j,k)= -xm2(j)*dup(j,k)-dvp(j,k)
!          print *,'j=',j,'w(j,k) 1=',w(j,k),'xm2=',xm2(j)
          w(j,k)= w(j,k) - g *(1.0-ek(j,k))
!          print *,'j=',j,'w(j,k) 2=',w(j,k),'ek=',ek(j,k)
          w(j,k)= w(j,k) + xm2(j)*corf2(j)*umean
!          print *,'j=',j,'w(j,k) 3=',w(j,k),'corf=',corf2(j),'um=',umean
          w(j,k)= w(j,k) + xm2(j)*s2/rerth
!          print *,'j=',j,'w(j,k) 4=',w(j,k),'s2=',s2

        enddo
!        call rmpsynall
!        call rmpabort

      enddo
      k=levr
      do j=1,lonlens
        w(j,  1)= b(j,1)*gzsdx(j)+a(j,1)*gzsdy(j)
        w(j,  1)= w(j,  1)*xm2(j) / g
        w(j,k+1)=(wng(j,k+1)-wngtopm(j))/deltim
      enddo
! -----------------------------------------------------
! put tendency to variable, lnps and q have done this.
!
      do k=1,levr
        do j=1,lonlens
          ug(j,k) =   b(j,k)
          vg(j,k) =   a(j,k)
          tg(j,k) = tau(j,k)
          png(j,k)= p(j,k) - dlax(j)
          tng(j,k)= t(j,k) - tau(j,k)
        enddo
      enddo
      do k=1,levr+1
        do j=1,lonlens
          wng(j,k) = w(j,k)
        enddo
      enddo
!       print *,'in nfidi, end9'
!
      return
      end
