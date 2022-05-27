      subroutine shalcv(im,ix,km,dt,del,prsi,prsl,prslk,kuo,q,t,dpshc)
!
      use machine , only : kind_phys
      use physcons, grav => con_g, cp => con_cp, hvap => con_hvap        &  
     &,             rd => con_rd
      implicit none
!
!     include 'constant.h'
!
      integer              im, ix, km, kuo(im)
      real(kind=kind_phys) del(ix,km),   prsi(ix,km+1), prsl(ix,km),     &
     &                     prslk(ix,km),                                 &
     &                     q(ix,km),     t(ix,km),      dt, dpshc
!
!     locals
!
      real(kind=kind_phys) ck,    cpdt,   dmse,   dsdz1, dsdz2,          &
     &                     dsig,  dtodsl, dtodsu, eldq,  g,              &
     &                     gocp,  rtdls
!
      integer              k,k1,k2,kliftl,kliftu,kt,n2,i,iku,ik1,ik,ii
      integer              index2(im), klcl(im), kbot(im), ktop(im),kk   &
     &,                    ktopm(im)
!c
!  physical parameters
      parameter(g=grav, gocp=g/cp)
!  bounds of parcel origin
      parameter(kliftl=2,kliftu=2)
      logical   lshc(im)
      real(kind=kind_phys) q2(im*km),     t2(im*km),                     &
     &                     prsl2(im*km),  prslk2(im*km),                 &
     &                     al(im*(km-1)), ad(im*km), au(im*(km-1))
!-----------------------------------------------------------------------
!  compress fields to points with no deep convection
!  and moist static instability.
      do i=1,im
        lshc(i)=.false.
      enddo
      do k=1,km-1
        do i=1,im
          if(kuo(i).eq.0) then
            eldq    = hvap*(q(i,k)-q(i,k+1))
            cpdt    = cp*(t(i,k)-t(i,k+1))
            rtdls   = (prsl(i,k)-prsl(i,k+1)) /                          &  
     &                 prsi(i,k+1)*rd*0.5*(t(i,k)+t(i,k+1))
            dmse    = eldq+cpdt-rtdls
            lshc(i) = lshc(i).or.dmse.gt.0.
          endif
        enddo
      enddo
      n2 = 0
      do i=1,im
        if(lshc(i)) then
          n2         = n2 + 1
          index2(n2) = i
        endif
      enddo
      if(n2.eq.0) return
      do k=1,km
        kk = (k-1)*n2
        do i=1,n2
          ik         = kk + i
          ii         = index2(i)
          q2(ik)     = q(ii,k)
          t2(ik)     = t(ii,k)
          prsl2(ik)  = prsl(ii,k)
          prslk2(ik) = prslk(ii,k)
        enddo
      enddo
      do i=1,n2
        ktopm(i) = km
      enddo
      do k=2,km
        do i=1,n2
          ii = index2(i)
          if (prsi(ii,1)-prsi(ii,k) .le. dpshc) ktopm(i) = k
        enddo
      enddo
!-----------------------------------------------------------------------
!  compute moist adiabat and determine limits of shallow convection.
!  check for moist static instability again within cloud.
      call mstadbt3(n2,km-1,kliftl,kliftu,prsl2,prslk2,t2,q2,            &  
     &            klcl,kbot,ktop,al,au)
      do i=1,n2
        kbot(i) = min(klcl(i)-1, ktopm(i)-1)
        ktop(i) = min(ktop(i)+1, ktopm(i))
        lshc(i) = .false.
      enddo
      do k=1,km-1
        kk = (k-1)*n2
        do i=1,n2
          if(k.ge.kbot(i).and.k.lt.ktop(i)) then
            ik      = kk + i
            iku     = ik + n2
            eldq    = hvap * (q2(ik)-q2(iku))
            cpdt    = cp   * (t2(ik)-t2(iku))
            rtdls   = (prsl2(ik)-prsl2(iku)) /                           &  
     &                 prsi(index2(i),k+1)*rd*0.5*(t2(ik)+t2(iku))
            dmse    = eldq + cpdt - rtdls
            lshc(i) = lshc(i).or.dmse.gt.0.
            au(ik)  = g/rtdls
          endif
        enddo
      enddo
      k1=km+1
      k2=0
      do i=1,n2
        if(.not.lshc(i)) then
          kbot(i) = km+1
          ktop(i) = 0
        endif
        k1 = min(k1,kbot(i))
        k2 = max(k2,ktop(i))
      enddo
      kt = k2-k1+1
      if(kt.lt.2) return
!-----------------------------------------------------------------------
!  set eddy viscosity coefficient cku at sigma interfaces.
!  compute diagonals and rhs for tridiagonal matrix solver.
!  expand final fields.
      kk = (k1-1) * n2
      do i=1,n2
        ik     = kk + i
        ad(ik) = 1.
      enddo
!
!     dtodsu=dt/del(k1)
      do k=k1,k2-1
!       dtodsl=dtodsu
!       dtodsu=   dt/del(k+1)
!       dsig=sl(k)-sl(k+1)
        kk = (k-1) * n2
        do i=1,n2
          ii     = index2(i)
          dtodsl = dt/del(ii,k)
          dtodsu = dt/del(ii,k+1)
          dsig   = prsl(ii,k) - prsl(ii,k+1)
          ik     = kk + i
          iku    = ik + n2
          if(k.eq.kbot(i)) then
            ck=1.5
          elseif(k.eq.ktop(i)-1) then
            ck=1.
          elseif(k.eq.ktop(i)-2) then
            ck=3.
          elseif(k.gt.kbot(i).and.k.lt.ktop(i)-2) then
            ck=5.
          else
            ck=0.
          endif
          dsdz1   = ck*dsig*au(ik)*gocp
          dsdz2   = ck*dsig*au(ik)*au(ik)
          au(ik)  = -dtodsl*dsdz2
          al(ik)  = -dtodsu*dsdz2
          ad(ik)  = ad(ik)-au(ik)
          ad(iku) = 1.-al(ik)
          t2(ik)  = t2(ik)+dtodsl*dsdz1
          t2(iku) = t2(iku)-dtodsu*dsdz1
        enddo
      enddo
      ik1=(k1-1)*n2+1
      call tridi2t3(n2,kt,al(ik1),ad(ik1),au(ik1),q2(ik1),t2(ik1),       &  
     &                                  au(ik1),q2(ik1),t2(ik1))
      do k=k1,k2
        kk = (k-1)*n2
        do i=1,n2
          ik = kk + i
          q(index2(i),k) = q2(ik)
          t(index2(i),k) = t2(ik)
        enddo
      enddo
!-----------------------------------------------------------------------
      return
      end
