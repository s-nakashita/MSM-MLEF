      program test_ccsm_cell
!
! oct 16, 2007
! consider monotone from cell-averaged value then find interface value 
! and interface first derivative
!
      implicit none
!  
      integer, parameter :: leni=32
      real          massi(0:leni+1),dd(leni)
      real          loci(leni+1),hh(leni),slpi(leni+1)
      real          qim(leni+1),qip(leni),mim(leni),mip(leni)
      real          cl(leni),cm(leni),cu(leni),rr(leni)
      real          dqmono(leni+1),dqi,dqimax,dqimin,qilc,qimp,qmax,qmin
      real          chk0,cc,c1,c2,c3,alpha,betta,tau,delta
      real          aa,bb,t,tt,ttt,qq,hhp2,hhm2,h1,t1,t2,t3,t4
      real          ah,bh,ch,dh,dai,daip1
      logical       tf
      integer       n,i,k,rc
      real          dx,masssum,chk0sum
      real*4        dx1,dx2,dy1,dy2,vx0,vx1,vx2,vy1,vy2
      real*4        x1,x2,x3,x4,y1,y2,y3,y4,xc,yt
      real*4        xx(leni+1),yy(leni+1)
      character*2   chr
!
      call opngks
!
      dx = 1./leni
      do i=1,leni+1
       loci(i) = (i-1)*dx
!      loci(i) = (i-1)
      enddo
! integral mean value(k) between loci(k) and loci(k+1)
      k=2
      if( k.eq.1 ) then
      massi(01) = 3.0
      massi(02) = 1.8
      massi(03) = 2.5
      massi(04) = 2.5
      massi(05) = 3.0
      massi(06) = 2.5
      massi(07) = 1.8
      massi(08) = 1.5
      massi(09) = 1.4
      massi(10) = 1.1
      massi(11) = 3.0
      massi(12) = 1.8
      massi(13) = 2.5
      massi(14) = 2.5
      massi(15) = 3.0
      massi(16) = 2.5
      massi(17) = 1.8
      massi(18) = 1.5
      massi(19) = 1.4
      massi(20) = 1.1
      massi(21) = 3.0
      massi(22) = 1.8
      massi(23) = 2.5
      massi(24) = 2.5
      massi(25) = 3.0
      massi(26) = 2.5
      massi(27) = 1.8
      massi(28) = 1.5
      massi(29) = 1.4
      massi(30) = 1.1
      massi(31) = 3.0
      massi(32) = 1.8
      endif
!
      if(k.eq.2) then
      massi(01) = 300.0
      massi(02) = 299.0
      massi(03) = 298.0
      massi(04) = 297.0
      massi(05) = 296.0
      massi(06) = 295.0
      massi(07) = 294.0
      massi(08) = 293.0
      massi(09) = 292.0
      massi(10) = 291.0
      massi(11) = 290.0
      massi(12) = 291.0
      massi(13) = 292.0
      massi(14) = 293.0
      massi(15) = 294.0
      massi(16) = 295.0
      massi(17) = 296.0
      massi(18) = 297.0
      massi(19) = 298.0
      massi(20) = 299.0
      massi(21) = 299.0
      massi(22) = 298.0
      massi(23) = 296.0
      massi(24) = 294.0
      massi(25) = 292.0
      massi(26) = 290.0
      massi(27) = 288.0
      massi(28) = 286.0
      massi(29) = 284.0
      massi(30) = 282.0
      massi(31) = 280.0
      massi(32) = 278.0
      endif
!
      if(k.eq.3) then
      massi(01) = 0.0
      massi(02) = 1.0
      massi(03) = 3.0
      massi(04) = 5.0
      massi(05) = 7.0
      massi(06) = 6.0
      massi(07) = 5.0
      massi(08) = 4.0
      massi(09) = 3.0
      massi(10) = 2.0
      massi(11) = 1.0
      massi(12) = 0.0
      massi(13) = 0.0
      massi(14) = 0.5
      massi(15) = 1.0
      massi(16) = 1.5
      massi(17) = 2.0
      massi(18) = 2.5
      massi(19) = 2.0
      massi(20) = 1.5
      massi(21) = 1.0
      massi(22) = 0.5
      massi(23) = 0.0
      massi(24) = 0.0
      massi(25) = 0.0
      massi(26) = 0.0
      massi(27) = 0.0
      massi(28) = 1.0
      massi(29) = 2.0
      massi(30) = 3.0
      massi(31) = 4.0
      massi(32) = 5.0
      endif
!
      if(k.eq.1) then
      dx1=0.1
      dx2=0.9
      dy1=0.1
      dy2=0.9
      vx0=0.0
      vx1=-1.0
      vx2=4.0
      vy1=0.0
      vy2=1.2
!     vy2=32.0
      call set(dx1, dx2, dy1, dy2, vx1, vx2, vy1, vy2, 1)
      endif
   
      if(k.eq.2) then
      dx1=0.1
      dx2=0.9
      dy1=0.1
      dy2=0.9
      vx0=260.0
      vx1=260.0
      vx2=310.0
      vy1=0.0
      vy2=1.2
!     vy2=32.0
      call set(dx1, dx2, dy1, dy2, vx1, vx2, vy1, vy2, 1)
      endif
 
      if(k.eq.3) then
      dx1=0.1
      dx2=0.9
      dy1=0.1
      dy2=0.9
      vx0=0.0
      vx1=-1.0
      vx2=8.0
      vy1=0.0
      vy2=1.2
!     vy2=32.0
      call set(dx1, dx2, dy1, dy2, vx1, vx2, vy1, vy2, 1)
      endif
   
      x1=vx0
      x4=vx0
      do i=1,leni
        x2=massi(i)
        x3=massi(i)
        y1=loci(i)
        y2=loci(i)
        y3=loci(i+1)
        y4=loci(i+1)
        call line(x1,y1,x2,y2)
        call line(x2,y2,x3,y3)
        call line(x3,y3,x4,y4)
        call line(x4,y4,x1,y1)
      enddo
!
!
! ---------------------------------------------
! compute monotonicity from cell-averaged value
! by using hermite cubic spline and its checker
! ---------------------------------------------
! cell width
      do i=1,leni
        hh(i)=loci(i+1)-loci(i)
      enddo
!
! -----------------------------------------
! tri_digonal solve the values at interface
! -----------------------------------------
! prepare coefficient
 
      i=2
      rr(1)=28.*(1/hh(2)+1./hh(1))*massi(2)/hh(2)**2
     &     +(8./hh(2)**2+4./hh(2)/hh(1)-4./hh(1)**2)*massi(1)/hh(1)
      cl(1)=0.0
      cm(1)=18./hh(2)**3+24./hh(2)**2/hh(1)
     &      +2./hh(2)/hh(1)**2-4./hh(1)**3
      cu(1)=10./hh(2)**3+12./hh(2)**2/hh(1)+2./hh(2)/hh(1)**2
      do i=3,leni-1
        rr(i-1)=8.*(massi(i)/hh(i)**2-massi(i-1)/hh(i-1)**2)
        cl(i-1)=-(1./hh(i)+3./hh(i-1))/hh(i-1)
        cm(i-1)=5.*(1/hh(i)**2-1/hh(i-1)**2)
        cu(i-1)=(3./hh(i)+1./hh(i-1))/hh(i)
      enddo
      n=leni
      rr(n-1)=-28.*(1/hh(n)+1./hh(n-1))*massi(n-1)/hh(n-1)**2
     &     -(8./hh(n-1)**2+4./hh(n-1)/hh(n)-4./hh(n)**2)*massi(n)/hh(n)
      cl(n-1)=-10./hh(n-1)**3-12./hh(n-1)**2/hh(n)-2./hh(n-1)/hh(n)**2
      cm(n-1)=-18./hh(n-1)**3-24./hh(n-1)**2/hh(n)
     &      -2./hh(n-1)/hh(n)**2+4./hh(n)**3
      cu(n-1)=0.0
!
      call tri_diag(1,n-1,cl,cm,cu,rr,yy)
    
      do i=2,leni
        print *,' i= ',i,' yy ',yy(i-1)
        qim(i)=yy(i-1)
      enddo
      alpha=3./hh(2)+2./hh(1)
      betta=4.*hh(1)/hh(2)**2*massi(2)+4.*(2./hh(2)+1./hh(1))*massi(1)
     &      -hh(1)/hh(2)**2*qim(3)
     &      -(3.*hh(1)/hh(2)**2+5./hh(2)+2./hh(1))*qim(2)
      qim(1)=betta/alpha
      n=leni
      alpha=3./hh(n-1)+2./hh(n)
      betta=4.*hh(n)/hh(n-1)**2*massi(n-1)
     &      +4.*(2./hh(n-1)+1./hh(n))*massi(n)
     &      -hh(n)/hh(n-1)**2*qim(n-1)
     &      -(3.*hh(n)/hh(n-1)**2+5./hh(n-1)+2./hh(n))*qim(n)
      qim(n+1)=betta/alpha
!
! compute all derivative value at interface
!
      do k=2,leni
        slpi(k)=(qim(k+1)-qim(k))/hh(k)+(qim(k)-qim(k-1))/hh(k-1)
        slpi(k)=0.5*slpi(k)
      enddo
! get slope in another way
!     do i=2,leni
!       hhp2=hh(i)*hh(i)
!       hhm2=hh(i-1)*hh(i-1)
!       slpi(i)=4.*(massi(i)/hhp2-massi(i-1)/hhm2)
!    &          -(qim(i+1)+3.*qim(i))/hhp2
!    &          +(3.*qim(i)+qim(i-1))/hhm2
!       slpi(i)=slpi(i)/(1./hh(i)+1./hh(i-1))
!     enddo
! natural condition
      slpi(1)=(4.*massi(1)-3.*qim(1)-qim(2))/hh(1)
! ==================================================
! check qi for monotonicity
!     do i=1,leni
!       c1=qip(i)-massi(i)
!       c2=massi(i)-qim(i)
!       if( c1*c2.le.0.0 ) then
!         qim(i)=massi(i)
!         qip(i)=massi(i)
!       endif
!     enddo
!     do i=1,leni
!       c1=(qip(i)-qim(i))*(massi(i)-0.5*(qip(i)+qim(i)))
!       c2=(qip(i)-qim(i))*(qip(i)-qim(i))/6.
!       if( c1.gt.c2 ) then
!         qim(i)=3.*massi(i)-2.*qip(i)
!       else if( c1.lt.-c2 ) then
!         qip(i)=3.*massi(i)-2.*qim(i)
!       endif
!     enddo
! modify slope for monotonicity
!     do i=1,leni
!       delta=(qip(i)-qim(i))/(hh(i+1)+hh(i))*2.0
!       if( delta.eq.0.0 ) then
!         mim(i) = 0.0
!         mip(i) = 0.0
!       endif
!     enddo
!     do i=1,leni
!       delta=(qip(i)-qim(i))/(hh(i+1)+hh(i))*2.0
!       if( delta.ne.0.0 ) then
!         alpha = mim(i)/delta
!         betta = mip(i)/delta
!         cc = alpha + betta - 2.0
!         if( cc.gt.0.0 ) then
!           c1 = 2.*alpha + betta - 3.0
!           c2 = alpha + 2.*betta - 3.0
!           c3 = alpha - c1*c1/cc/3.0
!           if( c1.gt.0.0 .and. c2.gt.0.0 .and. c3.lt.0.0 ) then
!             tau = 3.0/sqrt(alpha*alpha+betta*betta)
!             mim(i) = tau * alpha * delta
!             mip(i) = tau * betta * delta
!           endif
!         endif
!       endif
!     enddo
!
!
! check further values
   
      x1=vx0
      x4=vx0
      do i=1,leni
        x2=massi(i)
        x3=massi(i)
        y1=loci(i)
        y2=loci(i)
        y3=loci(i+1)
        y4=loci(i+1)
        call line(x1,y1,x2,y2)
        call line(x2,y2,x3,y3)
        call line(x3,y3,x4,y4)
        call line(x4,y4,x1,y1)
      enddo
!
      print *,'==========================================='
      write(6,103) i,qim(leni)
 103  format('--level=',i2,' qim=',g13.6)
      do i=leni,1,-1
        dh=qim(i)
        ch=hh(i)*slpi(i)
        bh=12.*massi(i)-3.*qip(i)-9.*qim(i)-3.*hh(i)*slpi(i)
        ah=4.*qip(i)+18.*qim(i)-12.*massi(i)+2.*hh(i)*slpi(i)
        do k=33,1,-1
          t=(k-1)*1./32.
          tt=t*t
          ttt=tt*t
!         qq=(2*ttt-3*tt+1)*qim(i)+(ttt-2*tt+t)*hh(i)*mim(i)+
!    &       (-2*ttt+3*tt)*qip(i)+(ttt-tt)*hh(i)*mip(i)
          qq=(8*ttt-9*tt+1)*qim(i)+(2*ttt-3*tt+t)*hh(i)*slpi(i)+
     &       (4*ttt-3*tt)*qim(i+1)+(-12*ttt+12*tt)*massi(i)
!         qq=ah*ttt+bh*tt+ch*t+dh
          print *,'  qq= ',qq
          xx(k)=qq
          yy(k)=loci(i)+(loci(i+1)-loci(i))*t
        enddo
        call curve(xx,yy,33)
      enddo
      xc=0.5*(vx1+vx2)
      yt=1.1
      call pwrit(xc,yt,'CCSM',4,2,0,0,0)
      call frame
!
      call clsgks
!
      stop
      end

