      program test_cubic_cell
!
! oct 16, 2007
! consider monotone from cell-averaged value then find interface value 
! and interface first derivative
!
      implicit none
!  
      integer, parameter :: leni=32,leno=32
      real          massi(0:leni+1),masso(leno),dd(leni),dai(leni)
      real          loci(leni+1),hh(leni),slpi(leni+1)
      real          qim(leni),qip(leni),mim(leni),mip(leni)
      real          loco(leno+1),valo(leno+1)
      real          dqmono(leni+1),dqi,dqimax,dqimin,qilc,qimp,qmax,qmin
      real          chk0,cc,c1,c2,c3,alpha,betta,tau,delta
      real          aa,bb,t,tt,ttt,qq,hhp2,hhm2,h1,t1,t2,t3,t4
      real          ah,bh,ch,dh
      logical       tf
      integer       i,k,rc,mono,method
      real          dx,masssum,chk0sum
      real*4        dx1,dx2,dy1,dy2,vx0,vx1,vx2,vy1,vy2
      real*4        x1,x2,x3,x4,y1,y2,y3,y4,xc,yt
      real*4        xx(leno+1),yy(leno+1)
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
      k=1
      if( k.eq.1 ) then
      massi(01) = 3.0
      massi(02) = 1.8
      massi(03) = 2.1
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
      massi(23) = 2.2
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
      dx = 1./leno
      do i=1,leno
        loco(i) = (i-1)
        loco(i) = loco(i)+0.1*sin(loco(i)*3.14159)
        print *,' i=',i,' loci=',loci(i),' loco=',loco(i)
      enddo
      loco(leno+1)=loci(leni+1)
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
      method=1
!
! ----------------------------
! estimate values at interface
! ----------------------------
! (1) method one: follow Lin 2004
! prepare monotonicity
 
      if( method.eq.1 ) then
!
      massi(0)=(3.*hh(1)+hh(2))*massi(1)-2.*hh(1)*massi(2)
      massi(0)=massi(0)/(hh(1)+hh(2))
      massi(leni+1)=(3.*hh(leni)+hh(leni-1))*massi(leni)
     &             -2.*hh(leni)*massi(leni-1)
      massi(leni+1)=massi(leni+1)/(hh(leni)+hh(leni-1))
      do k=1,leni
        dqi = 0.25*(massi(k+1)-massi(k-1))
        dqimax = max(massi(k-1),massi(k),massi(k+1)) - massi(k)
        dqimin = massi(k) - min(massi(k-1),massi(k),massi(k+1))
        dqmono(k) = sign( min( abs(dqimin), dqimin, dqimax ), dqi)
      enddo
!
! compute value at interface with momotone
!
      do k=2,leni
        qim(k)=(massi(k-1)*hh(k)+massi(k)*hh(k-1))/(hh(k)+hh(k-1))
     &       +(dqmono(k-1)-dqmono(k))/3.0
      enddo
      do k=1,leni-1
        qip(k)=qim(k+1)
      enddo
      qim(1)=massi(1)
      qip(1)=massi(1)
      qim(leni)=massi(leni)
      qip(leni)=massi(leni)

      endif
      if( method.eq.2 ) then
  
! (2) method two: by cubic spline
! estimate point by hermite cubic spline and monotone
      do i=2,leni-1
        dd(i)=(massi(i+1)-massi(i))/(hh(i+1)+hh(i))
     &       +(massi(i)-massi(i-1))/(hh(i-1)+hh(i))
      enddo
      dd(1)=(massi(2)-massi(1))/(hh(1)+hh(2))*2.0
      dd(leni)=(massi(leni)-massi(leni-1))/(hh(leni)+hh(leni-1))*2.
!  consider hermite monotone
      do i=1,leni-1
        if( massi(i).eq.massi(i+1) ) then
          dd(i) = 0.0
          dd(i+1) = 0.0
        endif
      enddo
      do i=1,leni-1
        delta=(massi(i+1)-massi(i))/(hh(i+1)+hh(i))*2.0
        if( delta.ne.0.0 ) then
          alpha = dd(i)/delta
          betta = dd(i+1)/delta
          cc = alpha + betta - 2.0
          if( cc.gt.0.0 ) then
            c1 = 2.*alpha + betta - 3.0
            c2 = alpha + 2.*betta - 3.0
            c3 = alpha - c1*c1/cc/3.0
            if( c1.gt.0.0 .and. c2.gt.0.0 .and. c3.lt.0.0 ) then
              tau = 3.0/sqrt(alpha*alpha+betta*betta)
              dd(i) = tau * alpha * delta
              dd(i+1) = tau * betta * delta
            endif
          endif
        endif
      enddo
      do i=2,leni
        h1=0.5*(hh(i-1)+hh(i))
        t1=hh(i-1)/(hh(i-1)+hh(i))
        t2=t1*t1
        t3=t2*t1
        qim(i)=(2.*t3-3.*t2+1.)*massi(i-1)+(t3-2.*t2+t1)*h1*dd(i-1)
     &         +(-2.*t3+3.*t2)*massi(i)+(t3-t2)*h1*dd(i)
      enddo
      qim(1)=2.*massi(1)-qim(2)
      do k=1,leni-1
        qip(k)=qim(k+1)
      enddo
      qim(1)=massi(1)
      qip(1)=massi(1)
      qim(leni)=massi(leni)
      qip(leni)=massi(leni)
 
      endif
      if( method.eq.3 ) then

! (3) method three: follow ppm original method
! estimate point by integral with 4th order polynomial function
      do i=2,leni-1
        dai(i)=hh(i)/(hh(i-1)+hh(i)+hh(i+1))*
     &      ( (2.*hh(i-1)+hh(i))/(hh(i+1)+hh(i))
     &       *(massi(i+1)-massi(i))
     &      + (hh(i)+2*hh(i+1))/(hh(i-1)+hh(i))
     &       *(massi(i)-massi(i-1)) )
      enddo
      do i=2,leni-1
        if( (massi(i+1)-massi(i))*(massi(i)-massi(i-1)).gt.0.0 ) then
          dai(i)=sign(min(abs(dai(i)),2.*abs(massi(i)-massi(i-1)),
     &                      2.*abs(massi(i)-massi(i+1))),dai(i))
        else
          dai(i)=0.0
        endif
      enddo
      do i=2,leni-2
        qip(i)=massi(i)+hh(i)/(hh(i)+hh(i+1))*(massi(i+1)-massi(i))
     &        +1./(hh(i-1)+hh(i)+hh(i+1)+hh(i+2))
     &        *( 2.*hh(i+1)*hh(i)/(hh(i)+hh(i+1))
     &          *((hh(i-1)+hh(i))/(2.*hh(i)+hh(i+1))
     &           -(hh(i+2)+hh(i+1))/(hh(i)+2.*hh(i+1)))
     &          *(massi(i+1)-massi(i))
     &          -hh(i)*(hh(i-1)+hh(i))/(2.*hh(i)+hh(i+1))*dai(i+1)
     &          +hh(i+1)*(hh(i+1)+hh(i+2))/(hh(i)+2.*hh(i+1))*dai(i))
      enddo
      qip(1)=qip(2)
      qip(leni-1)=qip(leni-2)
      qip(leni)=qip(leni-1)
      do i=1,leni-1
        qim(i+1)=qip(i)
      enddo
      qim(1)=massi(1)

      endif

!===============================================
! get slope
      do i=2,leni
        hhp2=hh(i)*hh(i)
        hhm2=hh(i-1)*hh(i-1)
        slpi(i)=4.*(massi(i)/hhp2-massi(i-1)/hhm2)
     &          -(qim(i+1)+3.*qim(i))/hhp2
     &          +(3.*qim(i)+qim(i-1))/hhm2
        slpi(i)=slpi(i)/(1./hh(i)+1./hh(i-1))
      enddo
      slpi(1)=slpi(2)
      k=leni
      slpi(k+1)=slpi(k)
! prepare for monotone
      do i=1,leni
        mim(i)=slpi(i)
        mip(i)=slpi(i+1)
      enddo
      do i=1,leni-1
        qip(i)=qim(i+1)
      enddo
      qip(leni)=2.*massi(leni)-qim(leni)
! b c
      qim(1)=massi(1)
      qip(1)=massi(1)
      mim(1)=0.0
      mip(1)=0.0
      qim(leni)=massi(leni)
      qip(leni)=massi(leni)
      mim(leni)=0.0
      mip(leni)=0.0
!
      mono=0
!
      if( mono.eq.1 ) then
!
! check qi for monotonicity
      do i=1,leni
        c1=qip(i)-massi(i)
        c2=massi(i)-qim(i)
        if( c1*c2.le.0.0 ) then
          qim(i)=massi(i)
          qip(i)=massi(i)
        endif
      enddo
      do i=1,leni
!       c1=(qip(i)-qim(i))*(massi(i)-0.5*(qip(i)+qim(i)))
!       c2=(qip(i)-qim(i))*(qip(i)-qim(i))/6.
!       if( c1.gt.c2 ) then
!         qim(i)=3.*massi(i)-2.*qip(i)
!       else if( c1.lt.-c2 ) then
!         qip(i)=3.*massi(i)-2.*qim(i)
!       endif
        c1=(qip(i)-qim(i))*
     &     (massi(i)-0.5*(qip(i)+qim(i))-mim(i)*hh(i)/6.)
        c2=(qip(i)-qim(i))*(qip(i)-qim(i))/6.
        if( c1.gt.c2 ) then
          qim(i)=3.*massi(i)-2.*qip(i) - mim(i)*hh(i)/2.0
        else if( c1.lt.-c2 ) then
          qip(i)=3.*massi(i)-2.*qim(i) - mim(i)*hh(i)/2.0
        endif
      enddo
! modify slope for monotonicity
      do i=1,leni
        delta=(qip(i)-qim(i))/hh(i)
        if( qip(i).eq.qim(i) ) then
          mim(i) = 0.0
          mip(i) = 0.0
        endif
      enddo
      do i=1,leni
        delta=(qip(i)-qim(i))/hh(i)
        if( delta.ne.0.0 ) then
          alpha = mim(i)/delta
          betta = mip(i)/delta
          cc = alpha + betta - 2.0
          if( cc.gt.0.0 ) then
            c1 = 2.*alpha + betta - 3.0
            c2 = alpha + 2.*betta - 3.0
            c3 = alpha - c1*c1/cc/3.0
            if( c1.gt.0.0 .and. c2.gt.0.0 .and. c3.lt.0.0 ) then
              tau = 3.0/sqrt(alpha*alpha+betta*betta)
              mim(i) = tau * alpha * delta
              mip(i) = tau * betta * delta
            endif
          endif
        endif
      enddo
!
      endif
!
! check consistency for mass conservation
!
      masssum=0.0
      chk0sum=0.0
      do i=1,leni
        chk0=0.5*(qim(i)+qip(i))+(mim(i)-mip(i))/12.*hh(i)
        print *,' i=',i,' massi=',massi(i),' mass=',chk0
        masssum=masssum+massi(i)
        chk0sum=chk0sum+chk0
      enddo
      print *,' total sum  massi=',masssum,' chk0=',chk0sum
!
! ---------------
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
          qq=(8*ttt-9*tt+1)*qim(i)+(2*ttt-3*tt+t)*hh(i)*mim(i)+
     &       (4*ttt-3*tt)*qip(i)+(-12*ttt+12*tt)*massi(i)
!         qq=ah*ttt+bh*tt+ch*t+dh
          print *,'  qq= ',qq
          xx(k)=qq
          yy(k)=loci(i)+(loci(i+1)-loci(i))*t
        enddo
        call curve(xx,yy,33)
      enddo
      xc=0.5*(vx1+vx2)
      yt=1.1
      if( mono.eq.1 ) then
      call pwrit(xc,yt,'PCSM_mono',9,2,0,0,0)
      else
      call pwrit(xc,yt,'PCSM',4,2,0,0,0)
      endif
      call frame
!
!     do i=1,leni
!       xx(i)=massi(i)
!       yy(i)=0.5*(loci(i)+loci(i+1))
!     enddo
!     call curve(xx,yy,leni)
!     call frame
! test
!     loco(1:leno+1) = loci(1:leno+1)
 
!     do k=1,50
!     call vert_cell_cubic_intp(loci,massi(1),loco,masso,1,leno,rc)
!     print *,' return code from vert_cell_cubic_intp = ',rc
!     x1=vx0
!     x4=vx0
!     do i=1,leno
!       x2=masso(i)
!       x3=masso(i)
!       y1=loco(i)
!       y2=loco(i)
!       y3=loco(i+1)
!       y4=loco(i+1)
!       call line(x1,y1,x2,y2)
!       call line(x2,y2,x3,y3)
!       call line(x3,y3,x4,y4)
!       call line(x4,y4,x1,y1)
!     enddo
!     i=2*k-1
!     write(chr,'(i2.2)') i
!     y4=loco(20)
!     call pwrit(x4,y4,chr,2,2,0,0)
!     do i=1,leni
!       xx(i)=massi(i)
!       yy(i)=0.5*(loci(i)+loci(i+1))
!     enddo
!     call curve(xx,yy,leni)
!     do i=1,leno
!       xx(i)=masso(i)
!       yy(i)=0.5*(loco(i)+loco(i+1))
!     enddo
!     call curve(xx,yy,leno)
!     call frame
!     call vert_cell_cubic_intp(loco,masso,loci,massi(1),1,leno,rc)
!     print *,' return code from vert_cell_cubic_intp = ',rc
!     x1=vx0
!     x4=vx0
!     do i=1,leni
!       x2=massi(i)
!       x3=massi(i)
!       y1=loci(i)
!       y2=loci(i)
!       y3=loci(i+1)
!       y4=loci(i+1)
!       call line(x1,y1,x2,y2)
!       call line(x2,y2,x3,y3)
!       call line(x3,y3,x4,y4)
!       call line(x4,y4,x1,y1)
!     enddo
!     i=2*k-1
!     write(chr,'(i2.2)') i
!     y4=loco(20)
!     call pwrit(x4,y4,chr,2,2,0,0)
!     do i=1,leni
!       xx(i)=massi(i)
!       yy(i)=0.5*(loci(i)+loci(i+1))
!     enddo
!     call curve(xx,yy,leni)
!     do i=1,leno
!       xx(i)=masso(i)
!       yy(i)=0.5*(loco(i)+loco(i+1))
!     enddo
!     call curve(xx,yy,leno)
!     call frame
!     enddo
!
      call clsgks
!
      stop
      end

