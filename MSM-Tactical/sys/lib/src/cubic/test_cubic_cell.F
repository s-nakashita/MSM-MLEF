      program test_cubic_cell
!
! ========== save 1 ======
!
      implicit none
!  
      integer, parameter :: leni=64,leno=64
      real          massi(0:leni+1),masso(leno)
      real          loci(leni+1),vali(leni+1),slpi(leni+1)
      real          loco(leno+1),valo(leno+1)
      real          dqmono(leni+1),dqi,dqimax,dqimin,qilc,qimp,qmax,qmin
      real          chk0,chk1,chk2,chk3,alpha,betta,tau
      real          aa,bb,t,tt,ttt,qq
      logical       tf
      integer       i,k
      real          dx
      real*4        dx1,dx2,dy1,dy2,vx1,vx2,vy1,vy2
      real*4        x1,x2,x3,x4,y1,y2,y3,y4
      real*4        xx(leno+1),yy(leno+1)
!
!     call opngks
!
      dx = 1./leni
      do i=1,leni+1
       loci(i) = (i-1)*dx
      enddo
! integral mean value(k) between loci(k) and loci(k+1)
      k=1
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
      massi(33) = 2.5
      massi(34) = 2.5
      massi(35) = 3.0
      massi(36) = 2.5
      massi(37) = 1.8
      massi(38) = 1.5
      massi(39) = 1.4
      massi(40) = 1.1
      massi(41) = 3.0
      massi(42) = 1.8
      massi(43) = 2.5
      massi(44) = 2.5
      massi(45) = 3.0
      massi(46) = 2.5
      massi(47) = 1.8
      massi(48) = 1.5
      massi(49) = 1.4
      massi(50) = 1.1
      massi(51) = 3.0
      massi(52) = 1.8
      massi(53) = 2.5
      massi(54) = 2.5
      massi(55) = 3.0
      massi(56) = 2.5
      massi(57) = 1.8
      massi(58) = 1.5
      massi(59) = 1.4
      massi(60) = 1.1
      massi(61) = 3.0
      massi(62) = 1.8
      massi(63) = 2.5
      massi(64) = 2.5
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
      massi(12) = 289.0
      massi(13) = 288.0
      massi(14) = 287.0
      massi(15) = 286.0
      massi(16) = 285.0
      massi(17) = 284.0
      massi(18) = 283.0
      massi(19) = 282.0
      massi(20) = 281.0
      massi(21) = 280.0
      massi(22) = 279.0
      massi(23) = 278.0
      massi(24) = 277.0
      massi(25) = 276.0
      massi(26) = 275.0
      massi(27) = 274.0
      massi(28) = 273.0
      massi(29) = 272.0
      massi(30) = 271.0
      massi(31) = 270.0
      massi(32) = 271.0
      massi(33) = 272.0
      massi(34) = 273.0
      massi(35) = 274.0
      massi(36) = 275.0
      massi(37) = 276.0
      massi(38) = 277.0
      massi(39) = 278.0
      massi(40) = 279.0
      massi(41) = 280.0
      massi(42) = 281.0
      massi(43) = 282.0
      massi(44) = 283.0
      massi(45) = 284.0
      massi(46) = 285.0
      massi(47) = 286.0
      massi(48) = 287.0
      massi(49) = 288.0
      massi(50) = 287.0
      massi(51) = 286.0
      massi(52) = 285.0
      massi(53) = 284.0
      massi(54) = 283.0
      massi(55) = 282.0
      massi(56) = 281.0
      massi(57) = 280.0
      massi(58) = 279.0
      massi(59) = 278.0
      massi(60) = 277.0
      massi(61) = 276.0
      massi(62) = 275.0
      massi(63) = 274.0
      massi(64) = 273.0
      endif
!
!     dx1=0.0
!     dx2=0.9
!     dy1=0.1
!     dy2=0.9
!     vx1=-1.0
!     vx2=4.0
!     vy1=0.0
!     vy2=1.0
!     call set(dx1, dx2, dy1, dy2, vx1, vx2, vy1, vy2, 1)
!
!     x1=0.0
!     x4=0.0
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
!
      dx = 1./leno
      do i=1,leno+1
        loco(i) = (i-1)*dx
      enddo
!
! ----------------------------
! estimate values at interface
! ----------------------------
! compute monotonic element
      massi(0)=2.*massi(1)-massi(2)
      k=leni
      massi(k+1)=2.*massi(k)-massi(k-1)
      do i=1,leni
        dqi = 0.25*(massi(i+1)-massi(i-1))
        dqimax = max(massi(i-1),massi(i),massi(i+1)) - massi(i)
        dqimin = massi(i) - min(massi(i-1),massi(i),massi(i+1))
        dqmono(i) = sign( min( abs(dqimin), dqimin, dqimax ), dqi)
      enddo
! compute value at interface
      do i=2,leni
        vali(i)=0.5*(massi(i-1)+massi(i))+(dqmono(i-1)-dqmono(i))/3.0
      enddo
      vali(1)=0.5*(massi(1)+massi(0))
      k=leni
      vali(k+1)=0.5*(massi(k)+massi(k+1))
! adjust value at interface --- equal distance grid
!     do i=1,leni
!       vali(i)=massi(i)-sign(min(abs(2*dqmono(i)),
!    &                            abs(vali(i)-massi(i))),dqmono(i))
!     enddo
!     k=leni
!     vali(k+1)=massi(k)+sign(min(abs(2*dqmono(k)),
!    &                            abs(vali(k+1)-massi(k))),dqmono(k))
! adjust value at interface --- wave-resolution scheme 
!     do i=1,leni-2
!       qilc = massi(i)+3./2.*(dqmono(i+2)-dqmono(i))-dqmono(i)
!       qimp = massi(i)-2.*dqmono(i)
!       qmax = max(massi(i),qimp,qilc)
!       qmin = min(massi(i),qimp,qilc)
!       vali(i) = min(max(vali(i),qmin),qmax)
!     enddo
!     do i=leni-2,leni
!       qilc = massi(i)+3./2.*(dqmono(i)-dqmono(i-2))+dqmono(i)
!       qimp = massi(i)+2.*dqmono(i)
!       qmax = max(massi(i),qimp,qilc)
!       qmin = min(massi(i),qimp,qilc)
!       vali(i+1) = min(max(vali(i+1),qmin),qmax)
!     enddo
! --------------------------
! compute slope at interface
! --------------------------
      do i=2,leni
        slpi(i) = 2.*(massi(i)-massi(i-1)) - (vali(i+1)-vali(i-1))/2.0
      enddo
      slpi(1)=slpi(2)+12.*massi(1)-6.*(vali(1)+vali(2))
      k=leni
      slpi(k+1)=slpi(k)-12.*massi(k)+6.*(vali(k)+vali(k+1))
!     call cubic_hermite_prep(leni+1,loci,vali,slpi,1)
!
! check consistency for mass conservation
!
      do i=1,leni
        chk0=0.5*(vali(i)+vali(i+1))+(slpi(i)-slpi(i+1))/12.
        print *,' i=',i,' massi=',massi(i),' mass=',chk0
      enddo
! adjust monotone
!     do i=1,leni
!       if( vali(i).eq.vali(i+1) ) then
!         slpi(i)=0.0
!         slpi(i+1)=0.0
!       endif
!     enddo
!     do i=1,leni
!       if( vali(i).ne.vali(i+1) ) then
!         dqi=vali(i+1)-vali(i)
!         alpha=slpi(i)/dqi
!         betta=slpi(i+1)/dqi
!         chk0=alpha+betta-2
!         chk1=2*alpha+betta-3
!         chk2=alpha+2.*betta-3.
!         chk3=alpha-chk1*chk1/chk0/3.
!         tf=chk0.gt.0.0.and.chk1.gt.0.0.and.chk2.gt.0.0.and.chk3.lt.0.0
!         if( tf ) then
!           tau=3/sqrt(alpha*alpha+betta*betta)
!           slpi(i)=tau*alpha*dqi
!           slpi(i+1)=tau*betta*dqi
!         endif
!       endif
!     enddo
! ---------------
! check monotonic
! ---------------
      write(6,101) leni+1,vali(leni+1),slpi(leni+1)
 101  format('--level=',i2,' vali=',g13.6,'  slpi=',g13.6)
      do i=leni,1,-1
        dqi=vali(i+1)-vali(i)
        if( dqi.ne.0.0 ) then
          alpha=slpi(i)/dqi
          betta=slpi(i+1)/dqi
          chk0=alpha+betta-2
          chk1=2*alpha+betta-3
          chk2=alpha+2.*betta-3.
          chk3=alpha-chk1*chk1/chk0/3.
          tf=chk0.gt.0.0.and.chk1.gt.0.0.and.chk2.gt.0.0.and.chk3.lt.0.0
          if( tf ) then
            tau=3/sqrt(alpha*alpha+betta*betta)
            slpi(i)=tau*alpha*dqi
            slpi(i+1)=tau*betta*dqi
            print *,'***** adjust *****',
     &              ' slpi(i)=',slpi(i),' slpi(i+1)=',slpi(i+1)
          endif
        else
          chk0=0.0
          chk1=0.0
          chk2=0.0
          chk3=0.0
          tf=slpi(i).ne.0.0.or.slpi(i+1).ne.0.0
        endif
        write(6,102) i,massi(i),dqi,chk0,chk1,chk2,chk3,tf
        write(6,101) i,vali(i),slpi(i)
      enddo
 102  format('   layer=',i2,' valm=',g13.6,' dqi=',g13.6,
     &                               ' check ',4g10.3,i5)
!
! check further values
      print *,'==========================================='
      write(6,103) i,vali(leni+1)
 103  format('--level=',i2,' vali=',g13.6)
      do i=leni,1,-1
        aa=4*(vali(i+1)+2.*vali(i))+2.*slpi(i)-12.*massi(i)
        bb=-3.*vali(i+1)-9.*vali(i)-3.*slpi(i)+12.*massi(i)
        do k=10,1,-1
          t=(k-1)*0.1+0.05
          tt=t*t
          ttt=tt*t
!         qq=aa*ttt+bb*tt+slpi(i)*t+vali(i)
          qq=(2*ttt-3*tt+1)*vali(i)+(ttt-2*tt+t)*slpi(i)+
     &       (-2*ttt+3*tt)*vali(i+1)+(ttt-tt)*slpi(i+1)
          print *,'  qq= ',qq
        enddo
        write(6,103) i,vali(i)
      enddo
!     do i=1,leni+1
!       xx(i)=vali(i)
!       yy(i)=loci(i)
!       call pwrit(xx(i),yy(i),'X',1,2,0,0)
!       print *,' i loc val ',i,loci(i),vali(i)
!     enddo
!     call curve(xx,yy,leni+1)
!     call frame
!
!     call cubic_hermite_intp(leni+1,loci,vali,slpi,leno,loco,valo)
!     do i=1,leno+1
!       xx(i)=valo(i)
!       yy(i)=loco(i)
!     enddo
!     call curve(xx,yy,leno+1)
!     call frame
!
!     call clsgks
!
      stop
      end

