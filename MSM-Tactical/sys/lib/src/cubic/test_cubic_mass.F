      program test_cubic_mass
!
!
      implicit none
!  
      integer, parameter :: leni=10,leno=40
      real          massi(leni),masso(leno)
      real          loci(leni+1),loco(leno+1)
      real          vali(leni+1),valo(leno+1)
      integer       i
      real          dx
      real*4        dx1,dx2,dy1,dy2,vx1,vx2,vy1,vy2
      real*4        x1,x2,x3,x4,y1,y2,y3,y4
      real*4        xx(leno+1),yy(leno+1)
!
      call opngks
!
      dx = 1./leni
      do i=1,leni+1
       loci(i) = (i-1)*dx
      enddo
! integral mean value(k) between loci(k) and loci(k+1)
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
!
      dx1=0.0
      dx2=0.9
      dy1=0.1
      dy2=0.9
      vx1=-1.0
      vx2=4.0
      vy1=0.0
      vy2=1.0
      call set(dx1, dx2, dy1, dy2, vx1, vx2, vy1, vy2, 1)
!
      x1=0.0
      x4=0.0
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
      do i=1,leno+1
        loco(i) = (i-1)*dx
      enddo
!
      call cubic_mass_intp(leni,loci,massi,vali,
     &                     leno,loco,masso,valo)
      do i=1,leni+1
        xx(i)=vali(i)
        yy(i)=loci(i)
        call pwrit(xx(i),yy(i),'X',1,2,0,0)
        print *,' i loc val ',i,loci(i),vali(i)
      enddo
!     call curve(xx,yy,leni+1)
!     call frame
!
      call cubic_hermite_intp(leni+1,loci,vali,leno+1,loco,valo)
      do i=1,leno+1
        xx(i)=valo(i)
        yy(i)=loco(i)
      enddo
      call curve(xx,yy,leno+1)
      call frame
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
!     call curve(valo,loco,leno+1)
!     call frame
!
      call clsgks
!
      return
      end

