      program test_cubic_hermite
!
!
      implicit none
!  
      integer, parameter :: leni=11,leno=41
      real          loci(leni),vali(leni),slpi(leni)
      real          loco(leno),valo(leno)
      integer       i,bc
      real          dx
      real*4        dx1,dx2,dy1,dy2,vx1,vx2,vy1,vy2
      real*4        x1,x2,x3,x4,y1,y2,y3,y4
      real*4        xx(leno+1),yy(leno+1)
!
      call opngks
!
      dx = 1./(leni-1.)
      do i=1,leni
       loci(i) = (i-1)*dx
      enddo
      vali(01) = 3.0
      vali(02) = 2.9
      vali(03) = 2.9
      vali(04) = 2.5
      vali(05) = 1.5
      vali(06) = 2.0
      vali(07) = 1.8
      vali(08) = 0.0
      vali(09) = 0.0
      vali(10) = 0.8
      vali(11) = 0.0
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
      do i=1,leni
        xx(i)=vali(i)
        yy(i)=loci(i)
        call pwrit(xx(i),yy(i),'X',1,2,0,0)
      enddo
      call curve(xx,yy,leni)
      call frame
!
      dx = 1./(leno-1.)
      do i=1,leno
        loco(i) = (i-1)*dx
      enddo
!
! bc=0 cyclic; bc=1 side boundary condition
      bc=1
      call cubic_hermite_prep(leni,loci,vali,slpi,bc)
      call cubic_hermite_intp(leni,loci,vali,slpi,leno,loco,valo)
      do i=1,leni
        xx(i)=vali(i)
        yy(i)=loci(i)
        call pwrit(xx(i),yy(i),'X',1,2,0,0)
      enddo
      do i=1,leno
        xx(i)=valo(i)
        yy(i)=loco(i)
      enddo
      call curve(xx,yy,leno)
      call frame
!
! bc=0 cyclic; bc=1 side boundary condition
      bc=1
      call cubic_hermite_prep_mono(leni,loci,vali,slpi,bc)
      call cubic_hermite_intp(leni,loci,vali,slpi,leno,loco,valo)
      do i=1,leni
        xx(i)=vali(i)
        yy(i)=loci(i)
        call pwrit(xx(i),yy(i),'X',1,2,0,0)
      enddo
      do i=1,leno
        xx(i)=valo(i)
        yy(i)=loco(i)
      enddo
      call curve(xx,yy,leno)
      call frame
!
      call clsgks
!
      stop
      end

