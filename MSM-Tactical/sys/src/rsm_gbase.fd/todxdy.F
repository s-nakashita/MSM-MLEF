      subroutine todxdy(a,ax,ay,im,jm,dx,dy)
      dimension a(im,jm),ax(im,jm),ay(im,jm)
      do j=1,jm
        jp1=min(jm,j+1)
        jm1=max( 1,j-1)
        do i=1,im
          ip1=min(im,i+1)
          im1=max( 1,i-1)
          ax(i,j)=(a(ip1,j)-a(im1,j))/dx/(ip1-im1)
          ay(i,j)=(a(i,jp1)-a(i,jm1))/dy/(jp1-jm1)
        enddo
      enddo
      return
      end
