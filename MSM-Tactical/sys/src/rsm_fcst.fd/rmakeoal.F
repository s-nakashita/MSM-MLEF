!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rmakeoal(var,ol,im,jm)
#include <paramodel.h>
!
      parameter(imn=360*60/mtnres,jmn=180*60/mtnres)
!
      integer zavg
      common/havg/ zavg(imn,jmn)
      integer zmax(imn,jmn)
      dimension ol(im,jm,4)
      dimension ist(im),ien(im),jst(jm),jen(jm)
      dimension var(im,jm),oro(im,jm),xnsum(im,jm)
      dimension xnsum1(im,jm),xnsum2(im,jm)
      dimension xnsum3(im,jm),xnsum4(im,jm)
      equivalence (zavg(1,1),zmax(1,1))
!
      im1 = im - 1
      jm1 = jm - 1
      delx  = 360./im       ! gaussian grid resolution
      delxn = 360./imn      ! mountain data resolution
!
!---- find the average of the modes in a grid box
!
      faclon  = delx / delxn
      do i=1,im
         ist(i) = faclon * float(i-1) + faclon * 0.5
         ist(i) = ist(i) + 1
         ien(i) = faclon * float(i) + faclon * 0.5
!x         print*, ' i ist ien ',i,ist(i),ien(i)
      enddo
      do j=1,jm
         jst(j) = faclon * float(j-1) + faclon * 0.5
         jst(j) = jst(j) + 1
         jen(j) = faclon * float(j) + faclon * 0.5
!x         print*, ' j jst jen ',j,jst(j),jen(j)
      enddo
      ien(im) = min(ien(im),imn)
      jen(jm) = min(jen(jm),jmn)
!
      do i=1,im
         do j=1,jm1
            xnsum(i,j) = 0.0
            oro(i,j) = 0.0
            do i1=ist(i),ien(i)
               do j1=jst(j),jen(j)
                  xnsum(i,j) = xnsum(i,j) + 1
                  height = float(zavg(i1,j1))
                  if(height.lt.-990.) height = 0.0
                  oro(i,j) = oro(i,j) + height
               enddo
            enddo
            if(xnsum(i,j).gt.0.) then
               oro(i,j) = oro(i,j) / xnsum(i,j)
            else
               oro(i,j) = 0.0
            endif
         enddo
      enddo
!
      do i=1,im
         oro(i,jm) = 0.
      enddo
!
!---- count number of max. peaks higher than the average mode
!     in a grid box
!
      read(13,11) zmax
      rewind(13)
11    format(20i4)
!
      do i=1,im
      do j=1,jm1
         xnsum(i,j) = 0.0
         do i1=ist(i),ien(i)
         do j1=jst(j),jen(j)
               if(float(zmax(i1,j1)) .gt. oro(i,j))
     &            xnsum(i,j) = xnsum(i,j) + 1
         enddo
         enddo
      enddo
      enddo
!
!---- count number of mode. higher than the hc, critical height
!     in a grid box
!
      read(11,11) zavg
      rewind(11)
!
      do i=1,im
      do j=1,jm1
         xnsum1(i,j) = 0.0
         xnsum2(i,j) = 0.0
         xnsum3(i,j) = 0.0
         xnsum4(i,j) = 0.0
         hc = 1116.2 - 0.878 * var(i,j)
         do i1=ist(i),ien(i)
         do j1=jst(j),jen(j)
               if(float(zavg(i1,j1)) .gt. hc)                            &
     &            xnsum1(i,j) = xnsum1(i,j) + 1
               xnsum2(i,j) = xnsum2(i,j) + 1
         enddo
         enddo
!
         inci = nint((ien(i)-ist(i)) * 0.5)
         isttt = min(max(ist(i)-inci,1),imn)
         ieddd = min(max(ien(i)-inci,1),imn)
!
         incj = nint((jen(j)-jst(j)) * 0.5)
         jsttt = min(max(jst(j)-incj,1),jmn)
         jeddd = min(max(jen(j)-incj,1),jmn)
!
         do i1=isttt,ieddd
         do j1=jsttt,jeddd
               if(float(zavg(i1,j1)) .gt. hc)                            &  
     &            xnsum3(i,j) = xnsum3(i,j) + 1
               xnsum4(i,j) = xnsum4(i,j) + 1
         enddo
         enddo
!x         print*,' i j hc var ',i,j,hc,var(i,j)
!x         print*,'xnsum12 ',xnsum1(i,j),xnsum2(i,j)
!x         print*,'xnsum34 ',xnsum3(i,j),xnsum4(i,j)
      enddo
      enddo
      do i=1,im
         xnsum1(i,jm) = 0.
         xnsum3(i,jm) = 0.
      enddo
!
!
!---- calculate the 3d orographic subgrid orography fraction
!     for 4 wind directions
!     (kwd = 1  2  3  4)
!     ( wd = w  s sw nw)
!
!
      do i=1,im1
      do j=1,jm-2
      do kwd = 1, 4
         if(kwd .eq. 1) then
            ol(i+1,j+1,kwd) = (xnsum3(i,j+1)+xnsum3(i+1,j+1))/           &  
     &                        (xnsum4(i,j+1)+xnsum4(i+1,j+1))
         else if(kwd .eq. 2) then
            ol(i+1,j+1,kwd) = (xnsum3(i+1,j)+xnsum3(i+1,j+1))/           &
     &                        (xnsum4(i+1,j)+xnsum4(i+1,j+1))
         else if(kwd .eq. 3) then
            ol(i+1,j+1,kwd) = (xnsum1(i+1,j)+xnsum1(i,j+1))/             &
     &                        (xnsum2(i+1,j)+xnsum2(i,j+1))
         else if(kwd .eq. 4) then
            ol(i+1,j+1,kwd) = (xnsum1(i,j)+xnsum1(i+1,j+1))/             &
     &                        (xnsum2(i,j)+xnsum2(i+1,j+1))
         endif
      enddo
      enddo
      enddo
!
      do kwd = 1, 4
      do i=1,im
         ol(i,1,kwd) = ol(i,2,kwd)
         ol(i,jm,kwd) = ol(i,jm-1,kwd)
      enddo
      enddo
!
      do kwd = 1, 4
      do j=1,jm
         ol(1,j,kwd) = ol(2,j,kwd)
      enddo
      enddo
!
      write(6,*) "! makeoa2 exit"
!
      return
      end
