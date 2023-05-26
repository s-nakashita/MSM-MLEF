!cccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rmakeoao(oa4,im,jm)
!cccccccccccccccccccccccccccccccccccccccccccccccccccc
#include <paramodel.h>
!
      parameter(imn=360*60/mtnres,jmn=180*60/mtnres)
!
      integer zavg
      common/havg/ zavg(imn,jmn)
      integer zmax(imn,jmn)
      dimension oa4(im,jm,4)
      dimension ist(im),ien(im),jst(jm),jen(jm)
      dimension oro(im,jm),xnsum(im,jm)
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
11    format(20i4)
!
      do i=1,im
      do j=1,jm1
         xnsum(i,j) = 0.0
         do i1=ist(i),ien(i)
         do j1=jst(j),jen(j)
               if(float(zmax(i1,j1)) .gt. oro(i,j))                      &  
     &            xnsum(i,j) = xnsum(i,j) + 1
         enddo
         enddo
      enddo
      enddo
!
!
!---- calculate the 3d orographic asymmetry for 4 wind directions
!     (kwd = 1  2  3  4)
!     ( wd = w  s sw nw)
!
      do kwd = 1, 4
      do i=1,im
         oa4(i,1,kwd) = 0.0
         oa4(i,jm,kwd) = 0.0
      enddo
      enddo
!
      do i=1,im1
      do j=1,jm-2
      do kwd = 1, 4
         if(kwd .eq. 1) then
            xnpu = xnsum(i,j) + xnsum(i,j+1)
            xnpd = xnsum(i+1,j) + xnsum(i+1,j+1)
         else if(kwd .eq. 2) then
            xnpu = xnsum(i,j+1) + xnsum(i+1,j+1)
            xnpd = xnsum(i,j) + xnsum(i+1,j)
         else if(kwd .eq. 3) then
            xnpu = xnsum(i,j+1) + ( xnsum(i,j) + xnsum(i+1,j+1) )*0.5
            xnpd = xnsum(i+1,j) + ( xnsum(i,j) + xnsum(i+1,j+1) )*0.5
         else if(kwd .eq. 4) then
            xnpu = xnsum(i,j) + ( xnsum(i+1,j) + xnsum(i,j+1) )*0.5
            xnpd = xnsum(i+1,j+1) +( xnsum(i+1,j) + xnsum(i,j+1) )*0.5
         endif
         oa4(i+1,j+1,kwd) = 1. - xnpd / max(xnpu , 1.)
         if(xnpd.eq.xnpu) oa4(i+1,j+1,kwd) = 0.
      enddo
      enddo
      enddo
!
      do kwd = 1, 4
      do j=1,jm1
         oa4(1,j,kwd) = oa4(2,j,kwd)
      enddo
      enddo
!
      do i=1,im
      do j=1,jm
      do kwd=1,4
         t = oa4(i,j,kwd)
         oa4(i,j,kwd) = sign( min( abs(t), 1. ), t )
      enddo
      enddo
      enddo
!
      write(6,*) "! makeoa1 exit"
!
      return
      end
