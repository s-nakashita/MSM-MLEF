!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine rmakeoao05(oa4,elvmax,im,jm,imn,jmn)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      integer, allocatable :: zavg(:,:),zmax(:,:)
      dimension oa4(im,jm,4)
      dimension elvmax(im,jm)
      dimension ist(im),ien(im),jst(jm),jen(jm)
      real, allocatable :: oro(:,:),xnsum(:,:)
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
      allocate ( oro(im,jm),xnsum(im,jm) )
      allocate ( zavg(imn,jmn) )
      read(11,11) zavg
      rewind(11)
11    format(20i4)
      do j=1,jm1
         do i=1,im
            xnsum(i,j) = 0.0
            oro(i,j) = 0.0
            do j1=jst(j),jen(j)
               do i1=ist(i),ien(i)
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
      deallocate ( zavg )
!
!---- count number of max. peaks higher than the average mode
!     in a grid box
!
      allocate ( zmax(imn,jmn) )
      read(13,13) zmax
      rewind(13)
13    format(20i4)
!
!....     initialize elvmax to oro
!
      do j=1,jm
        do i=1,im
          elvmax(i,j) = oro(i,j)
        enddo
      enddo
!
      do j=1,jm1
      do i=1,im
         xnsum(i,j) = 0.0
         do j1=jst(j),jen(j)
         do i1=ist(i),ien(i)
           if(float(zmax(i1,j1)) .gt. oro(i,j)) xnsum(i,j)=xnsum(i,j)+1
         enddo
         enddo
      enddo
      enddo
!
!........
!      the max elev peak (no averaging)
!........
      do j=1,jm
      do i=1,im
         do ii1=ist(i),ien(i)
            i1 = ii1
            if(ii1.le.0.)  i1 = i1 + imn
         do j1=jst(j),jen(j)
            if ( elvmax(i,j) .lt. zmax(i1,j1)) elvmax(i,j)=zmax(i1,j1)
         enddo
         enddo
      enddo
      enddo
!
      deallocate ( zmax )
!
!---- calculate the 3d orographic asymmetry for 4 wind directions
!     (kwd = 1  2  3  4)
!     ( wd = w  s sw nw)
!
      do kwd = 1, 4
      do j=1,jm
      do i=1,im
         oa4(i,j,kwd) = 0.0
      enddo
      enddo
      enddo
!
      do kwd = 1, 4
      do j=1,jm-2
      do i=1,im
         ii = i + 1
         if (ii .gt. im) ii = ii - im
         if(kwd .eq. 1) then
            xnpu = xnsum(i,j) + xnsum(i,j+1)
            xnpd = xnsum(ii,j) + xnsum(ii,j+1)
         else if(kwd .eq. 2) then
            xnpu = xnsum(i,j+1) + xnsum(ii,j+1)
            xnpd = xnsum(i,j) + xnsum(ii,j)
         else if(kwd .eq. 3) then
            xnpu = xnsum(i,j+1) + ( xnsum(i,j) + xnsum(ii,j+1) )*0.5
            xnpd = xnsum(ii,j) + ( xnsum(i,j) + xnsum(ii,j+1) )*0.5
         else if(kwd .eq. 4) then
            xnpu = xnsum(i,j) + ( xnsum(ii,j) + xnsum(i,j+1) )*0.5
            xnpd = xnsum(ii,j+1) +( xnsum(ii,j) + xnsum(i,j+1) )*0.5
         endif
         if (xnpd .ne. xnpu) oa4(ii,j+1,kwd)=1.-xnpd/max(xnpu,1.)
      enddo
      enddo
      enddo
!
      do kwd=1,4
      do j=1,jm
      do i=1,im
         t = oa4(i,j,kwd)
         oa4(i,j,kwd) = sign( min( abs(t), 1. ), t )
      enddo
      enddo
      enddo
!
      deallocate ( oro,xnsum )
      write(6,*) "! rmakeoao done"
!
      return
      end
