!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine rmakeoal05(var,ol,im,jm,imn,jmn)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      integer, allocatable :: zmax(:,:),zavg(:,:)
      dimension ol(im,jm,4)
      dimension ist(im),ien(im),jst(jm),jen(jm)
! ...................     SCC 05/27/05
!     dimension var(im,jm),oro(im,jm),xnsum(im,jm)
      dimension var(im,jm)
      real xnsumij
      real, allocatable :: oro(:,:)
      real, allocatable :: xnsum1(:,:),xnsum2(:,:)
      real, allocatable :: xnsum3(:,:),xnsum4(:,:)
!     dimension oro(im,jm)
!     dimension xnsum1(im,jm),xnsum2(im,jm)
!     dimension xnsum3(im,jm),xnsum4(im,jm)
! ...................
!
      allocate ( zavg(imn,jmn) )
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
      read(11,11) zavg
      rewind(11)
11    format(20i4)
      allocate ( oro(im,jm) )

      do j=1,jm1
         do i=1,im
            xnsumij = 0.0
            oro(i,j) = 0.0
            do j1=jst(j),jen(j)
               do i1=ist(i),ien(i)
                  xnsumij = xnsumij + 1
                  height = float(zavg(i1,j1))
                  if(height.lt.-990.) height = 0.0
                  oro(i,j) = oro(i,j) + height
               enddo
            enddo
            if(xnsumij.gt.0.) then
               oro(i,j) = oro(i,j) / xnsumij
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
      allocate ( zmax(imn,jmn) )
      read(13,13) zmax
      rewind(13)
13    format(20i4)
!
      do j=1,jm1        ! what for?????
      do i=1,im
         xnsumij = 0.0
         do j1=jst(j),jen(j)
         do i1=ist(i),ien(i)
           if(float(zmax(i1,j1)) .gt. oro(i,j)) xnsumij = xnsumij + 1
         enddo
         enddo
      enddo
      enddo
!
!---- count number of mode. higher than the hc, critical height
!     in a grid box
!
!     read(11,11) zavg
!     rewind(11)
!
      deallocate ( oro )
      allocate ( xnsum1(im,jm),xnsum2(im,jm) )
      allocate ( xnsum3(im,jm),xnsum4(im,jm) )
      do j=1,jm1
      do i=1,im
         xnsum1(i,j) = 0.0
         xnsum2(i,j) = 0.0
         xnsum3(i,j) = 0.0
         xnsum4(i,j) = 0.0
         hc = 1116.2 - 0.878 * var(i,j)
         do j1=jst(j),jen(j)
         do i1=ist(i),ien(i)
           if(float(zavg(i1,j1)) .gt. hc) xnsum1(i,j) = xnsum1(i,j) + 1
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
               if(float(zavg(i1,j1)) .gt. hc) xnsum3(i,j)=xnsum3(i,j)+1
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
      deallocate ( zavg ,zmax )
!
!---- calculate the 3d orographic subgrid orography fraction
!     for 4 wind directions
!     (kwd = 1  2  3  4)
!     ( wd = w  s sw nw)
!
!
      do kwd = 1, 4
      do j=1,jm-2
      do i=1,im
         ii = i + 1
         if (ii .gt. im) ii = ii - im
         if(kwd .eq. 1) then
            ol(ii,j+1,kwd) = (xnsum3(i,j+1)+xnsum3(ii,j+1))/		&
                              (xnsum4(i,j+1)+xnsum4(ii,j+1))
         else if(kwd .eq. 2) then
            ol(ii,j+1,kwd) = (xnsum3(ii,j)+xnsum3(ii,j+1))/		&
                              (xnsum4(ii,j)+xnsum4(ii,j+1))
         else if(kwd .eq. 3) then
            ol(ii,j+1,kwd) = (xnsum1(ii,j)+xnsum1(i,j+1))/		&
                              (xnsum2(ii,j)+xnsum2(i,j+1))
         else if(kwd .eq. 4) then
            ol(ii,j+1,kwd) = (xnsum1(i,j)+xnsum1(ii,j+1))/		&
                              (xnsum2(i,j)+xnsum2(ii,j+1))
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
      deallocate ( xnsum1,xnsum2 )
      deallocate ( xnsum3,xnsum4 )
      write(6,*) "! rmakeoal done"
!
      return
      end
