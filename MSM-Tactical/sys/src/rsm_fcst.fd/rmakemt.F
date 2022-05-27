!cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rmakemt(oro,slm,var,var4,im,jm)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#include <paramodel.h>
!
      parameter(imn=360*60/mtnres,jmn=180*60/mtnres)
!
      integer zavg
      common/havg/ zavg(imn,jmn)
      integer zvar(imn,jmn),zslm(imn,jmn)
      dimension oro(im,jm),slm(im,jm),var(im,jm),var4(im,jm)
      dimension ist(im),ien(im),jst(jm),jen(jm)
      equivalence (zslm(1,1),zvar(1,1))
      logical flag
!
      im1 = im - 1
      jm1 = jm - 1
      delx  = 360./im       ! lat-lon  grid resolution
      delxn = 360./imn      ! mountain data resolution
!
!---- find the average of the modes in a grid box
!
      print*,' im,jm,imn,jmn ',im,jm,imn,jmn
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
!...first, averaged height
!
      read(11,11) zavg
      read(14,12) zslm
      rewind(11)
11    format(20i4)
12    format(80i1)
!
      do i=1,im
         do j=1,jm
            slm(i,j)  = 0.0
            oro(i,j)  = 0.0
            xnsum = 0.0
            xland = 0.0
            xwatr = 0.0
            xl1 = 0.0
            xs1 = 0.0
            do ii1=ist(i),ien(i)
               i1 = ii1
               if(ii1.le.0.) i1 = i1 + imn
               do j1=jst(j),jen(j)
                  xland = xland + float(zslm(i1,j1))
                  xwatr = xwatr + float(1-zslm(i1,j1))
                  xnsum = xnsum + 1.
                  height = float(zavg(i1,j1))
                  if(height.lt.-990.) height = 0.0
                  xl1 = xl1 + height * float(zslm(i1,j1))
                  xs1 = xs1 + height * float(1-zslm(i1,j1))
               enddo
            enddo
            if(xnsum.gt.1.) then
               slm(i,j) = float(nint(xland/xnsum))
               if(slm(i,j).ne.0.) then
                  oro(i,j)= xl1 / xland
               else
                  oro(i,j)= xs1 / xwatr
               endif
            endif
         enddo
      enddo
!
      read(12,11) zvar
!
      do i=1,im
         do j=1,jm
            var(i,j)  = 0.0
            var4(i,j) = 0.0
            xnsum = 0.0
            xw1 = 0.0
            xw2 = 0.0
            xv2 = 0.0
            xw4 = 0.0
            do ii1=ist(i),ien(i)
               i1 = ii1
               if(ii1.le.0.) i1 = i1 + imn
               do j1=jst(j),jen(j)
                  xnsum = xnsum + 1.
                  height = float(zavg(i1,j1))
                  if(height.lt.-990.) height = 0.0
                  xw1 = xw1 + height
                  xw2 = xw2 + height ** 2
                  xv2 = xv2 + float(zvar(i1,j1)) ** 2
               enddo
            enddo
            if(xnsum.gt.1.) then
               var(i,j)=sqrt(max((xv2+xw2)/xnsum-(xw1/xnsum)**2,0.))
               do ii1=ist(i),ien(i)
                  i1 = ii1
                  if(ii1.lt.0.) i1 = i1 + imn
                  do j1=jst(j),jen(j)
                     height = float(zavg(i1,j1))
                     if(height.lt.-990.) height = 0.0
                     xw4 = xw4 + (height-oro(i,j)) ** 4
                  enddo
               enddo
               if(var(i,j).gt.1.) then
                  var4(i,j) = xw4/xnsum/var(i,j) **4
               endif
            endif
         enddo
      enddo
      write(6,*) "! make oro slm var var4 done"
!
      return
      end
