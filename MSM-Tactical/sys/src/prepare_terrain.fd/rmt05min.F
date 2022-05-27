      subroutine rmt05min(imr,jmr,iml,jml,rlat,rlon,			&
                          rslm,roro,rvar,rvar4,roa,rol,			&
                          rtheta,rgamma,rsigma,relvmax,condir,mtnres)
!
      dimension rlat(imr*jmr),rlon(imr*jmr)				&
               ,n00(imr*jmr),n01(imr*jmr),n10(imr*jmr),n11(imr*jmr)	&
               ,d00(imr*jmr),d01(imr*jmr),d10(imr*jmr),d11(imr*jmr)
      dimension rslm(imr*jmr),roro(imr*jmr)
      dimension rvar(imr*jmr)
      dimension rvar4(imr*jmr),roa(imr*jmr,4),rol(imr*jmr,4)
      dimension rtheta(imr*jmr),rgamma(imr*jmr)
      dimension rsigma(imr*jmr),relvmax(imr*jmr)
      character*120 condir
      real      mtnres
!
      integer, allocatable :: zavg(:,:),zvar(:,:),zslm(:,:)
      real*4 oro(iml,jml),slm(iml,jml),var(iml,jml),var4(iml,jml)
      dimension ist(iml),ien(iml),jst(jml),jen(jml)
      real, allocatable :: olll(:,:),elvv(:)
!
      imn=360*60*60/mtnres
      jmn=180*60*60/mtnres
      im1 = iml - 1
      jm1 = jml - 1
      delx  = 360./iml       ! lat-lon  grid resolution
      delxn = 360./imn      ! mountain data resolution
!
!---- find the average of the modes in a grid box
!
      print*,' iml,jml,imn,jmn ',iml,jml,imn,jmn

      lngrd = imr*jmr
      ijml  = iml*jml

      call defg2r(rlat,rlon,iml,jml,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)

      faclon  = delx / delxn
      do i=1,iml
         ist(i) = faclon * float(i-1) + faclon * 0.5
         ist(i) = ist(i) + 1
         ien(i) = faclon * float(i) + faclon * 0.5
!x         print*, ' i ist ien ',i,ist(i),ien(i)
      enddo
      do j=1,jml
         jst(j) = faclon * float(j-1) + faclon * 0.5
         jst(j) = jst(j) + 1
         jen(j) = faclon * float(j) + faclon * 0.5
!x         print*, ' j jst jen ',j,jst(j),jen(j)
      enddo
      ien(iml) = min(ien(iml),imn)
      jen(jml) = min(jen(jml),jmn)
!
!...first, averaged height
!
      allocate (zslm(imn,jmn),zavg(imn,jmn))
      read(11,11) zavg
      read(14,12) zslm
      rewind(11)
      rewind(14)
11    format(20i4)
12    format(80i1)
!
      do i=1,iml
         do j=1,jml
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
      deallocate (zslm)
      allocate (zvar(imn,jmn))
      read(12,11) zvar
      rewind(12)
!
      do i=1,iml
         do j=1,jml
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
      deallocate(zavg,zvar)

      write(6,*) "! make oro slm var var4 done"

      call bilinear(oro,ijml,roro,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
      call bilinear(slm,ijml,rslm,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
      call bilinear(var,ijml,rvar,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
      call bilinear(var4,ijml,rvar4,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
!
      call maxmin(roro,lngrd,1,1,1,'  oro')
      call maxmin(rslm,lngrd,1,1,1,'  slm')
      call maxmin(rvar,lngrd,1,1,1,'  var')
      call maxmin(rvar4,lngrd,1,1,1,'  var4')

!x    call rmakepc30(oro,slm,var,iml,jml)

      call bilinear(oro,ijml,rtheta,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
      call bilinear(slm,ijml,rgamma,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
      call bilinear(var,ijml,rsigma,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
!
      call maxmin(rtheta,lngrd,1,1,1,'  theta')
      call maxmin(rgamma,lngrd,1,1,1,'  gamma')
      call maxmin(rsigma,lngrd,1,1,1,'  sigma')
!
!   compute subgridscale mountain data : oa ol
!
      allocate (olll(iml*jml,4),elvv(iml*jml))
      call rmakeoal05(var(1,1),olll,iml,jml,imn,jmn)
!
      do k = 1,4
        call bilinear(olll(1,k),ijml,rol(1,k),lngrd,			&
                      n00,n01,n10,n11,d00,d01,d10,d11)
      enddo
!
      call rmakeoao05(olll,elvv,iml,jml,imn,jmn)
!
      do k = 1,4
        call bilinear(olll(1,k),ijml,roa(1,k),lngrd,			&
                      n00,n01,n10,n11,d00,d01,d10,d11)
      enddo
!
      call bilinear(elvv,ijml,relvmax,lngrd,n00,n01,n10,n11,d00,d01,d10,d11)
!
      call maxmin(roa,lngrd,4,1,4,'  oa')
      call maxmin(rol,lngrd,4,1,4,'  ol')
      call maxmin(relvmax,lngrd,1,1,1,'  elvmax1')
!
        maxc3 = 0
        maxc4 = 0
        maxc5 = 0
        maxc6 = 0
        maxc7 = 0
        maxc8 = 0
      do n = 1,lngrd
         if (relvmax(n) .gt. 3000.) maxc3 = maxc3 +1
         if (relvmax(n) .gt. 4000.) maxc4 = maxc4 +1
         if (relvmax(n) .gt. 5000.) maxc5 = maxc5 +1
         if (relvmax(n) .gt. 6000.) maxc6 = maxc6 +1
         if (relvmax(n) .gt. 7000.) maxc7 = maxc7 +1
         if (relvmax(n) .gt. 8000.) maxc8 = maxc8 +1
      enddo
      print *,' maxc3-8km:',maxc3,maxc4,maxc5,maxc6,maxc7,maxc8
       print *,' ===> replacing elvmax with elvmax-oro <=== '
       print *,' ===> if elvmax<oro replace with proxy <=== '
       print *,' ===> the sum of mean orog (oro) and std dev <=== '
      do n = 1,lngrd
        if (relvmax(n) .lt. roro(n) ) then
!---  subtracting off oro leaves std dev (this should never happen)
       relvmax(n) = max(  2. * rvar(n),0.)
        else
       relvmax(n) = max( relvmax(n) - roro(n),0.)
        endif
      enddo
      call maxmin(relvmax,lngrd,1,1,1,'  elvmax2')
        maxc3 = 0
        maxc4 = 0
        maxc5 = 0
        maxc6 = 0
        maxc7 = 0
        maxc8 = 0
      do n = 1,lngrd
         if (relvmax(n) .gt. 3000.) maxc3 = maxc3 +1
         if (relvmax(n) .gt. 4000.) maxc4 = maxc4 +1
         if (relvmax(n) .gt. 5000.) maxc5 = maxc5 +1
         if (relvmax(n) .gt. 6000.) maxc6 = maxc6 +1
         if (relvmax(n) .gt. 7000.) maxc7 = maxc7 +1
         if (relvmax(n) .gt. 8000.) maxc8 = maxc8 +1
      enddo
      print *,' after maxc 3-6 km:',maxc3,maxc4,maxc5,maxc6
!
      return
      end
