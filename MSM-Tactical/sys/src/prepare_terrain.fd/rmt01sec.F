      subroutine rmt01sec(imr,jmr,iml,jml,rlat,rlon,			&
                          slm,oro,var,var4,oa,ol,			&
                          theta,gamma,sigma,elvmax,condir,mtnres)
!
      dimension rlat(imr*jmr),rlon(imr*jmr)
      dimension slm(imr*jmr),oro(imr*jmr)
      dimension var(imr*jmr)
      dimension var4(imr*jmr),oa(imr*jmr,4),ol(imr*jmr,4)
      dimension theta(imr*jmr),gamma(imr*jmr)
      dimension sigma(imr*jmr),elvmax(imr*jmr)
      real mtnres

      character*120 condir
      character*12  dum
      integer ig(imr*jmr),jg(imr*jmr)
      real, allocatable :: orog(:,:),glon(:),glat(:)
      real, allocatable :: xnsum1(:,:),xnsum2(:,:)
      real, allocatable :: xnsum3(:,:),xnsum4(:,:)
!
      lngrd = imr * jmr
!
! read in the data 
!
!     do i=1,120
!        if(condir(i:i).eq.' ') then
!           mn=i
!           go to 123
!        endif
!     enddo
 123  nin=10
!     print *,' to open ',condir
!     open(nin,file=condir(1:mn),form='formatted',status='old')
      read(nin,*) dum,imd
      read(nin,*) dum,jmd
      read(nin,*) dum,xllcorner
      read(nin,*) dum,yllcorner
      read(nin,*) dum,cellsize
      read(nin,*) dum,nodata_value

      print *,' read data dimension ',imd,jmd

      if(xllcorner.lt.0)xllcorner=360.+xllcorner

      allocate ( orog(imd,jmd) )
      allocate ( glon(imd) )
      allocate ( glat(jmd) )
      do j=jmd,1,-1
         read(nin,*) orog(:,j)
      enddo
      xllcorner = xllcorner + 0.5*cellsize
      yllcorner = yllcorner + 0.5*cellsize
      do i=1,imd
        glon(i)=xllcorner+(i-1)*cellsize
      enddo
      do j=1,jmd
        glat(j)=yllcorner+(j-1)*cellsize
      enddo

      call defloc(rlon,rlat,glon,glat,ig,jg,imr,jmr,imd,jmd)

      gridsizex=0.0
      gridsizey=0.0
      do j=1,jmr-1
       do i=1,imr-1
        ij=i+(j-1)*imr
        gridsizex=gridsizex+(rlon(ij+1)-rlon(ij))
        gridsizey=gridsizey+(rlat(ij+imr)-rlat(ij))
       enddo
      enddo
      total=(imr-1)*(jmr-1)
      degree=90.0/asin(1.0)
      gridsizex=gridsizex/total*degree
      gridsizey=gridsizey/total*degree

      print *,' model grid size in degree ',gridsizex,gridsizey
      print *,' data  grid size in degree ',cellsize

      isize=gridsizex/cellsize
      jsize=gridsizey/cellsize

      print *,' size relation in x and y ',isize,jsize
!      
! find oro
!
      oro = 0.0
      do ij=1,lngrd
          sumnum=0.0
          sumorg=0.0
          if( ig(ij)-isize .lt. 1 .or. ig(ij)+isize .gt. imd ) then
            print *,' is out of domain '
          endif
          if( jg(ij)-jsize .lt. 1 .or. jg(ij)+jsize .gt. jmd ) then
            print *,' js out of domain '
          endif
          do js=jg(ij)-jsize,jg(ij)+jsize
            do is=ig(ij)-isize,ig(ij)+isize
              if( is.ge.1.and.is.le.imd .and. js.ge.1.and.js.le.jmd ) then
                sumorg=sumorg+orog(is,js)
                sumnum=sumnum+1.0
              endif
            enddo
          enddo
          if( sumnum.gt.1.0 ) oro(ij)=sumorg/sumnum
      enddo
      call maxmin(oro,lngrd,1,1,1,'  oro')
!
! find slm from oro for simplicity without slm data
!
      do ij=1,lngrd
          if( oro(ij).gt.0.0 ) then
            slm(ij)=1.0
          else
            slm(ij)=0.0
          endif
      enddo      
      call maxmin(slm,lngrd,1,1,1,'  slm')
!
! find var
!
      do ij=1,lngrd
          var(ij)=0.0
          sumnum=0.0
          sumvar=0.0
          do js=jg(ij)-jsize,jg(ij)+jsize
            do is=ig(ij)-isize,ig(ij)+isize
              if( is.ge.1.and.is.le.imd .and. js.ge.1.and.js.le.jmd ) then
                sumvar=sumvar+orog(is,js)**2.0
                sumnum=sumnum+1.0
              endif
            enddo
          enddo
          if( sumnum.gt.1.0 ) then
            var(ij)=sqrt( max(sumvar/sumnum-oro(ij)**2.0,0.0) )
          endif
      enddo
      call maxmin(var,lngrd,1,1,1,'  var')
!
! find var4
!
      do ij=1,lngrd
          var4(ij) = 0.0
          sumnum=0.0
          sumvar4=0.0
          do js=jg(ij)-jsize,jg(ij)+jsize
            do is=ig(ij)-isize,ig(ij)+isize
              if( is.ge.1.and.is.le.imd .and. js.ge.1.and.js.le.jmd ) then
                sumvar4=sumvar4+(orog(is,js)-oro(ij))**4.0
                sumnum=sumnum+1.0
              endif
            enddo
          enddo
          if( var(ij).gt.1. .and. sumnum.gt.1.0 ) then
            var4(ij) = sumvar4/sumnum/var(ij)**4.0
          endif
      enddo
      call maxmin(var4,lngrd,1,1,1,'  var4')
!
! for theta, gamma, sigma in hprime
!
      theta = oro
      gamma = slm 
      sigma = var
      call maxmin(theta,lngrd,1,1,1,'  theta')
      call maxmin(gamma,lngrd,1,1,1,'  gamma')
      call maxmin(sigma,lngrd,1,1,1,'  sigma')
!
! get ol
!
      allocate ( xnsum1(imr,jmr),xnsum2(imr,jmr) )
      allocate ( xnsum3(imr,jmr),xnsum4(imr,jmr) )
      do j=1,jmr
        do i=1,imr
          ij=i+(j-1)*imr
          xnsum1(i,j) = 0.0
          xnsum2(i,j) = 0.0
          xnsum3(i,j) = 0.0
          xnsum4(i,j) = 0.0
          hc = 1116.2 - 0.878 * var(ij)
          do js=jg(ij)-jsize,jg(ij)+jsize
            do is=ig(ij)-isize,ig(ij)+isize
              if( is.ge.1.and.is.le.imd .and. js.ge.1.and.js.le.jmd ) then
                if(orog(is,js) .gt. hc) xnsum1(i,j) = xnsum1(i,j) + 1
                xnsum2(i,j) = xnsum2(i,j) + 1
              endif
            enddo
          enddo
          do js=jg(ij)-2*jsize,jg(ij)
            do is=ig(ij)-2*isize,ig(ij)
              if( is.ge.1.and.is.le.imd .and. js.ge.1.and.js.le.jmd ) then
                if(orog(is,js) .gt. hc) xnsum3(i,j) = xnsum3(i,j) + 1
                xnsum4(i,j) = xnsum4(i,j) + 1
              endif
            enddo
          enddo
      enddo
      enddo
      call maxmin(xnsum1,lngrd,1,1,1,'  xnsum1 ')
      call maxmin(xnsum2,lngrd,1,1,1,'  xnsum2 ')
      call maxmin(xnsum3,lngrd,1,1,1,'  xnsum3 ')
      call maxmin(xnsum4,lngrd,1,1,1,'  xnsum4 ')
      do j=1,jmr-2
        jj=j+1
        do i=1,imr-2
          ii = i + 1
          iijj=ii+(jj-1)*imr
          ol(iijj,1) = (xnsum3(i,jj)+xnsum3(ii,jj))/            	&
                       (xnsum4(i,jj)+xnsum4(ii,jj))
          ol(iijj,2) = (xnsum3(ii,j)+xnsum3(ii,jj))/            	&
                       (xnsum4(ii,j)+xnsum4(ii,jj))
          ol(iijj,3) = (xnsum1(ii,j)+xnsum1(i,jj))/             	&
                       (xnsum2(ii,j)+xnsum2(i,jj))
          ol(iijj,4) = (xnsum1(i,j)+xnsum1(ii,jj))/             	&
                       (xnsum2(i,j)+xnsum2(ii,jj))
        enddo
      enddo
      ijm=lngrd+1
      do k=1,4
        do j=1,jmr
          ij = (j-1)*imr
          ol(ij+1,k) = ol(ij+2,k)
          ij = ij + imr
          ol(ij-1,k) = ol(ij-2,k)
        enddo
        do i=1,imr
          ol(i,k) = ol(i+imr,k)
          ol(ijm-i,k) = ol(ijm-i-imr,k)
        enddo
      enddo
!
      call maxmin(ol(1,1),lngrd,1,1,1,'  ol1 ')
      call maxmin(ol(1,2),lngrd,1,1,1,'  ol2 ')
      call maxmin(ol(1,3),lngrd,1,1,1,'  ol3 ')
      call maxmin(ol(1,4),lngrd,1,1,1,'  ol4 ')
!
! get oa
!
      oa = 0.0
      elvmax = oro
      do j=1,jmr
        do i=1,imr
          ij=i+(j-1)*imr
          xnsum1(i,j) = 0.0
          do js=jg(ij)-jsize,jg(ij)+jsize
            do is=ig(ij)-isize,ig(ij)+isize
              if( is.ge.1.and.is.le.imd .and. js.ge.1.and.js.le.jmd ) then
                if(orog(is,js) .gt. oro(ij)) then
                  xnsum1(i,j) = xnsum1(i,j) + 1
                  elvmax(ij) = orog(is,js)
                endif
              endif
            enddo
          enddo
        enddo
      enddo
      call maxmin(xnsum1,lngrd,1,1,1,'  xnsum1 for oa  ')
      do j=1,jmr-2
        jj = j + 1
        do i=1,imr
          ii = i + 1
          iijj=ii+(jj-1)*imr
          xnpu = xnsum1(i,j)  + xnsum1(i,jj)
          xnpd = xnsum1(ii,j) + xnsum1(ii,jj)
          if (xnpd .ne. xnpu) oa(iijj,1)=1.-xnpd/max(xnpu,1.)
          xnpu = xnsum1(i,jj) + xnsum1(ii,jj)
          xnpd = xnsum1(i,j)  + xnsum1(ii,j)
          if (xnpd .ne. xnpu) oa(iijj,2)=1.-xnpd/max(xnpu,1.)
          xnpu = xnsum1(i,jj) + ( xnsum1(i,j) + xnsum1(ii,jj) )*0.5
          xnpd = xnsum1(ii,j) + ( xnsum1(i,j) + xnsum1(ii,jj) )*0.5
          if (xnpd .ne. xnpu) oa(iijj,3)=1.-xnpd/max(xnpu,1.)
          xnpu = xnsum1(i,j)   + ( xnsum1(ii,j) + xnsum1(i,jj) )*0.5
          xnpd = xnsum1(ii,jj) + ( xnsum1(ii,j) + xnsum1(i,jj) )*0.5
          if (xnpd .ne. xnpu) oa(iijj,4)=1.-xnpd/max(xnpu,1.)
        enddo
      enddo
      do k=1,4
        do j=1,jmr
          ij = (j-1)*imr
          oa(ij+1,k) = oa(ij+2,k)
          ij = ij + imr
          oa(ij-1,k) = oa(ij-2,k)
        enddo
        do i=1,imr
          oa(i,k) = oa(i+imr,k)
          oa(ijm-i,k) = oa(ijm-i-imr,k)
        enddo
      enddo
      do k=1,4
       do ij=1,lngrd
         t = oa(ij,k)
         oa(ij,k) = sign( min( abs(t), 1. ), t )
       enddo
      enddo

      call maxmin(oa(1,1),lngrd,1,1,1,'  oa1 ')
      call maxmin(oa(1,2),lngrd,1,1,1,'  oa2 ')
      call maxmin(oa(1,3),lngrd,1,1,1,'  oa3 ')
      call maxmin(oa(1,4),lngrd,1,1,1,'  oa4 ')
      call maxmin(elvmax,lngrd,1,1,1,'  elvmax ')
!
! deallocate
!
      deallocate( orog )
      deallocate( glat )
      deallocate( glon )
      deallocate( xnsum1, xnsum2, xnsum3, xnsum4 )
!
      return
      end
