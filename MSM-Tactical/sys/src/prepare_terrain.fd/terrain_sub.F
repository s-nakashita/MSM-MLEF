      subroutine terrain_sub(iml,jml,type,condir)
!
      use comrloc
      use comrpln
      use comrfft
!
      integer	iml,jml
      integer 	type
      character*120 condir
!
      real flat(im*jm),flon(im*jm)
      real slm(im*jm),oro(im*jm),ors(im*jm),oss(im*jm)
      real var(im*jm),vas(im*jm)
      real var4(im*jm),oa(im*jm,4),ol(im*jm,4)
      real theta(im*jm),gamma(im*jm)
      real sigma(im*jm),elvmax(im*jm)
      real wave(iw*jw),grids(im*2),smooth(iw*jw)
      real hprimes(im*jm,mtnvar)
!
      im2 = im*2
      jm2 = jm/2
!
!- open(unit=11,form='formatted',err=900) ! average
!- open(unit=12,form='formatted',err=900) ! variance
!- open(unit=13,form='formatted',err=900) ! maximum
!- open(unit=14,form='formatted',err=900) ! sea-land-lake-mask
!
      print *,'+++++ define model grid lat/lon +++++'
      call setgrd(flat,flon,delx,dely,dlamda0)
!     
!     get and compute mountain data 
!
      if ( type == 1 ) then		! do arc 1 sec
        call rmt01sec(im,jm,iml,jml,flat,flon,				&
                      slm,oro,var,var4,oa,ol,				&
                      theta,gamma,sigma,elvmax,condir,mtnvar)
      else if ( type == 30 ) then		! do gtopo30sec
        call rmt30sec(im,jm,iml,jml,flat,flon,				&
                      slm,oro,var,var4,oa,ol,				&
                      theta,gamma,sigma,elvmax,condir,mtnvar)
      else if ( type == 300 ) then		! do gtopo05min
        call rmt05min(im,jm,iml,jml,flat,flon,				&
                      slm,oro,var,var4,oa,ol,				&
                      theta,gamma,sigma,elvmax,condir,mtnvar)
      else
        print *,' type of data is not supported. abort!! '
        stop
      endif
!
!     zero over ocean
!
      do n = 1,lngrd
        slm(n) = float(nint(slm(n)))
        if(slm(n).eq.0.) then
           var(n)  = 0.
           var4(n) = 0.
           ol(n,1) = 0.
           ol(n,2) = 0.
           ol(n,3) = 0.
           ol(n,4) = 0.
           oa(n,1) = 0.
           oa(n,2) = 0.
           oa(n,3) = 0.
           oa(n,4) = 0.
           theta(n) = 0.0
           gamma(n) = 0.0
           sigma(n) = 0.0
           elvmax(n) = 0.0
        endif
      enddo
!
!  spectral smoothing and lnczs filter for oro
!
      print *,' +++++ do fourier(cos,cos) transform ++++'
      call rplnini(im,jm,iw,jw)
      call rfftini(im,iw)
!
      do m=1,iw*jw
         wave(m) = 0.0
      enddo
!
      do lat = 1,jm2
!.... for n.s. grid
        js = (lat-1)*im
        jn = (jm-lat)*im
        do i = 1,im
          grids(i   ) = oro(js+i)
          grids(i+im) = oro(jn+i)
        enddo
        call ffacos(grids,im,iw,2)
        call sumggc(grids,wave,gcosc(1,lat),im2,lnwav,iw,jw,im,1)
      enddo
!
      do lat = 1,jm2
        js = (lat-1)*im
        jn = (jm-lat)*im
        call sumffc(wave,grids,ccosg(1,lat),im2,lnwav,iw,jw,im,1)

        call ffscos(grids,im,iw,2)
        do i = 1,im
          ors(js+i)=grids(i)
          ors(jn+i)=grids(i+im)
        enddo
      enddo
!
      call lnczs(wave,smooth,iw,jw,order)
!
      do lat = 1,jm2
        js = (lat-1)*im
        jn = (jm-lat)*im
        call sumffc(wave,grids,ccosg(1,lat),im2,lnwav,iw,jw,im,1)
        call ffscos(grids,im,iw,2)
        do i = 1,im
          oss(js+i) = grids(i)
          oss(jn+i) = grids(i+im)
        enddo
      enddo
!
!  spectral smoothing for var
!
      do m=1,iw*jw
         wave(m) = 0.0
      enddo
!
      do lat = 1,jm2
!.... for n.s. grid
        js = (lat-1)*im
        jn = (jm-lat)*im
        do i = 1,im
          grids(i   ) = var(js+i)
          grids(i+im) = var(jn+i)
        enddo
        call ffacos(grids,im,iw,2)
        call sumggc(grids,wave,gcosc(1,lat),im2,lnwav,iw,jw,im,1)
      enddo
!
      do lat = 1,jm2
        js = (lat-1)*im
        jn = (jm-lat)*im
        call sumffc(wave,grids,ccosg(1,lat),im2,lnwav,iw,jw,im,1)
        call ffscos(grids,im,iw,2)
        do i = 1,im
          vas(js+i) = grids(i)
          vas(jn+i) = grids(i+im)
        enddo
      enddo
!
      do n = 1,lngrd
        vas(n) = max(vas(n),0.)
        if(slm(n).eq.0.) then
           vas(n) = 0.
        endif
      enddo
!
      write(51)       (slm(i) ,i=1,lngrd)
      write(52)       (oro(i) ,i=1,lngrd)
      if(mtnvar.gt.1) then
        hprimes(:, 1)  = var(:)
        hprimes(:, 2)  = var4(:)
        hprimes(:, 3)  = oa(:,1)
        hprimes(:, 4)  = oa(:,2)
        hprimes(:, 5)  = oa(:,3)
        hprimes(:, 6)  = oa(:,4)
        hprimes(:, 7)  = ol(:,1)
        hprimes(:, 8)  = ol(:,2)
        hprimes(:, 9)  = ol(:,3)
        hprimes(:,10)  = ol(:,4)
        hprimes(:,11)  = theta(:)
        hprimes(:,12)  = gamma(:)
        hprimes(:,13)  = sigma(:)
        hprimes(:,14)  = elvmax(:)
        write(53)       hprimes
      else
        write(53)       (var(i),i=1,lngrd)
      endif
      write(54)       (ors(i) ,i=1,lngrd)
      write(55)       (oss(i) ,i=1,lngrd)
!
      print *,' finish writing all date '
!
      return
      end
