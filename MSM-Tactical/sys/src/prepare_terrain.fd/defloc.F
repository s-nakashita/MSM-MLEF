!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine defloc(flon,flat,glon,glat,ig,jg,im,jm,imd,jmd)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ----- bilinear interpolation coefficient ------
!
      real flat(im*jm),flon(im*jm)
      real glon(imd),glat(jmd)
      integer ig(im*jm),jg(im*jm)
      real xlon(imd),xlat(jmd)
!
      glonmax=glon(1)
      glonmin=glon(1)
      glatmax=glat(1)
      glatmin=glat(1)
      do i=1,imd
        glonmax = max(glonmax,glon(i))
        glonmin = min(glonmin,glon(i))
      enddo
      do j=1,jmd
        glatmax = max(glatmax,glat(j))
        glatmin = min(glatmin,glat(j))
      enddo
      print *,' data domain is ',glonmin,glatmin,glonmax,glatmax

      flonmax=flon(1)
      flonmin=flon(1)
      flatmax=flat(1)
      flatmin=flat(1)
      do i=1,im*jm
        flonmax = max(flonmax,flon(i))
        flonmin = min(flonmin,flon(i))
        flatmax = max(flatmax,flat(i))
        flatmin = min(flatmin,flat(i))
      enddo
      degree = 90. / asin(1.0)
      flonmax = flonmax * degree
      flonmin = flonmin * degree
      flatmax = flatmax * degree
      flatmin = flatmin * degree
      print *,' model domain is ',flonmin,flatmin,flonmax,flatmax

      if( flonmin.lt.glonmin .or. flonmax.gt.glonmax .or.		&
          flatmin.lt.glatmin .or. flatmax.gt.glatmax ) then
        print *,' model domain larger than data domain '
        stop
      endif

      do i=1,imd
        xlon(i)=glon(i)/degree
      enddo
      do j=1,jmd
        xlat(j)=glat(j)/degree
      enddo
      
      do 1000 n=1,im*jm

        nlon=0
        nlat=0
        do ii=1,imd
          if( flon(n).ge.xlon(ii) .and. flon(n).lt.xlon(ii+1) ) then
            nlon = ii
            go to 10
          endif
        enddo
 10     continue
        do jj=1,jmd
          if( flat(n).ge.xlat(jj) .and. flat(n).lt.xlat(jj+1) ) then
            nlat = jj
            go to 20
          endif
        enddo
 20     continue
        if( nlat.eq.0 .or. nlon.eq.0 ) then
          print *,' model domain out of data domain '
          stop
        else
          ig(n) = nlon
          jg(n) = nlat
        endif
!
1000  continue
!
      return
      end
