!
!***********************************************************************
!
      subroutine coszmn(fhswr,solhr,sinlat,coslat,sdec,cdec,slag,       &
     &                  xlon,imx2,jmx2,coszen,ldg,coszdg)
!fpp$ noconcur r
!
!===>  compute mean cos solar zen angl over fhswr hrs
!....   cosine of solar zen angl for both n. and s. hemispheres.
!        solhr=time(hrs) after 00z (greenwich time)..
!        xlon is east long(radians)..
!        sinlat, coslat are sin and cos of latitude (n. hemisphere)
!        sdec, cdec = the sine and cosine of the solar declination.
!        slag = equation of time
!cc
      dimension xlon(imx2,jmx2),coszen(imx2,jmx2)
      logical ldg
      dimension coszdg(imx2,jmx2)
      dimension sinlat(imx2,jmx2),coslat(imx2,jmx2)
! local array
      dimension coszn(imx2),istsun(imx2)
      nlon2=imx2
      nlat2=jmx2
      nstp = 6
      istp = nstp*fhswr
      pid12 = (2.e0 * asin(1.e0)) / 12.e0
!----
      do 2000 j=1,nlat2
        do 20 i=1,nlon2
          coszen(i,j) = 0.e0
          istsun(i) = 0
   20   continue
      do 1000 it=1,istp
       cns = pid12 * (solhr-12.e0+(it-1)*1.e0/nstp) +slag
       do 40 i=1,nlon2
         ss=sinlat(i,j)*sdec
         cc=coslat(i,j)*cdec
         coszn(i) = ss + cc * cos(cns + xlon(i,j))
!x       coszn(i) = sinlat(i,j)*sdec +                                  &
!x   &              coslat(i,j)*cdec * cos(cns + xlon(i,j))
         coszen(i,j) = coszen(i,j) +  max (0.e0, coszn(i))
         if(coszn(i).gt.0.e0) istsun(i) = istsun(i) + 1
   40  continue
 1000 continue
       do 1500 i=1,nlon2
        if(ldg) coszdg(i,j) = coszen(i,j) / istp
        if(istsun(i).gt.0) coszen(i,j) = coszen(i,j) / istsun(i)
 1500  continue
 2000 continue
      return
      end
