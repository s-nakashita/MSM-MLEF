      subroutine ana_sst(out,ijmax,flat,flon)
      use machine , only : kind_io8,kind_io4
      implicit none
      real, allocatable :: gdata(:,:),glon(:),glat(:)
      integer igmax,jgmax
      real out(ijmax),flat(ijmax),flon(ijmax)
      integer mdata
      parameter (mdata=2500*1250)
      integer lugb,i,j
      integer ijmax,kgds11
      integer lugi,lskip,iret,jret
      integer jpds(200),jgds(200),kpds(200),kgds(200)
      integer ndata
      logical*1 lbms(mdata)
      real(kind=kind_io8)    data4(mdata)
      real    fhour
      integer idate(4)
      real (kind=kind_io8) f0lon,f0lat,elon,dlon,dlat,rslat,wlon,rnlat
      real xx,yy
      integer x0,y0,x1,y1
      character*500 fngrib
      data fngrib/'sst.grib'/
      real(8),parameter:: con_pi      =3.1415926535897931 ! pi
      real(8),parameter:: deg_per_rad = 180./con_pi
      real(8),parameter:: rad_per_deg = con_pi/180.
!
      lugb=71
      close(lugb)
      call baopenr(lugb,fngrib(1:8),iret)
      if (iret .ne. 0) then
        write(6,*) ' error in opening file ',fngrib(1:50)
        print *,'error in opening file ',fngrib(1:50)
        call abort
      endif
!
      lugi    = 0
      lskip   = 0
      jpds    = -1
      jgds    = -1
      jpds(5) = 11      
      jpds(6) = 1
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,        &
     &       kpds,kgds,lbms,data4,jret)
      if(jret.eq.0) then
        if(ndata.eq.0) then
          write(6,*) ' error in getgb'
          write(6,*) ' kpds=',kpds
          write(6,*) ' kgds=',kgds
          call abort
        endif
        igmax=kgds(2)
        jgmax=kgds(3)
        allocate(gdata(igmax,jgmax))
        allocate(glon(igmax),glat(jgmax))

        if (kgds(1).eq.0) then
          dlat   = float(kgds(10)) * 0.001
          dlon   = float(kgds( 9)) * 0.001
          f0lon  = float(kgds(5))  * 0.001
          f0lat  = float(kgds(4))  * 0.001
          kgds11 = kgds(11)
          if(kgds11.ge.128) then
            wlon = f0lon - dlon*(kgds(2)-1)
            elon = f0lon
            if(dlon*kgds(2).gt.359.99) then
              wlon =f0lon - dlon*kgds(2)
            endif
            dlon   = -dlon
            kgds11 = kgds11 - 128
          else
            wlon = f0lon
            elon = f0lon + dlon*(kgds(2)-1)
            if(dlon*kgds(2).gt.359.99) then
              elon = f0lon + dlon*kgds(2)
            endif
          endif
          if(kgds11.ge.64) then
            rnlat  = f0lat + dlat*(kgds(3)-1)
            rslat  = f0lat
            kgds11 = kgds11 - 64
          else
            rnlat = f0lat
            rslat = f0lat - dlat*(kgds(3)-1)
            dlat  = -dlat
          endif
          if(wlon.gt.180.) wlon = wlon - 360.
          if(elon.gt.180.) elon = elon - 360.
          wlon  = nint(wlon*1000.)  * 0.001
          elon  = nint(elon*1000.)  * 0.001
          rslat = nint(rslat*1000.) * 0.001
          rnlat = nint(rnlat*1000.) * 0.001
          do i=1,igmax
            glon(i) = wlon + (i-1)*dlon
          enddo
          do j=1,jgmax
            glat(j) = rnlat + (j-1) * dlat
          enddo
        else
          write(6,*) 'unsuppport grid on ana sst'
          stop
        endif
        do i=1,igmax
        do j=1,jgmax
          gdata(i,j)=data4(i+(j-1)*igmax)
        enddo
        enddo
      else
        write(6,*) 'cannot find analysis and forecast sst'
        stop
      endif
!      print *,'wlon dlon rnlat dlat',wlon,dlon,rnlat,dlat
!      print *,kgds(4),kgds(5),kgds(9),kgds(10),kgds(11),kgds(2),kgds(3)
!
! bi-linear interpolate
!
      do i=1,ijmax
        xx=(flon(i)*deg_per_rad-wlon)/dlon+1
        yy=(flat(i)*deg_per_rad-rnlat)/dlat+1
        if (xx.lt.1 .or. xx.gt.igmax) then
          print *,xx,' out side of ',glon(1),' and ',glon(igmax)
          stop
        else if (yy.lt.1 .or. yy.gt.jgmax) then
          print *,yy,' out side of ',glat(1),' and ',glat(jgmax)
          stop
        endif 
        x0 = floor(xx)
        y0 = floor(yy)
        x1 = ceiling(xx)
        y1 = ceiling(yy)
        if (x0 .eq. x1) then
          if (y0 .eq.y1) then
            out(i)=gdata(x0,y0)
          else
            out(i)=gdata(x0,y0)*(real(y1)-yy)+gdata(x0,y1)*(yy-real(y0))
          end if
        else if (y0 .eq. y1) then
          if (x0 .eq. x1) then
            out(i)=gdata(x0,y0)
          else
            out(i)=gdata(x0,y0)*(real(x1)-xx)+gdata(x1,y0)*(xx-real(x0))
          end if
        else
          out(i)=(yy-y0)*(gdata(x0,y1)*(real(x1)-xx)+gdata(x1,y1)*(xx-real(x0)))   &
     &          +(y1-yy)*(gdata(x0,y0)*(real(x1)-xx)+gdata(x1,y0)*(xx-real(x0)))
        end if
!        print *,'xx',xx,yy,x0,y0,x1,y1
!        print *,flon(i),flat(i),glon(x0),glat(y0),glon(x1),glat(y1)
!        out(i)=gdata(x0,y0)
      enddo
!      
      return
      end
