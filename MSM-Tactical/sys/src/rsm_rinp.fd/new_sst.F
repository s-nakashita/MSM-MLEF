module newsst_module
  use machine , only : kind_io8,kind_io4
  implicit none
  private
  ! HIMSST
  character(len=10),parameter :: himfile='himsst.txt'
  integer, parameter :: nlonh=800, nlath=600
  real, parameter :: dlonh=0.1, dlath=0.1, lonhw=100.05, lathn=59.95
  
  public :: new_sst
  contains
    subroutine new_sst(out,ijmax,flat,flon,issttype)
      implicit none
      real, allocatable :: gdata(:,:),glon(:),glat(:)
      integer igmax,jgmax
      real out(ijmax),flat(ijmax),flon(ijmax)
      integer issttype !0=himsst
      integer lugb,i,j
      integer ijmax,kgds11
      integer lugi,lskip,iret,jret
      integer jpds(200),jgds(200),kpds(200),kgds(200)
      integer ndata
      real    fhour
      integer idate(4)
      real (kind=kind_io8) f0lon,f0lat,elon,dlon,dlat,rslat,wlon,rnlat
      real xx,yy
      integer x0,y0,x1,y1
      real(8),parameter:: con_pi      =3.1415926535897931 ! pi
      real(8),parameter:: deg_per_rad = 180./con_pi
      real(8),parameter:: rad_per_deg = con_pi/180.
!
      if(issttype.eq.0) then
        igmax=nlonh; jgmax=nlath
        allocate(gdata(igmax,jgmax))
        allocate(glon(igmax),glat(jgmax))
        call read_him(gdata,glon,glat)

        dlon   = dlonh
        wlon   = glon(1)
        elon   = glon(igmax)
        dlat   = -dlath !North to South
        rnlat  = glat(1)
        rslat  = glat(jgmax)
        wlon  = nint(wlon*1000.)  * 0.001
        elon  = nint(elon*1000.)  * 0.001
        rslat = nint(rslat*1000.) * 0.001
        rnlat = nint(rnlat*1000.) * 0.001
      else
        write(6,*) 'cannot find analysis and forecast sst'
        stop
      endif
      print *,'wlon dlon rnlat dlat',wlon,dlon,rnlat,dlat
!
! bi-linear interpolate
!
      do i=1,ijmax
        xx=(flon(i)*deg_per_rad-wlon)/dlon+1
        yy=(flat(i)*deg_per_rad-rnlat)/dlat+1
        if (xx.lt.1 .or. xx.gt.igmax) then
          print *, flon(i),'(',xx,') out side of ',glon(1),' and ',glon(igmax)
          out(i)=0.e0
          continue
          !stop
        else if (yy.lt.1 .or. yy.gt.jgmax) then
          print *,flat(i),'(',yy,') out side of ',glat(1),' and ',glat(jgmax)
          out(i)=0.e0
          continue
          !stop
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
      write(6,*) 'newsst = ', minval(out), maxval(out)
!      
      return
    end
!
    subroutine read_him(sst,lonh,lath)
      implicit none
      real, intent(out) :: sst(nlonh,nlath)
      real, intent(out) :: lonh(nlonh), lath(nlath)
      character(len=20) :: format
      character(len=12) :: yyyymmdd
      character(len=3), dimension(nlonh,nlath) :: sstc
      integer :: iuni, ilon, ilat

      ! set lon lat
      do ilon=1,nlonh
        lonh(ilon) = lonhw + (ilon-1)*dlonh
      end do
      do ilat=1,nlath
        lath(ilat) = lathn - (ilat-1)*dlath
      end do
      write(6,*) 'lon for him = ',minval(lonh), maxval(lonh)
      write(6,*) 'lat for him = ',minval(lath), maxval(lath)
      ! read txt
      iuni=71
      open(iuni,file=himfile,status='old')
      format='(12a)'
      read(iuni,format) yyyymmdd
      print *, yyyymmdd
      
      format='(800a3)'
      do ilat=1,nlath
        read(iuni,format) (sstc(ilon,ilat),ilon=1,nlonh)
      end do
      close(iuni)
      ! convert to real
      do ilat=1,nlath
        do ilon=1,nlonh
          if( sstc(ilon,ilat)=='888' ) then !ice
            sst(ilon,ilat)=271.20e0
          else if( sstc(ilon,ilat)=='999' ) then !land
            sst(ilon,ilat)=271.21e0
          else
            read(sstc(ilon,ilat),*) sst(ilon,ilat)
            sst(ilon,ilat)=sst(ilon,ilat)*0.1e0+273.16e0
          end if
        end do
      end do
      write(6,*) 'himsst = ', minval(sst), maxval(sst)
      return
    end
end module
