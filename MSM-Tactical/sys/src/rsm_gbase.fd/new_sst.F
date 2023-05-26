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
    subroutine new_sst(out,mask,ijmax,flat,flon,issttype)
      implicit none
      real(kind=kind_io4),intent(out) :: out(ijmax) 
      real(kind=kind_io8),intent(in)  :: flat(ijmax),flon(ijmax)
      integer            ,intent(out) :: mask(ijmax)
      integer            ,intent(in)  :: ijmax
      integer            ,intent(in)  :: issttype !0=himsst
      real(kind=kind_io4),allocatable :: gdata(:,:),glon(:),glat(:)
      integer            ,allocatable :: maskg(:,:)
      integer                         :: mask4(4)
      real(kind=kind_io4)             :: data4(4)
      integer                         :: igmax,jgmax
      integer                         :: i,j
      real (kind=kind_io8) f0lon,f0lat,elon,dlon,dlat,rslat,wlon,rnlat
      real (kind=kind_io4) xx,yy
      integer x0,y0,x1,y1
      real(8),parameter:: con_pi      =3.1415926535897931 ! pi
      real(8),parameter:: deg_per_rad = 180./con_pi
      real(8),parameter:: rad_per_deg = con_pi/180.
!
      if(issttype.eq.0) then
        igmax=nlonh; jgmax=nlath
        allocate(gdata(igmax,jgmax))
        allocate(maskg(igmax,jgmax))
        allocate(glon(igmax),glat(jgmax))
        call read_him(gdata,maskg,glon,glat)
        write(6,*) 'himsst = ', minval(gdata), maxval(gdata)
        write(6,*) 'mask   = ', minval(maskg), maxval(maskg)

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
      out(:)=0.0
      mask(:)=1
      do i=1,ijmax
        xx=(flon(i)*deg_per_rad-wlon)/dlon+1
        yy=(flat(i)*deg_per_rad-rnlat)/dlat+1
        if (xx.lt.1 .or. xx.gt.igmax) then
          !write(6,200) flon(i),'(',xx,') out side of ',glon(1),' and ',glon(igmax)
          cycle
        else if (yy.lt.1 .or. yy.gt.jgmax) then
          !write(6,200) flat(i),'(',yy,') out side of ',glat(1),' and ',glat(jgmax)
          cycle
        endif 
 200    format (f6.2,a1,f8.2,a14,f6.2,a5,f6.2)
        x0 = floor(xx)
        y0 = floor(yy)
        x1 = ceiling(xx)
        y1 = ceiling(yy)
        if (x0 .eq. x1) then
          if (y0 .eq.y1) then
            mask(i)=maskg(x0,y0)
            if(mask(i).eq.0) then
              out(i)=gdata(x0,y0)
              !print *, i, out(i)
            end if
          else
            mask4(1)=maskg(x0,y0)
            mask4(2)=maskg(x0,y1)
            if(mask4(1).eq.0.and.mask4(2).eq.0) then
              mask(i)=0
              out(i)=gdata(x0,y0)*(real(y1,kind=kind_io4)-yy)&
                   &+gdata(x0,y1)*(yy-real(y0,kind=kind_io4))
            else if(mask4(1).eq.0) then
              mask(i)=0
              out(i)=gdata(x0,y0)
            else if(mask4(2).eq.0) then
              mask(i)=0
              out(i)=gdata(x0,y1)
            end if
            !print *, i, out(i)
          end if
        else if (y0 .eq. y1) then
          if (x0 .eq. x1) then
            mask(i)=maskg(x0,y0)
            if(mask(i).eq.0) then
              out(i)=gdata(x0,y0)
              !print *, i, out(i)
            end if
          else
            mask4(1)=maskg(x0,y0)
            mask4(2)=maskg(x1,y0)
            if(mask4(1).eq.0.and.mask4(2).eq.0) then
              mask(i)=0
              out(i)=gdata(x0,y0)*(real(y1,kind=kind_io4)-yy)&
                   &+gdata(x0,y1)*(yy-real(y0,kind=kind_io4))
            else if(mask4(1).eq.0) then
              mask(i)=0
              out(i)=gdata(x0,y0)
            else if(mask4(2).eq.0) then
              mask(i)=0
              out(i)=gdata(x1,y0)
            end if
            !print *, i, out(i)
          end if
        else
          mask(i)=max(maskg(x0,y0),maskg(x1,y0))
          mask(i)=max(mask(i),maskg(x0,y1))
          mask(i)=max(mask(i),maskg(x1,y1))
          mask4(1)=maskg(x0,y0)
          mask4(2)=maskg(x1,y0)
          mask4(3)=maskg(x0,y1)
          mask4(4)=maskg(x1,y1)
          if(mask4(1).eq.0.and.mask4(2).eq.0.and.&
            &mask4(3).eq.0.and.mask4(4).eq.0) then
            mask(i)=0
            out(i)=(yy-y0)*(gdata(x0,y1)*(real(x1,kind=kind_io4)-xx)    &
       &                   +gdata(x1,y1)*(xx-real(x0,kind=kind_io4)))   &
       &          +(y1-yy)*(gdata(x0,y0)*(real(x1,kind=kind_io4)-xx)    &
       &                   +gdata(x1,y0)*(xx-real(x0,kind=kind_io4)))
          else if(mask4(1)*mask4(2)*mask4(3)*mask4(4).eq.0) then
            if((xx-real(x0)).gt.(real(x1)-xx)) then
              if((yy-real(y0)).gt.(real(y1)-yy)) then
                data4(1)=gdata(x1,y1);mask4(1)=maskg(x1,y1)
                data4(2)=gdata(x1,y0);mask4(2)=maskg(x1,y0)
                data4(3)=gdata(x0,y1);mask4(3)=maskg(x0,y1)
                data4(4)=gdata(x0,y0);mask4(4)=maskg(x0,y0)
              else
                data4(1)=gdata(x1,y0);mask4(1)=maskg(x1,y0)
                data4(2)=gdata(x1,y1);mask4(2)=maskg(x1,y1)
                data4(3)=gdata(x0,y0);mask4(3)=maskg(x0,y0)
                data4(4)=gdata(x0,y1);mask4(4)=maskg(x0,y1)
              end if
            else
              if((yy-real(y0)).gt.(real(y1)-yy)) then
                data4(1)=gdata(x0,y1);mask4(1)=maskg(x0,y1)
                data4(2)=gdata(x0,y0);mask4(2)=maskg(x0,y0)
                data4(3)=gdata(x1,y1);mask4(3)=maskg(x1,y1)
                data4(4)=gdata(x1,y0);mask4(4)=maskg(x1,y0)
              else
                data4(1)=gdata(x0,y0);mask4(1)=maskg(x0,y0)
                data4(2)=gdata(x0,y1);mask4(2)=maskg(x0,y1)
                data4(3)=gdata(x1,y0);mask4(3)=maskg(x1,y0)
                data4(4)=gdata(x1,y1);mask4(4)=maskg(x1,y1)
              end if
            end if
            if(mask4(1).eq.0) then
              mask(i)=0
              out(i)=data4(1)
            else if(mask4(2).eq.0) then
              mask(i)=0
              out(i)=data4(2)
            else if(mask4(3).eq.0) then
              mask(i)=0
              out(i)=data4(3)
            else if(mask4(4).eq.0) then
              mask(i)=0
              out(i)=data4(4)
            end if
          end if
          !print *, i, out(i)
        end if
!        print *,'xx',xx,yy,x0,y0,x1,y1
!        print *,flon(i),flat(i),glon(x0),glat(y0),glon(x1),glat(y1)
!        out(i)=gdata(x0,y0)
      enddo
      write(6,*) 'newsst = ', minval(out), maxval(out)
      write(6,*) 'mask   = ', minval(mask), maxval(mask)
!      
      return
    end
!
    subroutine read_him(sst,mask,lonh,lath)
      implicit none
      real(kind=kind_io4), intent(out) :: sst(nlonh,nlath)
      integer, intent(out)             :: mask(nlonh,nlath)
                                         !=0:sea,=1:land,=2:ice
      real(kind=kind_io4), intent(out) :: lonh(nlonh), lath(nlath)
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
      mask=0
      do ilat=1,nlath
        do ilon=1,nlonh
          if( sstc(ilon,ilat)=='888' ) then !ice
            mask(ilon,ilat)=2
            sst(ilon,ilat)=real(271.20,kind=kind_io4)
          else if( sstc(ilon,ilat)=='999' ) then !land
            mask(ilon,ilat)=1
            sst(ilon,ilat)=real(271.21,kind=kind_io4)
          else
            read(sstc(ilon,ilat),*) sst(ilon,ilat)
            sst(ilon,ilat)=sst(ilon,ilat)*0.1+273.16
          end if
        end do
      end do
      return
    end
end module
