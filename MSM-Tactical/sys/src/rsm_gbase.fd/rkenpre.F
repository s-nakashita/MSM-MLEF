      subroutine rkenpre(rlatp,rlonp,rgzsp,nflx)
!....
!  this routine computes the kendata grid point indices igrd,jgrd for
!   fcst grid and igrdr,jgrdr for radiation grid, for the
!   npoint points,given the longitude and latitude of same (con).
!   colrad is the colatitude of the fcst grid (dimensioned jdim2,which
!   is half of the total latitudinal grid points),
!....
      use machine , only : kind_io4
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#endif
!
#include <rscomgpd.h>
!cc
      real(kind=kind_io4) fhours
      real(kind=kind_io4), allocatable :: tmps(:,:)
!
      dimension rlatp(IGRD1S,JGRD1S),rlonp(IGRD1S,JGRD1S),           & 
     &          rgzsp(IGRD1S,JGRD1S)
!
      dimension alat(nptken),alon(nptken),kpoi(nptken)
      real, allocatable :: rlat(:,:),rlon(:,:),rgzs(:,:)
      real, allocatable :: blatf(:,:),blonf(:,:),slimsk(:,:)
!jw      data         ilong /igrd1/ ,jlatg /jgrd1/
!jw      data         ilong2/igrd1/,jlatg2/jgrd1/
      ilong =igrd1 ;jlatg =jgrd1
      ilong2=igrd1 ;jlatg2=jgrd1
!
!....   begin here..
!
      allocate (tmps(igrd1,jgrd1))
      allocate (rlat(igrd1,jgrd1))
      allocate (rlon(igrd1,jgrd1))
      allocate (rgzs(igrd1,jgrd1))
      allocate (blatf(igrd1,jgrd1))
      allocate (blonf(igrd1,jgrd1))
      allocate (slimsk(igrd1,jgrd1))
!
#ifdef MP
      me=mype
#else
      me=0
#endif
!
       itnum=1
       do k=1,nstken
        do j=1,nptken
         do i=1,nvrken
          svdata(i,j,k) = 0.
         enddo
        enddo
       enddo
#ifdef MP
      call rmpgp2f(rlatp,igrd1p,jgrd1p, rlat,igrd1,jgrd1,1)
      call rmpgp2f(rlonp,igrd1p,jgrd1p, rlon,igrd1,jgrd1,1)
      call rmpgp2f(rgzsp,igrd1p,jgrd1p, rgzs,igrd1,jgrd1,1)
      call rmpbcastr(rlat,lngrd)
      call rmpbcastr(rlon,lngrd)
      call rmpbcastr(rgzs,lngrd)
#else
      do j=1,jgrd1
      do i=1,igrd1
        rlat(i,j) = rlatp(i,j)
        rlon(i,j) = rlonp(i,j)
        rgzs(i,j) = rgzsp(i,j)
      enddo
      enddo
#endif
!
!--- get slmsk because we wish to get nearest point
!       of same sfc type...this array not available til after step1
!       if we were not trying to cover ourselves for out-board radi8
!       ,then this code could be called from step1...
!
! mpi everybody reads slmsk
!
       rewind nflx
       read(nflx)
       read(nflx) fhours,id1,id2,id3,id4
       ghour=fhours
   99 format(1h ,'fhour, idate=',f6.2,2x,4(1x,i4))
       if(me.eq.0) then
       print *,'in kenpre read slmsk from unit=',nflx
       print 99,ghour, id1,id2,id3,id4
       endif
       read(nflx)
       read(nflx)
       read(nflx)
       read(nflx)
       read(nflx)
       read(nflx)
!.....  skip cv, cvb, cvt, albedo
       read(nflx)
       read(nflx)
       read(nflx)
       read(nflx)
       read(nflx) tmps
       call sgl2ful(tmps,slimsk,igrd1*jgrd1)
       rewind nflx
!
!----    get latitude of gaussian grids (degree)
       do j = 1,jlatg
       do i = 1,ilong
         blatf(i,j) = rlat(i,j) * 180. / pi
         blonf(i,j) = rlon(i,j) * 180. / pi
       enddo
       enddo
!
!...    put lat/lon into useable arrays (max=200),where
!         npoint gt 0 implies npoint lat/lon s in con and
!           if abs(lat) between   0, 90 look for nearest point
!                       between 100,190 look for nearest land point
!                       between 200,290 look for nearest sea point
!         npoint lt 0 implies lat/lon of center of region ,only..
!                    for example, lat,lon = con(1301),con(1501)
!           let xy=abs(npoint) and always be 2 digits
!                              and do not differentiate land/sea,
!             then x between 1,9 means create array of every x points
!                   (i.e. x=1 means every point,x=3 means every 3rd,..
!              and y between 0,9 means create (y+1,y+1) array..
!           thus xy can have values 10-99
!....
       npute = -1
       if (npoint.lt.0) then
           xy = abs(npoint)
           if (xy.lt.10..or.xy.gt.99.) then
              npute = 0
              if(me.eq.0) print 98,npoint
              npoint = 1
           else
              npoint = 1
              iskp = xy/10
              iy   = xy - iskp*10 + 1
              if(me.eq.0) print 97,iy,iy,iskp
              npute = iy * iy
           end if
       end if
   97   format(1h ,' prepare regional (',i2,',',i2,') array - every',    &   
     &             i2,' points')
   98   format(1h ,' out of -range, so set=1')
!
       do 5 k = 1, npoint
        ils = -1
        ylat = abs(gpdlat(k))
        if (ylat.ge.100.and.ylat.le.190.) then
!...     land point is desired...
           ils = 1
           sgn = gpdlat(k) / ylat
           gpdlat(k) = ylat-100.
           if (sgn.lt.0.) gpdlat(k) = - (ylat-100.)
        end if
        if (ylat.ge.200.and.ylat.le.290.) then
           ils = 0
           sgn = gpdlat(k) / ylat
           gpdlat(k) = ylat-200.
           if (sgn.lt.0.) gpdlat(k) = - (ylat-200.)
        end if
        xlat = gpdlat(k)
        xlon = gpdlon(k)
        if(me.eq.0) then
        if (npute.lt.0.and.ils.eq.-1) print 197,k,xlat,xlon
        if (npute.lt.0.and.ils.eq.0) print 198,k,xlat,xlon
        if (npute.lt.0.and.ils.eq.1) print 199,k,xlat,xlon
        endif
  197  format(1h ,' ==== rsm : station ',i4,' at latlon=',2f8.2,         &   
     &            ' desired as nearest point')
  198  format(1h ,' ==== rsm : station ',i4,' at latlon=',2f8.2,         &
     &            ' desired as ocean pt')
  199  format(1h ,' ==== rsm : station ',i4,' at latlon=',2f8.2,         &
     &            ' desired as land pt')
        if (npute.gt.0.and.k.gt.1) go to 195
        alat(k) = gpdlat(k)
        alon(k) = gpdlon(k)
  195  continue
        if (xlon.lt.0) xlon = 360. + gpdlon(k)
        if (npute.lt.0) then
           call rgetij (xlat,xlon,slimsk,blatf,blonf,                    &
     &               ils,ilong,jlatg,ki,kj,me)
        else
           ils = -1
           call rgetij (xlat,xlon,slimsk,blatf,blonf,                    & 
     &                ils,ilong,jlatg,ki,kj,me)
        end if
        igpd(k) = ki
        jgpd(k) = kj
!....
        if(npute.gt.0) go to 5
!           if(kj.gt.jlatg/2) then
!             igpd(k) = ki + ilong
!             jgpd(k) = jlatg + 1 - kj
!           endif
           igpdr(k) = igpd(k)
           jgpdr(k) = jgpd(k)
    5 continue
!
!....    regional block , i,j still in single latitude structure..
!
       if (npute.gt.0) then
          iback = iy/2
!....    if iy = 1 the all we want is 1 point
         if (iback.le.0) then
            npoint = 1
            go to 59
         end if
         istarf = igpd(1) - iback*iskp
         jstarf = jgpd(1) - iback*iskp
         do kyj=1,iy
            do kxi=1,iy
               npoint = npoint + 1
               igpd(npoint) = istarf + (kxi-1)*iskp
               jgpd(npoint) = jstarf + (kyj-1)*iskp
            enddo
         enddo
         do 32 n=1,npoint
            kpoi(n) = 0
            if (jgpd(n).gt.jlatg.or.jgpd(n).lt.1) go to 32
            if (igpd(n).gt.ilong.or.igpd(n).lt.1) go to 32
            kpoi(n) = n
   32    continue
!...    squeeze out the out of bounds points(kpoi=0)
         npp = 0
         do 33 n=1,npoint
         if (kpoi(n).le.0) go to 33
            npp = npp + 1
            igpd(npp) = igpd(kpoi(n))
            jgpd(npp) = jgpd(kpoi(n))
!            if (jgpd(npp).gt.jlatg) then
!               igpd(npp) = igpd(npp) + ilong
!               jgpd(npp) = jlatg+1-jgpd(npp)
!            end if
            gpdnam(npp) = gpdnam(kpoi(n))
            gpdlat(npp) = gpdlat(kpoi(n))
            gpdlon(npp) = gpdlon(kpoi(n))
            gpdgzs(npp) = gpdgzs(kpoi(n))
            idstat(npp) = npp
   33    continue
         npoint = npp
       end if
       if(me.eq.0) print*,'npoint= ',npoint,' after rkenpre'
!.......... save model grid point for rkenwrt .....
       do n=1,npoint
         gpmlat(n) = rlat(igpd(n),jgpd(n)) * 180. / pi
         gpmlon(n) = rlon(igpd(n),jgpd(n)) * 180. / pi
         gpmgzs(n) = rgzs(igpd(n),jgpd(n))
       enddo
!...................  debug print
   59  continue
       do 60 k=1,npoint
          ig=igpd(k)
          jg=jgpd(k)
          igpdr(k) = ig
          jgpdr(k) = jg
          iclnd=ig
          jclnd=jg
!          if(igpd(k).gt.ilong) then
!             iclnd=ig-ilong
!             jclnd=jlatg+1-jg
!          endif
          blat=blatf(iclnd,jclnd)
          blon=blonf(iclnd,jclnd)
          if(me.eq.0) then
          write(6,61) k,alat(k),alon(k),blat,blon
          write(6,62) jgpd(k),igpd(k),slimsk(iclnd,jclnd)
          endif
60     continue
61    format(' rkenpre: k,orig lat-lon,compt lat-lon=',i4,4f8.2)
62    format('          ....jgpd,igpd,slmsk=',2i6,f6.1)
63    format('             ....jgpdr,  igpdr    =',2i8)
!
      deallocate (tmps)
      deallocate (rlat)
      deallocate (rlon)
      deallocate (rgzs)
      deallocate (blatf)
      deallocate (blonf)
      deallocate (slimsk)
!
      return
      end
