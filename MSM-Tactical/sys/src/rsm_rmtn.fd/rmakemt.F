!cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#include <define.h>
#ifdef GTOPO30
      subroutine rmakemt(oro,slm,var,var4,im,jm,flat1,flat2,flon1,flon2,          &
     &                   condir)
#else
      subroutine rmakemt(oro,slm,var,var4,im,jm)
#endif
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#include <paramodel.h>
!
      parameter(imn=360*60/mtnres,jmn=180*60/mtnres)
#ifndef GTOPO30
      integer, allocatable :: zavg(:,:),zvar(:,:),zslm(:,:)
      dimension oro(im,jm),slm(im,jm),var(im,jm),var4(im,jm)
#else
      real oro(im,jm),slm(im,jm),var(im,jm),var4(im,jm)
#endif
!
      dimension ist(im),ien(im),jst(jm),jen(jm)
#ifdef GTOPO30
      character*120 condir
      character title(15)*7,titlem(15)*11,filen*180
      character titlelon(9)*4,titlelat(4)*3,titletmp(9)*4,title_s(6)*4
      data titlelon /'W020','E020','E060','E100','E140',                          &
     &               'W180','W140','W100','W060'/
      data titlelat /'N90','N40','S10','S60'/
      data title_s /'W000','E060','E120','W180','W120','W060'/
      data pi/3.141593/
      logical positive
      integer zslmask
      integer*2, allocatable :: ifile(:,:),jfile(:,:)
      integer*1, allocatable :: imfile(:,:),jmfile(:,:)
      integer, allocatable :: lat_start(:),lon_start(:)
      real, allocatable :: zavg(:,:),gtopo(:,:)
      integer, allocatable :: zslm(:,:),gmask(:,:)
#endif
      logical flag
!
      print*,' in rmakemt, im,jm,imn,jmn ',im,jm,imn,jmn
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
#ifdef GTOPO30
!
!     To select domain from GTOPO30 terrain data.
!     Make sure that each file of GTOPO30 data is blocked.
!
      print*,180/pi*flat1,180/pi*flat2 ,180/pi*flon1,180/pi*flon2
      lat1=nint(180/pi*flat1)-2
      lat2=nint(180/pi*flat2)+2
      lon1=nint(180/pi*flon1)-2
      lon2=nint(180/pi*flon2)+2
      print*,lon1,lon2,lat1,lat2
      if(lon1.lt.lon2.and.(lon1.ge.20.and.lon1.lt.340)                           &
     &   .and.(lon2.gt.20.and.lon2.le.340)) positive=.true.
       print*,'longitude type= ',positive
!
      if(.not.positive) then
        if(lon1.lt.180) then
           lon1=lon1+180
        else
           lon1=lon1-180
        endif
        if(lon2.lt.180) then
           lon2=lon2+180
        else
           lon2=lon2-180
        endif
        do i=1,9
           if(i.ge.6) titletmp(i-5)=titlelon(i)
           if(i.lt.6) titletmp(i+4)=titlelon(i)
        enddo
        do i=1,9
           titlelon(i)=titletmp(i)
        enddo
      endif
!
      do i=1,120
         if(condir(i:i).eq.' ') then
            mn=i
            goto 10
         endif
      enddo
10    continue
!
      if(lat1.ge.-60) then
!
      do i=1,3
         if(lat1.ge.(50*i-110).and.lat1.lt.(50*i-60)) na1=4-i
         if(lat2.gt.(50*i-110).and.lat2.le.(50*i-60)) na2=4-i
      enddo
      do i=1,9
         if(positive) then
           if(lon1.ge.(40*i-60).and.lon1.lt.(40*i-20)) no1=i
           if(lon2.gt.(40*i-60).and.lon2.le.(40*i-20)) no2=i
         else
           if(lon1.ge.(40*i-40).and.lon1.lt.(40*i)) no1=i
           if(lon2.gt.(40*i-40).and.lon2.le.(40*i)) no2=i
         endif
      enddo
      latnum=na1-na2+1
      lonnum=no2-no1+1
!
!
      itotal=latnum*lonnum
        print*,'lat=',na2,na1,'lon=',no1,no2
        print*,'tiles #',itotal
      n=1
      do j=na2,na1
      do i=no1,no2
         title(n)(1:4)=titlelon(i)
         title(n)(5:7)=titlelat(j)
         print*,'tile name= ',title(n)
         n=n+1
      enddo
      enddo
!
      lat_grid1=nint((90-lat1)/(0.5/60)+.5)
      lat_grid2=nint((90-lat2)/(0.5/60)+.5)
      lon_grid1=nint(lon1/(0.5/60)+.5)
      lon_grid2=nint(lon2/(0.5/60)+.5)
      if(lat_grid1.eq.21601) lat_grid1=21600
      if(lon_grid1.eq.43201) lon_grid2=43200
      print*,'absoulte grid #',lon_grid1,lon_grid2,lat_grid2,lat_grid1
      print*,'idim*jdim= ',lon_grid2-lon_grid1,'*',lat_grid1-lat_grid2
      allocate (lat_start(itotal),lon_start(itotal))
      n=1
      do j=na2,na1
      do i=no1,no2
         lat_start(n)=(50*j-50)/(0.5/60)+1
         if(positive) then
           lon_start(n)=(40*i-60)/(0.5/60)+1
           if(lon_start(n).le.0) lon_start(n)=lon_start(n)+43200
         else
           lon_start(n)=(40*i-40)/(0.5/60)+1
         endif
         print*,'starting pts of each tile',lon_start(n),lat_start(n)
      n=n+1
      enddo
      enddo
!
      allocate (gtopo(lon_grid1:lon_grid2-1,                                   &
     &                           21600-lat_grid1:21600-lat_grid2+1))
      allocate (gmask(lon_grid1:lon_grid2-1,                                   &
     &                           21600-lat_grid1:21600-lat_grid2+1))
      lat_rel2=lat_grid2-lat_start(1)+1
      lat_rel1=lat_grid1-lat_start((na1-na2)*(no2-no1+1)+1)
      lon_rel2=lon_grid2-lon_start(itotal)
      lon_rel1=lon_grid1-lon_start(1)+1
      print*,'relative grid #',lon_rel1,lon_rel2,lat_rel2,lat_rel1
!
      n=1
      do i=na2,na1
      do j=no1,no2
      allocate (ifile(4800,6000),imfile(4800,6000))
!x       filen=condir(1:mn-1)//'/'//title(n)//'\0'
!hmhj    filen=title(n)//'.DEM'
         write(filen,"(a,a)") title(n),'.DEM'
         print *,'opening file=',filen
         open(unit=80,file=filen,form='unformatted',                           &
     &                access='direct',recl=57600000)
         read(80,rec=1) ((ifile(l,m),l=1,4800),m=1,6000)
         close(80)
         titlem(n)(1:7)=title(n)
         titlem(n)(8:11)='.SRC'
!x       titlem(n)(8:11)='.slm'
!
!  possible error mk 010605
!
!        filen(mn:mn+18)=condir(1:nm-1)//'/'//titlem(n)//'\0'
!x       filen=condir(1:mn-1)//'/'//titlem(n)//'\0'
         filen=titlem(n)
         print *,'opening file=',filen
         open(unit=80,file=filen,form='unformatted',                           &
     &                access='direct',recl=28800000)
         read(80,rec=1) ((imfile(l,m),l=1,4800),m=1,6000)
         close(80)
         n_s=1
         n_e=6000
         if(i.eq.na2) n_s=lat_rel2
         if(i.eq.na1) n_e=lat_rel1
         m_s=1
         m_e=4800
         if(j.eq.no1) m_s=lon_rel1
         if(j.eq.no2) m_e=lon_rel2
         do jj=n_s,n_e
         do ii=m_s,m_e
            gtopo(lon_start(n)+ii-1,21600-(lat_start(n)+jj)+2)                 &
     &           =ifile(ii,jj)
            gmask(lon_start(n)+ii-1,21600-(lat_start(n)+jj)+2)                 &
     &           =imfile(ii,jj)
         enddo
         enddo
         n=n+1
         deallocate (ifile)
         deallocate (imfile)
      enddo
      enddo
!      write(*,'(20i5)') ((int(gmask(i,j)),i=lon_grid1,lon_grid2-1,240)        &
!     &              ,j=21600-lat_grid2+1,21600-lat_grid1+1,-200)
!
      else
      if(.not.positive) then
        do i=1,6
           if(i.ge.4) titletmp(i-3)=title_s(i)
           if(i.lt.4) titletmp(i+3)=title_s(i)
        enddo
        do i=1,6
           title_s(i)=titletmp(i)
        enddo
      endif
!
      do i=1,3
         if(lat2.gt.(50*i-110).and.lat2.le.(50*i-60)) na2=4-i
      enddo
      if(lat2.ge.-90.and.lat2.lt.-60) na2=4
      na1=4
      if(na2.ne.4) then
      do i=1,9
         if(positive) then
           if(lon1.ge.(40*i-60).and.lon1.lt.(40*i-20)) no1=i
           if(lon2.gt.(40*i-60).and.lon2.le.(40*i-20)) no2=i
         else
           if(lon1.ge.(40*i-40).and.lon1.lt.(40*i)) no1=i
           if(lon2.gt.(40*i-40).and.lon2.le.(40*i)) no2=i
         endif
      enddo
      endif
      do i=1,6
           if(lon1.ge.(60*i-60).and.lon1.lt.(60*i)) nos1=i
           if(lon2.gt.(60*i-60).and.lon2.le.(60*i)) nos2=i
      enddo
!
      do i=1,4
      if(na2.eq.i) itotal=nos2-nos1+1+(4-i)*(no2-no1+1)
      enddo
      long=(4-na2)*(no2-no1+1)
      iwide=nos2-nos1+1
        print*,'lat=',na2,na1,'lon=',no1,no2
        print*,'                 /lon_s=',nos1,nos2
        print*,'tiles#',itotal,'(long:wide=)',long,iwide
      n=1
      if(na2.ne.4) then
        do j=na2,3
        do i=no1,no2
           title(n)(1:4)=titlelon(i)
           title(n)(5:7)=titlelat(j)
           print*,'tile name= ',title(n)
           n=n+1
        enddo
        enddo
      endif
      do i=nos1,nos2
         title(n)(1:4)=title_s(i)
         title(n)(5:7)=titlelat(4)
         print*,'tile name_s= ',title(n)
         n=n+1
      enddo
!
      lat_grid1=nint((90-lat1)/(0.5/60)+.5)
      lat_grid2=nint((90-lat2)/(0.5/60)+.5)
      lon_grid1=nint(lon1/(0.5/60)+.5)
      lon_grid2=nint(lon2/(0.5/60)+.5)
      if(lat_grid1.eq.21601) lat_grid1=21600
      if(lon_grid1.eq.43201) lon_grid2=43200
      print*,'absoulte grid #',lon_grid1,lon_grid2,lat_grid2,lat_grid1
      print*,'idim*jdim= ',lon_grid2-lon_grid1,'*',lat_grid1-lat_grid2
      allocate (lat_start(itotal),lon_start(itotal))
      n=1
      if(na2.ne.4) then
        do j=na2,3
        do i=no1,no2
           lat_start(n)=(50*j-50)/(0.5/60)+1
           if(positive) then
             lon_start(n)=(40*i-60)/(0.5/60)+1
             if(lon_start(n).le.0) lon_start(n)=lon_start(n)+43200
           else
             lon_start(n)=(40*i-40)/(0.5/60)+1
           endif
           print*,'starting pts of each tile',lon_start(n),lat_start(n)
           n=n+1
        enddo
        enddo
      endif
      do i=nos1,nos2
         lat_start(n)=150/(0.5/60)+1
         lon_start(n)=(60*i-60)/(0.5/60)+1
         print*,'starting pts of each tile',lon_start(n),lat_start(n)
         n=n+1
      enddo
!
      allocate (gtopo(lon_grid1:lon_grid2-1,                                     &
     &                           21600-lat_grid1:21600-lat_grid2+1))
      allocate (gmask(lon_grid1:lon_grid2-1,                                     &
     &                           21600-lat_grid1:21600-lat_grid2+1))
      lat_rel2=lat_grid2-lat_start(1)+1
      lat_rel1=lat_grid1-lat_start(long+1)
      if(long.ne.0) then
        lon_rel1=lon_grid1-lon_start(1)+1
        lon_rel2=lon_grid2-lon_start(long)
      endif
      lon_rels1=lon_grid1-lon_start(long+1)+1
      lon_rels2=lon_grid2-lon_start(itotal)
      print*,'relative grid #',lon_rel1,lon_rel2,lat_rel2,lat_rel1
      print*,'relative grid_s #',lon_rels1,lon_rels2
!
      n=1
      if(long.ne.0) then
        do i=na2,3
        do j=no1,no2
           allocate (ifile(4800,6000))
           allocate (imfile(4800,6000))
!x         filen=condir(1:mn-1)//'/'//title(n)//'\0'
!hmhj      filen=title(n)//'.DEM'
           write(filen,"(a,a)") title(n),'.DEM'
           print *,'opening file=',filen
           open(unit=80,file=filen,form='unformatted',                          &
     &                  access='direct',recl=57600000)
           read(80,rec=1) ((ifile(l,m),l=1,4800),m=1,6000)
           close(80)
           titlem(n)(1:7)=title(n)
           titlem(n)(8:11)='.SRC'
!x         titlem(n)(8:11)='.slm'
!x         filen(mn:mn+18)=condir(1:nm-1)//'/'//titlem(n)//'\0'
           filen(mn:mn+18)=titlem(n)
           print *,'opening file=',filen
           open(unit=80,file=filen,form='unformatted',                          &
     &                  access='direct',recl=28800000)
           read(80,rec=1) ((imfile(l,m),l=1,4800),m=1,6000)
           close(80)
           n_s=1
           if(i.eq.na2) n_s=lat_rel2
           m_s=1
           m_e=4800
           if(j.eq.no1) m_s=lon_rel1
           if(j.eq.no2) m_e=lon_rel2
           do jj=n_s,6000
           do ii=m_s,m_e
              gtopo(lon_start(n)+ii-1,21600-(lat_start(n)+jj)+2)                &
     &             =ifile(ii,jj)
              gmask(lon_start(n)+ii-1,21600-(lat_start(n)+jj)+2)                &
     &             =imfile(ii,jj)
           enddo
           enddo
           n=n+1
           deallocate (ifile)
           deallocate (imfile)
        enddo
        enddo
      endif
      do j=nos1,nos2
         allocate (jfile(7200,3600))
         allocate (jmfile(7200,3600))
!x       filen=condir(1:mn-1)//'/'//title(n)//'\0'
!hmhj    filen=title(n)//'.DEM'
         write(filen,"(a,a)") title(n),'.DEM'
         print *,'opening file=',filen
         open(unit=80,file=filen,form='unformatted',                            &
     &                access='direct',recl=51840000)
         read(80,rec=1) ((jfile(l,m),l=1,7200),m=1,3600)
         close(80)
         titlem(n)(1:7)=title(n)
         titlem(n)(8:11)='.SRC'
!x       titlem(n)(8:11)='.slm'
!x       filen(mn:mn+18)=condir(1:nm-1)//'/'//titlem(n)//'\0'
         filen(mn:mn+18)=titlem(n)
         print *,'opening file=',filen
         open(unit=80,file=filen,form='unformatted',                            &
     &                access='direct',recl=25920000)
         read(80,rec=1) ((jmfile(l,m),l=1,7200),m=1,3600)
         close(80)
         n_s=1
         n_e=lat_rel1
         if(4.eq.na2) n_s=lat_rel2
         m_s=1
         m_e=7200
         if(j.eq.nos1) m_s=lon_rels1
         if(j.eq.nos2) m_e=lon_rels2
         do jj=n_s,n_e
         do ii=m_s,m_e
            gtopo(lon_start(n)+ii-1,21600-(lat_start(n)+jj)+2)                  &
     &            =jfile(ii,jj)
            gmask(lon_start(n)+ii-1,21600-(lat_start(n)+jj)+2)                  &
     &            =jmfile(ii,jj)
         enddo
         enddo
         n=n+1
         deallocate (jfile)
         deallocate (jmfile)
      enddo
      endif
!
         istart=lon_grid1
         iend=lon_grid2-1
         jstart=21600-lat_grid1
         jend=21600-lat_grid2+1
      print*,'start==end ',istart,iend,jstart,jend
      print*,'positive=',positive
!
      if(positive) then
         allocate (zavg(istart:iend,jstart:jend))
         allocate (zslm(istart:iend,jstart:jend))
         do i=istart,iend
         do j=jstart,jend
            zavg(i,j)=gtopo(i,j)
            zslm(i,j)=gmask(i,j)
         enddo
         enddo
         deallocate(gtopo)
         deallocate(gmask)
         do i=1,im
            if(istart.ge.ist(i).and.istart.le.ien(i)) is=i+1
            if(iend.ge.ist(i).and.iend.le.ien(i)) ie=i-1
         enddo
      else
         if(istart.le.21600.and.iend.gt.21600) then
           allocate (zavg(1:iend-21600,jstart:jend))
           allocate (zslm(1:iend-21600,jstart:jend))
           do i=21601,iend
           do j=jstart,jend
              zavg(i-21600,j)=gtopo(i,j)
              zslm(i-21600,j)=gmask(i,j)
           enddo
           enddo
           do i=1,im
              is=1
              if(iend-21600.ge.ist(i).and.iend-21600.le.ien(i)) ie=i-1
           enddo
         endif
         if(istart.gt.21600.and.istart.lt.24000) then
           allocate (zavg(istart-21600:iend-21600,jstart:jend))
           allocate (zslm(istart-21600:iend-21600,jstart:jend))
           do i=istart,iend
           do j=jstart,jend
              zavg(i-21600,j)=gtopo(i,j)
              zslm(i-21600,j)=gmask(i,j)
           enddo
           enddo
           positive=.true.
           deallocate(gtopo)
           deallocate(gmask)
           do i=1,im
            if(istart-21600.ge.ist(i).and.istart-21600.le.ien(i)) is=i+1
              if(iend-21600.ge.ist(i).and.iend-21600.le.ien(i)) ie=i-1
           enddo
         endif
         if(iend.le.21600) then
           allocate (zavg(istart+21600:iend+21600,jstart:jend))
           allocate (zslm(istart+21600:iend+21600,jstart:jend))
           do i=istart,iend
           do j=jstart,jend
              zavg(i+21600,j)=gtopo(i,j)
              zslm(i+21600,j)=gmask(i,j)
           enddo
           enddo
           positive=.true.
           deallocate(gtopo)
           deallocate(gmask)
           do i=1,im
            if(istart+21600.ge.ist(i).and.istart+21600.le.ien(i))is=i+1
              if(iend+21600.ge.ist(i).and.iend+21600.le.ien(i))ie=i-1
           enddo
         endif
      endif
      do j=1,jm
         if(jstart.ge.jst(j).and.jstart.le.jen(j)) js=j+1
         if(jend.ge.jst(j).and.jend.le.jen(j)) je=j-1
      enddo
#else
!
!...first, averaged height
!
      allocate (zavg(imn,jmn))
      allocate (zslm(imn,jmn))
      read(11,11) zavg
      read(14,12) zslm
      rewind(11)
      rewind(14)
11    format(20i4)
12    format(80i1)
!
#endif
!
#ifdef GTOPO30
159   print*,is,ie,js,je
      do i=is,ie
         do j=js,je
#else
      do i=1,im
         do j=1,jm
#endif
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
#ifdef GTOPO30
                  height = zavg(i1,j1)
#else
                  height = float(zavg(i1,j1))
#endif
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
#ifdef GTOPO30
      do i=is,ie
         do j=js,je
#else
      deallocate (zslm)
      allocate (zvar(imn,jmn))
      read(12,11) zvar
      rewind(12)
!
      do i=1,im
         do j=1,jm
#endif
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
#ifdef GTOPO30
                  height = zavg(i1,j1)
#else
                  height = float(zavg(i1,j1))
#endif
                  if(height.lt.-990.) height = 0.0
                  xw1 = xw1 + height
                  xw2 = xw2 + height ** 2
#ifndef GTOPO30
                  xv2 = xv2 + float(zvar(i1,j1)) ** 2
#endif
               enddo
            enddo
            if(xnsum.gt.1.) then
               var(i,j)=sqrt(max((xv2+xw2)/xnsum-(xw1/xnsum)**2,0.))
               do ii1=ist(i),ien(i)
                  i1 = ii1
                  if(ii1.lt.0.) i1 = i1 + imn
                  do j1=jst(j),jen(j)
#ifdef GTOPO30
                     height = zavg(i1,j1)
#else
                     height = float(zavg(i1,j1))
#endif
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
#ifdef GTOPO30
      if(.not.positive) then
           deallocate(zavg)
           deallocate(zslm)
           allocate (zavg(istart+21600:43200,jstart:jend))
           allocate (zslm(istart+21600:43200,jstart:jend))
           do i=istart,21600
           do j=jstart,jend
              zavg(i+21600,j)=gtopo(i,j)
              zslm(i+21600,j)=gmask(i,j)
           enddo
           enddo
           positive=.true.
           deallocate(gtopo)
           deallocate(gmask)
           do i=1,im
            if(istart+21600.ge.ist(i).and.istart+21600.le.ien(i))is=i-1
              ie=im
           enddo
           goto 159
      endif
      deallocate(zavg)
      deallocate(zslm)
#else
      deallocate(zavg,zvar)
#endif

      write(6,*) "! make oro slm var var4 done"
!
      return
      end
