      subroutine crhtab(rhcl,ier,me)
!---------------------------------------------------------------------
!..  cld-rh relations obtained from mitchell-hahn procedure, here read
!     cld/rh tuning tables for day 0,1,...,5 and merge into 1 file..
!                         .............k.a.c.   mar 93
!     use only one table (day 1) for all fcst hrs....k.a.c. feb 94
!...    4 cld types .... kac  feb96
!...    smooth out last bunch of bins of the tables...kac aug97
!    output:
!        rhcl - tuning tables for all forecast days
!        ier  - =1 if tables available.. =-1 if no tables
!--------------------------------------------------------------------
      use machine , only : kind_io4,kind_io8
!c      include 'mpi_inc.h'
!rh1t parameter (mcld=3,nseal=2,ida=6,
!mcl3 parameter (mcld=3,nseal=2,ida=1,
      parameter (mcld=4,nseal=2,ida=1,                                  &
     &           nbin=100,nlon=2,nlat=4)
      real (kind=kind_io8) rhfd(nbin,nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rrhfd(nbin,nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rtnffd(nbin,nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rrnffd(nbin,nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rhcf(nbin,nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rtnfcf(nbin,nlon,nlat,mcld,nseal)
      integer kpts(nlon,nlat,mcld,nseal)
      integer kkpts(nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rhc(nlon,nlat,mcld,nseal)
      real (kind=kind_io8) rhcl (nbin,nlon,nlat,mcld,nseal,ida)
      real (kind=kind_io8) rhcla(nbin,nlon,nlat,mcld,nseal)
      integer icdays(15),idate(4),me
      real(kind=kind_io4) fhour
      real(kind=kind_io4) rhfd4(nbin,nlon,nlat,mcld,nseal)
      real(kind=kind_io4) rtnffd4(nbin,nlon,nlat,mcld,nseal)
!...........................  begin here  ..............
      ier = 1
      do 8000 itim=1,ida
      icfq = 43 + itim-1
      rewind icfq
!mcl3       nclds=1,2,3 (l,m,h)..jsl=1,2 (land,sea)
!mcl4       mcld=1,2,3,4 (bl,l,m,h)
      binscl = 1./nbin
      do 1000 m=1,nseal
       do 1000 l=1,mcld
        do 1000 k=1,nlat
         do 1000 j=1,nlon
          do 1000 i=1,nbin
           rrhfd(i,j,k,l,m) = 0.
           rrnffd(i,j,k,l,m) = 0.
 1000 continue
      do 1001 m=1,nseal
       do 1001 l=1,mcld
        do 1001 k=1,nlat
         do 1001 j=1,nlon
          kkpts(j,k,l,m) = 0
 1001 continue
!....  read the data off the rotating file
      read (icfq,err=998,end=999) nbdayi,icdays
      if (me.eq.0) print 11,nbdayi
      do 53 ld=1,nbdayi
       id = icdays(ld) / 10000
       im = (icdays(ld)-id*10000) / 100
       iy = icdays(ld)-id*10000-im*100
       if (me.eq.0) print 51,id,im,iy
   53 continue
      read (icfq,err=998,end=999) fhour,idate
!sela print 3003,idate,fhour,itim
      do 1300 kd=1,nbdayi
       read (icfq) rhfd4
       rhfd=rhfd4
       read (icfq) rtnffd4
       rtnffd=rtnffd4
       read (icfq) kpts

       do 1002 m=1,nseal
        do 1002 l=1,mcld
         do 1002 k=1,nlat
          do 1002 j=1,nlon
           do 1002 i=1,nbin
            rrhfd(i,j,k,l,m) = rrhfd(i,j,k,l,m) + rhfd(i,j,k,l,m)
            rrnffd(i,j,k,l,m) = rrnffd(i,j,k,l,m)+rtnffd(i,j,k,l,m)
 1002  continue
       do 1003 m=1,nseal
        do 1003 l=1,mcld
         do 1003 k=1,nlat
          do 1003 j=1,nlon
           kkpts(j,k,l,m) = kkpts(j,k,l,m) + kpts(j,k,l,m)
 1003  continue
 1300 continue
!
      do 1004 m=1,nseal
       do 1004 l=1,mcld
        do 1004 k=1,nlat
         do 1004 j=1,nlon
          do 1004 i=1,nbin
           rhcf(i,j,k,l,m) = rrhfd(i,j,k,l,m)
           rtnfcf(i,j,k,l,m) = rrnffd(i,j,k,l,m)
 1004 continue
      do 1005 m=1,nseal
       do 1005 l=1,mcld
        do 1005 k=1,nlat
         do 1005 j=1,nlon
          kpts(j,k,l,m) = kkpts(j,k,l,m)
 1005 continue
!.....  compute the cumulative frequency distribution..
      do 200 n=1,nseal
       do 200 k=1,mcld
        do 200 l=1,nlat
         do 200 j=1,nlon
          do 190 i=2,nbin
           rhcf(i,j,l,k,n) = rhcf(i-1,j,l,k,n) + rhcf(i,j,l,k,n)
           rtnfcf(i,j,l,k,n)=rtnfcf(i-1,j,l,k,n) + rtnfcf(i,j,l,k,n)
  190     continue
  200 continue
      do 300 n=1,nseal
       do 300 l=1,nlat
        do 300 j=1,nlon
         do 300 k=1,mcld
          do 300 i=1,nbin
           if (kpts(j,l,k,n).gt.0) then
            rhcf(i,j,l,k,n) = rhcf(i,j,l,k,n) / kpts(j,l,k,n)
            rtnfcf(i,j,l,k,n) = rtnfcf(i,j,l,k,n) / kpts(j,l,k,n)
!...  cause we mix calculations of rh retune with cray and ibm words
!      the last value of rhcf is close to but ne 1.0,
!      so we reset it in order that the 360 loop gives compleat tabl
!...  rtnfcf caused couple of problems, seems to be ever so slightly
!      gt 1.0
            if (i.eq.nbin) then
             rhcf(i,j,l,k,n) = 1.0
            end if
            if (rtnfcf(i,j,l,k,n).ge.1.0) then
             rtnfcf(i,j,l,k,n) = 1.0
            end if
           else
            rhcf(i,j,l,k,n) = -0.1
            rtnfcf(i,j,l,k,n) = -0.1
           end if
  300 continue
      do 255 nsl=1,nseal
       do 255 kcl=1,mcld
!sela   print 264,kcl,nsl
!sela   print 265,((kpts(i,l,kcl,nsl),i=1,nlon),l=1,nlat)
  255 continue
      do 360 nsl=1,nseal
       do 360 k=1,mcld
        do 360 l=1,nlat
         do 360 j=1,nlon
          if (kpts(j,l,k,nsl).le.0) go to 317
          do 320 i=1,nbin
           icrit = i
           if (rhcf(i,j,l,k,nsl).ge.rtnfcf(1,j,l,k,nsl)) go to 350
  320     continue
!... no critical rh
  317     icrit=-1
          if (me.eq.0) print 210,l,j,nsl
          if (me.eq.0) print 202
          do 135 i=1,nbin
           if (me.eq.0) print 203,rhcf(i,j,l,k,nsl),rtnfcf(i,j,l,k,nsl)
  135     continue
  350     rhc(j,l,k,nsl) = icrit * binscl
  360 continue
!sela do 1210 nsl=1,nseal
!sela  do 1210 k=1,mcld
!sela   print 1221,k,nsl
!sela   do 1210 l=1,nlat
!sela    print 211,(rhc(j,l,k,nsl),j=1,nlon)
!sela  1210 continue
      do 450 nsl=1,nseal
       do 450 ken=1,mcld
        do 450 l=1,nlat
         do 450 jl=1,nlon
          do 400 i=1,nbin
           rhcl(i,jl,l,ken,nsl,itim) = -0.1
  400     continue
  450 continue
      do 751 nsl=1,nseal
       do 751 ken=1,mcld
        do 751 l=1,nlat
         do 751 jl=1,nlon
          if (kpts(jl,l,ken,nsl).le.0) go to 751
          do 753 i=1,nbin
           do 755 j=1,nbin
            if (rhcf(j,jl,l,ken,nsl).ge.rtnfcf(i,jl,l,ken,nsl)) then
             rhcl(i,jl,l,ken,nsl,itim) = j*binscl
             go to 753
            end if
  755      continue
  753     continue
  751 continue
      do 3000 lon=1,nlon
       do 3000 lat=1,nlat
        do 3000 nc=1,mcld
         do 3000 nsl=1,nseal
         isat = 0
         do 67 it=1,nbin
          cfrac = binscl * (it-1)
          if (rhcl(it,lon,lat,nc,nsl,itim).lt.0.) then
           print 1941,it,nsl,nc,lat,lon
           stop
          end if
          if (it.lt.nbin.and.rtnfcf(it,lon,lat,nc,nsl).ge.1.) then
           if (isat.le.0) then
            isat = it
            rhsat = rhcl(it,lon,lat,nc,nsl,itim)
            clsat = cfrac
           end if
           rhcl(it,lon,lat,nc,nsl,itim) =                               &
     &               rhsat + (1.-rhsat)*(cfrac-clsat)/(1.-clsat)
          end if
          if (it.eq.nbin) rhcl(it,lon,lat,nc,nsl,itim) = 1.
   67    continue
 3000 continue
!... smooth out the table as it reaches rh=1.0, via linear interpolation 
!      between location of rh ge .98 and the nbin bin (where rh=1.0)
!... previously rh=1.0 occurred for many of the latter bins in the 
!      table, thereby giving a cloud value of less then 1.0 for rh=1.0
      nb=nbin-2
      do 4000 lon=1,nlon
       do 4000 lat=1,nlat
        do 4000 nc=1,mcld
         do 4000 nsl=1,nseal
         do 4167 it=1,nbin
          rhcla(it,lon,lat,nc,nsl)=rhcl(it,lon,lat,nc,nsl,itim)
 4167    continue
         do 4067 it=1,nb
          ibs=it
          cfrac=binscl*ibs
          if (rhcl(it,lon,lat,nc,nsl,itim).ge..98) then
!c           print 4011,nsl,nc,lat,lon,ibs,nbin
!c 4011      format (1h ,'nsl,nc,lat,lon,ibs,nbin=',6i4)
           do 4068 kt=ibs,nbin
            cstem=binscl*kt
            rhcla(kt,lon,lat,nc,nsl) =                                  &
     &       rhcl(ibs,lon,lat,nc,nsl,itim)+                             &
     &       (rhcl(nbin,lon,lat,nc,nsl,itim)                            &
     &                      -rhcl(ibs,lon,lat,nc,nsl,itim))*            &
     &       (cstem-cfrac)/(1.-cfrac)
!           if (nc.eq.2.and.lat.eq.2.and.lo.eq.1.and.nsl.eq.2) then
!            print 4012,kt,cstem,cfrac,rhcl(ibs,lon,lat,nc,nsl,itim),    &
!    &                rhcla(kt,lon,lat,nc,nsl)
!  4012 format(1h ,'kt,cs,cf,rhibs,rhcla=',i5,4f12.8)
!           end if
 4068     continue
          go to 4000
          end if
 4067    continue
 4000 continue
!... restore table data to preferred location..
      do 4200 lon=1,nlon
       do 4200 lat=1,nlat
        do 4200 nc=1,mcld
         do 4200 nsl=1,nseal
          do 4200 it=1,nbin
           rhcl(it,lon,lat,nc,nsl,itim)= rhcla(it,lon,lat,nc,nsl)
 4200 continue
 8000 continue
      do 8001 ken=1,ida
       icfq = 42 + ken
       rewind icfq
 8001 continue
      if (me.eq.0) print *,'completed crhtab for cloud tuninig tables'
      return
  998 print 988,itim
      ier = -1
      return
  999 print 989,itim
      ier = -1
      return
   11 format(1h ,'  from crhtab days on file =',i5)
   51 format(1h ,'  from crhtab archv data from da,mo,yr=',3i4)
  202 format(1h0,' model rh ',' obs rtcld')
  203 format(2f10.2)
  210 format(1h ,' no crit rh for lat=',i3,' and lon band=',i3,         &
     &           ' land(=1) sea=',i3)
  211 format(1h ,15f6.2)
  264 format(1h ,' number of gg points used in each area..by latitude', &
     &           '..for cloud type=',i4,'sealand=',i2)
  265 format(1h ,15i8)
  988 format(1h ,'from crhtab error reading tables for time=',i4)
  989 format(1h ,'from crhtab e.o.f reading tables for time=',i4)
 1221 format(1h0,' critical rh for lon,lat arrays for cld type=',i3,    &
     &           ' land(=1) sea=',i3)
 1941 format(1h ,' neg rhcl for it,nsl,nc,lat,lon=',5i4,'...stoppp..')
 3003 format(5x,'...last date/time and current itim',/,10x,             &
     &       4i15,f7.1,i6)
      end
