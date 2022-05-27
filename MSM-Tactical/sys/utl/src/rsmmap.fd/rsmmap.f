      program rmap
c
c creat map for display the selected domain
c
      common /comrloc/ igrd,jgrd
     1,rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd
c
      real , allocatable :: flat(:)
      real , allocatable :: flatm(:)
      real , allocatable :: flon(:)
      real , allocatable :: fi(:)
      real , allocatable :: fj(:)
      integer , allocatable :: iz(:)
c
      namelist /namloc/ igrd,jgrd
     1,rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd
c
      read(5,namloc)
      write(6,namloc)
      igrd1=igrd+1
      jgrd1=jgrd+1
      lngrd=igrd1*jgrd1

      allocate( flat(lngrd) )
      allocate( flatm(lngrd) )
      allocate( flon(lngrd) )
      allocate( fi(lngrd) )
      allocate( fj(lngrd) )
      allocate( iz(lngrd) )

      call getgrd(flat,flon,lngrd)
      print *,' flat ',(flat(i),i=1,10)
      print *,' flon ',(flon(i),i=1,10)
c     
      nout=51
      open(nout,file='rmap.data',status='unknown',form='unformatted')
      mout=52
      open(mout,file='rmap.parm',status='unknown',form='formatted')
      n=0
      do j=1,jgrd1
        do i=1,igrd1
          n=n+1
          fi(n)=i
          fj(n)=j
        enddo
      enddo
      write(nout) (fi(i),i=1,lngrd)
      write(nout) (fj(i),i=1,lngrd)
      write(nout) (flat(i),i=1,lngrd)
      write(nout) (flon(i),i=1,lngrd)
      close(nout)
c
      nctl=61
      open(nctl,file='rmap.ctl',status='unknown',form='formatted')
c
      ifh=0
      ihr=0
      iday=1
      imon=1
      iyr =99
      strlon=flon(1)*180./acos(-1.)
      endlon=flon(igrd1)*180./acos(-1.)
      if(endlon.lt.strlon) endlon=endlon+360.
      dlon=(endlon-strlon)/float(igrd1-1)
      print *,' strlon endlon dlon ',strlon,endlon,dlon
      do j=1,jgrd1
        i=(j-1)*igrd1+1
        flatm(j)=flat(i)*180./acos(-1.)
      enddo
      strlat=flatm(1)
      dlat=flatm(2)-flatm(1)
      iz(1)=0
      levr=1
      dx=rdelx/1000.
      call ctlhead(nctl,igrd1,jgrd1,levr,
     1             rproj,rlftgrd,rbtmgrd,rorient,dx,
     1             ihr,iday,imon,iyr,ifh,strlon,dlon,strlat,dlat,
     2             flatm,iz)
c
      write(nctl,100)
 100  format('vars 4')
      write(nctl,101)
      write(nctl,102)
      write(nctl,103)
      write(nctl,104)
 101  format('fi  0 99 i grid')
 102  format('fj  0 99 j grid')
 103  format('flat  0 99 latitudes')
 104  format('flon  0 99 longitudes')
      write(nctl,300)
 300  format('endvars')
c
      iproj=rproj
      if(iproj.eq.0) then
        flon1=strlon
        flon2=endlon
      else
        flon1=flon(1)
        flon2=flon(1)
        flat1=flat(1)
        flat2=flat(1)
        do n=2,lngrd
          flon1=min(flon1,flon(n))
          flon2=max(flon2,flon(n))
          flat1=min(flat1,flat(n))
          flat2=max(flat2,flat(n))
        enddo
        flon1=flon1*180./acos(-1.)
        flon2=flon2*180./acos(-1.)
        flat1=flat1*180./acos(-1.)
        flat2=flat2*180./acos(-1.)
        rorient=mod(rorient,360.)
        flon1=mod(flon1,360.)
        flon2=mod(flon2,360.)
        if( rorient.lt.0.0 ) rorient=rorient+360.
        if( flon1.lt.0.0 ) flon1=flon1+360.
        if( flon2.lt.0.0 ) flon2=flon2+360.
        d1=abs(flon1-rorient)
        d2=abs(flon2-rorient)
        dd=max(d1,d2)
        dd=min(180.,dd)
        flon1=rorient-dd
        flon2=rorient+dd
      endif
      write(mout,123) igrd1,jgrd1,iproj,flon1,flon2,flat1,flat2
 123  format(3i5,4g14.6)
c
      stop
      end

      subroutine ctlhead(nn,im,jm,km,proj,pi,pj,or,dx,
     1                  ihr,iday,imon,iyr,ifh,
     2                  strlon,dlon,strlat,dlat,
     3                  rlat,iz)
c$$$
      dimension rlat(jm),iz(km)
      character*2 hour,day
      character*3 mon(12)
      data mon/'jan','feb','mar','apr','may','jun',
     1         'jul','aug','sep','oct','nov','dec'/
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print *,' write control header to ctlhead '
      write(nn,101)
      write(nn,102)
      write(nn,105)
      write(nn,106)
 101  format('dset ^rmap.data')
 102  format('options sequential')
 105  format('undef -9.99e+33')
 106  format('title exp1')
c
      if( proj.eq.0.0 ) then
        write(nn,108) im,strlon,dlon
        write(nn,110) jm
        write(nn,111) (rlat(j),j=1,jm)
      else if( proj.eq.1.0 ) then
        write(nn,107) im,jm,pi,pj,or,dx
        imp=360.*111/dx
        strlonp=0.0
        dlonp=360./imp
        jmp=imp/4
        strlatp=0.0
        dlatp=dlonp
        write(nn,108) imp,strlonp,dlonp
        write(nn,109) jmp,strlatp,dlatp
      else if( proj.eq.-1.0 ) then
        write(nn,1071) im,jm,pi,pj,or,dx
        imp=360.*111/dx
        strlonp=0.0
        dlonp=360./imp
        jmp=imp/4
        strlatp=-90.0
        dlatp=dlonp
        write(nn,108) imp,strlonp,dlonp
        write(nn,109) jmp,strlatp,dlatp
      else
        print *,' Error in projection '
        call abort
      endif
 107  format('pdef',2i5,' nps',4g14.6)
 1071 format('pdef',2i5,' sps',4g14.6)
 108  format('xdef',i5,' linear',2g14.6)
 109  format('ydef',i5,' linear',2g14.6)
 110  format('ydef',i5,' levels')
 111  format(5g14.6)
c
      write(nn,112) km
 112  format('zdef',i5,' levels 0 ')
      if( ihr.lt.10 ) then
        write(hour,90) ihr
      else
        write(hour,91) ihr
      endif
      if( iday.lt.10 ) then
        write(day,90) iday
      else
        write(day,91) iday
      endif
  90  format('0',i1)
  91  format(i2)
      if( ifh.eq.0 ) ifh=1
      write(nn,114) hour,day,mon(imon),iyr,ifh
 114  format('tdef 1 linear ',a2,'z',a2,a3,i2,i10,'hr')
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end


      subroutine getgrd(flat,flon,lngrd)
      dimension flat(lngrd),flon(lngrd)
c
      common /comrloc/ igrd,jgrd
     1,rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd
c
      print 1234
 1234 format(' ==== in routine setgrd === ')
c
      igrd1=igrd+1
      jgrd1=jgrd+1
      hfpi = dasin(1.0d0)
      qtpi = hfpi * 0.5
      pi = 2.0 * hfpi
      twopi = 2.0 * pi
      rad = pi / 180.
      delx = rdelx
      dely = rdely
c
c --------- setup regional lat/lon and map factor -----
c
c if proj=0  do mercater projection
c if proj=1  do north polar projection
c if proj=-1 do south polar projection
c
      nproj = rproj
c
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
c ++++++++++++++++++++++++++++++++++++++
c polar projection
c ++++++++++++++++++++++++++++++++++++++
      truth  = rtruth * rad
      truth  = nproj * truth
      orient  = rorient * rad
      dlamda0 = orient + hfpi
      a2 =  6371200. * ( 1.0 + sin(truth) )
      radlat = rcenlat * rad
      radlon = rcenlon * rad - dlamda0
      radlat = nproj * radlat
      radlon = nproj * radlon
      yyy = a2 * cos( radlat )/(1. + sin( radlat ) )
      cenlat = rcenlat
      if( abs(cenlat) .eq. 90. ) then yyy = 0.0
      y00 = yyy * sin( radlon ) - ( rbtmgrd -1.) * dely
      x00 = yyy * cos( radlon ) - ( rlftgrd -1.) * delx
      print *,' delx x00 y00 ',delx,x00,y00
c
c =========
c           lat loop
      do 100 j = 1,jgrd1
      lats = j
      ijlats = (lats-1)*igrd1
      ys = y00 + (lats-1)*dely
c
      do 100 i=1,igrd1
      x = x00 + (i-1)*delx
      if( x .gt. 0. 0 ) then
         flons = atan(ys/x)
      else if ( x .lt. 0. 0 ) then
         flons = pi + atan(ys/x)
      else
         flons = hfpi
         if( ys .lt. 0. 0 ) flons = flons * 3.0
      endif
      flons = nproj * flons + dlamda0
      flons = mod(flons,twopi)
      if(flons.lt.0. 0) flons = twopi + flons
c
      rsoa2 = sqrt( x*x + ys*ys )/a2
      flats = hfpi - 2.0  * atan(rsoa2)
      flat(ijlats+i) = nproj * flats
      flon(ijlats+i) = flons
c
 100  continue
c
      else if ( nproj.eq.0 ) then
c
c ++++++++++++++++++++++++++++
c do mercater
c ++++++++++++++++++++++++++++
      truth  = rtruth * rad
      cenlat = rcenlat * rad
      cenlon = rcenlon * rad 
      a2 =  6371200. * cos( truth ) 
      x0 = 0.0
      y0 = a2 * log( abs( tan( qtpi + 0.5 * cenlat ) ) )
      x00 = - ( rlftgrd - 1.0 ) * delx
      y00 = - ( rbtmgrd - 1.0 ) * dely
      dlamda0 = 0.0
c
      do 200 j = 1,jgrd1
      lats = j
      ijlats = (lats-1)*igrd1
      ys = y00 + (lats-1)*dely + y0
c
       do 200 i=1,igrd1
         x = x00 + (i-1)*delx + x0
         flons = x / a2 + cenlon
c
         flats = 2.0 *( atan( exp( ys/a2 ) ) - qtpi )
         flat(ijlats+i) = flats
         flon(ijlats+i) = flons
c
 200  continue
      print *, 'flat ',(flat(i),i=1,10)
      print *, 'flon ',(flon(i),i=1,10)
c
      endif
c
c
      return
      end
