      subroutine ctlhead(nn,im,jm,km,proj,tru,ori,                      &
     &                  clat,clon,pi,pj,dx,dy,                          &
     &                  ihr,iday,imon,iyr,ifh,                          &
     &                  strlon,dlon,strlat,dlat,                        &
     &                  rlat,iz)
!$$$
      dimension rlat(jm),iz(km)
      character*2 hour,day
      character*3 mon(12)
      data mon/'jan','feb','mar','apr','may','jun',                     &
     &         'jul','aug','sep','oct','nov','dec'/
      save mon
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print *,' write control header to ctlhead '
      write(nn,101)
      write(nn,102)
      if( proj.eq.8 ) then
        write(nn,123)
      else
        write(nn,103)
      endif
      write(nn,104)
      write(nn,105)
      write(nn,106)
 101  format('dset datafile')
 102  format('dtype grib')
 103  format('options template')
 123  format('options yrev template')
 104  format('index mapfile')
 105  format('undef -9.99e+33')
 106  format('title exp1')
!
      if( proj.eq.0.0 ) then
        write(nn,108) im,strlon,dlon
        write(nn,110) jm
        write(nn,111) (rlat(j),j=1,jm)
      else if( proj.eq.1.0 .or. proj.eq.-1.0 ) then
        write(nn,107) im,jm,pi,pj,ori,dx
        imp=360.*111/dx
        strlonp=0.0
        dlonp=360./imp
        jmp=imp/4
        strlatp=0.0
        dlatp=dlonp
        write(nn,108) imp,strlonp,dlonp
        write(nn,109) jmp,strlatp,dlatp
      else if( proj.eq.2.0 .or. proj.eq.-2.0 ) then
        write(nn,117) im,jm,clat,clon,pi,pj,tru,tru,ori,dx,dy
        imp=360.*111/dx
        strlonp=0.0
        dlonp=360./imp
        jmp=imp/4
        strlatp=0.0
        dlatp=dlonp
        write(nn,108) imp,strlonp,dlonp
        write(nn,109) jmp,strlatp,dlatp
      else 
        write(nn,108) im,strlon,dlon
        write(nn,109) jm,strlat,dlat
      endif
 117  format('pdef',2i5,' lcc',9f9.3)
 107  format('pdef',2i5,' nps',4f11.5)
 108  format('xdef',i5,' linear',2f9.3)
 109  format('ydef',i5,' linear',2f9.3)
 110  format('ydef',i5,' levels')
 111  format(10f8.3)
!
      write(nn,112) km
      write(nn,113) (iz(k),k=1,km)
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
      if( iyr.lt.100 ) iyr=2050-mod(2050-iyr,100)
      write(nn,114) hour,day,mon(imon),iyr,ifh
 112  format('zdef',i5,' levels')
 113  format(10i8)
 114  format('tdef 99 linear ',a2,'z',a2,a3,i4,i10,'hr')
      write(nn,115)
 115  format('vars totalnum')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      return
      end
