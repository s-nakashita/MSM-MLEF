      program sig2grads
      implicit none
      integer levmax,nwext
      parameter(levmax=100,nwext=512-(6+2*levmax))
      integer nsig,nsfc,nflx,nfile,nfctl
      character*8 label(4)
      real*4 fhour,sisl(2*levmax+1),ext(nwext)
      real,allocatable :: a(:,:,:),b(:,:,:)
      character*250 fname
      character*100 filename
      integer ncloud
      character*1 ccloud
      integer idate(4),lon,lat,lev,irec,ilev,i,j,icloud,nonhyd,k
      integer isfcnum,lsoil,cons2,cons4
      parameter(isfcnum=20,lsoil=2,cons2=2,cons4=4)
      character*5 sfcvar(isfcnum)
      character*100 chrcld
      data sfcvar/'tsea ','smc  ','shel ','stc  ','tg3  ','zorl ',                &
     &            'cv   ','cvb  ','cvt  ','albe ','slms ','vfra ',                &
     &            'cano ','f10m ','vtyp ','styp ','facs ','uust ',                &
     &            'ffmm ','ffhh '/
!
      ncloud=3
      fname='sig'
!
      nsig=11 
      nsfc=12
      nfile=21
      nfctl=22
!
!hmhj open(nfctl,file=trim(fname)//'.ctl')
      write(filename,"(a,a)") trim(fname),'.ctl'
      open(nfile,file=filename)
!
      call ctlhead(nsig,nfctl,fname,ncloud)
!
      rewind nsig 
      rewind nsfc
      read(nsig) 
      read(nsig)  fhour,idate,sisl,ext
      read(nsfc)
      read(nsfc)
      lon=ext(3)
      lat=ext(4)
      lev=ext(5)
      nonhyd=ext(16)
!
      allocate(a(lon,lat,lev))
      allocate(b(lon,lat,lev))
!
      open(nfile,file=trim(fname),form='unformatted',access='direct',           &
     &     recl=lon*lat*4)
      irec=1
!
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'gz   ',1  
      irec=irec+1
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'lnps ',1  
      irec=irec+1
      do ilev=1,lev
         read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
         irec=irec+1
      enddo
      write(nfctl,113) 't    ',lev
      do ilev=1,lev
         read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
         read(nsig) ((b(i,j,ilev),i=1,lon),j=1,lat)
      enddo
      do ilev=1,lev
         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
         irec=irec+1
      enddo
      write(nfctl,113) 'u    ',lev
      do ilev=1,lev
         write(nfile,rec=irec) ((b(i,j,ilev),i=1,lon),j=1,lat)
         irec=irec+1
      enddo
      write(nfctl,113) 'v    ',lev
      do icloud=1,ncloud
      do ilev=1,lev
         read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
         irec=irec+1
      enddo
      write(ccloud,'(i1)') icloud 
!hmhj write(nfctl,113) 'q'//ccloud//'---',lev
      write(chrcld,"(a,a,a)") 'q',ccloud,'   '
      write(nfctl,113) trim(chrcld),lev
      enddo
      if (nonhyd.eq.1) then
	 do ilev=1,lev
            read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
            write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
            irec=irec+1
         enddo
         write(nfctl,113) 'pn   ',lev
         do ilev=1,lev
            read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
            write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
            irec=irec+1
         enddo
         write(nfctl,113) 'tn   ',lev
         do ilev=1,lev
            read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
            write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
            irec=irec+1
         enddo
         read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
         write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
         irec=irec+1
         write(nfctl,113) 'wn   ',lev+1
      endif
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'fm2  ',1   
      irec=irec+1
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'fm2x ',1   
      irec=irec+1
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'fm2y ',1   
      irec=irec+1
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'flat ',1   
      irec=irec+1
      read(nsig) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
      write(nfctl,113) 'flon ',1   
      irec=irec+1
!      do ilev=1,lev
!         read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
!         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
!         irec=irec+1
!      enddo
!      call maxmin(a,lon*lat,lev,1,lev,'fice')
!      write(nfctl,113) 'f_ice',lev
!      do ilev=1,lev
!         read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
!         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
!         irec=irec+1
!      enddo
!      call maxmin(a,lon*lat,lev,1,lev,'frain')
!      write(nfctl,113) 'frain',lev
!      do ilev=1,lev
!         read(nsig) ((a(i,j,ilev),i=1,lon),j=1,lat)
!         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
!         irec=irec+1
!      enddo
!      call maxmin(a,lon*lat,lev,1,lev,'frime')
!      write(nfctl,113) 'frime',lev

!
      do ilev=1,isfcnum
      if (ilev.eq.2.or.ilev.eq.4) then
        read(nsfc) (((a(i,j,k),i=1,lon),j=1,lat),k=1,lsoil)
        do k=1,lsoil
          write(nfile,rec=irec) ((a(i,j,k),i=1,lon),j=1,lat)
          irec=irec+1
        enddo
        write(nfctl,113) sfcvar(ilev),lsoil
      else if (ilev.eq.10) then
        read(nsfc) (((a(i,j,k),i=1,lon),j=1,lat),k=1,cons4)
        do k=1,cons4
          write(nfile,rec=irec) ((a(i,j,k),i=1,lon),j=1,lat)
          irec=irec+1
        enddo
        write(nfctl,113) sfcvar(ilev),cons4
      else if (ilev.eq.17) then
        read(nsfc) (((a(i,j,k),i=1,lon),j=1,lat),k=1,cons2)
        do k=1,cons2
          write(nfile,rec=irec) ((a(i,j,k),i=1,lon),j=1,lat)
          irec=irec+1
        enddo
        write(nfctl,113) sfcvar(ilev),cons2
      else
        read(nsfc) ((a(i,j,1),i=1,lon),j=1,lat)
        write(nfile,rec=irec) ((a(i,j,1),i=1,lon),j=1,lat)
        write(nfctl,113) sfcvar(ilev),1
        irec=irec+1
      endif
      enddo
!
      write(nfctl,114)
      close(nfile)
      close(nfctl)
      close(nsfc)
      close(nsig)
      deallocate(a)
      deallocate(b)
!
 112  format('vars ',i5)
 113  format(a,i5,' 99 temp data')
 114  format('endvars')
      end
! 
      subroutine ctlhead(nsig,nfctl,fname,ncloud)
      implicit none
      integer levmax,nwext
      parameter(levmax=100,nwext=512-(6+2*levmax))
      integer nsig,nfctl
      real*4 fhour,sisl(2*levmax+1),ext(nwext)
      real,allocatable :: clon(:,:),clat(:,:),flat(:)
      character*250 fname
      integer ncloud
      integer idate(4),igrd1,jgrd1,levs,i,j,nonhyd,varnum,nskip
      real proj,truth,orient,cenlat,cenlon,grdleft,grdbotm,delx,dely      
      integer imp,jmp
      real strlonp,dlonp,strlatp,dlatp,dlon,dlat,strlat,strlon
      real rlon1,rlon2,rlat1,rlat2,endlon
      rewind nsig
      read(nsig) 
      read(nsig) fhour,idate,sisl,ext
      igrd1  =ext(3)
      jgrd1  =ext(4)
      levs   =ext(5)
      proj   =ext(7)
      truth  =ext(8)
      orient =ext(9)
      cenlat =ext(10)
      cenlon =ext(11)
      grdleft=ext(12)
      grdbotm=ext(13)
      delx   =ext(14)
      dely   =ext(15)
      nonhyd =ext(16)
!      varnum=5+ncloud+nonhyd*3+5+3+20
      varnum=5+ncloud+nonhyd*3+5+20
!
      allocate(clon(igrd1,jgrd1))
      allocate(clat(igrd1,jgrd1))
      allocate(flat(jgrd1))
!
      nskip=2+3*levs+ncloud*levs+3+nonhyd*(3*levs+1)
      do i=1,nskip
         read(nsig)
      enddo
      read(nsig) clat
      read(nsig) clon
      rlon1=clon(1,1)
      rlon2=clon(igrd1,jgrd1)
      rlat1=clat(1,1)
      rlat2=clat(igrd1,jgrd1)
      strlon=rlon1*180./acos(-1.)
      endlon=rlon2*180./acos(-1.)
      dlon=(endlon-strlon)/float(igrd1-1)
      do j=1,jgrd1
        flat(j)=clat(1,j)*180./acos(-1.)
      enddo
      strlat=flat(1)
      dlat=flat(2)-flat(1)
!
      write(nfctl,101) trim(fname)
      write(nfctl,102)
      write(nfctl,105)
!      write(nfctl,106)
 101  format('dset ^',a)
 102  format('options big_endian')
 105  format('undef -9.99e+33')
 106  format('options yrev')
!
      if( proj.eq.0.0 ) then
        write(nfctl,108) igrd1,strlon,dlon
        write(nfctl,110) jgrd1
        write(nfctl,111) (flat(j),j=1,jgrd1)
      else if( proj.eq.1.0 .or. proj.eq.-1.0 ) then
        write(nfctl,107) igrd1,jgrd1,grdleft,grdbotm,orient,delx
        imp=360.*111/delx
        strlonp=0.0
        dlonp=360./imp
        jmp=imp/4
        strlatp=0.0
        dlatp=dlonp
        write(nfctl,108) imp,strlonp,dlonp
        write(nfctl,109) jmp,strlatp,dlatp
      else if( proj.eq.2.0 .or. proj.eq.-2.0 ) then
        write(nfctl,117) igrd1,jgrd1,cenlat,cenlon,grdleft,grdbotm,                      &
     &                truth,truth,orient,delx,dely
        imp=360.*111/delx
        strlonp=0.0
        dlonp=360./imp
        jmp=imp/4
        strlatp=0.0
        dlatp=dlonp
        write(nfctl,108) imp,strlonp,dlonp
        write(nfctl,109) jmp,strlatp,dlatp
      else
        write(nfctl,108) igrd1,strlon,dlon
        write(nfctl,109) jgrd1,strlat,dlat
      endif
 107  format('pdef',2i5,' nps',4f15.5)
 108  format('xdef',i5,' linear',2f9.3)
 109  format('ydef',i5,' linear',2f9.3)
 110  format('ydef',i5,' levels')
 111  format(10f8.3)
 117  format('pdef',2i5,' lcc',9f9.3)
!
      write(nfctl,118) levs,0.0,1.0
      write(nfctl,119)
 118  format('zdef',I5,' linear',2F9.3)
 119  format('tdef 99 linear 00z01dec2002 1mo')
      write(nfctl,120) varnum
 120  format('vars ',i5)
      end
!
      subroutine maxmin(a,len,k,k1,k2,ch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  maxmin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do print maximum and minimum of a given array.
!
! program history log:
!
! usage:    call  maxmin(a,len,k,k1,k2,ch)
!   input argument list:
!     a         - array for computing max and min (len,k)
!     len       - the first dimension of a
!     k         - the second dimension of a
!     k1        - lower limit of second dimension to print
!     k2        - upper limit to print
!     ch        - charcter string to print
!                 fpl and fml
!
!   output argument list:
!
!   input files: none
!
!   output files:
!     standard output
!
!   subprograms called:
!     intrinsic functions: amax1 amin1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      dimension a(len,k)
      character ch*(*)
!
      do 100 j=k1,k2
      aamax = a(1,j)
      aamin = a(1,j)
      do 10 m=1,len
      aamax = max( aamax, a(m,j) )
      if (aamax.eq.a(m,j)) mmax = m
      aamin = min( aamin, a(m,j) )
      if (aamin.eq.a(m,j)) mmin = m
10    continue
!     write(0,*)ch,' has max=',aamax,' min=',aamin,' at k=',j,mmax,mmin
      print   *,ch,' has max=',aamax,' min=',aamin,' at k=',j
100   continue
      return
      end



