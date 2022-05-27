      program sfc2grads
      implicit none
      integer nsig,nsfc,nflx,nfile,nfctl
      character*8 label(4)
      real*4 fhour,version
      real,allocatable :: a(:,:)
      character*250 fname
      character*100 filename
      integer idate(4),lon,lat,lev,irec,i,j,varnum
!
      lev=1
      fname='sfc'
!
      nsfc=12 
      nfile=21
      nfctl=22
!
      rewind nsfc 
      read(nsfc) label
      write(*,*) label
      read(nsfc) fhour,idate,lon,lat,version
      write(*,*) fhour,idate,lon,lat,version
      varnum=20
!
      allocate(a(lon,lat))
!
      open(nfile,file=trim(fname),form='unformatted',access='direct',           &
     &     recl=lon*lat*4)
      irec=1
!hmhj open(nfctl,file=trim(fname)//'.ctl')
      write(filename,"(a,a)") trim(fname),'.txt'
      open(nfile,file=filename)
      write(nfctl,101) trim(fname)
      write(nfctl,102)
      write(nfctl,105)
      write(nfctl,108) lon,real(lon),0.1
      write(nfctl,109) lat,real(lat),0.1
      write(nfctl,110) lev,0.0,1.0
      write(nfctl,111)
      write(nfctl,112) varnum
!
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'tsea ',1  
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'smc  ',1  
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'shel ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'stc  ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'tg3  ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'zorl ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'cv   ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'cvb  ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'cvt  ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'albe ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'slms ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'vfra ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'cano ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'f10m ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'vtyp ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'styp ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'facs ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'uust ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'ffmm ',1
      irec=irec+1
      read(nsfc) ((a(i,j),i=1,lon),j=1,lat)
      write(nfile,rec=irec) ((a(i,j),i=1,lon),j=1,lat)
      write(nfctl,113) 'ffhh ',1
      irec=irec+1
!
      write(nfctl,114)
      close(nfile)
      close(nfctl)
      deallocate(a)
!
 101  FORMAT('dset ',a)
 102  FORMAT('options big_endian')
 105  FORMAT('undef -9.99E+33')
 108  FORMAT('xdef',I5,' linear',2F9.3)
 109  FORMAT('ydef',I5,' linear',2F9.3)
 110  FORMAT('zdef',I5,' linear',2F9.3)
 111  FORMAT('tdef 1 linear 00Z01dec2002 1mo')
 112  FORMAT('vars ',I5)
 113  FORMAT(a,I5,' 99 temp data')
 114  FORMAT('endvars')
      end
