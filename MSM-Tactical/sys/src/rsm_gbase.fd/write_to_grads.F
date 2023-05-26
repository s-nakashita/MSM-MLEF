      subroutine write_to_grads(b,lon,lat,lev,fname)
      use machine , only : kind_io4
      implicit none
      integer lon,lat,lev,nfile,irec,ilev,i,j
      real b(lon,lat,lev)
      real(kind=kind_io4) a(lon,lat,lev)
      character fname*(*),filename*100
      a=b
      nfile=999
!hmhj open(nfile,file=trim(fname)//'.txt')
      write(filename,"(a,a)") trim(fname),'.txt'
      open(nfile,file=filename)
      do ilev=1,lev
      do j=1,lat
         write(nfile,100) (a(i,j,ilev),i=1,lon)
      enddo
      enddo
      close(nfile) 
 100  format(221f10.3)
      open(nfile,file=trim(fname),form='unformatted',access='direct',                   &
     &     recl=lon*lat*4)
      irec=1
      do ilev=1,lev
         call ful2sgl(b,a,lon*lat)
         write(nfile,rec=irec) ((a(i,j,ilev),i=1,lon),j=1,lat)
         irec=irec+1
      enddo
      close(nfile)
!hmhj open(nfile,file=trim(fname)//'.ctl')
      write(filename,"(a,a)") trim(fname),'.ctl'
      open(nfile,file=filename)
      write(nfile,101) trim(fname)
      write(nfile,102) 
      write(nfile,105)
      write(nfile,108) lon,real(lon),0.1
      write(nfile,109) lat,real(lat),0.1
      write(nfile,110) lev,0.0,1.0
      write(nfile,111)
      write(nfile,112)
      write(nfile,113) lev
      write(nfile,114)
 101  FORMAT('dset ',a20)
 102  FORMAT('options big_endian')
 105  FORMAT('undef -9.99E+33')
 108  FORMAT('xdef',I5,' linear',2F9.3)
 109  FORMAT('ydef',I5,' linear',2F9.3)
 110  FORMAT('zdef',I5,' linear',2F9.3)
 111  FORMAT('tdef 1 linear 00Z01dec2002 1mo')
 112  FORMAT('vars 1')
 113  FORMAT('temp',I5,' 99 temp data')
 114  FORMAT('endvars')
      close(nfile)
      end
