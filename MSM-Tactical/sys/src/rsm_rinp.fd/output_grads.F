      subroutine output_grads(im,jm,hh,time,chr)
!
! output to file unit fh for hh(im,jm) with name chr for grads control file
!
      implicit none
!
      integer	fh,im,jm
      real(4) 	hh(im,jm)
      character*(*)	chr
      character*(*)	time
!
      real*4	h4(im,jm)
      integer	n,i,j
      integer, save :: out,ctl,sav,total_record

      if( time.eq.'first' ) then
        total_record=0
        out=50
        ctl=80
        sav=81
        open(unit=out,file='rotate.out',form='unformatted')
        open(unit=80,file='rotate.ctl',form='formatted')
        open(unit=81,file='rotate.sav',form='formatted')
        write(ctl,'(a)') 'dset rotate.out'
        write(ctl,'(a)') 'options sequential'
        write(ctl,'(a)') 'undef -9.99E+33'
        write(ctl,'(a)') 'title SOLID_ROTATE'
        write(ctl,'(a,i5,a)') 'xdef',im,' linear 1 1'
        write(ctl,'(a,i5,a)') 'ydef',jm,' linear 1 1'
        write(ctl,'(a)') 'zdef 1 levels 0'
        write(ctl,'(a)') 'tdef 1 linear 00Z01JAN2000 1hr'
      endif

      write(sav,'(a)') chr
      total_record=total_record+1 

      call maxmin(hh,im*jm,1,1,1,chr)
      do j=1,jm
        do i=1,im
          h4(i,j)=hh(i,j)
        enddo
      enddo
      write(out) ((h4(i,j),i=1,im),j=1,jm)

      if( time.eq.'lastd' ) then
        write(ctl,'(a,i4)') 'vars ',total_record
        rewind sav
        do n=1,total_record
          read(sav,'(a)') chr
          write(ctl,'(a)') chr
        enddo
        write(ctl,'(a)') 'endvars'
      endif

      return
      end

