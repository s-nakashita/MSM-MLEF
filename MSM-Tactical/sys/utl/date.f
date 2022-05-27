      program datech
      CHARACTER*8 label(4)
      REAL*4 fhour 
      dimension idate(4)
      read(11) label
      read(11) fhour,idate
      ihour=nint(fhour)
      write(6,100) idate(1),idate(2),idate(3),idate(4),ihour
100   format(4i6,i10)
      stop
      end
