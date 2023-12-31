      subroutine preadg2sfc(ifile,ijmax,nfld,out,iscale,iefs)
      use grib_mod
#include <paramodel.h>
#include <rscomloc.h>
      type(gribfield) :: gfld
      integer,dimension(200) :: jids,jpdt,jgdt
      logical :: unpack=.true.
!
      integer :: ijmax,iret,iscale,iefs
      integer :: nfld
      real    :: out(ijmax,nfld)
      integer :: lugi,j
      integer :: jdisc0(nfld),jpdt1(nfld),jpdt2(nfld)
      integer :: jpdt10(nfld),jpdt11(nfld),jpdt12(nfld)
      integer :: jpdtn,jgdtn,n,jdisc,ifile
      integer :: nterr
! scaling level
      integer :: tmplev, iscl
! 
! 
      jpdt1=-9999
      jpdt2=-9999
      jpdt10=-9999
      jpdt11=-9999
      jpdt12=-9999
! tsea
      jdisc0(1)=0
      jpdt1(1)=0
      jpdt2(1)=0
      jpdt10(1)=1
      jpdt11(1)=0    
      jpdt12(1)=0       
! smc(0-10cm)
      jdisc0(2)=2
      jpdt1(2)=0
      jpdt2(2)=192
      jpdt10(2)=106
      if ( iscale.eq.0 ) then
          jpdt11(2)=2
      else
          jpdt11(2)=0
      endif
      jpdt12(2)=0
! smc(10-40cm)
      jdisc0(3)=2
      jpdt1(3)=0
      jpdt2(3)=192
      jpdt10(3)=106
      if ( iscale.eq.0 ) then
          jpdt11(3)=2
          jpdt12(3)=10
      else
          jpdt11(3)=1
          jpdt12(3)=1
      endif
! smc(40-100cm) 
      jdisc0(4)=2
      jpdt1(4)=0
      jpdt2(4)=192
      jpdt10(4)=106
      if ( iscale.eq.0 ) then
          jpdt11(4)=2  
          jpdt12(4)=40 
      else
          jpdt11(4)=1  
          jpdt12(4)=4 
      endif
! smc(100-200cm)
      jdisc0(5)=2
      jpdt1(5)=0
      jpdt2(5)=192
      jpdt10(5)=106
      if ( iscale.eq.0 ) then
          jpdt11(5)=2  
          jpdt12(5)=100
      else
          jpdt11(5)=0  
          jpdt12(5)=1
      endif
! sheleg
      jdisc0(6)=0
      jpdt1(6)=1
      jpdt2(6)=13 
      jpdt10(6)=1
      jpdt11(6)=0
      jpdt12(6)=0
! stc(0-10cm)
      jdisc0(7)=2   ! for new GFS
      jpdt1(7)=0
      jpdt2(7)=2    ! for new GFS
      jpdt10(7)=106
      if ( iscale.eq.0 ) then
          jpdt11(7)=2
          jpdt12(7)=0
      else
          jpdt11(7)=0
          jpdt12(7)=0
      endif
! stc(10-40cm)
      jdisc0(8)=2    
      jpdt1(8)=0
      jpdt2(8)=2
      jpdt10(8)=106
      if ( iscale.eq.0 ) then
          jpdt11(8)=2  
          jpdt12(8)=10 
      else
          jpdt11(8)=1  
          jpdt12(8)=1
      endif 
! stc(40cm-100cm)
      jdisc0(9)=2
      jpdt1(9)=0
      jpdt2(9)=2
      jpdt10(9)=106
      if ( iscale.eq.0 ) then
          jpdt11(9)=2  
          jpdt12(9)=40
      else
          jpdt11(9)=1  
          jpdt12(9)=4
      endif 
! stc(100cm-200cm)
      jdisc0(10)=2
      jpdt1(10)=0
      jpdt2(10)=2
      jpdt10(10)=106
      if ( iscale.eq.0 ) then
          jpdt11(10)=2  
          jpdt12(10)=100
      else
          jpdt11(10)=0  
          jpdt12(10)=1
      endif
! cv
      jdisc0(11)=0
      jpdt1(11)=6
      jpdt2(11)=1
      if ( iscale.eq.0 ) then
          jpdt10(11)=244
          jpdt11(11)=0
          jpdt12(11)=0
      else
          jpdt10(11)=10
          jpdt11(11)=0
          jpdt12(11)=0
      endif
! land
      jdisc0(12)=2
      jpdt1(12)=0
      jpdt2(12)=0
      jpdt10(12)=1
      jpdt11(12)=0
      jpdt12(12)=0
! ice 
      jdisc0(13)=10
      jpdt1(13)=2
      jpdt2(13)=0
      jpdt10(13)=1
      jpdt11(13)=0
      jpdt12(13)=0

!
      do n=1,nfld
         lugi=0
         j=0
         jdisc=jdisc0(n)
         jids=-9999
         if ( iefs.eq.1 ) then
         jpdtn=1
         else
         jpdtn=0
         end if
         jpdt=-9999
         jgdtn=-1
         jgdt=-9999

! Set GRIB2 field identification values to search for
         jpdt(1)=jpdt1(n)
         jpdt(2)=jpdt2(n)
         jpdt(10)=jpdt10(n)
         jpdt(11)=jpdt11(n)
         jpdt(12)=jpdt12(n)
!         jpdt(13)=jpdt13(n)
!         jpdt(14)=0
!         jpdt(15)=jpdt15(n)

! Get field from file
         call getgb2(ifile,lugi,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,      &
     &            unpack,j,gfld,iret)

! Process field ...
         if (iret.eq.0) then
            if (ijmax.eq.gfld%ngrdpts) then
               out(:,n)=gfld%fld
            else
               write(6,*) 'grib projection donot fit rscomloc'
               write(6,*) 'ijmax',ijmax,gfld%ngrdpts
               write(6,*) jdisc0(n),jpdt1(n),jpdt2(n)
               call abort
            endif
         else
            write(6,*) 'canot find data',jdisc0(n),jpdt1(n),jpdt2(n)
            out(:,n)=0.0
         endif
      enddo
!
! Free memory when done with field
      call gf_free(gfld)
 
      end

