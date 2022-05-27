      subroutine preadg2(ifile,ijmax,kdim,plevels,iqvar,                &
     &                   nfld,out,fhour,idate,iflip,iscale)
      use grib_mod
#include <paramodel.h>
#include <rscomloc.h>
      type(gribfield) :: gfld
      integer,dimension(200) :: jids,jpdt,jgdt
      logical :: unpack=.true.
!
      integer :: ijmax,kdim,iret,ifile
      real :: plevels(kdim)
      integer :: nfld
      real    :: out(ijmax,kdim,nfld)
      integer :: lugi,j
      integer :: jpdt2(nfld),jpdt3(nfld),jpdt13(kdim)
      integer :: jdisc,jpdtn,jgdtn
      integer :: nterr,iflip,iqvar,iscale
      real    :: fhour
      integer :: idate(4)
      integer :: n,k,kk
! scaling level
      integer :: tmplev, iscl
! 
! gz
      jpdt2(1)=3
      jpdt3(1)=5
! t
      jpdt2(2)=0
      jpdt3(2)=0
! u
      jpdt2(3)=2
      jpdt3(3)=2
! v 
      jpdt2(4)=2
      jpdt3(4)=3
!
      if (iqvar.eq.1) then
! spfh
         jpdt2(5)=1
         jpdt3(5)=0
      else
! rh
         jpdt2(5)=1
         jpdt3(5)=1
      endif
! o3
      jpdt2(6)=14
      jpdt3(6)=192
! cloud water
      jpdt2(7)=1
      jpdt3(7)=22
! terrain
      jpdt2(8)=3
      jpdt3(8)=5
!
      jpdt13=int(plevels)
!
!
      do n=1,nfld-1
      do k=1,kdim
         lugi=0
         j=0
         jdisc=-1
         jids=-9999
         jpdtn=0
         jpdt=-9999
         jgdtn=-1
         jgdt=-9999

! Set GRIB2 field identification values to search for
         jpdt(1)=jpdt2(n)
         jpdt(2)=jpdt3(n)
         jpdt(10)=100
         if ( iscale.eq.0 ) then
             jpdt(11)=0
             jpdt(12)=jpdt13(k)*100
         else
             tmplev=jpdt13(k)*100
             iscl=0
             do while ( mod(tmplev,10).eq.0 )
                iscl=iscl-1
                tmplev=tmplev/10
             enddo
             write(6,'(I4)') iscl, tmplev
             jpdt(11)=iscl
             jpdt(12)=tmplev
         endif

! Get field from file
         call getgb2(ifile,lugi,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,      &
     &            unpack,kk,gfld,iret)

! Process field ...
         if (iret.eq.0) then
            if (ijmax.eq.gfld%ngrdpts) then
               out(:,k,n)=gfld%fld
            else
               write(6,*) 'grib projection donot fit rscomloc'
               write(6,*) 'ijmax=',ijmax,' ngrdpts=',gfld%ngrdpts
               call abort
            endif
           if (gfld%igdtmpl(12).gt.gfld%igdtmpl(15)) then
              iflip=1
           else
              iflip=0
           endif
           idate(4)=gfld%idsect(6)
           idate(2)=gfld%idsect(7)
           idate(3)=gfld%idsect(8)
           idate(1)=gfld%idsect(9)
           fhour=gfld%ipdtmpl(9)
         else
            write(6,*) 'cannot find var ',jpdt2(n),jpdt3(n),' in ',      &
     &                 plevels(k),' level'
            write(6,*) 'iret= ',iret
            if (k.eq.1) then
               out(:,k,n)=0.0
            else
               out(:,k,n)=out(:,k-1,n)
            end if
         endif
      enddo
      enddo
!
         lugi=0
         j=0
         jdisc=-1
         jids=-9999
         jpdtn=0
         jpdt=-9999
         jgdtn=-1
         jgdt=-9999

! Set GRIB2 field identification values to search for
         jpdt(1)=jpdt2(8)
         jpdt(2)=jpdt3(8)
         jpdt(10)=1
         jpdt(11)=0
         jpdt(12)=0              

! Get field from file
         call getgb2(ifile,lugi,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,     &
     &            unpack,kk,gfld,iret)

! Process field ...
         if (iret.eq.0) then
            if (ijmax.eq.gfld%ngrdpts) then
               out(:,1,8)=gfld%fld
            else
               write(6,*) 'grib projection donot fit rscomloc'
               call abort
            endif
           idate(4)=gfld%idsect(6)
           idate(2)=gfld%idsect(7)
           idate(3)=gfld%idsect(8)
           idate(1)=gfld%idsect(9)
           fhour=gfld%ipdtmpl(9)
         else
            write(6,*) 'cannot find var ',jpdt2(8),jpdt3(8)
            write(6,*) 'iret= ',iret
            out(:,1,8)=0.0
         endif
! Free memory when done with field
      call gf_free(gfld)
 
      end

