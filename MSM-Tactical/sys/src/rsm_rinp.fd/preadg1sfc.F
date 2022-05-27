      subroutine preadg1sfc(lugb,ijmax,nfld,out)
      use machine , only : kind_io8,kind_io4
#include <paramodel.h>
#include <rscomloc.h>
      integer mdata
      parameter (mdata=2500*1250)
      integer lugb
      integer ijmax
      integer nfld
      real    out(ijmax,nfld)
      integer lugi,lskip,iret,jret
      integer jpds(200),jgds(200),kpds(200),kgds(200)
      integer ndata 
      logical*1 lbms(mdata)
      real(kind=kind_io8)    data4(mdata)
      integer kpds5(nfld),kpds6(nfld),kpds7(nfld)
      integer n
!
      kpds5=-1
      kpds6=-1
      kpds7=-1
!
      kpds5(1)=11   ! tsea
      kpds6(1)=1
!
      kpds5(2)=144  ! smc(0-10cm)
      kpds6(2)=112
      kpds7(2)=10
!
      kpds5(3)=144  ! smc(10-40cm)
      kpds6(3)=112
      kpds7(3)=2600
!
      kpds5(4)=144  ! smc(40-100cm)
      kpds6(4)=112
      kpds7(4)=10340
!
      kpds5(5)=144  ! smc(100-200cm)
      kpds6(5)=112
      kpds7(5)=25800
!
      kpds5(6)=65  ! sheleg (weasd)
      kpds6(6)=1
!
      kpds5(7)=11  ! stc(0-10cm)
      kpds6(7)=112
      kpds7(7)=10
!
      kpds5(8)=11  ! stc(10-40cm)
      kpds6(8)=112
      kpds7(8)=2600
!
      kpds5(9)=11  ! stc(40-100cm)
      kpds6(9)=112
      kpds7(9)=10340
!
      kpds5(10)=11  ! stc(100-200cm)
      kpds6(10)=112
      kpds7(10)=25800
!
      kpds5(11)=71  ! cv
      kpds6(11)=244
!
      kpds5(12)=81  ! land
      kpds6(12)=1
!     
      kpds5(13)=91  ! ice 
      kpds6(13)=1
! 
      do n=1,nfld
         lugi    = 0
         lskip   = 0
         jpds    = -1
         jgds    = -1

         jpds(5) = kpds5(n)
         jpds(6) = kpds6(n)
         jpds(7) = kpds7(n)
         call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,            &
     &       kpds,kgds,lbms,data4,jret)
         if(jret.eq.0) then
            if(ndata.eq.0) then
              write(6,*) ' error in getgb'
              write(6,*) ' kpds=',kpds
              write(6,*) ' kgds=',kgds
              call abort
            endif
!           imax=kgds(2)
!           jmax=kgds(3)
            if (ijmax.eq.(kgds(2)*kgds(3))) then
               out(:,n)=data4(1:ijmax)
            else
               write(6,*) 'grib projection donot fit rscomloc'
               call abort
            endif 
!           if (me .eq. 0) write(6,*) 'imax,jmax,ijmax=',imax,jmax,ijmax
         else
            write(6,*) 'cannot find data'
            write(6,*) jpds
            call abort
         endif
      enddo
!
      end

