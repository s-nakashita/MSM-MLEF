      subroutine preadg1(lugb,ijmax,kdim,plevels,iqvar,                 &
     &                   nfld,out,fhour,idate,iflip)
      use machine , only : kind_io8,kind_io4
#include <paramodel.h>
#include <rscomloc.h>
      integer mdata
      parameter (mdata=2500*1250)
      integer lugb
      integer kdim,ijmax
      real plevels(kdim)
      integer nfld
      real    out(ijmax,kdim,nfld)
      integer lugi,lskip,iret,jret
      integer jpds(200),jgds(200),kpds(200),kgds(200)
      integer ndata 
      logical*1 lbms(mdata)
      real(kind=kind_io8)    data4(mdata)
      integer kpds5(nfld)
      real    fhour
      integer idate(4)
      integer nterr,iqvar,iflip
      integer n,k
!
      kpds5(1)=7   ! z
      kpds5(2)=11  ! t
      kpds5(3)=33  ! u
      kpds5(4)=34  ! v
      if (iqvar.eq.1) then
         kpds5(5)=51  ! spfh
      else
         kpds5(5)=52  ! RH  
      endif
      kpds5(6)=154 ! o3   
      kpds5(7)=153 ! clwmr
      kpds5(8)=7   ! terrain
! 
      do n=1,nfld-1
      do k=1,kdim
         lugi    = 0
         lskip   = 0
         jpds    = -1
         jgds    = -1
         jpds(5) = kpds5(n)
         jpds(6) = 100
         jpds(7) = plevels(k)
         call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,        &
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
               out(:,k,n)=data4(1:ijmax)
            else
               write(6,*) 'imax= ',kgds(2),' jmax= ',kgds(3)
               write(6,*) 'grib projection donot fit rscomloc'
               call abort
            endif 
            if (kgds(4).gt.kgds(7)) then
               iflip=1
            else
               iflip=0
            endif
!           if (me .eq. 0) write(6,*) 'imax,jmax,ijmax=',imax,jmax,ijmax
            fhour=kpds(14)
            idate(4)=kpds(8)
            idate(2)=kpds(9)
            idate(3)=kpds(10)
            idate(1)=kpds(11)
         else
            write(6,*) 'cannot find var ',kpds5(n),' in ',              &
     &                 plevels(k),' level'
            out(:,k,n)=0.0
         endif
      enddo
      enddo
!
         lugi    = 0
         lskip   = 0
         jpds    = -1
         jgds    = -1
         jpds(5) = kpds5(8)
         jpds(6) = 1
         call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,        &
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
               out(:,1,8)=data4(1:ijmax)
            else
               write(6,*) 'imax= ',kgds(2),' jmax= ',kgds(3)
               write(6,*) 'grib projection donot fit rscomloc'
               call abort
            endif
!           if (me .eq. 0) write(6,*) 'imax,jmax,ijmax=',imax,jmax,ijmax
            fhour=kpds(14)
            idate(4)=kpds(8)
            idate(2)=kpds(9)
            idate(3)=kpds(10)
            idate(1)=kpds(11)
         else
            write(6,*) 'cannot find var ',kpds5(8)
            out(:,1,8)=0.0
         endif

!
      end

