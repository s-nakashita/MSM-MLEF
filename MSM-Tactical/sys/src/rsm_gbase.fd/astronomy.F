!cc
      subroutine astronomy(me,lonl2,latd,sinlar,coslar,                 &
     &  xlon,fhswr,idate,phour,lsswr,lslwr,                             &
     &  solc,rsin1,rcos1,rcos2,slag,sdec,cdec,coszer,coszdg)
!c
      use machine , only :kind_rad
      implicit none
!
      integer me,latd,idate(4),lonl2
      integer jdnmc,kyear,jd,imon,iday,iztim,ihr
      integer im,id,iyear
      logical lsswr,lslwr
      character*4 munth
!
      integer loz,jmr,jmout
      parameter (jmr=18,loz=17,jmout=37)
!
      real(kind=kind_rad) o3clim(jmr,loz,12),                           &
     &                  o3out(jmout,loz),pstr(loz)
!
      real (kind=kind_rad) sinlar(lonl2,latd),coslar(lonl2,latd)
      real (kind=kind_rad) xlon(lonl2,latd)
      real (kind=kind_rad) fhswr,phour
      real (kind=kind_rad) slag,sdec,cdec,solhr,fjdnmc,sc
      real (kind=kind_rad) coszdg(lonl2,latd),coszer(lonl2,latd)
      real (kind=kind_rad) fjd,dlt,r1,alf,xmin
      real (kind=kind_rad) solc,rsin1,rcos1,rcos2
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      solhr=mod(phour+idate(1),24.)
!
!      if(me.eq.0) print 1001, jcap, levs
! 1001 format (1h0,'gfdl/hou redcrad',i2,i2,'became oper. june 15 1998')
!
!    ****************************************************************
!... * astronomy calculations-once for each new radiation interval  *
!    ****************************************************************
!..      get 4 digit year for julian day computation
      kyear = idate(4)
      imon  = idate(2)
      iday  = idate(3)
      iztim = idate(1)
      call compjd(kyear,imon,iday,iztim,0,jdnmc,fjdnmc)
      call fcstim(phour,imon,iday,iztim,jdnmc,fjdnmc,                 &
     &            rsin1,rcos1,rcos2,jd,fjd)
!..**************************
      if(lsswr) then
        call solar(jd,fjd,r1,dlt,alf,slag,sdec,cdec)
!        if(me.eq.0) print*,'in astronomy completed sr solar'
        call coszmn(fhswr,solhr,sinlar,coslar,sdec,cdec,slag,         &
     &              xlon,lonl2,latd,coszer,.true.,coszdg)
!        if(me.eq.0)print*,'in astronomy completed sr coszmn'
!ccmrfjs
!        call coszmn(fhswr,solhr,sinlat,coslat,sdec,cdec,slag,         &
!    &    xlon,lonl2,latd,coszen,.true.,coszdg,nlats,lons_lar,         &
!    &    global_lats_r)
!
!       calculate solar input appropriate for date
        sc=2.
        solc=sc/(r1*r1)
      endif
!
      call cdate(jd,fjd,munth,im,id,iyear,ihr,xmin)
!        if(me.eq.0)print*,'in astronomy completed sr cdate'
      if (me.eq.0) call prtime(id,munth,iyear,ihr,xmin,                 &
     &                     jd,fjd,dlt,alf,r1,slag,solc)
!        if(me.eq.0)print*,'in astronomy completed sr prtime'
!
!jw initial
      o3clim=0.
      call o3intpnasa(phour,idate,o3clim,pstr,o3out)
!        if(me.eq.0)print*,'completed sr o3intpnasa and astronomy!!!'
!
      return
      end
