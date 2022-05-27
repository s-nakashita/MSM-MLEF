      subroutine s2rinp(nsfc,nrsfc,nrslm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  s2rinp
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  transform global surface fields to regional surfcace file for
!            input
!
! program history log:
!
! usage:    call s2rinp(nsfc,nmtv,nrsfc)
!   input argument list:
!     nsfc      - unit number of global surface file
!     nmtv      - unit number of global mountain variance file
!     nrsfc     - unit number of regional surface file
!
!   output argument list:
!
!   input files:
!       nsfc  nmtv
!
!   output files:
!       nrsfc
!
!   subprograms called:
!       setlatlon  glats  g2rini  gg2rg  maxmin  
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use machine , only : kind_io4
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
!cc
!.....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....read global gaucian grid values, then interpolate to
!.....regional grid values.
!   **note*** lonf=lonb and latg=latb for rsm
!.....
!
      parameter (imax=lonf, jmax=latg, ijmax=imax*jmax)
!
      real(kind=kind_io4) ghours,fhours
      real(kind=kind_io4) ggs(ijmax),ggls(ijmax,lsoil)
      real(kind=kind_io4) gg2s(ijmax,2),gg4s(ijmax,4)
      real(kind=kind_io4) rgs(lngrd),rgls(lngrd,lsoil)
      real(kind=kind_io4) rg2s(lngrd,2),rg4s(lngrd,4)
      real(kind=kind_io4) fcsttime
      character*8 label(4)
!
      real, allocatable :: slg(:),ggl(:,:)
      real, allocatable ::  gg(:),gg2(:,:)
      real, allocatable :: gg4(:,:)
      real, allocatable :: gclat(:)
      real, allocatable :: colrad(:), wgt(:),wgtcs(:),rcs2(:)
      integer, allocatable :: iwork(:,:),idate(:)
      real, allocatable :: ggsea(:,:),ggland(:,:),rr(:)
      real, allocatable :: tsea  (:), smc   (:,:)
      real, allocatable :: sheleg(:), stc   (:,:)
      real, allocatable :: tg3   (:), vfrac (:)
      real, allocatable :: zorl  (:), slmsk (:)
      real, allocatable :: cv    (:), cvb   (:)
      real, allocatable :: cvt   (:)
      real, allocatable :: f10m  (:), canopy(:)
      real, allocatable :: vtype (:)
      real, allocatable :: stype (:), uustar(:)
      real, allocatable :: ffmm  (:),   ffhh(:)
      real, allocatable :: alvsf (:),  alvwf(:)
      real, allocatable :: alnsf (:),  alnwf(:)
      real, allocatable :: facsf (:),  facwf(:)
      real, allocatable :: slmk(:),simk(:)
      real, allocatable :: flat(:),flon(:)
      real, allocatable :: dflat(:),dflon(:)
!
      integer version
      data version/199802/
      namelist/namsfc/fcsttime
      data fcsttime/-9999./
      character*500 fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh
      namelist/namclim/iclim,fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,  &
     &                 fnmskh
      real tg3clm(lngrd),zorclm(lngrd),albclm(lngrd,4),vegclm(lngrd)     
      real vetclm(lngrd),sotclm(lngrd),alfclm(lngrd,2)
!
      print *,' ----- into s2rinp ----- '
!
      allocate ( slg(ijmax),ggl(ijmax,lsoil) )
      allocate ( gg(ijmax),gg2(ijmax,2) )
      allocate ( gg4(ijmax,4) )
      allocate ( gclat(jmax) )
      allocate ( colrad(latg2), wgt(latg2),wgtcs(latg2),rcs2(latg2) )
      allocate ( iwork(imax,jmax) )
      allocate ( idate(4) )
      allocate ( ggsea(imax,jmax),ggland(imax,jmax),rr(lngrd) )
      allocate ( tsea  (lngrd), smc   (lngrd,lsoil) )
      allocate ( sheleg(lngrd), stc   (lngrd,lsoil) )
      allocate ( tg3   (lngrd), vfrac (lngrd) )
      allocate ( zorl  (lngrd), slmsk (lngrd) )
      allocate ( cv    (lngrd), cvb   (lngrd) )
      allocate ( cvt   (lngrd) )
      allocate ( f10m  (lngrd), canopy(lngrd) )
      allocate ( vtype (lngrd) )
      allocate ( stype (lngrd), uustar(lngrd) )
      allocate ( ffmm  (lngrd),   ffhh(lngrd) )
      allocate ( alvsf (lngrd),  alvwf(lngrd) )
      allocate ( alnsf (lngrd),  alnwf(lngrd) )
      allocate ( facsf (lngrd),  facwf(lngrd) )
      allocate ( slmk(lngrd),simk(lngrd) )
      allocate ( flat(lngrd),flon(lngrd) )
      allocate ( dflat(lngrd),dflon(lngrd) )
!
      call setlatlon(flat,flon,delx,dely,dlamda0)
      call glats(latg2,colrad,wgt,wgtcs,rcs2)
      do 7 j=1,latg2
      gclat(j) = colrad(j)
      jj = latg - j + 1
      gclat(jj) = pi - colrad(j)
 7    continue
      call g2rini(gclat,flat,flon)
!
      print *,' nsfc=',nsfc,' nrsfc=',nrsfc
!
! read initial surface condition
!
      rewind nsfc
!
! get slmask first
      read(nsfc) label
      read(nsfc) ghours,idate
! set init sfc fcst hour to 00
      read(5,namsfc)
      if ( fcsttime .ne. -9999.) then
        ghour=fcsttime
      else
        ghour=ghours
      endif
!
      read(nsfc)               ! tsea
      read(nsfc)               ! smc
      read(nsfc) ggs            ! sheleg
      call sgl2ful(ggs,gg,ijmax)
      call gg2rg(gg,sheleg,1)
      read(nsfc)               ! stc
      read(nsfc)               ! tg3
      read(nsfc)               ! zorl
      read(nsfc)               ! cv
      read(nsfc)               ! cvb
      read(nsfc)               ! cvt
      read(nsfc)               ! alvsf,alvwf,alnsf,alnwf
      read(nsfc) ggs          ! slmsk
      call sgl2ful(ggs,slg,ijmax)
! get sea and ice index
      do 10 i=1,ijmax
      gg(i) = 0.0
      if( slg(i) .eq. 2.0 ) gg(i) = 2.0
 10   continue
      call gg2rg(gg,simk,1)
      do 20 i=1,lngrd
      if( simk(i) .gt. 0.9 ) then
          simk(i) = 2.0
      else
          simk(i) = 0.0
      endif
 20   continue
! get sea and land index
      do 30 i=1,ijmax
      gg(i) = 0.0
      if( slg(i) .eq. 1.0 ) gg(i) = 1.0
 30   continue
      call gg2rg(gg,slmk,1)
      do 40 i=1,lngrd
      if( slmk(i) .gt. 0.4 ) then
          slmk(i) = 1.0
      else
          slmk(i) = 0.0
      endif
 40   continue
!      do 45 i=1,lngrd
!      slmsk(i) = max( simk(i), slmk(i) )
!  45  continue
!
! get regional land-sea mask
        rewind nrslm
        read(nrslm) rgs  
        call sgl2ful(rgs,slmsk,lngrd)
        rewind nrslm
! before add sea ice info
! read climatology data
      read(5,namclim)
      if (iclim.eq.1) then
        write(*,*) fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh
        dflat=flat*180/pi
        dflon=flon*180/pi
        call clima(idate(4),idate(2),idate(3),idate(1),ghour,             &
     &             lngrd,lsoil, slmsk,dflat,dflon,                        &
     &             fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh,      &
     &             tg3clm,zorclm,albclm,vegclm,vetclm,sotclm,alfclm)
        call maxmin(zorclm,lngrd,1,1,1,'tg3clm')
        call maxmin(albclm,lngrd,4,1,4,'albclm')
        call maxmin(vegclm,lngrd,1,1,1,'vegclm')
        call maxmin(vetclm,lngrd,1,1,1,'vetclm')
        call maxmin(sotclm,lngrd,1,1,1,'sotclm')
        call maxmin(alfclm,lngrd,2,1,2,'alfclm')
      endif
!
        do 45 i=1,lngrd
! add sea ice info
        slmsk(i) = max(slmsk(i),simk(i))
 45     continue
!
! update climate var sea ice info
      if (iclim.eq.1) then
        do i=1,lngrd
          if (slmsk(i).eq.2.0) then
            vetclm(i)=13.0
            sotclm(i)=9.0
            zorclm(i)=1.0
          endif
        enddo
      endif
!
! put sea ice point into sea point
        do i=1,ijmax 
        if(slg(i).eq.2.0) slg(i)=0.0
        enddo
!
!------ read from gsm, interpolate to rsm, and save to nrsfc -----
      rewind nsfc
      rewind nrsfc
! label ----- record numer 1
      read(nsfc) label
      write(nrsfc) label
! hour idate ----- 2
      read(nsfc) ghours,idate
      write(*,*) ghours,idate
! set initial time to 0
       ghours=0.0
      if ( fcsttime .ne. -9999.) then
        ghours=fcsttime
      endif
      write(nrsfc) ghours,idate,igrd1,jgrd1,version
! tsea ----------- 3
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,tsea,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,tsea,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) tsea(i)=rr(i)
      enddo
      do 50 i=1,lngrd
      if( slmsk(i) .eq. 2.0e0 ) then
          tsea(i)= min( tsea(i), 271.20e0 )
      else if( slmsk(i).eq.0.0e0 ) then
          tsea(i)= max( tsea(i), 271.21e0 )
      else if( slmsk(i).eq.1.0e0 .and.                                  &
     &         sheleg(i).gt.0.0e0 ) then
          tsea(i)= min( tsea(i), 273.16e0 )
      endif
 50   continue
!      call maxmin(  tsea,lngrd,1,1,1,'tsea')
      call ful2sgl( tsea,rgs,lngrd)
      print *,'in no merge,fft, rtsea(1:10)=',tsea(1:10)
      print *,'in no merge,fft, rtsea(121:130)=',tsea(121:130)
      write(nrsfc)  rgs
! soil moisture ----- 4
      read(nsfc) ggls
      call sgl2ful(ggls,ggl,lsoil*ijmax)
!     call gg2rg(ggl,smc,lsoil)
      do k=1,lsoil 
        call expnd(ggl(1,k),slg,0,ggsea,iwork,imax,jmax)
        call expnd(ggl(1,k),slg,1,ggland,iwork,imax,jmax)
        call gg2rg(ggsea,smc(1,k),1)
        call gg2rg(ggland,rr,1)
        do i=1,lngrd 
          if(slmsk(i).eq.1.0) smc(i,k)=rr(i)
        enddo
      enddo
!      call maxmin(   smc,lngrd,lsoil,1,lsoil,'smc')
      call ful2sgl(  smc,rgls,lsoil*lngrd)
      write(nrsfc)   rgls
! snow depth ----- 5
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,sheleg,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,sheleg,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) sheleg(i)=rr(i)
      enddo
!      call maxmin(  sheleg,lngrd,1,1,1,'sheleg')
      call ful2sgl( sheleg,rgs,lngrd)
      write(nrsfc)  rgs
! stc ------------ 6
      read(nsfc) ggls
      call sgl2ful(ggls,ggl,lsoil*ijmax)
!     call gg2rg(ggl,stc,lsoil)
      do k=1,lsoil
        call expnd(ggl(1,k),slg,0,ggsea,iwork,imax,jmax)
        call expnd(ggl(1,k),slg,1,ggland,iwork,imax,jmax)
        call gg2rg(ggsea,stc(1,k),1)
        call gg2rg(ggland,rr,1)
        do i=1,lngrd
          if(slmsk(i).eq.1.0) stc(i,k)=rr(i)
        enddo
      enddo
!      call maxmin(   stc,lngrd,lsoil,1,lsoil,'stc')
      call ful2sgl(  stc,rgls,lsoil*lngrd)
      write(nrsfc)   rgls
! tg3 ------------ 7
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,tg3,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,tg3,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) tg3(i)=rr(i)
      enddo
!      call maxmin(  tg3,lngrd,1,1,1,'tg3')
      if (iclim.eq.1) then
        call ful2sgl( tg3clm,rgs,lngrd)
      else
        call ful2sgl( tg3,rgs,lngrd)
      endif
      write(nrsfc)  rgs
! roughness ----- 8
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,zorl,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,zorl,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) zorl(i)=rr(i)
      enddo
!      call maxmin(  zorl,lngrd,1,1,1,'zorl')
      if (iclim.eq.1) then
        call ful2sgl( zorclm,rgs,lngrd)
      else
        call ful2sgl( zorl,rgs,lngrd)
      endif
      write(nrsfc)  rgs
! cloud amount -- 9
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
      call gg2rg(gg,cv,1)
! cloud base --- 10
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
!c      do i=1,ijmax
!c        if(gg(i).eq.100.) gg(i)=0.0
!c      enddo
      call gg2rg(gg,cvb,1)
!c      do i=1,lngrd
!c        if(cvb(i).eq.0.0) cvb(i)=100.
!c      enddo
! cloud top ---- 11
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
      call gg2rg(gg,cvt,1)
! consistency check among cv cvb and cvt
!c      do i=1,lngrd
!c        if(cvt(i).lt.cvb(i) .or. cv(i).eq.0.0) then
!c          cv (i)=0.0
!c          cvt(i)=0.0
!c          cvb(i)=100.0
!c        endif
!c      enddo
!      call maxmin(  cv, lngrd ,1,1,1,'cv')
      call ful2sgl( cv,rgs, lngrd )
      write(nrsfc)  rgs
!      call maxmin(  cvb, lngrd ,1,1,1,'cvb')
      call ful2sgl( cvb,rgs, lngrd )
      write(nrsfc)  rgs
!      call maxmin(  cvt, lngrd ,1,1,1,'cvt')
      call ful2sgl( cvt,rgs, lngrd )
      write(nrsfc)  rgs
! 4 albedo ------- 12
      read(nsfc) gg4s
      call sgl2ful(gg4s,gg4,ijmax*4)
!     call gg2rg(gg4(1,1),alvsf,1)
!     call gg2rg(gg4(1,2),alvwf,1)
!     call gg2rg(gg4(1,3),alnsf,1)
!     call gg2rg(gg4(1,4),alnwf,1)
      call expnd(gg4(1,1),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,1),slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,alvsf,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) alvsf(i)=rr(i)
      enddo
      call expnd(gg4(1,2),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,2),slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,alvwf,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) alvwf(i)=rr(i)
      enddo
      call expnd(gg4(1,3),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,3),slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,alnsf,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) alnsf(i)=rr(i)
      enddo
      call expnd(gg4(1,4),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,4),slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,alnwf,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) alnwf(i)=rr(i)
      enddo
!      call maxmin(  alvsf,lngrd,1,1,1,'alvsf')
!      call maxmin(  alvwf,lngrd,1,1,1,'alvwf')
!      call maxmin(  alnsf,lngrd,1,1,1,'alnsf')
!      call maxmin(  alnwf,lngrd,1,1,1,'alnwf')
      if (iclim.eq.1) then
        call ful2sgl( albclm(1,1),rg4s(1,1),lngrd)
        call ful2sgl( albclm(1,2),rg4s(1,2),lngrd)
        call ful2sgl( albclm(1,3),rg4s(1,3),lngrd)
        call ful2sgl( albclm(1,4),rg4s(1,4),lngrd)
      else
        call ful2sgl( alvsf,rg4s(1,1),lngrd)
        call ful2sgl( alvwf,rg4s(1,2),lngrd)
        call ful2sgl( alnsf,rg4s(1,3),lngrd)
        call ful2sgl( alnwf,rg4s(1,4),lngrd)
      endif
      write(nrsfc)  rg4s
! sea land mask -- 13
      read(nsfc) ggs
!      call maxmin( slmsk,lngrd,1,1,1,'slmsk')
      call ful2sgl( slmsk,rgs,lngrd)
      write(nrsfc) rgs
! vegitation fraction ---- 14
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,vfrac,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,vfrac,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) vfrac(i)=rr(i)
      enddo
!      call maxmin(  vfrac,lngrd,1,1,1,'vfrac')
      if (iclim.eq.1) then
        call ful2sgl( vegclm,rgs,lngrd)
      else
        call ful2sgl( vfrac,rgs,lngrd)
      endif
      write(nrsfc)  rgs
! prepare 15 to end for end of file condition
      do i=1,lngrd
        canopy(i)=60.0
        f10m(i)=0.8
        vtype(i)=1.0
        stype(i)=1.0
        facsf(i)=1.0
        facwf(i)=1.0
        uustar(i) = 1.
        ffmm(i) = log(30.)
        ffhh(i) = log(30.)
      enddo
! vegitation canopy ------ 15
      read(nsfc,end=1500) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,canopy,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,canopy,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) canopy(i)=rr(i)
      enddo
!      call maxmin(  canopy,lngrd,1,1,1,'canopy read')
      call ful2sgl( canopy,rgs,lngrd)
      write(nrsfc)  rgs
!
! f10m -------------- 16
      read(nsfc,end=1600) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,f10m,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,f10m,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) f10m(i)=rr(i)
      enddo
!      call maxmin(  f10m,lngrd,1,1,1,'f10m read')
      call ful2sgl( f10m,rgs,lngrd)
      write(nrsfc)  rgs
!
! vegitation type ---- 17
      read(nsfc,end=1700) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,vtype,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,vtype,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) vtype(i)=rr(i)
      enddo
!      call maxmin(  vtype,lngrd,1,1,1,'vtype read')
      if (iclim.eq.1) then
        call ful2sgl( vetclm,rgs,lngrd)
      else
        call ful2sgl( vtype,rgs,lngrd)
      endif
      write(nrsfc)  rgs
!
! soil type ---------- 18
      read(nsfc,end=1800) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,stype,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,stype,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) stype(i)=rr(i)
      enddo
!      call maxmin(  stype,lngrd,1,1,1,'stype read')
      if (iclim.eq.1) then
        call ful2sgl( sotclm,rgs,lngrd)
      else
        call ful2sgl( stype,rgs,lngrd)
      endif
      write(nrsfc)  rgs
!
! zenith angle dependent factor ---- 19
      read(nsfc,end=1900) gg2s
      call sgl2ful(gg2s,gg2,ijmax*2)
      call gg2rg(gg2(1,1),facsf,1)
      call gg2rg(gg2(1,2),facwf,1)
!      call maxmin(  facsf,lngrd,1,1,1,'facsf read')
!      call maxmin(  facwf,lngrd,1,1,1,'facwf read')
      if (iclim.eq.1) then
        call ful2sgl( alfclm(1,1),rg2s(1,1),lngrd)
        call ful2sgl( alfclm(1,2),rg2s(1,2),lngrd)
      else
        call ful2sgl( facsf,rg2s(1,1),lngrd)
        call ful2sgl( facwf,rg2s(1,2),lngrd)
      endif
      write(nrsfc)  rg2s
!
! ustar ------------ 20
      read(nsfc,end=2000) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,uustar,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,uustar,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) uustar(i)=rr(i)
      enddo
!      call maxmin(  uustar,lngrd,1,1,1,'uustar read')
      call ful2sgl( uustar,rgs,lngrd)
      write(nrsfc)  rgs
!
! momentum exchange similarity function -- 21
      read(nsfc,end=2100) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,ffmm,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,ffmm,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) ffmm(i)=rr(i)
      enddo
!      call maxmin(  ffmm,lngrd,1,1,1,'ffmm read')
      call ful2sgl( ffmm,rgs,lngrd)
      write(nrsfc)  rgs
!
! heat exchange similarity function -- 22
      read(nsfc,end=2200) ggs
      call sgl2ful(ggs,gg,ijmax)
!     call gg2rg(gg,ffhh,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,ffhh,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) ffhh(i)=rr(i)
      enddo
!      call maxmin(  ffhh,lngrd,1,1,1,'ffhh read')
      call ful2sgl( ffhh,rgs,lngrd)
      write(nrsfc)  rgs
!
      print *,' ****** nornal end of s2rinp ******* '
      return
!
1500  call ful2sgl( canopy,rgs,lngrd)
      call maxmin(  canopy,lngrd,1,1,1,'canopy specified')
      write(nrsfc)         rgs 
1600  call ful2sgl( f10m,  rgs,lngrd)
      call maxmin(  f10m,  lngrd,1,1,1,'f10m specified')
      write(nrsfc)         rgs 
1700  call ful2sgl( vtype, rgs,lngrd)
      call maxmin(  vtype, lngrd,1,1,1,'vtype specified')
      write(nrsfc)         rgs 
1800  call ful2sgl( stype, rgs,lngrd)
      call maxmin(  stype, lngrd,1,1,1,'stype specified')
      write(nrsfc)         rgs 
1900  call ful2sgl( facsf, rg2s(1,1),lngrd)
      call maxmin(  facsf, lngrd,1,1,1,'facsf specified')
      call ful2sgl( facwf, rg2s(1,2),lngrd)
      call maxmin(  facwf, lngrd,1,1,1,'facwf specified')
      write(nrsfc)         rg2s
2000  call ful2sgl( uustar,rgs,lngrd)
      call maxmin(  uustar,lngrd,1,1,1,'uustar specified')
      write(nrsfc)         rgs
2100  call ful2sgl( ffmm,  rgs,lngrd)
      call maxmin(  ffmm,  lngrd,1,1,1,'ffmm specified')
      write(nrsfc)         rgs
2200  call ful2sgl( ffhh,  rgs,lngrd)
      call maxmin(  ffhh,  lngrd,1,1,1,'ffhh specified')
      write(nrsfc)         rgs
!
      close(nrsfc)
!
      deallocate ( slg,ggl )
      deallocate ( gg,gg2 )
      deallocate ( gg4 )
      deallocate ( gclat )
      deallocate ( colrad, wgt,wgtcs,rcs2 )
      deallocate ( iwork )
      deallocate ( idate )
      deallocate ( ggsea,ggland,rr )
      deallocate ( tsea  , smc  )
      deallocate ( sheleg, stc  )
      deallocate ( tg3   , vfrac  )
      deallocate ( zorl  , slmsk  )
      deallocate ( cv    , cvb    )
      deallocate ( cvt    )
      deallocate ( f10m  , canopy )
      deallocate ( vtype  )
      deallocate ( stype , uustar )
      deallocate ( ffmm  ,   ffhh )
      deallocate ( alvsf ,  alvwf )
      deallocate ( alnsf ,  alnwf )
      deallocate ( facsf ,  facwf )
      deallocate ( slmk,simk )
      deallocate ( flat,flon )
      deallocate ( dflat,dflon )
!
!
      print *,' +++++ normal exit with some fields specified +++ '
!
      return
      end
