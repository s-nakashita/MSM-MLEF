      subroutine s2rinp_gfs(nsfc,nrsfc,nrslm,ivs)
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
      real, allocatable :: tg3clm(:),zorclm(:)
      real, allocatable :: albclm(:,:),vegclm(:)
      real, allocatable :: vetclm(:),sotclm(:),alfclm(:,:)

      real ghour
      integer version
      data version/199802/
      namelist/namsfc/fcsttime
      data fcsttime/-9999./
      character*500 fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh
      namelist/namclim/iclim,fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,  &
     &                 fnmskh
      data fntg3c/'global_tg3clim.2.6x1.5.grb'/
      data fnmskh/'global_slmask.t126.grb'/
      data fnalbc/'global_albedo4.1x1.grb'/
      data fnsotc/'global_soiltype.1x1.grb'/
      data fnvegc/'global_vegfrac.1x1.grb'/
      data fnvetc/'global_vegtype.1x1.grb'/
      data fnzorc/'global_zorclim.1x1.grb'/
!
      print *,' ----- into s2rinp_gfs ----- '

      allocate ( slg(ijmax),ggl(ijmax,lsoil) )
      allocate (  gg(ijmax),gg2(ijmax,2) )
      allocate ( gg4(ijmax,4) )
      allocate (  gclat(jmax) )
      allocate ( colrad(latg2), wgt(latg2),wgtcs(latg2),rcs2(latg2) )
      allocate ( iwork(imax,jmax) )
      allocate ( ggsea(imax,jmax),ggland(imax,jmax),rr(lngrd) )
      allocate ( idate(4) )
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
      allocate ( tg3clm(lngrd),zorclm(lngrd) )
      allocate ( albclm(lngrd,4),vegclm(lngrd) )
      allocate ( vetclm(lngrd),sotclm(lngrd),alfclm(lngrd,2) )
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
      call rdsfchead(nsfc,ghour,idate,nhead,ivs)
!
!------ read from gsm, interpolate to rsm, and save to nrsfc -----
      rewind nrsfc
! label ----- record numer 1
      label='        '
      write(nrsfc) label
! hour idate ----- 2
      ghours=ghour
      write(*,*) ghours,idate
! set initial time to 0
      read(5,namsfc)
      ghours=0.0
      if ( fcsttime .ne. -9999.) then
        ghours=fcsttime
      endif
      write(nrsfc) ghours,idate,igrd1,jgrd1,version
!
! this version read 200501 ncep sfc file for 2000 version
! the updated 200501 version has 5 header then single layer for data
! the data record of updated 200501 as
! 1 slmsk(11)   11 vfrac        21 hice         31 stc2         41 slc4
! 2 orog        12 canopy(13)   22 fice         32 stc3(4)
! 3 tsea(1)     13 f10m (14)    23 tprcp        33 stc4
! 4 sheleg(3)   14 vtype        24 srflag       34 smc1(2)
! 5 tg3(5)      15 stype        25 snowdph      35 smc2
! 6 zorl(6)     16 facsf(10)    26 shdmin       36 smc3(2)
! 7 alvsf(10)   17 facwf(10)    27 shdmax       37 smc4
! 8 alvwf(10)   18 uustar       28 slope        38 slc1
! 9 alnsf(10)   19 ffmm         29 snoalb       39 slc2
!10 alnwf(10)   20 ffhh         30 stc1(4)      40 slc3
!
! for version read 200509 ncep sfc file for 2000 version
! the updated 200509 version has 5 header then single layer for data
! the data record of updated 200509 as
! 1 slmsk(11)   11 vfrac        21 ffmm         31 slope        41 slc1
! 2 orog        12 canopy(13)   22 ffhh         32 snoalb       42 slc2
! 3 tsea(1)     13 f10m (14)    23 hice         33 stc1(4)      43 slc3
! 4 sheleg(3)   14 t2m          24 fice         34 stc2         44 slc4
! 5 tg3(5)      15 q2m          25 tisfc        35 stc3(4)
! 6 zorl(6)     16 vtype        26 tprcp        36 stc4
! 7 alvsf(10)   17 stype        27 srflag       37 smc1(2)
! 8 alvwf(10)   18 facsf(10)    28 snowdpt      38 smc2
! 9 alnsf(10)   19 facwf(10)    29 shdmin       39 smc3(2)
!10 alnwf(10)   20 uustar       30 shdmax       40 smc4
!
! for 2000 version we have cv(7) cvb(8) cvt(9) and platr(12)
! since we are using 2000 version for reanalysis as rsm97 version so
! 1 tsea (1)            6 zorl (6)              11 slmsk (11)
! 2 smc1 and smc3 (2)   7 cv (7)                12 plantr (12)
! 3 sheleg (3)          8 cvb (8)               13 canopy (13)
! 4 stc1 and stc3 (4)   9 cvt (9)               14 f10m (14)
! 5 tg3 (5)             10 albedo (10)
!
      if( ivs.eq.2000 ) then            ! reanalysis version (default rsm97)

      itsea   = 1
      ismc    = 2
      isheleg = 3
      istc    = 4
      itg3    = 5
      izorl   = 6
      icv     = 7
      icvb    = 8
      icvt    = 9
      ialbedo = 10
      islmsk  = 11
      iplantr = 12
      icanopy = 13
      if10m   = 14

      else if( ivs.eq.200501 ) then

      islmsk  = 1
      iorog   = 2
      itsea   = 3
      isheleg = 4
      itg3    = 5
      izorl   = 6
      ialvsf  = 7
      ialvwf  = 8
      ialnsf  = 9
      ialnwf  = 10
      ivfrac  = 11
      icanopy = 12
      if10m   = 13
      ivtype  = 14
      istype  = 15
      ifacsf  = 16
      ifacwf  = 17
      iuustar = 18
      iffmm   = 19
      iffhh   = 20
      ihice   = 21
      ifice   = 22
      itprcp  = 23
      isrflag = 24
      isnowdph= 25
      ishdmin = 26
      ishdmax = 27
      islope  = 28
      isnoalb = 29
      istc1   = 30
      istc2   = 31
      istc3   = 32
      istc4   = 33
      ismc1   = 34
      ismc2   = 35
      ismc3   = 36
      ismc4   = 37
      islc1   = 38
      islc2   = 39
      islc3   = 40
      islc4   = 41

      else if( ivs.eq.200509 ) then

      islmsk  = 1
      iorog   = 2
      itsea   = 3
      isheleg = 4
      itg3    = 5
      izorl   = 6
      ialvsf  = 7
      ialvwf  = 8
      ialnsf  = 9
      ialnwf  = 10
      ivfrac  = 11
      icanopy = 12
      if10m   = 13
      it2m    = 14
      iq2m    = 15
      ivtype  = 16
      istype  = 17
      ifacsf  = 18
      ifacwf  = 19
      iuustar = 20
      iffmm   = 21
      iffhh   = 22
      ihice   = 23
      ifice   = 24
      itisfc  = 25
      itprcp  = 26
      isrflag = 27
      isnowdph= 28
      ishdmin = 29
      ishdmax = 30
      islope  = 31
      isnoalb = 32
      istc1   = 33
      istc2   = 34
      istc3   = 35
      istc4   = 36
      ismc1   = 37
      ismc2   = 38
      ismc3   = 39
      ismc4   = 40
      islc1   = 41
      islc2   = 42
      islc3   = 43
      islc4   = 44

      else
        print *,' no correct version to convert '
        call abort
      endif
!
! get slmask first
      call rdsfcdata(nsfc,gg,ijmax,nhead,isheleg)
      call gg2rg(gg,sheleg,1)
      call rdsfcdata(nsfc,slg,ijmax,nhead,islmsk)
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
        dflat=flat*180/pi
        dflon=flon*180/pi
        call clima(idate(4),idate(2),idate(3),idate(1),ghour,             &
     &             lngrd,lsoil, slmsk,dflat,dflon,                        &
     &             fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh,      &
     &             tg3clm,zorclm,albclm,vegclm,vetclm,sotclm,alfclm)
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
! tsea ----------- 3
      call rdsfcdata(nsfc,gg,ijmax,nhead,itsea)
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
      call maxmin(  tsea,lngrd,1,1,1,'tsea')
      call ful2sgl( tsea,rgs,lngrd)
      print *,'in no merge,fft, rtsea(1:10)=',tsea(1:10)
      print *,'in no merge,fft, rtsea(121:130)=',tsea(121:130)
      write(nrsfc)  rgs
! soil moisture ----- 4
      if( ivs.eq.2000 ) then
        call rdsfcdata2(nsfc,ggl,ijmax,nhead,ismc)
      else
        call rdsfcdata(nsfc,gg,ijmax,nhead,ismc1)
        ggl(:,1)=gg
        call rdsfcdata(nsfc,gg,ijmax,nhead,ismc3)
        ggl(:,2)=gg
      endif
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
      call maxmin(   smc,lngrd,lsoil,1,lsoil,'smc')
      call ful2sgl(  smc,rgls,lsoil*lngrd)
      write(nrsfc)   rgls
! snow depth ----- 5
      call rdsfcdata(nsfc,gg,ijmax,nhead,isheleg)
!     call gg2rg(gg,sheleg,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,sheleg,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) sheleg(i)=rr(i)
      enddo
      call maxmin(  sheleg,lngrd,1,1,1,'sheleg')
      call ful2sgl( sheleg,rgs,lngrd)
      write(nrsfc)  rgs
! stc ------------ 6
      if( ivs.eq.2000 ) then
        call rdsfcdata2(nsfc,ggl,ijmax,nhead,istc)
      else
        call rdsfcdata(nsfc,gg,ijmax,nhead,istc1)
        ggl(:,1)=gg
        call rdsfcdata(nsfc,gg,ijmax,nhead,istc3)
        ggl(:,2)=gg
      endif
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
      call maxmin(   stc,lngrd,lsoil,1,lsoil,'stc')
      call ful2sgl(  stc,rgls,lsoil*lngrd)
      write(nrsfc)   rgls
! tg3 ------------ 7
      call rdsfcdata(nsfc,gg,ijmax,nhead,itg3)
!     call gg2rg(gg,tg3,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,tg3,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) tg3(i)=rr(i)
      enddo
      call maxmin(  tg3,lngrd,1,1,1,'tg3')
      if (iclim.eq.1) then
         call ful2sgl( tg3clm,rgs,lngrd)
      else
         call ful2sgl( tg3,rgs,lngrd)
      endif
      write(nrsfc)  rgs
! roughness ----- 8
      call rdsfcdata(nsfc,gg,ijmax,nhead,izorl)
!     call gg2rg(gg,zorl,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,zorl,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) zorl(i)=rr(i)
      enddo
      call maxmin(  zorl,lngrd,1,1,1,'zorl')
      if (iclim.eq.1) then
        call ful2sgl( zorclm,rgs,lngrd)
      else
        call ful2sgl( zorl,rgs,lngrd)
      endif
      write(nrsfc)  rgs
! cloud amount -- 9
! cloud base --- 10
! cloud top ---- 11
      cv=0.0
      cvb=100.0
      cvt=0.0
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
      if( ivs.eq.2000 ) then
        call rdsfcdata2(nsfc,gg4,ijmax,nhead,ialbedo)
      else
        call rdsfcdata(nsfc,gg,ijmax,nhead,ialvsf)
        gg4(:,1)=gg 
        call rdsfcdata(nsfc,gg,ijmax,nhead,ialvwf)
        gg4(:,2)=gg
        call rdsfcdata(nsfc,gg,ijmax,nhead,ialnsf)
        gg4(:,3)=gg
        call rdsfcdata(nsfc,gg,ijmax,nhead,ialnwf)
        gg4(:,4)=gg
      endif
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
      call maxmin(  alvsf,lngrd,1,1,1,'alvsf')
      call maxmin(  alvwf,lngrd,1,1,1,'alvwf')
      call maxmin(  alnsf,lngrd,1,1,1,'alnsf')
      call maxmin(  alnwf,lngrd,1,1,1,'alnwf')
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
      call maxmin( slmsk,lngrd,1,1,1,'slmsk')
      call ful2sgl( slmsk,rgs,lngrd)
      write(nrsfc) rgs
! vegitation fraction ---- 14
      call rdsfcdata(nsfc,gg,ijmax,nhead,ivfrac)
!     call gg2rg(gg,vfrac,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,vfrac,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) vfrac(i)=rr(i)
      enddo
      call maxmin(  vfrac,lngrd,1,1,1,'vfrac')
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
      call rdsfcdata(nsfc,gg,ijmax,nhead,icanopy)
!     call gg2rg(gg,canopy,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,canopy,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) canopy(i)=rr(i)
      enddo
      call maxmin(  canopy,lngrd,1,1,1,'canopy read')
      call ful2sgl( canopy,rgs,lngrd)
      write(nrsfc)  rgs
!
! f10m -------------- 16
      call rdsfcdata(nsfc,gg,ijmax,nhead,if10m)
!     call gg2rg(gg,f10m,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,f10m,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) f10m(i)=rr(i)
      enddo
      call maxmin(  f10m,lngrd,1,1,1,'f10m read')
      call ful2sgl( f10m,rgs,lngrd)
      write(nrsfc)  rgs
!
! vegitation type ---- 17
      call rdsfcdata(nsfc,gg,ijmax,nhead,ivtype)
!     call gg2rg(gg,vtype,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,vtype,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) vtype(i)=rr(i)
      enddo
      call maxmin(  vtype,lngrd,1,1,1,'vtype read')
      if (iclim.eq.1) then
        call ful2sgl( vetclm,rgs,lngrd)
      else
        call ful2sgl( vtype,rgs,lngrd)
      endif
      write(nrsfc)  rgs
!
! soil type ---------- 18
      call rdsfcdata(nsfc,gg,ijmax,nhead,istype)
!     call gg2rg(gg,stype,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,stype,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) stype(i)=rr(i)
      enddo
      call maxmin(  stype,lngrd,1,1,1,'stype read')
      if (iclim.eq.1) then
        call ful2sgl( sotclm,rgs,lngrd)
      else
        call ful2sgl( stype,rgs,lngrd)
      endif
      write(nrsfc)  rgs
!
! zenith angle dependent factor ---- 19
      if( ivs.eq.2000 ) then
        call rdsfcdata2(nsfc,gg2,ijmax,nhead,ialbedo)
      else
        call rdsfcdata(nsfc,gg,ijmax,nhead,ifacsf)
        gg2(:,1)=gg
        call rdsfcdata(nsfc,gg,ijmax,nhead,ifacwf)
        gg2(:,2)=gg
      endif
      call gg2rg(gg2(1,1),facsf,1)
      call gg2rg(gg2(1,2),facwf,1)
      call maxmin(  facsf,lngrd,1,1,1,'facsf read')
      call maxmin(  facwf,lngrd,1,1,1,'facwf read')
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
      call rdsfcdata(nsfc,gg,ijmax,nhead,iuustar)
!     call gg2rg(gg,uustar,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,uustar,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
        if(slmsk(i).eq.1.0) uustar(i)=rr(i)
      enddo
      call maxmin(  uustar,lngrd,1,1,1,'uustar read')
      call ful2sgl( uustar,rgs,lngrd)
      write(nrsfc)  rgs
!
! momentum exchange similarity function -- 21
      call rdsfcdata(nsfc,gg,ijmax,nhead,iffmm)
!     call gg2rg(gg,ffmm,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,ffmm,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) ffmm(i)=rr(i)
      enddo
      call maxmin(  ffmm,lngrd,1,1,1,'ffmm read')
      call ful2sgl( ffmm,rgs,lngrd)
      write(nrsfc)  rgs
!
! heat exchange similarity function -- 22
      call rdsfcdata(nsfc,gg,ijmax,nhead,iffhh)
!     call gg2rg(gg,ffhh,1)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call gg2rg(ggsea,ffhh,1)
      call gg2rg(ggland,rr,1)
      do i=1,lngrd
         if (slmsk(i).eq.1.0) ffhh(i)=rr(i)
      enddo
      call maxmin(  ffhh,lngrd,1,1,1,'ffhh read')
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

      deallocate ( slg,ggl )
      deallocate (  gg,gg2 )
      deallocate ( gg4 )
      deallocate (  gclat )
      deallocate ( colrad, wgt,wgtcs,rcs2 )
      deallocate ( iwork )
      deallocate ( ggsea,ggland,rr )
      deallocate ( idate )
      deallocate ( tsea  , smc    )
      deallocate ( sheleg, stc    )
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
      deallocate ( tg3clm,zorclm )
      deallocate ( albclm,vegclm )
      deallocate ( vetclm,sotclm,alfclm )
!
!
      print *,' +++++ normal exit with some fields specified +++ '
!
      return
      end
      subroutine rdsfchead(nsfcin,fhour,idate,nhead,ivs)
      use machine , only : kind_io4
      integer     nsfcin,idate(4)
      real        fhour
      character*4 cgfs,csfc
      integer*4   ivs,nhead,ndata,nresv(3)
      integer*4   fhours,idates(4),ims,jms,lsoils
      integer*4, allocatable :: lpl(:)
      real(kind=kind_io4),    allocatable :: zoil(:)
      rewind nsfcin
      read(nsfcin) cgfs,csfc,ivs,nhead,ndata,nresv
      write(*,*) 'sfc head',cgfs,csfc,ivs,nhead,ndata,nresv
      if( cgfs.ne.'GFS ' ) go to 100
      read(nsfcin)
      read(nsfcin) fhours,idates,ims,jms,lsoils
      allocate( lpl(jms/2), zoil(lsoils) )
      read(nsfcin) (lpl(j),j=1,jms/2)
      read(nsfcin) (zoil(k),k=1,lsoils)
      nhead=5
      go to 200
100   rewind nsfcin             ! read reanalysis version
      print *,' read sfc file of reanalysis kind '
      read(nsfcin)
      read(nsfcin) fhours,idates
      ivs=2000
      nhead=2
200   fhour=fhours
      do j=1,4
        idate(j)=idates(j)
      enddo
      return
      end

      subroutine rdsfcdata(nsfcin,sfcdata,ijm,nhead,nrec)
      use machine , only : kind_io4
      real   sfcdata(ijm)
      integer nsfcin,ijm,nrec,nhead
      real(kind=kind_io4) sfcs(ijm)
      integer k,ij
      if( nrec.le.0 ) then
        print *,' error in record number in rdsfcdata '
        call abort
      endif
      rewind nsfcin
      do k=1,nhead+nrec-1
        read(nsfcin)
      enddo
      read(nsfcin) (sfcs(ij),ij=1,ijm)
      do ij=1,ijm
        sfcdata(ij) = sfcs(ij)
      enddo
      return
      end
      subroutine rdsfcdata2(nsfcin,sfcdata2,ijm,nhead,nrec)
      use machine , only : kind_io4
      real   sfcdata2(ijm,2)
      integer nsfcin,ijm,nrec
      real(kind=kind_io4) sfcs2(ijm,2)
      integer k,ij
      if( nrec.le.0 ) then
        print *,' error in record number in rdsfcdata2 '
        call abort
      endif
      rewind nsfcin
      do k=1,nhead+nrec-1
        read(nsfcin)
      enddo
      read(nsfcin) ((sfcs2(ij,k),ij=1,ijm),k=1,2)
      do ij=1,ijm
        sfcdata2(ij,1) = sfcs2(ij,1)
        sfcdata2(ij,2) = sfcs2(ij,2)
      enddo
      print *,' read sfc data 2d '
      return
      end

