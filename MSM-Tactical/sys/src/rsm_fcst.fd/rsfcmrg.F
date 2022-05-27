       subroutine rsfcmrg(label,fhour,idate,                             &  
     &    tsea,smc,sheleg,stc,tg3,zorl,                                  &
     &    cv,cvb,cvt,slmsk,f10m,canopy,vfrac,vtype,stype,                &
     &    uustar,ffmm,ffhh,alvsf,alvwf,alnsf,alnwf,facsf,facwf)
!
! usage:    call rsfcmrg(fhour,tsea,smc,sheleg,stc,tg3,zorl,
!          &    cv,cvb,cvt,albed,slmsk,f10m,canopy,hprime,
!          &    ioflag,nread,nunit,shour)
!   input argument list: or output argument list: depends on ioflag
!     fhour     - forecast hour
!     tsea      - land and sea surface temperature (k)
!     smc       - two layers of soil moisture contents (0.47 - 0.1)
!     sheleg    - snow depth (cm)
!     stc       - two layers of soil temperature (k)
!     tg3       - the lowest soil temperature at 3m (k)
!     zorl      - surface roughness
!     plantr    - planetary resistence
!     cv        - cloud amount
!     cvb       - cloud base (sigma layer number)
!     cvt       - cloud top (sigma layer number)
!     albed     - albedo
!     slmsk     - sea land mask
!     f10m      - 10 m height factor for wind
!     canopy    - surface canopy
!
!$$$
      use machine , only : kind_io4
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define LNGRDS lngrdp
#else
#define LNGRDS lngrd
#endif
#ifdef G2R
#define IIMAX lonf
#define JJMAX latg
#else
#define IIMAX cigrd1
#define JJMAX cjgrd1
#endif

      parameter (imax=IIMAX, jmax=JJMAX, ijmax=imax*jmax)

      real(kind=kind_io4) ghours,fhours
      real(kind=kind_io4) ggs(ijmax),ggls(ijmax,lsoil)
      real(kind=kind_io4) gg2s(ijmax,2),gg4s(ijmax,4)
      real(kind=kind_io4) rgs(LNGRDS),rgls(LNGRDS,lsoil)
      real(kind=kind_io4) rg2s(LNGRDS,2),rg4s(LNGRDS,4)
!cc jun write out
      real(kind=kind_io4) grgs(lngrd),grgls(lngrd,lsoil)
      real(kind=kind_io4) grg2s(lngrd,2),grg4s(lngrd,4)
      real(kind=kind_io4) rrgs(lngrd),rrgls(lngrd,lsoil)
      real(kind=kind_io4) rrg2s(lngrd,2),rrg4s(lngrd,4)
!cc
      dimension slg(ijmax),ggl(ijmax,lsoil)                              &  
     &         , gg(ijmax),gg2(ijmax,2)                                  &
     &         ,gg4(ijmax,4)
      dimension ggsea(ijmax),ggland(ijmax),iwork(ijmax)
      dimension fgrgs(lngrd), fgrgls(lngrd,lsoil)                        &
     &         ,fgrg2s(lngrd,2), fgrg4s(lngrd,4)                         
      dimension  gclat(jmax)                                             &
     &  , colrad(latg2), wgt(latg2),wgtcs(latg2),rcs2(latg2)
      dimension idate(4)
      character*8 label1(4), label(4)
      dimension gtsea  (LNGRDS), gsmc   (LNGRDS,lsoil),                  &
     &          gsheleg(LNGRDS), gstc   (LNGRDS,lsoil),                  &
     &          gtg3   (LNGRDS), gvfrac (LNGRDS),                        &
     &          gzorl  (LNGRDS), gslmsk (LNGRDS),                        &
     &          gcv    (LNGRDS), gcvb   (LNGRDS),                        &
     &          gcvt   (LNGRDS),                                         &
     &          gf10m  (LNGRDS), gcanopy(LNGRDS),                        &
     &          gvtype (LNGRDS),                                         &
     &          gstype (LNGRDS), guustar(LNGRDS),                        &
     &          gffmm  (LNGRDS),   gffhh(LNGRDS),                        &
     &          galvsf (LNGRDS),  galvwf(LNGRDS),                        &
     &          galnsf (LNGRDS),  galnwf(LNGRDS),                        &
     &          gfacsf (LNGRDS),  gfacwf(LNGRDS)
      dimension rr     (LNGRDS)
      dimension tsea  (LNGRDS), smc   (LNGRDS,lsoil),                    &
     &          sheleg(LNGRDS), stc   (LNGRDS,lsoil),                    &
     &          tg3   (LNGRDS), vfrac (LNGRDS),                          &
     &          zorl  (LNGRDS), slmsk (LNGRDS),                          &
     &          cv    (LNGRDS), cvb   (LNGRDS),                          &
     &          cvt   (LNGRDS),                                          &
     &          f10m  (LNGRDS), canopy(LNGRDS),                          &
     &          vtype (LNGRDS),                                          &
     &          stype (LNGRDS), uustar(LNGRDS),                          &
     &          ffmm  (LNGRDS),   ffhh(LNGRDS),                          &
     &          alvsf (LNGRDS),  alvwf(LNGRDS),                          &
     &          alnsf (LNGRDS),  alnwf(LNGRDS),                          &
     &          facsf (LNGRDS),  facwf(LNGRDS)
!     &          ,rslmsk (LNGRDS),
      integer,save :: version
      data version/199802/
      dimension slmk(LNGRDS),simk(LNGRDS)                                &
     &         ,flat(LNGRDS),flon(LNGRDS)
      character cfhour*16,cform*40,filename*100
      data  nsfc/12/
      save nsfc

!cc
#ifdef MP
      if( mype.eq.master ) then
#endif
      kh=nint(fhour)
      ndig=max(log10(kh+0.5)+1.,2.)
      write(cform,'("(i",i1,".",i1,")")') ndig,ndig
      write(cfhour,cform) kh
!hmhj open(unit=nsfc,file='rb_sfcf'//cfhour                              &  
!hmhj&    ,status='old',form='unformatted')
      write(filename,"(a,a)") 'rb_sfcf',cfhour
      open(unit=nsfc,file=filename                                       &  
     &    ,status='old',form='unformatted')
      print *,'in s2rinp_mrg, nsfc=',nsfc,'open rb_sfcf=',cfhour
!
      rewind nsfc
!
! get slmask first
      read(nsfc) label1
      read(nsfc) ghours,idate
      ghour=ghours
      read(nsfc)               ! tsea
      read(nsfc)               ! smc
      read(nsfc) ggs            ! sheleg
      call sgl2ful(ggs,gg,ijmax)
#ifdef MP
      endif
       call rmpbcastr(gg,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(gg,gsheleg,1)
#else
      call cg2rg_mpi(gg,gsheleg,1)
#endif
#ifdef MP
      if( mype.eq.master ) then
#endif
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
#ifdef MP
       endif
       call rmpbcastr(gg,ijmax)
#endif
#ifdef G2R
       call gg2rg_mpi(gg,simk,1)
#else
       call cg2rg_mpi(gg,simk,1)
#endif
      do 20 i=1,LNGRDS
      if( simk(i) .gt. 0.9 ) then
          simk(i) = 2.0
      else
          simk(i) = 0.0
      endif
 20   continue

#ifdef MP
      if( mype.eq.master ) then
#endif
! get sea and land index
      do 30 i=1,ijmax
      gg(i) = 0.0
      if( slg(i) .eq. 1.0 ) gg(i) = 1.0
 30   continue
#ifdef MP
       endif
       call rmpbcastr(gg,ijmax)
#endif
#ifdef G2R
       call gg2rg_mpi(gg,slmk,1)
#else
       call cg2rg_mpi(gg,slmk,1)
#endif
        
      do 40 i=1,LNGRDS
      if( slmk(i) .gt. 0.4 ) then
          slmk(i) = 1.0
      else
          slmk(i) = 0.0
      endif
 40   continue
!       do 45 i=1,LNGRDS
!       gslmsk(i) = max( simk(i), slmk(i) )
!   45  continue
        do 45 i=1,LNGRDS
! add sea ice info
        slmsk(i) = max(slmsk(i),simk(i))
 45     continue
!
! put sea ice point into sea point
#ifdef MP
      if( mype.eq.master ) then
#endif
        do i=1,ijmax 
        if(slg(i).eq.2.0) slg(i)=0.0
        enddo
#ifdef MP
       endif
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      rewind nsfc

      read(nsfc) label1
! hour idate ----- 2
      read(nsfc) ghours,idate
! tsea ----------- 3  -----merge from Ocean 
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
      call maxmin(gg,ijmax,1,1,1,'tsea')
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,gtsea,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,gtsea,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gtsea(i)=rr(i)
      enddo
      do 50 i=1,LNGRDS
      if( slmsk(i) .eq. 2.0e0 ) then
          gtsea(i)= min( gtsea(i), 271.20e0 )
      else if( slmsk(i).eq.0.0e0 ) then
          gtsea(i)= max( gtsea(i), 271.21e0 )
      else if( slmsk(i).eq.1.0e0 .and.                                &  
     &         gsheleg(i).gt.0.0e0 ) then
          gtsea(i)= min( gtsea(i), 273.16e0 )
      endif
 50   continue

      do 101 i=1,LNGRDS
        if( slmsk(i) .eq. 0.0 ) tsea(i)= gtsea(i)
 101  continue
#ifdef MP
      if( mype.eq.master ) then
#endif
! soil moisture ----- 4
      read(nsfc) ggls
!!!      call sgl2ful(ggls,ggl,lsoil*ijmax)
#ifdef MP
      endif
!!!      call rmpbcastr(ggl,lsoil*ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(ggl,gsmc,lsoil)
!!!#else
!!!      call cg2rg_mpi(ggl,gsmc,lsoil)
!!!#endif
!
! snow depth ----- 5
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nsfc) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
!!!      call maxmin(gg,ijmax,1,1,1,'snow depth')
#ifdef MP
      endif
!!!      call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(gg,gsheleg,1)
!!!#else
!!!      call cg2rg_mpi(gg,gsheleg,1)
!!!#endif
!
! stc ------------ 6
#ifdef MP
      if( mype.eq.master ) then
#endif
       read(nsfc) ggls
!!!      call sgl2ful(ggls,ggl,lsoil*ijmax)
#ifdef MP
      endif
!!!      call rmpbcastr(ggl,lsoil*ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(ggl,gstc,lsoil)
!!!#else
!!!      call cg2rg_mpi(ggl,gstc,lsoil)
!!!#endif

! tg3 ------------ 7  ---- Merge all from global cycle ----
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nsfc)  ggs
      call sgl2ful(ggs,gg,ijmax)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,gtg3,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,gtg3,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gtg3(i)=rr(i)
      enddo
      do 60 i=1,LNGRDS
        tg3(i)= gtg3(i)
 60   continue
! roughness ----- 8 ---- merge ovre land ----
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call maxmin(gg,ijmax,1,1,1,'roughness')
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,gzorl,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,gzorl,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gzorl(i)=rr(i) 
      enddo
      do 70 i=1,LNGRDS
        if( slmsk(i) .ge. 1.0 ) zorl(i)= gzorl(i)
 70   continue

#ifdef MP
      if( mype.eq.master ) then
#endif
! cloud amount -- 9
      read(nsfc) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
#ifdef MP
      endif
!!!      call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(gg,gcv,1)
!!!#else
!!!      call cg2rg_mpi(gg,gcv,1)
!!!#endif

#ifdef MP
      if( mype.eq.master ) then
#endif
! cloud base --- 10
      read(nsfc) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
#ifdef MP
      endif
!!!      call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(gg,gcvb,1)
!!!#else
!!!      call cg2rg_mpi(gg,gcvb,1)
!!!#endif

#ifdef MP
      if( mype.eq.master ) then
#endif
! cloud top ---- 11
      read(nsfc) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
!!!      call maxmin(gg,ijmax,1,1,1,'cloud top')
#ifdef MP
      endif
!!!      call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(gg,gcvt,1)
!!!#else
!!!      call cg2rg_mpi(gg,gcvt,1)
!!!#endif

! 4 albedo ------- 12
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nsfc) gg4s
      call sgl2ful(gg4s,gg4,ijmax*4)
      call expnd(gg4(1,1),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,1),slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,galvsf,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,galvsf,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) galvsf(i)=rr(i)
      enddo
#ifdef MP
      if( mype.eq.master ) then
#endif
      call expnd(gg4(1,2),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,2),slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,galvwf,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,galvwf,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) galvwf(i)=rr(i)
      enddo
#ifdef MP
      if( mype.eq.master ) then
#endif
      call expnd(gg4(1,3),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,3),slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,galnsf,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,galnsf,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) galnsf(i)=rr(i)
      enddo
#ifdef MP
      if( mype.eq.master ) then
#endif
      call expnd(gg4(1,4),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg4(1,4),slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,galnwf,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,galnwf,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) galnwf(i)=rr(i)
      enddo
!cjun


!  for r sfc

      do 2103 i=1,LNGRDS
          alvsf(i)= galvsf(i)
2103     continue
      do 2104 i=1,LNGRDS
          alvwf(i)= galvwf(i)
2104     continue
      do 2105 i=1,LNGRDS
          alnsf(i)= galnsf(i)
2105     continue
      do 2106 i=1,LNGRDS
          alnwf(i)= galnwf(i)
2106     continue

!
#ifdef MP
      if( mype.eq.master ) then
#endif
! sea land mask -- 13
      read(nsfc) ggs
#ifdef MP
      endif
#endif

#ifdef MP
      if( mype.eq.master ) then
#endif
! vegitation fraction ---- 14
      read(nsfc) ggs
      call sgl2ful(ggs,gg,ijmax)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call maxmin(gg,ijmax,1,1,1,'vegitation fraction')
#ifdef MP
      endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,gvfrac,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,gvfrac,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gvfrac(i)=rr(i)
      enddo
      do 103 i=1,LNGRDS
          if( slmsk(i) .ge. 1.0 ) vfrac(i)= gvfrac(i)
103     continue
! prepare 15 to end for end of file condition
      do i=1,LNGRDS
        gcanopy(i)=60.0
        gf10m(i)=0.8
        gvtype(i)=1.0
        gstype(i)=1.0
        gfacsf(i)=1.0
        gfacwf(i)=1.0
        guustar(i) = 1.
        gffmm(i) = log(30.)
        gffhh(i) = log(30.)
      enddo
#ifdef MP
      if( mype.eq.master ) then
#endif
! vegitation canopy ------ 15
      read(nsfc,end=1500) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
!!!      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
!!!      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
#ifdef MP
      endif
!!!      call rmpbcastr(ggsea,ijmax)
!!!      call rmpbcastr(ggland,ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(ggsea,gcanopy,1)     
!!!     call gg2rg_mpi(ggland,rr,1)
!!!#else
!!!      call cg2rg_mpi(ggsea,gcanopy,1)
!!!      call cg2rg_mpi(ggland,rr,1)
!!!#endif
!!!      do i=1,LNGRDS
!!!         if (slmsk(i).eq.1.0) gcanopy(i)=rr(i)    
!!!      enddo
!!!        do 104 i=1,LNGRDS
!!!          if( slmsk(i) .ge. 1.0 ) canopy(i)= gcanopy(i)
!!!104     continue
#ifdef MP
      if( mype.eq.master ) then
#endif
! f10m -------------- 16
      read(nsfc,end=1500)ggs
!!!      call sgl2ful(ggs,gg,ijmax)
#ifdef MP
      endif
!!!      call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!      call gg2rg_mpi(gg,gf10m,1)
!!!#else
!!!      call cg2rg_mpi(gg,gf10m,1)
!!!#endif

#ifdef MP
      if( mype.eq.master ) then
#endif
! vegitation type ---- 17 ---merge over land ----
      read(nsfc,end=1500) ggs
      call sgl2ful(ggs,gg,ijmax)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
#ifdef MP
       endif
      call rmpbcastr(ggsea,ijmax)
      call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
       call gg2rg_mpi(ggsea,gvtype,1)
       call gg2rg_mpi(ggland,rr,1)
#else
       call cg2rg_mpi(ggsea,gvtype,1)
       call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gvtype(i)=rr(i)     
      enddo
!cjun

!      call ful2sgl(vtype,rgs,LNGRDS)
!      call sgl2ful(rgs,vtype,LNGRDS)
        do 105 i=1,LNGRDS
          if( slmsk(i) .ge. 1.0 ) vtype(i)= gvtype(i)
105     continue
!
#ifdef MP
      if( mype.eq.master ) then
#endif
! soil type ---------- 18 ----merge over land ----
      read(nsfc,end=1500) ggs
      call sgl2ful(ggs,gg,ijmax)
      call expnd(gg,slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg,slg,1,ggland,iwork,imax,jmax)
      call maxmin(gg,ijmax,1,1,1,'soil type 18')
#ifdef MP
       endif
       call rmpbcastr(ggsea,ijmax)
       call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
       call gg2rg_mpi(ggsea,gstype,1)
       call gg2rg_mpi(ggland,rr,1)
#else
       call cg2rg_mpi(ggsea,gstype,1)
       call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gstype(i)=rr(i)      
      enddo

!      call ful2sgl(stype,rgs,LNGRDS)
!      call sgl2ful(rgs,stype,LNGRDS)
        do 106 i=1,LNGRDS
          if( slmsk(i) .ge. 1.0 ) stype(i)= gstype(i)
106     continue
#ifdef MP
      if( mype.eq.master ) then
#endif
! zenith angle dependent factor ---- 19 ----merge onver land ----
      read(nsfc,end=1500) gg2s
      call sgl2ful(gg2s,gg2,ijmax*2)
      call expnd(gg2(1,1),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg2(1,1),slg,1,ggland,iwork,imax,jmax)
      call maxmin(gg,ijmax*2,1,1,1,'zenith angle')
#ifdef MP
       endif
       call rmpbcastr(ggsea,ijmax)
       call rmpbcastr(ggland,ijmax)
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,gfacsf,1)
      call gg2rg_mpi(ggland,rr,1)
#else
      call cg2rg_mpi(ggsea,gfacsf,1)
      call cg2rg_mpi(ggland,rr,1)
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gfacsf(i)=rr(i)
      enddo
#ifdef MP
      if( mype.eq.master ) then
#endif
      call expnd(gg2(1,2),slg,0,ggsea,iwork,imax,jmax)
      call expnd(gg2(1,2),slg,1,ggland,iwork,imax,jmax)
#ifdef MP
       endif
       call rmpbcastr(ggsea,ijmax)   
       call rmpbcastr(ggland,ijmax)   
#endif
#ifdef G2R
      call gg2rg_mpi(ggsea,gfacwf,1)   
      call gg2rg_mpi(ggland,rr,1)          
#else
      call cg2rg_mpi(ggsea,gfacwf,1)   
      call cg2rg_mpi(ggland,rr,1)          
#endif
      do i=1,LNGRDS
         if (slmsk(i).eq.1.0) gfacwf(i)=rr(i)
      enddo

!  for r sfc
      do 107 i=1,LNGRDS
        if( slmsk(i) .ge. 1.0 ) facsf(i)= gfacsf(i)
        if( slmsk(i) .ge. 1.0 ) facwf(i)= gfacwf(i)
107   continue
1500  continue
#ifdef MP
      if( mype.eq.master ) then
#endif
! ustar ------------ 20
      read(nsfc,end=2000) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
!!!      call maxmin(gg,ijmax,1,1,1,'ustar')
#ifdef MP
       endif
!!!       call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!       call gg2rg_mpi(gg,guustar,1)
!!!#else
!!!       call cg2rg_mpi(gg,guustar,1)
!!!#endif
!
!
#ifdef MP
      if( mype.eq.master ) then
#endif
!!!      call maxmin(gg,ijmax,1,1,1,'ustar')
! momentum exchange similarity function -- 21
      read(nsfc,end=2000) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
!!!      call maxmin(gg,ijmax,1,1,1,'momentum function')
#ifdef MP
       endif
!!!       call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!       call gg2rg_mpi(gg,gffmm,1)
!!!#else
!!!       call cg2rg_mpi(gg,gffmm,1)
!!!#endif
!cjun

#ifdef MP
      if( mype.eq.master ) then
#endif
! heat exchange similarity function -- 22
      read(nsfc,end=2000) ggs
!!!      call sgl2ful(ggs,gg,ijmax)
!!!      call maxmin(gg,ijmax,1,1,1,'heat exchange function')
#ifdef MP
       endif
!!!       call rmpbcastr(gg,ijmax)
#endif
!!!#ifdef G2R
!!!       call gg2rg_mpi(gg,gffhh,1)
!!!#else
!!!       call cg2rg_mpi(gg,gffhh,1)
!!!#endif


 2000  continue
#ifdef MP
      if( mype.eq.master ) then
#endif
       close(nsfc)
#ifdef MP
       endif
#endif

       return
       end
