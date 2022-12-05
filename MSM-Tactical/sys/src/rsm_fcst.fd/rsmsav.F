      subroutine rsmsav
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsmsav
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional sigma, flux, and surface files output
!
! program history log:
!
! usage:    call rsmsav
!   input argument list:
!       gfhour  - global model forecast hour
!
!   output argument list:
!
!   common block:
!       rscomf  comcon
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!       swrite  rfixio
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
#include <rscomfi.h>
#include <rscomspc.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscompln.h>
#include <rscomrad.h>
#include <rscomloc.h>
#include <rscommap.h>
#ifdef RKN
#include <rscomgpd.h>
#endif
!cjun
      dimension tmp(lngrd,levr),tmpk1(lngrd)
      character cfhour*16,cform*40
      integer iflag
!cc
#ifdef MP
      if(mype.eq.master) then
#endif
      print *,' ---- into rwrtres write restart files  ---- '
!
      print 800,deltim,fhour
 800  format(1h0,'dt=',e10.2,2x,'fcst hour =',f10.2,' h')
#ifdef MP
      endif
#endif
!
!..................................................................
      call swrite(nrsmo1,label,fhour,idate,                              &  
     &            flat,flon,fm2,fm2x,fm2y,sl,si,                         &
     &            gz,qm,tem,uum,vvm,rm                                   &
#ifdef NONHYD
     &                     ,pnm,tnm,onm                                  &
#endif
     &                                )

#ifdef MP
      call rmpsynall
#endif

      call swrite(nrsmo2,label,fhour,idate,                              &
     &            flat,flon,fm2,fm2x,fm2y,sl,si,                         &
     &            gz,q ,te ,uu ,vv ,rq                                   &
#ifdef NONHYD
     &                     ,pn ,tn ,on                                   &
#endif
     &                                )

#ifdef MP
      call rmpsynall
#endif

!ccj
      if(ncld.gt.0) then
! 4: zhao scheme;  3: ferrier scheme
      if ((num_p3d .eq. 4) .or.                                          &
     &    (num_p3d .eq. 3)) then   
! iflag=1 for output f_ice f_rain f_rimsf
        iflag=1
        call swrite2(nrsmo1,phy_f3d,phy_f2d,iflag)
#ifdef MP
        call rmpsynall
#endif
        iflag=0
        call swrite2(nrsmo2,phy_f3d,phy_f2d,iflag)
#ifdef MP
        call rmpsynall
#endif
      endif
      endif
!cc
!
!sfc
!................................................
      call rfixio(label,fhour,idate,                                     &
     &   tsea,smc,sheleg,stc,tg3,zorl,                                   &
     &   cv,cvb,cvt,slmsk,f10m,canopy,vfrac,vtype,stype,                 &
     &   uustar,ffmm,ffhh,alvsf,alvwf,alnsf,alnwf,facsf,facwf,           &
     &   1,nrflop,rsfcsec,lsfcmrg,                                       &
     &   flon,flat,lnewsst)
#ifdef MP
      call rmpsynall
#endif
!................................................
!..................................................................
! flx
      secphy=(fhour-zhour)*3600.
      secswr=max(secphy,fhswr*3600.)
      seclwr=max(secphy,fhlwr*3600.)
      nrflxbs=73
!
      call rwrtsfc(zhour,fhour,idate,nrflxbs,slmsk,rlat,rlon,            &  
     &            dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,stc,sheleg,           &
     &            secswr,seclwr,dlwsfc,ulwsfc,                           &
     &            geshem,bengsh,gflux,fluxr,                             &
     &            u10m,v10m,t2m,q2m,psurf,zorl,tmpmax,tmpmin,            &
     &            spfhmax,spfhmin,wvuflx,wvvflx,                         &
     &            srunoff,soilm,snwdph,                                  &
     &            runoff,ep,cldwrk,dugwd,dvgwd,hpbl,pwat,                &
     &            icen,igen,icen2,ienst,iensi)
#ifdef MP
      call rmpsynall
#endif
!..............regional ken points ...................
#ifdef RKN
      if( npoint.gt.0 ) then
#ifdef MP
      if( mype.eq.master ) then
#endif
        open(unit=nrkenp,file='r_ken_final',                             &   
     &           status='unknown',form='unformatted')
#ifdef MP
      endif
#endif
        call rkenwrt(nrkenp,label,fhour,idate,si,sl)
#ifdef MP
      if( mype.eq.master ) then
#endif
        close(nrkenp)
#ifdef MP
        call rmpsynall
      endif
#endif
      endif
#endif
!................................................
      return
      end
