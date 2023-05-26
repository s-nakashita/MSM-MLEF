      subroutine rwrtout
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
#include <rscomgpd.h>
!cc
!
      character cfhour*16,cform*40,asgnstr*32,filename*100
      integer iflag
      save icall
      data icall/0/
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,' ---- into rwrtout ---- '
!
      print 800,deltim,fhour
 800  format(1h0,'rsmsav dt=',e10.2,2x,'fcst hour =',f10.2,' h')
#ifdef MP
      endif
#endif
!
#ifdef CRA
      if(icall.eq.0) then
        icall=1
        write(asgnstr,'(29hassign -nibm -fcos -cascii u:,i2)') nrsmop
        call assign(asgnstr)
        write(asgnstr,'(29hassign -nibm -fcos -cascii u:,i2)') nrsfcp
        call assign(asgnstr)
        write(asgnstr,'(29hassign -nibm -fcos -cascii u:,i2)') nrflxp
        call assign(asgnstr)
        write(asgnstr,'(30hassign -ff77 -nieee -cascii u:,i2)') nrkenp
        call assign(asgnstr)
      endif
#endif
      if(lfnhr) then
#ifdef MP
      if( mype.eq.master ) then
#endif
        kh=nint(fhour)
        ndig=max(log10(kh+0.5)+1.,2.)
        write(cform,'("(i",i1,".",i1,")")') ndig,ndig
        write(cfhour,cform) kh
#ifdef MP
      endif
#endif
      else
#ifdef MP
      if( mype.eq.master ) then
#endif
        ks=nint(fhour*3600)
        kh=ks/3600
        km=(ks-kh*3600)/60
        ks=ks-kh*3600-km*60
        ndig=max(log10(kh+0.5)+1.,2.)
        write(cform,'("(i",i1,".",i1,",a1,i2.2,a1,i2.2)")') ndig,ndig
        write(cfhour,cform) kh,':',km,':',ks
#ifdef MP
      endif
#endif
      endif
!
! sig
#ifdef MP
      if( mype.eq.master ) then
#endif
!hmhj open(unit=nrsmop,file='r_sigf'//cfhour                             &   
!hmhj&      ,status='unknown',form='unformatted')
      write(filename,"(a,a)") 'r_sigf',cfhour
      open(unit=nrsmop,file=filename                                     &   
     &      ,status='unknown',form='unformatted')
      write(filename,"(a,a)") 'rbssigf',cfhour
      open(unit=nrsmbo,file=filename                                     &   
     &      ,status='unknown',form='unformatted')
#ifdef MP
      endif
#endif
      call swrite(nrsmop,label,fhour,idate,                              &
     &               flat,flon,fm2,fm2x,fm2y,sl,si,                      &
     &               gz,qm ,tem ,uum ,vvm ,rm                            &
#ifdef NONHYD
     &               ,pnm,tnm,onm                                        &
#endif
     &               )
      call bwrite(nrsmbo,label,fhour,idate,                              &
     &            flat,flon,fm2,fm2x,fm2y,sl,si,                         &
     &            sgzb,sqb,steb,suub,svvb,srqb                           &
     &                                )
      if(ncld.gt.0) then
! 4: zhao scheme;  3: ferrier scheme
      if ((num_p3d .eq. 4) .or.                                          &
     &    (num_p3d .eq. 3)) then
! iflag=1 for output f_ice f_rain f_rimsf
        iflag=1
        call swrite2(nrsmop,phy_f3d,phy_f2d,iflag)
      endif
      endif
#ifdef MP
      if( mype.eq.master ) then
#endif
      close(nrsmop)
! flx
!hmhj open(unit=nrflxp,file='r_flxf'//cfhour,                            &
!hmhj&        status='unknown',form='unformatted')
      write(filename,"(a,a)") 'r_flxf',cfhour
      open(unit=nrflxp,file=filename,                                    &   
     &        status='unknown',form='unformatted')
#ifdef MP
      endif
#endif
      secphy=(fhour-zhour)*3600.
      secswr=max(secphy,fhswr*3600.)
      seclwr=max(secphy,fhlwr*3600.)
!
      call rwrtsfc(zhour,fhour,idate,nrflxp,slmsk,rlat,rlon,             &
     &            dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,stc,sheleg,           &
     &            secswr,seclwr,dlwsfc,ulwsfc,                           &
     &            geshem,bengsh,gflux,fluxr,                             &
     &            u10m,v10m,t2m,q2m,psurf,zorl,tmpmax,tmpmin,            &
     &            spfhmax,spfhmin,wvuflx,wvvflx,                         &
     &            srunoff,soilm,snwdph,                                  &
     &            runoff,ep,cldwrk,dugwd,dvgwd,hpbl,pwat,                &
     &            icen,igen,icen2,ienst,iensi)
#ifdef MP
      if( mype.eq.master ) then
#endif
      close(nrflxp)
! sfc
!hmhj open(unit=nrsfcp,file='r_sfcf'//cfhour,                            &
!hmhj&        status='unknown',form='unformatted') 
      write(filename,"(a,a)") 'r_sfcf',cfhour
      open(unit=nrsfcp,file=filename,                                    &   
     &        status='unknown',form='unformatted') 
#ifdef MP
      endif
#endif
      call rfixio(label,fhour,idate,                                     &
     &   tsea,smc,sheleg,stc,tg3,zorl,                                   &
     &   cv,cvb,cvt,slmsk,f10m,canopy,vfrac,vtype,stype,                 &
     &   uustar,ffmm,ffhh,alvsf,alvwf,alnsf,alnwf,facsf,facwf,           &
     &   1,nrsfcp,rsfcsec,lsfcmrg,                                       &
     &   flon,flat,lnewsst)
#ifdef MP
      if( mype.eq.master ) then
#endif
      close(nrsfcp)

#ifdef MP
      endif
#endif
!..............regional ken points ...................
#ifdef RKN
      if(npoint.gt.0) then
#ifdef MP
      if( mype.eq.master ) then
#endif
!hmhj   open(unit=nrkenp,file='r_kenf'//cfhour,                          &  
!hmhj&           status='unknown',form='unformatted')
        write(filename,"(a,a)") 'r_kenf',cfhour
        open(unit=nrkenp,file=filename,                                    &   
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
      endif
#endif
      endif
#endif
!
      return
      end
