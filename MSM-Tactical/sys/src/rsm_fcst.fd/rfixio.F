      subroutine rfixio(label,fhour,idate,                               &  
     &    tsea,smc,sheleg,stc,tg3,zorl,                                  &
     &    cv,cvb,cvt,slmsk,f10m,canopy,vfrac,vtype,stype,                &
     &    uustar,ffmm,ffhh,alvsf,alvwf,alnsf,alnwf,facsf,facwf,          &
     &    ioflag,nunit,rsfcsec,lsfcmrg,                                  &
     &    flon,flat,lnewsst) !added by SN
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rfixio
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional surface input and output
!
! program history log:
!
! usage:    call rfixio(fhour,tsea,smc,sheleg,stc,tg3,zorl,plantr,
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
!     hprime    - mountain variance
!     ioflag    - 0 : for read, so all of above as output
!                 1 : for write, so all of above as input
!     shour     - second for the current sequential integration
!
!
!   input files:
!     nread
!
!   output files:
!     nunit
!
!   subprograms called: 
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use machine , only : kind_io4
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      real, allocatable :: spec(:)
      real, allocatable :: spec2(:)
      real, allocatable :: spec3(:)
      real, allocatable :: grid(:,:)
#define LNGRDS lngrdp
#else
#define LNGRDS lngrd
#endif
!cc
      real(kind=kind_io4) fhours
      real(kind=kind_io4), allocatable :: tmps(:)
      real(kind=kind_io4), allocatable :: tmpsl(:,:)
      real(kind=kind_io4), allocatable :: tmps2(:,:)
      real(kind=kind_io4), allocatable :: tmps4(:,:)
      dimension idate(4)
      character*8 label(4)
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
     &          facsf (LNGRDS),  facwf(LNGRDS),                          &
     &          flon  (LNGRDS),  flat (LNGRDS) !added by SN
      integer,save :: version
      data version/199802/
      logical lsfcmrg
      logical lnewsst !added by SN
#ifdef MP
      allocate (spec(lngrd))
      allocate (spec2(lngrd))
      allocate (spec3(lngrd))
      allocate (grid(lngrd,lsoil))
#endif
      allocate (tmps(lngrd))
      allocate (tmpsl(lngrd,lsoil))
      allocate (tmps2(lngrd,2))
      allocate (tmps4(lngrd,4))
!
!  ioflag = 0  ...  read fixed field from unit nunit
!  ioflag = 1  ...  write fixed field to unit nunit
!
      if(ioflag.eq.0) then
!
! start read --------------------------------------
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      rewind nunit
      read(nunit) label
      read(nunit) fhours, idate
      fhour=fhours
99    format(1h ,'fhour, idate=',f6.2,2x,4(1x,i4))
      print *,'fix field read in from unit=',nunit
      print 99,fhour, idate
#ifdef MP
      endif
      call rmpbcastc(label,32)
      call rmpbcastr(fhour,1)
      call rmpbcasti(idate,4)
#endif
!--------------------------------------------1.tsea
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,tsea,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,tsea,lngrd)
#endif
!--------------------------------------------2.smc
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmpsl
#ifdef MP
      call sgl2ful(tmpsl,grid, lsoil*lngrd)
      endif
      call rmpgf2p(grid,igrd1,jgrd1,smc,igrd1p,jgrd1p,lsoil)
#else
      call sgl2ful(tmpsl,smc,lsoil*lngrd)
#endif
!--------------------------------------------3.sheleg
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
      !tmps = tmps * 10 !cm->mm
      !tmps = tmps * 0.1 !mm->cm
      !tmps = tmps * 0.01 !mm->m
      tmps = tmps * 0.0 !no snow
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,sheleg,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,sheleg,lngrd)
#endif
!--------------------------------------------4.stc
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmpsl
#ifdef MP
      call sgl2ful(tmpsl,grid, lsoil*lngrd)
      endif
      call rmpgf2p(grid,igrd1,jgrd1,stc,igrd1p,jgrd1p,lsoil)
#else
      call sgl2ful(tmpsl,stc,lsoil*lngrd)
#endif
!--------------------------------------------5.tg3
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,tg3,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,tg3,lngrd)
#endif
!--------------------------------------------6.zorl
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,zorl,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,zorl,lngrd)
#endif
!--------------------------------------------7.cv
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,cv,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,cv,lngrd)
#endif
!--------------------------------------------8.cvb
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,cvb,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,cvb,lngrd)
#endif
!--------------------------------------------9.cvt
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,cvt,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,cvt,lngrd)
#endif
!--------------------------------------------10.albedo
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps4
#ifdef MP
      call sgl2ful(tmps4(1,1),spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,alvsf,igrd1p,jgrd1p,1)
      if( mype.eq.master ) then
      call sgl2ful(tmps4(1,2),spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,alvwf,igrd1p,jgrd1p,1)
      if( mype.eq.master ) then
      call sgl2ful(tmps4(1,3),spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,alnsf,igrd1p,jgrd1p,1)
      if( mype.eq.master ) then
      call sgl2ful(tmps4(1,4),spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,alnwf,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps4(1,1),alvsf,lngrd)
      call sgl2ful(tmps4(1,2),alvwf,lngrd)
      call sgl2ful(tmps4(1,3),alnsf,lngrd)
      call sgl2ful(tmps4(1,4),alnwf,lngrd)
#endif
!--------------------------------------------11.slmsk
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,slmsk,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,slmsk,lngrd)
#endif
!--------------------------------------------12.vfrac
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,vfrac,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,vfrac,lngrd)
#endif
!--------------------------------------------13.canopy
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,err=5000)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,canopy,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,canopy,lngrd)
#endif
!--------------------------------------------14.f10m
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,err=5000)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,f10m,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,f10m,lngrd)
#endif
!--------------------------------------------15.vtype
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,err=5000)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,vtype,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,vtype,lngrd)
#endif
!--------------------------------------------16.stype
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,err=5000)  tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,stype,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,stype,lngrd)
#endif
!--------------------------------------------17.facswf
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,err=5000)  tmps2
#ifdef MP
      call sgl2ful(tmps2(1,1),spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,facsf,igrd1p,jgrd1p,1)
      if( mype.eq.master ) then
      call sgl2ful(tmps2(1,2),spec, lngrd)
      endif
      call rmpgf2p(spec,igrd1,jgrd1,facwf,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps2(1,1),facsf,lngrd)
      call sgl2ful(tmps2(1,2),facwf,lngrd)
#endif
!--------------------------------
#ifdef MP
      do ij = 1, lngrd
        spec(ij) = 1.
        spec2(ij) = log(30.)
        spec3(ij) = log(30.)
      enddo
#else
      do ij = 1, lngrd
        uustar(ij) = 1.
        ffmm(ij) = log(30.)
        ffhh(ij) = log(30.)
      enddo
#endif
!--------------------------------------------18.uustar
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,end=200)   tmps
#ifdef MP
      call sgl2ful(tmps,spec, lngrd)
 200  continue
      endif
      call rmpgf2p(spec,igrd1,jgrd1,uustar,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,uustar,lngrd)
 200  continue
#endif
!--------------------------------------------19.ffmm
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,end=201)  tmps
#ifdef MP
      call sgl2ful(tmps,spec2, lngrd)
 201  continue
      endif
      call rmpgf2p(spec2,igrd1,jgrd1,ffmm,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,ffmm,lngrd)
 201  continue
#endif
!--------------------------------------------20.ffhh
#ifdef MP
      if( mype.eq.master ) then
#endif
      read(nunit,end=202)  tmps
#ifdef MP
      call sgl2ful(tmps,spec3, lngrd)
 202  continue
      endif
      call rmpgf2p(spec3,igrd1,jgrd1,ffhh,igrd1p,jgrd1p,1)
#else
      call sgl2ful(tmps,ffhh,lngrd)
 202  continue
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,'in rfixio, rsfc read finished!'
#ifdef MP
      endif
#endif
      else
!
!  start to write -----------------------------------------------
!
!ccc jun change to add merge sfc
!
      kfhours=nint(fhour*3600)
      kdays=mod(kfhours,nint(rsfcsec))
      if ( lsfcmrg .and. kdays.eq.0 .and. fhour .gt.0) then
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,'rsfcmrg write from unit=',nunit,                          &  
     &      ' fhour,idate=',fhour,idate
#ifdef MP
      endif
#endif
           call rsfcmrg(label,fhour,idate,                               &   
     &   tsea,smc,sheleg,stc,tg3,zorl,                                   &
     &   cv,cvb,cvt,slmsk,f10m,canopy,vfrac,vtype,stype,                 &
     &   uustar,ffmm,ffhh,alvsf,alvwf,alnsf,alnwf,facsf,facwf,           &
     &   flon,flat,lnewsst) !added by SN
      endif
#ifdef MP
      if( mype.eq.master ) then
      print *,'rsfcmrg finished'
#endif
      rewind nunit
      label(1)='ncep'
      label(2)='rsm'
      label(3)='mpi'
      label(4)='version'
      write(nunit) label
      fhours=fhour
      write(nunit) fhours, idate, igrd1, jgrd1, version
#ifdef MP
      endif
#endif
!
!1.tsea
#ifdef MP
      call rmpgp2f(tsea,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(tsea,  tmps,lngrd)
      write(nunit) tmps
#endif
!2.smc
#ifdef MP
      call rmpgp2f(smc,igrd1p,jgrd1p,grid,igrd1,jgrd1,lsoil)
      if( mype.eq.master ) then
        call ful2sgl(grid,  tmpsl,lsoil*lngrd)
        write(nunit) tmpsl
      endif
#else
      call ful2sgl(smc,   tmpsl,lsoil*lngrd)
      write(nunit) tmpsl
#endif
!3.sheleg
#ifdef MP
      call rmpgp2f(sheleg,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        !tmps = tmps * 0.1 !mm->cm
        !tmps = tmps * 10 !cm->mm
        !tmps = tmps * 100 !m->mm
        write(nunit) tmps
      endif
#else
      call ful2sgl(sheleg,tmps,lngrd)
      !tmps = tmps * 0.1 !mm->cm
      !tmps = tmps * 10 !cm->mm
      !tmps = tmps * 100 !m->mm
      write(nunit) tmps
#endif
!4.stc
#ifdef MP
      call rmpgp2f(stc,igrd1p,jgrd1p,grid,igrd1,jgrd1,lsoil)
      if( mype.eq.master ) then
        call ful2sgl(grid,  tmpsl,lsoil*lngrd)
        write(nunit) tmpsl
      endif
#else
      call ful2sgl(stc,   tmpsl,lsoil*lngrd)
      write(nunit) tmpsl
#endif
!5.tg3
#ifdef MP
      call rmpgp2f(tg3,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(tg3,   tmps,lngrd)
      write(nunit) tmps
#endif
!6.zorl
#ifdef MP
      call rmpgp2f(zorl,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(zorl,  tmps,lngrd)
      write(nunit) tmps
#endif
!7.cv
#ifdef MP
      call rmpgp2f(cv,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(cv,    tmps,lngrd)
      write(nunit) tmps
#endif
!8.cvb
#ifdef MP
      call rmpgp2f(cvb,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(cvb,   tmps,lngrd)
      write(nunit) tmps
#endif
!9.cvt
#ifdef MP
      call rmpgp2f(cvt,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(cvt,   tmps,lngrd)
      write(nunit) tmps
#endif
!10.albedo
#ifdef MP
      call rmpgp2f(alvsf,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps4(1,1),lngrd)
      endif
      call rmpgp2f(alvwf,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps4(1,2),lngrd)
      endif
      call rmpgp2f(alnsf,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps4(1,3),lngrd)
      endif
      call rmpgp2f(alnwf,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps4(1,4),lngrd)
        write(nunit) tmps4
      endif
#else
      call ful2sgl(alvsf, tmps4(1,1),lngrd)
      call ful2sgl(alvwf, tmps4(1,2),lngrd)
      call ful2sgl(alnsf, tmps4(1,3),lngrd)
      call ful2sgl(alnwf, tmps4(1,4),lngrd)
      write(nunit) tmps4
#endif
!11.slmsk
#ifdef MP
      call rmpgp2f(slmsk,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(slmsk, tmps,lngrd)
      write(nunit) tmps
#endif
!12.vfrac
#ifdef MP
      call rmpgp2f(vfrac,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(vfrac, tmps,lngrd)
      write(nunit) tmps
#endif
!13.canopy
#ifdef MP
      call rmpgp2f(canopy,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(canopy,tmps,lngrd)
      write(nunit) tmps
#endif
!14.f10m
#ifdef MP
      call rmpgp2f(f10m,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(f10m,  tmps,lngrd)
      write(nunit) tmps
#endif
!15.vtype
#ifdef MP
      call rmpgp2f(vtype,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(vtype, tmps,lngrd)
      write(nunit) tmps
#endif
!16.stype
#ifdef MP
      call rmpgp2f(stype,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(stype, tmps,lngrd)
      write(nunit) tmps
#endif
!17.facswf
#ifdef MP
      call rmpgp2f(facsf,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps2(1,1),lngrd)
      endif
      call rmpgp2f(facwf,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps2(1,2),lngrd)
        write(nunit) tmps2
      endif
#else
      call ful2sgl(facsf, tmps2(1,1),lngrd)
      call ful2sgl(facwf, tmps2(1,2),lngrd)
      write(nunit) tmps2
#endif
!18.uustar
#ifdef MP
      call rmpgp2f(uustar,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(uustar,tmps,lngrd)
      write(nunit) tmps
#endif
!19.ffmm
#ifdef MP
      call rmpgp2f(ffmm,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(ffmm,  tmps,lngrd)
      write(nunit) tmps
#endif
!20.ffhh
#ifdef MP
      call rmpgp2f(ffhh,igrd1p,jgrd1p,spec,igrd1,jgrd1,1)
      if( mype.eq.master ) then
        call ful2sgl(spec,  tmps,lngrd)
        write(nunit) tmps
      endif
#else
      call ful2sgl(ffhh,  tmps,lngrd)
      write(nunit) tmps
#endif
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      close(nunit)
      print *,'rfixio write to unit=',nunit,' fhour,idate=',fhour,idate
#ifdef MP
      endif
#endif
!----end if(ioflag)----
      endif
!
#ifdef MP
      deallocate (spec)
      deallocate (spec2)
      deallocate (spec3)
      deallocate (grid)
#endif
      deallocate (tmps)
      deallocate (tmpsl)
      deallocate (tmps2)
      deallocate (tmps4)
!
      return
5000  print *,' error read in rfixio '
      call abort
      end
