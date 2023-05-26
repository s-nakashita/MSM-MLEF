        module physics_module
!
! module 
!
#include <define.h>
#ifdef RAS
      use module_ras , only : ras_init
#endif
!c
      integer igrd1s, jgrd1s, lngrds,igrd1,jgrd1
      integer num_p3d, num_p2d,levr,levh,levrp1,lsoil
      integer me,lonlens,latlens,ncpuss
      logical lsfwd,lscca,lsswr
      integer idate(4)
      real wrk,fhour,phour,fhswr,deltim
      real, allocatable :: xkt2(:,:),xkt2full(:,:)
      real, allocatable ::  phy_f3dv(:,:,:)
      real, allocatable ::  phy_f2dv(:,:)
      integer nsphys

      private idate,lsfwd,lscca,lsswr,me ,lonlens,latlens
      private ncpuss,deltim,igrd1,jgrd1
      private igrd1s, jgrd1s, lngrds,wrk,fhour,phour,fhswr
      private num_p3d,num_p2d,levr,levh,levrp1,phy_f3dv,phy_f2dv,lsoil
      private xkt2,xkt2full
      public nsphys
!cc
#ifdef RAS
      real rannum(200,5)
      logical, parameter :: flipv = .true.
      integer ncrnd
      public rannum,flipv, ncrnd
#endif
!ccccc
      integer, allocatable ::  nrnd(:)
      integer krsize, iseed
      real    dtlast, seed0
      logical first
      data first/.true./, dtlast/0.0/
      real dtp,dtf
      private iseed
      public krsize, first, nrnd, dtlast, seed0,dtp,dtf
      save    krsize, first, nrnd, dtlast, seed0

      contains
      
      subroutine physics_init(ime,ilonlens,ilatlens,ilngrds,             & 
     &   inum_p3d,inum_p2d,ilevr,ilevh,ilevrp1,incpuss,                  &
     &   iigrd1s, ijgrd1s,iigrd1,ijgrd1,ifhour,ifhswr,iphour,ilsoil,     &
     &   ilsfwd, ideltim, ilscca,ilsswr,                                 &
     &   osi,iidate,ophy_f3d,ophy_f2d)
!cc
      integer ime,ilonlens,ilatlens
      integer inum_p3d,ilevr,ilevh,ilevrp1,ilsoil
      integer iigrd1s, ijgrd1s,iigrd1,ijgrd1
      real ifhour,iphour,ifhswr,ideltim
      logical ilsfwd,ilscca,ilsswr
      integer iidate(4)
      real  osi(ilevrp1)
      real  ophy_f3d(iigrd1s,ijgrd1s,ilevr,inum_p3d)
      real  ophy_f2d(iigrd1s,ilevr,inum_p2d)
!      if(me.eq.0) print *,' rloopb start'
!.................................................................
!
      me=ime
      lonlens=ilonlens
      latlens=ilatlens
      lngrds=ilngrds
      num_p3d=inum_p3d
      num_p2d=inum_p2d
      levr=ilevr
      levh=ilevh
      levrp1=ilevrp1
      lsoil=ilsoil
      ncpuss=incpuss
      igrd1s=iigrd1s
      jgrd1s=ijgrd1s
      igrd1 =iigrd1 
      jgrd1 =ijgrd1 
      fhour=ifhour
      fhswr=ifhswr
      phour=iphour
      deltim=ideltim
      lsfwd=ilsfwd
      lscca=ilscca
      lsswr=ilsswr

!      print *,'in physics_init ',me,lonlens,latlens,lngrds,num_p3d,
!     &    num_p2d,levr,levh,levrp1,ncpuss,igrd1s,jgrd1s,fhour,
!     &    fhswr,phour,laswd,lscca,lsswr

      idate=iidate
      dtphys=3600.
      nsphys=max(int(2*deltim/dtphys+0.9999),1)
!      nsphys=1
      dtp=2*deltim/nsphys
      dtf=0.5*dtp
      if(lsfwd) dtf=dtp
!
      if (num_p3d .eq. 3 .and. dtp .ne. dtlast) then
!      initialization and/or constant evaluation for brad microphysics
        call micro_init(levr, igrd1s*jgrd1s, ophy_f3d(1,1,1,1),          &   
     &       ophy_f3d(1,1,1,2), ophy_f3d(1,1,1,3), dtp, fhour, me,       &
     &       first)
        dtlast = dtp
      endif
      if ( first ) then
        allocate(xkt2(igrd1s,jgrd1s),xkt2full(igrd1,jgrd1))
        allocate(phy_f3dv(igrd1s,levr,num_p3d),                          &
     &     phy_f2dv(igrd1s,num_p2d))

        call random_seed(size=krsize)
        if (me.eq.0) then
          print *,' krsize=',krsize
        endif
        allocate (nrnd(krsize))
!
        seed0 = idate(1) + idate(2) + idate(3) + idate(4)
        nrnd  = idate(1) + idate(3) * 24
        if ( me.eq.0 ) then
#ifdef IBMSP
        call random_seed(generator=2)
#else
        call random_seed
#endif
        call random_seed(put=nrnd)
        call random_number(wrk)
        endif
        call rmpbcastr(wrk,1)
        seed0 = seed0 + nint(wrk*1000.0)
        if (me .eq. 0) then
          print *,' seed0=',seed0,' idate=',idate,' wrk=',wrk
          if (num_p3d .eq. 3) print *,' using brad-microphysics'
          if (num_p3d .eq. 4) print *,' using zhao-microphysics'
        endif
        if (fhour .eq. 0.0) then
          do j=1,latlens
            do i=1,lonlens
              ophy_f2d(i,j,num_p2d) = 0.0
            enddo
          enddo
        endif
#ifdef RAS
        call ras_init(levr, osi, flipv, me , 1,  1, 1, 1, 1, 1, levr)
#endif
       endif
       first=.false.
         
      return
      end subroutine physics_init

      subroutine physics_prep(oozprd,oozdis,oozprdout,oozdisout,        &
     &   osolhr,oclstp)
                                       
      integer  lats18,lev46
      PARAMETER (lats18=18, lev46=46)

      real osolhr,oclstp
      
      real oozprd(igrd1s,jgrd1s,lev46),oozdis(igrd1s,jgrd1s,lev46)
      real oozprdout(lngrds,lev46),oozdisout(lngrds,lev46)

      osolhr=mod(phour+idate(1),24.)
!...  set switch for saving convective clouds
      if(lscca.and.lsswr) then
        oclstp=1100+min(fhswr,fhour,99.)  !initialize,accumulate,convert
      elseif(lscca) then
        oclstp=0100+min(fhswr,fhour,99.)  !accumulate,convert
      elseif(lsswr) then
        oclstp=1100                       !initialize,accumulate
      else
        oclstp=0100                       !accumulate
      endif
!
#ifdef RAS
      call setras( dtp, nsphys, fhour, rannum, dtf, seed0,              &
     &             krsize, nrnd, ncrnd, me, 1, 1)
#else
!     iseed = mod(100.0*sqrt(seed0+fhour*3600+100.0*me),1.0e9) + 1
      iseed = mod(100.0*sqrt(fhour*3600+100.0*me),1.0e9) + 1 + seed0
!c      iseed = mod(100.0*sqrt(fhour*3600),1.0e9) + 1 + seed0
      nrnd  = iseed
      if ( me.eq.0 ) then
#ifdef IBMSP
        call random_seed(generator=2)
#else
        call random_seed
#endif
        call random_seed(put=nrnd)
        call random_number(xkt2full)
      endif 
      call rmpgf2p(xkt2full,igrd1,jgrd1,xkt2,igrd1s,jgrd1s,1)
#endif
!ccj
!
! doing ozone i/o and latitudinal interpolation to local gauss lats
!c
      call ozinterpol(idate,fhour)
!
      do 540 k = 1,lev46
      do 540 lat = 1,latlens
        jlat = (lat-1)*igrd1s
        do 550 i=1,lonlens
          ijlat = i + jlat
          oozprd(i,lat,k) = oozprdout(ijlat,k)
          oozdis(i,lat,k) = oozdisout(ijlat,k)
 550    continue
 540  continue
!ccc

      return
      end subroutine physics_prep
 

      subroutine physics_run(lan,lat,                                   &
     &  plamgrs,pphigrs,ugrs,vgrs,pgr,tgrh,qgrs,xgrs,                   &
     &  anlt,anlq,anlu,anlv,                                            &
#ifdef NONHYD
     &  pgrs,wgrs,tgrs,                                                 &
     &  gp0,gw0,gt0,                                                    &
#endif
#ifdef RKN
     &    psexp,radsl,                                                  &
     &    dlwsf1,qss,                                                   &
     &    gflx,cd,                                                      &
     &    cdq,evap,                                                     &
     &    hflx,drain,                                                   &
     &    runof,cld1d,                                                  &
     &    gamt,gamq,                                                    &
     &    dqsfc1,dtsfc1,                                                &
     &    dusfc1,dvsfc1,                                                &
     &    dusfcg,dvsfcg,                                                &
     &    rainc,rainl,                                                  &
     &    hsw,vvel,                                                     &
     &    stsoil,smsoil,                                                &
#endif
     &  phy_f3d,phy_f2d)

      integer lan,lat
      dimension  plamgrs(igrd1s)
      dimension  pphigrs(igrd1s)
      dimension     ugrs(igrd1s,levr)
      dimension     vgrs(igrd1s,levr)
      dimension      pgr(igrd1s)
      dimension     tgrh(igrd1s,levr)
      dimension     tgrs(igrd1s,levr)
      dimension     qgrs(igrd1s,levh)
      dimension     xgrs(igrd1s,levr)
      dimension      anlt(igrd1s,levr)
      dimension      anlq(igrd1s,levh)
      dimension      anlu(igrd1s,levr)
      dimension      anlv(igrd1s,levr)

#ifdef NONHYD
      real pgrs(igrd1s,levr),wgrs(igrd1s,levr+1)
      real gp0(igrd1s,levr),gw0(igrd1s,levr+1)
      real gt0(igrd1s,levr),gtotmp(igrd1s,levr)
#endif
      real phy_f3d(igrd1s,jgrd1s,levr,num_p3d)
      real phy_f2d(igrd1s,jgrd1s,num_p2d)
     
#ifdef RKN
      dimension  psexp(igrd1s),radsl(igrd1s),                           &
     & dlwsf1(igrd1s),qss(igrd1s),                                      &
     & gflx(igrd1s),cd(igrd1s),                                         &
     & cdq(igrd1s),evap(igrd1s),                                        &
     & hflx(igrd1s),drain(igrd1s),                                      &
     & runof(igrd1s),cld1d(igrd1s),                                     &
     & gamt(igrd1s),gamq(igrd1s),                                       &
     & dqsfc1(igrd1s),dtsfc1(igrd1s),                                   &
     & dusfc1(igrd1s),dvsfc1(igrd1s),                                   &
     & dusfcg(igrd1s),dvsfcg(igrd1s),                                   &
     & rainc(igrd1s),rainl(igrd1s),                                     &
     & hsw(igrd1s,levr),vvel(igrd1s,levr),                              &
     & stsoil(igrd1s,lsoil),smsoil(igrd1s,lsoil)
#endif
!cjune
                                                                               
      do n=1,num_p3d
        do k=1,levr
        do i=1,lonlens
          phy_f3dv(i,k,n) = phy_f3d(i,lat,k,n)
        enddo
        enddo
      enddo
      do n=1,num_p2d
      do i=1,lonlens
        phy_f2dv(i,n)     = phy_f2d(i,lat,n)
      enddo
      enddo
!
#ifdef NONHYD
       gt0=tgrh
#else
       tgrs=tgrh
#endif

       call rbphys(dtp,dtf,xkt2(1,lat),                                 &
#ifndef NONHYD
     &  plamgrs,pphigrs,                                                &
#endif
     &  ugrs,vgrs,pgr,tgrs,qgrs,                                        &
#ifndef NONHYD
     &  xgrs,                                                           &
#endif
     &  anlt,anlq,anlu,anlv,                                            &
#ifdef NONHYD
     &    pgrs,wgrs,                                                    &
     &    gp0,gw0,tgrh,gt0,                                             &
#endif
#ifdef RAS
     & rannum(1,nsphys),flipv, ncrnd,                                   &
#endif
#ifdef RKN
     & psexp,radsl,                                                     &
     & dlwsf1,qss,                                                      &
     & gflx,cd,								&
     & cdq,evap,							&
     & hflx,drain,							&
     & runof,cld1d,							&
     & gamt,gamq,							&
     & dqsfc1,dtsfc1,							&
     & dusfc1,dvsfc1,							&
     & dusfcg,dvsfcg,							&
     & rainc,rainl,							&
     & hsw,vvel,							&
     & stsoil,smsoil,							&
#endif
     & phy_f3dv,phy_f2dv,me,						&
     & lonlens,igrd1s,							&
     & lat)
!
#ifdef NONHYD
!gto is the nonhyd temp, anlt is hyd tmp,tgrh does not need any more
       gtotmp=gt0
       gt0=anlt
       anlt=gtotmp
#endif
      do n=1,num_p3d
        do k=1,levr
        do i=1,lonlens
          phy_f3d(i,lat,k,n) = phy_f3dv(i,k,n)
        enddo
        enddo
      enddo
      do n=1,num_p2d
      do i=1,lonlens
        phy_f2d(i,lat,n)     = phy_f2dv(i,n)
      enddo
      enddo
!c
!       print *,'end of rbphys, mype=',me
      return
      end subroutine physics_run

      end module physics_module
