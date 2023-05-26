      module radiation_module
!ccjun
      use machine , only : kind_phys,kind_rad
      use funcphys , only : fpkap
      use physcons, rocp => con_rocp,fv => con_fvirt                     &
     &,             grav => con_g, cp => con_cp, rd => con_rd            &
     &,             rv => con_rv, hvap => con_hvap,  hfus => con_hfus    &
     &,             rerth => con_rerth                                   &
#ifdef NONHYD
     &,             ccv => con_cv                                        &
#endif
     &,             pi => con_pi
      use rnddta, only : nblw,nblx,nbly,nblm
!
      use module_radsw_parameters
      use module_radlw_main,  only : rlwinit
!     use module_radlw_rrtm,  only : rlwinit
!cc
#include <define.h>
!ccj
!!
      integer idat(8),jdat(8),iday,imon,midmon,k1oz, k2oz, midm,         &    
     &        midp,id,istrat,icfc,icwp,iflip,ier1,kmon,iovr
      save istrat
!!
      integer iunco2
      integer iswsrc(5)
      integer kk,me,lonlens,lonstrs,latlens,levr,levh,levrp1,ncpuss
      integer l,lp1,lp1v,igrd1s,jgrd1s,lnwavs,ncpus,ntotal
      integer lat1s,lat2s,latts
      logical lsswr,lslwr,lssav
      integer lw_type,ntoz,ncld,nvect
      integer num_p3d,nmtvr
      integer idate(4)
!!
      integer mcld,nseal,nbin,nlon,nlat
!
      real (kind=kind_rad) work1, qmin
      parameter (qmin=1.0e-10)
      logical first
      data first/.true./
      data  iswsrc/ 1, 1, 1, 1, 1/
!                   a o2 co2 wv o3
!                   a o2 co2 wv o3
!....  iovr controls cloud overlap for rrtm
!                     (iovr = 0 or 1 for random or random-max)
      data  iovr/1/
!
      integer     days(13)
      data        days/31,28,31,30,31,30,31,31,30,31,30,31,30/
      real (kind=kind_rad) rinc(5),facoz,fhour,fhswr,fhlwr,deltim
      real (kind=kind_rad) hsigma,daysec,rco2
      real (kind=kind_rad) solc,rsin1,rcos1,rcos2
      real (kind=kind_rad) phour
      real (kind=kind_rad) raddt,dtlw,rrs2,tsmin,tsmax
      real (kind=kind_rad) shmin,shmax
!
      logical change,lprnt
       
      private idat,jdat,iday,imon,midmon,k1oz, k2oz, midm
      private midp,id,icfc,icwp,iflip,ier1,kmon,iovr
      private iunco2,iswsrc
      private days,rinc,facoz,hsigma,daysec,rco2
      private raddt,dtlw,rrs2,solc,rsin1,rcos1,rcos2
      private fhour,phour,fhswr,fhlwr,deltim
      private lsswr,lslwr,lssav,change,lprnt
      private tsmin,tsmax,shmin,shmax
!
      private kk,me
      private lonlens,lonstrs,latlens,levr,levh,levrp1,ncpus,ncpuss
      private igrd1s,jgrd1s,lnwavs,num_p3d
      private lw_type,ntoz,ncld,nvect,ntotal,nmtvr
      private l,lp1,lp1v
      private idate
!cc
      parameter (mcld=4,nseal=2,nbin=100,nlon=2,nlat=4)
!bl    the 4 cld domains are bl,l,m,h (for 3 it was l,m,h)
!o3
!c      parameter (loz=17)
!c      common /sbuv/ psnasa(loz),o3nasa(37,loz)
!c      dimension pstr(loz)
!o3
!...
       real, allocatable :: si(:), sl(:)
       real, allocatable :: rlat(:,:), rlon(:,:)
!
       real, allocatable :: coszdg(:,:)
       real, allocatable :: fluxr1(:,:), slk(:),tser(:,:)
#ifndef NONHYD
       real, allocatable :: ze(:,:)
#endif
       real, allocatable :: psexp(:), vvel(:,:),hprimer(:,:),stcr(:,:)
       real, allocatable :: tsermin(:),tsermax(:),                       &                      
     &     shelegmax(:),shelegmin(:)
!cc
       real, allocatable :: prsi(:,:), prsl(:,:),                        &
     &    prslk(:,:),psk(:)
       real(kind=kind_rad), allocatable :: cmixg(:,:,:), denng(:,:,:)
       integer, allocatable :: idxcg(:,:,:), kprfg(:,:)
       integer, allocatable :: idm(:,:)
       real (kind=kind_rad), allocatable :: haer(:,:),taur(:,:)
!
       real (kind=kind_rad), allocatable :: sgtmp(:,:),                  &
     &                     co21d(:,:),  co22d(:,:,:),                    &
     &                     co21d3(:,:), co21d7(:,:),                     &
     &                     source(:,:), dsrce(:,:)
!cc
!....
!tune
!...  array added for rh-cl calculation
!     indices for lon,lat,cld type(l,m,h), land/sea respectively
!     nlon=1-2, for eastern and western hemispheres
!     nlat=1-4, for 60n-30n,30n-equ,equ-30s,30s-60s
!     land/sea=1-2 for land(and seaice),sea
      dimension rhcl (nbin,nlon,nlat,mcld,nseal)
!
       private mcld,nseal,nbin,nlon,nlat
       private fluxr1,ze,slk,psexp,vvel,hprimer,stcr
       private tserf,shelegf,rhcl,coszdg
       private prsi,prsl,prslk,psk,cmixg,denng,idxcg,kprfg
       private idm,haer,taur,sgtmp,co21d,co22d,co21d3,dsrce
!
       private si,sl,rlat,rlon
       save sgtmp,co21d,co22d, co21d3,co21d7, first, source,dsrce        &             
     &,    icfc, icwp, iswsrc, hsigma, daysec, rco2, iflip               &
     &,    k1oz, k2oz, facoz, kmon, iovr                                 &
     &,    cmixg, denng, idxcg, kprfg, haer, taur, idm, days

       contains 
       
       subroutine radiation_init(ime,ilonlens,ilonstrs,ilatlens,         &
     &   ilevr,ilevrp1,                                                  &
     &   ilevh,incpuss,ilat1s,ilat2s,ilatts,il,ilp1,ilp1v,               &
     &   iigrd1s,ijgrd1s,incpu,intotal,                                  &
     &   ilnwavs,incld,invect,inum_p3d,inmtvr,                           &
     &   ifhour,iphour,ifhswr,ifhlwr,ideltim,                            &
     &   ilsswr,ilslwr,ilssav,ilw_type,intoz,                            &
     &   osi,osl,iidate,rrlat,rrlon,                                     &
     &   sinlar,coslar,itimsw,itimlw)   
!
!...    istrat = 0, calculate crh in cldjms, = 1 use tuned rhcld
!
       integer ime,ilonlens,ilonstrs,ilatlens,ilevr,ilevrp1,             &
     &    ilevh,il,ilp1,ilp1v
       integer iigrd1s,ijgrd1s,ilnwavs,incld,intotal,imtvr
       integer invect,ilat1s,ilat2s,ilatts
       real ifhour,iphour,ifhswr,ifhlwr,ideltim
       logical ilsswr,ilslwr,ilssav
       integer ilw_type,intoz

       integer iidate(4)
       real osi(ilevrp1)
       real osl(ilevr)
       real rrlat(iigrd1s,ijgrd1s)
       real rrlon(iigrd1s,ijgrd1s)
       real sinlar(iigrd1s,ijgrd1s),coslar(iigrd1s,ijgrd1s)
!c
!c
       me=ime
!      if (me .eq. 0) then
!       print *,' ------ enter radiation_init ------ ,me=',me
!      endif
!c
       ncpus=ncpu
       lonlens=ilonlens
       lonstrs=ilonstrs
       latlens=ilatlens
       igrd1s=iigrd1s
       jgrd1s=ijgrd1s
       lnwavs=ilnwavs
       levr=ilevr
       levrp1=ilevrp1
       levh=ilevh
       ncpuss=incpuss
       lat1s=ilat1s
       lat2s=ilat2s
       latts=ilatts
       l=il
       lp1=ilp1
       lp1v=ilp1v
       ncld=incld
       nvect=invect
       num_p3d=inum_p3d
       ntotal=intotal
       nmtvr=inmtvr

       lsswr=ilsswr
       lslwr=ilslwr
       lssav=ilssav
       lw_type=ilw_type
       ntoz=intoz

       fhour=ifhour
       fhswr=ifhswr
       fhlwr=ifhlwr
       phour=iphour
       deltim=ideltim

       idate=iidate
        if (lw_type .eq. 0) then
          allocate (sgtmp(lp1,2),    co21d(l,6), co22d(lp1,lp1,6),       &   
     &              co21d3(lp1,6),   co21d7(lp1,6),                      &
     &              source(28,nbly), dsrce(28,nbly))
        endif
!
! ******************************************************
!
      idat = 0
      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(5) = idate(1)
      rinc = 0.
      rinc(2) = fhour
      call w3movdat(rinc, idat, jdat)
!
      if (ntoz .eq. 0) then                ! climatological ozone!
        iday   = jdat(3)
        imon   = jdat(2)
        midmon = days(imon)/2 + 1
        change = first .or.                                              &  
     &          ( (iday .eq. midmon) .and. (jdat(5).eq.0) )
!
        if (change) then
            if (iday .lt. midmon) then
               k1oz = mod(imon+10,12) + 1
               midm = days(k1oz)/2 + 1
               k2oz = imon
               midp = days(k1oz) + midmon
            else
               k1oz = imon
               midm = midmon
               k2oz = mod(imon,12) + 1
               midp = days(k2oz)/2 + 1 + days(k1oz)
            endif
        endif
!
        if (iday .lt. midmon) then
           id = iday + days(k1oz)
        else
           id = iday
        endif
        facoz = real (id-midm) / real (midp-midm)
      endif
!
!....... set up initial values ..........
      if(first) then
!ccj
      allocate(prsi(igrd1s,levr+1), prsl(igrd1s,levr),                   & 
     &     prslk(igrd1s,levr), psk(igrd1s))
      allocate(psexp(igrd1s),vvel(igrd1s,levr),                          &
     &     hprimer(igrd1s,jgrd1s),stcr(igrd1s,jgrd1s))
      allocate(fluxr1(igrd1s,27),slk(levr),                              &
     &    tser(igrd1s,jgrd1s))
#ifndef NONHYD
      allocate(ze(lnwavs,levr))
#endif

      allocate(coszdg(igrd1s,jgrd1s))
!      allocate(mtopaf(igrd1s,3,ncpuss),mbotaf(igrd1s,3,ncpuss))
      allocate(si(levrp1),sl(levr))
      allocate(rlat(igrd1s,jgrd1s),rlon(igrd1s,jgrd1s))

      allocate(tsermax(jgrd1s),tsermin(jgrd1s))
      allocate(shelegmax(jgrd1s),shelegmin(jgrd1s))

       si=osi
       sl=osl
       rlat=rrlat
       rlon=rrlon

        if (nsrc .ne. 5) stop
        hsigma = 5.673e-5
        daysec = 1.0 / 86400.0
!
        istrat = 1
        icfc   = 1           ! close to operational version
!       icfc   = 0
        icwp   = 0
        if (ncld .gt. 0) icwp = 1
        iflip  = 1         ! levels run bottom to top!
!c
        if (me .eq. 0) then
           print *,' icwp=',icwp,' icfc=',icfc
           print *,' iswsrc=',iswsrc
           if (iswsrc(1) .eq. 0)                                         &
     &       print *,' no aerosol effect included in sw'
           if (iswsrc(2) .eq. 0)                                         &
     &        print *,' no oxygen effect included in sw'
           if (iswsrc(3) .eq. 0)                                         &
     &        print *,' no co2 effect included in sw'
           if (iswsrc(4) .eq. 0)                                         &
     &        print *,' no h2o effect included in sw'
           if (iswsrc(5) .eq. 0)                                         &
     &        print *,' no o3 effect included in sw'
        endif
!c
!
!===> ... atmosperic carbon dioxide concentration is now read by conrad
!         but it defaults to 330 ppm for backward compatibility.
!
        rco2 = 3.3e-4
        if (lw_type .eq. 0) then
          if (me .eq. 0) print *,                                        &
     &                  ' using gfdl code for longwave radiation'
          iunco2 = 15
          call hconst
          call conrad(iunco2,rco2,  l,     lp1,    lp1v,   nbly,   nblw  &
     &,               sgtmp, co21d, co22d, co21d3, co21d7, source, dsrce & 
     &,               me)
          if (rco2 .le. 1.0e-4) rco2 = 3.3e-4
        endif
        if (lw_type .eq. 1) then
          if (me .eq. 0) print *,' using rrtm for longwave radiation'
          rco2 = 3.5e-4
!         call rrtminit      !.... call the aer initializing routine
          call rlwinit       !.... call the aer initializing routine
        endif
!
        call gcljms(si,levr)
!
!       get cld-rh relations in tabular form for day 1
        if (istrat .eq. 1) call crhtab(rhcl, ier1,me)
        if(me .eq. 0) print *,' ier1=',ier1,' istrat=',istrat
        if (ier1 .lt. 0) istrat = 0
!mic$ do all
!mic$1 shared(rlat,sinlar,coslar)
!mic$1 private(i)
        do  j=1,latlens
        do  i=1,lonlens
          sinlar(i,j)=sin(rlat(i,j))
          coslar(i,j)=sqrt(1.e0 - sinlar(i,j)*sinlar(i,j))
        enddo
        enddo
!
        allocate (taur(l,nbd))
        call ralsct(si,l,nbd,taur)         ! rayleigh scattering!
        if (iswsrc(1) .gt. 0) then         ! initialize aerosols!
          allocate (haer(ndm,nae),idm(l,nae))
          call gaeros(si,sl,l,lp1,nae,nbd,ndm,haer,idm)
          kmon   = 0
          allocate (cmixg(nxc,imxae,jmxae), denng(ndn,imxae,jmxae)        &
     &,             idxcg(nxc,imxae,jmxae), kprfg(imxae,jmxae))
        endif
!
        first = .false.
        if(me.eq.0) print *,'- finish initial setup in radiation_init -'
      endif
!....        
      itimsw = 0
      itimlw = 0
      if(lsswr) itimsw = 1
      if(lslwr) itimlw = 1
      if(itimsw.eq.0 .and. itimlw.eq.0) return
!....
         if ( me.eq.0) print *,'in radiation_init end, me=',me

       return
       end subroutine radiation_init
!
! **************************************
!
       subroutine radiation_prep(sdec,cdec,slag,q,uu,te,vv,del,rq,       &
     &    tsea,sheleg,coszer,hprime,stc,sinlar,coslar)
!
       real solhr
       real q(lnwavs), uu(lnwavs,levr),te(lnwavs,levr)
       real vv(lnwavs,levr),del(levr), rq(lnwavs,levr,ntotal)

       real tsea(igrd1s,jgrd1s),hprime(igrd1s,jgrd1s),                   &
     &      stc(igrd1s,jgrd1s),sheleg(igrd1s,jgrd1s),                    &  
     &      coszer(igrd1s,jgrd1s)
       real sinlar(igrd1s,igrd1s),coslar(igrd1s,igrd1s)

!....... start to prepare for computing radiation ..........
      if(me.eq.0)                                                        &  
     &   print *,' ----- check perturbation values inside of rloopr'
!
      call rmsgtr( q, uu, te, vv,del,rq)
!cc
!
!     read in monthly aerosol data
!
      if (iswsrc(1) .gt. 0) then
        if (kmon .ne. jdat(2)) then
          kmon = jdat(2)
          if (me.eq.0)                                                   &
     &    print *,' reading monthly aerosol data for month = ', kmon     
          call getaer(101,kprfg,idxcg,cmixg,denng                        &  
     &  ,             nxc, ndn, imxae, jmxae, kmon)
        endif
      endif
!
! ***************************************************************
!     astronomy for radiation calculation.
!     -----------------------------------
      call astronomy(me,igrd1s,jgrd1s,                                   & 
     &  sinlar,coslar,rlon,                                              & 
     &  fhswr,idate,phour,lsswr,lslwr,                                   &
     &  solc,rsin1,rcos1,rcos2,slag,sdec,cdec,coszer,coszdg)
!cc
      jo3=2
!
!     transform for radiation calculation.
!     -----------------------------------
!
!ccj
      do k=1,levr
         slk(k)=sl(k)**rocp
      enddo
!cc
      do j=1,latlens
      do i=1,lonlens
        tser(i,j)=tsea(i,j)
      enddo
      enddo
      raddt = 3600. * fhswr
      dtlw  = 3600. * fhlwr
!
      do j=1,latlens
      do i=1,lonlens
         hprimer(i,j)=hprime(i,j)
      enddo
      enddo
      do j=1,latlens
      do i=1,lonlens
         stcr(i,j)=stc(i,j)
      enddo
      enddo
!
      tsmin=tser(1,1)
      tsmax=tser(1,1)
      shmin=sheleg(1,1)
      shmax=sheleg(1,1)
      rrs2=1.0

       return
       end subroutine radiation_prep

       subroutine radiation_run(lat,lan,lotsyn,                          &  
     &   kpy,kvv,kpx,kuu,kdd,kpp,ktt,kqq,                                &
#ifdef NONHYD
     &   wgrs,                                                           &
#endif
     &   syn,phy_f3d,fluxr,slmsk,                                        &
     &   hprime,stc,sheleg,zorl,cv,cvt,cvb,                              &
     &   alvsf,alnsf,alvwf,alnwf,facsf,facwf,                            &
     &   swh,hlw,sfcnsw,sfcdlw,coszer,tsflw,cvavg, del                   &
#ifdef RKN
     &   ,flwupf,fswupf,fswdnf,                                          &
     &   sswupf,sswdnf,                                                  &
     &   slwupf,slwdnf,                                                  &
     &   cldsaf,mtopaf,mbotaf                                            &
#endif
     &    )
!cc
        integer lat,lan,lotsyn
        real del(levr)
        real syn(igrd1s,lotsyn,ncpuss),                                  &
     &    phy_f3d(igrd1s,jgrd1s,levr,num_p3d),                           &
     &    fluxr(igrd1s,jgrd1s,27),slmsk(igrd1s,jgrd1s),                  &
     &    hprime(igrd1s,jgrd1s),                                         &
     &    stc(igrd1s,jgrd1s),                                            &
     &    sheleg(igrd1s,jgrd1s), zorl(igrd1s,jgrd1s),                    &
     &    cv(igrd1s,jgrd1s),  cvt(igrd1s,jgrd1s),                        &
     &    cvb(igrd1s,jgrd1s),                                            &
     &    cvavg(igrd1s,jgrd1s),                                          &
     &    alvsf(igrd1s,jgrd1s),    alvwf(igrd1s,jgrd1s),                 &
     &    alnsf(igrd1s,jgrd1s),    alnwf(igrd1s,jgrd1s),                 &
     &    facsf(igrd1s,jgrd1s),    facwf(igrd1s,jgrd1s),                 &
     &    swh(igrd1s,levr,jgrd1s),hlw(igrd1s,levr,jgrd1s),               &
     &    sfcnsw(igrd1s,jgrd1s),   sfcdlw(igrd1s,jgrd1s),                &
     &    coszer(igrd1s,jgrd1s),   tsflw(igrd1s,jgrd1s)                  &
#ifdef RKN
     &   , flwupf(igrd1s),fswupf(igrd1s),fswdnf(igrd1s),                 &
     &   sswupf(igrd1s),sswdnf(igrd1s),                                  &
     &   slwupf(igrd1s),slwdnf(igrd1s),                                  &
     &   cldsaf(igrd1s,5)                 
         integer mtopaf(igrd1s,3),mbotaf(igrd1s,3)                       &
#endif
     &   ,ttmptmp !end arrays
#ifdef NONHYD
       real wgrs(igrd1s,levr+1)
#endif

            lprnt = .false.
!         if ( me.eq.0) print *,'in radiation_run, me=',me
!ccj
        do j=1,lonlens
         psexp(j)= exp(syn(j,kpp,lan))
        enddo
!
        if (ncld .gt. 0 .and. num_p3d .eq. 3) then
          kk=1+ntoz
          do lev=1,levr
          do i=1,lonlens
           syn(i,kqq-1+kk*levr+lev,lan)=syn(i,kqq-1+kk*levr+lev,lan)*    & 
     &              (phy_f3d(i,lat,lev,1) + (1.0-phy_f3d(i,lat,lev,1))   &
     &                          * (1.0-phy_f3d(i,lat,lev,2)))
          enddo
          enddo
        endif
!ccj
        do k=1,27
        do i=1,lonlens
          fluxr1(i,k)=fluxr(i,lat,k)
        enddo
       enddo
!cc
!....
!    convert virt. temp to thermodynamic temp.
!.......................................................
        do  k=1,levr
         do j=1,lonlens
!          if(syn(j,kqq-1+k,lan).le.0.) syn(j,kqq-1+k,lan)=qmin
!          work1 = 1.0 + fv*syn(j,kqq-1+k,lan)
          work1 = 1.0 + fv*max(syn(j,kqq-1+k,lan),qmin)
          syn(j,ktt-1+k,lan)=syn(j,ktt-1+k,lan) / work1
        enddo
       enddo
!
        do i=1,lonlens
          psk(i)  = fpkap(psexp(i)*1000.0)
          do k=1,levr
            prsl(i,k)     = psexp(i)       * sl(k)
            prsi(i,k)     = psexp(i)       * si(k)
            prslk(i,k)    = psk(i)         * slk(k)
          enddo
          prsi(i,levr+1) = psexp(i) * si(levr+1)
        enddo
!c
#ifdef NONHYD
        do k=1,levr
        do j=1,lonlens
          wmean=-0.5*(wgrs(j,k)+wgrs(j,k+1))
          vvel(j,k)=wmean*prsl(j,k)*grav/(rd*syn(j,ktt-1+k,lan))
        enddo
        enddo
#else
        call omegast3(lonlens,igrd1s,levr,                               & 
     &                syn(1,kpy,lan),syn(1,kpx,lan),                     &
     &                syn(1,kuu,lan),syn(1,kvv,lan),syn(1,kdd,lan),      &
     &                del,rrs2,vvel,psexp,sl)
#endif
!
!        if ( me.eq.0) then
!          print *,'in radiat,bf grrad,lonlens=',lonlens,'igrd1s=',       &
!     &    igrd1s,'levr=',levr,'del=',del,'rrs2=',rrs2,                   &
!     &    'psexp=',psexp,'sl=',sl
!          print *,'in radiat,syntt,me=',mype,(syn(i,ktt,lan),i=1,5)
!          print *,'in radiat,synqq,me=',mype,(syn(i,kqq,lan),i=1,5)
!          print *,'in radiat,vvel,me=',mype,(vvel(i,1),i=1,5)
!        endif
        call grrad(lonlens,igrd1s,levr,                                  &
     &               ntotal,ntoz,ncld,                                   &  
     &               icfc,icwp,iflip,iswsrc,lw_type,iovr,                &
     &               syn(1,ktt,lan),syn(1,kqq,lan),                      &
     &               vvel,                                               &
     &               slmsk(1,lat),coszer(1,lat),                         &
     &               rlon(1,lat),rlat(1,lat),                            &
     &               tser(1,lat),sheleg(1,lat),                          &
     &               zorl(1,lat),hprimer(1,lat),                         &
     &               stcr(1,lat),                                        &
     &               cv(1,lat),cvt(1,lat),cvb(1,lat),                    &
     &               rhcl, istrat,rsin1,rcos1,rcos2,                     &
     &               alvsf(1,lat),alnsf(1,lat),alvwf(1,lat),             &
     &               alnwf(1,lat),facsf(1,lat),facwf(1,lat),             &
     &               solc,lsswr,lslwr,                                   &
     &               swh(1,1,lat),hlw(1,1,lat),                          &
     &               sfcnsw(1,lat),sfcdlw(1,lat),                        &
     &               tsflw(1,lat),                                       &
     &               prsi,prsl,prslk,lssav,raddt,dtlw,                   &
     &               coszdg(1,lat), fluxr1(1,1),                         &
     &               me,k1oz,k2oz,facoz,                                 &
     &               hsigma, daysec, rco2,                               &
     &               l,lp1,lp1v,llp1,nsrc,nbd,nvb,nae,nbly,              &
     &               ndm, nvect, sgtmp,co21d,co22d, co21d3,co21d7,       &
     &               source,dsrce,                                       &
     &               haer,  idm, taur,                                   &
     &               kprfg, idxcg, cmixg, denng,                         &
     &               imxae, jmxae, nxc, ndn,                             &
#ifdef RKN
     &               flwupf,fswupf,fswdnf,                               &
     &               sswupf,sswdnf,                                      &
     &               slwupf,slwdnf,                                      &
     &               cldsaf,mtopaf,mbotaf,                               &
#endif
     &               lprnt)
!!
!ccj
        do k=1,27
        do i=1,lonlens
          fluxr(i,lat,k)=fluxr1(i,k)
        enddo
        enddo
        do i=1,lonlens
          hprime(i,lat)=hprimer(i,lat)
        enddo
        do i=1,lonlens
          stc(i,lat)=stcr(i,lat)
        enddo
!cc
!.......................................................
!      enddo
!
!#ifdef RKN
!c
!       if(npoint.gt.0) then
!#ifdef MP
!!         lones=lonlen(mype)
!!         lon1s=lonstr(mype)
!         lones=lonlens
!         lon1s=lonstrs
!         lon2s=lon1s-1+lones
!#else
!         lon1=1
!         lon2=lonlens
!#endif
!         latt1=latts+lat1s
!         latt2=latts+lat2s
!c
!#ifdef MP
!         call rkenputr(igrd1s,ncpuss,lon1s,lon2s,
!     &        lones,
!#else
!         call rkenputr(igrd1s,ncpuss,lon1,lon2,
!#endif
!     &        latt1,latt2,sdec,
!     &        slmsk(1,lat1s),tser(1,lat1s),
!     &        flwupf(1,1),fswupf(1,1),fswdnf(1,1),
!     &        sswupf(1,1),sswdnf(1,1),slwupf(1,1),slwdnf(1,1),
!     &        cldsaf(1,1,1),mtopaf(1,1,1),mbotaf(1,1,1),
!     &        itimsw,itimlw)
!
!      endif
!c
!#endif
!.......................................................
       tsermax(lan)=tser(1,lan)
       do j=1,lonlens
         tsermax(lan)=max(tsermax(lan),tser(j,lan))
       enddo
       tsermin(lan)=tser(1,lan)
       do j=1,lonlens
         tsermin(lan)=min(tsermin(lan),tser(j,lan))
       enddo

       shelegmax(lan)=sheleg(1,lan)
       do j=1,lonlens
         shelegmax(lan)=max(shelegmax(lan),sheleg(j,lan))
       enddo
       shelegmin(lan)=sheleg(1,lan)
       do j=1,lonlens
         shelegmin(lan)=min(shelegmin(lan),sheleg(j,lan))
       enddo
!
      if(lssav) then
        do  i=1,lonlens
          cvavg(i,lan) =  cvavg(i,lan) + raddt * cv(i,lan)
        enddo
      endif
!....
!      print *,' after radiation_run,me=',me
!....
       return
       end subroutine radiation_run

      end module radiation_module
