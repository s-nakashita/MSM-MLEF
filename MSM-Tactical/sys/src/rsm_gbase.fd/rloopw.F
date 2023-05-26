      subroutine rloopw
!
! loop to get hydrostatic vertical velocity
!
!.....
!.....fold north y and south y 
!.....use perturbation method by substraction of full field forcing
!     by large scale forcing.
!.....
      use machine , only : kind_phys
      use funcphys , only : fpkap
      use physcons, rocp => con_rocp, cp => con_cp, rd => con_rd
!cc
#include <paramodel.h>
#ifdef NONHYD
#include <ncpu.h>
#include <rscomcpu.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define LNWAVS lnwavp
#define LNGRDS lngrdp
#define LLWAVS llwavp
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define IWAV1S iwav1p
#define LEVRS levrp
#define LEVHS levhp
#else
#define LNWAVS lnwav
#define LNGRDS lngrd
#define LLWAVS lnwav
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define IWAV1S iwav1
#define LEVRS levr
#define LEVHS levh
#endif
!
#include <rscomfi.h>
#include <rscomspc.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscompln.h>
#include <rscomrad.h>
#include <rscomloc.h>
!
#include <rscommap.h>
#include <rscomltb.h>
#include <rscomgpd.h>
#include <rscombgt.h>
!
#include <rscomoz.h>
!....
!ifdef AFA
!     parameter(ncpus=ncpu)
!endif
      parameter(numcs = 1 + levr*4 , numcs2 = numcs * 2 )
      parameter(numcos = 1 + levr, numcos2 = numcos * 2 )
!                 cos             sin          in x direction
      parameter( kpy=1        , kpx=numcs+kpy,                           &  
     &           kvv=1    +kpy, kuu=numcs+kvv,                           &
     &           kty=levr+kvv, ktx=numcs+kty,                            &
     &           kvy=levr+kty, kvx=numcs+kvy,                            &
     &           kux=levr+kvy, kuy=numcs+kux )
      parameter( kpp=numcs2+1,                                           &
     &           ktt=1    +kpp,                                          &
     &           kww=levr+ktt,                                           &
     &           kwh=levr+kww+1 )
      parameter( numbgd=1+levr*3,                                        &
     &           kppb=1,                                                 &
     &           kuub=kppb+1,                                            &
     &           kvvb=kuub+levr,                                         &
     &           kttb=kvvb+levr,                                         &
     &           kpxb=numbgd+1,                                          &
     &           kuxb=kpxb+1,                                            &
     &           kvxb=kuxb+levr,                                         &
     &           ktxb=kvxb+levr,                                         &
     &           kpyb=2*numbgd+1,                                        &
     &           kuyb=kpyb+1,                                            &
     &           kvyb=kuyb+levr,                                         &
     &           ktyb=kvyb+levr )
#ifdef MP
      parameter(numcsp = 1 + levrp*4 , numcs2p = numcsp * 2 )
      parameter(numcosp = 1 + levrp, numcos2p = numcosp * 2 )
!                 cos             sin          in x direction
      parameter( kpyp=1         , kpxp=numcsp+kpyp,                      &
     &           kvvp=1    +kpyp, kuup=numcsp+kvvp,                      &
     &           ktyp=levrp+kvvp, ktxp=numcsp+ktyp,                      &
     &           kvyp=levrp+ktyp, kvxp=numcsp+kvyp,                      &
     &           kuxp=levrp+kvyp, kuyp=numcsp+kuxp )
      parameter( kppp=numcs2p+1,                                         &
     &           kttp=1    +kppp,                                        &
     &           kwwp=levrp+kttp,                                        &
     &           kwhp=levrp+kwwp+1 )
#endif
#ifdef MP
#define NCPUSS jgrd1p
#define LEVRS levrp
#define LEVHS levhp
#else
#define NCPUSS ncpus
#define LEVRS levr
#define LEVHS levh
#endif
      parameter(lotsyn=numcs2+numcos+levr*1+1)
      dimension syn(IGRD1S,lotsyn,NCPUSS)
      dimension bgd(IGRD1S,numbgd*3,NCPUSS)
      dimension tmpu(IGRD1S,NCPUSS),tmpv(IGRD1S,NCPUSS)
      dimension tmpw(IGRD1S,NCPUSS)
      dimension tmp(lnwav,levrp1),tmpwork(igrd1,jgrd1)
! zyf test
      dimension zyftmp(lnwavp,levrp1),zyfgrid(lngrdp,levr+1)
      dimension zyffull(lngrd,levr+1)
#ifdef MP
      parameter(lotsynk=numcs2p+numcosp+levrp*1+1)
      dimension synxk(igrd1p,lotsynk,jgrd1)
      dimension synyk(igrd1,lotsynk,jgrd1p)
#endif
      common/reprow/                                                     &       
     &  tex(LNWAVS,levr)                                                 &! cos in y direction
     &, vvy(LNWAVS,levr)                                                 &
     &, uux(LNWAVS,levr)                                                 &
     &, tey(LNWAVS,levr)                                                 &! sin in y direction
     &, vvx(LNWAVS,levr)                                                 &
     &, uuy(LNWAVS,levr)                                                 &
#ifdef MP
     &, texk(llwavp,levrp)                                               &! cos in y direction
     &, vvyk(llwavp,levrp)                                               &
     &, uuxk(llwavp,levrp)                                               &
     &, teyk(llwavp,levrp)                                               &! sin in y directio
     &, vvxk(llwavp,levrp)                                               &
     &, uuyk(llwavp,levrp)                                               &
#endif
     &, tmprepro
!...
!.................................................................
!
#ifdef MP
      me=mype
      lonlens=lonlen(mype)
#else
      me=0
      lonlens=igrd1
#endif

!
! ---------------- q   dqdx    dqdy   d dx  d dy   ---------
!
      call dellcc (LNWAVS,q, dpdlam, dpdphi, epsx, epsy, 1)
!
      call dellsc(LNWAVS,uu, uux, uuy, epsx, epsy, levr)
      call dellcs(LNWAVS,vv, vvx, vvy, epsx, epsy, levr)
      call dellcc(LNWAVS,te, tex, tey, epsx, epsy, levr)
!
#ifdef MP
#define XS xa
#define YS ya
#define WS wa
#define ZS za
#define RTS rta
#define PS pa
#define TS ta
#define ONS ona
#else
#define XS x
#define YS y
#define WS w
#define ZS z
#define RTS rt
#define PS p
#define TS t
#define ONS on
#endif
!
#ifdef NONHYD
      do k=1,LEVRS+1
      do j=1,LNWAVS
      ONS(j,k)=0.e0
      enddo
      enddo
#endif
!
!           lat loop
! compute lat band limits
!
#ifndef MP
      last=mod(JGRD1S,ncpus)
      nggs=JGRD1S/ncpus
      if(last.ne.0) nggs=nggs+1
      inclat=ncpus
      lat1=1-ncpus
      lat2=0
      latdon=0
!     print *,' start multi-tasking'
!     print *,' ncpus nggs ',ncpus,nggs
!
      do 10000 ngg=1,nggs
      if((ngg.eq.nggs).and.(last.ne.0)) inclat=last
      lat1=lat1+ncpus
      lat2=lat2+inclat
!       print *,' lat1 lat2 ',lat1,lat2
#endif
!
#ifdef MP
       call rmpnn2n (q,lnwavp,qa,llwavp,1)
       call rmpnn2n (dpdlam,lnwavp,dpdlama,llwavp,1)
       call rmpnn2nk(uu,lnwavp,levr,uua,llwavp,levrp,1)
       call rmpnn2nk(te,lnwavp,levr,tea,llwavp,levrp,1)
       call rmpnn2n (dpdphi,lnwavp,dpdphia,llwavp,1)
       call rmpnn2nk(vv,lnwavp,levr,vva,llwavp,levrp,1)
       call rmpnn2nk(tex,lnwavp,levr,texk,llwavp,levrp,6)
#define LAT1S 1
#define LAT2S jgrd1
#define KPXS kpxp
#define KPYS kpyp
#define KTXS ktxp
#define KTYS ktyp
#define KUXS kuxp
#define KUYS kuyp
#define KVXS kvxp
#define KVYS kvyp
#define KPPS kppp
#define DPDLAMS dpdlama
#define DPDPHIS dpdphia
#define TEXS texk
#define TEYS teyk
#define LNPS qa
#define SYNS synxk
#define LATDONS 0
#else
#define LAT1S lat1
#define LAT2S lat2
#define KPXS kpx
#define KPYS kpy
#define KTXS ktx
#define KTYS kty
#define KUXS kux
#define KUYS kuy
#define KVXS kvx
#define KVYS kvy
#define KPPS kpp
#define DPDLAMS dpdlam
#define DPDPHIS dpdphi
#define TEXS tex
#define TEYS tey
#define LNPS q
#define SYNS syn
#define LATDONS latdon
#endif
!
! first lat loop
!mic$ do all
!mic$1 shared(syn,csing,ccosg,gsinc,gcosc,lat1,lat2,latdon)
!mic$1 shared(kpp,kpm,kpx,kpy,ktt,ktm,ktx,kty,kqq,kqm,kqx,kqy)
!mic$1 shared(kuu,kum,kux,kuy,kvv,kvm,kvx,kvy)
!mic$1 shared(q,dpdlam,dpdphi)
!mic$1 shared(qm,uum,vvm,tem,rm)
!mic$1 shared(uu,uux,uuy,vv,vvx,vvy)
!mic$1 shared(te,tex,tey,rq,rqx,rqy)
!mic$1 private(i,k,lat,lan,synhold)
!      autoscope
!
!      do 1000 lat = LAT1S,LAT2S
!      lan=lat-LATDONS
!
! ....... obtain perturbation values
!                 cos    sin  in y direction
!x    call sumfcs(dpdlam,dpdphi,syn(1,kpx,lan),syn(1,kpy,lan),
!x   1                          ccosg(1,lat),csing(1,lat), 1)
!x    call sumfcs( uu   , vv   ,syn(1,kuu,lan),syn(1,kvv,lan),
!x   1                          ccosg(1,lat),csing(1,lat), levr)
!      call sumfcs(DPDLAMS,DPDPHIS,SYNS(1,KPXS,lan),SYNS(1,KPYS,lan),
!     &                           ccosg(1,lat),csing(1,lat),
!     &                           IGRD12S,LLWAVS,IWAV1S,jwav1,IGRD1S,
!     &                           LEVRS+1)
!1.dpdla,dpdphi...uu,vv...uum...vvm
       call ffsncsy1(DPDLAMS,DPDPHIS,SYNS,                               &
     &    KPXS,KPYS,IWAV1S,IGRD1S,lotsynk,LEVRS+1)
!x    call sumfcs( tex  , tey  ,syn(1,ktx,lan),syn(1,kty,lan),
!x   1                          ccosg(1,lat),csing(1,lat), levr)
!x    call sumfcs( vvy  , vvx  ,syn(1,kvy,lan),syn(1,kvx,lan),
!x   1                          ccosg(1,lat),csing(1,lat), levr)
!x    call sumfcs( uux  , uuy  ,syn(1,kux,lan),syn(1,kuy,lan),
!x   1                          ccosg(1,lat),csing(1,lat), levr)
!      call sumfcs( TEXS  , TEYS  ,SYNS(1,KTXS,lan),SYNS(1,KTYS,lan),
!     &                           ccosg(1,lat),csing(1,lat),
!     &                           IGRD12S,LLWAVS,IWAV1S,jwav1,IGRD1S,
!     &                           LEVRS*3)
!!.2.tex,tey...vvy,vvx...uux,uuy
       call ffsncsy1(TEXS,TEYS,SYNS,                                     &
     &    KTXS,KTYS,IWAV1S,IGRD1S,lotsynk,levrp*3)
! switch uux uuy and vvy vvx for ffsncs
      do lat= LAT1S,LAT2S
      lan=lat-LATDONS
      do k=1,LEVRS
      do i=1,lonlens
        synhold              = SYNS(i,KUXS-1+k,lan)
        SYNS(i,KUXS-1+k,lan) = SYNS(i,KUYS-1+k,lan)
        SYNS(i,KUYS-1+k,lan) = synhold
        synhold              = SYNS(i,KVXS-1+k,lan)
        SYNS(i,KVXS-1+k,lan) = SYNS(i,KVYS-1+k,lan)
        SYNS(i,KVYS-1+k,lan) = synhold
      enddo
      enddo
      enddo
! 
!x    call sumffc( q    ,syn(1,kpp,lan),ccosg(1,lat),     1)
!x    call sumffc( te   ,syn(1,ktt,lan),ccosg(1,lat), levr)
!      call sumffc( LNPS ,SYNS(1,KPPS,lan),ccosg(1,lat),
!     &       IGRD1S,LLWAVS,IWAV1S,jwav1,IGRD1S,LEVRS+1)
      call ffscosy1(LNPS,SYNS,KPPS,                                      &
     &    IWAV1S,IGRD1S,lotsynk,LEVRS+1)
!
!1000  continue
#undef LAT1S
#undef LAT2S
#undef SYNS
#ifdef MP
      call rmpnl2ny(synxk,igrd1p,jgrd1,synyk,igrd1,jgrd1p,               &
     &               lotsynk,1,lotsynk)

!       call maxmin(synyk(1,kuup,1),igrd1,1,1,1,'in rloopmp, check u')
#define LAT1S 1
#define LAT2S latlen(mype)
#define NUMCSS numcsp
#define NUMCS2S numcs2p
#define NUMCOS2S numcos2p
#define NUMCOSS numcosp
#define SYNS synyk
#define LATDONS 0
#define LOCNCOSS locncosp
#define NONCOS2S noncos2p
#define LOCNCSS locncsp
#define NONCS2S noncs2p
#else
#define LAT1S lat1
#define LAT2S lat2
#define NUMCSS numcs
#define NUMCS2S numcs2
#define NUMCOS2S numcos2
#define NUMCOSS numcos
#define SYNS syn
#define LATDONS latdon
#define LOCNCOSS locncos
#define NONCOS2S noncos2
#define LOCNCSS locncs
#define NONCS2S noncs2
#endif
!
! second loop
!mic$ do all
!mic$1 shared(wh,syn,bgd,lat1,lat2,latdon,deltim)
!mic$1 shared(kpp,kpm,kpx,kpy,ktt,ktm,ktx,kty,kqq,kqm,kqx,kqy)
!mic$1 shared(kuu,kum,kux,kuy,kvv,kvm,kvx,kvy)
!mic$1 shared(numcs,numcs2,numcos,numcos2,numbgd)
!mic$1 shared(qb,qltb,wsltb,dwsltb)
!mic$1 shared(kppb,kttb,kqqb,kuub,kvvb)
!mic$1 shared(kpxb,ktxb,kqxb,kuxb,kvxb)
!mic$1 shared(kpyb,ktyb,kqyb,kuyb,kvyb)
!mic$1 shared(kplb,ktlb,kqlb,kulb,kvlb)
!mic$1 shared(sltb,rltb,bndrlx)
!mic$1 shared(si,sl,p1,p2,am,xm2,xm2px,xm2py)
!mic$1 shared(corf,gzdx,gzdy,del,rdel2)
!mic$1 private(lat,lan,latj,i,j,k)
!      autoscope
!
!      do 2000 lat=LAT1S,LAT2S
!      lan=lat-LATDONS
!!
!      call ffsncs( SYNS(1,1,lan), SYNS(1,NUMCSS+1,lan),NUMCS2S )
!      call ffscos( SYNS(1,NUMCS2S+1,lan),NUMCOS2S )
!2000  continue
        
       call ffsncsx1(synyk,jgrd1p,1,NUMCSS+1,lotsynk,NUMCSS)
       call ffscosx1(synyk,jgrd1p,NUMCS2S+1,lotsynk,NUMCOSS)
!
#undef LAT1S
#undef LAT2S
#ifdef MP
      call rmpx2nx(synyk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &     
     &             jgrd1p,kpyp,kpy,1)
      call rmpx2nx(synyk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &
     &             jgrd1p,kpxp,kpx,1)
      call rmpx2nx(synyk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &
     &             jgrd1p,kppp,kpp,1)
      call rmpnk2nx(synyk,igrd1,lotsynk,syn,igrd1p,lotsyn,               &
     &             jgrd1p,levrp,levr,kvvp,kvv,4)
      call rmpnk2nx(synyk,igrd1,lotsynk,syn,igrd1p,lotsyn,               &
     &             jgrd1p,levrp,levr,kuup,kuu,4)
      call rmpnk2nx(synyk,igrd1,lotsynk,syn,igrd1p,lotsyn,               &
     &             jgrd1p,levrp,levr,kttp,ktt,1)
!

     
#define LAT1S 1
#define LAT2S latlen(mype)
#define LATDONS 0
#define LATX lan
#define LATTS latstr(mype)-1
#else
#define LAT1S lat1
#define LAT2S lat2
#define LATDONS latdon
#define LATX lat
#define LATTS 0
#endif
!
!      call maxmin(uub,lngrdb,levr,1,levr,' base u bef pint3')
!
      do 2050 lat=LAT1S,LAT2S
      lan=lat-LATDONS
      latt=lat+LATTS

!
! ........obtain base field values
!
      call pint3(qb  ,bgd(1,kppb,lan),                                   &   
     &                bgd(1,kpxb,lan),                                   &
     &                bgd(1,kpyb,lan),wsltb,dwsltb,latt,numbgd,          &
     &                IGRD1S,                                            &
     &                ib1,ib2,jb1,jb2,jbx,istr,ilen,                     &
     &                bgf,border,jgrd1)
!
!      call maxmin(bgd(1,kuub,lan),IGRD1S,levr,1,levr,' base u bef hydrow')
!
      latj = (lat-1)*IGRD1S
!
! .......obtain full field values
!
      do 310 i=1,lonlens
      syn(i,kpp,lan)=syn(i,kpp,lan) + bgd(i,kppb,lan)
      syn(i,kpy,lan)=syn(i,kpy,lan) + bgd(i,kpyb,lan)
      syn(i,kpx,lan)=syn(i,kpx,lan) + bgd(i,kpxb,lan)
 310  continue
      do 320 k=1,levr
      do 320 i=1,lonlens
      syn(i,kuu-1+k,lan)=syn(i,kuu-1+k,lan)+bgd(i,kuub-1+k,lan)
      syn(i,kvv-1+k,lan)=syn(i,kvv-1+k,lan)+bgd(i,kvvb-1+k,lan)
      syn(i,ktt-1+k,lan)=syn(i,ktt-1+k,lan)+bgd(i,kttb-1+k,lan)
      syn(i,kux-1+k,lan)=syn(i,kux-1+k,lan)+bgd(i,kuxb-1+k,lan)
      syn(i,kuy-1+k,lan)=syn(i,kuy-1+k,lan)+bgd(i,kuyb-1+k,lan)
      syn(i,kvx-1+k,lan)=syn(i,kvx-1+k,lan)+bgd(i,kvxb-1+k,lan)
      syn(i,kvy-1+k,lan)=syn(i,kvy-1+k,lan)+bgd(i,kvyb-1+k,lan)
      syn(i,ktx-1+k,lan)=syn(i,ktx-1+k,lan)+bgd(i,ktxb-1+k,lan)
      syn(i,kty-1+k,lan)=syn(i,kty-1+k,lan)+bgd(i,ktyb-1+k,lan)
 320  continue
!
2050  continue

      do 2060 lat=LAT1S,LAT2S
      lan=lat-LATDONS
      latt=lat+LATTS

      call hydrow(syn(1,kuu,lan),syn(1,kvv,lan),                         &   
     &           syn(1,ktt,lan),                                         &
     &           syn(1,kpx,lan),syn(1,kpy,lan),                          &
     &           syn(1,kux,lan),syn(1,kuy,lan),                          &
     &           syn(1,kvx,lan),syn(1,kvy,lan),                          &
     &           syn(1,ktx,lan),syn(1,kty,lan),                          &
     &           syn(1,kww,lan),                                         &
     &           si,sl,p1,p2,am,                                         &
     &           xm2(1,lat),gzdx(1,lat),gzdy(1,lat),                     &
     &           del,rdel2,lonlens,IGRD1S,levr,levrp1,levrm1)
!

2060  continue
!
#ifdef MP
      synyk=0.
      call rmpnx2nk(syn,igrd1p,lotsyn,synyk,igrd1,lotsynk,               &
     &             jgrd1p,levr,levrp,kww,kwwp,1)
      call rmpnx2x(syn,igrd1p,lotsyn,synyk,igrd1,lotsynk,                &
     &             jgrd1p,kww+levr,kwwp+levrp,1)

#define LAT1S 1
#define LAT2S latlen(mype)
#define KWWS kwwp
#define LEVRS levrp
#define SYNS synyk
#define LATDONS 0
#else
#define LAT1S lat1
#define LAT2S lat2
#define KWWS kww
#define LEVRS levr
#define SYNS syn
#define LATDONS latdon
#endif
!
!      do 2080 lat=LAT1S,LAT2S
!      lan=lat-LATDONS
!
!  the following separation of ffa depends on vertical resolution
!
!      call ffacos( SYNS(1,KWWS,lan), LEVRS*1+1 )
!
!2080  continue
       call ffacosx1(SYNS,jgrd1p,KWWS,lotsynk,LEVRS*1+1 )
!
#undef LAT1S
#undef LAT2S
#undef SYNS
!
#ifdef MP
      synxk=0.
      call rmpny2nl(synyk,igrd1,jgrd1p,synxk,igrd1p,                     &   
     &       jgrd1,lotsynk,kwwp,levrp1p)

#define LAT1S 1
#define LAT2S jgrd1
#define KWWS kwwp
#define LEVRS levrp
#define ONS ona
#define SYNS synxk
#define LATDONS 0
#else
#define LAT1S lat1
#define LAT2S lat2
#define KWWS kww
#define LEVRS levr
#define ONS on
#define SYNS syn
#define LATDONS latdon
#endif

!
! last loop
!  sum the regional full forcing
!
      call ffacosy1(SYNS,ONS,KWWS,IGRD1S,IWAV1S,lotsynk,LEVRS+1)
!      do 3000 lat=LAT1S,LAT2S
!      lan=lat-LATDONS
!      call sumggc(SYNS(1,KWWS,lan),ONS, gcosc(1,lat),
!     &            IGRD1S,LLWAVS,IWAV1S,jwav1,IGRD1S,LEVRS+1)
!
!3000  continue
!
#undef KWWS
#undef SYNS
#undef ONS
#undef LAT1S
#undef LAT2S
#undef LEVRS
#undef LATDONS
#ifndef MP
      latdon=latdon+(lat2-lat1+1)
!     print *,' finish a slab  latdon=',latdon
!
10000 continue
#endif
!
#ifdef MP
      call rmpnk2nn(ona,llwavp,levrp,on,lnwavp,levr,1)
      call rmpn2nn(ona(1,levrp1p),llwavp,on(1,levrp1),lnwavp,1)
#endif
!       zyftmp=on
!       call cctogd(zyftmp,zyfgrid,levr)
!       call cctogd(zyftmp(1,levrp1),zyfgrid(1,levrp1),1)
!       call rmpgp2f(zyfgrid,igrd1p,jgrd1p,zyffull,igrd1,jgrd1,levr+1)
!       if( mype.eq.master ) then
!          call maxmin(zyffull,lngrd,levrp1,1,levrp1,'rloopw wp')
!          call write_to_grads(zyffull,igrd1,jgrd1,levrp1,'rloopwwp')
!       endif
!       tmp=0.
!       call rmpsp2f(on,lnwavp,tmp,lnwav,levrp1)
!       if ( mype.eq.0) then
!         call maxmin(tmp,lnwav,levrp1,1,levrp1,'wng sp in rloopw 1')
!       endif
      call rmpsynall 
!      call rmpabort
!      print *,'after rmpsynall,finish rloopw '
!
#endif
      return
      end
