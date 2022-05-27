       subroutine rloopsp
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rloopa
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  
!   program  starts with spectral coefficients temp.
!   of x-wind, y-wind, specific humidity, and
!   ln((psfc).  converts them to the gaussian grid at each
!   row and calls rfidi,  for the northern and southern
!   rows at the same time.  after return from rfidi
!   sr.  completes calculation of perturbation tendencies 
!   of temp. wind. and lnps. specific humidity with relaxation. 
!   all input/output  is via commons.
!
! program history log:
!
! usage:    call rloopa
!   input argument list:
!
!   output argument list:
!
!   common block:
!       rscomf  rscommap  rscomltb
!
!   input files: none
!
!   output files: none
!
!   subpcrograms called: 
!       dellcc  dellsc  dellcs  sumfcs  sumffc  ffsncs ffscos  pint3
!       rfidi   pint1   ffancs  ffacos  sumgcs  sumggc 
!       
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!.....
!.....fold north y and south y 
!.....use perturbation method by substraction of full field forcing
!     by large scale forcing.
!.....
!ccj
        use physics_module
        use radiation_module
!c
#include <paramodel.h>
#ifdef CRA
#include <rscomcpu.h>
#else
#include <ncpu.h>
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
#include <rscomoz.h>
#include <rdparm.h>
!
!#ifdef AFA
!      parameter(ncpus = ncpu )
!#endif
#ifdef T
      dimension at(igrd1,levr),bt(igrd1,levr),ct(igrd1,levr)
#endif
#ifdef Q
      dimension aq(igrd1,levr,ntotal),bq(igrd1,levr,ntotal)
#endif
#ifdef U
      dimension au(igrd1,levr),bu(igrd1,levr),fu(igrd1,levr)
     &         ,pu(igrd1,levr)
#endif
#ifdef V
      dimension av(igrd1,levr),bv(igrd1,levr),fv(igrd1,levr)
     &         ,pv(igrd1,levr)
#endif
#ifdef P
      dimension ap(igrd1),bp(igrd1)
#endif
!....
      parameter(numcs = 1 + levr*5 + levh, numcs2 = numcs * 2 )
      parameter(numcos = 1 + levr*1 + levh, numcos2 = numcos * 2 )
!                 cos             sin          in x direction
      parameter( kpy=1       , kpx=numcs+kpy,                            &  
     &           kvv=1   +kpy, kuu=numcs+kvv,                            &
     &           kvm=levr+kvv, kum=numcs+kvm,                            &
     &           kty=levr+kvm, ktx=numcs+kty,                            &
     &           kqy=levr+kty, kqx=numcs+kqy,                            &
     &           kvy=levh+kqy, kvx=numcs+kvy,                            &
     &           kux=levr+kvy, kuy=numcs+kux )
      parameter( kpp=numcs2+1,                                           &
     &           ktt=1    +kpp,                                          &
     &           kqq=levr+ktt,                                           &
     &           ktm=levh+kqq,                                           &
     &           kpm=levr+ktm,                                           &
     &           kqm=1   +kpm)
!
      parameter( numbgd=1+levr*3+levh,                                   &
     &           kplb=1,                                                 &
     &           kulb=kplb+1,                                            &
     &           kvlb=kulb+levr,                                         &
     &           ktlb=kvlb+levr,                                         &
     &           kqlb=ktlb+levr )
      parameter( kppb=numbgd+1,                                          &
     &           kuub=kppb+1,                                            &
     &           kvvb=kuub+levr,                                         &
     &           kttb=kvvb+levr,                                         &
     &           kqqb=kttb+levr)
      parameter( kpxb=2*numbgd+1,                                        &
     &           kuxb=kpxb+1,                                            &
     &           kvxb=kuxb+levr,                                         &
     &           ktxb=kvxb+levr,                                         &
     &           kqxb=ktxb+levr )
      parameter( kpyb=3*numbgd+1,                                        &
     &           kuyb=kpyb+1,                                            &
     &           kvyb=kuyb+levr,                                         &
     &           ktyb=kvyb+levr,                                         &
     &           kqyb=ktyb+levr )
!jun
#ifdef NONHYD
      parameter( noncs=1+levr*3,noncs2=noncs*2,                          &
     &           locncs=1,                                               &
     &           knpy=locncs   ,   knpx=noncs+knpy,                      &
     &           knty=knpy+levr,   kntx=noncs+knty,                      &
     &           knwy=knty+levr,   knwx=noncs+knwy,                      &
     &           noncos=1+levr*3,noncos2=noncos*2,                       &
     &           locncos=locncs+noncs2,                                  &
     &           knpp=locncos   ,   knpm=noncos+knpp,                    &
     &           kntt=knpp+levr,   kntm=noncos+kntt,                     &
     &           knww=kntt+levr,   knwm=noncos+knww)
      parameter( knppa=1,                                                &
     &           kntta=knppa+levr,                                        &
     &           knwwa=kntta+levr )
#endif
!
      parameter(kus=1,                                                   &
     &          kvs=levr+kus,                                            &
     &          kts=levr+kvs,                                            &
     &          kqs=levr+kts)
      parameter(kpyr=1,        kvvr=1    +kpyr,                          &!cos
     &          kpxr=levr+kvvr,kuur=1    +kpxr,                          &!sin
     &          kddr=levr+kuur,                                          &
     &          kppr=levr+kddr,                                          &
     &          kttr=1   +kppr,                                          &
     &          kqqr=levr+kttr )
      parameter(kuum=1,                                                  &
     &          kvvm=levr+kuum,                                          &
     &          kttm=levr+kvvm,                                          &
     &          kppm=levr+kttm,                                          &
     &          kqqm=1   +kppm)

#define ncpus ncpu
!c
      parameter(lotsyn=numcs2+numcos2)
      parameter(lotsynr=3+4*levr+levh)
      parameter(lotsynm=1+3*levr+levh)
      dimension syn(igrd1,lotsyn,ncpus)
      dimension synr(igrd1,lotsynr,ncpus)
      dimension synm(igrd1,lotsynm,ncpus)
      dimension syndd(igrd1,levr,ncpus)
      dimension bgd(igrd1,numbgd*4,ncpus)
#ifdef NONHYD
      parameter(lotsnnk=noncs2+noncos2)
      parameter(lotannk=levr*3+1)
      dimension snnxk(igrd1,lotsnnk,jgrd1)
      dimension snnyk(igrd1,lotsnnk,jgrd1)
      dimension annxk(igrd1,lotannk,jgrd1)
      dimension annyk(igrd1, lotannk,jgrd1)

      parameter(lotsnn=noncs2+noncos2)
      parameter(lotann=levr*3+1)
      dimension snn(igrd1,lotsnn,jgrd1)
      dimension ann(igrd1,lotann,jgrd1)
#endif

!
      common/reproas/                                                     &
     &  tex(lnwav,levr)                                                  &! cos in y direction
     &, rqx(lnwav,levr,ntotal)                                           &
     &, vvy(lnwav,levr)                                                  &
     &, uux(lnwav,levr)                                                  &
     &, tey(lnwav,levr)                                                  &! sin in y direction
     &, rqy(lnwav,levr,ntotal)                                           &
     &, vvx(lnwav,levr)                                                  &
     &, uuy(lnwav,levr)                                                  &
     &, spd(igrd1,levr)
      parameter(lotanl=levr*3+levh)
      parameter(lotbgd=levr*7+levh+3)
      dimension anl(igrd1,lotanl, ncpus)
      dimension ze(lnwav,levr)
!
#ifdef NONHYD
      common/repronon/                                                   &
     &  pnx(lnwav,levr)                                                 &
     &, tnx(lnwav,levr)                                                 &
     &, onx(lnwav,levrp1)                                               &
     &, pny(lnwav,levr)                                                 &
     &, tny(lnwav,levr)                                                 &
     &, ony(lnwav,levrp1)   
#endif
!...........................................................
#ifdef RKN
      dimension udx(igrd1,levr,ncpus),udy(igrd1,levr,ncpus)
      dimension vdx(igrd1,levr,ncpus),vdy(igrd1,levr,ncpus)
!
!radiation_run
      real (kind=kind_rad) flwupf(igrd1p,ncpus),                         & 
     &     fswupf(igrd1p,ncpus),fswdnf(igrd1p,ncpus),                    &
     &     sswupf(igrd1p,ncpus),sswdnf(igrd1p,ncpus),                    &
     &     slwupf(igrd1p,ncpus),slwdnf(igrd1p,ncpus),                    &
     &     cldsaf(igrd1p,5,ncpus)
      integer mtopaf(igrd1p,3,ncpus),mbotaf(igrd1p,3,ncpus)
!jun
      dimension psexp(igrd1,ncpus),radsl(igrd1,ncpus),                   &
     &  dlwsf1(igrd1,ncpus),qss(igrd1,ncpus),                            &
     &  gflx(igrd1,ncpus),cd(igrd1,ncpus),                               &
     &  cdq(igrd1,ncpus),evap(igrd1,ncpus),                              &
     &  hflx(igrd1,ncpus),drain(igrd1,ncpus),                            &
     &  runof(igrd1,ncpus),cld1d(igrd1,ncpus),                           &
     &  gamt(igrd1,ncpus),gamq(igrd1,ncpus),                             &
     &  dqsfc1(igrd1,ncpus),dtsfc1(igrd1,ncpus),                         &
     &  dusfc1(igrd1,ncpus),dvsfc1(igrd1,ncpus),                         &
     &  dusfcg(igrd1,ncpus),dvsfcg(igrd1,ncpus),                         &
     &  rainc(igrd1,ncpus),rainl(igrd1,ncpus),                           &
     &  hsw(igrd1,levr,ncpus),vvel(igrd1,levr,ncpus),                    &
     &  stsoil(igrd1,lsoil,ncpus),smsoil(igrd1,lsoil,ncpus),             &
     &  gu0(igrd1,levr,ncpus),gv0(igrd1,levr,ncpus),                     &
     &  gt0(igrd1,levr,ncpus),gq0(igrd1,levh,ncpus)  
!june    
#endif
       integer itimsw,itimlw
#ifdef NONHYD
      dimension hydtmp(igrd1,levr)
#endif

!...
      common/rcomdhcs/spdlat(levr,jgrd1),sm(lnwav)
!.................................................................
!
      me=0
      lonlens=igrd1
      lonstrs=igrd1
      latlens=jgrd1
      last=mod(jgrd1,ncpus)
      nggs=jgrd1/ncpus
      if(last.ne.0) nggs=nggs+1
      inclat=ncpus
      lat1=1-ncpus
      lat2=0
      latdon=0
      lon1=1
      lon2=lonlens
!c
!     if(me.eq.0) print *,' rloopsp start'
!............................................................
! ---------------- q   dqdx    dqdy   d dx  d dy   ---------
      call dellcc(lnwav,q,dpdlam,dpdphi,epsx,epsy,1)
      call dellsc(lnwav,uu,uux,uuy,epsx,epsy,levr)
      call dellcs(lnwav,vv,vvx,vvy,epsx,epsy,levr)
      call dellcc(lnwav,te,tex,tey,epsx,epsy,levr+levh)
#ifdef NONHYD
      call dellcc(lnwav,pn,pnx,pny,epsx,epsy,levr*3+1)
#endif
!x    call dellcc(lnwav,rq, rqx, rqy, epsx, epsy, levr)
!jun
       call physics_init(me,lonlens,latlens,lngrd,                       &
     &   num_p3d,num_p2d,levr,levh,levrp1,ncpus,                         &
     &   igrd1,jgrd1,igrd1,jgrd1,fhour,fhswr,phour,lsoil,lsfwd,          &  
     &   deltim,lscca,lsswr,                                             &
     &   si(1),idate(1),phy_f3d(1,1,1,1),phy_f2d(1,1,1))
!
       if (lsphy) then
       call radiation_init(me,lonlens,lonstrs,latlens,levr,levrp1,       &
     &   levh,ncpus,lat1s,lat2s,latts,l,lp1,lp1v,                        &
     &   igrd1,jgrd1,ncpu,ntotal,                                        &
     &   lnwav,ncld,nvect,num_p3d,nmtvr,                                 &
     &   fhour,phour,fhswr,fhlwr,deltim,                                 &
     &   lsswr,lslwr,lssav,lw_type,ntoz,                                 &
     &   si,sl,idate,rlat,rlon,                                          &
     &   sinlar,coslar,itimsw,itimlw)
       endif

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
       if (lsphy) then
       call physics_prep(ozprd(1,1,1),ozdis(1,1,1),                      &
     &  ozprdout(1,1),ozdisout(1,1),solhr,clstp)
!
        call radiation_prep(sdec,cdec,slag,q,uu,te,vv,del,rq,            &
     &    tsea,sheleg,coszer,hprime(1,1,1),stc(1,1,1),                   &
     &    sinlar,coslar)
       endif
!ccjune
!
      uua=0.e0
      vva=0.e0
      tea=0.e0
      rqa=0.e0
      xa=0.e0
      ya=0.e0
      wa=0.e0
      rta=0.e0
!
      z=0.e0
#ifdef NONHYD
      pa=0.
      ta=0.
      oa=0.
      pna=0.
      tna=0.
      ona=0.
#endif
!
!           lat loop
! compute lat band limits
!
!      last=mod(jgrd1,ncpus)
!      nggs=jgrd1/ncpus
!      if(last.ne.0) nggs=nggs+1
!      inclat=ncpus
!      lat1=1-ncpus
!      lat2=0
!      latdon=0
!
      do 10000 ngg=1,nggs
      if((ngg.eq.nggs).and.(last.ne.0)) inclat=last
      lat1=lat1+ncpus
      lat2=lat2+inclat
!
! first lat loop
!$omp parallel do private(i,k,lat,lan,synhold)
      do 1000 lat = lat1,lat2
      lan=lat-latdon
! 1.dpdla,dpdphi...uu,vv...uum...vvm     
       call sumfcs1(dpdlam,dpdphi,syn(1,kpx,lan),syn(1,kpy,lan),          &  
     &   ccosg(1,lat),csing(1,lat),                                       &
     &   igrd1,lnwav,iwav1,jwav1,                                         &
     &   levr*2+1)
!.2.tex,tey...rqx,rqy...vvy,vvx...uux,uuy
       call sumfcs1(tex,tey,syn(1,ktx,lan),syn(1,kty,lan),                &
     &      ccosg(1,lat),csing(1,lat),                                    &
     &      igrd1,lnwav,iwav1,jwav1,                                      &
     &      levr*3+levh)
!
! switch uux uuy and vvy vvx for ffsncs
      do k=1,levr
      do i=1,lonlens
        synhold            = syn(i,kux-1+k,lan)
        syn(i,kux-1+k,lan) = syn(i,kuy-1+k,lan)
        syn(i,kuy-1+k,lan) = synhold
        synhold            = syn(i,kvx-1+k,lan)
        syn(i,kvx-1+k,lan) = syn(i,kvy-1+k,lan)
        syn(i,kvy-1+k,lan) = synhold
      enddo
      enddo
!
!.3. q,te,rq,rwm,qm,rm
       call sumffc1( q ,syn(1,kpp,lan),ccosg(1,lat),                     &  
     &      igrd1,lnwav,iwav1,jwav1,                                     &
     &      levr*2+levh*2+2)
1000   continue
!     
! second loop
!
      do 2000 lat=lat1,lat2
      lan=lat-latdon
      call ffsncs( syn(1,1,lan), syn(1,numcs+1,lan),numcs )
      call ffscos( syn(1,numcs2+1,lan),numcos2 )
2000  continue
!
      latts=0
      do 2050 lat=lat1,lat2
      lan=lat-latdon
      latt=lat+latts
!
!      print *,'before rfidi,pert',syn(5,kuu,lan),syn(5,kvv,lan),
!     &    syn(5,ktt,lan),syn(5,kqq,lan),syn(5,kpp,lan)
! ........obtain base field values
!
      call pint3(qb  ,bgd(1,kppb,lan),                                   &  
     &                bgd(1,kpxb,lan),                                   &  
     &                bgd(1,kpyb,lan),wsltb,dwsltb,latt,numbgd,          &
     &                igrd1,                                             &
     &                ib1,ib2,jb1,jb2,jbx,istr,ilen,                     &
     &                bgf,border,jgrd1)
!
      latj = (lat-1)*igrd1   
! .......obtain full field values
!
      do 310 i=1,lonlens
      syn(i,kpp,lan)=syn(i,kpp,lan) + bgd(i,kppb,lan)
      syn(i,kpy,lan)=syn(i,kpy,lan) + bgd(i,kpyb,lan)
      syn(i,kpx,lan)=syn(i,kpx,lan) + bgd(i,kpxb,lan)
 310  continue
      do 320 k=1,levr
      do 320 i=1,lonlens
      syn(i,ktt-1+k,lan)=syn(i,ktt-1+k,lan)+bgd(i,kttb-1+k,lan)
      syn(i,kuu-1+k,lan)=syn(i,kuu-1+k,lan)+bgd(i,kuub-1+k,lan)
      syn(i,kvv-1+k,lan)=syn(i,kvv-1+k,lan)+bgd(i,kvvb-1+k,lan)
      syn(i,kux-1+k,lan)=syn(i,kux-1+k,lan)+bgd(i,kuxb-1+k,lan)
      syn(i,kuy-1+k,lan)=syn(i,kuy-1+k,lan)+bgd(i,kuyb-1+k,lan)
      syn(i,kvx-1+k,lan)=syn(i,kvx-1+k,lan)+bgd(i,kvxb-1+k,lan)
      syn(i,kvy-1+k,lan)=syn(i,kvy-1+k,lan)+bgd(i,kvyb-1+k,lan)
      syn(i,ktx-1+k,lan)=syn(i,ktx-1+k,lan)+bgd(i,ktxb-1+k,lan)
      syn(i,kty-1+k,lan)=syn(i,kty-1+k,lan)+bgd(i,ktyb-1+k,lan)
#ifdef RKN
      udx(i,k,lan)=syn(i,kux-1+k,lan)
      udy(i,k,lan)=syn(i,kuy-1+k,lan)
      vdx(i,k,lan)=syn(i,kvx-1+k,lan)
      vdy(i,k,lan)=syn(i,kvy-1+k,lan)
#endif
 320  continue
      do 325 k=1,levh
      do 325 i=1,lonlens
      syn(i,kqq-1+k,lan)=syn(i,kqq-1+k,lan)+bgd(i,kqqb-1+k,lan)
      syn(i,kqx-1+k,lan)=syn(i,kqx-1+k,lan)+bgd(i,kqxb-1+k,lan)
      syn(i,kqy-1+k,lan)=syn(i,kqy-1+k,lan)+bgd(i,kqyb-1+k,lan)
 325  continue
!
!jun radiation
      do j=1,lonlens
          synr(j,kppr,lan) = syn(j,kpp,lan)
          synr(j,kpxr,lan) = xm2(j,lat)*syn(j,kpx,lan)
          synr(j,kpyr,lan) = xm2(j,lat)*syn(j,kpy,lan)
      enddo
      do k=1,levr
      do j=1,lonlens
          synr(j,kttr-1+k,lan)=syn(j,ktt-1+k,lan)
          synr(j,kuur-1+k,lan)=syn(j,kuu-1+k,lan)
          synr(j,kvvr-1+k,lan)=syn(j,kvv-1+k,lan)
          synr(j,kddr-1+k,lan)=xm2(j,lat)*                               & 
     &            (syn(j,kux-1+k,lan)+syn(j,kvy-1+k,lan))
      enddo
      enddo
      do k=1,levh
      do j=1,lonlens
         synr(j,kqqr-1+k,lan)=syn(j,kqq-1+k,lan)
      enddo
      enddo

       if (lsphy) then
!        if (me.eq.0) print*,'in rloopa,lat=',lat,'lotsynr=',lotsynr
!     &  ,kpyr,kvvr,kpxr,kuur,kddr,kppr,kttr,kqqr

        call radiation_run(lat,lan,lotsynr,                              & 
     &   kpyr,kvvr,kpxr,kuur,kddr,kppr,kttr,kqqr,                        &
#ifdef NONHYD 
     &   snn(1,knww,lan),                                                &
#endif
     &   synr,phy_f3d,fluxr,slmsk,                                       &
     &   hprime(1,1,1),stc(1,1,1),sheleg,zorl,cv,cvt,cvb,                &
     &   alvsf,alnsf,alvwf,alnwf,facsf,facwf,                            &
     &   swh,hlw,sfcnsw,sfcdlw,coszer,tsflw,cvavg,del                    &
#ifdef RKN
     &    ,flwupf(1,lan),fswupf(1,lan),fswdnf(1,lan),                    &
     &    sswupf(1,lan),sswdnf(1,lan),                                   &
     &    slwupf(1,lan),slwdnf(1,lan),                                   &
     &    cldsaf(1,1,lan),mtopaf(1,1,lan),mbotaf(1,1,lan)                &
#endif
     &    )
       endif
!
! ......... compute full field forcing
!
!
!      print *,'before rfidi ',syn(5,kuu,lan),syn(5,kvv,lan),
!     &    syn(5,ktt,lan),syn(5,kqq,lan),syn(5,kpp,lan)
#ifdef NONHYD
      call nfidi(syn(1,kuu,lan),syn(1,kvv,lan),                          &
     &           syn(1,ktt,lan),syn(1,kqq,lan),                          &
     &           syn(1,kpx,lan),syn(1,kpy,lan),                          &
     &           syn(1,kux,lan),syn(1,kuy,lan),                          &
     &           syn(1,kvx,lan),syn(1,kvy,lan),                          &
     &           syn(1,ktx,lan),syn(1,kty,lan),                          &
     &           syn(1,kqx,lan),syn(1,kqy,lan),                          &
     &           syn(1,kpp,lan),spdlat(1,lan),spd,                       &
     &           snn(1,knpp,lan),snn(1,kntt,lan),snn(1,knww,lan),        &
     &           snn(1,knpx,lan),snn(1,kntx,lan),snn(1,knwx,lan),        &
     &           snn(1,knpy,lan),snn(1,knty,lan),snn(1,knwy,lan),        &
     &           snn(1,knwm+levr,lan),deltim,                            &
     &           bgd(1,ktlb,lan),                                        &
     &           si,sl,p1,p2,am,                                         &
     &           xm2(1,lat),xm2px(1,lat),xm2py(1,lat),                   &
     &           corf(1,lat),corf2(1,lat),gzdx(1,lat),gzdy(1,lat),       &
     &           del,rdel2,lonlens,igrd1p,levr,ntotal)
#else
      call rfidi(syn(1,kuu,lan),syn(1,kvv,lan),                          &  
     &           syn(1,ktt,lan),syn(1,kqq,lan),                          &
     &           syn(1,kpx,lan),syn(1,kpy,lan),                          &
     &           syn(1,kux,lan),syn(1,kuy,lan),                          &
     &           syn(1,kvx,lan),syn(1,kvy,lan),                          &
     &           syn(1,ktx,lan),syn(1,kty,lan),                          &
     &           syn(1,kqx,lan),syn(1,kqy,lan),                          &
     &           syn(1,kpp,lan),spdlat(1,lan),                           &
     &     p1,p2,am,xm2(1,lat),xm2px(1,lat),xm2py(1,lat),                &
     &     corf(1,lat),gzdx(1,lat),gzdy(1,lat),del,rdel2,                &
     &     lonlens,igrd1,levr,levrm1,levrp1,ntotal                       &
#ifdef T
     &    ,at,bt,ct                                                      &
#endif
#ifdef Q
     &    ,aq,bq                                                         &
#endif
#ifdef U
     &    ,au,bu,fu,pu                                                   &
#endif
#ifdef V
     &    ,av,bv,fv,pv                                                   &
#endif
#ifdef P
     &    ,ap,bp                                                         &
#endif
     &    )
#endif
!jun
!       print *,'after rfidi, utend',syn(5,kuu,lan),
!     & syn(5,kvv,lan),syn(5,ktt,lan),syn(5,kqq,lan),
!     & syn(5,kpp,lan)
!cc compute n-1 partial total field temp,u,v,q,p 
!
      do i=1,lonlens
      synm(i,kppm,lan) = syn(i,kpm,lan)+bgd(i,kppb,lan)
      enddo
      do k=1,levr
      do i=1,lonlens
      synm(i,kuum-1+k,lan)=syn(i,kum-1+k,lan)+bgd(i,kuub-1+k,lan)
      synm(i,kvvm-1+k,lan)=syn(i,kvm-1+k,lan)+bgd(i,kvvb-1+k,lan)
      synm(i,kttm-1+k,lan)=syn(i,ktm-1+k,lan)+bgd(i,kttb-1+k,lan)
      enddo
      enddo
      do  k=1,levh
      do  i=1,lonlens
        synm(i,kqqm-1+k,lan)=syn(i,kqm-1+k,lan)+bgd(i,kqqb-1+k,lan)
      enddo
      enddo
!
! ...... get base field tendencies ....
!
      call pint1(qltb,bgd(1,kplb,lan),wsltb,latt,numbgd,igrd1,           &  
     &           ib1,ib2,jb1,jb2,jbx,istr,ilen,                          &
     &           bgf,border,jgrd1)

!      print *,'after pint, bs tend',bgd(5,kulb,lan),
!     &  bgd(5,kvlb,lan),bgd(5,ktlb,lan),bgd(5,kqlb,lan),
!     &  bgd(5,kplb,lan)

! update total field: add base field increment to nstar
      dt=dt2/2.
      do i=1,lonlens
      synm(i,kppm,lan)= -bgd(i,kplb,lan)*dt+synm(i,kppm,lan)
      enddo
      do k=1,levr
      do i=1,lonlens
      synm(i,kuum-1+k,lan)=-bgd(i,kulb-1+k,lan)*dt+synm(i,kuum-1+k,lan)
      synm(i,kvvm-1+k,lan)=-bgd(i,kvlb-1+k,lan)*dt+synm(i,kvvm-1+k,lan)
      synm(i,kttm-1+k,lan)=-bgd(i,ktlb-1+k,lan)*dt+synm(i,kttm-1+k,lan)
      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
      synm(i,kqqm-1+k,lan)=-bgd(i,kqlb-1+k,lan)*dt+synm(i,kqqm-1+k,lan)
      enddo
      enddo

!update to nstar
      do i=1,lonlens
      syn(i,kpp,lan) = syn(i,kpp,lan)*dt2+synm(i,kppm,lan)
      enddo
      do k=1,levr
      do i=1,lonlens
      syn(i,kuu-1+k,lan)=syn(i,kuu-1+k,lan)*dt2+synm(i,kuum-1+k,lan)
      syn(i,kvv-1+k,lan)=syn(i,kvv-1+k,lan)*dt2+synm(i,kvvm-1+k,lan)
      syn(i,ktt-1+k,lan)=syn(i,ktt-1+k,lan)*dt2+synm(i,kttm-1+k,lan)
#ifdef NONHYD
      hydtmp(i,k)=syn(i,ktt-1+k,lan)-bgd(i,kttb-1+k,lan)
#endif
      enddo
      enddo
      do  k=1,levh
      do  i=1,lonlens
      syn(i,kqq-1+k,lan)=syn(i,kqq-1+k,lan)*dt2+synm(i,kqqm-1+k,lan)
      enddo
      enddo

 
      do i=1,lonlens
      syn(i,kpx,lan) = xm2(i,lat)*syn(i,kpx,lan)
      syn(i,kpy,lan) = xm2(i,lat)*syn(i,kpy,lan)
      enddo
      do k=1,levr
      do i=1,lonlens
      syn(i,kuu-1+k,lan)=xm (i,lat)*syn(i,kuu-1+k,lan)
      syn(i,kvv-1+k,lan)=xm (i,lat)*syn(i,kvv-1+k,lan)
      enddo
      enddo
      do k=1,levr
      do i=1,lonlens
      syndd(i,k,lan)=xm2(i,lat)*(syn(i,kux-1+k,lan)+                     & 
     &                               syn(i,kvy-1+k,lan))
      enddo
      enddo
#ifdef NONHYD
      do k=1,levr
      do i=1,lonlens
      syn(i,ktt-1+k,lan)=syn(i,ktt-1+k,lan)+snn(i,kntt-1+k,lan)
      snn(i,knpp-1+k,lan)=syn(i,kpp,lan)+snn(i,knpp-1+k,lan)+log(sl(k))
      enddo
      enddo
#endif

!      print *,'rloopa,phys_run, uu ',syn(5,kuu,lan),syn(5,kvv,lan)
!     & ,(syn(5,kuu,lan)-synm(5,kuum,lan))/dt2,xm(5,lat)
!      print *,'before phys_run'

        call physics_run(lan,lat,                                       &
     &    syn(1,kpx,lan),syn(1,kpy,lan),                                &
     &    syn(1,kuu,lan),syn(1,kvv,lan),syn(1,kpp,lan),                 &
     &    syn(1,ktt,lan),syn(1,kqq,lan),syndd(1,1,lan),                 &
     &    anl(1,kts,lan),anl(1,kqs,lan),anl(1,kus,lan),anl(1,kvs,lan),  &
#ifdef NONHYD
     &    snn(1,knpp,lan),snn(1,knww,lan), snn(1,kntt,lan),             &
     &    ann(1,knppa,lan),ann(1,knwwa,lan),ann(1,kntta,lan),           &
#endif
#ifdef RKN
     &    psexp(1,lan),radsl(1,lan),                                    &
     &    dlwsf1(1,lan),qss(1,lan),                                     &
     &    gflx(1,lan),cd(1,lan),                                        &
     &    cdq(1,lan),evap(1,lan),                                       &
     &    hflx(1,lan),drain(1,lan),                                     &
     &    runof(1,lan),cld1d(1,lan),                                    &
     &    gamt(1,lan),gamq(1,lan),                                      &
     &    dqsfc1(1,lan),dtsfc1(1,lan),                                  &
     &    dusfc1(1,lan),dvsfc1(1,lan),                                  &
     &    dusfcg(1,lan),dvsfcg(1,lan),                                  &
     &    rainc(1,lan),rainl(1,lan),                                    &
     &    hsw(1,1,lan),vvel(1,1,lan),                                   &
     &    stsoil(1,1,lan),smsoil(1,1,lan),                              &
#endif
     &   phy_f3d,phy_f2d)

!      print *,'after phys, uu n+1',anl(5,kus,lan)/xm(5,lan),
!     &   syn(5,kuu,lan)/xm(5,lan),synm(5,kuum,lan),
!     &  anl(5,kvs,lan)/xm(5,lan),syn(5,kvv,lan)/xm(5,lan),
!     & synm(5,kvvm,lan)
!debug
!      do k=1,levr
!      do i=1,lonlens
!      anl(i,kus-1+k,lan)=syn(i,kuu-1+k,lan)
!      anl(i,kvs-1+k,lan)=syn(i,kvv-1+k,lan)
!      anl(i,kts-1+k,lan)=syn(i,ktt-1+k,lan)
!      enddo
!      enddo
!      do  k=1,levh
!      do  i=1,lonlens
!      anl(i,kqs-1+k,lan)=syn(i,kqq-1+k,lan)
!      enddo
!      enddo

      do k=1,levr
      do i=1,lonlens
      anl(i,kus-1+k,lan)=anl(i,kus-1+k,lan)/xm(i,lat)
      anl(i,kvs-1+k,lan)=anl(i,kvs-1+k,lan)/xm(i,lat)
      enddo
      enddo

      do i=1,lonlens
      syn(i,kpp,lan) = (syn(i,kpp,lan)-synm(i,kppm,lan))/dt2
      enddo
       do k=1,levr
       do i=1,lonlens
      syn(i,kuu-1+k,lan)=(anl(i,kus-1+k,lan)-                            & 
     &                    synm(i,kuum-1+k,lan))/dt2
      syn(i,kvv-1+k,lan)=(anl(i,kvs-1+k,lan)-                            &
     &                    synm(i,kvvm-1+k,lan))/dt2
#ifdef NONHYD
      snn(i,kntt-1+k,lan)=(anl(i,kts-1+k,lan)-synm(i,ktt-1+k,lan))/dt2
      snn(i,knpp-1+k,lan)=(ann(i,knppa-1+k,lan)-snn(i,knpp-1+k,lan))/dt2
#else
      syn(i,ktt-1+k,lan)= (anl(i,kts-1+k,lan)-synm(i,kttm-1+k,lan))      &
     &                    /dt2
#endif
      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
      syn(i,kqq-1+k,lan)=(anl(i,kqs-1+k,lan)-synm(i,kqqm-1+k,lan))       &
     &                  /dt2
      enddo
      enddo
#ifdef NONHYD
      do k=1,levr+1
      do i=1,lonlens
      snn(i,knww-1+k,lan)=(ann(i,knwwa-1+k,lan)-snn(i,knww-1+k,lan))/dt2
      enddo
      enddo
#endif
!
!       print *,'total forcing',syn(5,kuu,lan),
!     & syn(5,kvv,lan),syn(5,ktt,lan),syn(5,kqq,lan),
!     & syn(5,kpp,lan)

!
#ifdef RKN
      do k=1,levr
      do i=1,lonlens
        gu0(i,k,lan)=anl(i,kus-1+k,lan)
        gv0(i,k,lan)=anl(i,kvs-1+k,lan)
        gt0(i,k,lan)=anl(i,kts-1+k,lan)
      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
        gq0(i,k,lan)=anl(i,kqs-1+k,lan)
      enddo
      enddo
#endif
!
!colse phys
 2050 continue
!c radiation

!cjun print out spdlat
!......................................................

!  ken point diagnostics
!
#ifdef RKN
!
       if(isave.ne.0.and.npoint.gt.0) then
         lon1=1
         lon2=lonlens
         latt1=latt+lat1
         latt2=latt+lat2
!
         call rkenputr(igrd1s,ncpuss,lon1s,lon2s,                        & 
     &        latt1,latt2,sdec,                                          &
     &        slmsk(1,lat1s),tser(1,lat1s),                              &
     &        flwupf(1,1),fswupf(1,1),fswdnf(1,1),                       &
     &        sswupf(1,1),sswdnf(1,1),slwupf(1,1),slwdnf(1,1),           &
     &        cldsaf(1,1,1),mtopaf(1,1,1),mbotaf(1,1,1),                 &
     &        itimsw,itimlw)
!
         call rkenput(igrd1,ncpus,lon1,lon2,                             &
     &        latt1,latt2,rcl,                                           &
     &        slmsk(1,lat1),psexp(1,1),tg3(1,lat1),sheleg(1,lat1),       &
     &        radsl(1,1),dlwsf1(1,1),                                    &
     &        tsea(1,lat1),qss(1,1),gflx(1,1),                           &
     &        zorl(1,lat1),cd(1,1),cdq(1,1),                             &
     &        evap(1,1),hflx(1,1),stsoil(1,1,1),                         &
     &        canopy(1,lat1),drain(1,1),smsoil(1,1,1),                   &
     &        runof(1,1),cld1d(1,1),                                     &
     &        u10m(1,lat1),v10m(1,lat1),t2m(1,lat1),q2m(1,lat1),         &
     &        hpbl(1,lat1),gamt(1,1),gamq(1,1),                          &
     &        dqsfc1(1,1),dtsfc1(1,1),dusfc1(1,1),dvsfc1(1,1),           &
     &        dusfcg(1,1),dvsfcg(1,1),                                   &
     &        rainc(1,1),rainl(1,1),                                     &
     &        gu0(1,1,1),gv0(1,1,1),                                     &
     &        gt0(1,1,1),gq0(1,1,1),                                     &
     &        hsw(1,1,1),hlw(1,1,lat1),vvel(1,1,1),sl(1),                &!binbin add sl(1) here
     &        vfrac(1,lat1),tmpmin(1,lat1),tmpmax(1,lat1),               &
     &        bengsh(1,lat1),geshem(1,lat1))                             
!
       endif
!
#endif
      do 2070 lat=lat1,lat2
      lan=lat-latdon
      latt=lat+latt
      latj = (lat-1)*igrd1
!cc
#ifdef A
      do k=1,levr
      do i=1,lonlens
        j=i+latj
#endif
#ifdef T
        wt(j,k,1)=at(i,k)
        wt(j,k,2)=bt(i,k)
        wt(j,k,3)=ct(i,k)
#endif
#ifdef U
        wu(j,k,1)=au(i,k)
        wu(j,k,2)=bu(i,k)
        wu(j,k,3)=fu(i,k)
        wu(j,k,4)=pu(i,k)
#endif
#ifdef V
        wv(j,k,1)=av(i,k)
        wv(j,k,2)=bv(i,k)
        wv(j,k,3)=fv(i,k)
        wv(j,k,4)=pv(i,k)
#endif
#ifdef A
      enddo
      enddo
      do nc=1,ntotal
      do k=1,levr
      do i=1,lonlens
        j=i+latj
#endif
#ifdef Q
        wq(j,k,1)=aq(i,k,nc)
        wq(j,k,2)=bq(i,k,nc)
#endif
#ifdef A
      enddo
      enddo
      enddo
#endif
#ifdef P
      do i=1,lonlens
        j=i+latj
        wp(j,1,1)=ap(i)
        wp(j,1,2)=bp(i)
      enddo
#endif
!ccjun
#ifdef A
      do k=1,levr
      do i=1,lonlens
      j=i+latj
#endif
#ifdef T
      wt(j,k,13)=syn(i,ktt-1+k,lan)-bgd(i,ktlb-1+k,lan)
#endif
#ifdef U
      wu(j,k, 9)=syn(i,kuu-1+k,lan)-bgd(i,kulb-1+k,lan)
#endif
#ifdef V
      wv(j,k, 9)=syn(i,kvv-1+k,lan)-bgd(i,kvlb-1+k,lan)
#endif
#ifdef A
      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
      j=i+latj
#endif
#ifdef Q
      wq(j,k, 8)=syn(i,kqq-1+k,lan)-bgd(i,kqlb-1+k,lan)
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef P
      do i=1,lonlens
      j=i+latj
      wp(j,1, 4)=syn(i,kpp,lan)-bgd(i,kplb,lan)
      enddo
#endif
!
! ......... adding relaxation and damping to total forcing
! ......... compute perturbation forcing by (full - base) forcing
!
      do i=1,lonlens
        syn(i,kpp,lan)= syn(i,kpp,lan)-bgd(i,kplb,lan)                   &  
     &                 -syn(i,kpm,lan)*sltb(i+latj)/rlxmsec
      enddo
      do k=1,levr
      do i=1,lonlens
      syn(i,kuu-1+k,lan)= syn(i,kuu-1+k,lan)-bgd(i,kulb-1+k,lan)         &
     &                   -syn(i,kum-1+k,lan)*sltb(i+latj)/rlxmsec
      syn(i,kvv-1+k,lan)= syn(i,kvv-1+k,lan)-bgd(i,kvlb-1+k,lan)         &
     &                   -syn(i,kvm-1+k,lan)*sltb(i+latj)/rlxmsec
      syn(i,ktt-1+k,lan)= syn(i,ktt-1+k,lan)-bgd(i,ktlb-1+k,lan)         &
     &                   -syn(i,ktm-1+k,lan)*sltb(i+latj)/rlxhsec
      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
      syn(i,kqq-1+k,lan)= syn(i,kqq-1+k,lan)-bgd(i,kqlb-1+k,lan)         &
     &                   -syn(i,kqm-1+k,lan)*sltb(i+latj)/rlxhsec
      enddo
      enddo
!
      if( iimprlx.eq.1 ) then
        do i=1,lonlens
          rsltb=1./(1.+sltb(i+latj)*2*deltim/rlxmsec)
          syn(i,kpp,lan)= syn(i,kpp,lan)*rsltb
        enddo
        do k=1,levr
          do i=1,lonlens
            rsltb=1./(1.+sltb(i+latj)*2*deltim/rlxmsec)
            syn(i,kuu-1+k,lan)= syn(i,kuu-1+k,lan)*rsltb
            syn(i,kvv-1+k,lan)= syn(i,kvv-1+k,lan)*rsltb
#ifdef NONHYD
            snn(i,knpp-1+k,lan)= snn(i,knpp-1+k,lan)*rsltbh*rsltn
            snn(i,kntt-1+k,lan)= snn(i,kntt-1+k,lan)*rsltbh*rsltn
#else
            rsltb=1./(1.+sltb(i+latj)*2*deltim/rlxhsec)
            syn(i,ktt-1+k,lan)= syn(i,ktt-1+k,lan)*rsltb
#endif
          enddo
        enddo
        do k=1,levh
          do i=1,lonlens
            rsltb=1./(1.+sltb(i+latj)*2*deltim/rlxhsec)
            syn(i,kqq-1+k,lan)= syn(i,kqq-1+k,lan)*rsltb
          enddo
        enddo
!
!cc vertical relax
!
!        levt=levr-6
        do k=levt+1,levr
          do i=1,lonlens
            rsltb=(sl(levt)-sl(k))/(sl(levt)-sl(levr))/rlxzsec
            rsltn=1./(1.+2.*deltim*rsltb)
            syn(i,kuu-1+k,lan)= (syn(i,kuu-1+k,lan) -                    &  
     &                   rsltb * syn(i,kum-1+k,lan)) * rsltn
            syn(i,kvv-1+k,lan)= (syn(i,kvv-1+k,lan) -                    &
     &                   rsltb * syn(i,kvm-1+k,lan)) * rsltn
            syn(i,ktt-1+k,lan)= (syn(i,ktt-1+k,lan) -                    &
     &                   rsltb * syn(i,ktm-1+k,lan)) * rsltn
          enddo
        enddo
        do kc=1,ntotal
         nkkc=(kc-1)*levr
         do k=levt+1,levr
          do i=1,lonlens
            rsltb=(sl(levt)-sl(k))/(sl(levt)-sl(levr))/rlxzsec
            rsltn=1./(1.+2.*deltim*rsltb)
            syn(i,kqq-1+nkkc+k,lan)= (syn(i,kqq-1+nkkc+k,lan) -          &  
     &                   rsltb * syn(i,kqm-1+nkkc+k,lan)) * rsltn
          enddo
         enddo
        enddo
      endif
!
#ifdef A
      do k=1,levr
      do i=1,lonlens
      j=i+latj
#endif
#ifdef T
      wt(j,k,13)=syn(i,ktt-1+k,lan)-wt(j,k,13)
#endif
#ifdef U
      wu(j,k, 9)=syn(i,kuu-1+k,lan)-wu(j,k, 9)
#endif
#ifdef V
      wv(j,k, 9)=syn(i,kvv-1+k,lan)-wv(j,k, 9)
#endif
#ifdef T
      wt(j,k,14)=-wt(j,k,14)+syn(i,ktt-1+k,lan)
#endif
#ifdef U
      wu(j,k,10)=-wu(j,k,10)+syn(i,kuu-1+k,lan)
#endif
#ifdef V
      wv(j,k,10)=-wv(j,k,10)+syn(i,kvv-1+k,lan)
#endif
#ifdef A
      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
      j=i+latj
#endif
#ifdef Q
      wq(j,k, 8)=syn(i,kqq-1+k,lan)-wq(j,k, 8)
      wq(j,k, 9)=-wq(j,k, 9)+syn(i,kqq-1+k,lan)
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef P
      do i=1,lonlens
      j=i+latj
      wp(j,1, 4)=syn(i,kpp,lan)-wp(j,1, 4)
      wp(j,1, 5)=-wp(j,1, 5)+syn(i,kpp,lan)
      enddo
#endif
!
2070  continue
!...........................
!cj
#ifdef RKN
!
!  ken point - div and vort
!
      if(ilshrt.lt.1) then
        if(isave.ne.0.and.npoint.gt.0) then
         lon1=1
         lon2=lonlens
         latt1=latt+lat1
         latt2=latt+lat2
!
         do igpt=1,npoint
         if(jgpd(igpt).ge.latt1.and.jgpd(igpt).le.latt2) then
         if(igpd(igpt).ge.lon1.and.igpd(igpt).le.lon2) then
           jlat=jgpd(igpt)-latt1+1
           jlat1=jgpd(igpt)
           ilon=igpd(igpt)-lon1+1
           do k=1,levr
           svdata(k+rslvark+8*levr,igpt,itnum) =                         &  
     &        xm2(ilon,jlat1)*                                           &
     &       (udx(ilon,k,jlat)+vdy(ilon,k,jlat))
           svdata(k+rslvark+9*levr,igpt,itnum) =                         &
     &        xm2(ilon,jlat1)*                                           &
     &       (vdx(ilon,k,jlat)-udy(ilon,k,jlat))
           enddo
         endif
         endif
         enddo
        endif
      endif
#endif
!
      do 2080 lat=lat1,lat2
      lan=lat-latdon
!  the following separation of ffa depends on vertical resolution
      call ffancs( syn(1,kvv,lan), syn(1,kuu,lan), levr)
      call ffacos( syn(1,kpp,lan), levr+levh+1 )
#ifdef NONHYD
      call ffacosx1(snnyk,jgrd1,knpp,lotsnnk, levr*6+2 )
#endif
2080  continue
!
      do 3000 lat=lat1,lat2
      lan=lat-latdon
!.1. uu,vv
      call sumgcs1(syn(1,kuu,lan),syn(1,kvv,lan),x,w,                    &  
     &    gcosc(1,lat),gsinc(1,lat),                                     &
     &    igrd1,lnwav,iwav1,jwav1,igrd1,levr)
!.2. z,y,rt
      call sumggc1(syn(1,kpp,lan),z,gcosc(1,lat),                        &
     &    igrd1,lnwav,iwav1,jwav1,igrd1,levr+levh+1)
3000   continue
#ifdef NONHYD
!.3.
      call ffacosy1(snnxk,pa,knpp,igrd1,iwav1,lotsnnk,levr*3+1)
#endif
!..................................................................
      latdon=latdon+(lat2-lat1+1)
!     
10000 continue
!..................................................................
!
#ifdef T
      call cctogd(y,tmpbgt,levr)
      do k=1,levr
        do i=1,lngrd
          wt(i,k,14)=tmpbgt(i,k)-wt(i,k,14)
        enddo
      enddo
#endif
!
#ifdef Q
      call cctogd(rt,tmpbgt,levh)
      do k=1,levh
        do i=1,lngrd
          wq(i,k, 9)=tmpbgt(i,k)-wq(i,k, 9)
        enddo
      enddo
#endif
!
#ifdef U
      call sctogd(x,tmpbgt,levr)
      do k=1,levr
        do i=1,lngrd
          wu(i,k,10)=tmpbgt(i,k)-wu(i,k,10)
        enddo
      enddo
#endif
!
#ifdef V
      call cstogd(w,tmpbgt,levr)
      do k=1,levr
        do i=1,lngrd
          wv(i,k,10)=tmpbgt(i,k)-wv(i,k,10)
        enddo
      enddo
#endif
!
#ifdef P
      call cctogd(z,tmpbgt(1,1),1)
      do i=1,lngrd
        wp(i,1, 5)=tmpbgt(i,1)-wp(i,1, 5)
      enddo
#endif
!
!$omp parallel do private(k,lat)
      do k=1,levr
      spdmax(k) = 0.0
       do lat=1,jgrd1
        spdmax(k)=max(spdmax(k),spdlat(k,lat))
       enddo
      enddo
!
! update ww at top for n time step (see nfidi)
!
#ifdef NONHYD
      do j=1,lnwavp
        on(j,levr+1)=onm(j,levr+1)+deltim*o(j,levr+1)
      enddo
#endif
!......................................................
      if(me.eq.0) print 100,(spdmax(k),k=1,levr)
100   format(' rsm spdmx(01:10)=',10f5.0,/' rsm spdmx(11:20)=',10f5.0,   &  
     &     /' rsm spdmx(21:30)=',10f5.0,/' rsm spdmx(31:40)=',10f5.0,    &
     &     /' rsm spdmx(41:50)=',10f5.0,/' rsm spdmx(51:60)=',10f5.0,    &
     &     /' rsm spdmx(61:70)=',10f5.0,/' rsm spdmx(71:80)=',10f5.0,    &
     &     /' rsm spdmx(81:90)=',10f5.0,/' rsm spdmx(91:00)=',10f5.0)
!
      return
      end

