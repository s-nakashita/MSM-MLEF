      subroutine gsm2bgd(gz,q,te,di,ze,rq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsm2rsm
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do global to regional grids transformation.
!
! program history log:
!
! usage:    call gsm2rsm(gz,q,te,di,ze,rq)
!   input argument list:
!     gz  - global coefficient of terrain height.
!     q   - global coefficient of log surface pressure.
!     te  - global coefficient of virtual temperature.
!     di  - global coefficient of divergence.
!     ze  - global coefficient of vorticity.
!     rq  - global coefficient of specific humidity.
!
!   output argument list:
!
!   common block:
! rscomltb
! regin1
! repro
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! dztouv  maxmin  pln2i  sums2i  sumtop  fti_lonf  flpflm
! sphpt1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!
!$$$
!.....
! ..... this routine uses less memory but more time spends
!... routine get gsm coef to rsm coef and rsm grid for incode nested model
!    by hann-ming henry juang (oct 1991)
!....
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
#ifdef CRA
#include <rscomcpu.h>
#else
#include <ncpu.h>
#endif
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#include <commpi.h>
#include <commpildef.h>
#endif
#include <rscomver.h>
#include <rscomltb.h>
!...sof include..........................................
!...
#ifdef MP
#define JCAP1S jcap1p
#define LEVSS  levsp
#define LNT22S lnt22p
#define LNT2S  lnt2p
#define LLN22S  lln22p
#define LLN2S  lln2p
#define LCAPS lcapp
#define LCAP22S lcap22p
#define LATG2S latg2p
#else
#define JCAP1S jcap1
#define LEVSS  levs
#define LNT22S lnt22
#define LNT2S  lnt2
#define LLN22S lnt22
#define LLN2S  lnt2
#define LCAPS twoj1
#define LCAP22S lonf22
#define LATG2S latg2
#endif
!....
      dimension gz(LNT22S), q(LNT22S)                                   &
     &  , te(LNT22S,levs), rq(LNT22S,levs,ntotal)                       &
     &  , di(LNT22S,levs), ze(LNT22S,levs)
#ifdef MP
      real, allocatable :: dia(:,:),zea(:,:)
#endif
!
      common /regin1/ gm(LLN22S), xm(lngrdb)                            &
     &  , csln(lonf2), snln(lonf2), epsi(jcap2,jcap1)                   &
     &  , colrad(latg2), wgt(latg2)                                     &
     &  , rcslb(lngrdb), rsnlb(lngrdb)                                  &
     &  , rlatb(lngrdb),rlonb(lngrdb)                                   &
#ifndef MP
     &  ,lwvdef(jcap+1),latdef(latg/2)                                  &
#endif
     &  , tmpa! end regin1 
!....
#ifdef MP
#define NCPUSS latg2
#else
#define NCPUSS ncpus
#endif
      parameter(lotsts=1+2*LEVSS)
      parameter(lotss=lotsts*2)
      real, allocatable :: syn(:,:,:)
      real, allocatable :: flp(:,:,:,:),flm(:,:,:,:)
      real, allocatable :: uvtop(:,:,:)
#ifdef MP
      real, allocatable :: syf(:,:,:)
#endif
!
      common /reprogr1/qtt(LLN2S,latg2),                                  &
     &         qvv(LCAPS,latg2),qww(LLN2S,latg2)
!
      common /reprogb/                                                    &
#ifdef SPT
     &    ggz(LNT22S),       gq(LNT22S),                                  &
#endif
     &    uu(LNT22S,levs), vv(LNT22S,levs)                                &
#ifdef SPT
     &   ,tt(LNT22S,levs), qq(LNT22S,levs,ntotal)                         &
#endif
     &   ,uln(LLN22S,LEVSS),vln(LLN22S,LEVSS),ggm(LLN22S)                 &
#ifdef MP
     &   ,uua(LLN22S,LEVSS), vva(LLN22S,LEVSS)                            &
#endif
     &  ,tmpb  !end comm
!.................................................................
      save numprt
      data numprt/0/
!.................................................................
#ifdef MP
      allocate (dia(LLN22S,LEVSS))
      allocate (zea(LLN22S,LEVSS))
      allocate (syf(lonf22,lotss,latg2p))
#endif
      allocate (syn(LCAP22S,lotss,NCPUSS))
      allocate (flp(2,JCAP1S,LEVSS*2,NCPUSS))
      allocate (flm(2,JCAP1S,LEVSS*2,NCPUSS))
      allocate (uvtop(2,JCAP1S,lotsts))
!
#ifdef MP
      llstr=glwvstr(mype)
      llens=glwvlen(mype)
      jstr=glatstr(mype)
      jend=glatstr(mype)+glatlen(mype)-1
#else
      llstr=0
      llens=jcap1
#endif
!
#ifdef MP
      call mpnn2nk(di,lnt22p,levs, dia,lln22p,levsp,1)
      call mpnn2nk(ze,lnt22p,levs, zea,lln22p,levsp,1)
#define DIS dia
#define ZES zea
#else
#define DIS di
#define ZES ze
#endif
      call dzuvlemp(DIS,ZES,uln,vln,uvtop(1,1,1),                       &
     &     uvtop(1,1,LEVSS+1),llstr,llens,lwvdef)
!
!x    do 60 k=1,levs
!x    call dztouv( di(1,k), ze(1,k), uln(1,k), vln(1,k),                &
!x   &    utop(1,1,k), vtop(1,1,k))
!x60  continue
!
#ifdef ORIGIN_THREAD
!$doacross share(uu,vv),local(j,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(uu,vv)
!mic$1 private(j,k)
#endif
#ifdef OPENMP
!$omp parallel do private(j,k)
#endif
!
#ifdef MP
#define UUS uua
#define VVS vva
#else
#define UUS uu
#define VVS vv
#endif
      do 70 k=1,LEVSS
      do 70 j=1,LLN22S
        UUS(j,k) = 0.0
        VVS(j,k) = 0.0
  70  continue
!
      do j=1,LLN22S
        ggm(j) = gm(j)
      enddo
!
#undef LLN22S
#undef ZES
#undef DIS
!
!#ifdef MP
!      if (mype .eq. master) then
!#endif
!      call maxmin(ggm,LLN2S,1,1,1,'ggm global coef')
!      call maxmin(uln,LLN2S,LEVSS,1,LEVSS,'uln global coef')
!      call maxmin(vln,LLN2S,LEVSS,1,LEVSS,'vln global coef')
!#ifdef MP
!      endif
!#endif
!
!..........................................................
!           lat loop
! compute lat band limits
!
#ifndef MP
      last=mod(latg2,ncpus)
      nggs=latg2/ncpus
      if(last.ne.0) nggs=nggs+1
      inclat=ncpus
      lat1=1-ncpus
      lat2=0
      latdon=0
!
      do 10000 ngg=1,nggs
      if((ngg.eq.nggs).and.(last.ne.0)) inclat=last
      lat1=lat1+ncpus
      lat2=lat2+inclat
#endif
!
#ifdef MP
      lat1=1
      lat2=latg2
      latdon=0
#endif
!
#ifdef ORIGIN_THREAD
!$doacross share(syntop,syn,qtt,qvv,lat1,lat2,latdon,
!$&        colrad,uln,llstr,llens,lwvdef)
!$&        local(lat,lan,llens)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(syntop,syn,qtt,qvv,lat1,lat2,latdon)
!mic$1 shared(colrad,uln,llstr,llens,lwvdef)
!mic$1 private(lat,lan,llens)
#endif
#ifdef OPENMP
!$omp parallel do private(lat,lan)
#endif
!
      do 1000 lat = lat1,lat2
      lan=lat-latdon
!
      call sums2imp(uln ,syn(1,       1 ,lan),qtt(1,lat),               &
     &            llstr,llens,lwvdef,lotsts,jcap                        &
#ifndef MP
     &           ,lonf,lnt2,lnt22                                       &
#else
     &           ,lcapp,lln2p,lln22p                                    &
#endif
     &            )
!x    call sums2i(vln ,syn(1,1*levs+1,lan),qtt(1,lan),levs)
!x    call sums2i(ggm  ,syn(1,2*levs+1,lan),qtt(1,lan),1)
      call sumtopmp(syn(1,1,lan),uvtop,qvv(1,lat),                      &
     &            llstr,llens,lwvdef,lotsts-1,jcap                      &
#ifndef MP
     &           ,lonf                                                  &
#else
     &           ,jcapp,lcapp                                           &
#endif
     &            )
!x    call sumtop(syn(1,levs+1,lan),vtop,qvv(1,lan),                    &
!x   &         levs,lonf,lonf/2)
1000  continue
!
#ifdef MP
      call mpnl2ny(jcapp,lonf,syn,lcap22p,latg2,                        &
     &             syf,lonf22,latg2p,lotss,1,lotsts,                    &
     &             lwvdef,latdef,jcap,latg)
      lat1=jstr
      lat2=jend
      latdon=jstr-1
#define SYNS syf
#else
#define SYNS syn
#endif
!
#ifdef ORIGIN_THREAD
!$doacross share(syf,syn,lat1,lat2,latdon),
!$&        local(lat,lan)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$& shared(syf,syn)
!mic$& shared(lat1,lat2,latdon)
!mic$& private(lat,lan)
#endif
#ifdef OPENMP
!$omp parallel do private(lat,lan,dummy,coslat,rcslt)
#endif
      do 1200 lat=lat1,lat2
      lan=lat-latdon
!
      call fti_lonf (SYNS(1,1,lan),dummy,lotss,1)
!
      coslat = cos( pi * 0.5 - colrad(latdef(lat)) )
      rcslt = 0.0
      if( coslat.ne.0.0 ) rcslt = 1. / coslat
      do 170 j=1,lonf2
      SYNS(j, LEVSS*2+1,lan) = SYNS(j, LEVSS*2+1,lan) * rcslt
  170 continue
!
      do 173 k=1,LEVSS
      do 173 j=1,lonf2
      SYNS(j,     k ,lan) = SYNS(j,     k,lan)                          &
     &                    * SYNS(j,LEVSS*2+1,lan)
      SYNS(j,LEVSS+k,lan)= SYNS(j,LEVSS+k,lan)                          &
     &                    * SYNS(j,LEVSS*2+1,lan)
  173 continue
      do 175 k=1,LEVSS
      do 175 j=1,lonf2
      SYNS(j,2*LEVSS+k,lan)= -SYNS(j,      k,lan)*snln(j)               &
     &                       -SYNS(j,LEVSS+k,lan)*csln(j)
      SYNS(j,3*LEVSS+k,lan)=  SYNS(j,      k,lan)*csln(j)               &
     &                       -SYNS(j,LEVSS+k,lan)*snln(j)
  175 continue
!
      call fti_lonf (SYNS(1,2*LEVSS+1,lan),SYNS(1,1,lan)                &
     &               ,(2*LEVSS)*2,-1)
!
1200  continue
!
#ifdef MP
      call mpny2nl(jcapp,lonf,syf,lonf22 ,latg2p,                       &
     &             syn,lcap22p,latg2 ,lotss,1,2*LEVSS,                  &
     &             lwvdef,latdef,jcap,latg)
!
      lat1=1
      lat2=latg2
      latdon=0
#endif
!
#ifdef ORIGIN_THREAD
!$doacross share(syn,flp,flm,lat1,lat2,latdon),
!$&        local(lan,lat)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(syn,flp,flm)
!mic$1 shared(lat1,lat2,latdon)
!mic$1 private(i,lan,lat)
#endif
#ifdef OPENMP
!$omp parallel do private(lan,lat)
#endif
      do 1400 lat=lat1,lat2
      lan=lat-latdon
!
      call flpflm(flp(1,1,1,lan),flm(1,1,1,lan),syn(1,1,lan),           &
     &            llens,LEVSS*2                                         &
#ifndef MP
     &           ,jcap1,lonf,lonf22                                     &
#else
     &           ,jcap1p,lcapp,lcap22p                                  &
#endif
     &            )
!
1400  continue
!
#ifdef ORIGIN_THREAD
!$doacross share(qww,uu,flp,flm,lat1,lat2,latdon),
!$&        local(lan,lat)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(qww,uu,flp,flm)
!mic$1 shared(lat1,lat2,latdon)
!mic$1 private(i,lan,lat)
#endif
#ifdef OPENMP
!$omp parallel do private(lan,lat)
#endif
      do 1500 lat=lat1,lat2
      lan=lat-latdon
!
      call fl2imp(flp(1,1,1,lan),flm(1,1,1,lan),UUS,qww(1,lat),         &
     &            llstr,llens,lwvdef,LEVSS*2,jcap                       &
#ifndef MP
     &           ,lnt2,lnt22                                            &
#else
     &           ,jcapp,lln2p,lln22p                                    &
#endif
     &            )
!
 1500 continue
!
#ifndef MP
      latdon=latdon+(lat2-lat1+1)
10000 continue
#endif
!
#ifdef MP
      call mpnk2nn(uua,lln22p,levsp, uu,lnt22p,levs,2)
#endif
!
!#ifdef MP
!      if (mype .eq. master) then
!#endif
!      call maxmin(uu,LNT2S,levs,1,levs,'u/m global coef')
!      call maxmin(vv,LNT2S,levs,1,levs,'v/m global coef')
!#ifdef MP
!      endif
!#endif
!
#ifdef SPT
      do 2010 n=1,LNT2S
      ggz(n)=gz(n)
       gq(n)= q(n)
 2010 continue
!ccj
      do 2020 k=1,levs
      do 2020 n=1,LNT2S
      tt(n,k) = te(n,k)
 2020 continue
      do 2030 kc=1,ntotal
      do 2030 k=1,levs
      do 2030 n=1,LNT2S
      qq(n,k,kc) = rq(n,k,kc)
 2030 continue
!     
! .......................................................
! last loop get all variables and its derivations
!
! obtain gzb qltb uultb vvltb teltb rqltb
      length=2+3*levs+levs*ntotal
      call sphpt1(length,ggz,gzb,rlatb,rlonb,lngrdb)
#endif
!
#ifdef SQK
!-sqk length=2+3*levs+levs*ntotal
!-sqk call sphqk1mp(length,gz,gzb,lwvdef,latdef,lngrdb)
      call sphqk1mp(    1,gz, gzb,lwvdef,latdef,lngrdb)
      call sphqk1mp(    1, q, qltb,lwvdef,latdef,lngrdb)
      call sphqk1mp(levs, uu,uultb,lwvdef,latdef,lngrdb)
      call sphqk1mp(levs, vv,vvltb,lwvdef,latdef,lngrdb)
      call sphqk1mp(levs, te,teltb,lwvdef,latdef,lngrdb)
!ccj
      do kc = 1,ntotal
        call sphqk1mp(levs,rq(1,1,kc),rqltb(1,1,kc),                    &
     &                lwvdef,latdef,lngrdb)
      enddo
!cc
#endif
!
!testjun      if( numprt .lt. 0 ) then
!
!#ifdef MP
!      if (mype .eq. master) then
!#endif
!      call maxmin(gzb,lngrdb,1,1,1,'gz base')
!      call maxmin( qltb,lngrdb,1,1,1,' q base')
!      call maxmin(teltb,lngrdb,levs,1,levs,'te base')
!      call maxmin(rqltb(1,1,1),lngrdb,levs,1,levs,'rq base')
!      if(ntoz.eq.1)                                                     &
!     & call maxmin(rqltb(1,1,2),lngrdb,levs,1,levs,'oz base')
!      if(ncld.eq.1)                                                     &
!     & call maxmin(rqltb(1,1,3),lngrdb,levs,1,levs,'cw base')
!      call maxmin(uultb,lngrdb,levs,1,levs,'uu base')
!      call maxmin(vvltb,lngrdb,levs,1,levs,'vv base')
!#ifdef MP
!      endif
!#endif
!
      numprt = numprt + 1
!testjun      endif
! .......................................................
#ifdef MP
      deallocate (dia)
      deallocate (zea)
      deallocate (syf)
#endif
      deallocate (syn)
      deallocate (flp)
      deallocate (flm)
      deallocate (uvtop)
!
!#ifdef MP
!      print *, 'mype=',mype,'finished gsm2bgd'
!#endif
      return
      end
