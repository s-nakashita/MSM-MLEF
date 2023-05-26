#include <define.h>
       subroutine rloopmp
#ifdef MP
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
        use physcons, pi => con_pi
        use physics_module
        use radiation_module
        use cnttime
!c
#include <paramodel.h>
#include <ncpu.h>
#include <rscomcpu.h>
#include <npesi.h>
#include <comrmpi.h>
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
!ifdef AFA
!     parameter(ncpus = ncpu )
!endif
#ifdef T
      dimension at(igrd1p,levr),bt(igrd1p,levr),ct(igrd1p,levr)
#endif
#ifdef Q
      dimension aq(igrd1p,levr,ntotal),bq(igrd1p,levr,ntotal)
#endif
#ifdef U
      dimension au(igrd1p,levr),bu(igrd1p,levr),fu(igrd1p,levr)
     &         ,pu(igrd1p,levr)
#endif
#ifdef V
      dimension av(igrd1p,levr),bv(igrd1p,levr),fv(igrd1p,levr)
     &         ,pv(igrd1p,levr)
#endif
#ifdef P
      dimension ap(igrd1p),bp(igrd1p)
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
      parameter(numcsp =1+levrp*5+levhp, numcs2p=numcsp*2)
      parameter(numcosp=1+levrp*1+levhp, numcos2p=numcosp*2)
      parameter( kpyp=1         , kpxp=numcsp+kpyp,                      &
     &           kvvp=1    +kpyp, kuup=numcsp+kvvp,                      &
     &           kvmp=levrp+kvvp, kump=numcsp+kvmp,                      &
     &           ktyp=levrp+kvmp, ktxp=numcsp+ktyp,                      &
     &           kqyp=levrp+ktyp, kqxp=numcsp+kqyp,                      &
     &           kvyp=levhp+kqyp, kvxp=numcsp+kvyp,                      &
     &           kuxp=levrp+kvyp, kuyp=numcsp+kuxp )
      parameter( kppp=numcs2p+1,                                         &
     &           kttp=1     +kppp,                                       &
     &           kqqp=levrp+kttp,                                        &
     &           ktmp=levhp+kqqp,                                        &
     &           kpmp=levrp+ktmp,                                        &
     &           kqmp=1    +kpmp )
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
!!
#ifdef NONHYD
      parameter( noncs=1+levr*3,noncs2=noncs*2,                          &  
     &           locncs=1,                                               &
     &           knpy=locncs   ,   knpx=noncs+knpy,                      &
     &           knty=knpy+levr,   kntx=noncs+knty,                      &
     &           knwy=knty+levr,   knwx=noncs+knwy,                      &
     &           noncos=1+levr*3,noncos2=noncos*2,                       &
     &           locncos=locncs+noncs2,                                  &
     &           knpp=locncos   ,  knpm=noncos+knpp,                    &
     &           kntt=knpp+levr,   kntm=noncos+kntt,                     &
     &           knww=kntt+levr,   knwm=noncos+knww)
      parameter( knppa=1,                                                &
     &           kntta=knppa+levr,                                       &
     &           knwwa=kntta+levr )
      parameter( knppm=1,                                                &
     &           knttm=knppm+levr,                                       &
     &           knwwm=knttm+levr )
!for mpi
      parameter( noncsp=1+levrp*3,noncs2p=noncsp*2,                      &
     &           locncsp=1,                                              &
     &           knpyp=locncsp   ,    knpxp=noncsp+knpyp,                &
     &           kntyp=knpyp+levrp,   kntxp=noncsp+kntyp,                &
     &           knwyp=kntyp+levrp,   knwxp=noncsp+knwyp,                &
     &           noncosp=1+levrp*3,noncos2p=noncosp*2,                   &
     &           locncosp=locncsp+noncs2p,                               &
     &           knppp=locncosp   ,   knpmp=noncosp+knppp,               &
     &           knttp=knppp+levrp,   kntmp=noncosp+knttp,               &
     &           knwwp=knttp+levrp,   knwmp=noncosp+knwwp)
      parameter(lotsnnk=noncs2p+noncos2p)
      parameter(lotannk=levrp*3+1)
      dimension snnxk(igrd1p,lotsnnk,jgrd1)
      dimension snnyk(igrd1,lotsnnk,jgrd1p)
      dimension annxk(igrd1p,lotannk,jgrd1)
      dimension annyk(igrd1, lotannk,jgrd1p)

!<<
      parameter(lotsnn=noncs2+noncos2)
      parameter(lotann=levr*3+1)
      dimension snn(igrd1p,lotsnn,jgrd1p)
      dimension snnm(igrd1p,lotann,jgrd1p)
      dimension ann(igrd1p,lotann,jgrd1p)
#endif

!jun
      parameter(kus=1,                                                   &
     &          kvs=levr+kus,                                            &
     &          kts=levr+kvs,                                            &
     &          kqs=levr+kts)
      parameter(kpyr=1,        kvvr=1    +kpyr,                          & !cos
     &          kpxr=levr+kvvr,kuur=1    +kpxr,                          & !sin
     &          kddr=levr+kuur,                                          &
     &          kppr=levr+kddr,                                          &
     &          kttr=1   +kppr,                                          &
     &          kqqr=levr+kttr )
      parameter(kuum=1,                                                  &
     &          kvvm=levr+kuum,                                          &
     &          kttm=levr+kvvm,                                          &
     &          kppm=levr+kttm,                                          &
     &          kqqm=1   +kppm)

      parameter(                                                         &
     &          kusp=1,                                                  &
     &          kvsp=levrp+kusp,                                         &
     &          ktsp=levrp+kvsp,                                         &
     &          kqsp=levrp+ktsp)
!c
      parameter(lotsyn=numcs2+numcos2)
      parameter(lotsynr=3+4*levr+levh)
      parameter(lotsynm=1+3*levr+levh)
      dimension syn(igrd1p,lotsyn,jgrd1p)
      dimension synr(igrd1p,lotsynr,jgrd1p)
      dimension synm(igrd1p,lotsynm,jgrd1p)
      dimension syndd(igrd1p,levr,jgrd1p)
      dimension bgd(igrd1p,numbgd*4,jgrd1p)
      parameter(lotsynk=numcs2p+numcos2p)
      dimension syfk(igrd1p,lotsynk,jgrd1)
      dimension synk(igrd1,lotsynk,jgrd1p)
!
      common/reproa/                                                     &  
     &  tex(lnwavp,levr)                                                 &! cos in y direction
     &, rqx(lnwavp,levr,ntotal)                                          &
     &, vvy(lnwavp,levr)                                                 &
     &, uux(lnwavp,levr)                                                 &
     &, tey(lnwavp,levr)                                                 &! sin in y direction
     &, rqy(lnwavp,levr,ntotal)                                          &
     &, vvx(lnwavp,levr)                                                 &
     &, uuy(lnwavp,levr)                                                 &
     &, texk(llwavp,levrp)                                               &! cos in y direction
     &, rqxk(llwavp,levrp,ntotal)                                        &
     &, vvyk(llwavp,levrp)                                               &
     &, uuxk(llwavp,levrp)                                               &
     &, teyk(llwavp,levrp)                                               &! sin in y direction
     &, rqyk(llwavp,levrp,ntotal)                                        &
     &, vvxk(llwavp,levrp)                                               &
     &, uuyk(llwavp,levrp)
!
#ifdef NONHYD
      common/repronon/                                                   &
     &  pnx(lnwavp,levr)                                                 &
     &, tnx(lnwavp,levr)                                                 &
     &, onx(lnwavp,levrp1)                                               &
     &, pny(lnwavp,levr)                                                 &
     &, tny(lnwavp,levr)                                                 &
     &, ony(lnwavp,levrp1)                                               &!mpi
     &, pnxk(llwavp,levrp)                                               &
     &, tnxk(llwavp,levrp)                                               &
     &, onxk(llwavp,levrp1p)                                             &
     &, pnyk(llwavp,levrp)                                               &
     &, tnyk(llwavp,levrp)                                               &
     &, onyk(llwavp,levrp1p)                                                   
#endif

      parameter(lotanl=levr*3+levh)
      parameter(lotbgd=levr*7+levh+3)
      dimension anl(igrd1p,lotanl, jgrd1p)
      dimension ze(lnwavp,levr)
!
#ifdef RKN
       dimension                                                         &
     & udx(igrd1p,levr,jgrd1p),udy(igrd1p,levr,jgrd1p),                  &
     & vdx(igrd1p,levr,jgrd1p),vdy(igrd1p,levr,jgrd1p)
!radiation_run
      real (kind=kind_rad) flwupf(igrd1p,jgrd1p),                        &
     &     fswupf(igrd1p,jgrd1p),fswdnf(igrd1p,jgrd1p),                  &
     &     sswupf(igrd1p,jgrd1p),sswdnf(igrd1p,jgrd1p),                  &
     &     slwupf(igrd1p,jgrd1p),slwdnf(igrd1p,jgrd1p),                  &
     &     cldsaf(igrd1p,5,jgrd1p)
      integer mtopaf(igrd1p,3,jgrd1p),mbotaf(igrd1p,3,jgrd1p)
!physics_run
       dimension psexp(igrd1p,jgrd1p),radsl(igrd1p,jgrd1p),              &
     &  dlwsf1(igrd1p,jgrd1p),qss(igrd1p,jgrd1p),                        &
     &  gflx(igrd1p,jgrd1p),cd(igrd1p,jgrd1p),                           &
     &  cdq(igrd1p,jgrd1p),evap(igrd1p,jgrd1p),                          &
     &  hflx(igrd1p,jgrd1p),drain(igrd1p,jgrd1p),                        &
     &  runof(igrd1p,jgrd1p),cld1d(igrd1p,jgrd1p),                       &
     &  gamt(igrd1p,jgrd1p),gamq(igrd1p,jgrd1p),                         &
     &  dqsfc1(igrd1p,jgrd1p),dtsfc1(igrd1p,jgrd1p),                     &
     &  dusfc1(igrd1p,jgrd1p),dvsfc1(igrd1p,jgrd1p),                     &
     &  dusfcg(igrd1p,jgrd1p),dvsfcg(igrd1p,jgrd1p),                     &
     &  rainc(igrd1p,jgrd1p),rainl(igrd1p,jgrd1p),                       &
     &  hsw(igrd1p,levr,jgrd1p),vvel(igrd1p,levr,jgrd1p),                &
     &  stsoil(igrd1p,lsoil,jgrd1p),smsoil(igrd1p,lsoil,jgrd1p),         &
     &  gu0(igrd1p,levr,jgrd1p),gv0(igrd1p,levr,jgrd1p),                 &
     &  gt0(igrd1p,levr,jgrd1p),gq0(igrd1p,levh,jgrd1p)
#endif
!...........................................................
!...
      common/rcomdhc/spdlat(levr,jgrd1p),sm(lnwavp)
!jun
!      dimension tmp(lnwav),tmp1(igrd1p,jgrd1p),                      &    
!    &   tmpg(lngrd),tmpsk(lnwav,levr+1)
!      real(kind=4) atmp(lnwav),atmp1(igrd1p,jgrd1p),                 &
!    &   atmpg(lngrd),atmpsk(lnwav,levr)
!     dimension tmpu(IGRD1S,jgrd1p),tmpv(IGRD1S,jgrd1p)
!     dimension tmpw(IGRD1S,jgrd1p),tmpwork(igrd1,jgrd1)
!jun- radiation
       integer itimsw,itimlw, kk
! zyf test
!     dimension zyftmp(lnwavp,levr),zyfgrid(lngrdp,levr)
!     dimension zyffull(lngrd,levr)

!.................................................................
!
      me=mype
      lonlens=lonlen(mype)
      latlens=latlen(mype)
      lonstrs=lonstr(mype)
      lat1s=1
      lat2s=latlen(mype)
      latts=latstr(mype)-1
!c
!     if(me.eq.0) print *,' rloopmp start'
!............................................................
! ---------------- q   dqdx    dqdy   d dx  d dy   ---------
      call dellcc(lnwavp,q,dpdlam,dpdphi,epsx,epsy,1)
      call dellsc(lnwavp,uu,uux,uuy,epsx,epsy,levr)
      call dellcs(lnwavp,vv,vvx,vvy,epsx,epsy,levr)
      call dellcc(lnwavp,te,tex,tey,epsx,epsy,levr+levh)
#ifdef NONHYD
      call dellcc(lnwavp,pn,pnx,pny,epsx,epsy,levr*3+1)
#endif
!              tmp=0.
!      call rmpsp2f(on,lnwavp,tmpsk,lnwav,levr)
!      if (mype.eq.master) then
!        call maxmin(tmpsk,lnwav,levr,1,levr,'rloopmp wn sp')
!      endif

!
       if (lsphy) then
       call physics_init(me,lonlens,latlens,lngrdp,                      &  
     &   num_p3d,num_p2d,levr,levh,levrp1,jgrd1p,                        &
     &   igrd1p,jgrd1p,igrd1,jgrd1,fhour,fhswr,phour,lsoil,lsfwd,        &
     &   deltim,lscca,lsswr,                                             &
     &   si(1),idate(1),phy_f3d(1,1,1,1),phy_f2d(1,1,1))
!
       call radiation_init(me,lonlens,lonstrs,latlens,levr,levrp1,       &
     &   levh,jgrd1p,lat1s,lat2s,latts,l,lp1,lp1v,                       &
     &   igrd1p,jgrd1p,ncpu,ntotal,                                      &
     &   lnwavp,ncld,nvect,num_p3d,nmtvr,                                &
     &   fhour,phour,fhswr,fhlwr,deltim,                                 &
     &   lsswr,lslwr,lssav,lw_type,ntoz,                                 &
     &   si,sl,idate,rlat,rlon,                                          &
     &   sinlar,coslar,itimsw,itimlw)
       endif

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
       if (lsphy) then
        call physics_prep(ozprd(1,1,1),ozdis(1,1,1),                     &
     &    ozprdout(1,1),ozdisout(1,1),solhr,clstp)
!        print *,'in rloopmp,ozprd=',ozprd(1,1,1),ozprd(1,1,lev46)
!
        if(itimsw.ne.0 .or. itimlw.ne.0) then
          call radiation_prep(sdec,cdec,slag,q,uu,te,vv,del,rq,          &
     &      tsea,sheleg,coszer,hprime(1,1,1),stc(1,1,1),                 &
     &      sinlar,coslar)
        endif
       endif
!ccjune
!
      uua=0.
      vva=0.
      tea=0.
      rqa=0.
      xa=0.
      ya=0.
      wa=0.
      rta=0.
      za=0.
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
! first lat loop
!
#ifdef CNT_1
        call cnttime_on("one_loop_spectal")
#endif
!
       call rmpnn2n(q,lnwavp,qa,llwavp,1)
       call rmpnn2n(dpdlam,lnwavp,dpdlama,llwavp,1)
       call rmpnn2n(dpdphi,lnwavp,dpdphia,llwavp,1)
       call rmpnn2n(qm,lnwavp,qma,llwavp,1)
       call rmpnn2nk(uu,lnwavp,levr,uua,llwavp,levrp,2)
       call rmpnn2nk(vv,lnwavp,levr,vva,llwavp,levrp,2)
       call rmpnn2nk(te,lnwavp,levr,tea,llwavp,levrp,2+ntotal)
       call rmpnn2nk(rm,lnwavp,levr,rma,llwavp,levrp,ntotal)
       call rmpnn2nk(tex,lnwavp,levr,texk,llwavp,levrp,6+2*ntotal)
#ifdef NONHYD
       call rmpnn2nk(pnx,lnwavp,levr,pnxk,llwavp,levrp,3)
       call rmpnn2n (onx(1,levrp1),lnwavp,onxk(1,levrp1p),llwavp,1)
       call rmpnn2nk(pny,lnwavp,levr,pnyk,llwavp,levrp,3)
       call rmpnn2n (ony(1,levrp1),lnwavp,onyk(1,levrp1p),llwavp,1)
       call rmpnn2nk(pn,lnwavp,levr,pna,llwavp,levrp,3)
       call rmpnn2n (on(1,levrp1),lnwavp,ona(1,levrp1p),llwavp,1)
       call rmpnn2nk(pnm,lnwavp,levr,pnma,llwavp,levrp,3)
       call rmpnn2n (onm(1,levrp1),lnwavp,onma(1,levrp1p),llwavp,1)
       call rmpnn2nk(p,lnwavp,levr,pa,llwavp,levrp,3)
       call rmpnn2n(o(1,levrp1),lnwavp,oa(1,levrp1p),llwavp,1)
#endif
!
!jun -- define variable
       syfk=0.
! 1.dpdla,dpdphi...uu,vv...uum...vvm     
       call ffsncsy1(dpdlama,dpdphia,syfk,                               &   
     &    kpxp,kpyp,iwav1p,igrd1p,lotsynk,levrp*2+1)
!.2.tex,tey...rqx,rqy...vvy,vvx...uux,uuy
       call ffsncsy1(texk,teyk,syfk,                                     &
     &    ktxp,ktyp,iwav1p,igrd1p,lotsynk,levrp*3+levhp)
!
#ifdef NONHYD
!.2.NHYD.  pnx,pny...tnx,tny...onx,ony
      call ffsncsy1(pnxk,pnyk,snnxk,                                     &
     &    knpxp,knpyp,iwav1p,igrd1p,lotsnnk,levrp*3+1)
!.2.NHYD.  pna,tna,ona,pnma,tnma,onma
      call ffscosy1(pna,snnxk,knppp,                                      &
     &    iwav1p,igrd1p,lotsnnk,levrp*6+2)
!      
#endif

!.3. q,te,rq,rwm,qm,rm
       call ffscosy1(qa,syfk,kppp,                                       &
     &    iwav1p,igrd1p,lotsynk,levrp*2+levhp*2+2)

! switch uux uuy and vvy vvx for ffsncs
#ifdef OPENMP
!$omp parallel do private(j,k,i,synhold)
#endif
      do j=1,jgrd1
      do k=1,levrp
      do i=1,lonlens
        synhold            = syfk(i,kuxp-1+k,j)
        syfk(i,kuxp-1+k,j) = syfk(i,kuyp-1+k,j)
        syfk(i,kuyp-1+k,j) = synhold
        synhold            = syfk(i,kvxp-1+k,j)
        syfk(i,kvxp-1+k,j) = syfk(i,kvyp-1+k,j)
        syfk(i,kvyp-1+k,j) = synhold
      enddo
      enddo
      enddo
!     
! second loop
!
      call rmpnl2ny(syfk,igrd1p,jgrd1,synk,igrd1,jgrd1p,                 &   
     &               lotsynk,1,lotsynk)
#ifdef NONHYD
      call rmpnl2ny(snnxk,igrd1p,jgrd1,snnyk,igrd1,jgrd1p,               &
     &               lotsnnk,1,lotsnnk)
#endif
!       call maxmin(synk(1,kuup,1),igrd1,1,1,1,'in rloopmp, check u')
!
        lat1s=1
        lat2s=latlen(mype)
        latdons=0
!
      call ffsncsx1(synk,jgrd1p,kpyp,kpxp,lotsynk,numcsp)
      call ffscosx1(synk,jgrd1p,numcs2p+1,lotsynk,numcos2p)
#ifdef NONHYD
      call ffsncsx1(snnyk,jgrd1p,knpyp,knpxp,lotsnnk,noncsp)
      call ffscosx1(snnyk,jgrd1p,locncosp,lotsnnk,noncos2p)
#endif
!
      call rmpx2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                 & 
     &             jgrd1p,kpyp,kpy,1)
      call rmpnk2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &
     &             jgrd1p,levrp,levr,kvvp,kvv,5+ntotal)
      call rmpx2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                 &
     &             jgrd1p,kpxp,kpx,1)
      call rmpnk2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &
     &             jgrd1p,levrp,levr,kuup,kuu,5+ntotal)
      call rmpx2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                 &
     &             jgrd1p,kppp,kpp,1)
      call rmpnk2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &
     &             jgrd1p,levrp,levr,kttp,ktt,2+ntotal)
      call rmpx2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                 &
     &             jgrd1p,kpmp,kpm,1)
      call rmpnk2nx(synk,igrd1,lotsynk,syn,igrd1p,lotsyn,                &
     &             jgrd1p,levrp,levr,kqmp,kqm,ntotal)
#ifdef NONHYD
      call rmpnk2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,               &
     &             jgrd1p,levrp,levr,knpyp,knpy,3)
      call rmpx2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,                &
     &             jgrd1p,knwyp+levrp,knwy+levr,1)
      call rmpnk2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,               &
     &             jgrd1p,levrp,levr,knpxp,knpx,3)
      call rmpx2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,                &
     &             jgrd1p,knwxp+levrp,knwx+levr,1)
      call rmpnk2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,               &
     &             jgrd1p,levrp,levr,knppp,knpp,3)
      call rmpx2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,                &
     &             jgrd1p,knwwp+levrp,knww+levr,1)
      call rmpnk2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,               &
     &             jgrd1p,levrp,levr,knpmp,knpm,3)
      call rmpx2nx(snnyk,igrd1,lotsnnk,snn,igrd1p,lotsnn,                &
     &             jgrd1p,knwmp+levrp,knwm+levr,1)
#endif
!
#ifdef CNT_1
        call cnttime_off("one_loop_spectal")
#endif
!
      lat1s=1
      lat2s=latlen(mype)
      latdons=0
      latts=latstr(mype)-1
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj,i,j,k,dt)
#endif
      do 2052 lat=lat1s,lat2s
        lan=lat-latdons
        latt=lat+latts
!
! ........obtain base field values
!
      call pint3(qb  ,bgd(1,kppb,lan),                                   &  
     &                bgd(1,kpxb,lan),                                   &  
     &                bgd(1,kpyb,lan),wsltb,dwsltb,latt,numbgd,          &
     &                igrd1p,                                            &
     &                ib1,ib2,jb1,jb2,jbx,istr,ilen,                     &
     &                bgf,border,jgrd1)
!
      latj = (lat-1)*igrd1p     
! .......obtain full field values
!
!       call maxmin(syn(1,ktt,lan),lonlens,levr,1,levr,'aft pint3 tmp')
!       call maxmin(syn(1,ktt,lan),lonlens,levr,1,levr,'aft pint3 tmp')
!       call maxmin(bgd(1,kttb,lan),lonlens,levr,1,levr,'aft pint3 bgd')
!       call maxmin(snn(1,kntt,lan),lonlens,levr,1,levr,'aft pint3 nt')
!       call maxmin(snn(1,knww,lan),lonlens,levr,1,levr,'aft pint3 nw')
!       call maxmin(snn(1,knpp,lan),lonlens,levr,1,levr,'aft pint3 np')

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
      if (lsphy) then

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

        if(itimsw.ne.0 .or. itimlw.ne.0) then
         call radiation_run(lat,lan,lotsynr,                             &
     &    kpyr,kvvr,kpxr,kuur,kddr,kppr,kttr,kqqr,                       &  
#ifdef NONHYD
     &    snn(1,knww,lan),                                               &
#endif
     &    synr,phy_f3d,fluxr,slmsk,                                      &
     &    hprime(1,1,1),stc(1,1,1),sheleg,zorl,cv,cvt,cvb,               &
     &    alvsf,alnsf,alvwf,alnwf,facsf,facwf,                           &
     &    swh,hlw,sfcnsw,sfcdlw,coszer,tsflw,cvavg,del                   &
#ifdef RKN
     &    ,flwupf(1,lan),fswupf(1,lan),fswdnf(1,lan),                    &
     &    sswupf(1,lan),sswdnf(1,lan),                                   &
     &    slwupf(1,lan),slwdnf(1,lan),                                   &
     &    cldsaf(1,1,lan),mtopaf(1,1,lan),mbotaf(1,1,lan)                &
#endif
     &     )
        endif
      endif
! end radiation
 2052 continue
!

!
! ...... get base field tendencies ....
!
      do lat=lat1s,lat2s
        lan=lat-latdons
        latt=lat+latts
      call pint1(qltb,bgd(1,kplb,lan),wsltb,latt,numbgd,igrd1p,          &
     &           ib1,ib2,jb1,jb2,jbx,istr,ilen,                          &
     &           bgf,border,jgrd1)
       enddo
!
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj,i,j,k,dt)
#endif
      do 2053 lat=lat1s,lat2s
        lan=lat-latdons
        latt=lat+latts
! ......... compute full field forcing
!
!
#ifdef NONHYD
      call nfidi(syn(1,kuu,lan),syn(1,kvv,lan),                          &
     &           syn(1,ktt,lan),syn(1,kqq,lan),                          &
     &           syn(1,kpx,lan),syn(1,kpy,lan),                          &
     &           syn(1,kux,lan),syn(1,kuy,lan),                          &
     &           syn(1,kvx,lan),syn(1,kvy,lan),                          &
     &           syn(1,ktx,lan),syn(1,kty,lan),                          &
     &           syn(1,kqx,lan),syn(1,kqy,lan),                          &
     &           syn(1,kpp,lan),spdlat(1,lan),                           &
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
     &     lonlens,igrd1p,levr,levrm1,levrp1,ntotal                      &
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
! end radiation
 2053 continue
!
!****************** physics
      if(lsphy) then
!
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj,i,j,k,dt)
#endif
      do 2054 lat=lat1s,lat2s
        lan=lat-latdons
        latt=lat+latts

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
!snnm
!yj#ifdef NONHYD
!yj      do k=1,levr
!yj      do i=1,lonlens
!yj      snnm(i,knppm-1+k,lan)=snn(i,knpm-1+k,lan)+                        &
!yj     &     synm(i,kppm,lan)+log(sl(k))
!yj      snnm(i,knttm-1+k,lan)=snn(i,kntm-1+k,lan)+synm(i,kttm-1+k,lan)
!yj      enddo
!yj      enddo
!yj      do  k=1,levrp1
!yj      do  i=1,lonlens
!yj        snnm(i,knwwm-1+k,lan)=snn(i,knwm-1+k,lan)
!yj      enddo
!yj      enddo
!yj#endif

!update to nstar
      do i=1,lonlens
      syn(i,kpp,lan) = syn(i,kpp,lan)*dt2+synm(i,kppm,lan)
      enddo
      do k=1,levr
      do i=1,lonlens
      syn(i,kuu-1+k,lan)=syn(i,kuu-1+k,lan)*dt2+synm(i,kuum-1+k,lan)
      syn(i,kvv-1+k,lan)=syn(i,kvv-1+k,lan)*dt2+synm(i,kvvm-1+k,lan)
      syn(i,ktt-1+k,lan)=syn(i,ktt-1+k,lan)*dt2+synm(i,kttm-1+k,lan)
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
!yj: 1. update nonhyd pert. 2.total field=nonhyd.pert.+hyd.fullfield
      do k=1,levr
      do i=1,lonlens
      snn(i,kntt-1+k,lan)=snn(i,kntt-1+k,lan)*dt2+snn(i,kntm-1+k,lan)   &
     &                   +syn(i,ktt-1+k,lan)
      snn(i,knpp-1+k,lan)=snn(i,knpp-1+k,lan)*dt2+snn(i,knpm-1+k,lan)   &
     &                   +syn(i,kpp,lan)+log(sl(k))
      enddo
      enddo
      do k=1,levr+1
      do i=1,lonlens
      snn(i,knww-1+k,lan)=snn(i,knww-1+k,lan)*dt2+snn(i,knwm-1+k,lan)
      enddo
      enddo
!yj
!yj      do k=1,levr
!yj      do i=1,lonlens
!yj      snn(i,kntt-1+k,lan)=snn(i,kntt-1+k,lan)*dt2+snnm(i,knttm-1+k,lan)
!yj      snn(i,knpp-1+k,lan)=snn(i,knpp-1+k,lan)*dt2+snnm(i,knppm-1+k,lan)
!yj      enddo
!yj      enddo
!yj      do k=1,levr+1
!yj      do i=1,lonlens
!yj      snn(i,knww-1+k,lan)=snn(i,knww-1+k,lan)*dt2+snnm(i,knwwm-1+k,lan)
!yj      enddo
!yj      enddo
#endif

! end rfidi
 2054 continue

!
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj,i,j,k,n,dt)
#endif
!       print *,'physics_run is called,me=',me,'lats1=',lat1s,           &
!     &  'lat2s=',lat2s,'latdons=',latdons,'latt=',latt,'latts=',latts
      do 2056 lat=lat1s,lat2s
        lan=lat-latdons
        latt=lat+latts

        call physics_run(lan,lat,                                        &
     &    syn(1,kpx,lan),syn(1,kpy,lan),                                 &  
     &    syn(1,kuu,lan),syn(1,kvv,lan),syn(1,kpp,lan),                  &
     &    syn(1,ktt,lan),syn(1,kqq,lan),syndd(1,1,lan),                  &
     &    anl(1,kts,lan),anl(1,kqs,lan),anl(1,kus,lan),anl(1,kvs,lan),   &
#ifdef NONHYD
     &    snn(1,knpp,lan),snn(1,knww,lan),snn(1,kntt,lan),               &
     &    ann(1,knppa,lan),ann(1,knwwa,lan),ann(1,kntta,lan),            &
#endif
#ifdef RKN
     &    psexp(1,lan),radsl(1,lan),                                     &
     &    dlwsf1(1,lan),qss(1,lan),                                      &
     &    gflx(1,lan),cd(1,lan),                                         &
     &    cdq(1,lan),evap(1,lan),                                        &
     &    hflx(1,lan),drain(1,lan),                                      &
     &    runof(1,lan),cld1d(1,lan),                                     &
     &    gamt(1,lan),gamq(1,lan),                                       &
     &    dqsfc1(1,lan),dtsfc1(1,lan),                                   &
     &    dusfc1(1,lan),dvsfc1(1,lan),                                   &
     &    dusfcg(1,lan),dvsfcg(1,lan),                                   &
     &    rainc(1,lan),rainl(1,lan),                                     &
     &    hsw(1,1,lan),vvel(1,1,lan),                                    &
     &    stsoil(1,1,lan),smsoil(1,1,lan),                               &
#endif
     &   phy_f3d,phy_f2d)

! end rfidi
 2056 continue

!
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj,i,j,k,dt)
#endif
      do 2058 lat=lat1s,lat2s
        lan=lat-latdons
        latt=lat+latts

      do k=1,levr
      do i=1,lonlens
      anl(i,kus-1+k,lan)=anl(i,kus-1+k,lan)/xm(i,lat)
      anl(i,kvs-1+k,lan)=anl(i,kvs-1+k,lan)/xm(i,lat)
      enddo
      enddo

#ifdef NONHYD
! for pn, tn
      do k=1,levr
      do i=1,lonlens
!yj: nonhyd. pert.= total field - hydro.
!yj: nonhyd. pert. tendency = (nonhyd.pert.(n+1)-nonhyd.pert.(n))/dt2
!yj: note: syn(kts), syn(kpp) are not updated by physics,
!yj:       i.e., anl(kts)=syn(ktt), and syn(kpp) n = syn(kpp) n+1
      snn(i,kntt-1+k,lan)=(  ann(i,kntta-1+k,lan)                       &
     &                       -anl(i,kts+k-1,lan)                        &
     &                       -snn(i,kntm-1+k,lan)   )/dt2
      snn(i,knpp-1+k,lan)=(  ann(i,knppa-1+k,lan)                       &
     &                       -syn(i,kpp,lan)-log(sl(k))                 &
     &                       -snn(i,knpm-1+k,lan)   )/dt2

!yj      snn(i,kntt-1+k,lan)=(ann(i,kntta-1+k,lan)-                     &
!yj     &                     snnm(i,knttm-1+k,lan))/dt2
!yj      snn(i,knpp-1+k,lan)=(ann(i,knppa-1+k,lan)-                     &
!yj     &                     snnm(i,knppm-1+k,lan))/dt2
      enddo
      enddo
! for w
      do k=1,levr+1
      do i=1,lonlens
      snn(i,knww-1+k,lan)=(ann(i,knwwa-1+k,lan)-                        &
     &                     snn(i,knwm-1+k,lan))/dt2
!yj      snn(i,knww-1+k,lan)=(ann(i,knwwa-1+k,lan)-                     &
!yj     &                     snnm(i,knwwm-1+k,lan))/dt2
      enddo
      enddo
#endif

!yj: note: syn(kpp) updated as tendency (Do nonhyd. before update
!yj:       syn(kpp) to tendency.)
      do i=1,lonlens
      syn(i,kpp,lan) = (syn(i,kpp,lan)-synm(i,kppm,lan))/dt2
      enddo

       do k=1,levr
       do i=1,lonlens
      syn(i,kuu-1+k,lan)=(anl(i,kus-1+k,lan)-                            &  
     &                    synm(i,kuum-1+k,lan))/dt2
      syn(i,kvv-1+k,lan)=(anl(i,kvs-1+k,lan)-                            &
     &                    synm(i,kvvm-1+k,lan))/dt2
      syn(i,ktt-1+k,lan)= (anl(i,kts-1+k,lan)-synm(i,kttm-1+k,lan))      &
     &                    /dt2
      enddo
      enddo
!
      do k=1,levh
      do i=1,lonlens
      syn(i,kqq-1+k,lan)=(anl(i,kqs-1+k,lan)-synm(i,kqqm-1+k,lan))      &
     &                  /dt2
      enddo
      enddo
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
 2058 continue

!......................................................
!      if(me.eq.0) print 100,(spdmax(k),k=1,levr)

!  ken point diagnostics
!
#ifdef RKN
!
       if(isave.ne.0.and.npoint.gt.0) then
         lones=lonlen(mype)
         lon1s=lonstr(mype)
         lon2s=lon1s-1+lones
         latt1=latts+lat1s
         latt2=latts+lat2s
!radiation _run
         call rkenputr(igrd1p,jgrd1p,lon1s,lon2s,                        &
     &        lones,                                                     &
     &        latt1,latt2,sdec,                                          &
     &        slmsk(1,lat1s),tser(1,lat1s),                              &
     &        flwupf(1,1),fswupf(1,1),fswdnf(1,1),                       &
     &        sswupf(1,1),sswdnf(1,1),slwupf(1,1),slwdnf(1,1),           &
     &        cldsaf(1,1,1),mtopaf(1,1,1),mbotaf(1,1,1),                 &
     &        itimsw,itimlw)                         
!         call rkenputr(igrd1p,jgrd1p,lon1s,lon2s,                        &
!     &        lones,                                                     &
!     &        itimsw,itimlw)
!
!physics _run
         call rkenput(igrd1p,jgrd1p,lon1s,lon2s,                         &   
     &                    lones,                                         &
     &        latt1,latt2,rcl,                                           &
     &        slmsk(1,lat1s),psexp(1,1),tg3(1,lat1s),sheleg(1,lat1s),    &
     &        radsl(1,1),dlwsf1(1,1),                                    &
     &        tsea(1,lat1s),qss(1,1),gflx(1,1),                          &
     &        zorl(1,lat1s),cd(1,1),cdq(1,1),                            &
     &        evap(1,1),hflx(1,1),stsoil(1,1,1),                         &
     &        canopy(1,lat1s),drain(1,1),smsoil(1,1,1),                  &
     &        runof(1,1),cld1d(1,1),                                     &
     &        u10m(1,lat1s),v10m(1,lat1s),t2m(1,lat1s),q2m(1,lat1s),     &
     &        hpbl(1,lat1s),gamt(1,1),gamq(1,1),                         &
     &        dqsfc1(1,1),dtsfc1(1,1),dusfc1(1,1),dvsfc1(1,1),           &
     &        dusfcg(1,1),dvsfcg(1,1),                                   &
     &        rainc(1,1),rainl(1,1),                                     &
     &        gu0(1,1,1),gv0(1,1,1),                                     &
     &        gt0(1,1,1),gq0(1,1,1),                                     &
     &        hsw(1,1,1),hlw(1,1,lat1s),vvel(1,1,1),sl(1),               & !binbin add sl(1) here
     &        vfrac(1,lat1s),tmpmin(1,lat1s),tmpmax(1,lat1s),            &
     &        bengsh(1,lat1s),geshem(1,lat1s))
!
       endif
#endif
!
!************************* end physics
      endif
!
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj,i,j,nc,k,rsltb)
#endif
      do 2070 lat=lat1s,lat2s
      lan=lat-latdons
      latt=lat+latts
      latj = (lat-1)*igrd1p
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
        if (k.gt.levt) then
          rsltb=1.-cos(pi*(sl(levt)-sl(k))/(sl(levt)-sl(levr)))
          rsltb=rsltb/(2.*rlxzsec)
        else
          rsltb=0.
        endif

      do i=1,lonlens
      syn(i,kuu-1+k,lan)= syn(i,kuu-1+k,lan)-bgd(i,kulb-1+k,lan)         &
     &                   -syn(i,kum-1+k,lan)*sltb(i+latj)/rlxmsec
      syn(i,kvv-1+k,lan)= syn(i,kvv-1+k,lan)-bgd(i,kvlb-1+k,lan)         &
     &                   -syn(i,kvm-1+k,lan)*sltb(i+latj)/rlxmsec
      syn(i,ktt-1+k,lan)= syn(i,ktt-1+k,lan)-bgd(i,ktlb-1+k,lan)         &
#ifdef NONHYD
#ifdef INTDTB
     &                   -syn(i,ktm-1+k,lan)*sltb(i+latj)/rlxhsec
#else
     &                   +0.
#endif
#else
     &                   -syn(i,ktm-1+k,lan)*sltb(i+latj)/rlxhsec
#endif
#ifdef NONHYD
      snn(i,knpp-1+k,lan)= snn(i,knpp-1+k,lan)                           &
     &                   -snn(i,knpm-1+k,lan)*sltb(i+latj)/rlxhsec       &
     &                   -snn(i,knpm-1+k,lan)*rsltb
      snn(i,kntt-1+k,lan)= snn(i,kntt-1+k,lan)                           &
     &                   -snn(i,kntm-1+k,lan)*sltb(i+latj)/rlxhsec       &
     &                   -snn(i,kntm-1+k,lan)*rsltb
#endif

      enddo
      enddo
      do k=1,levh
      do i=1,lonlens
      syn(i,kqq-1+k,lan)= syn(i,kqq-1+k,lan)-bgd(i,kqlb-1+k,lan)         &
     &                   -syn(i,kqm-1+k,lan)*sltb(i+latj)/rlxhsec
      enddo
      enddo
2070  continue
!
#ifdef OPENMP
!$omp parallel do private(lat,lan,latt,latj), &
!$omp&        private(i,j,n,k,rsltb,rsltn,rsltbm,rsltbh,nkkc)
#endif
      do 2080 lat=lat1s,lat2s
      lan=lat-latdons
      latt=lat+latts
      latj = (lat-1)*igrd1p
      if( iimprlx.eq.1 ) then
!        levt=levr-6
        do i=1,lonlens
          rsltb=1./(1.+sltb(i+latj)*2*deltim/rlxhsec)
          syn(i,kpp,lan)= syn(i,kpp,lan)*rsltb
        enddo
        do k=1,levr
          if (k.gt.levt) then
            rsltn=1.-cos(pi*(sl(levt)-sl(k))/(sl(levt)-sl(levr)))
            rsltn=rsltn/(2.*rlxzsec)
          else
            rsltn=0.
          endif
          rsltn=1./(1.+2.*deltim*rsltn)
          do i=1,lonlens
            rsltbm=1./(1.+sltb(i+latj)*2*deltim/rlxmsec)
            rsltbh=1./(1.+sltb(i+latj)*2*deltim/rlxhsec)
            syn(i,kuu-1+k,lan)= syn(i,kuu-1+k,lan)*rsltbm
            syn(i,kvv-1+k,lan)= syn(i,kvv-1+k,lan)*rsltbm
#ifdef NONHYD
#ifdef INTDTB
            syn(i,ktt-1+k,lan)= syn(i,ktt-1+k,lan)*rsltbh
#endif
#else
            syn(i,ktt-1+k,lan)= syn(i,ktt-1+k,lan)*rsltbh
#endif
#ifdef NONHYD
            snn(i,knpp-1+k,lan)= snn(i,knpp-1+k,lan)*rsltbh*rsltn
            snn(i,kntt-1+k,lan)= snn(i,kntt-1+k,lan)*rsltbh*rsltn
#endif
          enddo
        enddo
        do n=1,ntotal
        nkkc=(n-1)*levr
        do k=1,levr
          do i=1,lonlens
            rsltb=1./(1.+sltb(i+latj)*2*deltim/rlxhsec)
            syn(i,kqq-1+nkkc+k,lan)= syn(i,kqq-1+nkkc+k,lan)*rsltb
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
2080  continue

!...........................
!cj
#ifdef RKN
!
!  ken point - div and vort
!
      if(ilshrt.lt.1) then
        if(isave.ne.0.and.npoint.gt.0) then
         lones=lonlen(mype)
         lon1s=lonstr(mype)
         lon2s=lon1s-1+lones
         latt1=latts+lat1s
         latt2=latts+lat2s
!
#ifdef OPENMP
!$omp parallel do private(igpt,jlat,jlat1,k)
#endif
         do igpt=1,npoint
         if(jgpd(igpt).ge.latt1.and.jgpd(igpt).le.latt2) then
         if((igpd(igpt).ge.lon1s.and.igpd(igpt).le.lon2s) ) then
!     &      (igpd(igpt).ge.lon1n.and.igpd(igpt).le.lon2n)) then
           jlat=jgpd(igpt)-latt1+1
           jlat1=jlat
           if(igpd(igpt).ge.lon1s.and.igpd(igpt).le.lon2s) then
             ilon=igpd(igpt)-lon1s+1
           endif

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
#ifdef CNT_1
        call cnttime_on("one_loop_spectal")
#endif
!
      call rmpnx2x(syn,igrd1p,lotsyn,synk,igrd1,lotsynk,                 &
     &             jgrd1p,kpp,kppp,1)
      call rmpnx2nk(syn,igrd1p,lotsyn,synk,igrd1,lotsynk,                &
     &             jgrd1p,levr,levrp,kvv,kvvp,1)
      call rmpnx2nk(syn,igrd1p,lotsyn,synk,igrd1,lotsynk,                &
     &             jgrd1p,levr,levrp,kuu,kuup,1)
      call rmpnx2nk(syn,igrd1p,lotsyn,synk,igrd1,lotsynk,                &
     &             jgrd1p,levr,levrp,ktt,kttp,1+ntotal)
#ifdef NONHYD
      call rmpnx2nk(snn,igrd1p,lotsnn,snnyk,igrd1,lotsnnk,               &
     &             jgrd1p,levr,levrp,knpp,knppp,3)
      call rmpnx2x(snn,igrd1p,lotsnn,snnyk,igrd1,lotsnnk,                &
     &             jgrd1p,knww+levr,knwwp+levrp,1)
#endif
!
      call ffancsx1(synk,jgrd1p,kvvp,kuup,lotsynk,levrp)
      call ffacosx1(synk,jgrd1p,kppp,lotsynk,levrp+levhp+1)
#ifdef NONHYD
      call ffacosx1(snnyk,jgrd1p,knppp,lotsnnk, levrp*3+1 )
#endif
!
      call rmpny2nl(synk,igrd1,jgrd1p,syfk,igrd1p,                       &
     &       jgrd1,lotsynk,1,lotsynk)
#ifdef NONHYD
      call rmpny2nl(snnyk,igrd1,jgrd1p,snnxk,igrd1p,                     &
     &       jgrd1,lotsnnk,1,lotsnnk)
#endif
!
!.1. uu,vv
      call ffancsy1(syfk,xa,wa,kuup,kvvp,igrd1p,iwav1p,lotsynk,levrp)
!.2. z,y,rt
      call ffacosy1(syfk,za,kppp,igrd1p,iwav1p,lotsynk,levrp+levhp+1)
#ifdef NONHYD
!.3. 
      call ffacosy1(snnxk,pa,knppp,igrd1p,iwav1p,lotsnnk,levrp*3+1)
#endif
!..................................................................
!
      call rmpnk2nn(ya,llwavp,levrp,y,lnwavp,levr,1+ntotal)
      call rmpn2nn(za,llwavp,z,lnwavp,1)
      call rmpnk2nn(xa,llwavp,levrp,x,lnwavp,levr,1)
      call rmpnk2nn(wa,llwavp,levrp,w,lnwavp,levr,1)
#ifdef NONHYD
      call rmpnk2nn(pa,llwavp,levrp,p,lnwavp,levr,3)
      call rmpn2nn(oa(1,levrp1p),llwavp,o(1,levrp1),lnwavp,1)
#endif
!
#ifdef CNT_1
        call cnttime_off("one_loop_spectal")
#endif
!
#ifdef T
      call cctogd(y,tmpbgt,levr)
      do k=1,levr
        do i=1,lngrdp
          wt(i,k,14)=tmpbgt(i,k)-wt(i,k,14)
        enddo
      enddo
#endif
!
#ifdef Q
      call cctogd(rt,tmpbgt,levh)
      do k=1,levh
        do i=1,lngrdp
          wq(i,k, 9)=tmpbgt(i,k)-wq(i,k, 9)
        enddo
      enddo
#endif
!
#ifdef U
      call sctogd(x,tmpbgt,levr)
      do k=1,levr
        do i=1,lngrdp
          wu(i,k,10)=tmpbgt(i,k)-wu(i,k,10)
        enddo
      enddo
#endif
!
#ifdef V
      call cstogd(w,tmpbgt,levr)
      do k=1,levr
        do i=1,lngrdp
          wv(i,k,10)=tmpbgt(i,k)-wv(i,k,10)
        enddo
      enddo
#endif
!
#ifdef P
      call cctogd(z,tmpbgt(1,1),1)
      do i=1,lngrdp
        wp(i,1, 5)=tmpbgt(i,1)-wp(i,1, 5)
      enddo
#endif
!
#ifdef OPENMP
!$omp parallel do private(k,lat)
#endif
!
      do k=1,levr
      spdmax(k) = 0.0
       do lat=1,latlen(mype)
        spdmax(k)=max(spdmax(k),spdlat(k,lat))
       enddo
      enddo
      call rmpgetspd(levr,spdmax)
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
#endif
!

      return
      end

