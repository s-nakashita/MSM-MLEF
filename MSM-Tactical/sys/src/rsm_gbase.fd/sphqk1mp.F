      subroutine sphqk1mp(kmx,ff,ffb,lwvdef,latdef,lngrd1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sphpt1
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do global spherical transform to a given latitude
!            longitude grid.
!
! program history log:
!
! usage:    call sphpt1(km,wave,gridc,rlat,rlon,lngrdb)
!   input argument list:
!     km  - dimension of horizontal surface
!     wave  - global wave coefficient
!     rlat  - regional latitude values
!     rlon  - regional longitude values
!     lngrdb  - the total regional grid on a horizontal surface
!
!   output argument list:
!     gridc - regional grid values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!   rpln2i  sgemvx1
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
#ifdef CRA
#include <rscomcpu.h>
#else
#include <ncpu.h>
#endif
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#include <commpi.h>
#endif
!cc
!
#ifdef MP
#define LEVSS  levsp
#define LNT22S lnt22p
#define LLN22S  lln22p
#define LLN2S  lln2p
#define LCAPS lcapp
#define LCAP22S lcap22p
#define LONFS lonf
#define LATGS latgp
#else
#define LEVSS  levs
#define LNT22S lnt22
#define LLN22S lnt22
#define LLN2S  lnt2
#define LCAPS twoj1
#define LCAP22S lonf22
#define LONFS lonf
#define LATGS latg
#endif
!
!  icntl=1  does variable itself
!
      dimension ff(LNT22S,kmx)                                           &  
     &     ,lwvdef(jcap+1),latdef(latg/2)
#ifdef MP
      real, allocatable :: ffa(:,:)
#endif
      dimension ffb(lngrd1,kmx)
!
! local array
#ifdef MP
#define NCPUSS latg2
#else
#define NCPUSS ncpus
#endif
!
      real, allocatable :: syn(:,:,:)
#ifdef MP
      real, allocatable :: syf(:,:,:)
#endif
!
#ifdef MP
      real, allocatable :: grid(:)
#else
      real, allocatable :: grid(:,:)
#endif
#ifdef MP
      real, allocatable :: syf1(:,:,:)
      real, allocatable :: grip(:,:,:)
      real, allocatable :: grip1(:,:)
      real, allocatable :: grif(:,:)
#endif
!
      common /reprogr1/qtt(LLN2S,latg2),                                 &  
     &         qvv(LCAPS,latg2),qww(LLN2S,latg2)
!....................................................
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
      lot=kmx
      if(lot.eq.levs) then
        kkm=LEVSS
      else
        kkm=1
      endif
!
      allocate (syn(LCAP22S,LEVSS,NCPUSS))
#ifdef MP
      allocate (ffa(LLN22S,LEVSS))
      allocate (syf(lonf22,LEVSS,latg2p))
      allocate (grid(lonf*latg))
      allocate (syf1(lonf22,1,latg2p))
      allocate (grip(lonf2p,kmx,latg2p))
      allocate (grip1(lonf2p,latg2p))
      allocate (grif(lonf2,latg2))
#else
      allocate (grid(lonf*latg,kmx))
#endif
!
#ifdef MP
      if(lot.eq.levs) then
        call mpnn2nk(ff,lnt22p,levs, ffa,lln22p,levsp,1) ! (lp,np,kf) to (lp,nf,kp)
      else
        call mpnn2n(ff,lnt22p,ffa,lln22p,1)
      endif
#define FFS ffa
#else
#define FFS ff
#endif
!......................................................
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
!$doacross share(syn,qtt,qvv,colrad,lat1,lat2,latdon,kmx,lot,ff),
!$&        local(dummy,lat,lan,lotnfs)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(syn,qtt,qvv,colrad,lat1,lat2,latdon,kmx,lot)
!mic$1 shared(ff)
!mic$1 private(dummy,lat,lan,lotnfs)
#endif
#ifdef OPENMP
!$omp parallel do private(lat,lan)
#endif
!
      do 1000 lat = lat1,lat2
      lan=lat-latdon
!
      call sums2imp(FFS ,syn(1,       1 ,lan),qtt(1,lat),                &
     &            llstr,llens,lwvdef,kkm,jcap                            &
#ifndef MP
     &           ,lonf,lnt2,lnt22                                        &
#else 
     &           ,lcapp,lln2p,lln22p                                     &
#endif
     &            )
!
1000  continue
!
#ifdef MP
      call mpnl2ny(jcapp,lonf,syn,lcap22p,latg2,                         &  
     &             syf,lonf22,latg2p,levsp,1,kkm,                        & ! (lp,jf,kp) to (lf,jp,kp)
     &             lwvdef,latdef,jcap,latg)
!
      lat1=jstr
      lat2=jend
      latdon=jstr-1
#define SYNS syf
#else
#define SYNS syn
#endif
!
      do 1200 lat = lat1,lat2
      lan=lat-latdon
      call fti_lonf (SYNS(1,1,lan),dummy,kkm*2,1)
1200  continue
!
#ifdef ORIGIN_THREAD
!$doacross share(syn,grid,lot,lat1,lat2,latdon),
!$&        local(lat,lan,latn,lats,latnx,latsx,i,kk)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(syn,grid,lot,lat1,lat2,latdon)
!mic$1 private(lat,lan,latn,lats,latnx,latsx,i,kk)
#endif
!
#ifndef MP
      do 1500 lat=lat1,lat2
      lan=lat-latdon
      latn = lat
      lats = latg + 1 - lat
      latnx = (latn-1)*lonf
      latsx = (lats-1)*lonf
      do 170 k=1,lot
      do 171 i=1,lonf
      grid(i+latnx,k)=syn(i,k,lan)
171   continue
      do 172 i=1,lonf
      grid(i+latsx,k)=syn(i+lonf,k,lan)
172   continue
170   continue
!
1500  continue
!
      latdon=latdon+(lat2-lat1+1)
10000 continue
!......................................................
! interpolation to regional grid
      if(lngrd1.eq.lngrd) then
         call ggtorg(grid(1,1),ffb,lot)
      else
         call ggtobg(grid(1,1),ffb,lot)
      endif
!
#endif
!
#ifdef MP
      if(lot.eq.levs) then
       call mpnk2nx(lonf,syf,lonf22,levsp,                               &  ! (if,jp,kp) to (ip,jp,kf)
     &         grip,lonf2p,levs,latg2p,levsp,levs,1,1,1)
      else
       do lat = lat1,lat2
        lan=lat-latdon
        do i=1,lonf22
          syf1(i,1,lan)=syf(i,1,lan)
        enddo
       enddo
       call mpx2nx (lonf,syf1,lonf22,1,                                  & 
     &              grip,lonf2p,1,latg2p,1,1,1)
      endif
!
#ifdef OPENMP
!$omp parallel do private(k,lat,lan,latn,lats,latnx,latsx,i,kk)
#endif
      do 2000 k=1,lot
!
      do lat = lat1,lat2
        lan=lat-latdon
        do i=1,lonf2p
          grip1(i,lan)=grip(i,k,lan)
        enddo
      enddo
!
      call mpgp2f(lonf,grip1,lonf2p,latg2p,grif,lonf2,latg2,             &   ! (ip,jp,kf) to (if,jf,kf)
     &            1,latdef,latg)
!
      call rmpbcastr(grif,lonf2*latg2)
!
      do 1700 lat=1,latg2
      latn = lat
      lats = latg + 1 - lat
      latnx = (latn-1)*lonf
      latsx = (lats-1)*lonf
      do 191 i=1,lonf
      grid(i+latnx)=grif(i,lat)
191   continue
      do 192 i=1,lonf
      grid(i+latsx)=grif(i+lonf,lat)
192   continue
1700  continue
!......................................................
! interpolation to regional grid
      call ggtobg(grid(1),ffb(1,k),1)
!
2000  continue
#endif
!
      deallocate (syn)
      deallocate (grid)
#ifdef MP
      deallocate (ffa)
      deallocate (syf)
      deallocate (syf1)
      deallocate (grip)
      deallocate (grip1)
      deallocate (grif)
#endif
!
      return
      end
