      subroutine sphqk1(kmx,ff,ffb,colrad,lngrd1)
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
#define ncpus ncpu
!cc
!
!  icntl=1  does variable itself
!
      dimension ff(lnt22,kmx) ,colrad(latg2)
      dimension ffb(lngrd1,kmx)
!
! local array
           dimension                                                     & 
     &              syn(lonf22,levs,ncpus)                               &
     &             ,grid(lonf*latg,levs)                                 &
     &             ,qtt(lnt2,ncpus),qvv(lnut2,ncpus)
!
      lot=kmx
!
!......................................................
! compute lat band limits
!
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
!$omp parallel do private(dummy,lat,lan,lotnfs)
#endif
!
      do 1000 lat = lat1,lat2
      lan=lat-latdon
!
      call pln2i (qtt(1,lan),qvv(1,lan),colrad,lat)
!
      call sums2i(ff ,syn(1,1,lan),qtt(1,lan),kmx)
!
      lotnfs=lot*2
      call fti_lonf (syn(1,1,lan),dummy,lotnfs,1)
!
1000  continue
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
#ifdef OPENMP
!$omp parallel do private(lat,lan,latn,lats,latnx,latsx,i,kk)
#endif
      do 1500 lat=lat1,lat2
      lan=lat-latdon
      latn = lat
      lats = latg + 1 - lat
      latnx = (latn-1)*lonf
      latsx = (lats-1)*lonf
      do 170 kk=1,lot
      do 171 i=1,lonf
      grid(i+latnx,kk)=syn(i,kk,lan)
171   continue
      do 172 i=1,lonf
      grid(i+latsx,kk)=syn(i+lonf,kk,lan)
172   continue
170   continue
!
1500  continue
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
      return
      end
