      subroutine sphqk2(kmx,ff,ffxb,ffyb,                                &                  
     &           colrad,epsi,rcsln,rsnln,lngrd1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sphpt2
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do global spherical transform the derivatives to a given
!            latitude-longitude grid.
!
! program history log:
!
! usage:    call sphpt2(km,wave,gridx,gridy,
!                       rlat,rlon,rsnln,rcsln,lngrdb)
!   input argument list:
!     km  - dimension of horizontal surface
!     wave  - global wave coefficient
!     rlat  - regional latitude values
!     rlon  - regional longitude values
!     rsnln - the coefficient to rotate global grid to regional grid
!     rcsln - the coefficient to rotate global grid to regional grid
!     lngrdb  - the total regional grid on a horizontal surface
!
!   output argument list:
!     gridx - regional grid x derivative
!     gridy - regional grid y derivative
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
#include <ncpu.h>
#include <rscomcpu.h>
!cc
      parameter(ncpus=ncpu)
!
!  icntl=2  does d a / dx and d a /dy
!
      dimension ff(lnt22,kmx),epsi(jcap1,jcap2) ,colrad(latg2)
      dimension ffxb(lngrd1,kmx),ffyb(lngrd1,kmx)                        &                         
     &         ,rsnln(lngrd1),rcsln(lngrd1)
!
! local array
           dimension                                                     &
     &              syn(lonf22,levs*2,ncpus)                             &
     &             ,grid(lonf*latg,levs*2)                               &
     &             ,qtt(lnt2,ncpus),qvv(lnut2,ncpus)                     &
     &     ,dffdlam(lnt22,levs),dffdphi(lnt22,levs)                      &
     &     ,vectop(2,jcap1,levs)
!
      lot   = kmx * 2
!
      do 200 k=1,kmx
      call dellnp(ff(1,k),dffdphi(1,k),vectop(1,1,k),dffdlam(1,k))
 200  continue
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
      do 20000 ngg=1,nggs
      if((ngg.eq.nggs).and.(last.ne.0)) inclat=last
      lat1=lat1+ncpus
      lat2=lat2+inclat
!
#ifdef ORIGIN_THREAD
!$doacross share(syn,qtt,qvv,colrad,lat1,lat2,latdon,kmx,lot,
!$&              dffdlam,dffdphi,vectop)
!$&        local(dummy,lat,lan,lotnfs)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(syn,qtt,qvv,colrad,lat1,lat2,latdon,kmx,lot)
!mic$1 shared(dffdlam,dffdphi,vectop)
!mic$1 private(dummy,lat,lan,lotnfs)
#endif
#ifdef OPENMP
!$omp parallel do private(dummy,lat,lan,lotnfs)
#endif
!
      do 2000 lat = lat1,lat2
      lan=lat-latdon
!
      call pln2i(qtt(1,lan),qvv(1,lan),colrad,lat)
!
      call sums2i(dffdlam,syn(1,0*kmx+1,lan),qtt(1,lan),kmx)
      call sums2i(dffdphi,syn(1,1*kmx+1,lan),qtt(1,lan),kmx)
!
      call sumtop(syn(1,1*kmx+1,lan),vectop,qvv(1,lan),                  &                     
     &       kmx,lonf,lonf/2)
!
      lotnfs=lot*2
      call fti_lonf (syn(1,1,lan),dummy,lotnfs,1)
!
2000  continue
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
      do 2500 lat=lat1,lat2
      lan=lat-latdon
      latn = lat
      lats = latg + 1 - lat
      latnx = (latn-1)*lonf
      latsx = (lats-1)*lonf
      do 270 kk=1,lot
      do 271 i=1,lonf
      grid(i+latnx,kk)=syn(i,kk,lan)
271   continue
      do 272 i=1,lonf
      grid(i+latsx,kk)=syn(i+lonf,kk,lan)
272   continue
270   continue
!
2500  continue
      latdon=latdon+(lat2-lat1+1)
20000 continue
!......................................................
! interpolation to regional grid
      if(lngrd1.eq.lngrd) then
        call ggtorg(grid(1,       1),ffxb,kmx)
        call ggtorg(grid(1,1*kmx+1),ffyb,kmx)
      else
        call ggtobg(grid(1,       1),ffxb,kmx)
        call ggtobg(grid(1,1*kmx+1),ffyb,kmx)
      endif
!
#ifdef ORIGIN_THREAD
!$doacross share(ffxb,ffyb,rsnln,rcsln,lngrd,kmx),
!$&        local(qx,qy,n,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(ffxb,ffyb,rsnln,rcsln,lngrd,kmx)
!mic$1 private(qx,qy,n,k)
#endif
#ifdef OPENMP
!$omp parallel do private(qx,qy,n,k)
#endif
        do 120 k=1,kmx
        do 120 n=1,lngrd1
          qx=ffxb(n,k)
          qy=ffyb(n,k)
          ffxb(n,k)= - qx * rsnln(n) - qy * rcsln(n)
          ffyb(n,k)=   qx * rcsln(n) - qy * rsnln(n)
120     continue
!
      return
      end
