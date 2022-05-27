      subroutine setg2b(dlamda0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setg2b
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare some arraies for global to regional transformation
!            including map factor
!
! program history log:
!
! usage:    call setg2b(dlamda0)
!   input argument list:
!     dlamda0 - angel between 0 longitude and x axies
!
!   output argument list:
!
!   common block:
! rscomltb  /regin1/  /repro/
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! glats  gftlonf  gpln2i  epsilo  cmpind  ggozrm  pln2i  ftilonf
! fplfml  fl2i  sphpt1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!
!$$$
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
#include <rscomloc.h>
#include <rscomltb.h>
!cc
!
#ifdef MP
#define JCAP1S jcap1p
#define LNT22S lnt22p
#define LNT2S  lnt2p
#define LLN22S  lln22p
#define LLN2S  lln2p
#define LCAPS lcapp
#define LCAP22S lcap22p
#define LATG2S latg2p
#else
#define JCAP1S jcap1
#define LNT22S lnt22
#define LNT2S  lnt2
#define LLN22S  lnt22
#define LLN2S  lnt2
#define LCAPS twoj1
#define LCAP22S lonf22
#define LATG2S latg2
#endif
!....
      common /regin1/ gm(LLN22S), xm(lngrdb)                             & 
     &  , csln(lonf2), snln(lonf2), epsi(jcap2,jcap1)                    &
     &  , colrad(latg2), wgt(latg2)                                      &
     &  , rcslb(lngrdb), rsnlb(lngrdb)                                   &
     &  , rlatb(lngrdb),rlonb(lngrdb)                                    &
#ifndef MP
     &  ,lwvdef(jcap+1),latdef(latg/2)                                   &
#endif
     &  ,dummy
!....
#ifdef MP
      real, allocatable :: tmpq(:)
#endif
      real, allocatable :: tmpqtt(:),tmpqvv(:),gmtmp(:)
!....
#ifdef MP
#define NCPUSS latg2
#else
#define NCPUSS ncpus
#endif
!
      real, allocatable :: syn(:,:,:)
      real, allocatable :: flp(:,:,:,:),flm(:,:,:,:)
#ifdef MP
      real, allocatable :: syf(:,:,:)
#endif
!
      common /reprogr/ wgtcs(latg2), rcs2(latg2), gclat(latg)
!
      common /reprogr1/qtt(LLN2S,latg2),                                 &   
     &         qvv(LCAPS,latg2),qww(LLN2S,latg2)
!.................................................................
#ifdef MP
      allocate (syf(lonf22,2,latg2p))
      allocate (tmpq(lnt2p))
#endif
      allocate (syn(LCAP22S,2,NCPUSS))
      allocate (flp(2,JCAP1S,1,NCPUSS))
      allocate (flm(2,JCAP1S,1,NCPUSS))
      allocate (tmpqtt(lnt2))
      allocate (tmpqvv(lnut2))
      allocate (gmtmp(LNT22S))
!
#ifdef MP
      me=mype
#else
      me=0
#endif
!
#ifdef MP
      llstr=glwvstr(mype)
      llens=glwvlen(mype)
      jstr=glatstr(mype)
      jend=glatstr(mype)+glatlen(mype)-1
#else
      llstr=0
      llens=jcap1
      do l=1,jcap1
        lwvdef(l)=l-1
      enddo
      do l=1,latg2
        latdef(l)=l
      enddo
#endif
!
!.................................................................
      if(me.eq.0) print *,' ------- into rsetg2b ----- '
!.................................................................
! define global grid
      call glats(latg2,colrad,wgt,wgtcs,rcs2)
      do 7 j=1,latg2
        gclat(j) = colrad(j)
        jj = latg - j + 1
        gclat(jj) = pi - colrad(j)
 7    continue
! .....................................................
! define global to regional grid interpolation in physic grids
      call g2bini(gclat,blat,blon)
      call gtobini(gclat,blat,blon)
! ....................................................
! define global constant
      nproj = rproj
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        dxlon = pi * 2.0 / lonf 
        do i=1,lonf 
          xlon = dxlon*(i-1) - dlamda0
          xlon = nproj * xlon
          sinlon = sin(xlon)
          coslon = cos(xlon)
          snln(i) = nproj * sinlon
          csln(i) = nproj * coslon
          snln(i+lonf) = snln(i)
          csln(i+lonf) = csln(i)
        enddo
      else if( nproj.eq.0 ) then
        do i=1,lonf
          snln(i) = -1.0
          csln(i) =  0.0
          snln(i+lonf) = snln(i)
          csln(i+lonf) = csln(i)
        enddo
      endif
!
      call gft_lonf
      call gpln2i
      call epsilo(epsi,jcap)
      call cmpind
      call ggozrm(epsi)
! ...........................................................
! first loop to get maping factor in coefficiend then trancate
!
!           lat loop
      rad = pi / 180.0
      do 10 j=1,LLN22S 
      gm(j) = 0.0
 10   continue
      truth = rtruth
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        a2 = ( 1.0 + sin( nproj * truth  * rad ) )
      else if( nproj.eq.0 ) then
        a2 = cos( truth * rad )
      endif
!
      do lat=1,latg2
!
        call pln2i(tmpqtt,tmpqvv,colrad,lat)
#ifdef MP
        call mpsf2p (jcap,lwvdef,tmpqtt,lnt2,tmpq,lnt2p,1)
        call mpnn2n (tmpq,lnt2p,qtt(1,lat),lln2p,1)
        do n=1,lln2p
          qww(n,lat)=qtt(n,lat)*wgt(lat)
        enddo
#else
        do n=1,lnt2
          qtt(n,lat)=tmpqtt(n)
          qww(n,lat)=tmpqtt(n)*wgt(lat)
        enddo
#endif
        len=2*jcap1
        j=len
        do l=0,jcap
          tmpqvv(2*l+1)=tmpqvv(j+1)
          tmpqvv(2*l+2)=tmpqvv(j+2)
          j=j+len
          len=len-2
        enddo
        do ll=1,llens
          j=lwvdef(llstr+ll)
          l=ll-1
          qvv(2*l+1,lat)=tmpqvv(2*j+1)
          qvv(2*l+2,lat)=tmpqvv(2*j+2)
        enddo
      enddo
!
!..........................................................
!           lat loop
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
      lat1=jstr
      lat2=jend
      latdon=jstr-1
#define SYNS syf
#else
#define SYNS syn
#endif
!
      do 1000 lat = lat1,lat2
      lan=lat-latdon
!
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        sinlat= cos(colrad(latdef(lat)))
        sinlat = nproj * sinlat
        xn = 1.0 + sinlat
        xs = 1.0 - sinlat
        rmn =  xn / a2
        rms =  xs / a2
      else if( nproj.eq.0 ) then
        rmn =  sin( colrad(latdef(lat)) ) / a2
        rms =  rmn
      endif
      if(me.eq.0) print *,' 1./mapping factor ',rmn,rms
!
      do 170 i=1,lonf 
      j = lonf  + i
      SYNS(i,2,lan) = rmn
      SYNS(j,2,lan) = rms
170   continue
!
      call fti_lonf(SYNS(1,2,lan),SYNS(1,1,lan),2,-1)
!
1000  continue
!
#ifdef MP
      call mpny2nl(jcapp,lonf,syf,lonf22 ,latg2p,                        &  
     &             syn,lcap22p,latg2 ,2,1,1,                             &
     &             lwvdef,latdef,jcap,latg)
!
      lat1=1
      lat2=latg2
      latdon=0
#endif
!
      do 1200 lat = lat1,lat2
      lan=lat-latdon
!
      call flpflm(flp(1,1,1,lan),flm(1,1,1,lan),syn(1,1,lan),            &
     &            llens,1                                                &
#ifndef MP
     &           ,jcap1,lonf,lonf22                                      &
#else
     &           ,jcap1p,lcapp,lcap22p                                   &
#endif
     &            )
!
      call fl2imp(flp(1,1,1,lan),flm(1,1,1,lan),gm,qww(1,lat),           &
     &            llstr,llens,lwvdef,1,jcap                              &
#ifndef MP
     &           ,lnt2,lnt22                                             &
#else
     &           ,jcapp,lln2p,lln22p                                     &
#endif
     &            )
!
1200  continue
!
#ifndef MP
      latdon=latdon+(lat2-lat1+1)
10000 continue
#endif
!
#ifdef MP
      call mpn2nn(gm,lln22p,gmtmp,lnt22p,1)
#else
      do j=1,lnt2 
        gmtmp(j)=gm(j)
      enddo
#endif
!
!.................................................................
!
#ifdef SPT
      call sphpt1(1,gmtmp,xm,blat,blon,lngrdb)
#endif
#ifdef SQK
      call sphqk1mp(1,gmtmp,xm,lwvdef,latdef,lngrdb)
#endif
! define regional grid
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        do 2000 i=1,lngrdb 
          sinclg = cos( blat(i) )
          xxrr = 0
          if( sinclg .ne. 0.0 ) xxrr = xm(i) / sinclg
          rgln = blon(i) - dlamda0
          rgln = nproj * rgln
          rcslb(i) = nproj * cos( rgln ) * xxrr
          rsnlb(i) = nproj * sin( rgln ) * xxrr
          rlonb(i) = blon(i)
          rlatb(i) = blat(i)
 2000   continue
      else if( nproj.eq.0 ) then
        do i=1,lngrdb
          sinclg = cos( blat(i) )
          xxrr = 0
          if( sinclg .ne. 0.0 ) xxrr = xm(i) / sinclg
          rsnlb(i) = -1.0 * xxrr
          rcslb(i) =  0.0
          rlonb(i) = blon(i)
          rlatb(i) = blat(i)
        enddo
      endif
!
#ifdef MP
      deallocate (syf)
      deallocate (tmpq)
#endif
      deallocate (syn)
      deallocate (flp)
      deallocate (flm)
      deallocate (tmpqtt)
      deallocate (tmpqvv)
      deallocate (gmtmp)
!--------------------------------------------------------
      return
      end
