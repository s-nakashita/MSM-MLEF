      subroutine g2rinp(iunit,nout,ivs,idvc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  g2rinp
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do global grid to regional grid interpolation
!
! program history log:
!
! usage:    g2rinp(iunit,nout)
!   input argument list:
!     iunit - unit number for global file input.
!     nout  - unit number for regional output file.

!
!   output argument list:
!
!   common blocks:
! gbli1 regi1 regi2 gbli2
!
!   input files:
! iunit
!
!   output files:
! nout
!
!   subprograms called:
! setlatlon  glats g2rini  gftlonf  gpln2i  epsilo  cmpind
! ggozrm  pln2i  ftilonf  fplfml  fl2i  maxmin  sphpt1  sphpt2
! dztouv  sums2i  sumtop  gg2rg  rwrite
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!....
      use machine , only : kind_io4
      use physcons, pi => con_pi, rocp => con_rocp, rocpr => con_cpor
!ccj
#include <paramodel.h>
#include <rscomloc.h>
!cc
! ... this version contain less memory to run but spend time .....
!...sof include..........................................
!...
      parameter ( mwave=jcap,                                            &                        
     &            imax=lonf, jmax=latg, kmax=levs,                       &
     &            imax2=lonf2, jmaxhf=latg2, ijmax=imax*jmax,            &
     &            maxwv=lnt, maxwv2=lnt2, maxwv22=lnt22,                 &
     &            muxwv=lnut, muxwv2=lnut2, muxwv22=lnut22) 
      parameter (levmax=100,nwext=44)
      real(kind=kind_io4) sisl(2*levmax+1),ext(nwext)
      real(kind=kind_io4) fhours,sis(kmax+1),sls(kmax),tmps(maxwv22)
      character*8 label(4)
      character*4 cgfs,csig
      real(kind=kind_io4),allocatable :: vcoords(:,:)
      real,allocatable :: vcoord(:,:)
      real del(kmax),ci(kmax+1)
      real cl(kmax),rpi(kmax-1)
      common /gbli1/  si(kmax+1), sl(kmax)                               &
     &  , gz(maxwv22), q(maxwv22)                                        &
     &  , te(maxwv22,kmax), rq(maxwv22,kmax,ntotal)                      &
     &  , uu(maxwv22,kmax), vv(maxwv22,kmax)                             &
     &  , di(maxwv22,kmax), ze(maxwv22,kmax)                             &
     &  ,uln(maxwv22,kmax),vln(maxwv22,kmax), gm(maxwv22)
      common /hydrid/ siin3d(lngrd,kmax+1),slin3d(lngrd,kmax)
!
      common /regi1i/ idate(4)
      common /regi1/ fhour,fsl(levp1),fsi(levs)                          &
     & ,flat(lngrd),flon(lngrd)                                          &
     & ,fm2(lngrd),fm2x(lngrd),fm2y(lngrd)
!
      common /regi2/ ggz(lngrd), gq(lngrd)                               &
     &  , gte(lngrd,levs), grq(lngrd,levs,ntotal)                        &
     &  , guu(lngrd,levs), gvv(lngrd,levs)                               &
#ifdef NONHYD
     &  , gpn(lngrd,levs), gtn(lngrd,levs)                               &
     &  , gwn(lngrd,levs+1)                                              &
#endif
     &  ,tmpregi2

!....
      parameter(numlev=levs*2+1)
      parameter(numlot=2*numlev)
      real, allocatable ::  syn(:,:) 
      real, allocatable ::  qtt(:), qvv(:), qww(:) 
      real, allocatable ::  fpl(:,:,:), fml(:,:,:) 
      real, allocatable ::  epsi(:,:) 
      real, allocatable ::  colrad(:), wgt(:),wgtcs(:),rcs2(:) 
      real, allocatable ::  gclat(:),  xm(:) 
      real, allocatable ::  rcsln(:), rsnln(:) 
      real, allocatable ::  csln(:), snln(:) 
      real, allocatable ::  precip(:),rain(:) 
      real, allocatable ::  dummy(:,:) 
      real, allocatable ::  uvtop(:,:,:)
!
      real(kind=kind_io4) fcsttime
      namelist/namsig/fcsttime
      data fcsttime/-9999./
!
      allocate ( syn(lonf22,numlot) )
      allocate ( qtt(maxwv2), qvv(muxwv2), qww(maxwv2) )
      allocate ( fpl(2,jcap1,numlev), fml(2,jcap1,numlev) )
      allocate ( epsi(jcap2,jcap1) )
      allocate ( colrad(latg2), wgt(latg2),wgtcs(latg2),rcs2(latg2) )
      allocate ( gclat(jmax),  xm(lngrd) )
      allocate ( rcsln(lngrd), rsnln(lngrd) )
      allocate ( csln(lonf2), snln(lonf2) )
      allocate ( precip(lonf*latg),rain(lngrd) )
      allocate ( dummy(lngrd,levs) )
      allocate ( uvtop(2,jcap1,levs*2) )
!.................................................................
! define regional grid
      call setlatlon(flat,flon,delx,dely,dlamda0)
! .....................................................
! define glats for colrad for dynamic grid
      call glats(latg2,colrad,wgt,wgtcs,rcs2)
      do 7 j=1,latg2
      gclat(j) = colrad(j)
      jj = latg - j + 1
      gclat(jj) = pi - colrad(j)
 7    continue
! .....................................................
! define global to regional grid interpolation in physic grids
      call g2rini(gclat,flat,flon)
      call gtorini(gclat,flat,flon)
! .....................................................
! define global constant
      hfpi = pi * 0.5
      nproj = rproj
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        dxlon = pi * 2.0 / lonf
        do i=1,lonf
          xlon = dxlon*(i-1) - dlamda0
          xlon = nproj * xlon
          sinlon = nproj * sin(xlon)
          coslon = nproj * cos(xlon)
          snln(i) = sinlon
          csln(i) = coslon
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
      call cmpind
      call gft_lonf
      call gpln2i
      call epsilo(epsi,jcap)
      call ggozrm(epsi)
! ...........................................................
! first loop to get maping factor in coefficiend then trancate
!
!           lat loop
      rad = pi / 180.0
      do 10 j=1,maxwv2
      gm(j) = 0.0
 10   continue
      truth = rtruth
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        a2 = ( 1.0 + sin( nproj * truth * rad ) )
      else if( nproj.eq.0 ) then
        a2 = cos( truth * rad )
      endif
!
      do 1000 lat=1,latg2
!
      call pln2i(qtt,qvv,colrad,lat)
!
!           lat loop
!
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        sinlat= cos(colrad(lat))
        sinlat= nproj * sinlat
        xn = 1.0 + sinlat
        xs = 1.0 - sinlat
        rmn =  xn / a2
        rms =  xs / a2
      else if( nproj.eq.0 ) then
        rmn =  sin( colrad(lat) ) / a2
        rms =  rmn
      endif
      print *,' 1./mapping factor ',rmn,rms
!
      latj = latg + 1 - lat
      do 170 i=1,lonf
      j = lonf + i
      syn(i,2) = rmn
      syn(j,2) = rms
170   continue
!
      call fti_lonf(syn(1,2),syn(1,1),2,-1)
!
      do 180 i=1,maxwv2
      qww(i)=qtt(i)*wgt(lat)
180   continue
!
      call fplfml(fpl(1,1,1),fml(1,1,1),syn(1,1),1)
      call fl2i(fpl(1,1,1),fml(1,1,1),gm,qww(1),1)
!
1000  continue
!
! obtained 1/m coefficient
!     call maxmin(gm,maxwv2,1,1,1,'1/m before cutwav')
!     print *,' gm before cut ',(gm(i),i=1,300)
!     call cutwav(gm,maxwv2,mwave,2,'h',1)
!     print *,' gm after cut ',(gm(i),i=1,300)
!     call maxmin(gm,maxwv2,1,1,1,'1/m after cutwav')
!
!c
!     begin=second()
      kone = 1
      print *,' debug one ',kone,gm(1),xm(1),lngrd
#ifdef SPT
      call sphpt1(kone,gm,xm,flat,flon,lngrd)
#endif
#ifdef SQK
      call sphqk1(kone,gm,xm,colrad,lngrd)
#endif
!     print *,' sphpt1 used ',second()-begin
! define regional grid
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        do i=1,lngrd
          sinclg = cos( flat(i) )
          xxrr = 0
          if( sinclg .ne. 0.0 ) xxrr = xm(i) / sinclg
          rgln = flon(i) - dlamda0
          rgln = nproj * rgln
          rcsln(i) = nproj * cos( rgln ) * xxrr
          rsnln(i) = nproj * sin( rgln ) * xxrr
        enddo
      else if( nproj.eq.0 ) then
        do i=1,lngrd
          sinclg = cos( flat(i) )
          xxrr = 0
          if( sinclg .ne. 0.0 ) xxrr = xm(i) / sinclg
          rsnln(i) = -1.0 * xxrr
          rcsln(i) =  0.0
        enddo
      endif
!     begin=second()
      kone = 1
      print *,' debug 2 kone=',kone
#ifdef SPT
      call sphpt2(kone,gm,fm2x,fm2y,flat,flon,rsnln,rcsln,lngrd)
#endif
#ifdef SQK
      call sphqk2(kone,gm,fm2x,fm2y,colrad,epsi,rcsln,rsnln,lngrd)
#endif
!     print *,' sphpt2 used ',second()-begin
      do 1600  ij=1,lngrd
      fm2(ij) = 1.0/ ( xm(ij) * xm(ij) )
      fm2x(ij) = -2.0 * fm2(ij)**1.5 * fm2x(ij)
      fm2y(ij) = -2.0 * fm2(ij)**1.5 * fm2y(ij)
 1600 continue
      call maxmin(fm2,lngrd,1,1,1,'fm2')
      call maxmin(fm2x,lngrd,1,1,1,'fm2x')
      call maxmin(fm2y,lngrd,1,1,1,'fm2y')
!....................................................
!     iunit=10
!     nout=50
! -------------------------------------------------------
! ...........................  time loop  ...............
! read global data
      rewind iunit
      read(iunit) label         
      if(label(1).eq.'GFS SIG ') then  ! modern sigma file
        rewind iunit
        read(iunit) cgfs,csig,ivs,nhead,ndata
        if(ivs.eq.200509) then
          read(iunit)
          read(iunit) fhours,idate,ijcap,ilevs,itrun,iorder,irealf,      &
     &                igen,ilatf,ilonf,ilatb,ilonb,ilatr,ilonr,intrac,   &
     &                nvcoord,icen2,iens1,iens2,idpp,idsl,idvc,idvm,     &
     &                idvt,idrun,idusr,pdryini,ncldt,ixgr
          allocate(vcoords(levs+1,nvcoord))
          allocate(vcoord(levs+1,nvcoord))
          read(iunit) vcoords
          call sgl2ful(vcoords,vcoord,nvcoord*(levs+1))
          read(iunit) 
          if ((idvc.eq.1).or.(idvc.eq.0)) then
           do k=1,levs
              si(k)=vcoord(k,1)
              if (idsl.eq.1.or.idsl.eq.0) then
                 sl(k)=(1-si(k+1)**(rocp+1))/((rocp+1)*(1-si(k+1)))
                 sl(k)=sl(k)**rocpr
              elseif (idsl.eq.2) then
                 sl(k)=(1+si(k+1))/2.0
              else
                 write(*,*) 'no support idsl=',idsl
                 stop
              endif
           enddo
           si(levs+1)=vcoord(levs+1,1)
          elseif (idvc.eq.2) then
! create false si & sl to fit file format
           si=0.0
           sl=0.0
          else
           write(*,*) 'no support idvc=',idvc
           stop
          endif
        else
          write(*,*) 'sig file error'
          stop
        endif
      else
        read(iunit) fhours,idate,sisl,ext
        ijcap=ext(1)
        ilevs=ext(2)
        itrun=ext(3)
        iorder=ext(4)
        irealf=ext(5)
        igen=ext(6)
        ilonf=ext(7)
        ilatf=ext(8)
        ilonb=ext(9)
        ilatb=ext(10)
        ilonr=ext(11)
        ilatr=ext(12)
        intrac=ext(13)
        icen2=ext(14)
        iens1=ext(15)
        iens2=ext(16)
        idpp=ext(17)
        idsl=ext(18)
        idvc=ext(19)
        idvm=ext(20)
        idvt=ext(21)
        idrun=ext(22)
        idusr=ext(23)
        pdryini=ext(24)
        ncldt=ext(25)
        ixgr=ext(26)
        if ((idvc.eq.1).or.(idvc.eq.0)) then
           si(1:(levs+1))=sisl(1:(levs+1))
           sl(1:levs)=sisl((levs+2):(2*levs+1))
        elseif (idvc.eq.2) then
           allocate(vcoord(levs+1,2))
           vcoord(1:(levs+1),1)=sisl(1:(levs+1))
           vcoord(1:(levs+1),2)=sisl((levs+2):2*(levs+1))
! create false si & sl to fit file format
           si=0.0
           sl=0.0
        else
           write(*,*) 'no support idvc=',idvc
           stop
        endif
      endif
!
      read (5,namsig)
      if(fcsttime.eq.-9999.) then
        fhour=fhours
      else
        fhour=fcsttime
      endif

      fhour=fhours
      write(6,210) (idate(i),i=1,4),fhour,                               &           
     &            (si(k),k=1,kmax+1),(sl(k),k=1,kmax)
210   format(5x,' input date and fcst hour ',4i5,f7.1/(2x,g13.6))
!
      read(iunit) (tmps(nw),nw=1,maxwv2)
      call sgl2ful(tmps,gz,maxwv2)
      read(iunit) (tmps(nw),nw=1,maxwv2)
      call sgl2ful(tmps,q,maxwv2)
      do 220 k=1,kmax
      read(iunit) (tmps(nw),nw=1,maxwv2)
      call sgl2ful(tmps,te(1,k),maxwv2)
220   continue
      do 230 k=1,kmax
      read(iunit) (tmps(nw),nw=1,maxwv2)
      call sgl2ful(tmps,di(1,k),maxwv2)
      read(iunit) (tmps(nw),nw=1,maxwv2)
      call sgl2ful(tmps,ze(1,k),maxwv2)
230   continue
      do 240 k=1,kmax
      read(iunit) (tmps(nw),nw=1,maxwv2)
      call sgl2ful(tmps,rq(1,k,1),maxwv2)
240   continue
!ccj
       if(ntoz.eq.1) then
         do kt = 1,ntoz
           ktrace = 1+kt
           do k=1,kmax
             read(iunit)(tmps(kw),kw=1,maxwv2)
             call sgl2ful(tmps,rq(1,k,ktrace),maxwv2)
           enddo
         enddo
       endif
!
       if(ncldb.ge.1) then
         do kc = 1,ncldb
           kcloud = 1+ntoz+kc
           do k=1,kmax
             read(iunit)(tmps(kw),kw=1,maxwv2)
             call sgl2ful(tmps,rq(1,k,kcloud),maxwv2)
           enddo
         enddo
       endif
!
       if(ncldb.eq.0.and.ncld.ge.1) then
         do kc = 1,ncld
           kcloud = 1+ntoz+kc
           do k=1,kmax
           do i=1,maxwv2
             rq(i,k,kcloud)=0.
           enddo
           enddo
         enddo
       endif
!cc
!.................................................................
! second loop to do ustar, vstar
      call maxmin(di,maxwv2,kmax,1,kmax,'div global coef')
      call maxmin(ze,maxwv2,kmax,1,kmax,'vor global coef')
      call dzuvle(di,ze,uln,vln,uvtop(1,1,1),uvtop(1,1,levs+1))
      call maxmin(uln,maxwv2,kmax,1,kmax,'uln global coef')
      call maxmin(vln,maxwv2,kmax,1,kmax,'vln global coef')
      do 265 k=1,kmax
      do 265 j=1,maxwv2
      uu(j,k) = 0.0
      vv(j,k) = 0.0
 265  continue
!
!           lat loop
!
      do 2000 lat=1,latg2
!
      call pln2i(qtt,qvv,colrad,lat)
!
      call sums2i(uln ,syn(1,       1),qtt,kmax*2+1)
!x    call sums2i(vln ,syn(1,  kmax+1),qtt,kmax)
!x    call sums2i(gm  ,syn(1,2*kmax+1),qtt,1)
      call sumtop(syn(1,     1),uvtop,qvv,kmax*2,lonf,lonf/2)
!x    call sumtop(syn(1,kmax+1),vtop,qvv,kmax,lonf,lonf/2)
!
!
      lot   = 2*kmax+1
      lotnfs=(2*kmax+1)*2
!     
      call fti_lonf(syn(1,1),empty,lotnfs,1)
!
      coslat = cos( hfpi - colrad(lat) )
      rcslt = 0.0
      if( coslat.ne.0.0 ) rcslt = 1. / coslat
      km2p1 = kmax*2 + 1
      do 270 j=1,lonf2
      syn(j, km2p1) = syn(j, km2p1) * rcslt
  270 continue
!
      do 273 k=1,kmax
      do 273 j=1,lonf2
      syn(j,     k) = syn(j,     k) * syn(j,km2p1)
      syn(j,kmax+k) = syn(j,kmax+k) * syn(j,km2p1)
  273 continue
      do 275 k=1,kmax
      do 275 j=1,lonf2
      syn(j,2*kmax+k)= -syn(j,k)*snln(j)-syn(j,kmax+k)*csln(j)
      syn(j,3*kmax+k)=  syn(j,k)*csln(j)-syn(j,kmax+k)*snln(j)
  275 continue
!
      lotnfa=(2*kmax)*2
      call fti_lonf(syn(1,2*kmax+1),syn(1,1),lotnfa,-1)
!
      do 280 i=1,maxwv2
      qww(i)=qtt(i)*wgt(lat)
280   continue
!
      call fplfml(fpl,fml,syn(1,1),kmax)
      call fl2i(fpl,fml,uu,qww,kmax)
      call fplfml(fpl,fml,syn(1,kmax+1),kmax)
      call fl2i(fpl,fml,vv,qww,kmax)
!
 2000 continue
      call maxmin(uu,maxwv2,kmax,1,kmax,'u/m global coef')
      call maxmin(vv,maxwv2,kmax,1,kmax,'v/m global coef')
! .................................................
! third loop get all full varaibles gz, q, te, uu, vv, rq.
!
#ifdef SPT
      kmall=2+3*levs+levs*ntotal
      call sphpt1(kmall,gz,ggz,flat,flon,lngrd)
#endif
!
!-sqk kmall=2+4*levs
!-sqk call sphqk1(kmall,gz,ggz,colrad,lngrd)
      kone = 1
      print *,' debug 3 kone=',kone
#ifdef SQK
      call sphqk1(    kone,gz,ggz,colrad,lngrd)
      call sphqk1(    kone, q, gq,colrad,lngrd)
      call sphqk1(levs,te,gte,colrad,lngrd)
!ccj
      do nc = 1,ntotal
        call sphqk1(levs,rq(1,1,nc),grq(1,1,nc),colrad,lngrd)
      enddo
!cc
      call sphqk1(levs,uu,guu,colrad,lngrd)
      call sphqk1(levs,vv,gvv,colrad,lngrd)
#endif
!
! caculate siin3d & slin3d for sigma-p input data
        if (idvc.eq.2) then
           do i=1,lngrd
           ps=exp(gq(i))*1000
           pid=ps
           do k=1,levs
              piu=vcoord(k+1,1)+vcoord(k+1,2)*ps
              if (idsl.eq.1.or.idsl.eq.0) then
                 pidk=pid**rocp
                 piuk=piu**rocp
                 pm=(pid*pidk-piu*piuk)/((rocp+1)*(pid-piu))
                 pm=pm**rocpr
              elseif (idsl.eq.2) then
                 pm=(pid+piu)/2.0
              else
                 write(*,*) 'no support idsl=',idsl
                 stop
              endif
              siin3d(i,k+1)=piu/ps
              slin3d(i,k)=pm/ps
              pid=piu
           enddo
           enddo
           siin3d(:,1)=1.0
        elseif ((idvc.eq.0).or.(idvc.eq.1)) then
           do i=1,lngrd
              siin3d(i,:)=si
              slin3d(i,:)=sl
           enddo
        else
           write(*,*) 'no support idvc=',idvc
           stop
        endif
!
#ifdef NONHYD
      do k=1,levs
        do i=1,lngrd
          gtn(i,k)=gte(i,k)
          gpn(i,k)=gq(i)+log(sl(k))
        enddo
      enddo
      do k=1,levs+1
        do i=1,lngrd
          gwn(i,k)=0.0
        enddo
      enddo
#endif
!
      call maxmin(ggz,lngrd,1,1,1,'gz full')
      call maxmin( gq,lngrd,1,1,1,' q full')
      call maxmin(gte,lngrd,levs,1,levs,'te full')
      call maxmin(grq(1,1,1),lngrd,levs,1,levs,'rq full')
      if(ntoz.eq.1)                                                      &                      
     & call maxmin(grq(1,1,2),lngrd,levs,1,levs,'ozone full')
      if(ncld.eq.1)                                                      &
     & call maxmin(grq(1,1,3),lngrd,levs,1,levs,'cloud water full')
      call maxmin(guu,lngrd,levs,1,levs,'uu full')
      call maxmin(gvv,lngrd,levs,1,levs,'vv full')
#ifdef NONHYD
      call maxmin(gpn,lngrd,levs,1,levs,'pn full')
      call maxmin(gtn,lngrd,levs,1,levs,'tn full')
      call maxmin(gwn,lngrd,levs+1,1,levs+1,'wn full')
#endif
      call maxmin(fm2,lngrd,1,1,1,'fm2')
!
! ...............................................
!
7777   print *,' === write regional data at ',fhour
       call rwrite(nout,label,fhour,idate,kmax,                          &  
     &             flat,flon,fm2,fm2x,fm2y,                              &  
     &             sl,si,ggz,gq,gte,guu,gvv,grq                          &
#ifdef NONHYD
     &                             ,gpn,gtn,gwn                          &
#endif
     &                                         )
!......................................................
      deallocate ( syn )
      deallocate ( qtt, qvv, qww )
      deallocate ( fpl, fml )
      deallocate ( epsi )
      deallocate ( colrad, wgt,wgtcs,rcs2 )
      deallocate ( gclat,  xm )
      deallocate ( rcsln, rsnln )
      deallocate ( csln, snln )
      deallocate ( precip,rain )
      deallocate ( dummy )
      deallocate ( uvtop )
!
      return
      end
