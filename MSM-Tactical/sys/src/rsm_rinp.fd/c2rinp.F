      subroutine c2rinp(ncsig,nrsig,ncsfc,nrsfc,nrslm)
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
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
#include <rscomloc.h>
!cc
!...
      parameter (imax=cigrd1, jmax=cjgrd1)
      character*8 label(4)
!
      real(kind=kind_io4) fhours,sis(levs+1),sls(levs)                  & 
     &         ,cgs(clngrd), cgls(clngrd,lsoil)                         &
     &         ,cg2s(clngrd,2), cg4s(clngrd,4)                          &
     &         ,rgs(lngrd),  rgls(lngrd,lsoil)                          &
     &         ,rg2s(lngrd,2),  rg4s(lngrd,4)
!
      real, allocatable :: slg(:)
      real, allocatable :: cg(:)  , cgl(:,:)
      real, allocatable :: cg2(:,:), cg4(:,:)
      integer, allocatable :: iwork(:),idate(:)
      real, allocatable :: cgsea(:),cgland(:),rr(:)
      real, allocatable :: sl(:),si(:)
      real, allocatable :: flat(:),flon(:)
      real, allocatable :: cm2(:)
      real, allocatable :: fm2(:),fm2x(:),fm2y(:)
      real, allocatable ::  dflat(:),dflon(:)
!
      real, allocatable :: gz(:), q(:)
      real, allocatable :: te(:,:), rq(:,:,:)
      real, allocatable :: uu(:,:), vv(:,:)
#ifdef NONHYD
      real, allocatable :: pp(:,:), tt(:,:) 
      real, allocatable :: ww(:,:)
#endif
      real, allocatable :: rain(:),cvrn(:)
      real, allocatable :: rcsln(:),rsnln(:)
!....
      real, allocatable :: tsea  (:), smc   (:,:)
      real, allocatable :: sheleg(:), stc   (:,:)
      real, allocatable :: tg3   (:), vfrac (:)
      real, allocatable :: zorl  (:), slmsk (:)
      real, allocatable :: cv    (:), cvb   (:)
      real, allocatable :: cvt   (:)
      real, allocatable :: f10m  (:), canopy(:)
      real, allocatable :: vtype (:)
      real, allocatable :: stype (:), uustar(:)
      real, allocatable :: ffmm  (:),   ffhh(:)
      real, allocatable :: alvsf (:),  alvwf(:)
      real, allocatable :: alnsf (:),  alnwf(:)
      real, allocatable :: facsf (:),  facwf(:)
      real, allocatable :: slmk(:),simk(:)
      real, allocatable :: tg3clm(:),zorclm(:)
      real, allocatable :: albclm(:,:),vegclm(:)
      real, allocatable :: vetclm(:),sotclm(:),alfclm(:,:)

      integer version
      data version/199802/
      character*500 fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh
      namelist/namclim/iclim,fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc, &
     &                 fnmskh
      common /hydrid/ siin3d(lngrd,levs+1),slin3d(lngrd,levs)

      data fntg3c/'global_tg3clim.2.6x1.5.grb'/
      data fnmskh/'global_slmask.t126.grb'/
      data fnalbc/'global_albedo4.1x1.grb'/
      data fnsotc/'global_soiltype.1x1.grb'/
      data fnvegc/'global_vegfrac.1x1.grb'/
      data fnvetc/'global_vegtype.1x1.grb'/
      data fnzorc/'global_zorclim.1x1.grb'/

      print *,' enter c2rinp '
!
      allocate ( slg(clngrd) )
      allocate ( cg(clngrd)  , cgl(clngrd,lsoil) )
      allocate ( cg2(clngrd,2), cg4(clngrd,4) )
      allocate ( cgsea(clngrd),cgland(clngrd) )
      allocate ( iwork(clngrd) )
      allocate ( idate(4),sl(levs),si(levs+1) )
      allocate ( flat(lngrd),flon(lngrd) )
      allocate ( cm2(lngrd) )
      allocate ( fm2(lngrd),fm2x(lngrd),fm2y(lngrd) )
      allocate ( dflat(lngrd),dflon(lngrd) )
!
      allocate ( gz(lngrd), q(lngrd) )
      allocate ( te(lngrd,levs), rq(lngrd,levs,ntotal) )
      allocate ( uu(lngrd,levs), vv(lngrd,levs) )
#ifdef NONHYD
      allocate ( pp(lngrd,levs), tt(lngrd,levs) )
      allocate ( ww(lngrd,levs+1) )
#endif
      allocate ( rain  (lngrd), cvrn  (lngrd) )
      allocate ( rcsln (lngrd), rsnln (lngrd) )
!....
      allocate ( rr    (lngrd))
      allocate ( tsea  (lngrd), smc   (lngrd,lsoil) )
      allocate ( sheleg(lngrd), stc   (lngrd,lsoil) )
      allocate ( tg3   (lngrd), vfrac (lngrd) )
      allocate ( zorl  (lngrd), slmsk (lngrd) )
      allocate ( cv    (lngrd), cvb   (lngrd) )
      allocate ( cvt   (lngrd) )
      allocate ( f10m  (lngrd), canopy(lngrd) )
      allocate ( vtype (lngrd) )
      allocate ( stype (lngrd), uustar(lngrd) )
      allocate ( ffmm  (lngrd),   ffhh(lngrd) )
      allocate ( alvsf (lngrd),  alvwf(lngrd) )
      allocate ( alnsf (lngrd),  alnwf(lngrd) )
      allocate ( facsf (lngrd),  facwf(lngrd) )
      allocate ( slmk  (lngrd),   simk(lngrd) )
      allocate ( tg3clm(lngrd), zorclm(lngrd) )
      allocate ( albclm(lngrd,4),vegclm(lngrd) )
      allocate ( vetclm(lngrd),sotclm(lngrd),alfclm(lngrd,2) )

!.................................................................
! define regional grid
      call setlatlon(flat,flon,delx,dely,dlamda0)
      call maxmin(flat,lngrd,1,1,1,' flat ')
      call maxmin(flon,lngrd,1,1,1,' flon ')
! .....................................................
! define coarse regional to regional grid interpolation
      call c2rini(ncsig,flat,flon)
      call ctorini(ncsig,flat,flon)
! ................................................
! define regional map factors
      rad = pi / 180.0
      hfpi= 0.5 * pi
      truth = rtruth
      nproj = rproj
      if( nproj.eq.1 .or. nproj.eq.-1 ) then
        orient = rorient
        dlamda0 = orient * rad + hfpi
        a2 = ( 1.0 + sin( nproj * truth * rad ) )
        do n=1,lngrd
          fm2(n)=(1.+sin( nproj * flat(n) ))/a2  ! 1/(map factor)
          rgln = flon(n) - dlamda0
          rgln = nproj * rgln
          rcsln(n) = nproj * cos( rgln ) 
          rsnln(n) = nproj * sin( rgln )
        enddo
      else if( nproj.eq.0 ) then
        a2 = cos( truth * rad )
        do n=1,lngrd
          fm2(n)=cos( flat(n) )/a2               ! 1/(map factor)
          rcsln(n) =  0.0
          rsnln(n) = -1.0 
        enddo
      endif
!
      call todxdy(fm2,fm2x,fm2y,igrd1,jgrd1,rdelx,rdely)
!
      do ij=1,lngrd
      fm2(ij) = 1.0/ ( fm2(ij) * fm2(ij) )
      fm2x(ij) = -2.0 * fm2(ij)**1.5 * fm2x(ij)
      fm2y(ij) = -2.0 * fm2(ij)**1.5 * fm2y(ij)
      enddo
      call maxmin(fm2,lngrd,1,1,1,'fm2')
      call maxmin(fm2x,lngrd,1,1,1,'fm2x')
      call maxmin(fm2y,lngrd,1,1,1,'fm2y')
!....................................................
!     iunit=10
! do sig file or not
      if( nrsig.ne.0 ) then
! ....................................................
! read coarse regional data
        rewind ncsig
        rewind nrsig
        read(ncsig) label
        write(6,*) label
        read(ncsig) fhours,(idate(i),i=1,4),                             &          
     &            (sis(k),k=1,levs+1),(sls(k),k=1,levs)
        fhour=fhours
        call sgl2ful(sis,si,levs+1)
        call sgl2ful(sls,sl,levs)
! use siin3d & slin3d to transfer si & sl
        do i=1,lngrd
              siin3d(i,:)=si
              slin3d(i,:)=sl
        enddo
!
        write(6,210) (idate(i),i=1,4),fhour,                             &
     &            (si(k),k=1,levs+1),(sl(k),k=1,levs)
210     format(5x,' input date and fcst hour ',4i5,f7.1/(2x,g13.6))
! gz
        read(ncsig) (cgs(i),i=1,clngrd)
        call sgl2ful(cgs,cg,clngrd)
        call cgtorg(cg,gz,1)
! q
        read(ncsig) (cgs(i),i=1,clngrd)
        call sgl2ful(cgs,cg,clngrd)
        call cgtorg(cg,q,1)
! te
        do k=1,levs
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,te(1,k),1)
        enddo
! uu vv
        do k=1,levs
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,uu(1,k),1)
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,vv(1,k),1)
        enddo
! rq
        do k=1,levs
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,rq(1,k,1),1)
        enddo
!ccj
       if(ntoz.eq.1) then
         do ntt = 1,ntoz
           ktrace = 1+ntt
           do k=1,levs
             read(ncsig)(cgs(i),i=1,clngrd)
             call sgl2ful(cgs,cg,clngrd)
             call cgtorg(cg,rq(1,k,ktrace),1)
           enddo
         enddo
       endif
!
       if(ncldb.ge.1) then
         do nc = 1,ncldb
           kcloud = 1+ntoz+nc
           do k=1,levs
             read(ncsig)(cgs(i),i=1,clngrd)
             call sgl2ful(cgs,cg,clngrd)
             call cgtorg(cg,rq(1,k,kcloud),1)
           enddo
         enddo
       endif
!
       if(ncldb.eq.0.and.ncld.ge.1) then
         do kc = 1,ncld
           kcloud = 1+ntoz+kc
           do k=1,levs
           do i=1,clngrd
             rq(i,k,kcloud)=0.
           enddo
           enddo
         enddo
       endif
!cc
#ifdef NONHYD
! pp
#ifdef N2R
        do k=1,levs
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,pp(1,k),1)
        enddo
#else
        do k=1,levs
         do i=1,lngrd
          pp(i,k)=q(i)+log(sl(k))
         enddo
        enddo
#endif
! tt
#ifdef N2R
        do k=1,levs
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,tt(1,k),1)
        enddo
#else
        do k=1,levs
         do i=1,lngrd
          tt(i,k)=te(i,k)
         enddo
        enddo
#endif
! ww
#ifdef N2R
        do k=1,levs+1
          read(ncsig) (cgs(i),i=1,clngrd)
          call sgl2ful(cgs,cg,clngrd)
          call cgtorg(cg,ww(1,k),1)
        enddo
#else
        do k=1,levs+1
         do i=1,lngrd
          ww(i,k)=0.0
         enddo
        enddo
#endif
#endif
! fm2
        read(ncsig) (cgs(i),i=1,clngrd)
        call sgl2ful(cgs,cg,clngrd)
        call cgtorg(cg,cm2,1)
! fm2x
        read(ncsig) (cgs(i),i=1,clngrd)
! fm2y
        read(ncsig) (cgs(i),i=1,clngrd)
! flat
        read(ncsig) (cgs(i),i=1,clngrd)
! flon
        read(ncsig) (cgs(i),i=1,clngrd)
!.................................................................
! define cgrid project transformation for u and v
      nproj = cproj
      orient = corient
      dlamda0 = orient * rad + hfpi
        do n=1,lngrd
          if( nproj.eq.1 .or. nproj.eq.-1 ) then
            call ll2xy(flat(n),flon(n),x,y)
            call xy2ll(x,y,clat,clon)
            rgln = clon - dlamda0
            rgln = nproj * rgln
            csll = nproj * cos( rgln )
            snll = nproj * sin( rgln )
          else if( nproj.eq.0 ) then
            csll =  0.0
            snll = -1.0 
          endif
          csmm = rcsln(n)
          snmm = rsnln(n)
          cmorm = sqrt( cm2(n) / fm2(n) )
          rcsln(n) = ( csll * csmm + snll * snmm ) * cmorm
          rsnln(n) = ( csll * snmm - snll * csmm ) * cmorm
        enddo
!
! rotate u v to new projection
        
        do k=1,levs
          do n=1,lngrd
            hold = uu(n,k)
            uu(n,k) = hold * rcsln(n) - vv(n,k) * rsnln(n)
            vv(n,k) = hold * rsnln(n) + vv(n,k) * rcsln(n)
          enddo
        enddo
!
        call maxmin(gz,lngrd,1,1,1,'gz full')
        call maxmin( q,lngrd,1,1,1,' q full')
        call maxmin(te,lngrd,levs,1,levs,'te full')
        call maxmin(rq(1,1,1),lngrd,levs,1,levs,'rq full')
        if(ntoz.eq.1)                                                    &         
     &   call maxmin(rq(1,1,2),lngrd,levs,1,levs,'ozone full')
        if(ncld.eq.1)                                                    &
     &   call maxmin(rq(1,1,3),lngrd,levs,1,levs,'cloud water full')
        call maxmin(uu,lngrd,levs,1,levs,'uu full')
        call maxmin(vv,lngrd,levs,1,levs,'vv full')
#ifdef NONHYD
        call maxmin(pp,lngrd,levs,1,levs,'pp full')
        call maxmin(tt,lngrd,levs,1,levs,'tt full')
        call maxmin(ww,lngrd,levs+1,1,levs+1,'ww full')
#endif
        call maxmin(fm2,lngrd,1,1,1,'fm2 full')
        call maxmin(fm2x,lngrd,1,1,1,'fm2x full')
        call maxmin(fm2y,lngrd,1,1,1,'fm2y full')
        call maxmin(flat,lngrd,1,1,1,'flat full')
        call maxmin(flon,lngrd,1,1,1,'flon full')
!
7777    print *,' === write regional data at ',fhour
!
! ...............................................
!
        call rwrite(nrsig,label,fhour,idate,levs,                        &          
     &             flat,flon,fm2,fm2x,fm2y,                              &
     &             sl,si,gz,q,te,uu,vv,rq                                &
#ifdef NONHYD
     &                          ,pp,tt,ww                                &
#endif
     &                                   )
!......................................................
        print *,' end of sig file interpolation. '
!
      endif
! ==============================================================
!.............................................................
! do sfc or not
      if( nrsfc.ne.0 ) then
!
        print *,' ncsfc=',ncsfc,' nrsfc=',nrsfc
!
! read initial surface condition
!
        rewind ncsfc
!
! get slmask first
        read(ncsfc) label
        read(ncsfc) fhours,idate
        fhour=fhours
        read(ncsfc)               ! tsea
        read(ncsfc)               ! smc
        read(ncsfc) cgs          ! sheleg
        call sgl2ful(cgs,cg,clngrd)
        call cg2rg(cg,sheleg,1)
        read(ncsfc)               ! stc
        read(ncsfc)               ! tg3
        read(ncsfc)               ! zorl
        read(ncsfc)               ! cv
        read(ncsfc)               ! cvb
        read(ncsfc)               ! cvt
        read(ncsfc)               ! alvsf,alvwf,alnsf,alnwf
        read(ncsfc) cgs          ! slmsk
        call sgl2ful(cgs,slg,clngrd)
! get sea and ice index
        do 10 i=1,clngrd
        cg(i) = 0.0
        if( slg(i) .eq. 2.0 ) cg(i) = 2.0
 10     continue
        call cg2rg(cg,simk,1)
        do 20 i=1,lngrd
        if( simk(i) .gt. 0.9 ) then
          simk(i) = 2.0
        else
          simk(i) = 0.0
        endif
 20     continue
! get sea and land index
        do 30 i=1,clngrd
        cg(i) = 0.0
        if( slg(i) .eq. 1.0 ) cg(i) = 1.0
 30     continue
        call cg2rg(cg,slmk,1)
        do 40 i=1,lngrd
        if( slmk(i) .gt. 0.4 ) then
          slmk(i) = 1.0
        else
          slmk(i) = 0.0
        endif
 40     continue
!        do 45 i=1,lngrd
!        slmsk(i) = max( simk(i), slmk(i) )
!  45    continue
!
! get regional land-sea mask
        rewind nrslm
        read(nrslm) rgs  
        call sgl2ful(rgs,slmsk,lngrd)
        rewind nrslm
! before add sea ice info
! read climatology data
      read(5,namclim)
      if (iclim.eq.1) then
        dflat=flat*180/pi
        dflon=flon*180/pi
        call clima(idate(4),idate(2),idate(3),idate(1),fhour,             &
     &             lngrd,lsoil, slmsk,dflat,dflon,                        &
     &             fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh,      &
     &             tg3clm,zorclm,albclm,vegclm,vetclm,sotclm,alfclm)
      endif
!
        do 45 i=1,lngrd
! add sea ice info
        slmsk(i) = max(slmsk(i),simk(i))
 45     continue
!
! update climate var sea ice info
      if (iclim.eq.1) then
        do i=1,lngrd
          if (slmsk(i).eq.2.0) then
            vetclm(i)=13.0
            sotclm(i)=9.0
            zorclm(i)=1.0
          endif
        enddo
      endif

!
! put sea ice point into sea point
        do i=1,clngrd
        if(slg(i).eq.2.0) slg(i)=0.0
        enddo
!
!------ read from gsm, interpolate to rsm, and save to nrsfc -----
        rewind ncsfc
        rewind nrsfc
! label --- 1
        read(ncsfc) label
        write(nrsfc) label
! hour idate --- 2
        read(ncsfc) fhours,idate
        write(nrsfc) fhours,idate,igrd1,jgrd1,version
! tsea 3
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,tsea,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,tsea,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) tsea(i)=rr(i)
        enddo
        do 50 i=1,lngrd
          if( slmsk(i) .eq. 2.0e0 ) then
            tsea(i)= min( tsea(i), 271.20e0 )
          else if( slmsk(i).eq.0.0e0 ) then
            tsea(i)= max( tsea(i), 271.21e0 )
          else if( slmsk(i).eq.1.0e0 .and.                               &   
     &         sheleg(i).gt.0.0e0 ) then
            tsea(i)= min( tsea(i), 273.16e0 )
          endif
 50     continue
        call maxmin(  tsea,lngrd,1,1,1,'tsea')
        call ful2sgl( tsea,rgs,lngrd)
        write(nrsfc)  rgs
! soil moisture ----- 4
        read(ncsfc) cgls
        call sgl2ful(cgls,cgl,clngrd*lsoil)
!       call cg2rg(cgl,smc,lsoil)
        do k=1,lsoil
          call expnd(cgl(1,k),slg,0,cgsea,iwork,imax,jmax)
          call expnd(cgl(1,k),slg,1,cgland,iwork,imax,jmax)
          call cg2rg(cgsea,smc(1,k),1)
          call cg2rg(cgland,rr,1)
          do i=1,lngrd
            if (slmsk(i).eq.1.0) smc(i,k)=rr(i)
          enddo
        enddo
        call maxmin(   smc,lngrd,lsoil,1,lsoil,'smc')
        call ful2sgl(  smc,rgls,lngrd*lsoil)
        write(nrsfc)   rgls
! snow depth ----- 5
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,sheleg,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,sheleg,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) sheleg(i)=rr(i)
        enddo
        call maxmin(  sheleg,lngrd,1,1,1,'sheleg')
        call ful2sgl( sheleg,rgs,lngrd)
        write(nrsfc)  rgs
! stc ------------ 6
        read(ncsfc) cgls
        call sgl2ful(cgls,cgl,clngrd*lsoil)
!       call cg2rg(cgl,stc,lsoil)
        do k=1,lsoil
          call expnd(cgl(1,k),slg,0,cgsea,iwork,imax,jmax)
          call expnd(cgl(1,k),slg,1,cgland,iwork,imax,jmax)
          call cg2rg(cgsea,stc(1,k),1)
          call cg2rg(cgland,rr,1)
          do i=1,lngrd
            if (slmsk(i).eq.1.0) stc(i,k)=rr(i)
          enddo
        enddo
        call maxmin(   stc,lngrd,lsoil,1,lsoil,'stc')
        call ful2sgl(  stc,rgls,lngrd*lsoil)
        write(nrsfc)   rgls
! tg3 ------------ 7
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,tg3,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,tg3,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) tg3(i)=rr(i)
        enddo
        call maxmin(  tg3,lngrd,1,1,1,'tg3')
        if (iclim.eq.1) then
           call ful2sgl( tg3clm,rgs,lngrd)
        else
           call ful2sgl( tg3,rgs,lngrd)
        endif
        write(nrsfc)  rgs
! roughness ----- 8
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,zorl,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,zorl,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) zorl(i)=rr(i)
        enddo
        call maxmin(  zorl,lngrd,1,1,1,'zorl')
        if (iclim.eq.1) then
          call ful2sgl( zorclm,rgs,lngrd)
        else
          call ful2sgl( zorl,rgs,lngrd)
        endif
        write(nrsfc)  rgs
! cloud amount -- 9
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
        call cg2rg(cg,cv,1)
! cloud base --- 10
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
        do i=1,clngrd
          if(cg(i).eq.100.) cg(i)=0.0
        enddo
        call cg2rg(cg,cvb,1)
        do i=1,lngrd
          if(cvb(i).eq.0.0) cvb(i)=100.
        enddo
! cloud top ---- 11
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
        call cg2rg(cg,cvt,1)
! consistency check among cv cvb and cvt
        do i=1,lngrd
          if(cvt(i).lt.cvb(i) .or. cv(i).eq.0.0) then
            cv (i)=0.0
            cvt(i)=0.0
            cvb(i)=100.0
          endif
        enddo
        call maxmin(  cv, lngrd ,1,1,1,'cv')
        call ful2sgl( cv,rgs, lngrd )
        write(nrsfc)  rgs
        call maxmin(  cvb, lngrd ,1,1,1,'cvb')
        call ful2sgl( cvb,rgs, lngrd )
        write(nrsfc)  rgs
        call maxmin(  cvt, lngrd ,1,1,1,'cvt')
        call ful2sgl( cvt,rgs, lngrd )
        write(nrsfc)  rgs
! four albedoes ------- 12
        read(ncsfc) cg4s
        call sgl2ful(cg4s,cg4,clngrd*4)
!       call cg2rg(cg4(1,1),alvsf,1)
!       call cg2rg(cg4(1,2),alvwf,1)
!       call cg2rg(cg4(1,3),alnsf,1)
!       call cg2rg(cg4(1,4),alnwf,1)
        call expnd(cg4(1,1),slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg4(1,1),slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,alvsf,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) alvsf(i)=rr(i)
        enddo
        call expnd(cg4(1,2),slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg4(1,2),slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,alvwf,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) alvwf(i)=rr(i)
        enddo
        call expnd(cg4(1,3),slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg4(1,3),slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,alnsf,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) alnsf(i)=rr(i)
        enddo
        call expnd(cg4(1,4),slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg4(1,4),slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,alnwf,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) alnwf(i)=rr(i)
        enddo
        call maxmin(  alvsf,lngrd,1,1,1,'alvsf')
        call maxmin(  alvwf,lngrd,1,1,1,'alvwf')
        call maxmin(  alnsf,lngrd,1,1,1,'alnsf')
        call maxmin(  alnwf,lngrd,1,1,1,'alnwf')
        if (iclim.eq.1) then
          call ful2sgl( albclm(1,1),rg4s(1,1),lngrd)
          call ful2sgl( albclm(1,2),rg4s(1,2),lngrd)
          call ful2sgl( albclm(1,3),rg4s(1,3),lngrd)
          call ful2sgl( albclm(1,4),rg4s(1,4),lngrd)
        else
          call ful2sgl( alvsf,rg4s(1,1),lngrd)
          call ful2sgl( alvwf,rg4s(1,2),lngrd)
          call ful2sgl( alnsf,rg4s(1,3),lngrd)
          call ful2sgl( alnwf,rg4s(1,4),lngrd)
        endif
        write(nrsfc)  rg4s
! sea land mask -- 13
        read(ncsfc) cgs
        call maxmin(  slmsk,lngrd,1,1,1,'slmsk')
        call ful2sgl( slmsk,rgs,lngrd)
        write(nrsfc)  rgs
! vegitation fraction --14
        read(ncsfc) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,vfrac,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,vfrac,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) vfrac(i)=rr(i)
        enddo
        call maxmin(  vfrac,lngrd,1,1,1,'vfrac')
        if (iclim.eq.1) then
          call ful2sgl( vegclm,rgs,lngrd)
        else
          call ful2sgl( vfrac,rgs,lngrd)
        endif
        write(nrsfc)  rgs
! prepare 15 to end for end of file condition
        do i=1,lngrd
          canopy(i)=60.0
          f10m(i)=0.8
          vtype(i)=1.0
          stype(i)=1.0
          facsf(i)=1.0
          facwf(i)=1.0
          uustar(i) = 1.
          ffmm(i) = log(30.)
          ffhh(i) = log(30.)
        enddo
! vegitation canopy ------ 15
        read(ncsfc,end=1500) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,canopy,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,canopy,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) canopy(i)=rr(i)
        enddo
        call maxmin(  canopy,lngrd,1,1,1,'canopy read')
        call ful2sgl( canopy,rgs,lngrd)
        write(nrsfc)  rgs 
!
! f10m -------------- 16
        read(ncsfc,end=1600) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,f10m,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,f10m,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) f10m(i)=rr(i)
        enddo
        call maxmin(  f10m,lngrd,1,1,1,'f10m read')
        call ful2sgl( f10m,rgs,lngrd)
        write(nrsfc)  rgs 
!
! vegitation type ---- 17
        read(ncsfc,end=1700) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,vtype,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,vtype,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) vtype(i)=rr(i)
        enddo
        call maxmin(  vtype,lngrd,1,1,1,'vtype read')
        if (iclim.eq.1) then
          call ful2sgl( vetclm,rgs,lngrd)
        else
          call ful2sgl( vtype,rgs,lngrd)
        endif
        write(nrsfc)  rgs
!
! soil type ---------- 18
        read(ncsfc,end=1800) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,stype,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,stype,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) stype(i)=rr(i)
        enddo
        call maxmin(  stype,lngrd,1,1,1,'stype read')
        if (iclim.eq.1) then
          call ful2sgl( sotclm,rgs,lngrd)
        else
          call ful2sgl( stype,rgs,lngrd)
        endif
        write(nrsfc)  rgs
!
! zenith angle dependent factor ---- 19
        read(ncsfc,end=1900) cg2s
        call sgl2ful(cg2s,cg2,clngrd*2)
!       call cg2rg(cg2(1,1),facsf,1)
!       call cg2rg(cg2(1,2),facwf,1)
        call expnd(cg2(1,1),slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg2(1,1),slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,facsf,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) facsf(i)=rr(i)
        enddo
        call expnd(cg2(1,2),slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg2(1,2),slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,facwf,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) facwf(i)=rr(i)
        enddo
        call maxmin(  facsf,lngrd,1,1,1,'facsf read')
        call maxmin(  facwf,lngrd,1,1,1,'facwf read')
        if (iclim.eq.1) then
          call ful2sgl( alfclm(1,1),rg2s(1,1),lngrd)
          call ful2sgl( alfclm(1,2),rg2s(1,2),lngrd)
        else
          call ful2sgl( facsf,rg2s(1,1),lngrd)
          call ful2sgl( facwf,rg2s(1,2),lngrd)
        endif
        write(nrsfc)  rg2s
!
! ustar ------------ 20
        read(ncsfc,end=2000) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,uustar,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,uustar,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) uustar(i)=rr(i)
        enddo
        call maxmin(  uustar,lngrd,1,1,1,'uustar read')
        call ful2sgl( uustar,rgs,lngrd)
        write(nrsfc)  rgs
!
! momentum exchange similarity function -- 21
        read(ncsfc,end=2100) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,ffmm,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,ffmm,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) ffmm(i)=rr(i)
        enddo
        call maxmin(  ffmm,lngrd,1,1,1,'ffmm read')
        call ful2sgl( ffmm,rgs,lngrd)
        write(nrsfc)  rgs
!
! heat exchange similarity function -- 22
        read(ncsfc,end=2200) cgs
        call sgl2ful(cgs,cg,clngrd)
!       call cg2rg(cg,ffhh,1)
        call expnd(cg,slg,0,cgsea,iwork,imax,jmax)
        call expnd(cg,slg,1,cgland,iwork,imax,jmax)
        call cg2rg(cgsea,ffhh,1)
        call cg2rg(cgland,rr,1)
        do i=1,lngrd
          if (slmsk(i).eq.1.0) ffhh(i)=rr(i)
        enddo
        call maxmin(  ffhh,lngrd,1,1,1,'ffhh read')
        call ful2sgl( ffhh,rgs,lngrd)
        write(nrsfc)  rgs
!
        print *,' ****** nornal end of s2rinp ******* '
        return
!
1500    continue
        call maxmin(  canopy,lngrd,1,1,1,'canopy specified')
        call ful2sgl( canopy,rgs,lngrd)
        write(nrsfc)  rgs 
!
1600    continue
        call maxmin(  f10m,lngrd,1,1,1,'f10m specified')
        call ful2sgl( f10m,rgs,lngrd)
        write(nrsfc)  rgs
!
1700    continue
        call maxmin(  vtype,lngrd,1,1,1,'vtype specified')
        call ful2sgl( vtype,rgs,lngrd)
        write(nrsfc)  rgs
!
1800    continue
        call maxmin(  stype,lngrd,1,1,1,'stype specified')
        call ful2sgl( stype,rgs,lngrd)
        write(nrsfc)  rgs
!
1900    continue
        call maxmin(  facsf,lngrd,1,1,1,'facsf specified')
        call maxmin(  facwf,lngrd,1,1,1,'facwf specified')
        call ful2sgl( facsf,rg2s(1,1),lngrd)
        call ful2sgl( facwf,rg2s(1,2),lngrd)
        write(nrsfc)  rg2s
!
2000    continue
        call maxmin(  uustar,lngrd,1,1,1,'uustar specified')
        call ful2sgl( uustar,rgs,lngrd)
        write(nrsfc)  rgs
!
2100    continue
        call maxmin(  ffmm,lngrd,1,1,1,'ffmm specified')
        call ful2sgl( ffmm,rgs,lngrd)
        write(nrsfc)  rgs
!
2200    continue
        call maxmin(  ffhh,lngrd,1,1,1,'ffhh specified')
        call ful2sgl( ffhh,rgs,lngrd)
        write(nrsfc)  rgs
!
        close(nrsfc)
!
        print *,' +++++ normal exit with some fields specified +++ '
!
      endif
!
      deallocate ( slg )
      deallocate ( cg  , cgl )
      deallocate ( cg2, cg4 )
      deallocate ( cgsea,cgland,rr )
      deallocate ( iwork )
      deallocate ( idate,sl,si )
      deallocate ( flat,flon )
      deallocate ( cm2 )
      deallocate ( fm2,fm2x,fm2y )
      deallocate ( dflat,dflon )
      deallocate ( gz, q )
      deallocate ( te, rq )
      deallocate ( uu, vv )
#ifdef NONHYD
      deallocate ( pp, tt )
      deallocate ( ww )
#endif
      deallocate ( rain  , cvrn   )
      deallocate ( rcsln , rsnln  )
!....
      deallocate ( tsea  , smc   )
      deallocate ( sheleg, stc   )
      deallocate ( tg3   , vfrac  )
      deallocate ( zorl  , slmsk  )
      deallocate ( cv    , cvb    )
      deallocate ( cvt    )
      deallocate ( f10m  , canopy )
      deallocate ( vtype  )
      deallocate ( stype , uustar )
      deallocate ( ffmm  ,   ffhh )
      deallocate ( alvsf ,  alvwf )
      deallocate ( alnsf ,  alnwf )
      deallocate ( facsf ,  facwf )
      deallocate ( slmk  ,   simk )
      deallocate ( tg3clm, zorclm )
      deallocate ( albclm, vegclm )
      deallocate ( vetclm, sotclm, alfclm )

!
      return
      end
