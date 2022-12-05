      subroutine rsmini_mpi(inpc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsmini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare all the constant arraies for regional computation
!      before regional forecast and read the input data set.
!
! program history log:
!
! usage:    call rsmini(gfhour,filtai,
!          1                   nin1i,nin2i,
!          2                   nrsmi1i,nrsmi2i,nrflipi,
!          3                   nrsmo1i,nrsmo2i,nrflopi,nrsflxi,nriniti)
!   input argument list:
!     gfhour  - forecast hour of global model
!     nin1i - unit number for global sigma file input 1
!     nin2i - unit number for global sigma file input 2
!     nrsmi1i - unit number for regional sigma file input 1
!     nrsmi2i - unit number for regional sigma file input 2
!     nrflipi - unit number for regional surface file input
!     nrsmo1i - unit number for regional sigma file outout 1
!     nrsmo2i - unit number for regional sigma file outout 2
!     nrflopi - unit number for regional surface file outout
!     nrsflxi - unit number for regional flux file outout
!     nriniti - unit number for regional initialization temperory file
!
!   output argument list:
!
!   common block:
! rscomf  rscomltb  comcon
!   input files: none
!
!   output files: none
!
!   subprograms called:
! setbgd  setg2b  getwt  rftini plnini rltbini  rsminp  rsetsig
! rsetmap  ramtm  epslxy  pint3  rsetsfc  rfixio
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!.................................................................
!..... this routine setup all the routine and constant for rsm.
!..... and get input data from gsm or others include surface files.
!.....
      use machine , only : kind_io4
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      real, allocatable :: grid1(:,:,:)
      real, allocatable :: coef(:,:)
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define LNWAVS lnwavp
#define LNGRDS lngrdp
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define LNWAVS lnwav
#define LNGRDS lngrd
#endif
#include <rscomfi.h>
#include <rscomspc.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscompln.h>
#include <rscomrad.h>
#include <rscomloc.h>
#include <rscomltb.h>
#include <rscomgpd.h>
#include <rscomoz.h>
!cc
      dimension gzzb(IGRD1S,JGRD1S),                                     &  
     &          gzxb(IGRD1S,JGRD1S),gzyb(IGRD1S,JGRD1S)                  
!cc Jun chnage for g2rini
      dimension  gclat(latg)                                             &
     &  , colrad(latg2), wgt(latg2),wgtcs(latg2),rcs2(latg2)
!cc
!
      parameter (km=1)
!
      dimension tmpsp(lnwav,levrp1)
      real(kind=kind_io4), allocatable :: tmps(:,:)
      real(kind=kind_io4) fhours,sis(levr+1),sls(levr)
!cjun
      dimension tmp(lngrd,levr),tmpk1(lngrd)
      dimension zyftemp(lnwav,levr)
      character cfhour*16,cform*40
!
#ifdef MP
      if (mype.eq.master) then
#endif
      print *,' ----- into rsmini ----- '
!
      print *,' ++++++++ header of rsm +++++++++++++++'
      print *,' ++ version with nest to global model +'
      print *,' ++ iwave=', iwav,' jwave=', jwav 
      print *,' ++ levs =', levr,' levh =', levh  
      print *,' ++ igrid=', igrd1,' jgrid=', jgrd1  
      print *,' ++ ncol=', ncol,' nrow=', nrow  
#ifdef MP
      endif
#endif
!
! ..... setup base field grid
!
#ifdef MP
      istr=lonstr(mype)
      iend=lonstr(mype)+lonlen(mype)-1
      jstrs=latstr(mype)
      jends=latstr(mype)+latlen(mype)-1
      jstrn=jgrd1-jends+1
      jendn=jgrd1-jstrs+1
      ilen=lonlen(mype)  
#else
      istr=1
      iend=igrd1
      jstrs=1
      jends=jgrd1
      jstrn=jgrd1+1
      jendn=jgrd1
      ilen=igrd1     
#endif
      ib1=(istr-1)/bgf-border+1
      ib2=ibgd1-1+ib1
      jb1=(jstrs-1)/bgf-border+1
      jb2=jbgd1-1+jb1
      jbx=jb2
#ifdef MP
      if (mype.eq.master) print *, 'ib1=',ib1,'jb2=',ib2,                &  
     &   'jb1=',jb1,'jb2=',jb2,'jbx=',jbx,                               &
     &   'bgf=',bgf,'border=',border,'ibgd1=',ibgd1,'jbgd=',jbgd1        &
     &    ,'igrd1p=',igrd1p,'jgrd1p=',jgrd1p
#endif
      call setbgd(blat(1),blon(1),delx,dely,dlamda0,                     &
     &            bgf,ib1,ib2,jb1,jb2                                    &
#ifdef MP
     &           ,mype,master                                            &
#endif
     &            )
!
! ..... prepare common array for transform gsm to rsm
!
#ifdef G2R
! abstract:  prepare some arraies for global to regional transformation
!            including map factor
      call setg2b(dlamda0)
#endif
#ifdef C2R
      call c2bini(inpc,blat,blon                                         &
#ifdef MP
     &     ,mype,master                                                  &
#endif
     &            )
      call ctobini(inpc,blat,blon                                        &
#ifdef MP
     &           ,mype,master                                            &
#endif
     &            )
#endif
      call getwt(border*2-bsmooth,bsmooth,bgf,wsltb,dwsltb)
!
!....< set up some constants for model >
!
      do 30 lev=1,levr
      tov(lev)= 300.0
 30   continue
!
      call rftini
! jun fft
      call plnini
      call rltbini
!
! ..... prepare input
!
#ifdef MP
      if (mype.eq.master) then
#endif
      rewind nrsmi1
      read(nrsmi1) 
      read(nrsmi1) fhours,idate,(sis(k),k=1,levrp1),(sls(k),k=1,levr)
      fhour=fhours
      call sgl2ful(sis,si,levr+1)
      call sgl2ful(sls,sl,levr)
!
#ifdef MP
      endif
      call rmpbcastr(fhour,1)
      call rmpbcasti(idate,4)
      call rmpbcastr(si,levr)
      call rmpbcastr(sl,levr)
#endif
#ifdef MP
      if (mype.eq.master) then
#endif
      rewind inpc
      rewind nrsmi1
#ifdef MP
      endif
#endif
      call rsminp(inpc,nrsmi1,1,                                         & 
     &            fhour,idate,flat,flon,fm2,fm2x,fm2y,sl,si,             &  
     &            gz,qm,tem,uum,vvm,rm                                   &
#ifdef NONHYD
     &                     ,pnm,tnm,onm,tov                              &
#endif
     &                                )

      if(fhour.eq.0.0) then
        do k=1,levr
          do i=1,LNWAVS
            uu(i,k)=uum(i,k)
            vv(i,k)=vvm(i,k)
            te(i,k)=tem(i,k)
#ifdef NONHYD
            pn(i,k)=pnm(i,k)
            tn(i,k)=tnm(i,k)
            on(i,k)=onm(i,k)
#endif
          enddo
        enddo
#ifdef NONHYD
        k=levr+1
        do i=1,LNWAVS
          on(i,k)=onm(i,k)
        enddo
#endif
        do nc=1,ntotal
          do k=1,levr
          do i=1,LNWAVS
            rq(i,k,nc)=rm(i,k,nc)
          enddo
          enddo
        enddo
        do 120 i=1,LNWAVS
          q(i)=qm(i)
 120    continue
        if(ncld.gt.0) then
        if ((num_p3d .eq. 4) .or.                                        &! zhao scheme!  
     &      (num_p3d .eq. 3)) then   ! ferrier scheme!
          do n=1,num_p3d
          do k=1,levr
          do j=1,JGRD1S
          do i=1,IGRD1S
             phy_f3d(i,j,k,n) = 0.0
          enddo
          enddo
          enddo
          enddo
          do n=1,num_p2d
          do j=1,JGRD1S
          do i=1,IGRD1S
             phy_f2d(i,j,n)=0.0
          enddo
          enddo
          enddo
        endif
        endif
      else
#ifdef MP
      if (mype.eq.master) then
#endif
        print *,' -rsm- restart read.'
        rewind inpc
        rewind nrsmi2
#ifdef MP
      endif
#endif
        call rsminp(inpc,nrsmi2,0,                                       &  
     &              fhour,idate,flat,flon,fm2,fm2x,fm2y,sl,si,           &
     &              gz,q,te,uu,vv,rq                                     &
#ifdef NONHYD
     &                     ,pn,tn,on,tov                                 &
#endif
     &                                  )
        if(ncld.gt.0) then
        if ((num_p3d .eq. 4) .or.                                        &! zhao scheme!
     &      (num_p3d .eq. 3)) then   ! ferrier scheme!
          call sread2(nrsmi2,phy_f3d,phy_f2d)
        endif
        endif
      endif
!
#ifdef MP
      if (mype.eq.master) then
#endif
      print 20,fhour,idate
 20   format(1h ,'fhour=',f6.2,2x,' date= ',4(i4,2x))
#ifdef MP
      endif
#endif
!
      kdt=nint(fhour*3600/deltim)
      fhour=kdt*deltim/3600
      phour=fhour
!
      call rsetsig(levr, levrm1, levrp1,                                 &  
     &             ci, si, del, sl, cl, rpi)
      sl1=sl(1)
      do 40 k=1,levrm1
      rpirec(k) = 1.e0/rpi(k)
 40   continue
!
      do 50 k=2,levr
      rdel2(k)=0.5e0 / del(k)
 50   continue
      rdel2(1)=0.5e0/del(1)
      rdel2(levrp1)=0.5e0/del(levr)
!
!     do 50 k=1,levr
!     rdel2(k)=0.5e0/del(k)
!50   continue
!
!cc Jun change to add g2rini here
#ifdef G2R
      call glats(latg2,colrad,wgt,wgtcs,rcs2)
      do 7 j=1,latg2
      gclat(j) = colrad(j)
      jj = latg - j + 1
      gclat(jj) = pi - colrad(j)
 7    continue
      call g2rini_mpi(gclat,flat,flon                                     &
#ifdef MP
     &                ,mype,master                                        &
#endif
     &               )
#else
      call c2rini_mpi(inpc,flat,flon                                      &
#ifdef MP
     &                ,mype,master                                        &
#endif
     &               )
#endif
!cc
!
! rsetmap:  pass in latitudes longitudes map factor, and compute the
!            mean map factor or the largest map factor and it derivatives
!            in x and y directions.

!c       call rsetmap(flat(1),flon(1),fm2,fm2x,fm2y,delx,dely)
!c       call rsetmap(flat(n),flon(n),fm2,fm2x,fm2y,delx,dely)
      call rsetmap(flat,flon,fm2,fm2x,fm2y,delx,dely)
! ramtm:  prepare some constant arraies for dynamic computation and semi-
!            implicit computation.
      call ramtm(del,rpi,sv,p1,p2,am,hm,tm,si)
      call rbmcm(tov,p1,p2,h1,h2,del,ci,bm,cm,sv,am)
#ifdef NONHYD
      call cdefghm(cmn,dmn,emn,fmn,gmn,hmn,                              &  
     &                   si,sl,tov,del,h1,h2)
#endif
!
! epslxy:  prepare derivative and laplacian for regional wave field.
!            for regional spectral model.
!
#ifdef MP
      allocate (coef(lnwav,4))
      call epslxy(lnwav,iwav1,jwav1,igrd,jgrd,                           &  
     &            coef(1,1),coef(1,2),coef(1,3),coef(1,4),delx,dely)
      rnnp1max=coef(lnwav,4)
      epsxmax=coef(lnwav,1)
      epsymax=coef(lnwav,2)
      call rmpsf2p(coef(1,1),lnwav,epsx,lnwavp,1)
      call rmpsf2p(coef(1,2),lnwav,epsy,lnwavp,1)
      call rmpsf2p(coef(1,3),lnwav,snnp1,lnwavp,1)
      call rmpsf2p(coef(1,4),lnwav,rnnp1,lnwavp,1)
      deallocate (coef)
#else
      call epslxy(lnwav,iwav1,jwav1,igrd,jgrd,                           &  
     &            epsx,epsy,snnp1,rnnp1,delx,dely)
      rnnp1max=rnnp1(lnwav)
      epsxmax=epsx(lnwav)
      epsymax=epsy(lnwav)
#endif
!
      do nn=1,LNWAVS
        dpsx(nn) = epsx(nn)*delx
        dpsy(nn) = epsy(nn)*dely
      enddo
!
#ifdef MP
#define LATLENS latlen(mype)
#else
#define LATLENS jgrd1
#endif
      do j=1,LATLENS
         lat=j+jstrs-1
         call pint3(gzb,gzzb(1,j),gzxb(1,j),gzyb(1,j)                    &  
     &             ,wsltb,dwsltb,lat,km,IGRD1S,ib1,ib2                   &
     &             ,jb1,jb2,jbx,istr,ilen                                &
     &             ,bgf,border,jgrd1)
      enddo
      call rsetsfc(gz,gzzb,gzxb,gzyb,epsx,epsy)
!....< initial setup for model physics >
!ccj
!
!c set ozon indices for interpolation
!c
      call setindxoz(LNGRDS,lats18,flat,ddy,jindx1,jindx2)
!c
! read climatologically varying ozone prod and dist rate
!
      call readoz_disprd(ozprdin,ozdisin,poz)
!
!ccccccccccccccccccccccccccccccccccccccccccccccc
      rcl=1.0
      iflws=0
!     read radiation files and some constants
!c      call gcljms(si)
!c      call grlwsw(sl,nco2)
!c      call gaeros(si,sl)
!....< set initial solar hour >
      solhr = fhour + idate(1)
      iday = solhr / 24.e0
      solhr = solhr - iday*24.e0
      if( solhr.lt.0.0 ) solhr=solhr+24.e0
!....< read fixed fields from regional fixfld prog>
#ifdef MP
      if (mype.eq.master) then
#endif
      print *,' -rsmini-read surface from regional.rsfcsec=',rsfcsec
#ifdef MP
      endif
#endif
      call rfixio(label,fhour,idate,                                     &  
     &   tsea,smc,sheleg,stc,tg3,zorl,                                   &
     &   cv,cvb,cvt,slmsk,f10m,canopy,vfrac,vtype,stype,                 &
     &   uustar,ffmm,ffhh,alvsf,alvwf,alnsf,alnwf,facsf,facwf,           &
     &   0,nrflip,rsfcsec,lsfcmrg,                                       &
     &   flon,flat,lnewsst) !added by SN
#ifdef NONHYD
      if(fhour.eq.0.0) then
        mid=0.5*LNWAVS
        dzdtsum=0.0
        do k=1,levr+1
          dzdtsum=dzdtsum+abs(on(mid,k))
        enddo
        if( dzdtsum.eq.0.0 ) then
          call rloopw
!
          call rmsgtr( q, pn, tn, on,del,rq)
          do k=1,levr+1
            do i=1,LNWAVS
              onm(i,k)=on(i,k)
            enddo
          enddo
        endif
      endif
#endif

!testjun
     if (mype.eq.master) then
          print *,' in rsmini_mpi,after (nonhydrostatic) '
     endif
!....read mountain variance etc
!
      allocate (tmps(lngrd,nmtvr))
#ifdef MP
      allocate (grid1(igrd1,jgrd1,nmtvr))
#define HPRIMES grid1
#else
#define HPRIMES hprime
#endif
#ifdef MP
      if( mype.eq.master ) then
#endif
         read(nmtnv) tmps
         call sgl2ful(tmps,HPRIMES,lngrd*nmtvr)
#ifdef MP
      endif
      call rmpgf2p(grid1,igrd1,jgrd1,                                    &  
     &            hprime,igrd1p,jgrd1p,nmtvr)
#endif
!
      do k=1,nmtvr
        do j = 1, JGRD1S
          do i = 1, IGRD1S
            if(slmsk(i,j).ne.1.) hprime(i,j,k) = 0.
          enddo
        enddo
      enddo
#ifdef MP
      call rmpgp2f(hprime,igrd1p,jgrd1p,grid1,igrd1,jgrd1,nmtvr)
      if( mype.eq.master ) then
#endif
!        call maxmin(grid1,lngrd,nmtvr,1,nmtvr,'hprimes full')
        call maxmin(hprime,IGRD1S*JGRD1S,nmtvr,1,nmtvr,'hprimes')
#ifdef MP
      endif
#endif
!
!jun test nrow*ncol
!#ifdef MP
!      tmpk1=0.
!      call rmpsp2f(gz,lnwavp,tmpk1,lnwav,1)
!      if (mype.eq.master) then
!        call maxmin(tmpk1,lnwav,1,1,1,'sread gz sp')
!        open ( 93,file='testrsmini_lev1',form='unformatted')
!        write(93) (tmpk1(i),i=1,lnwav)
!        close(93)
!      endif
!      tmp=0.
!      call rmpsp2f(tem,lnwavp,tmp,lnwav,levr)
!      if (mype.eq.master) then
!        call maxmin(tmp,lnwav,levr,1,levr,'sread gz sp')
!      kh=nint(fhour)
!      ndig=max(log10(kh+0.5)+1.,2.)
!      write(cform,'("(i",i1,".",i1,")")') ndig,ndig
!      write(cfhour,cform) kh
!
!        open ( 93,file='testrsmini_lev'//cfhour,form='unformatted')
!        do  k=1,levr
!           write(93) (tmp(i,k),i=1,lnwav)
!        enddo
!        close(93)
!      endif
!#endif

      deallocate (tmps)
#undef HPRIMES
#ifdef MP
      deallocate (grid1)
#endif
!
      call rzerout
! ...............prepare ken points .................
#ifdef RKN
       isave=0
       itnum=0
       if(npoint.ne.0) then
         isave=1
         itnum=1
         call rkenpre(flat,flon,gzzb,nrflip)
       endif
#endif
! ............... set up all routines and constants .......

      return
      end
