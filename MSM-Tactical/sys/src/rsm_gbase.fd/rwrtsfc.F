      subroutine rwrtsfc(zhour,fhour,idate,nn,slmask,rlat,rlon,          &  
     &                  dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,stc,sheleg,     &
     &                  secswr,seclwr,dlwsfc,ulwsfc,                     &
     &                  geshem,bengsh,gflux,fluxr,                       &
     &                  u10,v10,t2,q2,psurf,zorl,tmpmax,tmpmin,          &
     &                  spfhmax,spfhmin,wvuflx,wvvflx,                   &
     &                  srunoff,soilm,snwdph,                            &
     &                  runoff,ep,cldwrk,dugwd,dvgwd,hpbl,pwat,          &
     &                  iceni,igeni,icen2i,ienst,iensi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rwrtsfc
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  output the fluxes file in grib format.
!
! program history log:
!
! usage:    call rwrtsfc(fhour,thour,idate,nn,slmask,rlat,rlon,
!          1                  dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,sheleg,
!          2                  dswsfc,dlwsfc,geshem,bengsh,gflux,fluxr,
!          3                  u10,v10,t2,q2,psurf,zorl,tmpmax,tmpmin,
!          4                  runoff,ep,cldwrk,dugwd,dvgwd)
!   input argument list:
!     fhour     - forecast hour
!     thour     - total forecast hour
!     idate     - initial date of hour month day year
!     nn        - unit number for refional flux file
!     slmask    - sea land mask
!     rlat      - latitude
!     rlon      - longitude
!     dusfc    -  zonal stress on surface in n/m**2
!     dvsfc    -  merid stress on surface in n/m**2
!     dtsfc    -  sensible heat flux on surface in w/m**2
!     dqsfc    -  latent heat flux on surface in w/m**2
!     tsea     -  surface temperature in k
!     smc      - soil wetness parameter array (unit mm)
!     sheleg   - snow amount array (unit mm)
!     dswsfc    - short wave flux on surface
!     dlwsfc    - long wave flux on surface
!     geshem    - total accumulated precipitation
!     bengsh    - convective accumulated precipitation
!     gflux     -
!     fluxr     -
!     u10       - 10 m height u wind
!     v10       - 10 m height v wind
!     t2        -
!     q2        -
!     psurf     - 
!     zorl      - surface roughness
!     tmpmax    - maxmimum temperature
!     tmpmin    - minimum temperature
!     spfhmax   - maxmimum specific humidity
!     spfhmin   - minimum specific humidity
!     wvuflx    - Water vapor zonal flux (vertical int)
!     wvvflx    - Water vapor meridional flux (vertical int)
!    srunoff    - surface runoff
!     soilm     - total soil moisture content
!     runoff    -
!     ep        - 
!     cldwrk    - cloud work function
!     dugwd     - u component of gravity wave drag
!     dvgwd     - v component of gravity wave drag
!
!   output argument list:
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!      idsdef  gribit  wryte  
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
#include <rscomloc.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
!cc
      parameter(iprs=1,itemp=11,iznlw=33,imerw=34,isphum=51,ipwat=54,    &  
     &          ipcpr=59,isnowd=65,icldf=71,iccldf=72,                   &
     &          islmsk=81,izorl=83,ialbdo=84,isoilm=144,icemsk=91,       &
     &          ilhflx=121,ishflx=122,izws=124,imws=125,ighflx=155,      &
     &          iuswfc=160,idswfc=161,iulwfc=162,idlwfc=163,             &
     &          inswfc=164,inlwfc=165,                                   &
     &          idswvb=166,idswvd=167,idswnb=168,idswnd=169,             &
     &          itmx=15,itmn=16,irnof=90,iep=145,                        &
     &          iqmx=118,iqmn=119,                                       &
     &          icldwk=146,izgw=147,imgw=148,ihpbl=221,                  &
     &          idswf=204,idlwf=205,iuswf=211,iulwf=212,icpcpr=214)
      parameter(isfc=1,itoa=8,ielev=105,                                 &
     &          isglev=107,idbls=111,i2dbls=112,icolmn=200,              &
     &          ilcbl=212,ilctl=213,ilclyr=214,                          &
     &          imcbl=222,imctl=223,imclyr=224,                          &
     &          ihcbl=232,ihctl=233,ihclyr=234)
      parameter(len=igrd1*jgrd1)
!      logical   lbm(len)
      parameter(nfld=16)
      dimension   ipur(nfld),itlr(nfld)
      data      ipur/iulwf , iuswf , iuswf , idswf ,  icldf, iprs,       &
     &                 iprs, itemp ,  icldf,   iprs,   iprs, itemp ,     &
     &                icldf,   iprs,   iprs, itemp /
      data      itlr/itoa  , itoa  , isfc  , isfc  , ihclyr, ihctl ,     &
     &               ihcbl , ihctl , imclyr, imctl , imcbl , imctl ,     &
     &               ilclyr, ilctl , ilcbl , ilctl /
      save ipur,itlr
!
#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#endif
      parameter(lens=IGRD1S*JGRD1S)
      real    rtimer(nfld)
      integer       idate(4)
      dimension    slmask(lens)
      dimension    rlat(lens)
      dimension    rlon(lens)
      dimension    dusfc(lens)
      dimension    dvsfc(lens)
      dimension    dtsfc(lens)
      dimension    dqsfc(lens)
      dimension     tsea(lens)
      dimension      smc(lens,lsoil)
      dimension      stc(lens,lsoil)
      dimension   sheleg(lens)
      dimension   dlwsfc(lens)
      dimension   ulwsfc(lens)
      dimension   geshem(lens)
      dimension   bengsh(lens)
      dimension   gflux(lens)
      dimension   fluxr(lens,27)
      dimension   u10  (lens)
      dimension   v10  (lens)
      dimension   t2   (lens)
      dimension   q2   (lens)
      dimension   psurf(lens)
      dimension   zorl (lens)
      dimension   tmpmax (lens)
      dimension   tmpmin (lens)
      dimension   spfhmax (lens)
      dimension   spfhmin (lens)
      dimension   wvuflx (lens)
      dimension   wvvflx (lens)
      dimension  srunoff (lens)
      dimension    soilm (lens)
      dimension   runoff (lens)
      dimension   snwdph (lens)
      dimension   ep (lens)
      dimension   cldwrk (lens)
      dimension dugwd(lens), dvgwd(lens)
      dimension   hpbl (lens)
      dimension   pwat (lens)
!
      logical, allocatable :: lbm(:)
      real, allocatable :: fluxf(:,:),work1(:),slmsep(:)
      real(kind=kind_io4), allocatable :: works(:)
#ifdef MP
      real, allocatable :: workf(:,:),slmseps(:)
#endif
!
      dimension ids(255),iens(5)
!
      real(kind=kind_io4) colat
      real(kind=kind_io4) rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!
      allocate (lbm(len))
      allocate (fluxf(len,4))
      allocate (work1(len))
      allocate (slmsep(len))
      allocate (works(len))
#ifdef MP
      allocate (workf(len,27))
      allocate (slmseps(lens))
#endif
!
      maxbit=16
      colat=0.
      iptv=2
      icen=iceni
      igen=igeni
      ibm0=0
      ibm1=1
      il1k=0
      il2k=0
      ip1=0
      ip2=0
      ina=0
      inm=0
      icen2=icen2i
      inst=10
      iavg=3
      iacc=4
      ifhour=1
      ifday=2
!
#ifdef MP
      if(mype.eq.master) then
#endif
      rewind nn
#ifdef MP
      endif
#endif
      call idsdef(1,ids)
      ilpds=28
      if(icen2.eq.2) ilpds=45
      iens(1)=1
      iens(2)=ienst
      iens(3)=iensi
      iens(4)=1
      iens(5)=255
      iyr=idate(4)
      imo=idate(2)
      ida=idate(3)
      ihr=idate(1)
      ifhr=nint(zhour)
      ithr=nint(fhour)
      if(fhour.gt.zhour) then
        rtime=1./(3600.*(fhour-zhour))
      else
        rtime=0.
      endif
      if(secswr.gt.0.) then
        rtimsw=1./secswr
      else
        rtimsw=1.
      endif
      if(seclwr.gt.0.) then
        rtimlw=1./seclwr
      else
        rtimlw=1.
      endif
      do j=1,nfld
      rtimer(j)=rtimsw
      enddo
      rtimer(1)=rtimlw
      delx=rdelx
      dely=rdely
#ifdef MP
      call rmpgp2f(rlat,igrd1p,jgrd1p,                                   &  
     &                 work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
       rlat1=work1(1)
       rlat2=work1(igrd1)
      endif
#else
      rlat1=rlat(1)
      rlat2=rlat(igrd1)
#endif
#ifdef MP
      call rmpgp2f(rlon,igrd1p,jgrd1p,                                   &
     &                 work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
       rlon1=work1(1)
       rlon2=work1(igrd1)
      endif
#else
      rlon1=rlon(1)
      rlon2=rlon(igrd1)
#endif
      nproj=rproj
      if( nproj.eq.0 ) then 	! mercater
        idrt=1
        ortru=rtruth
        proj=0.0
      elseif( abs(nproj).eq.1 ) then	! polar projection
        idrt=5
        ortru=rorient
        proj=rproj
      else
        print *,' error in rwrtsfc for projection type....'
      endif
      do j=1,lens
#ifdef MP
      slmseps(j)=slmask(j)
#else
      slmsep(j)=slmask(j)
#endif
      enddo
#ifdef MP
      call rmpgp2f(slmseps,igrd1p,jgrd1p,                                &  
     &                 slmsep,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      call maxmin(slmsep,lngrd,1,1,1,'slmsep')
      do j=1,len
        lbm(j)=.true.
      enddo
#ifdef MP
      endif
#endif
!c----------------------------------------------
!1
#ifdef MP
      call rmpgp2f(dusfc,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else
      work1(j)=dusfc(j)*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &  
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,izws,isfc,il1k,il2k,iyr,imo,ida,ihr,              &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(izws),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!2
#ifdef MP
      call rmpgp2f(dvsfc,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else
      work1(j)=dvsfc(j)*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &  
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,imws,isfc,il1k,il2k,iyr,imo,ida,ihr,              &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(imws),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!3
#ifdef MP
      call rmpgp2f(dtsfc,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else 
      work1(j)=dtsfc(j)*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &  
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,ishflx,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ishflx),iens,  &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!4
#ifdef MP
      call rmpgp2f(dqsfc,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else 
      work1(j)=dqsfc(j)*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &  
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,ilhflx,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ilhflx),iens,  &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!5
#ifdef MP
      call rmpgp2f(tsea,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=tsea(j)
      enddo
#endif
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &  
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,itemp,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(itemp),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!6
#ifdef MP
      call rmpgp2f(smc(1,1),igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=smc(j,1)
      enddo
#endif
      do j=1,len
      lbm(j)=.true.
!c      lbm(j)=slmsep(j).eq.1.
      enddo
      il2k=10
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,isoilm,i2dbls,il1k,il2k,iyr,imo,ida,ihr,          &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(isoilm),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!7
#ifdef MP
      call rmpgp2f(smc(1,2),igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=smc(j,2)
      enddo
#endif
      il1k=10
      il2k=200
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,isoilm,i2dbls,il1k,il2k,iyr,imo,ida,ihr,          &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(isoilm),iens,   & 
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il1k=0
      il2k=0
#ifdef MP
      endif
#endif
!8
#ifdef MP
      call rmpgp2f(stc(1,1),igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=stc(j,1)
      enddo
#endif
      il2k=10
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,itemp,i2dbls,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(itemp),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!9
#ifdef MP
      call rmpgp2f(stc(1,2),igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=stc(j,2)
      enddo
#endif
      il1k=10
      il2k=200
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,itemp,i2dbls,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(itemp),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il1k=0
      il2k=0
#ifdef MP
      endif
#endif
!10
#ifdef MP
      call rmpgp2f(sheleg,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=sheleg(j)
      enddo
#endif
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,isnowd,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(isnowd),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!11
#ifdef MP
      call rmpgp2f(dlwsfc,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else
      work1(j)=dlwsfc(j)*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &  
     &            ibm0,idlwf,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(idlwf),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!12
#ifdef MP
      call rmpgp2f(ulwsfc,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else
      work1(j)=ulwsfc(j)*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &  
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,iulwf,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(iulwf),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!13-16
!.......  fix fluxes for approx diurnal cycle
#ifdef MP
      call rmpgp2f(fluxr,igrd1p,jgrd1p,workf,igrd1,jgrd1,27)
      if(mype.eq.master) then
#endif
      do 113 k=1,4
        do j=1,len
#ifdef MP
        work1(j)=workf(j,k)*rtimer(k)
#else
        work1(j)=fluxr(j,k)*rtimer(k)
#endif 
        enddo
        call ful2sgl(work1,works,len)
        write(nn)  works,lbm,idrt,igrd1,jgrd1,maxbit,colat,              &  
     &          ilpds,iptv,icen,igen,                                    &
     &          ibm0,ipur(k),itlr(k),il1k,il2k,iyr,imo,ida,ihr,          &
     &          ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ipur(k)),iens,   &
     &          rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  113 continue
!....interpolate cld top/bot data only from those points w/clds
!    care is taken not to spread clouds....
      do 813 k=5,7
        do 413 j=1,len
#ifdef MP
          if(workf(j,k).gt.0.) then
            fluxf(j,1) = workf(j,k) * rtimsw
            fluxf(j,2) = workf(j,k+3) / workf(j,k)
            fluxf(j,3) = workf(j,k+6) / workf(j,k)
            fluxf(j,4) = workf(j,k+9) / workf(j,k)
          else
!....  zero cld top temp if no clds--safety, cause use zero in ggintt
            fluxf(j,1) = workf(j,k) 
            fluxf(j,2) = workf(j,k+3) 
            fluxf(j,3) = workf(j,k+6)
            fluxf(j,4) = 0.0
          end if
#else
          if(fluxr(j,k).gt.0.) then
            fluxf(j,1) = fluxr(j,k) * rtimsw
            fluxf(j,2) = fluxr(j,k+3) / fluxr(j,k)
            fluxf(j,3) = fluxr(j,k+6) / fluxr(j,k)
            fluxf(j,4) = fluxr(j,k+9) / fluxr(j,k)
          else
!....  zero cld top temp if no clds--safety, cause use zero in ggintt
            fluxf(j,1) = fluxr(j,k) 
            fluxf(j,2) = fluxr(j,k+3) 
            fluxf(j,3) = fluxr(j,k+6)
            fluxf(j,4) = 0.0
          end if
#endif 
  413   continue
!17 21 25
        k4=4+(k-5)*4
        do j=1,len
          fluxf(j,1)=fluxf(j,1)*1.e2
          lbm(j)=fluxf(j,1).gt.0.5
        enddo
        do j=1,len
          work1(j)=fluxf(j,1)
        enddo
        l=k4+1
        call ful2sgl(work1,works,len)
        write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,             &
     &          ilpds,iptv,icen,igen,                                    &  
     &          ibm0,ipur(l),itlr(l),il1k,il2k,iyr,imo,ida,ihr,          &
     &          ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ipur(l)),iens,   &
     &          rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!18 22 26
        do j=1,len
          fluxf(j,2)=fluxf(j,2)*1.e3
          work1(j)=fluxf(j,2)
        enddo
        l=k4+2
        call ful2sgl(work1,works,len)
        write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,             &
     &          ilpds,iptv,icen,igen,                                    &   
     &          ibm1,ipur(l),itlr(l),il1k,il2k,iyr,imo,ida,ihr,          &
     &          ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ipur(l)),iens,   &  
     &          rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!19 23 27
        do j=1,len
          fluxf(j,3)=fluxf(j,3)*1.e3
          work1(j)=fluxf(j,3)
        enddo
        l=k4+3
        call ful2sgl(work1,works,len)
        write(nn)                                                        & 
     &          works,lbm,idrt,igrd1,jgrd1,maxbit,colat,                 &
     &          ilpds,iptv,icen,igen,                                    &
     &          ibm1,ipur(l),itlr(l),il1k,il2k,iyr,imo,ida,ihr,          &
     &          ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ipur(l)),iens,   &
     &          rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!20 24 28
        do j=1,len
          work1(j)=fluxf(j,4)
        enddo
        l=k4+4
        call ful2sgl(work1,works,len)
        write(nn)                                                        &    
     &          works,lbm,idrt,igrd1,jgrd1,maxbit,colat,                 &   
     &          ilpds,iptv,icen,igen,                                    &
     &          ibm1,ipur(l),itlr(l),il1k,il2k,iyr,imo,ida,ihr,          &
     &          ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ipur(l)),iens,   &
     &          rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!
  813 continue
#ifdef MP
      endif
#endif
!c
!29
#ifdef MP
      call rmpgp2f(geshem,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*1.e3*rtime
#else
      work1(j)=geshem(j)*1.e3*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               & 
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,ipcpr,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ipcpr),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!30
#ifdef MP
      call rmpgp2f(bengsh,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*1.e3*rtime
#else 
      work1(j)=bengsh(j)*1.e3*rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,icpcpr,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(icpcpr),iens,  &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!31
#ifdef MP
      call rmpgp2f(gflux,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*rtime
#else 
      work1(j)=gflux(j)*rtime
#endif
      enddo
      do j=1,len
      lbm(j)=slmsep(j).ne.0.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,ighflx,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ighflx),iens,  &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!32
!c this does not gather from pes, modifed from wrtflx.F.
!
#ifdef MP
      if(mype.eq.master) then
#endif
      do j=1,len
      work1(j)=mod(slmsep(j),2.)
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,islmsk,isfc,il1k,il2k,iyr,imo,ida,ihr,            & 
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(islmsk),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!33
!c this does not gather from pes, modifed from wrtflx.F. 
!
#ifdef MP
      if(mype.eq.master) then
#endif
      do j=1,len
      work1(j)=max(slmsep(j)-1.,0.)
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,icemsk,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(icemsk),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!34
#ifdef MP
      call rmpgp2f(u10,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=u10(j)
      enddo
#endif
      il2k=10
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,iznlw,ielev,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(iznlw),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!35
#ifdef MP
      call rmpgp2f(v10,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=v10(j)
      enddo
#endif
      il2k=10
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,imerw,ielev,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(imerw),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!36
#ifdef MP
      call rmpgp2f(t2,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=t2(j)
      enddo
#endif
      il2k=2
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,itemp,ielev,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(itemp),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!37
#ifdef MP
      call rmpgp2f(q2,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=q2(j)
      enddo
#endif
      il2k=2
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,isphum,ielev,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(isphum),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!38
#ifdef MP
      call rmpgp2f(psurf,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j)*1.e3
#else 
      work1(j)=psurf(j)*1.e3
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,iprs,isfc,il1k,il2k,iyr,imo,ida,ihr,              &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(iprs),iens,     &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!c
!39
#ifdef MP
      call rmpgp2f(tmpmax,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=tmpmax(j)
      enddo
#endif
      il2k=2
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,itmx,ielev,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(itmx),iens,     &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!
!39a
#ifdef MP
      call rmpgp2f(spfhmax,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=spfhmax(j)
      enddo
#endif
      il2k=2
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,iqmx,ielev,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(iqmx),iens,     &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!
!40
#ifdef MP
      call rmpgp2f(tmpmin,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=tmpmin(j)
      enddo
#endif
      il2k=2
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,itmn,ielev,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(itmn),iens,     &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!
!40a
#ifdef MP
      call rmpgp2f(spfhmin,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=spfhmin(j)
      enddo
#endif
      il2k=2
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,iqmn,ielev,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(iqmn),iens,     &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
      il2k=0
#ifdef MP
      endif
#endif
!
!41
#ifdef MP
      call rmpgp2f(runoff,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * 1.e3
#else     
      work1(j)=runoff(j) * 1.e3
#endif     
      lbm(j)=slmsep(j).ne.0.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,irnof,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ifhr,ithr,iacc,ina,inm,icen2,ids(irnof),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!42
#ifdef MP
      call rmpgp2f(ep,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * rtime
#else
      work1(j)=ep(j) * rtime
#endif     
      lbm(j)=slmsep(j).ne.0.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,iep,isfc,il1k,il2k,iyr,imo,ida,ihr,               &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(iep),iens,     &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!43
#ifdef MP
      call rmpgp2f(cldwrk,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * rtime
#else
      work1(j)=cldwrk(j) * rtime
#endif
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,icldwk,icolmn,il1k,il2k,iyr,imo,ida,ihr,          &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(icldwk),iens,  &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!44
#ifdef MP
      call rmpgp2f(dugwd,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * rtime
#else
      work1(j)=dugwd(j)*rtime
#endif     
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,izgw,isfc,il1k,il2k,iyr,imo,ida,ihr,              &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(izgw),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!45
#ifdef MP
      call rmpgp2f(dvgwd,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * rtime
#else
      work1(j)=dvgwd(j)*rtime
#endif     
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,imgw,isfc,il1k,il2k,iyr,imo,ida,ihr,              &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(imgw),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!46
#ifdef MP
      call rmpgp2f(hpbl,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=hpbl(j)
      enddo
#endif
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,ihpbl,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(ihpbl),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!47
#ifdef MP
      call rmpgp2f(pwat,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=pwat(j)
      enddo
#endif
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,ipwat,icolmn,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(ipwat),iens,    &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!c
!48  convert to percent
!....   in previous codes, fluxr(17) contained various attempts at
!        calculating surface albedo...it has proven unsatisfactory!!
!....   so now, sfc albedo will be calculated in wrtsfc as the
!        ratio of the time-mean of the sfcsw fluxes .. kac+mi dec98
!
#ifdef MP
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
       if(workf(j,4).gt.0.) then
         work1(j)=workf(j,3)/workf(j,4) * 100.
#else
       if(fluxr(j,4).gt.0.) then
         work1(j)=fluxr(j,3)/fluxr(j,4) * 100.
#endif 
         if(work1(j).gt.100.) work1(j)=100.
       else
         work1(j)=0.
       endif
!c      work1(j)=fluxr(j,17)*rtimsw * 100.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm0,ialbdo,isfc,il1k,il2k,iyr,imo,ida,ihr,            &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(ialbdo),iens,  &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!49
#ifdef MP
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=workf(j,26)*rtimsw * 100.
#else
      work1(j)=fluxr(j,26)*rtimsw * 100.
#endif
      lbm(j)=work1(j).gt.0.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &  
     &            ibm0,icldf,icolmn,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,ids(icldf),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!50
#ifdef MP
      call rmpgp2f(wvuflx,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
      call maxmin(work1,lngrd,1,1,1,'wvuflx')
#else
      do j=1,len
      work1(j)=wvuflx(j)
      enddo
#endif
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,131,icen,igen,                                   &
     &            ibm0,242  ,icolmn,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,0,iens,            &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!51
#ifdef MP
      call rmpgp2f(wvvflx,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
      call maxmin(work1,lngrd,1,1,1,'wvuflx')
#else
      do j=1,len
      work1(j)=wvvflx(j)
      enddo
#endif
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,131,icen,igen,                                   &
     &            ibm0,243  ,icolmn,il1k,il2k,iyr,imo,ida,ihr,           &
     &            ifhour,ifhr,ithr,iavg,ina,inm,icen2,0,iens,            &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!52
#ifdef MP
      call rmpgp2f(srunoff,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * 1.e3
#else
      work1(j)=srunoff(j) * 1.e3
#endif
      lbm(j)=slmsep(j).ne.0.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1,235  ,isfc,il1k,il2k,iyr,imo,ida,ihr,             &
     &            ifhour,ifhr,ithr,iacc,ina,inm,icen2,ids(irnof),iens,   &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!53
#ifdef MP
      call rmpgp2f(soilm,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#endif
      do j=1,len
#ifdef MP
      work1(j)=work1(j) * 1.e3
#else
      work1(j)=soilm(j) * 1.e3
#endif
      lbm(j)=slmsep(j).ne.0.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,               &
     &            ilpds,iptv,icen,igen,                                  &
     &            ibm1, 86  ,i2dbls,0,200,iyr,imo,ida,ihr,               &
     &            ifhour,ithr,0,inst,ina,inm,icen2,ids(86),iens,         &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!54
#ifdef MP
      call rmpgp2f(snwdph,igrd1p,jgrd1p,work1,igrd1,jgrd1,1)
      if(mype.eq.master) then
#else
      do j=1,len
      work1(j)=snwdph(j)
      enddo
#endif
      do j=1,len
      lbm(j)=.true.
      enddo
      call ful2sgl(work1,works,len)
      write(nn)   works,lbm,idrt,igrd1,jgrd1,maxbit,colat,                 &
     &            ilpds,iptv,icen,igen,                                    &
     &            ibm0, 66  ,isfc,il1k,il2k,iyr,imo,ida,ihr,               &
     &            ifhour,ithr,ip2,inst,ina,inm,icen2,ids(66),iens,         &
     &            rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
#ifdef MP
      endif
#endif
!c

#ifdef MP
      if(mype.eq.master) then
#endif
      print *,'rsm grib flux file written ',fhour,idate,nn
#ifdef MP
      endif
#endif
      deallocate (lbm)
      deallocate (fluxf)
      deallocate (work1)
      deallocate (slmsep)
      deallocate (works)
#ifdef MP
      deallocate (workf)
      deallocate (slmseps)
#endif
!
      return
      end
