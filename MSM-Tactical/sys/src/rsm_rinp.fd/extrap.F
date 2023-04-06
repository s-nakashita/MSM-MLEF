      subroutine extrap(lsig,lmtn,lp2r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  extrap
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do prepare new surface pressure and sigma to sigma interpolation 
!
! program history log:
!
! usage:    call  extrap(lsig,lmtn)
!   input argument list:
!     lsig      - logical variable for new sigma or not
!     lmtn      - logical variable for new terrain or not
!     lp2r      - logical variable for input is pressure levels or not
!
!   output argument list:
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!       maxmin  - print out the max and min of a given variable.
!       newps   - compute new surface pressure.
!       sg2sg   - sigma to sigma interpolation
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!
      use physcons, grav => con_g, rd => con_rd                          &               
     &,             rv => con_rv, hvap => con_hvap                       &
     &,             psat => con_psat, ttp => con_ttp
!ccj
#include <paramodel.h>
!cc
!
      parameter(idimi=igrd1i,jdimi=jgrd1i,ijdimi=idimi*jdimi,            &
     &          kdimi=levri,kdimpi=kdimi+1,kdimmi=kdimi-1)
      parameter(idimo=igrd1o,jdimo=jgrd1o,ijdimo=idimo*jdimo,            &
     &          kdimo=levro,kdimpo=kdimo+1,kdimmo=kdimo-1)
!
      parameter(idimt=ijdimo)
!
      common /hydrid/ siin3d(ijdimi,levs+1),slin3d(ijdimi,levs)
      common /ps_gfs/ zold(ijdimi,levs)
!
!  blank commons
!
      common /comrinp1/idate(4)
!
!  common for input arrays
!
      common /comrinp2/qi(ijdimi),tei(ijdimi,kdimi),uui(ijdimi,kdimi),   &            
     &       vvi(ijdimi,kdimi),rqi(ijdimi,kdimi,ntotal),gzi(ijdimi)      &
#ifdef NONHYD
     &      ,ppi(ijdimi,kdimi),tti(ijdimi,kdimi)                         &
     &      ,wwi(ijdimi,kdimi+1)                                         &
#endif
     &      ,flati(ijdimi),floni(ijdimi)                                 &
     &      ,fm2i(ijdimi),fm2xi(ijdimi),fm2yi(ijdimi)
!
!  common for after horizontal resolution change
!
      common /comrinp3/q (ijdimo),te (ijdimo,kdimi),uu (ijdimo,kdimi),   &
     &       vv (ijdimo,kdimi),rq (ijdimo,kdimi,ntotal),gz (ijdimo)      &
#ifdef NONHYD
     &      ,pp (ijdimo,kdimi),tt (ijdimo,kdimi)                         &
     &      ,ww (ijdimo,kdimi+1)                                         &
#endif
     &      ,flato(ijdimo),flono(ijdimo)                                 &
     &      ,fm2o(ijdimo),fm2xo(ijdimo),fm2yo(ijdimo)
!
!  common for arrays of modified vertical resolution
!
      common /comrinp4/qo(ijdimo),teo(ijdimo,kdimo),uuo(ijdimo,kdimo),   &
     &       vvo(ijdimo,kdimo),rqo(ijdimo,kdimo,ntotal),gzo(ijdimo)      &
#ifdef NONHYD
     &      ,ppo(ijdimo,kdimo),tto(ijdimo,kdimo)                         &
     &      ,wwo(ijdimo,kdimo+1)
      real*4 qo4,sl4                                                     &
#endif
     &       ,tmpcomrinp4
!
!
      common /inpver/siin(kdimpi),slin(kdimi),delin(kdimi),              &
     &                ciin(kdimpi),clin(kdimi),rpiin(kdimmi)
      common/comvet/si  (kdimpo),sl  (kdimo),del  (kdimo),               &
     &               ci (kdimpo),cl  (kdimo),rpi  (kdimmo)
!
      logical lsig,lmtn,lp2r
!
!-----------------------------------------------------------------------
!  water vapor functions
!-----------------------------------------------------------------------
!  virtual temperature from temperature and specific humidity
!     ftv(xt,xq)=xt*
!    1   (1. e 0+(rv/rd-1. e 0)*xq)
!  temperature from virtual temperature and specific humidity
!     ft(xtv,xq)=xtv/
!    1   (1. e 0+(rv/rd-1. e 0)*xq)
!  specific humidity from vapor pressure and air pressure
      fq(xpv,xp)=(rd/rv)*xpv/                                            &         
     &    (xp+(rd/rv-1.e0)*xpv)
!  vapor pressure from specific humidity and air pressure
!     fpv(xq,xp)=xp*xq/
!    1    ((rd/rv)-
!    2     (rd/rv-1. e 0)*xq)
!  saturation vapor pressure (kpa) from temperature
      fpvs(xt)=(psat*1.e-3)*exp((hvap/rv)/ttp-(hvap/rv)/xt)
!  saturation specific humidity from temperature and pressure (kpa)
      fqs(xt,xp)=fq(fpvs(xt),xp)
!-----------------------------------------------------------------------
!
      in=0
      tensn=10.0
! transfer from slin to sl
      print *,' transfer from slin to sl '
      print *,' slin ',slin
      print *,' sl   ',sl
!
      do 11 i = 1, idimt
      qo(i)=0.0
 11   continue
      do 12 k=1,kdimo
      do 12 i = 1, idimt
      teo(i,k)=0.0
      uuo(i,k)=0.0
      vvo(i,k)=0.0
   12 continue
      do 13 nc=1,ntotal
      do 13 k=1,kdimo
      do 13 i = 1, idimt
      rqo(i,k,nc)=0.0
   13 continue
!
!
!     print *,' check grid point values before sg2sg.  lat=', lat
       call maxmin(uu,idimt,kdimi,1,kdimi,' uf ')
       call maxmin(vv,idimt,kdimi,1,kdimi,' vf ')
       call maxmin(te,idimt,kdimi,1,kdimi,' tf ')
       call maxmin(rq,idimt,kdimi,1,kdimi,' rqf')
       call maxmin(q,idimt,1,1,1,' ps ')
!
      if( lmtn ) then
        if( lp2r ) then
          call maxmin(zold,idimt,kdimi,1,kdimi,' zold')
          call newps_gfs(idimt,kdimi,te,q,zold,gz,gzo,qo,siin3d)
        else
          call newps(idimt,kdimi,te,q,gz,gzo,qo,siin3d)
        endif
      else
        do 300 i = 1, idimt
        qo(i)=q(i)
 300    continue
      endif
!      open (111,file='newps',form='unformatted')
!      if (lp2r) then
!      read(111) qo
!      else
!      write(111) qo
!      endif
!      close(111)
!
! start sg2sg
!
       call maxmin(q,idimt,1,1,1,' q')
       call maxmin(qo,idimt,1,1,1,' q0')
       print *,'slin=',slin
       print *,'sl=',sl
       call sg2sg(q,slin3d,uu,qo,sl,uuo,idimt,1,kdimi,kdimo,in,tensn,1)
       call sg2sg(q,slin3d,vv,qo,sl,vvo,idimt,1,kdimi,kdimo,in,tensn,1)
       call sg2sg(q,slin3d,te,qo,sl,teo,idimt,1,kdimi,kdimo,in,tensn,1)
!ccj
       do nc = 1,ntotal
       call sg2sg(q,slin3d,rq(1,1,nc),qo,sl,rqo(1,1,nc),idimt,1,            &           
     &          kdimi,kdimo,in,tensn,1)
       enddo

!cc
       if(lat .eq. 1 ) then
       print *,' check grid point values after sg2sg.   lat=', lat
       call maxmin(uuo,idimt,kdimi,1,kdimi,' ufo')
       call maxmin(vvo,idimt,kdimi,1,kdimi,' vfo')
       call maxmin(teo,idimt,kdimi,1,kdimi,' tfo')
       call maxmin(rqo,idimt,kdimi,1,kdimi,' rqo')
       call maxmin( qo,idimt,    1,1,1,' pso')
       endif

!
! sg2sg sets values to constant outside of input domain.
! fix lapse rate and relative humidity below input surface.
!
      gamma=6.5e-3
      do 299 i=1,idimt
      ps1=log(slin3d(i,1))+q(i)
      rh1=rq(i,1,1)/fqs(te(i,1),exp(ps1))
      do 298 k=1,kdimo
      psk=log(sl(k))+qo(i)
      if(psk.lt.ps1) goto 299
      teo(i,k)=te(i,1)*exp(gamma*rd/grav*(psk-ps1))
      rqo(i,k,1)=rh1*fqs(teo(i,k),exp(psk))
298   continue
299   continue
#ifdef NONHYD
!c      call sg2sg(q,slin,pp,qo,sl,ppo,idimt,1,kdimi,kdimo,in,tensn,1)
!       call sg2sg(q,slin,tt,qo,sl,tto,idimt,1,kdimi,kdimo,in,tensn,1)
!       call sg2sg(q,slin,ww,qo,sl,wwo,idimt,1,kdimi,kdimo,in,tensn,1)
      do k=1,kdimo
      do i=1,idimt
        qo4=qo(i)
        sl4=log(sl(k))
        ppo(i,k)=qo4+sl4
!j      ppo(i,k)=qo(i)+log(sl(k))
        tto(i,k)=teo(i,k)
      enddo
      enddo
      do k=1,kdimo+1
      do i=1,idimt
        wwo(i,k)=0.0
      enddo
      enddo
#endif
!
!
! ======= check values ========
!
       if(lat .eq. 1 ) then
       print *,' check grid point values after sg2sgall domain.lat=', lat
       call maxmin(uuo,idimt,kdimi,1,kdim,' ufo')
       call maxmin(vvo,idimt,kdimi,1,kdim,' vfo')
       call maxmin(teo,idimt,kdimi,1,kdim,' tfo')
       call maxmin(rqo,idimt,kdimi,1,kdim,' rqo')
       call maxmin( qo,idimt,    1,1,1,' pso')
       endif
!
1000   continue
!
!
!
      return
      end
