#include <define.h>
      subroutine grrad (im,ix,levs,ntotal,ntoz,ncld,icfc,icwp,iflip,    &
     &                  iswsrc, lw_type, iovr,                          &
     &                  t,q,vvl,                                        &
     &                  slmsk, coszen, xlon,xlat,tsea,                  &
     &                  sheleg,zorl,hprime,stc,cv,cvt,cvb,rhcl,istrat,  &
     &                  rsin1,rcos1,rcos2,                              &
     &                  alvsf,alnsf,alvwf,alnwf,facsf,facwf,            &
     &                  solc,lsswr,lslwr,swh,hlw,                       &
     &                  sfcnsw,sfcdlw,tsflw,                            &
     &                  prsi,prsl,prslk,lssav,raddt,                    &
     &                  dtlw,coszdg,fluxr,                              &
     &                  me,k1oz,k2oz,facoz,                             &
     &                  hsigma, daysec, rco2                            &
!
     &,                 l,lp1,lp1v,llp1,nsrc,nbd,nvb,nae,nbly           &
     &,                 ndm, nvect, sgtmp,co21d,co22d, co21d3,co21d7    &
     &,                 source,dsrce                                    &
     &,                 haer, idm, taur,                                &
     &                  kprfg, idxcg, cmixg, denng,                     &
     &                  imxae, jmxae, nxc, ndn,                         &
#ifdef RKN
     &                  flwup,fswup,fswdn,sswup,sswdn,                  &
     &                  slwup,slwdn,cldsa,mtopa,mbota,                  &
#endif
     &                  lprnt)
!
!***********************************************************************
!
      use machine , only : kind_phys,kind_rad
      use funcphys , only : fpvs
      use physcons, eps => con_eps, epsm1 => con_epsm1                  &
     &,             grav => con_g,  t0c => con_t0c,  pi => con_pi
      implicit none
!
      real (kind=kind_rad), parameter :: qmin = 1.0e-10
!
      integer imxae, jmxae, nxc, ndn, im, ix
      integer l,lp1,lp1v,llp1,nsrc,nbd,nvb,nae,nbly,ndm,nvect
      real (kind=kind_rad) haer(ndm,nae),  taur(l,nbd)
      integer idm(l,nae)
!
      real (kind=kind_rad) sgtmp(lp1,2),   co21d(l,6), co22d(lp1,lp1,6) &
     &,                    co21d3(lp1,6),  co21d7(lp1,6)                &
     &,                    source(28,nbly),dsrce(28,nbly)
!
! ---  input global aerosol distribution data
      real (kind=kind_rad) cmixg(nxc,imxae,jmxae)                       &
     &,                     denng(ndn,imxae,jmxae)
      integer idxcg(nxc,imxae,jmxae), kprfg(imxae,jmxae)
! --- local temp variables of aerosol data on specified point
      real (kind=kind_rad) cmix(nxc,im),  denn(ndn,im)
      integer              idxc(nxc,im),  kprf(im)
!

      integer levs,ntotal,ntoz,ncld,iflip,icfc,icwp,iswsrc(nsrc)
      integer me, lw_type, iovr

      real (kind=kind_rad) prsi(ix,levs+1), prsl(ix,levs),              &
     &                                      prslk(ix,levs),             &
!    &                     del(ix,levs),    prslk(ix,levs),             &
     &                     vvl(ix,levs),    t(ix,levs),                 &
     &                     q(ix,levs*ntotal)
!
      real (kind=kind_rad) xlon(im),    xlat(im)  ,    coszdg(im),      &
     &                     slmsk(im),   sheleg(im),    zorl(im),        &
     &                     coszen(im),  tsea(im),      alvsf(im),       &
     &                     alnsf(im),   alvwf(im),     alnwf(im),       &
     &                     facsf(im),   facwf(im) ,    cv(im),          &
     &                     cvt(im),     cvb(im),       sfcnsw(im),      &
     &                     sfcdlw(im),  tsflw(im),     hprime(im),      &
     &                     stc(im)
      real (kind=kind_rad) swh(ix,levs), hlw(ix,levs),  fluxr(ix,27)

      logical lssav
      integer istrat,k1oz,k2oz
      real (kind=kind_rad) facoz
!
      integer mcld,nseal,nbin,nlon,nlat,ida
      parameter (mcld=4,nseal=2,nbin=100,nlon=2,nlat=4,ida=1)
      real (kind=kind_rad) rhcl(nbin,nlon,nlat,mcld,nseal,ida)
!
!     real (kind=kind_rad) cnwatt, cnprog
!     parameter (cnwatt=-jcal*1.e4/60.,cnprog=1./cnwatt)
!
!     local variables
!     ---------------
      real (kind=kind_rad) solc,rsin1,rcos1,rcos2,raddt,dtlw,exp
      logical lsswr, lslwr
      integer i, j, k, kk, nn, lv, itop, ibtc, ier, ilev, ipr
      real (kind=kind_rad) alvbf(im),alnbf(im),alvdf(im),alndf(im)
!
      real (kind=kind_rad)                                              &
     &                     tgrs(im,levs+1), qgrs(im,levs),              &
     &                     vvel(im,levs),   ogrs(im,levs),              &
     &                     prgi(im,levs+1), prgl(im,levs),              &
     &                     pri2(im,levs+1), prl2(im,levs),              &
     &                     tgr2(im,levs),   qgr2(im,levs),              &
     &                     ogr2(im,levs)                                &
!
     &,                    cldtot(im,levs), cldcnv(im,levs),            &
     &                     clw(im,levs),    clw2(im,levs),              &
     &                     swhr(im,levs),   hlwr(im,levs),              &
     &                     rh(im,levs),     qst(im,levs),               &
     &                     es(im,levs),     prkl(im,levs)
!
      real (kind=kind_rad) gdfvbr(im), gdfnbr(im), gdfvdr(im),          &
     &                     gdfndr(im), flwup(im),  fswup(im),           &
     &                     fswdn(im),  sswup(im),  sswdn(im),           &
     &                     slwup(im),  slwdn(im),  cldsa(im,5)
      integer mbota(im,3), mtopa(im,3)
!
      real (kind=kind_rad) hsigma, daysec, rco2, dltg, qs, tmt0         &
     &,                    tmt15,  qi, qint                             &
     &,                    tem1,   tem
      logical lprnt
!
!
!     do i=1,im
!       tem  = xlon(i) * 57.29578
!       if (tem .lt. 0.0) tem = tem + 360.0
!       tem1 = xlat(i) * 57.29578
!       lprnt =                                                          &
!    &       abs(tem-112.5) .lt. 1.0                                     &
!    &       .and. abs(tem1-25.714) .lt. 0.5
!       if (lprnt) then
!         ipr = i
!         exit
!       endif
!     enddo
!
!ccj
      ogrs=0.
      ogr2=0.
      clw=0.
      clw2=0.
!cc
      if (iflip .eq. 0) then       ! for level index from top
         do k=1,levs
           do i=1,im
             prgi(i,k)  = prsi(i,k)
             prgl(i,k)  = prsl(i,k)
             pri2(i,k)  = prsi(i,levs+1-k)
             prl2(i,k)  = prsl(i,levs+1-k)
           enddo
         enddo
         do i=1,im
           prgi(i,levs+1) = prsi(i,levs+1)
           pri2(i,levs+1) = prsi(i,1)
         enddo
      else                          ! for level index from bottom
         do k=1,levs
           ilev = levs + 1 - k                       ! for mrf
           do i=1,im
             prgi(i,k+1) = prsi(i,ilev)
             prgl(i,k)   = prsl(i,ilev)
             pri2(i,k)    = prsi(i,k)
             prl2(i,k)    = prsl(i,k)
           enddo
         enddo
         do i=1,im
           prgi(i,1)      = prsi(i,levs+1)
           pri2(i,levs+1) = prsi(i,levs+1)
         enddo
      endif
!
      dltg = 360.0 / float(imxae)
      call albaer(slmsk, sheleg, zorl,  coszen,                         &
     &            tsea,  hprime, stc,                                   &
     &            alvsf, alnsf,  alvwf, alnwf,                          &
     &            facsf, facwf,  pi,    dltg,                           &
     &            kprfg, idxcg,  cmixg, denng, xlat, xlon,              &
     &            alvbf, alnbf,  alvdf, alndf, iswsrc(1), kprf, idxc,   &
     &            cmix,  denn,   nxc,   ndn, imxae, jmxae, im, t0c)
!
!
      if (iflip .eq. 0) then       ! for level index from top
        do k=1,levs
          do i=1,im
            tgrs(i,k) = t(i,k)
            qgrs(i,k) = q(i,k)
            vvel(i,k) = vvl(i,levs+1-k)
            tgr2(i,k) = t(i,levs+1-k)
            qgr2(i,k) = q(i,levs+1-k)
          enddo
        enddo
        if (ntoz .eq. 1) then
          do k=1,levs
            kk = levs + 1 - k
            do i=1,im
              ogrs(i,k) = q(i,k+levs)
              ogr2(i,k) = q(i,kk+levs)
            enddo
          enddo
        endif
        if (ncld .gt. 0) then
          do k=1,levs
            do i=1,im
              clw(i,k) = 0.0
            enddo
            do nn=1,ncld
              lv=(1+ntoz)*levs+(nn-1)*levs
              do i=1,im
                clw(i,k)  = clw(i,k)  + q(i,k+lv)
              enddo
            enddo
            do i=1,im
              clw2(i,levs+1-k) = clw(i,k)
            enddo
          enddo
        endif
      else               ! for level index from bottom
        do k=1,levs
          kk = levs + 1 - k
          do i=1,im
            tgrs(i,k) = t(i,kk)
            qgrs(i,k) = q(i,kk)
            vvel(i,k) = vvl(i,k)
            tgr2(i,k) = t(i,k)
            qgr2(i,k) = q(i,k)
          enddo
        enddo
        if (ntoz .eq. 1) then
          do k=1,levs
            kk = levs + 1 - k
            do i=1,im
              ogrs(i,k) = q(i,kk+levs)
              ogr2(i,k) = q(i,k+levs)
            enddo
          enddo
        endif
        if (ncld .gt. 0) then
          do k=1,levs
            kk = levs + 1 - k
            do i=1,im
              clw(i,k) = 0.0
            enddo
            do nn=1,ncld
              lv=(1+ntoz)*levs+(nn-1)*levs
              do i=1,im
                clw(i,k)  = clw(i,k) + q(i,kk+lv)
              enddo
            enddo
            do i=1,im
              clw2(i,kk) = clw(i,k)
            enddo
          enddo
        endif
      endif
!
      do i=1,im
        tgrs(i,levs+1) = tsea(i)
      enddo
!
!                           clw is the condensate path in g/m**2!
!                           -------------------------------------
        if (ncld .gt. 0) then
          do k=1,levs
            do i=1,im
              tem = (prgi(i,k+1) - prgi(i,k)) * (1000000.0/grav)
              clw(i,k) = max(0.0, clw(i,k)) * tem
            enddo
          enddo
        endif
!
!       get relative humidity
!
!       es = 0.001 * fpvs(tgr2)                 ! fpvs in pa
        do k=1,levs
          do i=1,im
!            es(i,k)  = 0.001 * fpvs(tgr2(i,k))                 ! fpvs in pa
            es(i,k)  = min(prl2(i,k), 0.001 * fpvs(tgr2(i,k)))   ! fpvs in pa
            qs       = eps * es(i,k) / (prl2(i,k) + epsm1*es(i,k))
            qint     = max(qs,qmin)
            rh(i,k)  = max(0.0, min(1.0, max(qgr2(i,k),qmin)/qint))
            qst(i,k) = qint
          enddo
        enddo
!
          call getclds(im, im, levs,   tgr2,  qgr2,  vvel, rh, qst,     &
     &                 slmsk,  xlon,  xlat,                             &
     &                 cv,     cvt,   cvb,   rhcl,                      &
     &                 istrat, pri2,   prl2,   cldtot,   cldcnv,        &
     &                 cldsa,  mbota, mtopa, clw2, icwp)
!
      if (ntoz .le. 0) then
        do k=1,levs
          do i=1,im
            prkl(i,k) = prslk(i,k)
          enddo
        enddo
        call getozn(im, levs, ogrs, k1oz, k2oz, facoz,                  &
     &              prkl, iflip, xlat, 48)
        do k=1,levs
          kk = levs + 1 - k
          do i=1,im
            ogr2(i,k) = ogrs(i,kk)
          enddo
        enddo
      endif
!
      do k=1,levs
        do i=1,im
          qst(i,k) = rh(i,k)
        enddo
      enddo
      do k=1,levs
         kk = levs +1 - k
        do i=1,im
          rh(i,k) = qst(i,kk)
        enddo
      enddo
!
        call rdlwsw(im, im, prgi,   prgl,   solc,   slmsk,              &
     &              qgrs,   tgrs,   ogrs,   coszen, cldtot, cldcnv,     &
     &              qgr2,   tgr2,   ogr2,   iovr,                       &
!    &              qgr2,   tgr2,   ogr2,   pri2,   prl2, iovr,         &
     &              alvbf,  alnbf,  alvdf,  alndf,                      &
     &              kprf,   idxc,   cmix,   denn,  rh,                  &
     &              lsswr,  lslwr,  iswsrc, icwp,  clw, icfc, lw_type,  &
     &              hlwr,   slwup,  slwdn,  flwup,                      &
     &              swhr,   sswup,  sswdn,  fswup, fswdn,               &
     &              gdfvbr,gdfnbr,gdfvdr,gdfndr                         &
!
     &,             hsigma, rco2                                        &
     &,             l,lp1,lp1v,llp1,nsrc,nbd,nvb,nae,nbly,nxc,ndn       &
     &,             ndm, nvect, sgtmp,co21d,co22d, co21d3,co21d7        &
     &,             source,dsrce                                        &
     &,             haer, idm, taur                                     &
     &,             lprnt, me)
!
        if (iflip .eq. 0) then       ! for level index from top
          if (lsswr) then
!            dsw   = sswdn - sswup
!            rsw   = fswup
             do k=1,levs
               do i=1,im
                 swh(i,k) = swhr(i,k) * daysec !heating rate in k/s
               enddo
             enddo
          endif
          if (lslwr) then
!            dlw   = slwdn
!            olw   = flwup
!            tsflw = tgrs(levs+1)
             do k=1,levs
               do i=1,im
                 hlw(i,k) = hlwr(i,k) * daysec ! heating rate in k/s
               enddo
             enddo
          endif
        else                          ! for level index from bottom
          if (lsswr) then
!            dsw   = sswdn - sswup
!            rsw   = fswup
             do k=1,levs
               kk = levs + 1 - k
               do i=1,im
                 swh(i,kk) = swhr(i,k) * daysec  ! heating rate (deg/sec)
               enddo
             enddo
          endif
          if (lslwr) then
!            dlw   = slwdn
!            olw   = flwup
!            tsflw = tgrs(levs+1)
             do k=1,levs
               kk = levs + 1 - k
               ilev = levs + 1 - k
               do i=1,im
                 hlw(i,kk) = hlwr(i,k) * daysec  ! heating rate (deg/sec)
               enddo
             enddo
          endif
        endif
!
!...  cnprog is conversion from w/m**2 to progtm units
        if (lsswr) then
!         sfcnsw = (sswdn-sswup)*cnprog
          do i=1,im
            sfcnsw(i) = sswup(i) - sswdn(i)
          enddo
        endif
        if(lslwr) then
!         sfcdlw = slwdn*cnprog
          do i=1,im
            sfcdlw(i) = slwdn(i)
            tsflw(i)  = tgrs(i,levs+1)
          enddo
        endif
!
!....collect the fluxr data for wrtsfc
!..
!....   in previous codes, fluxr(17) contained various attempts at
!        calculating surface albedo...it has proven unsatisfactory!!
!....   so now, sfc albedo will be calculated in wrtsfc as the
!        ratio of the time-mean of the sfcsw fluxes .. kac+mi dec98
!..
        if (lssav) then
          if (lslwr) then
            do i=1,im
              fluxr(i,1 ) = fluxr(i,1 ) + dtlw * flwup(i)
              fluxr(i,19) = fluxr(i,19) + dtlw * slwdn(i)
              fluxr(i,20) = fluxr(i,20) + dtlw * slwup(i)
            enddo
          endif
!swdg proper diurnal sw wgt..coszro=mean cosz over daylight, while
!                           coszdg= mean cosz over entire interval
          if (lsswr) then
            do i=1,im
              if (coszen(i) .gt. 0.) then
                tem       = raddt * coszdg(i) / coszen(i)
                fluxr(i,2 ) = fluxr(i,2)  + fswup(i) * tem
                fluxr(i,3 ) = fluxr(i,3)  + sswup(i) * tem
                fluxr(i,4 ) = fluxr(i,4)  + sswdn(i) * tem
                fluxr(i,18) = fluxr(i,18) + fswdn(i) * tem
              end if
            enddo
!...     save total cloud and bl cloud
            do i=1,im
              fluxr(i,26) = fluxr(i,26) + raddt * cldsa(i,4)
              fluxr(i,27) = fluxr(i,27) + raddt * cldsa(i,5)
            enddo
!
!... save cld frac,toplyr,botlyr and top temp, note that the order
!     of h,m,l cloud is reversed for the fluxr output.
!... save interface pressure (cb) of top/bot
!..
            do k=1,3
            do i=1,im
                tem  = raddt * cldsa(i,k)
                itop = mtopa(i,k)
                ibtc = mbota(i,k)
                fluxr(i,8-k)  = fluxr(i,8-k)  + tem
                fluxr(i,11-k) = fluxr(i,11-k) + pri2(i,itop+1) * tem
                fluxr(i,14-k) = fluxr(i,14-k) + pri2(i,ibtc)   * tem
                fluxr(i,17-k) = fluxr(i,17-k) + tgr2(i,itop)   * tem
              enddo
            enddo
          endif
        endif
!
      return
      end
