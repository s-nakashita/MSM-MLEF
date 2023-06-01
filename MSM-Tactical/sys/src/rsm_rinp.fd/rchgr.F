      subroutine rchgr(nin,nsfcin,nslmsk,norog,nou,nsfcou,nsig,          &   
     &                 lsig,lmtn,lhor,lc2r,ls2r,lp2r,percmtn,            &
     &                 lsst,lmd)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rchgr
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  regional change resolution for sigma file and surface file
!
! program history log:
!
! usage:    call rchgr(nin,nsfcin,nslmsk,norog,mtnvar,
!          1           nou,nsfcou,lsig,lmtn,lhor)
!   input argument list:
!     nin       - unit number for sigma file input
!     nsfcin    - unit number for surface file input
!     nslmsk    - unit number for sea-land mask file input
!     norog     - unit number for terrain file input
!     mtnvar    - unit number for mountain variance file input
!     nou       - unit number for sigma file output
!     nsfcou    - unit number for surface file output
!     lsig      - logical index of using new sigma layes
!     lmtn      - logical index of using new mountain
!     lhor      - logical index of horizontal interpolation
!
!   output argument list:
!
!   input files: see input argument
!
!   output files: see input argument
!
!   subprograms called: 
!      rread  rsetsig  maxmin  newsig  extrap  rwrite  
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use machine , only : kind_io4, kind_io8
      use newsst_module, only : new_sst !added by SN
!ccj
#include <paramodel.h>
!cc
!
! change resolution in regional domain
!
      parameter(idimi=igrd1i,jdimi=jgrd1i,ijdimi=idimi*jdimi,            &                 
     &          kdimi=levri,kdimpi=kdimi+1,kdimmi=kdimi-1)
      parameter(idimo=igrd1o,jdimo=jgrd1o,ijdimo=idimo*jdimo,            &
     &          kdimo=levro,kdimpo=kdimo+1,kdimmo=kdimo-1)
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
     &       vv(ijdimo,kdimi),rq(ijdimo,kdimi,ntotal),gz(ijdimo)         &
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
     &      ,wwo(ijdimo,kdimo+1)                                         &
#endif
     &      ,tmpcomrinp4
!
      common/inpver/siin(kdimpi),slin(kdimi),delin(kdimi),               &
     &               ciin(kdimpi),clin(kdimi),rpiin(kdimmi)
      common/comvet/si (kdimpo),sl (kdimo),del (kdimo),                  &
     &               ci(kdimpo),cl (kdimo),rpi (kdimmo)
!
      common/llgz/ terr(ijdimo)
      character*8 label(4)
!
      dimension ggridil(ijdimi,lsoil),ggridol(ijdimo,lsoil)
      dimension ggridn(ijdimo),slmsk(ijdimo)
      dimension ggridi1(ijdimi),ggrido1(ijdimo)
      dimension ggridi2(ijdimi),ggrido2(ijdimo)
      dimension ggridi3(ijdimi),ggrido3(ijdimo)
      dimension ggridi4(ijdimi),ggrido4(ijdimo)
      dimension pgz(ijdimo),fgz(lnwav)
!
      real(kind=kind_io4) fhours
      real(kind=kind_io4) sgridil(ijdimi,lsoil),sgridol(ijdimo,lsoil)
      real(kind=kind_io4) sgridn(ijdimo)
      real(kind=kind_io4) sgridi1(ijdimi),sgrido1(ijdimo)
      real(kind=kind_io4) sgridi2(ijdimi),sgrido2(ijdimo)
      real(kind=kind_io4) sgridi3(ijdimi),sgrido3(ijdimo)
      real(kind=kind_io4) sgridi4(ijdimi),sgrido4(ijdimo)
!
      integer version
!
      logical lsig,lmtn,lhor,lc2r,ls2r,lp2r,lsst,lmd
!
! added by SN
      integer issttype
      namelist/namsst/ issttype
      data issttype/0/
      integer instmsk(ijdimo)
      real (kind=kind_io8) fha(5)
      integer ida(8),jda(8)
! added by SN
!
      print *,' +++++++++++++ start regional chgr ++++++++++ '
      if(lhor) then
        print *,' +++ change horizontal resolution ++ '
        print *,' +++ have not yet implemented!!!! ++ '
        print *,' +++ abort!!!!!!                  ++ '
        call abort
      endif
!
!  read in original initial data
!
      if( lc2r ) then
! ----------------------------------------------------------------------
      rewind nin
      call rread(nin,label,fhour,idate,kdimi,                            &             
     &           flati,floni,fm2i,fm2xi,fm2yi,slin,siin,                 &
     &           gzi,qi,tei,uui,vvi,rqi                                  &
#ifdef NONHYD
     &                     ,ppi,tti,wwi                                  &
#endif
     &                                 )
!
      print 1101,fhour,idate
 1101 format(1h0,1x,'original initial data read in',/,                   &
     &           3x,'fhour=',f6.1,' ihour=',i2,' month=',i2,' day=',i2,  &
     &              ' year=',i4)
      print 1102,idimi,jdimi,ijdimi,kdimi
 1102 format(1x,'output file resolution =',6i10)
      print 1102,idimo,jdimo,ijdimo,kdimo
 1103 format(1x,'output file resolution =',6i10)
!
!      call sigset(ciin, siin, delin, slin, clin, rpiin, kdimi)
!
      print *,' --- check input --- '
      call maxmin(uui,ijdimi,kdimi,1,kdimi,' uui ')
      call maxmin(vvi,ijdimi,kdimi,1,kdimi,' vvi ')
      call maxmin(tei,ijdimi,kdimi,1,kdimi,' tei ')
      call maxmin(rqi(1,1,1),ijdimi,kdimi,1,kdimi,' rqi ')
      if(ntoz.eq.1)                                                      &                       
     & call maxmin(rqi(1,1,2),ijdimi,kdimi,1,kdimi,' ozi ')       
      if(ncld.eq.1)                                                      &
     & call maxmin(rqi(1,1,3),ijdimi,kdimi,1,kdimi,' cwi ')
      call maxmin( qi,ijdimi,    1,1,    1,'  qi ')
      call maxmin(fm2i,ijdimi,    1,1,    1,'  fm2i ')
!
!  define new sigma layer/level ?
!
      if( lsig ) then
         call newsig(nsig,ci,si,del,sl,cl,rpi)
      else
         do 1110 k=1,kdimo
           sl(k) = slin(k)
           si(k) = siin(k)
1110     continue
         si(kdimo+1) = siin(kdimi+1)
         call sigset(ci, si, del, sl, cl, rpi, kdimo)
      endif
!
!
      if( lhor ) then
        print *,' +++ do change horizontal resolution ++ '
        print *,' +++ have not yet implemented  +++ '
        stop
      else
        do 2010 n=1,ijdimo
        flato(n) = flati(n)
        flono(n) = floni(n)
        fm2o(n)  = fm2i(n)
        fm2xo(n) = fm2xi(n)
        fm2yo(n) = fm2yi(n)
        gz(n) = gzi(n)
        q(n) = qi(n)
2010    continue
        do 2020 k=1,kdimi
        do 2020 n=1,ijdimo
        uu(n,k) = uui(n,k)
        vv(n,k) = vvi(n,k)
        te(n,k) = tei(n,k)
#ifdef NONHYD
        pp(n,k) = ppi(n,k)
        tt(n,k) = tti(n,k)
#endif
2020    continue
#ifdef NONHYD
        do k=1,kdimi+1
          do n=1,ijdimo
            ww(n,k) = wwi(n,k)
          enddo
        enddo
#endif
        do 2030 nc=1,ntotal
        do 2030 k=1,kdimi
        do 2030 i=1,ijdimo
        rq(i,k,nc) = rqi(i,k,nc)
2030    continue
      endif
!
          print *,'in rchgr 1,lngrd=',lngrd,'ijdimo=',ijdimo,            &
     &    'ijdimi=',ijdimi
      if(lmtn) then
        read(norog) sgrido1
        call sgl2ful(sgrido1,gzo,ijdimo)
        print *,'gz of output resolution read in'
        call maxmin(gzo,ijdimo,1,1,1,'  gzo ')
        call maxmin(terr,ijdimo,1,1,1,'  terr ')
        if(percmtn.gt.0.0) then
! modify lateral boundary mountain
          xc = float(igrd)/2.0 + 1.0  
          yc = float(jgrd)/2.0 + 1.0 
          xs = percmtn*xc
          ys = percmtn*yc
          xc1= xc - xs - 1.0            
          yc1= yc - ys - 1.0 
          pow=0.5
!         pow=1.0
          pi=2.*asin(1.0)
          do lat = 1,jgrd1
            yls= min( 1.0, max( abs(lat-yc)-yc1,0.0 )/ys )
            yls= (1.+sin(pi*(yls-0.5)))/2.
            jlats=(lat-1)*igrd1
            do i=1,igrd1      
              xl = min( 1.0, max( abs(i   -xc)-xc1,0.0 )/xs )
              xl = (1.+sin(pi*(xl-0.5)))/2.
              coes = max(xl,yls)
              if( coes.lt.1.e-6 ) then
                coes = 0.0
              else
                coes = coes ** pow
              endif
              if (lp2r) then
              gzo(i+jlats)=terr(i+jlats)*coes+gzo(i+jlats)*(1.-coes)
              else
              gzo(i+jlats)=gz(i+jlats)*coes+gzo(i+jlats)*(1.-coes)
              endif
            enddo
          enddo
          print *,'in rchgr 2  ,lngrd=',lngrd,'ijdimo=',ijdimo,          &
     &    'ijdimi=',ijdimi
! modify gzo with regional spectral smooth
! init spectral constant
          call plnini
          print *,'in rchgr 3  ,lngrd=',lngrd,'ijdimo=',ijdimo,          &
     &    'ijdimi=',ijdimi
          call rftini
          print *,'after rftini,lngrd=',lngrd,'ijdimo=',ijdimo,          &
     &    'ijdimi=',ijdimi
! get ptb
          do ij=1,lngrd
            if (lp2r) then
            pgz(ij)=gzo(ij)-terr(ij)
            else 
            pgz(ij)=gzo(ij)-gz(ij)
            endif
          enddo
          print *,'after pgz'
          call maxmin(pgz,ijdimo,1,1,1,' before gdtocc, pgz ')
! grid to coef back to grid
          call gdtocc(pgz,fgz,1)
          call cctogd(fgz,pgz,1)
          print *,'after cc gd pgz'
          call maxmin(pgz,ijdimo,1,1,1,' after cctogd, pgz ')
! get full
          do ij=1,lngrd
            if (lp2r) then
            gzo(ij)=pgz(ij)+terr(ij)
            else
            gzo(ij)=pgz(ij)+gz(ij)
            endif
          enddo
          print *,'gz of lateral boundar modified'
          call maxmin(gzo,ijdimo,1,1,1,'  gzo ')
        endif
!
      else
        do 3000 n=1,ijdimo
        gzo(n) = gz(n)
3000    continue
      endif
!ccccccccc
!
      print *,' ----- check grid value before sg2sg ---- '
      call maxmin(uu,ijdimo,kdimi,1,kdimi,' uu ')
      call maxmin(vv,ijdimo,kdimi,1,kdimi,' vv ')
      call maxmin(te,ijdimo,kdimi,1,kdimi,' te ')
      call maxmin(rq(1,1,1),ijdimo,kdimi,1,kdimi,' rq ')
      if(ntoz.eq.1)                                                      &                       
     & call maxmin(rq(1,1,2),ijdimo,kdimi,1,kdimi,' oz ')
      if(ncld.eq.1)                                                      &
     & call maxmin(rq(1,1,3),ijdimo,kdimi,1,kdimi,' cw ')
      call maxmin( q,ijdimo,    1,1,    1,'  q ')
#ifdef NONHYD
      call maxmin(pp,ijdimo,kdimi,1,kdimi,' pp ')
      call maxmin(tt,ijdimo,kdimi,1,kdimi,' tt ')
      call maxmin(ww,ijdimo,kdimi+1,1,kdimi+1,' ww ')
#endif
!
      if( lsig .or. lmtn ) then
          call extrap(lsig,lmtn,lp2r)
      else
        do 3010 n=1,ijdimo
        qo(n) = q(n)
3010    continue
        do 3020 k=1,kdimo
        do 3020 n=1,ijdimo
        uuo(n,k) = uu(n,k)
        vvo(n,k) = vv(n,k)
        teo(n,k) = te(n,k)
#ifdef NONHYD
        ppo(n,k) = pp(n,k)
        tto(n,k) = tt(n,k)
#endif
3020    continue
#ifdef NONHYD
        do k=1,kdimo
          do n=1,ijdimo
            wwo(n,k) = ww(n,k)
          enddo
        enddo
#endif
        do 3030 nc=1,ntotal
        do 3030 k=1,kdimo
        do 3030 i=1,ijdimo
        rqo(i,k,nc) = rq(i,k,nc)
3030    continue
      endif
!
      print *,' --- check output --- '
      call maxmin(uuo,ijdimo,kdimo,1,kdimo,' uuo ')
      call maxmin(vvo,ijdimo,kdimo,1,kdimo,' vvo ')
      call maxmin(teo,ijdimo,kdimo,1,kdimo,' teo ')
      call maxmin(rqo(1,1,1),ijdimo,kdimo,1,kdimo,' rqo ')
      if(ntoz.eq.1)                                                      &                  
     & call maxmin(rqo(1,1,2),ijdimo,kdimo,1,kdimo,' ozo ')
      if(ncld.eq.1)                                                      &
     & call maxmin(rqo(1,1,3),ijdimo,kdimo,1,kdimo,' cwo ')
      call maxmin( qo,ijdimo,    1,1,    1,'  qo ')
      call maxmin( flato,ijdimo,    1,1,    1,'  flato ')
      call maxmin( flono,ijdimo,    1,1,    1,'  flono ')
#ifdef NONHYD
      call maxmin(ppo,ijdimo,kdimo,1,kdimo,' ppo ')
      call maxmin(tto,ijdimo,kdimo,1,kdimo,' tto ')
      call maxmin(wwo,ijdimo,kdimo+1,1,kdimo+1,' wwo ')
#endif
!
      if(lmd.and.(fhour.ne.0.0)) then
! for nonhyd initial w in rsm_fcst
        print*,'Warning!! initial fhour is not zero!!!'
        print*,' fhour= ',fhour
        fha=0
        ida=0
        jda=0
        fha(2)=fhour
        ida(1)=idate(4)
        ida(2)=idate(2)
        ida(3)=idate(3)
        ida(5)=idate(1)
        call w3movdat(fha,ida,jda)
        idate(4)=jda(1)
        idate(2)=jda(2)
        idate(3)=jda(3)
        idate(1)=jda(5)
        print*,'Warning!! initial non-zero fhour is set to be zero!!!'
        fhour=0.
        print 1104,fhour,idate
 1104   format(1h0,1x,'modified date ',/,                   &
     &           3x,'fhour=',f6.1,' ihour=',i2,' month=',i2,' day=',i2,  &
     &              ' year=',i4)
      endif
!
      call rwrite(nou,label,fhour,idate,kdimo,                           &
     &            flato,flono,fm2o,fm2xo,fm2yo,sl,si,                    &
     &            gzo,qo,teo,uuo,vvo,rqo                                 &
#ifdef NONHYD
     &                      ,ppo,tto,wwo                                 &
#endif
     &                                  )
!
! --------------------------------------------------------------------
      endif
!
!  addition for the change resolution of surface file
!
      if( ls2r ) then
! ********************************************************************
      rewind nsfcin
      read (nsfcin) label
      print *,' end read label of sfc.'
      write(nsfcou) label
      read (nsfcin) fhours,idate,lon,lat,version
      fhour=fhours
      print *,' read fhour,idate of sfc.'
      print *,'fhour,idate=',fhour,idate
      if(lmd.and.(fhour.ne.0.0)) then
        fha=0
        ida=0
        jda=0
        fha(2)=fhour
        ida(1)=idate(4)
        ida(2)=idate(2)
        ida(3)=idate(3)
        ida(5)=idate(1)
        call w3movdat(fha,ida,jda)
        idate(4)=jda(1)
        idate(2)=jda(2)
        idate(3)=jda(3)
        idate(1)=jda(5)
        fhours=0.
        print 1104,fhours,idate
      endif
      write(nsfcou) fhours,idate,lon,lat,version
!
      do 5900 nfld = 3,100
!
      if (nfld.eq.3 .and. lsst) then
         read (nsfcin,end=6999) sgridi1
         call sgl2ful(sgridi1,ggridn,ijdimo)
!
! read regional land-sea mask
         rewind nslmsk
         read(nslmsk) sgridn
         call sgl2ful(sgridn,slmsk,ijdimo)
         rewind nslmsk
!
! use new sst on sea points
!
         rewind(5)
         read(5,namsst)
         call maxmin(ggridn,ijdimo,1,1,1,'tsea before') !added by SN
         !call ana_sst(ggrido1,ijdimo,flato,flono)
         call new_sst(sgrido1,instmsk,ijdimo,flato,flono,issttype) !added by SN
         call sgl2ful(sgrido1,ggrido1,ijdimo)
         do n=1,ijdimo
           if( slmsk(n).eq.0.0.and.instmsk(n).eq.0 ) then
!              & .and.ggrido1(n).gt.0.0 ) then
             ggridn(n)= max( ggrido1(n), 271.21e0 )
           endif
         enddo
         call maxmin(ggridn,ijdimo,1,1,1,'tsea')
         call ful2sgl(ggridn,sgrido1,ijdimo)
         write(nsfcou) sgrido1
         print *,' write output grid sfc. rec =',nfld
!
      else if( nfld.eq.4 .or. nfld.eq.6 ) then
         read (nsfcin,end=6999) sgridil
         call sgl2ful(sgridil,ggridil,lsoil*ijdimi)
         print *,' read input grid sfc. rec =',nfld
         call maxmin(ggridil,ijdimi,lsoil,1,lsoil,'  ')
         if( lhor ) then
           print *,' +++ do change horizontal resolution ++ '
           print *,' +++ have not yet implemented  +++ '
           stop
         else
            do 5015 l=1,lsoil
            do 5010 n=1,ijdimo
              ggridol(n,l) = ggridil(n,l)
5010        continue
5015        continue
         endif
         call ful2sgl(ggridol,sgridol,lsoil*ijdimo)
         write(nsfcou) sgridol
         print *,' write output grid sfc. rec =',nfld
!
      else if( nfld.eq.12 ) then
         read (nsfcin,end=6999) sgridi1,sgridi2,sgridi3,sgridi4
         call sgl2ful(sgridi1,ggridi1,ijdimi)
         call sgl2ful(sgridi2,ggridi2,ijdimi)
         call sgl2ful(sgridi3,ggridi3,ijdimi)
         call sgl2ful(sgridi4,ggridi4,ijdimi)
         print *,' read input grid sfc. rec =',nfld
         call maxmin(ggridi1,ijdimi,1,1,1,'alvsf')
         call maxmin(ggridi2,ijdimi,1,1,1,'alvwf')
         call maxmin(ggridi3,ijdimi,1,1,1,'alnsf')
         call maxmin(ggridi4,ijdimi,1,1,1,'alnwf')
         if( lhor ) then
           print *,' +++ do change horizontal resolution ++ '
           print *,' +++ have not yet implemented  +++ '
           stop
         else
            do 5012 n=1,ijdimo
              ggrido1(n) = ggridi1(n)
              ggrido2(n) = ggridi2(n)
              ggrido3(n) = ggridi3(n)
              ggrido4(n) = ggridi4(n)
5012        continue
         endif
         call ful2sgl(ggrido1,sgrido1,ijdimo)
         call ful2sgl(ggrido2,sgrido2,ijdimo)
         call ful2sgl(ggrido3,sgrido3,ijdimo)
         call ful2sgl(ggrido4,sgrido4,ijdimo)
         write(nsfcou) sgrido1,sgrido2,sgrido3,sgrido4
         print *,' write output grid sfc. rec =',nfld
!
      else if(nfld.eq.13) then  !!!! for sea land mask
         read (nsfcin,end=6999) sgridi1
         call sgl2ful(sgridi1,ggridi1,ijdimi)
         print *,' read input grid sfc. rec =',nfld
         call maxmin(ggridi1,ijdimi,1,1,1,'  ')
         if( lhor ) then
           print *,' +++ do change horizontal resolution ++ '
           print *,' +++ have not yet implemented  +++ '
           stop
         else
            do 5013 n=1,ijdimo
              ggrido1(n) = ggridi1(n)
5013        continue
         endif
         rewind nslmsk
         read (nslmsk,end=5115) sgridn
         rewind nslmsk
         call sgl2ful(sgridn,ggridn,ijdimo)
         print *,' read input grid of slmask'
         call maxmin(ggridn,ijdimo,1,1,1,' slmask')
         do 5113 n=1,ijdimo
           if( ggrido1(n).le.1.0 ) ggrido1(n)=0.0
           ggrido1(n) = max(ggridn(n),ggrido1(n))
5113     continue
5115     continue
         call ful2sgl(ggrido1,sgrido1,ijdimo)
         write(nsfcou) sgrido1
         print *,' write output grid sfc. rec =',nfld
      else if( nfld.eq.19 ) then
         read (nsfcin,end=6999) sgridi1,sgridi2
         call sgl2ful(sgridi1,ggridi1,ijdimi)
         call sgl2ful(sgridi2,ggridi2,ijdimi)
         print *,' read input grid sfc. rec =',nfld
         call maxmin(ggridi1,ijdimi,1,1,1,'facsf')
         call maxmin(ggridi2,ijdimi,1,1,1,'facwf')
         if( lhor ) then
           print *,' +++ do change horizontal resolution ++ '
           print *,' +++ have not yet implemented  +++ '
           stop
         else
            do 5019 n=1,ijdimo
              ggrido1(n) = ggridi1(n)
              ggrido2(n) = ggridi2(n)
5019        continue
         endif
         call ful2sgl(ggrido1,sgrido1,ijdimo)
         call ful2sgl(ggrido2,sgrido2,ijdimo)
         write(nsfcou) sgrido1,sgrido2
         print *,' write output grid sfc. rec =',nfld
!
      else
         read (nsfcin,end=6999) sgridi1
         call sgl2ful(sgridi1,ggridi1,ijdimi)
         print *,' read input grid sfc. rec =',nfld
         call maxmin(ggridi1,ijdimi,1,1,1,'  ')
         if( lhor ) then
           print *,' +++ do change horizontal resolution ++ '
           print *,' +++ have not yet implemented  +++ '
           stop
         else
            do 5055 n=1,ijdimo
              ggrido1(n) = ggridi1(n)
5055        continue
         endif
         call ful2sgl(ggrido1,sgrido1,ijdimo)
         write(nsfcou) sgrido1
         print *,' write output grid sfc. rec =',nfld
      endif
!
5900  continue
! ----- end of the surface loop ---------
!
6999  continue
      print *,'hit end of file reading sfc file'
! ******************************************************************
      endif
!
      close(nsfcou)
!
      print *,' ++++++ normal exit of regional chgr ++++ '
      return
!
      end
