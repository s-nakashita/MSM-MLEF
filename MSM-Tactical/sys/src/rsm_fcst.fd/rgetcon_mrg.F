      subroutine rgetcon_mrg
!....
      use machine , only : kind_io4
      use funcphys
      use module_progtm
      use physcons, rhoair0 => con_rhoair0 , rhoh2o => con_rhoh2o ,        &
                    rhosnow => con_rhosnow , cliq => con_cliq ,            &
                    cvap => con_cvap  
      use module_mp_wsm3
      use module_mp_wsm5
      use module_mp_wsm6

!ccj
!cccc  jun change
! add isfcmrg in namelist and ilist and rsfcsec in namelist and rlist
!cccc  jun change end
#include <paramodel.h>
!
#include <rscomfc.h>
#include <rscomio.h>
#ifdef CRA
#include <rscomcpu.h>
#endif
#include <rscomloc.h>
!
#include <rscomgpd.h>
!cc
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      dimension ilist(100),rlist(100),clist(100),glist(100)
#endif
      namelist/namrsm/                                                   &  
     &                icen,igen,icen2,ienst,iensi,runid,usrid,           &
     &                deltime,filta,seminpa,ncpus,                       &
     &                fcstsec,prntsec,rswrsec,rlwrsec,                   &
     &                basesec,rdfisec,rkensec,                           &
     &                rlxhsec,rlxmsec,rlxzsec,difhsec,difmsec,rsfcsec,   &
     &                isemimp,iimprlx,idmpjet,imdlphy,ioutnhr,           &
     &                isfcmrg,inewsst
#ifdef RKN
      namelist/namrkn/                                                   &
     &                npoint,idstat,gpdnam,gpdlat,gpdlon,gpdgzs,         & !binbin add: idstat
     &                fhken,nsken,isshrt,ilshrt
#endif
      namelist/namloc/                                                  &
     & rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd &
     &,cproj,ctruth,corient,cdelx,cdely,ccenlat,ccenlon,clftgrd,cbtmgrd &
     &,clat1,clat2,clon1,clon2
      real(kind=kind_io4) fhours
      logical, parameter   ::  allowed_to_read = .true.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  default namelist
      filta=0.92
      semimpa=0.8
      deltime=0
      fcstsec=10800.
      prntsec=10800.
      basesec=10800.
      rlwrsec=3600.
      rswrsec=3600.
      rdfisec=0.
      rkensec=60.
      difhsec=600.
      difmsec=600.
      rlxhsec=1800.
      rlxmsec=1800.
      rlxzsec=1800.
      rsfcsec=86400.
      isemimp=1
      iimprlx=1
      idmpjet=1
      imdlphy=1
      ioutnhr=1
      isfcmrg=0
      inewsst=0 !added by SN
      dtphys=0
      icen=7
      igen=rgen
      icen2=0
      ienst=0
      iensi=0
      runid=0
      usrid=0
      ncpus=1	! for opnemp, default is 1
      ndigyr=4
#ifdef RKN
      npoint=0
      nsken=0
      fhken=1
      gpdlat=0
      gpdlon=0
      isshrt=0
      ilshrt=0
#endif
      call gncpus(ncpus)
      ncpus1=ncpus+1
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,' ncpus = ',ncpus
#ifdef MP
      endif
#endif
!.............................................
! read namelist of forecast parameter
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      read (5,namrsm)
      write(6,namrsm)
#ifdef MP
!--------
      ilist(1)=icen
      ilist(2)=igen
      ilist(3)=icen2
      ilist(4)=ienst
      ilist(5)=iensi
      ilist(6)=ncpus
      ilist(7)=isemimp
      ilist(8)=iimprlx
      ilist(9)=idmpjet
      ilist(10)=imdlphy
      ilist(11)=ioutnhr
      ilist(12)=isfcmrg
      ilist(13)=inewsst !added by SN
!
      rlist(1)=runid
      rlist(2)=usrid
      rlist(3)=deltime
      rlist(4)=filta
      rlist(5)=seminpa
      rlist(6)=fcstsec
      rlist(7)=prntsec
      rlist(8)=rswrsec
      rlist(9)=rlwrsec
      rlist(10)=basesec
      rlist(11)=rdfisec
      rlist(12)=rkensec
      rlist(13)=rlxhsec
      rlist(14)=rlxmsec
      rlist(15)=rlxzsec
      rlist(16)=difhsec
      rlist(17)=difmsec
      rlist(18)=rsfcsec
!
      endif
!
      call rmpbcasti(ilist,13) !modified by SN
      call rmpbcastr(rlist,18)
!
      icen   =ilist(1)
      igen   =ilist(2)
      icen2  =ilist(3)
      ienst  =ilist(4)
      iensi  =ilist(5)
      ncpus  =ilist(6)
      isemimp=ilist(7)
      iimprlx=ilist(8)
      idmpjet=ilist(9)
      imdlphy=ilist(10)
      ioutnhr=ilist(11)
      isfcmrg=ilist(12)
      inewsst=ilist(13) !added by SN
!
      runid  =rlist(1)
      usrid  =rlist(2)
      deltime=rlist(3)
      filta  =rlist(4)
      seminpa=rlist(5)
      fcstsec=rlist(6)
      prntsec=rlist(7)
      rswrsec=rlist(8)
      rlwrsec=rlist(9)
      basesec=rlist(10)
      rdfisec=rlist(11)
      rkensec=rlist(12)
      rlxhsec=rlist(13)
      rlxmsec=rlist(14)
      rlxzsec=rlist(15)
      difhsec=rlist(16)
      difmsec=rlist(17)
      rsfcsec=rlist(18)
!------
#endif
!
! read namelist of ken point station location
! gwv  everybody reads the namelists
!
#ifdef RKN
!j      read (5,namrkn)   !standard input can't read larger than 32k file
!jw      open (500,file='station.parm',status='old')
!jw      read (500,namrkn)   !so, change to 500 to read 1300 stations
!jw      do i=1,npoint
!jw        idstat(i)=i
!jw      enddo
! -- done by Jun Du and Binbin on Jan. 7, 2005
      open (500,file='station.parm',status='old')
      do i = 1, nptken
        read (500,501) gpdlat(i),gpdlon(i),gpdnam(i),ztmp
!hmhj   gpdgzs(i) = float(ztmp)
        gpdgzs(i) = ztmp
        gpdlat(i) = 100.0 + gpdlat(i)
        gpdlon(i) = -1.0*gpdlon(i)
#ifdef MP
      if( mype.eq.master ) then
#endif
          print *,gpdlat(i),gpdlon(i),gpdnam(i),gpdgzs(i)
#ifdef MP
      endif
#endif
501     format(6x,f6.2,1x,f7.2,2x,a4,31x,i4)
      end do
#ifdef MP
!jw
      call rmpsynall
#endif
      close(500)

      npoint = nptken
      do i=1,npoint
        idstat(i)=i
      enddo

!
#ifdef MP
      if( mype.eq.master ) then
#endif
        do i=1,npoint
          print *,idstat(i),gpdnam(i),gpdlat(i),gpdlon(i),gpdgzs(i)
        enddo
#ifdef MP
      endif
#endif
#endif

! read namelist of model location
!
#ifdef MP
      if( mype.eq.master ) then
#endif
      read (5,namloc)
      write(6,namloc)
#ifdef MP
!--------
      glist(1)=rproj
      glist(2)=rtruth
      glist(3)=rorient
      glist(4)=rdelx
      glist(5)=rdely
      glist(6)=rcenlat
      glist(7)=rcenlon
      glist(8)=rlftgrd
      glist(9)=rbtmgrd
      clist(1)=cproj
      clist(2)=ctruth
      clist(3)=corient
      clist(4)=cdelx
      clist(5)=cdely
      clist(6)=ccenlat
      clist(7)=ccenlon
      clist(8)=clftgrd
      clist(9)=cbtmgrd
!
      endif
      call rmpbcastr(glist,9)
      call rmpbcastr(clist,9)
!
      rproj  =glist(1)
      rtruth =glist(2)
      rorient=glist(3)
      rdelx  =glist(4)
      rdely  =glist(5)
      rcenlat=glist(6)
      rcenlon=glist(7)
      rlftgrd=glist(8)
      rbtmgrd=glist(9)
      cproj  =clist(1)
      ctruth =clist(2)
      corient=clist(3)
      cdelx  =clist(4)
      cdely  =clist(5)
      ccenlat=clist(6)
      ccenlon=clist(7)
      clftgrd=clist(8)
      cbtmgrd=clist(9)
!------
#endif
!.............................................
!
! set constant and alter namelist if necessary
      filtb=(1.-filta)*0.5
      deltim=deltime
      if(deltim.le.0.0) deltim=rdelx/10000.*60.
      if(difmsec.le.0.0) difmsec=3*deltim
      if(difhsec.le.0.0) difhsec=6*deltim
      nsseg = nint(fcstsec/deltim)
      nsout = nint(prntsec/deltim)
      nsbas = nint(basesec/deltim)
      nsswr = nint(rswrsec/deltim)
      nslwr = nint(rlwrsec/deltim)
      nsdfi = nint(rdfisec/deltim)
      fhseg = nsseg*deltim/3600.
      fhout = nsout*deltim/3600.
      fhbas = nsbas*deltim/3600.
      fhswr = nsswr*deltim/3600.
      fhlwr = nslwr*deltim/3600.
      fhdfi = nsdfi*deltim/3600.
      lfnhr = ioutnhr.eq.1
      lsfcmrg = isfcmrg.eq.1
      lnewsst = inewsst.eq.1 !added by SN
#ifdef RKN
      if(npoint.gt.0.and.npoint.le.nptken) then
        if(nsken.le.0) then
           fhken=max(fhken,deltim/3600)
        else
          fhken=nsken*deltim/3600
        endif
        fhken=fhout/max(nint(fhout/fhken),1)
        nsken=nint(fhken*3600/deltim)
        if(nsout.gt.nsken*nstken) npoint=0
      else
        npoint=0
      endif
      nsken=max(nsken,1)
#endif
!
!  define unit numbers
!
!  input
      n1    = 11
      n2    = 12
      if(nsseg.le.0 .or. nsout.le.0 .or. nsbas.le.0 ) then 
#ifdef MP
      if( mype.eq.master ) then
#endif
        rewind n1
        rewind n2
        read(n1)
        read(n2)
        read(n1) fhours
        fhour1=fhours
        read(n2) fhours
        fhour2=fhours
        rewind n1
        rewind n2
#ifdef MP
      endif
      call rmpbcastr(fhour1,1)
      call rmpbcastr(fhour2,1)
#endif
        basesec=(fhour2-fhour1)*3600.
        nsbas = nint(basesec/deltim)
        nsseg = nsbas
        nsout = nsbas
        fhbas = nsbas*deltim/3600.
        fhseg = fhbas
        fhout = fhbas
      endif
      nges  = 13
      nflip = 14
      nco2  = 15
      nradr = 21
      nradf = 22
      nnmod = 23
      nmtnv = 24
      nrsmi1= 30
      nrsmi2= 31
      nrflip= 32
      nctune= 43
      nozon = 48
!  output
      n3    = 51
      n4    = 52
      nflop = 53
      nflps = 54
      nsigi = 55
      nsigs = 56
      nznli = 61
      nsfci = 62
      nsfcf = 63
      nznlf = 64
      ndgi  = 65
      ndgf  = 66
      ngpken= 67
      nsfcs = 68
      nznls = 69
      nrsmo1= 70
      nrsmo2= 71
      nrflop= 72
      nrflxi= 74
      nrsmop= 75	! for intermediate sigma file output
      nrsfcp= 76	! for intermediate surface file output
      nrkenp= 77        ! for intermediate ken point output
      nrinit= 78
      nrflxp= 79	! for intermediate flux file output
#ifdef A
      nrbgt = 85
!  work files 
      nrbgt1 = 95
      nrbgt2 = 96
#endif
      nr2dda = 98
!c
      call cmpind
!c      call gpvs
!c      call gtdp
!c      call gthe
!c      call gtma
      call gfuncphys
#ifdef G2R
      call gpln2i
      call epsilo(epsi,jcap)
      call ggozrm(epsi)
      call gft_lonf
#endif
      call grddf
      call grdkt
!
      if (allowed_to_read) then
        if (ncld.eq.2) call wsm3init(rhoair0, rhoh2o, rhosnow, cliq, cvap,  &
                           allowed_to_read)
        if (ncld.eq.4) call wsm5init(rhoair0, rhoh2o, rhosnow, cliq, cvap,  &
                           allowed_to_read)
        if (ncld.eq.5) call wsm6init(rhoair0, rhoh2o, rhosnow, cliq, cvap,  &
                           allowed_to_read)
      endif
!
      return
      end
