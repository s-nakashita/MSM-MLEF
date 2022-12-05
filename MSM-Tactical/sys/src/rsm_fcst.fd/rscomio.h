#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
      common/rcomio/  delx, dely, runid, usrid,                          &    
     &                snnp1(LNWAVS),rnnp1(LNWAVS),rnnp1max,		 &
     &                epsx(LNWAVS),epsy(LNWAVS),epsxmax,epsymax,	 &
     &                dpsx(LNWAVS),dpsy(LNWAVS),			 &
     &                deltim,dt2,dtltb,zhour,phour,fhour,		 &
     &                rcl,sl1,dk,tk
      common/rcomioi/ iimprlx,isemimp,idmpjet,imdlphy,			 &
     &                ifin,icen,igen,icen2,ienst,iensi,ndigyr,		 &
     &                kdt,limlow,numsum,nummax,ncldb1
      common/rcomioi/ nmtnv,nctune,nco2,nozon,				 &
#ifdef  A
     &                nrbgt,nrbgt1,nrbgt2,				 &
#endif
     &                nrinit,nrflxi,nrkenp,				 &
     &                nrsmi1,nrsmi2,nrflip,				 &
     &                nrsmop,nrsfcp,nrflxp,				 &
     &                nrsmo1,nrsmo2,nrflop 
      common/rcomio/  rlxmsec,rlxhsec,rlxzsec,filta,filtb,seminpa,	 &
     &                difmsec,difhsec,rsfcsec
      common/rcomio/  fhseg,fhout,fhswr,fhlwr,fhbas,fhdfi,fhken
      common/rcomioi/ nsseg,nsout,nsswr,nslwr,nsbas,nsdfi
      logical         lfnhr,lsfwd,lsftr,lssav,lsout,lsimp,lsphy,         &  
     &                lscca,lsswr,lslwr,lsbgt,lsken,lsdmp,lsfcmrg,lnewsst 
      common/rcomiol/ lfnhr,lsfwd,lsftr,lssav,lsout,lsimp,lsphy,         &  
     &                lscca,lsswr,lslwr,lsbgt,lsken,lsdmp,lsfcmrg,lnewsst 
