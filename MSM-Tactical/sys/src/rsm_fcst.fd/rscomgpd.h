      parameter(nvrken=rslvark+rmlvark*levr,nptken=rlpnt)
      parameter(nstken=rltstp)
!#ifdef RKN
       common/rcomgpd/ svdata(nvrken,nptken,nstken),                     &  
     &         gpdlat(nptken),gpdlon(nptken),gpdgzs(nptken),		 &
     &         gpmlat(nptken),gpmlon(nptken),gpmgzs(nptken),		 &
     &         idstat(nptken)                  !binbin add idstat
       common/rcomgpdi/ 						 &
     &               igpd (nptken),jgpd (nptken),			 &
     &               igpdr(nptken),jgpdr(nptken),			 &
     &               itnum,npoint,isave,isshrt,ilshrt,nsken,		 &
     &               imodk
       common/rcomgpdc/ gpdnam(nptken)
       character*8 gpdnam                     !binbin change back to 8
!#endif
