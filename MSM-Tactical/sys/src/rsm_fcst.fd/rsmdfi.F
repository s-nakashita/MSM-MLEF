      subroutine rsmdfi
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
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
!cc
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  include first two time levels
      kdtdfi=kdt+nsdfi
      call rdfini(-nsdfi-1,nsdfi)
      call rdfini(kdt-kdtdfi,nsdfi)
      kdt=kdt+1
      fhour=kdt*deltim/3600
      call rfwdstep
      phour=fhour
      call rdfini(kdt-kdtdfi,nsdfi)
!
      lsimp=isemimp.eq.1
      lsdmp=idmpjet.eq.1
      lsphy=imdlphy.eq.1
      lsftr=.true.
#ifdef A
      lsbgt=.true.
#endif
      lsfwd=.false.
      lsftr=.true.
      lssav=.true.
      idt=kdt+1
      mdt=kdtdfi
      do jdt=idt,mdt
        kdt=kdt+1
        fhour=kdt*deltim/3600
        lsout=mod(kdt,nsout).eq.0
        lscca=mod(kdt,nsswr).eq.0
        lsswr=mod(kdt,nsswr).eq.1
        lslwr=mod(kdt,nslwr).eq.1
#ifdef RKN
        lsken=npoint.gt.0.and.mod(kdt,nsken).eq.0
#endif
        call rsmstep
        call rdfini(kdt-kdtdfi,nsdfi)
        if(lsout) then
          call rwrtout
          call rzerout
        endif
        phour=fhour
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  save surface conditions
      rewind nrflop
      call rfixio(label,fhour,idate,tsea,smc,sheleg,stc,tg3,zorl,        & 
     &           cv,cvb,cvt,slmsk,f10m,canopy,                           &
     &           vfrac,vtype,stype,uustar,ffmm,ffhh,                     &
     &           alvsf,alvwf,alnsf,alnwf,facsf,facwf,                    &
     &           1,nrflop,rsfcsec,lsfcmrg)
! save base field
#ifdef MP
      if (mype .eq. master) then
#endif
      rewind nrinit
      write(nrinit) qb
      write(nrinit) teb
      write(nrinit) rqb
      write(nrinit) uub
      write(nrinit) vvb
      write(nrinit) qltb
      write(nrinit) teltb
      write(nrinit) rqltb
      write(nrinit) uultb
      write(nrinit) vvltb
#ifdef MP
      endif
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  include time levels up to hour fhour+2*fhdfi
!  but do not save diagnostics for this time
      lsimp=isemimp.eq.1
      lsdmp=idmpjet.eq.1
      lsphy=imdlphy.eq.1
      lsftr=.true.
#ifdef A
      lsbgt=.false.
#endif
      lssav=.false.
      lsout=.false.
#ifdef RKN
      lsken=.false.
#endif
      idt=kdt+1
      mdt=kdt+nsdfi
      do jdt=idt,mdt
        kdt=kdt+1
        fhour=kdt*deltim/3600
        lscca=mod(kdt,nsswr).eq.0
        lsswr=mod(kdt,nsswr).eq.1
        lslwr=mod(kdt,nslwr).eq.1
        call rsmstep
        call rdfini(kdt-kdtdfi,nsdfi)
        phour=fhour
      enddo
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  do final digital filter
      call rdfini(nsdfi+1,nsdfi)
      do j=1,LNWAVS
      qm(j)=q(j)
      enddo
      do k=1,levr
      do j=1,LNWAVS
      tem(j,k)=te(j,k)
      uum(j,k)=uu(j,k)
      vvm(j,k)=vv(j,k)
      enddo
      enddo
      do nc=1,ntotal
      do k=1,levr
      do j=1,LNWAVS
       rm(j,k,nc)=rq(j,k,nc)
      enddo
      enddo
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  replace surface conditions
      rewind nrflop
      call rfixio(label,fhour,idate,tsea,smc,sheleg,stc,tg3,zorl,        &  
     &           cv,cvb,cvt,slmsk,f10m,canopy,                           &
     &           vfrac,vtype,stype,uustar,ffmm,ffhh,                     &
     &           alvsf,alvwf,alnsf,alnwf,facsf,facwf,                    &
     &           0,nrflop,rsfcsec,lsfcmrg)
!  replace base field
#ifdef MP
      if (mype .eq. master) then
#endif
      rewind nrinit
      read(nrinit) qb
      read(nrinit) teb
      read(nrinit) rqb
      read(nrinit) uub
      read(nrinit) vvb
      read(nrinit) qltb
      read(nrinit) teltb
      read(nrinit) rqltb
      read(nrinit) uultb
      read(nrinit) vvltb
#ifdef MP
      endif
      call rmpbcastr(qb,lngrdb)
      call rmpbcastr(teb,lngrdb*levs)
      call rmpbcastr(rqb,lngrdb*levs)
      call rmpbcastr(uub,lngrdb*levs)
      call rmpbcastr(vvb,lngrdb*levs)
      call rmpbcastr(qltb,lngrdb)
      call rmpbcastr(teltb,lngrdb*levs)
      call rmpbcastr(rqltb,lngrdb*levs)
      call rmpbcastr(uultb,lngrdb*levs)
      call rmpbcastr(vvltb,lngrdb*levs)
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  reset clock and output if necessary
      kdt=kdtdfi
      fhour=kdt*deltim/3600
      lsout=mod(kdt,nsout).eq.0
      if(lsout) then
        call rwrtout
        call rzerout
      endif
      phour=fhour
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
