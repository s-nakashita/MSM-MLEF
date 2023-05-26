      subroutine rfwdstep
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
#include <rscommap.h>
#include <rscomgpd.h>
!cc
      parameter(mdtgen=2)
! ..............................................................
! ..  first step is forward. reduce dt to dt/(2**n)
! ..............................................................
      lsimp=isemimp.eq.1
      lsdmp=idmpjet.eq.1
      lsphy=imdlphy.eq.1
#ifdef A
      lsbgt=.false.
#endif
      lsftr=.false.    ! the same as lsgen=.true.
      lssav=.true.
      deltim=deltim/( 2. ** mdtgen )
      dtltb=deltim
      dt2=2*deltim
      do 400 jdt=1,mdtgen
!
        lsfwd=jdt.eq.1
        lsout=(mod(kdt,nsout).eq.0.or.phour.eq.0.).and.jdt.eq.mdtgen
        lscca=mod(kdt,nsswr).eq.0.and.jdt.eq.mdtgen
        lsswr=mod(kdt,nsswr).eq.1.and.jdt.eq.1
        lslwr=mod(kdt,nslwr).eq.1.and.jdt.eq.1
#ifdef RKN
        lsken=npoint.gt.0.and.(mod(kdt,nsken).eq.0.or.phour.eq.0.).and.  & 
     &        jdt.eq.mdtgen
        isave = 0
        if(lsken) isave = 1
#endif
!
        call rsmstep
!....
!....< advance deltim in double >
        if(jdt.ne.1) dtltb = deltim
        deltim= deltim*2.e0
        dt2   = deltim * 2.0
!
 400  continue
!
      if(lsout) then
        secphy=(fhour-zhour)*3600.
        secswr=max(secphy,fhswr*3600.)
        seclwr=max(secphy,fhlwr*3600.)
        call rwrtsfc(zhour,fhour,idate,nrflxi,slmsk,rlat,rlon,           & 
     &            dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,stc,sheleg,           &
     &            secswr,seclwr,dlwsfc,ulwsfc,                           &
     &            geshem,bengsh,gflux,fluxr,                             &
     &            u10m,v10m,t2m,q2m,psurf,zorl,tmpmax,tmpmin,            &
     &            spfhmax,spfhmin,wvuflx,wvvflx,                         &
     &            srunoff,soilm,snwdph,                                  &
     &            runoff,ep,cldwrk,dugwd,dvgwd,hpbl,pwat,                &
     &            icen,igen,icen2,ienst,iensi)
#ifdef MP
      if( mype.eq.master ) then
#endif
        close(nrflxi)
#ifdef MP
      endif
#endif
        call rzerout
      endif
!
#ifdef RKN
      if(lsken) then
!
#ifdef MP
        call rmpkencollect(itnum,nvrken,nptken,nstken,svdata)
        if( mype.eq.master ) then
#endif
        open(unit=nrkenp,file='r_ken.f00',                               &  
     &           status='unknown',form='unformatted')
#ifdef MP
        endif
#endif
        label(1)='ncep'
        label(2)='rsm'
        label(3)='mpi'
        label(4)='version'
        call rkenwrt(nrkenp,label,fhour,idate,si,sl)
#ifdef MP
        if( mype.eq.master ) then
#endif
        close(nrkenp)
#ifdef MP
        endif
#endif
!
! reset itnum after writing point sounding data.
!
        itnum=1
        do k=1,nstken
          do j=1,nptken
            do i=1,nvrken
              svdata(i,j,k) = 0.
            enddo
          enddo
        enddo
!
      endif
#endif
!
      return
      end
