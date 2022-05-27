      subroutine rsmsmf(fhnxt,ggz,gq,gte,gdu,gzv,grq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsmsmf
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do regional model forecast.
!
! program history log:
!
! usage:    call rsmsmf(gfhour,gshour,ggz,gq,gte,gdi,gze,grq)
!   input argument list:
!     gfhour    - global forecast hour
!     gshour    - global current time in second
!     ggz       - global terrain coefficient
!     gq        - global log surface pressure coefficient
!     gte       - global virtual temperature coefficient
!     gdi       - global divergence coefficient
!     gze       - global vorticity coefficient
!     grq       - global specific humidity coefficient
!
!   output argument list:
!
!   common block:
!     rscomf  rscomltb  comcon
!
!   input files: none
!
!   output files: none
!
!   subprograms called: 
!       rsmltb  rdfini  rmsgtr  rloopr  rbmcm  rloopa  rloopb  rdeldif
!       rsicdif  rlatbnd  rupdate  rfilt1  rfilt2
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!.................................................................
! regional spectral model developed by hann-ming henry juang
!       version with nested to global spectral model
!.................................................................
!.....
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
#include <rscomgpd.h>
!cc
#ifdef MP
#define LNT22S lnt22p
#else
#define LNT22S lnt22
#endif
!
#ifdef G2R
      dimension                                                          &  
     &    ggz(LNT22S), gq(LNT22S)                                        &
     &  , gte(LNT22S,levs), grq(LNT22S,levs,ntotal)                      &
     &  , gdu(LNT22S,levs), gzv(LNT22S,levs)
#endif
#ifdef C2R
      dimension                                                          &
     &    ggz(clngrd), gq(clngrd)                                        &
     &  , gte(clngrd,levr), grq(clngrd,levr,ntotal)                      &
     &  , gdu(clngrd,levr), gzv(clngrd,levr)
#endif
!
!test
      dimension tmpsp(lnwav,levrp1)
      logical lsouf
!
#ifdef MP
        if (mype.eq.master) then
#endif
      print *,' ------ into rsmsmf ----- '
#ifdef MP
        endif
#endif
!
! ....... run regional per certain seconds
      secnow=phour*3600.
      secnxt=fhnxt*3600.
      if( secnxt.le.secnow ) return
#ifdef MP
        if (mype.eq.master) then
#endif
      print *,' run rsm from ',secnow,' to ',secnxt
#ifdef MP
        endif
#endif
!
! ....... setup the target lateral boundary for rsm
      call rsmltb(secnow,secnxt,ggz,gq,gte,gdu,gzv,grq)
!
! ................................................
! ....... do step 1 if fhour=0 
      if( fhour.eq.0.0 ) then
#ifndef MP
        if( fhdfi.gt.0. ) then
          call rsmdfi
        endif
#endif
        kdt=kdt+1
        fhour=kdt*deltim/3600
        call rfwdstep
        phour=fhour
      endif
!
!....
!...............................................................
!...      smooth start time loop .............................
!
      lsimp=isemimp.eq.1
      lsdmp=idmpjet.eq.1
      lsphy=imdlphy.eq.1
#ifdef A
      lsbgt=.true.
#endif
      lsfwd=.false.
      lsftr=.true.     ! the same as lsgen=.false.
      lssav=.true.
!
      dtltb=deltim
      dt2=2.*deltim
      idt=kdt+1
      mdt=nint(secnxt/deltim)
#ifdef A
      call rsetbgt(dt2,idt,mdt,nrbgt,nrbgt1,nrbgt2)
#endif
!
!     print *,' run step from ',idt,' to ',mdt
!
      do 10000 jdt=idt,mdt
        kdt=kdt+1
        fhour=kdt*deltim/3600
        lsout=mod(kdt,nsout).eq.0
        lscca=mod(kdt,nsswr).eq.0
        lsswr=mod(kdt,nsswr).eq.1
        lslwr=mod(kdt,nslwr).eq.1
#ifdef RKN
        lsken=npoint.gt.0.and.mod(kdt,nsken).eq.0
        isave = 0
        if(lsken) isave = 1
#endif
!....
        call rsmstep
!.....
! intermedium output
        if(lsout) then
       if ( mype.eq.0) print *,'in rsmsmf lsout=',lsout,'kdt=',kdt       &   
     &  ,'nsseg=',nsseg,'nsout=',nsout,'bsout=',bsout,'lsouf=',          &
     &  lsouf
        lsouf=mod(kdt,nsseg).eq.0
        if(.not.lsouf) then
#ifdef RKN
#ifdef MP
          call rmpkencollect(itnum,nvrken,nptken,nstken,svdata)
#endif
#endif
          call rwrtout
          call rzerout
#ifdef RKN
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
#endif
!
        endif
        endif
        phour=fhour
10000 continue
!................................................
!          time loop end
!................................................
#ifdef A
      call rwrtbgt(fhour,idate)
#endif
!................................................
!
      return
      end
