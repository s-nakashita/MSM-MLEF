#include <define.h>
      subroutine rsminp(nin,ninr,iread,fhour,idate,                      &
     &     flat,flon,fm2,fm2x,fm2y,sl,si,                                &
     &     fgz,fq,fte,fuu,fvv,frq                                        &
#ifdef NONHYD
     &               ,fpn,ftn,fon,tov                                    &
#endif
     &                            )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsminp
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract: read global sigma file to get base field, and read regional
!           sigma file, then compute the perturbation.
!
! program history log:
!
! usage:    call rsminp(nin,ninr,ig2r,fhour,idate,
!          1     flat,flon,fm2,fm2x,fm2y,sl,si,
!          1     fgz,fq,fte,fuu,fvv,frq)
!   input argument list:
!     nin       - unit number of global sigma file
!     ninr      - unit number of regional sigma file
!
!   output argument list:
!     ig2r      - index to set global to regional initial call or not.
!     fhour     - forecast hour of regional model
!     idate     - date of hour month day year
!     flat      - latitude of regional grid field
!     flon      - longitude of regional grid field
!     fm2       - map factor square
!     fm2x      - map factor square of x - derivative
!     fm2y      - map factor square of y - derivative
!     si        - sigma interface
!     sl        - sigma layers
!     fgz       - regional coefficient of terrain height
!     fq        - regional coefficient of log surface pressure
!     fte       - regional coefficient of virtual temperature
!     fuu       - regional coefficient of x - wind
!     fvv       - regional coefficient of y - wind
!     frq       - regional coefficient of specific humidity
!
!   input files: 
!     nin
!
!   output files: none
!
!   subprograms called:
!       gsm2rsm  sread  maxmin
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!....
      use physcons, pi => con_pi
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#include <commpi.h>
#include <commpildef.h>
#define LNT22S lnt22p
#define LNWAVS lnwavp
#define LNGRDS lngrdp
#else
#define LNT22S lnt22
#define LNWAVS lnwav
#define LNGRDS lngrd
#endif
!....
#include <rscompln.h>
#include <rscomloc.h>
#include <rscomltb.h>
!...sof include..........................................
!...
      character*8 label(4)
#ifdef G2R
      dimension                                                          &  
     &    gz(LNT22S), q(LNT22S)                                          &
     &  , te(LNT22S,levs), rq(LNT22S,levs,ntotal)                        &
     &  , du(LNT22S,levs), zv(LNT22S,levs)
#endif
#ifdef C2R
      dimension                                                          &
     &    gz(clngrd), q(clngrd)                                          &
     &  , te(clngrd,levr), rq(clngrd,levr,ntotal)                        &
     &  , du(clngrd,levr), zv(clngrd,levr)                               &
     &  , cc(clngrd),dummy(clngrd)
       dimension cmap(lngrdb),bmap(lngrdb)
#endif
!
      dimension idate(4),sl(levr),si(levrp1)                             &   
     & ,fgz(LNWAVS),fq(LNWAVS)                                           &
     & ,fte(LNWAVS,levr),frq(LNWAVS,levr,ntotal)                         &
     & ,fuu(LNWAVS,levr),fvv(LNWAVS,levr)                                &
#ifdef NONHYD
     & ,fpn(LNWAVS,levr),ftn(LNWAVS,levr)                                &
     & ,fon(LNWAVS,levr+1),tov(levr)                                     &
#endif
     & ,flat(LNGRDS),flon(LNGRDS)                                        &
     & ,fm2(LNGRDS),fm2x(LNGRDS),fm2y(LNGRDS)
!
      dimension tmp(lnwav),tmpsk(lnwav,levr)
! zyf test
      dimension zyftmp(LNWAVS,levr),zyfgrid(LNWAVS,levr+1)
      dimension zyffull(lngrd,levr+1)
!....
!.................................................................
#ifdef MP
      if( mype.eq.master ) then
#endif
      print *,' ------- into rsminp ----- '
#ifdef MP
      endif
#endif
!.................................................................
!
      if( iread .eq. 1 ) then
!
! -------------------------------------------------------
! read global or regional data
#ifdef G2R
#ifdef MP
        ijmaxf=lnt22
!        ijmaxp=glntlen(mype)
        ijmaxp=lnt22p
#else
        ijmaxf=lnt22
        ijmaxp=lnt22
#endif
        kmax=levs
#endif
#ifdef C2R
        ijmaxf=clngrd
        ijmaxp=clngrd
        kmax=levr
#endif
        call cread(nin,ijmaxf,ijmaxp,kmax,                               &  
#ifdef MP
     &             mype,master,                                          &
#endif
#ifdef G2R
     &             jcap,lwvdef,                                          &
#endif
     &             label,gfhour,idate,sl,si,                             &
     &             gz,q,te,du,zv,rq                                      &
#ifdef C2R
     &            ,cc,dummy,dummy,dummy,dummy                            &
#endif
     &            ,ntotal,ntoz,ncld,ncldb)
! ................................................................
! setup u v rotate for rsm2bgd
#ifdef C2R
       call cgtobg(cc,cmap,1)
! define cgrid project transformation for u and v
       rad = pi / 180.0
       hfpi= 0.5 * pi
       truth = rtruth
       nproj = rproj
       orient = rorient
       dlamda0 = orient * rad + hfpi
       ncproj = cproj
       cdlamda0 = corient * rad + hfpi
       do n=1,lngrdb
         if( nproj.eq.1 .or. nproj.eq.-1 ) then
           a2 = ( 1.0 + sin( nproj * truth * rad ) )
           bmap(n)=a2/(1.+sin( nproj * blat(n) ))
           rgln = blon(n) - dlamda0
           rgln = nproj * rgln
           csmm = nproj * cos( rgln ) 
           snmm = nproj * sin( rgln )
         else if( nproj.eq.0 ) then
           a2 = cos( truth * rad )
           bmap(n)=a2/cos( blat(n) )
           csmm =  0.0
           snmm = -1.0 
         endif
         if( ncproj.eq.1 .or. ncproj.eq.-1 ) then
           call ll2xy(blat(n),blon(n),x,y)
           call xy2ll(x,y,clat,clon)
           rgln = clon - cdlamda0
           rgln = ncproj * rgln
           csll = ncproj * cos( rgln )
           snll = ncproj * sin( rgln )
         else if( ncproj.eq.0 ) then
           csll =  0.0
           snll = -1.0 
         endif
         cmorm = sqrt( cmap(n) )/ bmap(n)
         bcsln(n) = ( csll * csmm + snll * snmm ) * cmorm
         bsnln(n) = ( csll * snmm - snll * csmm ) * cmorm
       enddo
#endif
!.................................................................
! save bgd field in gz qltb etc
#ifdef G2R
       call gsm2bgd(gz,q,te,du,zv,rq)
#endif
#ifdef C2R
       call rsm2bgd(gz,q,te,du,zv,rq)
#endif
!...................................................................
      do 644 n=1,lngrdb
        qb(n)= qltb(n)
 644  continue
      do 655 k=1,levr
      do 655 n=1,lngrdb
        teb(n,k)= teltb(n,k)
        uub(n,k)= uultb(n,k)
        vvb(n,k)= vvltb(n,k)
 655  continue
      do 656 nc=1,ntotal
      do 656 k=1,levr
      do 656 i=1,lngrdb
        rqb(i,k,nc)= rqltb(i,k,nc)
 656  continue
!
      endif
!
! ....................................................
!.....
!.... read in regional perturbation coefficient
      call sread(ninr,label,fhour,idate,                                 &  
     &           flat,flon,fm2,fm2x,fm2y,sl,si,                          &
     &           fgz,fq,fte,fuu,fvv,frq                                  &
#ifdef NONHYD
     &                     ,fpn,ftn,fon,tov                              &
#endif
     &                                      )
!.....
! ...............................................
      l = levr
#ifdef MP
      if( mype.eq.master ) then
#endif
#ifdef NONHYD
      do k=1,levr
       print*,'tov = ',tov(k),' at lev= ',k
      enddo
#endif
!      call maxmin(fgz,LNWAVS,1,1,1,'gz coef perturbation')
!      call maxmin( fq,LNWAVS,1,1,1,' q coef perturbation')
!      call maxmin(fte,LNWAVS,levr,1,l,'te coef perturbation')
!      call maxmin(frq,LNWAVS,levr,1,l,'rq coef perturbation')
!      call maxmin(fuu,LNWAVS,levr,1,l,'uu coef perturbation')
!      call maxmin(fvv,LNWAVS,levr,1,l,'vv coef perturbation')
!#ifdef NONHYD
!      call maxmin(fpn,LNWAVS,levr,1,l,'pn coef perturbation')
!      call maxmin(ftn,LNWAVS,levr,1,l,'tn coef perturbation')
!      call maxmin(fon,LNWAVS,levr+1,1,l+1,'wn coef perturbation')
!#endif
#ifdef MP
      endif
#endif
! .......................................................
!
      return
      end
