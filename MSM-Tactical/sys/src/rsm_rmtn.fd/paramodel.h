!cccj
#include <define.h>
#include <defdim.h>
!ccc
!cc for base field
      integer              jcap,jcap1,jcap2,latg,latg2,latr,latr2
      integer              levh,levm1,levp1,levs,lnt,lnt2,lnt22
      integer              lnte,lnted,lnto,lntod,lnuv
      integer              lonf,lonfx,lonr,lonrx
      integer              lonb,latb,jromb,latb2,lonb2,lonb22
      integer              lonf2,lonf22,lonr2,lonr22
      integer              twoj1,lnu2,lnut,lnut2,lnut22
      integer              nvect_d
      integer              nxpt,nypt,jintmx,latgd
      integer ntotal,ntoz,ncld,ncldb
      integer lsoil,nmtvr
#ifdef GTOPO30
      real mtnres
#else
      integer mtnres
#endif
      integer igrd1i,jgrd1i,levri,igrd1o,jgrd1o,levro
      integer ilot,jlot,klot,ilor,ksph
      integer num_p3d,num_p2d
!cc
      parameter ( nxpt   = 1   )
      parameter ( nypt   = 2   )
      parameter ( jintmx   = 2 )
      parameter ( jcap   = _JCAP_  )
      parameter ( levs   = _LEVR_  )
      parameter ( lonf   = _LONF_ )

      parameter ( lonr   = _LONF_ )
      parameter ( latg   = _LATG_  )

      parameter ( latgd   = latg+ 2*jintmx )
      parameter ( latr   = _LATG_  )
!cccj
      parameter (jromb  = 0)
      parameter ( latb   = _LATG_  )
      parameter (latb2  = latb/2)
!c
      parameter ( lonb   = _LONF_  )
      parameter (lonb2  = lonb*2)
      parameter (lonb22 = lonb*2+2)
!c
      parameter (lonf2  = lonf*2)
      parameter (lonf22 = lonf*2+2)
      parameter (lonr2  = lonr*2)
      parameter (lonr22 = lonr*2+2)

!ccc
      parameter ( nvect_d  = 64  )
      parameter ( jcap1  = jcap+1 )
      parameter ( jcap2  = jcap+2 )
      parameter ( latg2  = latg/2 )
      parameter ( latr2  = latr/2 )
      parameter ( levm1  = levs-1 )
      parameter ( levp1  = levs+1 )
!cjfe  parameter ( lonfx  = lonf+2 )
      parameter ( lonfx  = lonf + 1 + 2*nxpt+1 )
      parameter ( lonrx  = lonr+2 )
      parameter ( lnt    = jcap2*jcap1/2 )
      parameter ( lnuv   = jcap2*jcap1 )
      parameter ( lnt2   = 2*lnt )
      parameter ( lnt22  = 2*lnt+1 )
      parameter ( lnte   = (jcap2/2)*((jcap2/2)+1)-1 )
      parameter ( lnto   = (jcap2/2)*((jcap2/2)+1)-(jcap2/2) )
      parameter ( lnted  = lnte )
      parameter ( lntod  = lnto )
!c
      parameter (twoj1  = 2*jcap1)
!c
      parameter (lnu2   = 2*lnuv)
      parameter (lnut   = lnt+jcap1)
      parameter (lnut2  = 2*lnut)
      parameter (lnut22 = 2*lnut +1)
!c
!
!     for zhao/carr/sundqvist microphysics
!
!     parameter(num_p3d = 4)
!     parameter(num_p2d = 3)
!
!     for brad ferrier microphysics
!
!     parameter(num_p3d = 3)
!     parameter(num_p2d = 1)
!
      parameter(num_p3d = _NUMP3D_ )
      parameter(num_p2d = _NUMP2D_ )
!cc
!
!     prognostic cloud condensate case
!
      parameter(ncldb   = _NCLDB_ )
!c     parameter(ncldb   = 0)
!c
!c  ncldb = 0 & ncld=1 ...  set base field cloud to be zero 
!c            (regional cloud is predicted without base field forcing)
!c  ncldb = 1 & ncld=1 ...  regional cloud is predicted with base field cloud forcing
!c
!c     parameter(ntoz   = 0)
      parameter(ntoz   = 1)
      parameter(ncld   = _NCLD_ )
      parameter(ntotal = 1+ntoz+ncld)
!!
#ifdef GTOPO30
      parameter(mtnres  = 0.5)
      parameter(nmtvr   = 1)
#else
      parameter(mtnres  = 4)
!c     parameter(nmtvr   = 1)
!c     parameter(nmtvr   = 6)
      parameter(nmtvr   = 10)
!c    parameter(nmtvr   = 14)
#endif
      parameter(lsoil   = 2)
!c
!c  for rsm
!c
      integer igrd,jgrd,igrd1,jgrd1,igrd2,jgrd2,                        &
     &        jumpr,jumpry,lngrd,iwav,jwav,iwav1,jwav1,lnwav,           &
     &        cigrd1,cjgrd1,clngrd,levr,levrp1,levrm1,                  & 
     &        rgen,border,bsmooth,bgf,rlpnt,rltstp,rslvark,             &  
     &        rmlvark,igrdcut,jgrdcut,grdcut
      integer nt,nq,nu,nv,np
      real order
!c 
      parameter(igrd    = _IGRD_)
      parameter(jgrd    = _JGRD_)
      parameter(levr    = _LEVR_)
!c
      parameter(igrd1   = igrd+1)
      parameter(jgrd1   = jgrd+1)
      parameter(igrd2   = igrd*2)
      parameter(jgrd2   = jgrd*2)
      parameter(jumpr   = igrd2+3)
      parameter(jumpry  = jgrd2+3)
      parameter(lngrd   = igrd1*jgrd1)
!c
      parameter(grdcut  = igrd-12)
      parameter(iwav    = grdcut/3*2)
      parameter(jwav    = iwav*jgrd/igrd2*2)
      parameter(iwav1   = iwav+1)
      parameter(jwav1   = jwav+1)
      parameter(lnwav   = iwav1*jwav1)
!c
      parameter(cigrd1  = _CIGRD1_ )
      parameter(cjgrd1  = _CJGRD1_ )
      parameter(clngrd  = cigrd1*cjgrd1)
!c
      parameter(levh    = levr*ntotal)
      parameter(levrp1  = levr+1)
      parameter(levrm1  = levr-1)
!c
      parameter(igrd1i  = igrd1)
      parameter(jgrd1i  = jgrd1)
      parameter(levri   = levr)
      parameter(igrd1o  = igrd1)
      parameter(jgrd1o  = jgrd1)
      parameter(levro   = levr)
!c
!c define model physic dimension for local;
!c
      parameter(ilot  = igrd1)
      parameter(jlot  = jgrd1)
      parameter(klot  = levr)
      parameter(ilor  = igrd1)
      parameter(ksph  = levr)
!c
!c  other parameters for rsm
!c
      parameter(rgen = 99)
!c regional model lanczos smoother for mountain height
!c jun change, make rmtn smoother
!c    parameter(order  = 0.5) 
      parameter(order  = 2.0) 
!c regional model base field parameters
      parameter(bgf     = _BGF_ )   ! delx base equal to  bgf time delx
      parameter(border  = 3)   ! interpolate order
      parameter(bsmooth = 1)   ! smooth order after interpolate
!c regional energy budget
      parameter(nt  = 15) ! numbers of term for temperature budget
      parameter(nq  = 10) ! numbers of term for moisture budget
      parameter(nu  = 11) ! numbers of term for u budget
      parameter(nv  = 11) ! numbers of term for v budget
      parameter(np  = 6)  ! numbers of term for lnps budget
!c ken point for regional
      parameter(rlpnt   = 1376)
      parameter(rltstp  = 3)
      parameter(rslvark = 80)
      parameter(rmlvark = 16)
!cc
!c
!c parameters for mpi
!c
#ifdef MP
!c
#include <ncolrowi.h>
!c
!cc for base field
      integer levsp,levhp,levp1p,levm1p,lonfp,lonf2p,                   & 
     &        lonf22p,latgp,latg2p,jcapp,jcap1p,lcapp,lcap22p,          &   
     &        lntp,lnt2p,lnt22p,llnp,lln2p,lln22p
      parameter(levsp   = (levs-1)/ncoli+1)
      parameter(levp1p  = levsp+1)
      parameter(levm1p  = levsp-1)
      parameter(lonfp   = (lonf-1)/ncoli+1)
      parameter(lonf2p  = lonfp*2)
      parameter(lonf22p = lonfp*2+2)
      parameter(latg2p  = (latg2-1)/nrowi+1)
      parameter(latgp   = latg2p*2)
      parameter(jcapp   = jcap/nrowi+1)
      parameter(jcap1p  = jcapp+1)
      parameter(lcapp   = (jcapp+1)*2)
      parameter(lcap22p = (jcapp+1)*4+2)
      parameter(llnp    = (lnt-1)/nrowi+1+(nrowi-1)/2+1)
      parameter(lln2p   = llnp*2)
      parameter(lln22p  = llnp*2+1)
      parameter(lntp    = (llnp-1)/ncoli+1)
      parameter(lnt2p   = lntp*2)
      parameter(lnt22p  = lntp*2+1)
!c
!cc for rsm
      integer levrp,igrd1p,jgrd1p,lngrdp,                                &  
     &        iwav1p,llwavp,lnwavp,levrp1p,levrm1p
      parameter(levrp   = levsp)
      parameter(igrd1p  = (igrd1-1)/ncoli+1)
      parameter(jgrd1p = (jgrd1-1)/nrowi+1)
      parameter(lngrdp  = igrd1p*jgrd1p)
      parameter(iwav1p  = (iwav1-1)/nrowi+1)
      parameter(levhp   = levrp*ntotal)
      parameter(levrp1p = levrp+1)
      parameter(levrm1p = levrp-1)
      parameter(llwavp  = iwav1p*jwav1)
      parameter(lnwavp  = (llwavp-1)/ncoli+1)
!c
#endif
